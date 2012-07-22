-module(linedancer_client_fsm).

-behaviour(gen_fsm).

-export([start_link/1, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		 terminate/3, code_change/4]).

-export([
	'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_DATA'/2
]).

-record(state, {socket  :: gen_tcp:socket(),  % The client socket
                addr    :: inet:ip_address(), % Client IP addr
                fsm     :: atom() | {atom(), atom()} | {global, term()} |
                           {via, atom(), term()} | pid(), % Reference to FSM
                fsmnext  = 'INIT' :: atom()   % Next state for protocol FSM
               }).

start_link(ProtocolFSM) ->
	gen_fsm:start_link(?MODULE, [ProtocolFSM], []).

-spec set_socket(pid(), port()) -> ok.
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
	gen_fsm:send_event(Pid, {socket_ready, Socket}).

init([ProtocolFSM]) ->
	process_flag(trap_exit, true),
	{_, ProtocolPid} = gen_fsm:start_link(ProtocolFSM, [], []),
	{ok, 'WAIT_FOR_SOCKET', #state{fsm = ProtocolPid}}.

-spec 'WAIT_FOR_SOCKET'({atom(), gen_tcp:socket()}, State :: #state{}) -> {next_state, term(), State :: #state{}}.
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State = #state{fsm = FSM, fsmnext = ExpectedEvent}) when is_port(Socket) ->
	inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
	{ok, {IP, _Port}} = inet:peername(Socket),
	NewState          = State#state{socket=Socket, addr=IP},
	ProtoState        = gen_fsm:sync_send_event(FSM, {ExpectedEvent, Socket}),
	{next_state, 'WAIT_FOR_DATA', NewState#state{fsmnext = ProtoState}};

'WAIT_FOR_SOCKET'(Other, State) ->
	error_logger:error_msg("State: 'WAIT_FOR_DATA'. Unexpected msg: ~p~n", [Other]),
	{next_state, 'WAIT_FOR_SOCKET', State}.

-spec 'WAIT_FOR_DATA'({atom(), binary()} | any(), State :: #state{}) -> {atom(), term(), State :: #state{}}.
'WAIT_FOR_DATA'({data, Data}, State = #state{socket  = Socket,
                                             fsm     = FSM,
                                             fsmnext = ExpectedEvent}) ->
	NextEvent = gen_fsm:sync_send_event(FSM, {ExpectedEvent, Socket, Data}),
	{next_state, 'WAIT_FOR_DATA', State#state{fsmnext = NextEvent}};

'WAIT_FOR_DATA'(timeout, State) ->
	{stop, normal, State};

'WAIT_FOR_DATA'(_, State) ->
	{next_state, 'WAIT_FOR_DATA', State}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	Reply = ok,
	{reply, Reply, StateName, State}.

handle_info({tcp, Socket, Bin}, StateName, State = #state{socket = Socket}) ->
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Bin}, State);

handle_info({tcp_closed, Socket}, _StateName, State = #state{socket = Socket,
                                                             addr = _IP}) ->
	gen_tcp:close(Socket),
	{stop, normal, State};

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, #state{socket = Socket}) ->
	(catch gen_tcp:close(Socket)),
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
