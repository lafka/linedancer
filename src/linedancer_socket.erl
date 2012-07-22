-module(linedancer_socket).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {network = any     :: binary(),          %% The client network id
                ip      = unknown :: inet:ip_address(), %% The client ip addr
                socket  = null    :: gen_tcp:socket(),  %% The current socket
                module            :: term(),            %% The FSM acceptor module
                acceptor                                %% The internal acceptor ref
               }).

-spec start_link(Port :: integer(), Module :: atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

-spec init(list()) -> {ok, pid()} | ignore | {error, term()}.
init([Port, Module]) ->
	process_flag(trap_exit, true),
	Opts = [binary, {active, once}, {reuseaddr, true},{keepalive, true},
	                {backlog, 30}, {packet, raw}],

	case gen_tcp:listen(Port, Opts) of
		{ok, Socket} ->
			{ok, Ref} = prim_inet:async_accept(Socket, -1),
			{ok, #state{socket   = Socket,
			            acceptor = Ref,
			            module   = Module}};
		{error, Reason} ->
			{stop, Reason}
	end.

handle_info({inet_async, Socket, Ref, {ok, Client}}, #state{socket = Socket,
                                                            acceptor = Ref,
                                                            module   = Module
                                                           } = State) ->
	try
		case set_sockopt(Socket, Client) of
			ok -> ok;
			{error, Reason} -> exit({set_sockopt, Reason})
		end,

		%%	Set the socket owner to our gen FSM process
		{ok, Pid} = linedancer_sup:start_client(),
		gen_tcp:controlling_process(Client, Pid),
		Module:set_socket(Pid, Client),

		case prim_inet:async_accept(Socket, -1) of
			{ok,    NewRef} -> ok;
			{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
		end,

		{noreply, State#state{acceptor = NewRef}}
	catch exit: Why ->
		error_logger:error_msg("Error in accept: ~p~n", [Why]),
		{stop, Why, State}
	end;

handle_info({inet_async, _Socket, _Ref, Error}, State = #state{ip = Ip}) ->
	error_logger:error_msg("Error in socket acceptor for ~p: ~p~n", [Ip, Error]),
	{stop, Error, State};

handle_info({tcp_closed, Reason}, State = #state{ip = Ip, socket = Socket}) ->
	error_logger:info_msg("Client ~p closed connection: ~p~n", [Ip, Reason]),
	gen_tcp:close(Socket),
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

handle_cast(_Info, State) ->
	{noreply, State}.

terminate(normal, #state{socket = Socket}) ->
	gen_tcp:close(Socket),
	ok;

terminate(Reason, #state{ip = Ip, socket = Socket}) ->
	gen_tcp:close(Socket),
	error_logger:info_msg("Client ~p terminated: ~p~n", [Ip, Reason]),
	ok.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

code_change(_OldVsn, State, []) ->
	{ok, State}.

set_sockopt(LSocket, Socket) ->
	true = inet_db:register_socket(Socket, inet_tcp),
	case prim_inet:getopts(LSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(Socket, Opts) of
				ok    -> ok;
				Error -> gen_tcp:close(Socket),
				         Error
				end;
		Error ->
			gen_tcp:close(Socket),
			Error
	end.
