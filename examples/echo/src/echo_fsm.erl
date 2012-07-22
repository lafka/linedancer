-module(echo_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		 terminate/3, code_change/4]).

-export(['INIT'/3, 'RECV'/3]).

-record(state, {socket :: gen_tcp:socket()}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init(Args) -> {ok, 'INIT', #state{}}.

'INIT'({_, Socket}, _Ref, State) ->
	{ok, {IP, Port}} = inet:peername(Socket),
	error_logger:info_msg("connection: ~p:~p~n", [IP, Port]),
	{reply, ok, 'RECV', State#state{socket = Socket}}.

'RECV'({_, Socket, Buf}, _Ref, State = #state{socket = Socket}) ->
	error_logger:info_msg("recv: ~p,~n", [Buf]),
	gen_tcp:send(Socket, Buf),
    {reply, ok, 'RECV', State}.

handle_info(_,  StateName, State) -> {next_state, StateName, State}.
handle_event(_, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_, _, StateName, State) -> {reply, ok, StateName, State}.
terminate(_, _, _State) -> ok.
code_change(_,   StateName, State, _) -> {ok, StateName, State}.
