-module(handshake_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export(['INIT'/3, 'HANDSHAKE'/3, 'RECV'/3]).

-record(state, {socket :: gen_tcp:socket(),
                key    :: binary()}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
	random:seed(erlang:now()),
	{ok, 'INIT', #state{}}.

'INIT'({_, Socket}, _Ref, State) ->
	[Key] = io_lib:format("~p", [random:uniform(1000000)]),
	gen_tcp:send(Socket, [
		"Welcome to the statefull authentication example.\n\n",
		"You can login by writing 'login:<key>' where <key>\n",
		"is the number presented below\n\n",
		<<"auth-req:">>, Key, <<"\n">>
	]),
	error_logger:info_msg("waiting for key ~p~n", [Key]),
	{reply, ok, 'HANDSHAKE', State#state{socket = Socket, key = list_to_binary(Key)}}.

'HANDSHAKE'({_, Socket, Buf}, _Ref, State = #state{key = Match, socket = Socket}) ->
	{ok, {IP, Port}} = inet:peername(Socket),
	Size = (byte_size(Buf) - 7) * 8,
	case Buf of
		<<"login:", Key:Size, _/binary>> when <<Key:Size>> =:= Match ->
			error_logger:info_msg("Key match, authenticating client ~p:~p~n", [IP, Port]),
			gen_tcp:send(Socket, ["authenticated as ", <<Key:Size>>, "\n"]),
			{reply, ok, 'RECV', State};
		<<"login:", Key:Size, _/binary>> ->
			error_logger:info_msg("authenticating failed for client ~p:~p (~p vs ~p)~n", [IP, Port, <<Key:Size>>, Match]),
			gen_tcp:send(Socket, "authentication failed, try again\n"),
			{reply, ok, 'RECV', State};
		_ ->
			[NewKey] = io_lib:format("~p", [random:uniform(1000000)]),
			gen_tcp:send(Socket, [<<"auth-req:">>, NewKey, <<"\n">>]),
			{reply, ok, 'HANDSHAKE', State#state{key = list_to_binary(NewKey)}}
	end.

'RECV'({_, Socket, Buf}, _Ref, State = #state{key = Key, socket = Socket}) ->
	gen_tcp:send(Socket, [<<"client[", Key/binary, "] -> ">>, Buf]),
	{reply, ok, 'RECV', State}.

handle_info(_,  StateName, State) -> {next_state, StateName, State}.
handle_event(_, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_, _, StateName, State) -> {reply, ok, StateName, State}.
terminate(_, _, _State) -> ok.
code_change(_,   StateName, State, _) -> {ok, StateName, State}.
