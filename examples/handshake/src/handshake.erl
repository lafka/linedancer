-module(handshake).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(handshake).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Args = application:get_all_env(),
	{_, Port} = lists:keyfind(port, 1, Args),
	error_logger:info_msg("Starting echo server: 0.0.0.0:~p...~n", [Port]),
	linedancer_sup:start_link(Args).

stop(_State) ->
	ok.
