-module(linedancer_sup).

-behaviour(supervisor).

-export([start_link/1, start_client/0]).

-export([init/1]).

-spec start_link(list()) -> supervisor:startlink_ret().
start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

-spec start_client() -> supervisor:startchild_ret().
start_client() ->
    supervisor:start_child(ld_client, []).

-spec init(list()) -> supervisor:child_spec() | (List :: [term()]).
init([FSM, ProtocolFSM]) when is_atom(FSM), is_atom(ProtocolFSM) ->
	{ok, {{simple_one_for_one, 10, 60},
		[
			% TCP Client
			{	undefined,
				{FSM, start_link, [ProtocolFSM]},
				temporary,
				4000,
				worker,
				[]
			}
		]}
	};

init(Args) ->
	Port        = valdefault(port, Args, 7000),
	FSM         = valdefault(client_handler, Args, linedancer_client_fsm),
	ProtocolFSM = valdefault(protocol_handler, Args, false),
	case ProtocolFSM of
		false ->
			exit({no_protocol_handler, Args});
		_     -> void
	end,

	{ok, {{one_for_one, 10, 60},
		[
			% TCP Listener supervisor
			{	ld_listener,
				{linedancer_socket, start_link, [Port, FSM]},
				permanent,
				4000,
				worker,
				[linedancer_socket]
			},
			% TCP Client supervisor
			{	ld_client,
				{supervisor, start_link, [{local, ld_client},
				                          ?MODULE,
				                          [FSM, ProtocolFSM]]},
				permanent,
				infinity,
				supervisor,
				[]
			}
		]}
	}.

-spec valdefault(term(), integer(), any()) -> any().
valdefault(K, L, D) ->
	case lists:keyfind(K, 1, L) of
		{_, A} -> A;
		_      -> D
	end.
