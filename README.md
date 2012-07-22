linedancer
==========

A protocol agnostic tcp/ip gateway that runs in the upper layer of the OSI stack

## Introduction
Linedancer is a little framework for creating non-blocking TCP/IP servers based
roughly based on the writings of Serge Aleynikov (http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles).

The server is just a library which you can include in your application, in it's
most basic form you can start the supervisor directly:

```erlang
linedancer_sup:start_link([{port, 7000}, {protocol_handler, protocolfsm}]).
```

This will start a supervisor accepting new connections (linedancer_socket) which
in turn creates a new process for handling the data (linedancer_client_fsm),
your implementation is wrapped around the last part. If you want to change the
client handler this can be specified as an option to the supervisor ``{client_handler, <module>}``


## Examples
Linedancer comes with 2 example implementations. To run any of the examples
you can use the following commands (requires http://github.com/basho/rebar):

```bash
$ git clone git://github.com/lafka/linedancer.git
$ cd linedancer
$ rebar compile
$ cd examples/<example>
$ rebar compile
$ erl -pa ebin -pa ../../ebin -boot start_sasl -s <example>
```

You can now access the example server by using telnet, socat, netcat or whatever
tool you prefer:

```bash
netcat localhost 7000
# telnet localhost 7000
# Connected to localhost.
# Escape character is '^]'.
# G`day
# G`day
````

## Creating your own protocol
To create your own protocol you need to create your own protocol handler, this
is done by using the gen_fsm behaviour:

```erlang
-module(myprotocol).

-behaviour(gen_fsm).

%% ... removed for readability ...

%% The initial state is set by the return value of init/1
init(_) ->
	{ok, 'INIT', #state{}}.

'INIT'({_, Socket}, _Ref, State) ->
	{reply, ok, 'RECV', State#state{socket = Socket}}.

'RECV'({_, Socket, Buf}, _Ref, State = #state{socket = Socket}) ->
	gen_tcp:send(Socket, Buf),
    {reply, ok, 'RECV', State}.

%% ... removed for readability ...
```

When you have your protocol module up, just point ``protocol_handler`` in your
''*.app file'' to the name of your module and it should start serving data.

## Performance
No real benchmarks on how this actually performs in a production setting, local
testing with a minimal protocol runs around 25K req/s.
