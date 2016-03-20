%% compile with c(link).
%% run with link:demo_*(*).

-module(link).
-export([demo_link/0, hello/1, world/0, force_exit/2]).

%% linking processes is simple

hello(World) ->
    link(World),
    io:format("hello~n", []),
    hello(World).

world() ->
    io:format("world~n", []),
    world().

force_exit(0, Hello) ->
    exit(Hello, link_demo);
force_exit(N, Hello) ->
    force_exit(N-1, Hello).

demo_link() ->
    World = spawn(?MODULE, world, []),
    Hello = spawn(?MODULE, hello, [World]),
    spawn(?MODULE, force_exit, [5000, Hello]).

%% linking allows for fault tolerant supervisor model
