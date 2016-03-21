%% compile with c(link).
%% run with link:demo_*(*).

-module(link).
-export([demo_link/0, hello/2, world/0]).

%% linking processes is simple

hello(2, World) ->
    link(World),
    exit(link_demo);
hello(N, World) ->
    link(World),
    io:format("hello ~w~n", [N]),
    hello(N-1, World).

world() ->
    io:format("world~n", []),
    world().

demo_link() ->
    World = spawn(?MODULE, world, []),
    spawn(?MODULE, hello, [5, World]).

%% linking allows for fault tolerant supervisor model
