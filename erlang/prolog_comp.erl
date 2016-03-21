%% compile with c(prolog_comp).
%% run demo with prolog_comp:demo().

-module(prolog_comp).
-export([demo/0]).

print(X) ->
    io:format("~w~n~n", [X]).



%% ===== append/2 =====

%% append([], Y, Y).
%% append([H|T1], Y, [H|T2]) :- append(T1, Y, T2).

append([], Y) ->
    Y;
append([H|T1], Y) ->
    [H|append(T1, Y)]. 

%% Erlang append/2 is similar to Prolog append/3,
%% but since Erlang has return values,
%% the "Z" list is moved to the right side of the clause.
%% However, the pattern matching power of Prolog is gone!
%% Can't do append([1,2], R, [1,2,3]), R = [3]. in Erlang.

demo_append() ->
    io:format("append([1,2], [3,4]) =>~n", []),
    print(append([1,2], [3,4])).
    


%% ===== insert/2 =====

%% insert(E, nil, node(E, nil, nil)).
%% insert(E, node(V, Lt, _), node(V, NewLt, _)) :- 
%%     E < V, insert(E, Lt, NewLt).
%% insert(E, node(V, _, Rt), node(V, _, NewRt)) :- 
%%     E > V, insert(E, Rt, NewRt).

insert(E, nil) ->
    {node, E, nil, nil};
insert(E, {node, V, Lt, Rt}) when E < V ->
    {node, V, insert(E, Lt), Rt};
insert(E, {node, V, Lt, Rt}) when E > V ->
    {node, V, Lt, insert(E, Rt)}.    

%% insert 5 into the following tree:
%%       3                 3
%%      / \      ==>      / \
%%     1   7             1   7
%%                          /
%%                         5
demo_insert() ->
    io:format("insert(5, {node, 3, {node, 1, nil, nil}, {node, 7, nil, nil}}) =>~n", []),
    print(insert(5, {node, 3, {node, 1, nil, nil}, {node, 7, nil, nil}})).



%% ===== search/2 =====
search(_, nil) ->
    false;
search(E, {node, E, _, _}) ->
    true;
search(E, {node, V, Lt, _}) when E < V ->
    search(E, Lt);
search(E, {node, V, _, Rt}) when E > V ->
    search(E, Rt).

%% search for 2 in the following tree:
%%  ex1)    3        ex2)    3
%%         / \              / \
%%        1   7            1   7
%%                          \
%%                           2
demo_search() ->
    io:format("search(2, {node, 3, {node, 1, nil, nil}, {node, 7, nil, nil}}) =>~n"),
    print(search(2, {node, 3, {node, 1, nil, nil}, {node, 7, nil, nil}})),
    io:format("search(2, {node, 3, {node, 1, nil, {node, 2, nil, nil}}, {node, 7, nil, nil}}) =>~n", []),
    print(search(2, {node, 3, {node, 1, nil, {node, 2, nil, nil}}, {node, 7, nil, nil}})).





demo() ->
    demo_append(),
    demo_insert(),
    demo_search().
