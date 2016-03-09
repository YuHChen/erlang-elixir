-module(prolog_comp).
-export([demo/0]).

print(X) ->
    io:format("~w~n~n", [X]).
print_str(X) ->
    io:format("~s~n", [X]).


%% append([], Y, Y).

append([], Y) ->
    Y;

%% append([H|T1], Y, [H|T2]) :- append(T1, Y, T2).
append([H|T1], Y) ->
    [H|append(T1, Y)]. 


%% append/2 is similar to Prolog append/3, but
%% since Erlang actually has return values,
%% the Z list is moved to the right side of the clause.
%% However, the pattern matching power of Prolog is gone!
%% Can't do append([1,2], R, [1,2,3]), R = [3].

demoAppend() ->
    print_str("expect [1,2,3,4]"),
    print(append([1,2], [3,4])).
    



%% insert(E, nil, node(E, nil, nil)).

insert(E, nil) ->
    {node, E, nil, nil};

%% insert(E, node(V, Lt, _), node(V, NewLt, _)) :- 
%%     E < V, insert(E, Lt, NewLt).

insert(E, {node, V, Lt, Rt}) when E < V ->
    {node, V, insert(E, Lt), Rt};

%% insert(E, node(V, _, Rt), node(V, _, NewRt)) :- 
%%     E > V, insert(E, Rt, NewRt).

insert(E, {node, V, Lt, Rt}) when E > V ->
    {node, V, Lt, insert(E, Rt)}.    





search(_, nil) ->
    false;
search(E, {node, E, _, _}) ->
    true;
search(E, {node, V, Lt, _}) when E < V ->
    search(E, Lt);
search(E, {node, V, _, Rt}) when E > V ->
    search(E, Rt).

demo() ->
    demoAppend(),
    print(insert(5, {node, 3, {node, 1, nil, nil}, {node, 7, nil, nil}})),
    print(search(2, {node, 3, {node, 1, nil, nil}, {node, 7, nil, nil}})),
    print(search(2, {node, 3, {node, 1, nil, {node, 2, nil, nil}}, {node, 7, nil, nil}})).
