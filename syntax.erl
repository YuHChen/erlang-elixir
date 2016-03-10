%% compile with c(syntax).
%% run demo with syntax:demo().

-module(syntax).
-export([demo_str/0, demo_maps/0]).

%% ===== Strings =====

%% modifiers are supported: http://erlang.org/doc/man/io.html
%% ~w: standard syntax. 
%% ~p: similar to ~w, but breaks long lines, also tries to detect lists of printable characters and to output these as strings.
%% ~s: use string syntax.

demo_str() ->
    Str1 = "Hello World!",
    Str2 = [97,98,99],
    io:format("with w: ~w~n", [Str1]),
    io:format("with p: ~p~n", [Str1]),
    io:format("with s: ~s~n", [Str1]),
    io:format("with w: ~w~n", [Str2]),
    io:format("with p: ~p~n", [Str2]),
    io:format("with s: ~s~n", [Str2]).



%% ===== Maps =====
new_list(M, E, C, A) ->
    #{
       milk => {M, gals}, 
       eggs => {E, dozens},
       cereal => {C, boxes}, 
       apples => {A, apples}
     }.

how_many(Key, List) ->
    try maps:get(Key, List) of
	{Val, _} ->
	    Val
    catch
	error: {badkey, _} ->
	    0;
	error: {badmap, _} ->
	    0
    end.

demo_maps() ->
    Shopping_list = new_list(0.5, 2, 1, 10),
    io:format("~w~n", [Shopping_list]),
    io:format("buy ~w apples~n", [how_many(apples, Shopping_list)]),
    io:format("buy ~w oranges~n", [how_many(oranges, Shopping_list)]).
