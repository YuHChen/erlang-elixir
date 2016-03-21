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
	{Val, Key} ->
	    io:format("buy ~w ~w~n", [Val, Key]);
	{Val, Unit} ->
	    io:format("buy ~w ~w ~w~n", [Val, Unit, Key])
    catch
	error: {badkey, _} ->
	    io:format("buy 0 ~w~n", [Key]);
	error: {badmap, _} ->
	    io:format("buy 0 ~w~n", [Key])
    end.

demo_maps() ->
    Shopping_list = new_list(0.5, 2, 1, 10),
    io:format("~w~n", [Shopping_list]),
    how_many(eggs, Shopping_list),
    how_many(apples, Shopping_list),
    how_many(oranges, Shopping_list).
