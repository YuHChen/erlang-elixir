%% compile with c(pass).
%% run with pass:demo_*(*).

-module(pass).
-export([demo_pass/0, say/0]).

say() ->
    receive
	{message, M} ->
	    io:format("~w~n", [M])
    end,
    say().

demo_pass() ->
    Say = spawn(?MODULE, say, []),
    Say ! {message, hello},
    Say ! {message, world},
    ok.
