%% compile with c(link).
%% run with link:demo().

-module(link).
-export([demo_link/0, student/0, teacher/1]).

%% linking processes is simple

teacher(Student) ->
    io:format("teacher ~w kicking student ~w out~n", [self(), Student]),
    exit(Student, "disruptive in class").

student() ->
    process_flag(trap_exit, true),
    receive
	{'EXIT', From, Reason} ->
	    io:format("student ~w leaving classroom because teacher ~w says: ~s~n", [self(), From, Reason])
    end.

demo_link() ->
    Student = spawn(?MODULE, student, []),
    spawn(?MODULE, teacher, [Student]).

%% linking allows for fault tolerant supervisor model
