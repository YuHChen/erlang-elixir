%% for more info: http://erlang.org/doc/getting_started/conc_prog.html#id68522

%% setup:
%% 1) cd
%% 2) cat super_secret > .erlang.cookie
%% 3) chmod 400 .erlang.cookie
%% 4) erl -sname node_name

%% compile with c(dist).
%% run with dist:demo_*().

-module(dist).
-export([demo_teacher/1, demo_student/0, teacher/2, student/0]).

%% similar to concur example, but in distributed environment

teacher(0, Student_Node) ->
    % use {registered_name, node_name} instead of PID to pass message
    {student, Student_Node} ! dismissed,
    io:format("class dismissed!~n", []);
teacher(N, Student_Node) ->
    % use {registered_name, node_name} instead of PID to pass message
    {student, Student_Node} ! {N, self()},
    receive
	{X, odd} ->
	    if
		X rem 2 == 1 ->
		    io:format("correct, ~w is odd!~n", [X]);
		true ->
		    io:format("sorry, ~w is not odd!~n", [X])
	    end;
	{X, even} ->
	    if
		X rem 2 == 1 ->
		    io:format("sorry, ~w is not even!~n", [X]);
		true ->
		    io:format("correct, ~w is even!~n", [X])
	    end
    end,
    teacher(N-1, Student_Node).

student() ->
    receive
	dismissed ->
	    io:format("it's finally over!~n", []);
	{N, Teacher} ->
	    io:format("Answer for ~w: ~w~n", [N, odd]),
	    Teacher ! {N, odd},
	    student()
    end.

%% start teacher/student on different machines

demo_student() ->
    register(student, spawn(dist, student, [])).

demo_teacher(Student_Node) ->
    spawn(dist, teacher, [4, Student_Node]).
