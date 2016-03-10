-module(concur).
-export([demo_spawn/0, say/2, demo_msg/0, teacher/2, student/0]).

%% creating processes is simple

say(_, 0) ->
    done;
say(Message, N) ->
    io:format("~w~n", [Message]),
    say(Message, N-1).

demo_spawn() ->
    spawn(concur, say, [hello, 1]),
    spawn(concur, say, [world, 2]).



%% message passing is simple too

teacher(0, Student) ->
    Student ! dismissed,
    io:format("class dismissed!~n", []);
teacher(N, Student) ->
    Student ! {N, self()},
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
    teacher(N-1, Student).
	
student() ->
    receive
	dismissed ->
	    io:format("it's finally over!~n", []);
	{N, Teacher} ->
	    io:format("Answer for ~w: ~w~n", [N, odd]),
	    Teacher ! {N, odd},
	    student()
    end.

demo_msg() ->
    Student = spawn(concur, student, []),
    spawn(concur, teacher, [4, Student]).
	    
