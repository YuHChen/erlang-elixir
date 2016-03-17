%% compile with c(concur).
%% run with concur:demo_*(*).

-module(concur).
-export([demo_spawn/0, say/2, demo_msg/3, teacher/4, student/1]).

%% creating processes is simple

say(_, 0) ->
    done;
say(Message, N) ->
    io:format("~w~n", [Message]),
    say(Message, N-1).

demo_spawn() ->
    spawn(concur, say, [hello, 1]),
    spawn(concur, say, [world, 2]).



%% message passing is simple too, use '!' operator

teacher(Name, 0, Students, _) ->
    %% send atom dismissed to process with PID Student
    Dismiss = fun(Student) -> Student ! dismissed end,
    %% dismiss all students
    lists:foreach(Dismiss, Students),
    io:format("~w: Class dismissed!~n", [Name]);
teacher(Name, N, [Student|Students], [Student_name|Student_names]) ->
    io:format("~w: ~w, parity of ~w?~n", [Name, Student_name, N]),
    Student ! {N, self()},
    receive
	%% pattern match on incoming messages
	{S, Sname, X, odd} ->
	    %% add student that replied to end of list
	    New_students = lists:append(Students, [S]),
	    New_student_names = lists:append(Student_names, [Sname]),
	    %% verify student's answer
	    if 
		X rem 2 == 1 ->
		    io:format("~w: Correct, ~w is odd!~n", [Name, X]);
		true ->
		    io:format("~w: Sorry, ~w is not odd!~n", [Name, X])
	    end,
	    %% loop to top, but with student appended
	    teacher(Name, N-1, New_students, New_student_names);
	{S, Sname, X, even} ->
	    %% add student that replied to end of list
	    New_students = lists:append(Students, [S]),
	    New_student_names = lists:append(Student_names, [Sname]),
	    %% verify student's answer
	    if 
		X rem 2 == 1 ->
		    io:format("~w: Sorry, ~w is not even!~n", [Name, X]);
		true ->
		    io:format("~w: Correct, ~w is even!~n", [Name, X])
	    end,
	    %% loop to top, but with student appended
	    teacher(Name, N-1, New_students, New_student_names)
    end.

student(Name) ->
    receive
	dismissed ->
	    io:format("~w: It's finally over!~n", [Name]);
	{N, Teacher} ->
	    io:format("~w: ~w is ~w~n", [Name, N, odd]),
	    %% reply to teacher
	    Teacher ! {self(), Name, N, odd},
	    student(Name)
    end.

%% each student process will receive Chances number of messages
demo_msg(Professor_name, Chances, Student_names) ->
    %% spawn new student process given student name
    New_student = fun(Name) -> spawn(?MODULE, student, [Name]) end,
    %% spawn all student processes
    Students = lists:map(New_student, Student_names),
    %% spawn teacher process
    spawn(concur, teacher, [Professor_name, Chances*length(Students), Students, Student_names]).
