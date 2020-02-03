:- use_module(prolog_to_minizinc).
:- initialization(main).

main :- 
        to_minizinc(demo, Output),
		writeln(Output),
		open('output.mzn',write,Stream),
        write(Stream,Output),
        nl(Stream),
        close(Stream).

demo :-
	length(A,3),
	foreach(member(A1,A),A1==1).

same_length(A,B) :-
    length(A,L),
    length(B,L).

is_between(1,1,1).
is_between(1,1,2).
