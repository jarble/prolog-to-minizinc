:- use_module(prolog_to_minizinc).
:- initialization(main).

main :- to_minizinc(demo, Output),
		writeln(Output),
		open('output.mzn',write,Stream),
        write(Stream,Output),
        nl(Stream),
        close(Stream).

demo :-
	Z=[1.0,2.0],length(Z,2),length(Z,L),length(T,L),foreach(memberchk(A,Z),is_between(1.0,A,3.0)).

is_between(A,B,C) :- A=<B,B=<C.
