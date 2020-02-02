:- use_module(prolog_to_minizinc).
:- initialization(main).

main :- to_minizinc(demo, Output),
		writeln(Output),
		open('output.mzn',write,Stream),
        write(Stream,Output),
        nl(Stream),
        close(Stream).

demo :-
	member(1,A),length(C,5),all_different(C),length(A,2),append(A,B,C).

is_between(A,B,C) :- A=<B,B=<C.
