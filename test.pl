:- use_module(prolog_to_minizinc).
:- initialization(main).

main :- 
        to_minizinc(demo(B), Output),
		writeln(Output),
		open('output.mzn',write,Stream),
        write(Stream,Output),
        nl(Stream),
        close(Stream).

demo(B) :-
	length(B,4),foreach(member(A,B),(A>3.0)),foreach(member(C,B),(C>3.0)).


is_between(1,1,1).
is_between(1,1,2).
