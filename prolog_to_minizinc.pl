:- initialization(main).
:- set_prolog_flag('double_quotes','chars').
:- use_module(library(clpfd)).

main :- to_minizinc(((A*B)+G=<2,forall(member(G,L),G<2)),Output),writeln(Output).

to_minizinc(Term,Output) :- prolog_to_minizinc((Term,forall(member(G,L),G<2)),C),term_variables(C,Vars),vars_to_digits(0,Vars),C_=["constraint ",C,";"],append_all(C_,C1),atom_chars(Output,C1).

vars_to_digits(_,[]).
vars_to_digits(Index,[Var1|Vars]) :-
	number_chars(Index,Var),
	Var1 = ['A',Var],
	Index1 is Index + 1,
	vars_to_digits(Index1,Vars).

unify_if_match(A,B) :-
	subsumes_term(A,B),A=B.

matches_any_([],B) :- false.
matches_any_([A|A1],B) :-
	subsumes_term(A,B),A=B;matches_any_(A1,B).

matches_any(A,B) :-
	nonvar(A),matches_any_(A,B).



append_all(A,A) :- number(A),var(A).
append_all([],[]).
append_all([A|B],C) :- is_list(A),append_all(A,A1),append_all(B,B1),append(A1,B1,C).
append_all([A|B],C) :- (var(A);atom(A);number(A)),append_all(B,B1),append([A],B1,C).


prolog_to_minizinc(A,A1) :-
	number(A),number_chars(A,A1).
prolog_to_minizinc(A,A) :-
	var(A).

prolog_to_minizinc(T,Output) :-
	unify_if_match((A;B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	[A1,"\\/",B1]=Output.

prolog_to_minizinc(T,Output) :-
	unify_if_match((A,B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"/\\",B1,")"]=Output.

prolog_to_minizinc(T,Output) :-
	unify_if_match((A+B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"+",B1,")"]=Output.

prolog_to_minizinc(T,Output) :-
	unify_if_match((A-B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"-",B1,")"]=Output.

prolog_to_minizinc(T,Output) :-
	matches_any([(A\=B),A\==B,dif(A,B)],T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"!=",B1,")"]=Output.

prolog_to_minizinc(T,Output) :-
	unify_if_match((A/B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"/",B1,")"]=Output.

prolog_to_minizinc(T,Output) :-
	unify_if_match((A*B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"*",B1,")"]=Output.
	
prolog_to_minizinc(T,Output) :-
	unify_if_match((A>B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,">",B1,")"]=Output.
	
prolog_to_minizinc(T,Output) :-
	unify_if_match((A<B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"<",B1,")"]=Output.

prolog_to_minizinc(T,Output) :-
	unify_if_match((A>B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,">=",B1,")"]=Output .
	
prolog_to_minizinc(T,Output) :-
	unify_if_match((A=<B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"<=",B1,")"] = Output.

prolog_to_minizinc(T,Output) :-
	unify_if_match((A->B),T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1,"->",B1,")"] = Output.

prolog_to_minizinc(T,Output) :-
	matches_any([member(A,B),memberchk(A1,B1)],T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["(",A1," in ",B1,")"]=Output.

prolog_to_minizinc(T,Output) :-
	matches_any([forall(A,B)],T),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	["forall(",A1,")(",B1,")"]=Output.
