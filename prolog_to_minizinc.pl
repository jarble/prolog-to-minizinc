:- initialization(main).
:- set_prolog_flag('double_quotes','chars').
:- use_module(library(clpfd)).

main :- to_minizinc((length(L1,2),append(L1,L2,L3),B2 = B1 * (1.0 + I) - R,M is Z**2),Output),writeln(Output).

to_minizinc(Term,Output) :- prolog_to_minizinc(Term,C),term_variables(C,Vars),vars_to_digits(0,Vars),C_=["constraint ",C,";"],append_all(C_,C1),atom_chars(Output,C1).

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



append_all(A,A) :- var(A).
append_all([],[]).
append_all([A|B],C) :- is_list(A),append_all(A,A1),append_all(B,B1),append(A1,B1,C).
append_all([A|B],C) :- (var(A);atom(A)),append_all(B,B1),append([A],B1,C).


prolog_to_minizinc(A,A1) :-
	number(A),number_chars(A,A1).
prolog_to_minizinc(A,A) :-
	var(A).

matches_to_outputs([Patterns|Patterns1],T,Output) :-
	Patterns = [Pattern1,Pattern2],
	matches_any(Pattern1,T),
	Pattern2=Output;
	matches_to_outputs(Patterns1,T,Output).

prolog_to_minizinc(T,Output) :-
	matches_to_outputs([
		[[append(A,B,C)],["(",A1,"++",B1,"==",C1,")"]]
	],T,Output),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	prolog_to_minizinc(C,C1).

prolog_to_minizinc(T,Output) :-
	matches_to_outputs([
		[[sin(A)],["sin(",A1,")"]],
		[[cos(A)],["cos(",A1,")"]],
		[[tan(A)],["tan(",A1,")"]],
		[[asin(A)],["sin(",A1,")"]],
		[[acos(A)],["cos(",A1,")"]],
		[[atan(A)],["tan(",A1,")"]],
		[[exp(A)],["exp(",A1,")"]],
		[[A**B],["pow(",A1,",",B1,")"]],
		[[sqrt(A)],["sqrt(",A1,")"]],
		[[log(A)],["ln(",A1,")"]],
		[[abs(A)],["abs(",A1,")"]]
	],T,Output),
	prolog_to_minizinc(A,A1).

prolog_to_minizinc(T,Output) :-
	matches_to_outputs([
		[[(A;B)],[A1,"\\/",B1]],
		[[length(A,B)],["length(",A1,"==",B1,")"]],
		[[(A,B)],["(",A1,"/\\",B1,")"]],
		[[(A+B)],["(",A1,"+",B1,")"]],
		[[(A/B)],["(",A1,"/",B1,")"]],
		[[(A>B)],["(",A1,">",B1,")"]],
		[[(A<B)],["(",A1,"<",B1,")"]],
		[[(A>=B)],["(",A1,">=",B1,")"]],
		[[(A=<B)],["(",A1,"<=",B1,")"]],
		[[(A\=B,A\==B)],["(",A1,"!=",B1,")"]],
		[[(A-B)],["(",A1,"-",B1,")"]],
		[[(A*B)],["(",A1,"*",B1,")"]],
		[[(A->B)],["(",A1,"->",B1,")"]],
		[[member(A,B),memberchk(A1,B1)],["(",A1," in ",B1,")"]],
		[[forall(A,B)],["forall(",A1,")(",B1,")"]],
		[[A==B,A=B,A is B],["(",A1," == ",B1,")"]]
	],T,Output),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1).
