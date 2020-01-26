:- initialization(main).
:- set_prolog_flag('double_quotes','chars').
:- use_module(type_inference).

main :- to_minizinc((B=B*2,length(L1,Z3),member(3,L1)), Output),
		writeln(Output),
		open('output.mzn',write,Stream),
        write(Stream,Output),
        nl(Stream),
        close(Stream).

types_to_vars([A],[]) :-
	var(A).
types_to_vars([(Var:Type)|Rest],[Var1|Rest1]) :-
	type_to_var(Var:Type,Var1),
	types_to_vars(Rest,Rest1).

type_to_var(A:bool,["var bool:",A,";\n"]) :- nonvar(A).
type_to_var(A:number,["var float:",A,";\n"]) :- nonvar(A).
type_to_var(A:[list,number],["array[int] of var float:",A,";\n"]) :- nonvar(A).
type_to_var(A:Type,[]) :- writeln('matching other pattern'),(nonvar(A),writeln(A:Type);var(A),writeln(A:Type)).

to_minizinc(Term,Output) :-
	type_inference(Term:Type,Types),writeln(Types),prolog_to_minizinc(Term,C),term_variables(C,Vars),writeln(Term),
	
	vars_to_digits(0,Vars),types_to_vars(Types,Types1),C_=[Types1,"constraint ",C,";"],append_all(C_,C1),atom_chars(Output,C1).

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
		[[length(A,B)],["(length(",A1,") == ",B1,")"]],
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
