:- module(prolog_to_minizinc, [to_minizinc/2]).
:- set_prolog_flag('double_quotes','chars').
:- use_module(type_inference).
:- use_module(partial_evaluation).

types_to_vars([A],[]) :-
	var(A).
types_to_vars([(Var:Type)|Rest],[Var1|Rest1]) :-
	type_to_var(Var:Type,Var1),
	types_to_vars(Rest,Rest1).

type(bool,"bool").
type(number,"float").
type(int,"int").

type_to_var(A:T,["var ",Type,":",A,";\n"]) :- type(T,Type),nonvar(A).
type_to_var(A:[list,Length,T],["array[",L,"] of var ",Type,":",A,";\n"]) :- type(T,Type),nonvar(A),(var(Length),L="int";nonvar(Length),number_chars(Length,Length1),L=["1..",Length1]).
type_to_var(A:Type,[]) :- writeln('matching other pattern'),(nonvar(A),writeln(A:Type);var(A),writeln(A:Type)).

to_minizinc(Term0,Output) :-
	find_all_clauses(Term0,Term),
	type_inference(Term:Type,Types),writeln(Types),prolog_to_minizinc(Term,C),term_variables(C,Vars),writeln(Term),
	
	vars_to_digits(0,Vars),types_to_vars(Types,Types1),C_=[Types1,"constraint ",C,";"],append_all(C_,C1),atom_chars(Output,C1).

vars_to_digits(_,[]).
vars_to_digits(Index,[Var1|Vars]) :-
	number_chars(Index,Var),
	Var1 = ['A',Var],
	Index1 is Index + 1,
	vars_to_digits(Index1,Vars).

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
	nonvar(Patterns1),
	matches_to_outputs(Patterns1,T,Output).

list_to_minizinc([],[]).
list_to_minizinc([L|Rest],[Next|Output1]) :-
	prolog_to_minizinc(L,L1),
	(Rest == [],Next=[L1];
	length(Rest,Length),Length>0,Next=[L1,","]),
	list_to_minizinc(Rest,Output1).

prolog_to_minizinc(T,["[",T1,"]"]) :-
	list_to_minizinc(T,T1).

prolog_to_minizinc(T,Output) :-
	matches_to_outputs([
		[[true],["true"]],
		[[false],["false"]]
	],T,Output);
	
	matches_to_outputs([
		[[append(A,B,C)],["(",A1,"++",B1,"==",C1,")"]],
		[[nth1(A,B,C)],["(",B1,"[",A1,"] == ",C1,")"]],
		[[nth0(A,B,C)],["(",B1,"[",A1,"+1] == ",C1,")"]]
	],T,Output),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1),
	prolog_to_minizinc(C,C1);
	
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
		[[abs(A)],["abs(",A1,")"]],
		[[number(A),float(A),rational(A),is_list(A),var(A),integer(A)],[]]
	],T,Output),
	prolog_to_minizinc(A,A1);
	
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
		[[forall(A,B),foreach(A,B)],["forall(",A1,")(",B1,")"]],
		[[A==B,A=B,A is B],["(",A1," == ",B1,")"]],
		[[findall(A,B,C)],["(",C," == [",A,"|",B,"])"]]
	],T,Output),
	prolog_to_minizinc(A,A1),
	prolog_to_minizinc(B,B1).
