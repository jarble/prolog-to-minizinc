:- module(type_inference, [type_inference/2]).
:- use_module(library(clpfd)).
%:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

type_inference(A,B) :- has_type(A,B).

main :- Term = (member(3,A),findall(A,B,C)),has_type(Term:Type,Types),writeln('Term with types to infer:'),writeln(Term),writeln('Types of variables in this term:'),writeln(Types).

greater_than(A,B) :-
	A > B.

matches_any_([],B) :- false.
matches_any_([A|A1],B) :-
	subsumes_term(A,B),A=B;matches_any_(A1,B).

matches_any(A,B) :-
	nonvar(A),matches_any_(A,B).

matches_types([[Patterns,Types]|Rest],Var,List) :-
	matches_any(Patterns,Var),
	has_types(Types,List);
	nonvar(Rest),matches_types(Rest,Var,List).

has_types([],_).
has_types([A|A1],B) :-
	has_type(A,B),has_types(A1,B).

list_or_set(Type) :- Type = list;Type = set.

has_type(Var:number,_) :-
	float(Var).

has_type(Var:int,_) :-
	integer(Var).

has_type(Var:[T,_,_],_) :-
	list_or_set(T),
	Var == [].

has_type([A|B]:[List_or_set,_,T],List) :-
	list_or_set(List_or_set),
	is_list(B),
	has_types([A:T,B:[list,_,T]],List).

has_type(Var:number,List) :-
	matches_any([sin(A),cos(A),tan(A),asin(A),acos(A),atan(A),sqrt(A),sinh(A),cosh(A),tanh(A),asinh(A),acosh(A),atanh(A),log(A),log10(A),exp(A)],Var),
	has_types([A:number],List);
	matches_any([pi,e,epsilon,inf],Var).

has_type(Var:Type,List) :-
	matches_any([abs(A)],Var),
	has_types([A:Type],List),
	(Type = int;Type = number).

has_type(Var:atom,_) :-
	atomic(Var),dif(Var,false),dif(Var,true).

has_type(Var:bool,List) :-
	Var==false;
	Var==true;
	
	(Type = int;Type = number),
	matches_types([
		[[sum_list(A,B),max_list(A,B),min_list(A,B)],[A:[list,_,Type],B:Type]],
		[[(A>B),(A<B),A>=B,A=<B,(A is B)],[[A,B]:[list,_,Type]]]
	],Var,List);
	
	list_or_set(List_or_set),
	matches_types([
		[[maplist(A,B)],[A:atom,B:[list,_,_]]],
		[[findall(A,B,C)],[[A,C]:[list,_,[list,_,T]],B:bool]],
		[[member(A,B),memberchk(A,B)],[A:T,B:[List_or_set,_,T]]],
		[[length(A,B)],[[A:[list,B,_]],B:number]],
		[[(A,B),(A;B),(A->B),foreach(A,B),forall(A,B)],[[A,B]:[list,_,bool]]],
		[[sort(A,B),msort(A,B)],[[A,B]:[list,_,_]]],
		[[is_list(A)],[A:[list,_,_]]],
		[[(A==B),(A = B),(A \= B),(A \== B),dif(A,B)],[[A,B]:[list,_,_]]],
		[[not(A),\+(A)],[A:bool]],
		[[integer(A),float(A),number(A)],[A:number]],
		[[atom(A),\+(A)],[A:atom]],
		[[union(A,B,C),intersection(A,B,C)],[A:[set,_,Type],B:[set,_,Type],C:[set,_,Type]]],
		[[subset(A,B)],[[A,B]:[list,_,[set,_,_]]]],
		[[is_set(A)],[A:[set,_,_]]],
		[[nth0(Index,List1,Elem),nth1(Index,List1,Elem)],[Elem:T,List1:[list,_,T],Index:int]],
		[[all_different(A),all_distinct(A)],[A:[list,_,int]]]
	],Var,List);
	T3 #= T1 + T2,T1 #> 0,T2 #> 0,T3 #> 0,
	matches_types([
		[[append(A,B,C)],[A:[list,T1,Type],B:[list,T2,Type],C:[list,T3,Type]]]
	],Var,List)
	;
	
	matches_any([length(A,B)],Var),
	has_types([A:[list,B,_],B:int],List).

has_type(Var:number,List) :-
	matches_any([(A+B),(A-B),(A1*B),(A/B),A**B],Var),
	has_types([[A,B]:[list,_,number]],List).
	
has_type(Vars,List) :-
	var(List),List=[_|_],has_type(Vars,List).

has_type(Var:Type,[Var1|Rest]) :-
	var(Var),
		(Var:Type == Var1;var(Var),var(Var1),(Var:Type) = Var1;
		nonvar(Var1),Var1 = V1:T1,(V1==Var,Type = T1;
		dif(V1, Var),has_type(Var:Type,Rest))).
