:- module(type_inference, [type_inference/2]).
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

has_type(Var:number,_) :-
	number(Var).

has_type(Var:[list,_],_) :-
	Var == [].

has_type([A|B]:[list,T],List) :-
	is_list(B),
	has_types([A:T,B:[list,T]],List).

has_type(Var:number,List) :-
	matches_any([sin(A),cos(A),tan(A),asin(A),acos(A),atan(A),sqrt(A),sinh(A),cosh(A),tanh(A),asinh(A),acosh(A),atanh(A),log(A),log10(A),exp(A)],Var),
	has_types([A:number],List);
	matches_any([pi,e,epsilon,inf],Var).

has_type(Var:atom,_) :-
	atomic(Var),dif(Var,true),dif(Var,false).

has_type(Var:bool,List) :-
	Var==true;Var==false;
	
	matches_types([
		[[maplist(A,B)],[A:atom,B:[list,_]]],
		[[findall(A,B,C)],[[A,C]:[list,[list,T]],B:bool]],
		[[member(A,B),memberchk(A,B)],[A:T,B:[list,T]]],
		[[length(A,B)],[[A:[list,_]],B:number]],
		[[(A,B),(A;B),(A->B),forall(A,B)],[[A,B]:[list,bool]]],
		[[sort(A,B),msort(A,B)],[[A,B]:[list,_]]],
		[[is_list(A)],[A:[list,_]]],
		[[(A>B),(A<B),A>=B,A=<B,(A is B)],[[A,B]:[list,number]]],
		[[(A==B),(A=B),(A \= B),(A \== B),dif(A,B)],[[A,B]:[list,_]]],
		[[not(A),\+(A)],[A:bool]],
		[[integer(A),float(A),number(A)],[A:number]],
		[[atom(A),\+(A)],[A:atom]],
		[[append(A,B,C)],[[A,B,C]:[list,[list,_]]]]
		
	],Var,List).

has_type(Var:number,List) :-
	matches_any([(A+B),(A-B),(A1*B),(A/B),A**B],Var),
	has_types([[A,B]:[list,number]],List).
	
has_type(Vars,List) :-
	var(List),List=[_|_],has_type(Vars,List).

has_type(Var:Type,[Var1|Rest]) :-
	var(Var),
		(Var:Type == Var1;var(Var),var(Var1),(Var:Type) = Var1;
		nonvar(Var1),Var1=V1:T1,(V1==Var,Type=T1;
		dif(V1, Var),has_type(Var:Type,Rest))).
