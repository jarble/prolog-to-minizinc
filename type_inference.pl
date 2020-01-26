:- module(type_inference, [type_inference/2]).
:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

type_inference(A,B) :- has_type(A,B).

main :- Term = (B=B*2),has_type(Term:Type,Types),writeln('Term with types to infer:'),writeln(Term),writeln('Types of variables in this term:'),writeln(Types).

greater_than(A,B) :-
	A > B.

matches_any_([],B) :- false.
matches_any_([A|A1],B) :-
	subsumes_term(A,B),A=B;matches_any_(A1,B).

matches_any(A,B) :-
	nonvar(A),matches_any_(A,B).

has_type(Var:number,_) :-
	number(Var).

has_type(Var:[list,_],_) :-
	Var == [].

has_type([A|B]:[list,T],List) :-
	is_list(B),
	has_type(A:T,List),
	has_type(B:[list,T],List).

has_type(Var:number,List) :-
	matches_any([sin(A),cos(A),tan(A),asin(A),acos(A),atan(A),sqrt(A),sinh(A),cosh(A),tanh(A),asinh(A),acosh(A),atanh(A),log(A),log10(A),exp(A)],Var),
	has_type(A:number,List);
	matches_any([pi,e,epsilon,inf],Var).

has_type(Var:atom,_) :-
	atomic(Var),dif(Var,true),dif(Var,false).

has_type(Var:bool,List) :-
	Var==true;Var==false;
	
	matches_any([maplist(A,B)],Var),
	has_type(B:[list,_],List),has_type(A:atom,List);
	
	matches_any([member(A,B),memberchk(A,B)],Var),
	has_type(B:[list,T],List),has_type(A:T,List);
	
	matches_any([length(A,B)],Var),
	has_type(A:[list,_],List),has_type(B:number,List);
	
	matches_any([append(A,B,C)],Var),
	has_type(A:T,List),has_type(B:T,List),has_type(C:T,List),T = [list,_];
	
	matches_any([(A,B),(A;B),(A->B),forall(A,B)],Var),
	has_type([A,B]:[list,bool],List);

	matches_any([sort(A,B),msort(A,B)],Var),
	has_type([A,B]:[list,_],List);
	
	matches_any([is_list(A)],Var),
	has_type(A:[list,_],List);
	
	matches_any([(A>B),(A<B),A>=B,A=<B,(A is B)],Var),
	has_type([A,B]:[list,number],List);
	
	matches_any([(A==B),(A=B),(A \= B),(A \== B),dif(A,B)],Var),
	has_type([A,B]:[list,_],List);
	
	matches_any([not(A),\+(A)],Var),
	has_type(A:bool,List);
	
	matches_any([integer(A),float(A),number(A)],Var),
	has_type(A:number,List);
	
	matches_any([atom(A),\+(A)],Var),
	has_type(A:atom,List).

has_type(Var:number,List) :-
	matches_any([(A+B),(A-B),(A1*B),(A/B),A**B],Var),
	has_type([A,B]:[list,number],List).
	
has_type(Vars,List) :-
	var(List),List=[_|_],has_type(Vars,List).

has_type(Var:Type,[Var1|Rest]) :-
	var(Var),
		(Var:Type == Var1;var(Var),var(Var1),(Var:Type) = Var1;
		nonvar(Var1),Var1=V1:T1,(V1==Var,Type=T1;
		dif(V1, Var),has_type(Var:Type,Rest))).
