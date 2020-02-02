:- module(partial_evaluation, [find_all_clauses/2]).

replace_all_vars_([],[]).
replace_all_vars_([Var|Var1],[Output|Output1]) :-
	find_all_clauses(Var,Output),replace_all_vars_(Var1,Output1).

replace_all_vars_(Input,Pattern,Output) :-
	term_variables(Pattern,Vars),
	copy_term(Pattern,Output),
	term_variables(Output,Vars1),
	subsumes_term(Pattern,Input),
	Pattern=Input,
	replace_all_vars_(Vars,Vars1).

replace_all_vars(Input,[A|A1],Output) :-
	replace_all_vars_(Input,A,Output);
	dif(A1,[]),replace_all_vars(Input,A1,Output).

matches_any_([],_) :- false.
matches_any_([A|A1],B) :-
	subsumes_term(A,B),A=B;matches_any_(A1,B).
matches_any(A,B) :-
	nonvar(A),matches_any_(A,B).

find_all_clauses(Var,Var) :-
	\+callable(Var);var(Var);is_list(Var);matches_any([length(_,_),_>_,_<_,_=_,_==_,_\=_,_\==_,_=<_,_=<_,dif(_,_),_ is _,true,false,max_list(_,_),min_list(_,_),union(_,_,_),intersection(_,_,_),subset(_,_),all_different(_),all_distinct(_)],Var).

find_all_clauses(Var,Var2) :-
	subsumes_term(call(Var1),Var),
	call(Var1) = Var,
	find_all_clauses(Var1,Var2).

find_all_clauses(Var,Output) :-
	replace_all_vars(Var,[
		\+(A),
		not(A),
		(A;B),
		(A,B),
		var(A),
		append(A,B,C),
		foreach(A,B),
		forall(A,B),
		memberchk(A,B),
		member(A,B)
	],Output).

find_all_clauses(Predicate,Output) :-
    findall(Predicate1,clause(Predicate,Predicate1),Output1),
    list_to_disjunction(Output1,Output).

list_to_disjunction([A],A1) :- find_all_clauses(A,A1).
list_to_disjunction([A|B],(A1;B1)) :- list_to_disjunction([A],A1),list_to_disjunction(B,B1).
