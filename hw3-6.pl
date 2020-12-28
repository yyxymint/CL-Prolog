

:- module(foResolution,[info/0,
								infix/0,
								prefix/0,
								rprove/2,
								rprove/3,
								rproveTestSuite/0]).

:- use_module(comsemPredicates,[infix/0,
										  prefix/0,
										  memberList/2,
				appendLists/3,
										  unionSets/3,
				selectFromList/3]).

:- use_module(folTestSuite,[formula/2]).

:- use_module(cnfFOL,[cnf/2]).



rprove(Formula,MaxIter):-
	cnf(not(Formula),CNF),
	nonRedundantFactors(CNF,NRF),
	refute(NRF,MaxIter).

rprove(Formula,MaxIter,Result):-
	cnf(not(Formula),CNF),
	nonRedundantFactors(CNF,NRF),
	(
		refute(NRF,MaxIter), !,
		Result = 'theorem'
	;
		Result = 'unknown'
	).
		

rproveTestSuite:-
	format('~n~n>>>>> FOL RESOLUTION PROVER ON TEST SUITE <<<<<~n',[]),
	formula(Formula,Status),
	format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
	rprove(Formula,2,Result),
	format('~nProver: ~p~n',[Result]),
	fail.

rproveTestSuite.


refute(C,MaxIter):-
	memberList([],C).

refute(C,MaxIter):-
	\+ memberList([],C),
	resolveList(C,[],Output),
	unionSets(Output,C,NewC),
	\+ NewC == C,
  NewMaxIter is MaxIter - 1,
  NewMaxIter > 0 ,
	refute(NewC,NewMaxIter).


resolveList([],Acc,Acc).

resolveList([Clause|List],Acc1,Acc3):-
	resolveClauseList(List,Clause,Acc1,Acc2),
	resolveList(List,Acc2,Acc3).



resolveClauseList([],_,Acc,Acc).

resolveClauseList([H|L],Clause,Acc,Output):-
	resolve(Clause,H,Result0),
	nonRedundantFactors([Result0],Result), 
	unionSets(Result,Acc,NewAcc),
	resolveClauseList(L,Clause,NewAcc,Output).

resolveClauseList([H|L],Clause,Acc1,Acc2):-
	\+ resolve(Clause,H,_),
	resolveClauseList(L,Clause,Acc1,Acc2).


resolve(Clause1,Clause2,NewClause):-
	selectFromList(Lit1,Clause1,Temp1),
	selectFromList(not(Lit2),Clause2,Temp2), 
	unify_with_occurs_check(Lit1,Lit2),
	unionSets(Temp1,Temp2,NewClause).

resolve(Clause1,Clause2,NewClause):-
	selectFromList(not(Lit1),Clause1,Temp1),
	selectFromList(Lit2,Clause2,Temp2),
	unify_with_occurs_check(Lit1,Lit2),
	unionSets(Temp1,Temp2,NewClause).



nonRedundantFactors([],[]).

nonRedundantFactors([C1|L1],L4):-
	findall(C2,nonRedFact(C1,C2),L3),
	nonRedundantFactors(L1,L2),
	appendLists(L3,L2,L4).



nonRedFact([],[]).
	
nonRedFact([X|C1],C2):-
	memberList(Y,C1),
	unify_with_occurs_check(X,Y),
	nonRedFact(C1,C2).

nonRedFact([X|C1],[X|C2]):-
	nonRedFact(C1,C2).


info:-
	format('~n> -------------------------------------------------------------------- <',[]),
	format('~n> foResolution.pl, by Patrick Blackburn and Johan Bos						<',[]),
	format('~n>																							 <',[]),
	format('~n> ?- rprove(Formula).			- Succeeds if Formula is a theorem		 <',[]),
	format('~n> ?- rprove(Formula,Result).  - Try to prove Formula, return result	 <',[]),
	format('~n> ?- rproveTestSuite.			- Run the test suite for theorem proving <',[]),
	format('~n> ?- infix.						 - switches to infix display mode			<',[]),
	format('~n> ?- prefix.						- switches to prefix display mode		  <',[]),
	format('~n> ?- info.						  - show this information						<',[]),
	format('~n> -------------------------------------------------------------------- <',[]),
	format('~n~n',[]).



:- info.

