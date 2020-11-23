/*************************************************************************

    File: foResolution.pl
    Copyright (C) 2004,2005,2006 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.3 (November 2006).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(foResolution,[info/0,
                        infix/0,
                        prefix/0,
                        rprove/1,
                        rprove/2,
                        rproveTestSuite/0]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
				appendLists/3,
                                unionSets/3,
				selectFromList/3]).

:- use_module(folTestSuite,[formula/2]).

:- use_module(cnfFOL,[cnf/2]).


/*========================================================================
   Main Predicate
========================================================================*/

rprove(Formula):-
   cnf(not(Formula),CNF),
   nonRedundantFactors(CNF,NRF),
   refute(NRF).

rprove(Formula,Result):-
   cnf(not(Formula),CNF),
   nonRedundantFactors(CNF,NRF),
   (
      refute(NRF), !,
      Result = 'theorem'
   ;
      Result = 'not a theorem'
   ).
      

/*========================================================================
   Try all formulas from the test suite
========================================================================*/

rproveTestSuite:-
   format('~n~n>>>>> FOL RESOLUTION PROVER ON TEST SUITE <<<<<~n',[]),
   formula(Formula,Status),
   format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
   rprove(Formula,Result), 
   format('~nProver: ~p~n',[Result]),
   fail.

rproveTestSuite.


/*========================================================================
   Refute
========================================================================*/

refute(C):-
   memberList([],C).

refute(C):-
   \+ memberList([],C),
   resolveList(C,[],Output),
   unionSets(Output,C,NewC),
   \+ NewC == C,
   refute(NewC).


/*========================================================================
   Resolve a list against a list
========================================================================*/

resolveList([],Acc,Acc).

resolveList([Clause|List],Acc1,Acc3):-
   resolveClauseList(List,Clause,Acc1,Acc2),
   resolveList(List,Acc2,Acc3).


/*========================================================================
   Resolve a clause against a list
========================================================================*/

resolveClauseList([],_,Acc,Acc).

resolveClauseList([H|L],Clause,Acc,Output):-
   resolve(Clause,H,Result0),
   nonRedundantFactors([Result0],Result), 
   unionSets(Result,Acc,NewAcc),
   resolveClauseList(L,Clause,NewAcc,Output).

resolveClauseList([H|L],Clause,Acc1,Acc2):-
   \+ resolve(Clause,H,_),
   resolveClauseList(L,Clause,Acc1,Acc2).


/*========================================================================
   Resolve two clauses
========================================================================*/

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


/*========================================================================
   Compute Non-Redundant Factors for a list of clauses
========================================================================*/
/* Aiden */
/*
subsume(L1,L2) :-
	subsume(L1,[],L2).

subsume([],L2,L2).

subsume([C1|L1],L2,L3) :-
	nonRedFact(C1,C2),
	(
		\+ memberList(C2,L2),
		appendLists(L2,[C2],L4),
		subsume(L1,L4,L3)
		;
		subsume(L1,L2,L3)
	).
*/
/* ��ġ�� */
/*
removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).

subsume(L, L1) :-
	memberList(X, L),
	memberList(Y, L),
	\+ X=Y,
	copy_list(X, X1),
	copy_list(Y, Y1),
	subsume_clause(X1, Y1),
	!,
	removeFirst(Y, L, L2),
	subsume(L2, L1).

subsume(L, L).
subsume_clause([], _).

subsume_clause([X|C1], C2) :-
	memberList(Y, C2),
	unify_with_occurs_check(X, Y),
	removeFirst(Y, C2, C),
	subsume_clause(C1, C).

copy_list([], []).
copy_list([X|L], [X1|L1]) :- copy_term(X, X1), copy_list(L, L1).
*/
/* Nam */
/*
subsume([],[]).
subsume([X|L],Thinned):-
	thinable(X),!,
	subsume(L,Thinned).
subsume([X|L],[X|Thinned]):-
	\+thinable(X),subsume(L,Thinned).

thinable(Clause):-
	memberList(X,Clause),
	memberList(Y,Clause),
	\+X==Y,
	unify_with_occurs_check(X,Y).
*/
/* �ſ��� - ��� ����� �� ���� */

subsume([],[]).
subsume([C1|L1],L5) :-
   findall(C2,subS(C1,C2),L3),
   subsume(L1,L2),
   appendLists(L3,L2,L5).

subS([],[]).
subS([X|C1],C2) :-
   memberList(Y,C1),
   unify_with_occurs_check(X,Y),
   subS(C1,C2).
subS([X|C1],[X|C2]) :-
   subS(C1,C2).

/* ��μ� - unify0 (���ǵ��� ����) */
/*
subsume(F1,F2) :-
	\+ (inst_vars(F2), 
    \+ (unify_with_occurs_check(F1,F2))).
inst_vars(F) :-
	inst_vars0(F,v(0),_).
inst_vars0(F,I,v(I)) :-
	var(F),!,
	F = I.
inst_vars0([],I,I) :- !.
inst_vars0([H|T],I,IOut) :- !,
	inst_vars0(H,I,I0),
	inst_vars0(T,I0,IOut).
inst_vars0(_F:V,I,IOut) :- !,
	inst_vars0(V,I,IOut).
inst_vars0(_,I,I).
*/
/* ����� */
/*
subsume([],[]).

subsume([T1|L], L2):-
	(
	unifyable(T1,L), subsume(L, L2)
	;
	\+ unifyable(T1,L), subsume(L, L3), L3=[T1|L2]
	).

unifyable(_,[]):-fail.
unifyable(T1,[T2|_]):-unify_with_occurs_check(T1,T2).
unifyable(T1,[_|L]):-unifyable(T1,L).
*/
/* ���ä */
/*
subsume(X,_) :-
	subsume_iter(X,X,[]). /*choose L4 itself to find clauses which subsumes by other clauses */

subsume_iter([],_,_). /* iterate till the L4 goes empty */

subsume_iter([X|Xtail],Z,Y) :- /* for each clause, check subsumption with other clauses of L4 */
	unify_iter(X,Z), 
	subsume_iter(Xtail,Z,Y). /* if subsumption is found, discard that clause */

subsume_iter([X|Xtail],Z,Y) :-
	\+ unify_iter(X,Z),
	subsume_iter(Xtail,Z,[X|Y]). /* if subsumption is not found, add that clause to L5 */


unify_iter(_,[]). /* iterate till the end of clauses of L4 */

unify_iter(X,[X|Ztail]) :- /* skip the clause itself */
	unify_iter(X,Ztail).

unify_iter(X,[Z|Ztail]) :- /* take unify_with_occurs_check for each clause */
	\+ X=Z,
	unify_with_occurs_check(X,Z),
	unify_iter(X,Ztail).
*/
/* ������ */
/*
subsume([],[]).

subsume([F1|List1],RList) :- 
	chk_more_unification(F1),
	RList = List2,subsume(List1, List2).

subsume([F1|List1],RList) :- 
	\+ chk_more_unification(F1),
	RList = [F1|List2], 
	subsume(List1, List2).

chk_more_unification([]) :- 
	false.

chk_more_unification([H|T]) :- 
	memberList(Y,T), H=Y.

chk_more_unification([_|T]) :- 
	chk_more_unification(T).
*/



nonRedundantFactors([],[]).

nonRedundantFactors([C1|L1],L5):-
   findall(C2,nonRedFact(C1,C2),L3),
   nonRedundantFactors(L1,L2),
   appendLists(L3,L2,L4),
   subsume(L4,L5).



/*========================================================================
   Compute Non-Redundant Factors for a Clause
========================================================================*/

nonRedFact([],[]).
   
nonRedFact([X|C1],C2):-
   memberList(Y,C1),
   unify_with_occurs_check(X,Y),
   nonRedFact(C1,C2).

nonRedFact([X|C1],[X|C2]):-
   nonRedFact(C1,C2).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> -------------------------------------------------------------------- <',[]),
   format('~n> foResolution.pl, by Patrick Blackburn and Johan Bos                  <',[]),
   format('~n>                                                                      <',[]),
   format('~n> ?- rprove(Formula).         - Succeeds if Formula is a theorem       <',[]),
   format('~n> ?- rprove(Formula,Result).  - Try to prove Formula, return result    <',[]),
   format('~n> ?- rproveTestSuite.         - Run the test suite for theorem proving <',[]),
   format('~n> ?- infix.                   - switches to infix display mode         <',[]),
   format('~n> ?- prefix.                  - switches to prefix display mode        <',[]),
   format('~n> ?- info.                    - show this information                  <',[]),
   format('~n> -------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

