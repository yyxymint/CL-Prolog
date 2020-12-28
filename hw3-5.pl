

:- module(freeVarTabl,[info/0,
                       infix/0,
                       prefix/0,
                       tprove/1,
                       tprove/2,
		       tprove/3,
		       tproveTestSuite/0]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
				removeFirst/3,
				appendLists/3,
                                basicFormula/1,
                                compose/3,
				newFunctionCounter/1,
                                substitute/4]).

:- use_module(folTestSuite,[formula/2]).



closedTableau([],_Q):- !.

closedTableau(OldTableau,Qdepth):-
   expand(OldTableau,Qdepth,TempTableau,NewQdepth), !,
   removeClosedBranches(TempTableau,NewTableau),
   closedTableau(NewTableau,NewQdepth).


removeClosedBranches([],[]).

removeClosedBranches([Branch|Rest],Tableau):-
   closedBranch(Branch), !,
   removeClosedBranches(Rest,Tableau).

removeClosedBranches([Branch|Rest],[Branch|Tableau]):-
   removeClosedBranches(Rest,Tableau).



closedBranch(Branch):-
   memberList(n(_,t(X)),Branch),
   memberList(n(_,f(Y)),Branch),
   basicFormula(X), 
   basicFormula(Y),
   unify_with_occurs_check(X,Y).



skolemFunction(VarList,SkolemTerm):-
   newFunctionCounter(N),
   compose(SkolemTerm,fun,[N|VarList]).



tprove(X,Qdepth):-
   notatedFormula(NotatedFormula,[],f(X)),
   closedTableau([[NotatedFormula]],Qdepth).

tprove(X,Qdepth,Result):-
   notatedFormula(NotatedFormula,[],f(X)),
   (
      closedTableau([[NotatedFormula]],Qdepth), !,
      Result = theorem
   ;
      Result = unknown
   ).



tprove(X):-
  assert(countNum(1)),
  findall(N,countNum(N),Count),
  length(Count,L),
  (
    (
      tprove(X,L)
    );
    (
      \+(tprove(X,L)),
      tprove(X)
    )
  ).


test_iter(35,400).
test_iter(777,420).

test_iter(Y):-
  assert(countNum(1)),
  findall(X,countNum(X),Count),
  length(Count,L),
  format('test in ~p',[L]),
  (
    (
      Y is L,
      test_iter(L,_)
    );
    (
      test_iter(Y)
    )
  ).




notatedFormula(n(Free,Formula),Free,Formula).



tproveTestSuite:-
   format('~n~n>>>>> FREE VARIABLE TABLEAU ON TEST SUITE <<<<<',[]),
   formula(Formula,Status),
   format('~n~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
   tprove(Formula,100,Result), 
   format('~nProver: ~p~n',[Result]),
   fail.

tproveTestSuite.



expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   unaryExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   conjunctiveExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   existentialExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch1,NewBranch2|Tableau],QD):-
   disjunctiveExpansion(Branch,NewBranch1,NewBranch2).

expand([Branch|Tableau],OldQD,NewTableau,NewQD):-
   universalExpansion(Branch,OldQD,NewBranch,NewQD),
   appendLists(Tableau,[NewBranch],NewTableau).

expand([Branch|Rest],OldQD,[Branch|Newrest],NewQD):-
   expand(Rest,OldQD,Newrest,NewQD).


unaryExpansion(Branch,[NotatedComponent|Temp]) :-
   unary(SignedFormula,Component),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotatedComponent,Free,Component).


conjunctiveExpansion(Branch,[NotatedComp1,NotatedComp2|Temp]):-
   conjunctive(SignedFormula,Comp1,Comp2),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotatedComp1,Free,Comp1),
   notatedFormula(NotatedComp2,Free,Comp2).



disjunctiveExpansion(Branch,[NotComp1|Temp],[NotComp2|Temp]):-
   disjunctive(SignedFormula,Comp1,Comp2),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotComp1,Free,Comp1),
   notatedFormula(NotComp2,Free,Comp2).


existentialExpansion(Branch,[NotatedInstance|Temp]):-
   notatedFormula(NotatedFormula,Free,SignedFormula),
   existential(SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   skolemFunction(Free,Term),
   instance(SignedFormula,Term,Instance),
   notatedFormula(NotatedInstance,Free,Instance).



universalExpansion(Branch,OldQD,NewBranch,NewQD):-
   OldQD > 0, 
   NewQD is OldQD - 1,
   memberList(NotatedFormula,Branch),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   universal(SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   instance(SignedFormula,V,Instance),
   notatedFormula(NotatedInstance,[V|Free],Instance),
   appendLists([NotatedInstance|Temp],[NotatedFormula],NewBranch).



conjunctive(t(and(X,Y)),t(X),t(Y)).
conjunctive(f(or(X,Y)),f(X),f(Y)).
conjunctive(f(imp(X,Y)),t(X),f(Y)).



disjunctive(f(and(X,Y)),f(X),f(Y)).
disjunctive(t(or(X,Y)),t(X),t(Y)).
disjunctive(t(imp(X,Y)),f(X),t(Y)).



unary(t(not(X)),f(X)).
unary(f(not(X)),t(X)). 


universal(t(all(_,_))).
universal(f(some(_,_))).



existential(t(some(_,_))).
existential(f(all(_,_))).



instance(t(all(X,F)),Term,t(NewF)):- substitute(Term,X,F,NewF).
instance(f(some(X,F)),Term,f(NewF)):- substitute(Term,X,F,NewF).
instance(t(some(X,F)),Term,t(NewF)):- substitute(Term,X,F,NewF).
instance(f(all(X,F)),Term,f(NewF)):- substitute(Term,X,F,NewF).



info:-
   format('~n> ----------------------------------------------------------------------------- <',[]),
   format('~n> freeVarTabl.pl, by Patrick Blackburn and Johan Bos                            <',[]),
   format('~n>                                                                               <',[]),
   format('~n> ?- tprove(Form).            - Try to prove Form                               <',[]),
   format('~n> ?- tprove(Form,QDepth).     - Try to prove Form using QDepth                  <',[]),
   format('~n> ?- tprove(Form,QDepth,Res). - Tyr to prove Form using QDepth, return Res      <',[]),
   format('~n> ?- tproveTestSuite.         - runs the test suite for theorem proving         <',[]),
   format('~n> ?- infix.                   - switches to infix display mode                  <',[]),
   format('~n> ?- prefix.                  - switches to prefix display mode                 <',[]),
   format('~n> ?- info.                    - show this information                           <',[]),
   format('~n> ----------------------------------------------------------------------------- <',[]),
   format('~n~n',[]).



:- info.

