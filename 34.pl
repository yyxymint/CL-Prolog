
treeToList([],SubList,SubList).

treeToList([H|T],Paper,SubList):-
  (
    t(_)=H;
    f(_)=H
  ),!,
  append(Paper,[H],NewPaper),
  treeToList(T,NewPaper,SubList).

treeToList([H|T],Paper,SubList):-
  \+(
    t(_)=H;
    f(_)=H
  ),!,
  (
    (
      treeToList(H,[],HPaper),
      append(Paper,HPaper,SubList)
    );
    (
      \+(T = []),
      treeToList(T,[],HPaper),
      append(Paper,HPaper,SubList) 
    )
  ).



expand([Branch|Tableau],[NewBranch|Tableau]):-
   unaryExpansion(Branch,NewBranch), !.

expand([Branch|Tableau],[NewBranch|Tableau]):-
   conjunctiveExpansion(Branch,NewBranch), !.

expand([Branch|Tableau],[NewBranch|Tableau]):-
   disjunctiveExpansion(Branch,NewBranch), !.

expand([Branch|Tableau],[[ExpandedInList1|InListRest]|Tableau]):-
  [InList|InListRest] = Branch,
  expand(InList,ExpandedInList1), !.

expand([Branch|Tableau],[[ExpandedInListIn|ExpandedInList2]|Tableau]):-
  [InList|InListRest] = Branch,
  expand([InList],ExpandedInList1), 
  [ExpandedInListIn|_]=ExpandedInList1,
  expand(InListRest,ExpandedInList2), !.

expand([Branch|Rest],[Branch|Newrest]):-
   expand(Rest,Newrest).




unaryExpansion(Branch,[Component|Temp]):-
   unary(SignedFormula,Component),
   removeFirst(SignedFormula,Branch,Temp).

conjunctiveExpansion(Branch,[Comp2,Comp1|Temp]):-
   conjunctive(SignedFormula,Comp1,Comp2),
   removeFirst(SignedFormula,Branch,Temp).

disjunctiveExpansion(Branch,[[Comp2],[Comp1]|Temp]):-
   disjunctive(SignedFormula,Comp1,Comp2),
   removeFirst(SignedFormula,Branch,Temp).




conjunctive(t(and(X,Y)),t(X),t(Y)).
conjunctive(f(or(X,Y)),f(X),f(Y)).
conjunctive(f(imp(X,Y)),t(X),f(Y)).

disjunctive(f(and(X,Y)),f(X),f(Y)).
disjunctive(t(or(X,Y)),t(X),t(Y)).
disjunctive(t(imp(X,Y)),f(X),t(Y)).

unary(t(not(X)),f(X)).
unary(f(not(X)),t(X)). 


removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).
