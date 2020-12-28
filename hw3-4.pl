
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


expand(Branch,RealNewBranch):-
  unaryExpansion(Branch,NewBranch), 
  (
    (
     expand(NewBranch,NewBranch2),
     RealNewBranch = NewBranch2
    );
    (
      \+(expand(NewBranch,NewBranch2)),
      RealNewBranch = NewBranch
    )
  ).


expand(Branch,RealNewBranch):-
  conjunctiveExpansion(Branch,NewBranch),
  [Left|RightBranch]=NewBranch,
  LeftBranch = [Left],
  (
    (
      expand(LeftBranch,NewLeftBranch),
      expand(RightBranch,NewRightBranch),
      append(NewLeftBranch,NewRightBranch,RealNewBranch)
    );
    (
      \+(expand(LeftBranch,NewLeftBranch)),
      expand(RightBranch,NewRightBranch),
      append(LeftBranch,NewRightBranch,RealNewBranch)
    );
    (
      expand(LeftBranch,NewLeftBranch),
      \+(expand(RightBranch,NewRightBranch)),
      append(NewLeftBranch,RightBranch,RealNewBranch)
    );
    (
      \+(expand(LeftBranch,NewLeftBranch)),
      \+(expand(RightBranch,NewRightBranch)),
      append(LeftBranch,RightBranch,RealNewBranch)
    )
  ).

expand(Branch,RealNewBranch):-
  disjunctiveExpansion(Branch,NewBranch),
  [LeftBranch|Right]= NewBranch,
  [RightBranch|_]=Right,
  (
    (
      expand(LeftBranch,NewLeftBranch),
      expand(RightBranch,NewRightBranch),
      append([NewLeftBranch],[NewRightBranch],RealNewBranch)
    );
    (
      \+(expand(LeftBranch,NewLeftBranch)),
      expand(RightBranch,NewRightBranch),
      append([LeftBranch],[NewRightBranch],RealNewBranch)
    );
    (
      expand(LeftBranch,NewLeftBranch),
      \+(expand(RightBranch,NewRightBranch)),
      append([NewLeftBranch],[RightBranch],RealNewBranch)
    );
    (
      \+(expand(LeftBranch,NewLeftBranch)),
      \+(expand(RightBranch,NewRightBranch)),
      append([LeftBranch],[RightBranch],RealNewBranch)
    )
  ).



  

removeBracket(MainList,Count,AnsCount,Result):-
  MainList = [InsideList],!,
  NewCount is Count + 1,
  removeBracket(InsideList,NewCount,AnsCount,Result).

removeBracket(MainList,Count,Count,MainList):-
  \+(MainList = [_]).


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
