
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

expand([X],[X]):-
  (
    t(A)=X;
    f(A)=X
  ),
  atom(A).



expand(Branch,RealNewBranch):-
  unaryExpansion(Branch,NewBranch),
  expand(NewBranch,RealNewBranch).


expand(Branch,RealNewBranch):-
  conjunctiveExpansion(Branch,NewBranch),
  [Left|RightBranch]=NewBranch,
  LeftBranch = [Left],
  expand(LeftBranch,NewLeftBranch),
  expand(RightBranch,NewRightBranch),
  (
    (
      [RightAtUp,LeftAtUp]=NewRightBranch,
      [RightAtDown,LeftAtDown]=NewLeftBranch,
      append([RightAtDown],[LeftAtDown],Deeper),
      append([Deeper],RightAtUp,L),
      append([Deeper],LeftAtUp,R),
      append([L],[R],RealNewBranch)
    );
    (
      \+([RightAtUp,LeftAtUp]=NewRightBranch),
      [RightAtDown,LeftAtDown]=NewLeftBranch,
      append([LeftAtDown],NewRightBranch,L),
      append([RightAtDown],NewRightBranch,R),
      append([R],[L],RealNewBranch)

    );
    (
      [RightAtUp,LeftAtUp]=NewRightBranch,
      \+([RightAtDown,LeftAtDown]=NewLeftBranch),
      append([NewLeftBranch],RightAtUp,L),
      append([NewLeftBranch],LeftAtUp,R),
      append([L],[R],RealNewBranch)

    );
    (
      \+([RightAtUp,LeftAtUp]=NewRightBranch),
      \+([RightAtDown,LeftAtDown]=NewLeftBranch),
      append(NewLeftBranch,NewRightBranch,RealNewBranch)
    )

  ).

expand(Branch,RealNewBranch):-
  disjunctiveExpansion(Branch,NewBranch),
  [LeftBranch|Right]= NewBranch,
  [RightBranch|_]=Right,
  expand(LeftBranch,NewLeftBranch),
  expand(RightBranch,NewRightBranch),
  append([NewLeftBranch],[NewRightBranch],RealNewBranch).

  

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
