yes([A,B,C,D,E|_]):-
	20 is A+B+C+D+E;
	35 is A+B+C+D+E;
	490 is A+B+C+D+E.

closer(Y,L,Ans):-
	(
		Ans = L,
		yes(L)
	)
	;
	(
		NewX is Y + 1,
		closer(NewX,[Y|L],Ans)
	).
