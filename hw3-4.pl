nnf(not(and(F1,F2)), or(N1,N2)) :-
	nnf(not(F1), N1),
	nnf(not(F2), N2).

nnf(and(F1,F2), and(N1,N2)) :-
	nnf(F1, N1),
	nnf(F2, N2).

nnf(not(or(F1,F2)), and(N1,N2)) :-
	nnf(not(F1), N1),
	nnf(not(F2), N2).

nnf(or(F1,F2), or(N1,N2)) :-
	nnf(F1, N1),
	nnf(F2, N2).

nnf(not(imp(F1,F2)),and(N1,N2)) :-
	nnf(F1,N1),
	nnf(not(F2),N2).

nnf(imp(F1,F2),or(N1,N2)) :-
	nnf(not(F1),N1),
	nnf(F2,N2).

nnf(not(not(F1)),N1) :-
	nnf(F1,N1).

nnf(bimp(F1,F2),or(and(N1,N2),and(N3,N4))) :-
	nnf(F1,N1),
	nnf(F2,N2),
	nnf(not(F1),N3),
	nnf(not(F2),N4).

nnf(not(bimp(F1,F2)),and(or(N3,N4),or(N1,N2))) :-
	nnf(F1,N1),
	nnf(F2,N2),
	nnf(not(F1),N3),
	nnf(not(F2),N4).

nnf(F1,F1) :-
	literal(F1).

literal(not(F)) :- atomic(F).
literal(F) :- atomic(F).