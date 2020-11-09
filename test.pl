
locate(1,6,rook,black).
locate(2,4,rook,black).
locate(2,5,rook,black).
locate(4,1,rook,black).
locate(5,8,rook,black).
locate(6,1,rook,black).
locate(6,3,rook,black).
locate(7,3,rook,black).
locate(8,6,rook,black).

locate(7,1,pawn,white).
locate(7,2,pawn,white).

opponentColor(black,white).
opponentColor(white,black).

betweenDiagonalRightUp(RowDown,ColDown,RowUp,ColUp,NewRow,NewCol):-
	between(RowUp,RowDown,Dist),
	Abs is Dist-RowUp,
	NewRow is Dist,
	NewCol is ColUp-Abs.

betweenDiagonalRightDown(RowDown,ColDown,RowUp,ColUp,NewRow,NewCol):-
	between(RowUp,RowDown,Dist),
	Abs is Dist-RowUp,
	NewRow is Dist,
	NewCol is ColUp+Abs.

canGo(NowRow,NowCol,NewRow,NewCol,knight,MyColor):-
	locate(NowRow,NowCol,knight,MyColor),
	knightCanGo(NowRow,NowCol,NewRow,NewCol,MyColor),
	\+locate(NewRow,NewCol,_,MyColor).

canAttack(NowRow,NowCol,NewRow,NewCol,knight,YourType,MyColor):-
	canGo(NowRow,NowCol,NewRow,NewCol,knight,MyColor),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor).

canGo(NowRow,NowCol,NewRow,NewCol,rook,MyColor):-
	locate(NowRow,NowCol,rook,MyColor),
	rookCanGo(NowRow,NowCol,NewRow,NewCol,MyColor),
	\+locate(NewRow,NewCol,_,MyColor).

canAttack(NowRow,NowCol,NewRow,NewCol,rook,YourType,MyColor):-
	canGo(NowRow,NowCol,NewRow,NewCol,rook,MyColor),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor).

canGo(NowRow,NowCol,NewRow,NewCol,bishop,MyColor):-
	locate(NowRow,NowCol,bishop,MyColor),
	bishopCanGo(NowRow,NowCol,NewRow,NewCol,MyColor),
	\+locate(NewRow,NewCol,_,MyColor).

canAttack(NowRow,NowCol,NewRow,NewCol,bishop,YourType,MyColor):-
	canGo(NowRow,NowCol,NewRow,NewCol,bishop,MyColor),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor).

canGo(NowRow,NowCol,NewRow,NewCol,queen,MyColor):-
	locate(NowRow,NowCol,queen,MyColor),
	queenCanGo(NowRow,NowCol,NewRow,NewCol,MyColor),
	\+locate(NewRow,NewCol,_,MyColor).

canAttack(NowRow,NowCol,NewRow,NewCol,queen,YourType,MyColor):-
	canGo(NowRow,NowCol,NewRow,NewCol,queen,MyColor),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor).

canGo(NowRow,NowCol,NewRow,NewCol,king,MyColor):-
	locate(NowRow,NowCol,king,MyColor),
	kingCanGo(NowRow,NowCol,NewRow,NewCol,MyColor),
	\+locate(NewRow,NewCol,_,MyColor).

canAttack(NowRow,NowCol,NewRow,NewCol,king,YourType,MyColor):-
	canGo(NowRow,NowCol,NewRow,NewCol,king,MyColor),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor).

canGo(NowRow,NowCol,NewRow,NewCol,pawn,MyColor):-
	locate(NowRow,NowCol,pawn,MyColor),
	pawnCanGo(NowRow,NowCol,NewRow,NewCol,MyColor),
	\+locate(NewRow,NewCol,_,MyColor).

canAttack(NowRow,NowCol,NewRow,NewCol,pawn,YourType,black):-
	locate(NowRow,NowCol,pawn,black),
	NewRow is NowRow+1,
	(
		NewCol is NowCol-1;
		NewCol is NowCol+1
	),
	locate(NewRow,NewCol,YourType,white),
	between(1,8,NewRow),
	between(1,8,NewCol).

canAttack(NowRow,NowCol,NewRow,NewCol,pawn,YourType,white):-
	locate(NowRow,NowCol,pawn,white),
	NewRow is NowRow-1,
	(
		NewCol is NowCol-1;
		NewCol is NowCol+1
	),
	locate(NewRow,NewCol,YourType,black),
	between(1,8,NewRow),
	between(1,8,NewCol).

knightCanGo(NowRow,NowCol,NewRow,NewCol,MyColor):-
	(
		NewRow is NowRow + 1, NewCol is NowCol + 2;
		NewRow is NowRow + 1, NewCol is NowCol - 2;
		NewRow is NowRow - 1, NewCol is NowCol + 2;
		NewRow is NowRow - 1, NewCol is NowCol - 2;
		NewRow is NowRow + 2, NewCol is NowCol + 1;
		NewRow is NowRow + 2, NewCol is NowCol - 1;
		NewRow is NowRow - 2, NewCol is NowCol + 1;
		NewRow is NowRow - 2, NewCol is NowCol - 1
	),
	between(1,8,NewRow),
	between(1,8,NewCol),
	\+locate(NewRow,NewCol,_,MyColor).

rookCanGo(NowRow,NowCol,NewRow,NewCol,MyColor):-
	(
		opponentColor(MyColor,YourColor),

		nearestRight(NowRow,NowCol,MyColor,MyColor,Nright1),
		nearestRight(NowRow,NowCol,MyColor,YourColor,Nright2),

		nearestLeft(NowRow,NowCol,MyColor,MyColor,Nleft1),
		nearestLeft(NowRow,NowCol,MyColor,YourColor,Nleft2),

		nearestUp(NowRow,NowCol,MyColor,MyColor,Nup1),
		nearestUp(NowRow,NowCol,MyColor,YourColor,Nup2),

		nearestDown(NowRow,NowCol,MyColor,MyColor,Ndown1),
		nearestDown(NowRow,NowCol,MyColor,YourColor,Ndown2),

		between(Nup1,Ndown1,NewRow),
		between(Nup1,Ndown2,NewRow),
		between(Nup2,Ndown1,NewRow),
		between(Nup2,Ndown2,NewRow),
		between(Nleft1,Nright1,NewCol),
		between(Nleft1,Nright2,NewCol),
		between(Nleft2,Nright1,NewCol),
		between(Nleft2,Nright2,NewCol)
	),
	(
		\+(
			(
				NewRow is NowRow,
				NewCol is NowCol
			)
		)
	),
	(
		NewRow is NowRow;
		NewCol is NowCol	
	).

bishopCanGo(NowRow,NowCol,NewRow,NewCol,MyColor):-
	
	opponentColor(MyColor,YourColor),

	nearestRightUp(NowRow,NowCol,MyColor,MyColor,NrightupRow1,NrightupCol1),
	nearestRightUp(NowRow,NowCol,MyColor,YourColor,NrightupRow2,NrightupCol2),

	nearestLeftUp(NowRow,NowCol,MyColor,MyColor,NleftupRow1,NleftupCol1),
	nearestLeftUp(NowRow,NowCol,MyColor,YourColor,NleftupRow2,NleftupCol2),

	nearestRightDown(NowRow,NowCol,MyColor,MyColor,NrightdownRow1,NrightdownCol1),
	nearestRightDown(NowRow,NowCol,MyColor,YourColor,NrightdownRow2,NrightdownCol2),

	nearestLeftDown(NowRow,NowCol,MyColor,MyColor,NleftdownRow1,NleftdownCol1),
	nearestLeftDown(NowRow,NowCol,MyColor,YourColor,NleftdownRow2,NleftdownCol2),
	
	(
		(
			betweenDiagonalRightUp(NleftdownRow1,NleftdownCol1,NrightupRow1,NrightupCol1,NewRow,NewCol),
			betweenDiagonalRightUp(NleftdownRow1,NleftdownCol1,NrightupRow2,NrightupCol2,NewRow,NewCol),
			betweenDiagonalRightUp(NleftdownRow2,NleftdownCol2,NrightupRow1,NrightupCol1,NewRow,NewCol),
			betweenDiagonalRightUp(NleftdownRow2,NleftdownCol2,NrightupRow2,NrightupCol2,NewRow,NewCol)
		);
		(
			betweenDiagonalRightDown(NrightdownRow1,NrightdownCol1,NleftupRow1,NleftupCol1,NewRow,NewCol),
			betweenDiagonalRightDown(NrightdownRow1,NrightdownCol1,NleftupRow2,NleftupCol2,NewRow,NewCol),
			betweenDiagonalRightDown(NrightdownRow2,NrightdownCol2,NleftupRow1,NleftupCol1,NewRow,NewCol),
			betweenDiagonalRightDown(NrightdownRow2,NrightdownCol2,NleftupRow2,NleftupCol2,NewRow,NewCol)
		)
	),
	(
		\+(
			(
				NewRow is NowRow,
				NewCol is NowCol
			)
		)
	).

queenCanGo(NowRow,NowCol,NewRow,NewCol,MyColor):-
	rookCanGo(NowRow,NowCol,NewRow,NewCol,MyColor);
	bishopCanGo(NowRow,NowCol,NewRow,NewCol,MyColor).

kingCanGo(NowRow,NowCol,NewRow,NewCol,MyColor):-
	(
		NewRow is NowRow - 1, NewCol is NowCol - 1;
		NewRow is NowRow - 1, NewCol is NowCol;
		NewRow is NowRow - 1, NewCol is NowCol + 1;
		NewRow is NowRow , NewCol is NowCol - 1;
		NewRow is NowRow , NewCol is NowCol + 1;
		NewRow is NowRow + 1, NewCol is NowCol - 1;
		NewRow is NowRow + 1, NewCol is NowCol;
		NewRow is NowRow + 1, NewCol is NowCol + 1
	),
	between(1,8,NewRow),
	between(1,8,NewCol),
	\+locate(NewRow,NewCol,_,MyColor).

pawnCanGo(NowRow,NowCol,NewRow,NewCol,black):-
	NewCol is NowCol,
	(
		(
			NowRow is 2,
			PRow is NowRow+1,
			NewRow is NowRow+2,
			\+locate(PRow,NewCol,_,_),
			\+locate(NewRow,NewCol,_,_)
		);
		(
			NewRow is NowRow+1,
			\+locate(NewRow,NewCol,_,_)
		)
	),
	between(1,8,NewRow).

pawnCanGo(NowRow,NowCol,NewRow,NewCol,white):-
	NewCol is NowCol,
	(
		(
			NowRow is 7,
			PRow is NowRow-1,
			NewRow is NowRow-2,
			\+locate(PRow,NewCol,_,_),
			\+locate(NewRow,NewCol,_,_)
		);
		(
			NewRow is NowRow-1,
			\+locate(NewRow,NewCol,_,_)
		)
	),
	between(1,8,NewRow).

nearestRight(Row,Col,MyColor,YourColor,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col+1,
		locate(Row,Pcol,_,MyColor)
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 8,
		Col >= 8
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	nearestRight(Row,Pcol,MyColor,YourColor,Ncol).




nearestRight(Row,Col,MyColor,YourColor,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col,
		locate(Row,Pcol,_,YourColor)
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 8,
		Col >= 8
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol):-
	opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	nearestRight(Row,Pcol,MyColor,YourColor,Ncol).


nearestLeft(Row,Col,MyColor,YourColor,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col-1,
		locate(Row,Pcol,_,MyColor)
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 1,
		Col =< 1
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	nearestLeft(Row,Pcol,MyColor,YourColor,Ncol).




nearestLeft(Row,Col,MyColor,YourColor,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col,
		locate(Row,Pcol,_,YourColor)
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 1,
		Col =< 1
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol):-
	opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	nearestLeft(Row,Pcol,MyColor,YourColor,Ncol).

nearestUp(Row,Col,MyColor,YourColor,Nrow):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row-1,
		locate(Prow,Col,_,MyColor)
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is 1,
		Row =< 1
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow):-
	\+opponentColor(MyColor,YourColor),
	Prow is Row-1,
	Prow >= 1,
	nearestUp(Prow,Col,MyColor,YourColor,Nrow).




nearestUp(Row,Col,MyColor,YourColor,Nrow):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row,
		locate(Prow,Col,_,YourColor)
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is 1,
		Row =< 1
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow):-
	opponentColor(MyColor,YourColor),
	Prow is Row-1,
	Prow >= 1,
	nearestUp(Prow,Col,MyColor,YourColor,Nrow).

nearestDown(Row,Col,MyColor,YourColor,Nrow):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row+1,
		locate(Prow,Col,_,MyColor)
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is 8,
		Row >= 8
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow):-
	\+opponentColor(MyColor,YourColor),
	Prow is Row+1,
	Prow =< 8,
	nearestDown(Prow,Col,MyColor,YourColor,Nrow).




nearestDown(Row,Col,MyColor,YourColor,Nrow):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row,
		locate(Prow,Col,_,YourColor)
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is 8,
		Row >= 8
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow):-
	opponentColor(MyColor,YourColor),
	Prow is Row+1,
	Prow =< 8,
	nearestDown(Prow,Col,MyColor,YourColor,Nrow).

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col+1,
		Prow is Row+1,
		locate(Prow,Pcol,_,MyColor)
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row =< 8,
		Col >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col =< 8,
		Row >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row+1,
	Prow =< 8,
	nearestRightDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor)
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row =< 8,
		Col >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col =< 8,
		Row >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row+1,
	Prow =< 8,
	nearestRightDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).




nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col+1,
		Prow is Row-1,
		locate(Prow,Pcol,_,MyColor)
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row >= 1,
		Col >= 8
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col =< 8,
		Row =< 1
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row-1,
	Prow >= 1,
	nearestRightUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).




nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor)
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row >= 1,
		Col >= 8
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col =< 8,
		Row =< 1
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row-1,
	Prow >= 1,
	nearestRightUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).



nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col-1,
		Prow is Row-1,
		locate(Prow,Pcol,_,MyColor)
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row >= 1,
		Col =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col >= 1,
		Row =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row-1,
	Prow >= 1,
	nearestLeftUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).




nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor)
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row >= 1,
		Col =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col >= 1,
		Row =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row-1,
	Prow >= 1,
	nearestLeftUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).


nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col-1,
		Prow is Row+1,
		locate(Prow,Pcol,_,MyColor)
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row =< 8,
		Col =< 1
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col >= 1,
		Row >= 8
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row+1,
	Prow =< 8,
	nearestLeftDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).




nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor)
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row =< 8,
		Col =< 1
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col >= 1,
		Row >= 8
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol):-
	opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row+1,
	Prow =< 8,
	nearestLeftDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol).
