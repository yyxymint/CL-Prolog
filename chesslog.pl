:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- style_check(-singleton).
:- style_check(-discontiguous).

dcgLocate(MyColor,Board) 
--> wordColor(MyColor),wordType(MyType),[is,located,at],wordRow(Row),wordCol(Col),
	{
		locate(Row,Col,MyType,MyColor,Board)
	}.

dcgCanAttack(MyColor,Board) 
--> wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),[can,attack],
	wordColor(YourColor),wordType(YourType),[at],wordRow(NewRow),wordCol(NewCol),
	{
		opponentColor(MyColor,YourColor),
		canAttack(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,Board)
	}.

dcgMakeCheckmate(MyColor,Board)
--> wordColor(MyColor),[can,make,checkmate,by,moving],wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
	[to],wordRow(NewRow),wordCol(NewCol),
	{
		makeCheckmate(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board)
	}.

dcgCanGo(MyColor,Board)
--> wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),[can,move,to],
	wordRow(NewRow),wordCol(NewCol),
	{
		canGo(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board)
	}.

dcgSummarize(MyColor,Board)
--> 
	(
		wordColor(MyColor),[did,not,lose,any,piece],
		{
			countTypeAll(MyColor,0,0,0,0,0,Board)
		}
	),!;
	(
		wordColor(MyColor),[lost],dcgSummarizeQueen(MyColor,Board),dcgSummarizeRook(MyColor,Board),dcgSummarizeBishop(MyColor,Board),
		dcgSummarizeKnight(MyColor,Board),dcgSummarizePawn(MyColor,Board)
	).

dcgSummarizeQueen(MyColor,Board)
--> 
	(
		[1,queen],
		{
			countType(MyColor,CountQueen,queen,Board),
			LoseQueen is 1-CountQueen,
			LoseQueen is 1
		}
	);
	(
		[LoseQueen,queens],
		{
			countType(MyColor,CountQueen,queen,Board),
			LoseQueen is 1-CountQueen,
			\+(LoseQueen is 1)
		}
	).

dcgSummarizeRook(MyColor,Board)
--> 
	(
		[1,rook],
		{
			countType(MyColor,CountRook,rook,Board),
			LoseRook is 2-CountRook,
			LoseRook is 1
		}
	);
	(
		[LoseRook,rooks],
		{
			countType(MyColor,CountRook,rook,Board),
			LoseRook is 2-CountRook,
			\+(LoseRook is 1)
		}
	).

dcgSummarizeBishop(MyColor,Board)
--> 
	(
		[1,bishop],
		{
			countType(MyColor,CountBishop,bishop,Board),
			LoseBishop is 2-CountBishop,
			LoseBishop is 1
		}
	);
	(
		[LoseBishop,bishops],
		{
			countType(MyColor,CountBishop,bishop,Board),
			LoseBishop is 2-CountBishop,
			\+(LoseBishop is 1)
		}
	).

dcgSummarizeKnight(MyColor,Board)
--> 
	(
		[1,knight],
		{
			countType(MyColor,CountKnight,knight,Board),
			LoseKnight is 2-CountKnight,
			LoseKnight is 1
		}
	);
	(
		[LoseKnight,knights],
		{
			countType(MyColor,CountKnight,knight,Board),
			LoseKnight is 2-CountKnight,
			\+(LoseKnight is 1)
		}
	).

dcgSummarizePawn(MyColor,Board)
--> 
	(
		[and,1,pawn],
		{
			countType(MyColor,CountPawn,pawn,Board),
			LosePawn is 8-CountPawn,
			LosePawn is 1
		}
	);
	(
		[and,LosePawn,pawns],
		{
			countType(MyColor,CountPawn,pawn,Board),
			LosePawn is 8-CountPawn,
			\+(LosePawn is 1)
		}
	).

dcgUnderAttack(MyColor,Board)
-->
	(
		{
			underAttack(OppoRow,OppoCol,NowRow,NowCol,MyType,YourType,MyColor,Board),
			escapeUnderAttack(NowRow,NowCol,NowRow2,NowCol2,NewRow,NewCol,OppoRow,OppoCol,MyType,MyType2,YourType,MyColor,Board),
			opponentColor(MyColor,YourColor)
		},
		wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),[is,now,under,attack,by],wordColor(YourColor),wordType(YourType),
		[at],wordRow(OppoRow),wordCol(OppoCol),[but,it,can,escape,attack,by,moving],wordColor(MyColor),wordType(MyType2),[at],wordRow(NowRow2),wordCol(NowCol2),
		[to],wordRow(NewRow),wordCol(NewCol)
	);
	(
		{
			underAttack(OppoRow,OppoCol,NowRow,NowCol,MyType,YourType,MyColor,Board),
			\+(escapeUnderAttack(NowRow,NowCol,NowRow2,NowCol2,NewRow,NewCol,OppoRow,OppoCol,MyType,MyType2,YourType,MyColor,Board)),
			opponentColor(MyColor,YourColor)
		},
		wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),[is,now,under,attack,by],wordColor(YourColor),wordType(YourType),
		[at],wordRow(OppoRow),wordCol(OppoCol),[but,it,can,not,escape,attack]
	).


dcgMoveAndAttack(MyColor,Board) 
--> [if],wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),[moves,to],wordRow(NewRow),wordCol(NewCol),
	[then],wordColor(MyColor),[can],[attack],wordColor(YourColor),wordType(YourType),[at],wordRow(OppoRow),wordCol(OppoCol),
	{
		opponentColor(MyColor,YourColor),
		moveAndAttack(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyType,YourType,MyColor,Board)
	}.

dcgMoveAndGetPoint(StartPoint,MyColor,Board)
--> dcgMoveAndGetPoint(StartPoint,MyColor,[10,7,6,5,2],Board).

dcgMoveAndGetPoint(StartPoint,MyColor,PointList,Board)
--> dcgMoveAndGetPoint(StartPoint,StartPoint,MyColor,PointList,Board).

dcgMoveAndGetPoint(StartPoint,WatchingPoint,MyColor,PointList,Board)
--> 
	(
		{
			opponentColor(MyColor,YourColor),
			moveAndGetPointHappy(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyRow,MyCol,OppoRow2,OppoCol2,MyType2,MyType,YourType,YourType2,MyColor,WatchingPoint,Point,SubPoint,PointList,Board),
			\+(MyType2 = none)
		},
		wordColor(MyColor),[can,expect],[WatchingPoint],wordPoint(WatchingPoint),[by,moving],wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
		[to],wordRow(NewRow),wordCol(NewCol),[and,attack,safely],wordColor(YourColor),wordType(YourType),[at],wordRow(OppoRow),wordCol(OppoCol),[worth,Point],wordPoint(Point),[also,if,it,is,attacked,after,moving,by],wordColor(YourColor),wordType(YourType2),
		[at],wordRow(OppoRow2),wordCol(OppoCol2),wordColor(MyColor),[can,get,SubPoint],wordPoint(SubPoint),[by,attack],wordColor(YourColor),wordType(YourType2),[again],
		[by,moving],wordType(MyType2),[at],wordRow(MyRow),wordCol(MyCol)
	);
	(
		{
			opponentColor(MyColor,YourColor),
			moveAndGetPointHappy(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyRow,MyCol,OppoRow2,OppoCol2,MyType2,MyType,YourType,YourType2,MyColor,WatchingPoint,Point,SubPoint,PointList,Board),
			MyType2 = none
		},
		wordColor(MyColor),[can,expect],[WatchingPoint],wordPoint(WatchingPoint),[by,moving],wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
		[to],wordRow(NewRow),wordCol(NewCol),[and,attack,safely],wordColor(YourColor),wordType(YourType),[at],wordRow(OppoRow),wordCol(OppoCol),[worth,Point],wordPoint(Point)
	);
	(
		{
			opponentColor(MyColor,YourColor),
			moveAndGetPointSad(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyRow,MyCol,OppoRow2,OppoCol2,MyType2,MyType,YourType,YourType2,MyColor,WatchingPoint,Point,SubPoint,PointList,Board),
			\+(MyType2 = none)
		},
		wordColor(MyColor),[can,expect],[WatchingPoint],wordPoint(WatchingPoint),[by,moving],wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
		[to],wordRow(NewRow),wordCol(NewCol),[and,attack,sadly],wordColor(YourColor),wordType(YourType),[at],wordRow(OppoRow),wordCol(OppoCol),[worth,Point],wordPoint(Point),[also,if,it,is,attacked,after,moving,by],wordColor(YourColor),wordType(YourType2),
		[at],wordRow(OppoRow2),wordCol(OppoCol2),wordColor(MyColor),[can,get,SubPoint],wordPoint(SubPoint),[by,attack],wordColor(YourColor),wordType(YourType2),[again],
		[by,moving],wordType(MyType2),[at],wordRow(MyRow),wordCol(MyCol)
	);
	(
		{
			opponentColor(MyColor,YourColor),
			moveAndGetPointSad(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyRow,MyCol,OppoRow2,OppoCol2,MyType2,MyType,YourType,YourType2,MyColor,WatchingPoint,Point,SubPoint,PointList,Board),
			MyType2 = none
		},
		wordColor(MyColor),[can,expect],[WatchingPoint],wordPoint(WatchingPoint),[by,moving],wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
		[to],wordRow(NewRow),wordCol(NewCol),[and,attack,sadly],wordColor(YourColor),wordType(YourType),[at],wordRow(OppoRow),wordCol(OppoCol),[worth,Point],wordPoint(Point)
	);
	(
		{
			WatchingPoint > -StartPoint,
			NewPoint is WatchingPoint - 1
		},
		dcgMoveAndGetPoint(StartPoint,NewPoint,MyColor,PointList,Board)
	).



dcgChecked(MyColor,Board)
--> wordColor(MyColor),[king,at],wordRow(NowKingRow),wordCol(NowKingCol),[is,checked,by],wordColor(YourColor),wordType(YourType),
	[at],wordRow(OppoRow),wordCol(OppoCol),[and],dcgAfterChecked(MyColor,Board),
	{
		opponentColor(MyColor,YourColor),
		checked(OppoRow,OppoCol,NowKingRow,NowKingCol,YourType,MyColor,Board)
	}.

dcgAfterChecked(MyColor,Board)
--> 
	(
		wordColor(MyColor),[can,escape,check],[by,moving],wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
		[to],wordRow(NewRow),wordCol(NewCol),
		{escapeCheck(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board)}
	);
	(
		[it,is,checkmate,so],wordColor(MyColor),[cannot,save,the,king,and],wordColor(YourColor),[wins],
		{
			opponentColor(MyColor,YourColor),
			checkmate(MyColor,Board)
		}
	),!.

dcgCanGetPoint(StartPoint,MyColor,Board)
--> dcgCanGetPoint(StartPoint,MyColor,[10,7,6,5,2],Board).

dcgCanGetPoint(StartPoint,MyColor,PointList,Board)
--> dcgCanGetPoint(StartPoint,StartPoint,MyColor,PointList,Board).

dcgCanGetPoint(StartPoint,Point,MyColor,PointList,Board)
--> 
	(
		wordColor(MyColor),[can,get],[Point],wordPoint(Point),[by,moving],wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
		[to],wordRow(NewRow),wordCol(NewCol),[and,take],wordColor(YourColor),wordType(YourType),[safely],
		{
			canGetPointHappy(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,Point,PointList,Board),
			opponentColor(MyColor,YourColor),
			locate(NewRow,NewCol,YourType,YourColor,Board)
		}
	);
	(
		wordColor(MyColor),[can,get],[Point],wordPoint(Point),[by,moving],wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),
		[to],wordRow(NewRow),wordCol(NewCol),[and,take],wordColor(YourColor),wordType(YourType),[sadly],
		{
			canGetPointSad(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,Point,PointList,Board),
			opponentColor(MyColor,YourColor),
			locate(NewRow,NewCol,YourType,YourColor,Board)
		}
	);
	(
		{
			Point > -StartPoint,
			NewPoint is Point - 1
		},
		dcgCanGetPoint(StartPoint,NewPoint,MyColor,PointList,Board)
	).

wordPoint(Point) 
--> 
	(
		[point],
		{
			Point is 1
		}
	);
	(
		[points],
		{
			Point > 1;
			Point < 1
		}
	).

wordColor(black) --> [black].
wordColor(white) --> [white].

wordRow(Row) --> [Row].
wordCol(Col) --> [Col].

wordType(knight) --> [knight].
wordType(pawn) --> [pawn].
wordType(king) --> [king].
wordType(queen) --> [queen].
wordType(bishop) --> [bishop].
wordType(rook) --> [rook].


locate(Row,Col,Type,Color,Board):-
	member(pos(Row,Col,Type,Color),Board).

opponentColor(black,white).
opponentColor(white,black).

typeNum(queen,1).
typeNum(rook,2).
typeNum(bishop,3).
typeNum(knight,4).
typeNum(pawn,5).

excludePiece([H|T],Paper,BoardAfter,PieceInformation):-
	H = PieceInformation,
	excludePiece(T,Paper,BoardAfter,PieceInformation).

excludePiece([H|T],Paper,BoardAfter,PieceInformation):-
	\+(H = PieceInformation),
	append(Paper,[H],NextPaper),
	excludePiece(T,NextPaper,BoardAfter,PieceInformation).

excludePiece([],BoardAfter,BoardAfter,PieceInformation).

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

boardAfterMoving(BoardBefore,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor):-
	opponentColor(MyColor,YourColor),
	canGo(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,BoardBefore),
	locate(NewRow,NewCol,YourType,YourColor,BoardBefore),
	excludePiece(BoardBefore,[],BoardAfterRemovingAttacked,pos(NewRow,NewCol,YourType,YourColor)),
	excludePiece(BoardAfterRemovingAttacked,[],BoardAfterAttacking,pos(NowRow,NowCol,MyType,MyColor)),
	append([pos(NewRow,NewCol,MyType,MyColor)],BoardAfterAttacking,BoardAfter).

boardAfterMoving(BoardBefore,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor):-
	opponentColor(MyColor,YourColor),
	canGo(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,BoardBefore),
	\+(locate(NewRow,NewCol,YourType,YourColor,BoardBefore)),
	excludePiece(BoardBefore,[],BoardAfterMovingMine,pos(NowRow,NowCol,MyType,MyColor)),
	append([pos(NewRow,NewCol,MyType,MyColor)],BoardAfterMovingMine,BoardAfter).

canGo(NowRow,NowCol,NewRow,NewCol,knight,MyColor,Board):-
	locate(NowRow,NowCol,knight,MyColor,Board),
	knightCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board),
	\+locate(NewRow,NewCol,_,MyColor,Board).

canAttack(NowRow,NowCol,NewRow,NewCol,knight,YourType,MyColor,Board):-
	canGo(NowRow,NowCol,NewRow,NewCol,knight,MyColor,Board),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor,Board).

canGo(NowRow,NowCol,NewRow,NewCol,rook,MyColor,Board):-
	locate(NowRow,NowCol,rook,MyColor,Board),
	rookCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board),
	\+locate(NewRow,NewCol,_,MyColor,Board).

canAttack(NowRow,NowCol,NewRow,NewCol,rook,YourType,MyColor,Board):-
	canGo(NowRow,NowCol,NewRow,NewCol,rook,MyColor,Board),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor,Board).

canGo(NowRow,NowCol,NewRow,NewCol,bishop,MyColor,Board):-
	locate(NowRow,NowCol,bishop,MyColor,Board),
	bishopCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board),
	\+locate(NewRow,NewCol,_,MyColor,Board).

canAttack(NowRow,NowCol,NewRow,NewCol,bishop,YourType,MyColor,Board):-
	canGo(NowRow,NowCol,NewRow,NewCol,bishop,MyColor,Board),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor,Board).

canGo(NowRow,NowCol,NewRow,NewCol,queen,MyColor,Board):-
	locate(NowRow,NowCol,queen,MyColor,Board),
	queenCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board),
	\+locate(NewRow,NewCol,_,MyColor,Board).

canAttack(NowRow,NowCol,NewRow,NewCol,queen,YourType,MyColor,Board):-
	canGo(NowRow,NowCol,NewRow,NewCol,queen,MyColor,Board),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor,Board).

canGo(NowRow,NowCol,NewRow,NewCol,king,MyColor,Board):-
	locate(NowRow,NowCol,king,MyColor,Board),
	kingCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board),
	\+locate(NewRow,NewCol,_,MyColor,Board).

canAttack(NowRow,NowCol,NewRow,NewCol,king,YourType,MyColor,Board):-
	canGo(NowRow,NowCol,NewRow,NewCol,king,MyColor,Board),
	opponentColor(MyColor,YourColor),
	locate(NewRow,NewCol,YourType,YourColor,Board).

canGo(NowRow,NowCol,NewRow,NewCol,pawn,MyColor,Board):-
	locate(NowRow,NowCol,pawn,MyColor,Board),
	pawnCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board),
	\+locate(NewRow,NewCol,_,MyColor,Board).

canAttack(NowRow,NowCol,NewRow,NewCol,pawn,YourType,black,Board):-
	locate(NowRow,NowCol,pawn,black,Board),
	NewRow is NowRow+1,
	(
		NewCol is NowCol-1;
		NewCol is NowCol+1
	),
	locate(NewRow,NewCol,YourType,white,Board),
	between(1,8,NewRow),
	between(1,8,NewCol).

canAttack(NowRow,NowCol,NewRow,NewCol,pawn,YourType,white,Board):-
	locate(NowRow,NowCol,pawn,white,Board),
	NewRow is NowRow-1,
	(
		NewCol is NowCol-1;
		NewCol is NowCol+1
	),
	locate(NewRow,NewCol,YourType,black,Board),
	between(1,8,NewRow),
	between(1,8,NewCol).



moveAndAttack(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyType,YourType,MyColor,Board):-
	opponentColor(MyColor,YourColor),
	canGo(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board),
	boardAfterMoving(Board,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor),
	canAttack(NewRow,NewCol,OppoRow,OppoCol,MyType,YourType,MyColor,BoardAfter),		
	\+(
		canAttack(NowRow,NowCol,OppoRow,OppoCol,MyType,YourType,MyColor,Board)
	),
	(
		(
			\+(
				canAttack(OppoRow2,OppoCol2,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter)
			)
		);
		(
			canAttack(OppoRow2,OppoCol2,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter),
			boardAfterMoving(BoardAfter,BoardLast,OppoRow2,OppoCol2,NewRow,NewCol,YourType2,YourColor),
			canAttack(MyRow,MyCol,NewRow,NewCol,MyType2,YourType2,MyColor,BoardLast)
		)
	).

moveAndGetPointHappy(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyRow,MyCol,OppoRow2,OppoCol2,MyType2,MyType,YourType,YourType2,MyColor,TotalPoint,Point,SubPoint,PointList,Board):-
	opponentColor(MyColor,YourColor),
	\+(
		canAttack(NowRow,NowCol,OppoRow,OppoCol,MyType,YourType,MyColor,Board)
	),
	canGo(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board),
	boardAfterMoving(Board,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor),
	canGetPointHappy(NewRow,NewCol,OppoRow,OppoCol,MyType,YourType,MyColor,Point,PointList,BoardAfter),
	(
		(
			\+(
				canAttack(OppoRow2,OppoCol2,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter)
			),
			YourType2 = none,
			SubPoint = 0,
			TotalPoint is Point + SubPoint,
			MyRow = 0,
			MyCol = 0,
			MyType2 = none
		);
		(
			canAttack(OppoRow2,OppoCol2,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter),
			boardAfterMoving(BoardAfter,BoardLast,OppoRow2,OppoCol2,NewRow,NewCol,YourType2,YourColor),
			canAttack(MyRow,MyCol,NewRow,NewCol,MyType2,YourType2,MyColor,BoardLast),
			typeNum(MyType,TypeNum1),
			typeNum(YourType2,TypeNum2),
			calculatePoint(TypeNum2,PointList,PlusPoint),
			calculatePoint(TypeNum1,PointList,MinusPoint),
			SubPoint is PlusPoint-MinusPoint,
			TotalPoint is Point + SubPoint
		)
	).

moveAndGetPointSad(NowRow,NowCol,NewRow,NewCol,OppoRow,OppoCol,MyRow,MyCol,OppoRow2,OppoCol2,MyType2,MyType,YourType,YourType2,MyColor,TotalPoint,Point,SubPoint,PointList,Board):-
	opponentColor(MyColor,YourColor),
	canGo(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board),
	\+(
		canAttack(NowRow,NowCol,OppoRow,OppoCol,MyType,YourType,MyColor,Board)
	),
	boardAfterMoving(Board,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor),
	canGetPointSad(NewRow,NewCol,OppoRow,OppoCol,MyType,YourType,MyColor,Point,PointList,BoardAfter),
	(
		(
			\+(
				canAttack(OppoRow2,OppoCol2,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter)
			),
			YourType2 = none,
			SubPoint = 0,
			TotalPoint is Point + SubPoint,
			MyRow = 0,
			MyCol = 0,
			MyType2 = none
		);
		(
			canAttack(OppoRow2,OppoCol2,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter),
			boardAfterMoving(BoardAfter,BoardLast,OppoRow2,OppoCol2,NewRow,NewCol,YourType2,YourColor),
			canAttack(MyRow,MyCol,NewRow,NewCol,MyType2,YourType2,MyColor,BoardLast),
			typeNum(MyType,TypeNum1),
			typeNum(YourType2,TypeNum2),
			calculatePoint(TypeNum2,PointList,PlusPoint),
			calculatePoint(TypeNum1,PointList,MinusPoint),
			SubPoint is PlusPoint-MinusPoint,
			TotalPoint is Point + SubPoint
		)
	).

countType(MyColor,Ans,MyType,Board):-
	countType(MyColor,0,Ans,MyType,Board).

countType(MyColor,Paper,Ans,MyType,[H|T]):-
	H = pos(_,_,MyType,MyColor),
	NewPaper is Paper + 1,
	countType(MyColor,NewPaper,Ans,MyType,T).	

countType(MyColor,Paper,Ans,MyType,[H|T]):-
	\+(H = pos(_,_,MyType,MyColor)),
	countType(MyColor,Paper,Ans,MyType,T).

countType(MyColor,Paper,Paper,MyType,[]).

countTypeAll(MyColor,LoseQueen,LoseRook,LoseBishop,LoseKnight,LosePawn,Board):-
	countType(MyColor,CountQueen,queen,Board),
	LoseQueen is 1-CountQueen,
	countType(MyColor,CountRook,rook,Board),
	LoseRook is 2-CountRook,
	countType(MyColor,CountBishop,bishop,Board),
	LoseBishop is 2-CountBishop,
	countType(MyColor,CountKnight,knight,Board),
	LoseKnight is 2-CountKnight,
	countType(MyColor,CountPawn,pawn,Board),
	LosePawn is 8-CountPawn.

calculatePoint(TypeNum,PointList,Point):-
	(
		TypeNum is 1,
		[Point,_,_,_,_]=PointList
	);
	(
		TypeNum is 2,
		[_,Point,_,_,_]=PointList
	);
	(
		TypeNum is 3,
		[_,_,Point,_,_]=PointList
	);
	(
		TypeNum is 4,
		[_,_,_,Point,_]=PointList
	);
	(
		TypeNum is 5,
		[_,_,_,_,Point]=PointList
	).

canGetPointHappy(NowRow,NowCol,NewRow,NewCol,MyType,YourType1,MyColor,Point,PointList,Board):-
	opponentColor(MyColor,YourColor),
	canAttack(NowRow,NowCol,NewRow,NewCol,MyType,YourType1,MyColor,Board),
	boardAfterMoving(Board,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor),
	(
		(
			\+(
				canAttack(OppoRow,OppoCol,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter)
			)
		);
		(
			canAttack(OppoRow,OppoCol,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter),
			boardAfterMoving(BoardAfter,BoardLast,OppoRow,OppoCol,NewRow,NewCol,YourType2,YourColor),
			canAttack(NewRow2,NewCol2,NewRow,NewCol,MyType2,YourType2,MyColor,BoardLast)
		)
	),
	typeNum(MyType,TypeNum1),
	typeNum(YourType1,TypeNum2),
	calculatePoint(TypeNum2,PointList,PlusPoint),
	Point is PlusPoint.

canGetPointSad(NowRow,NowCol,NewRow,NewCol,MyType,YourType1,MyColor,Point,PointList,Board):-
	opponentColor(MyColor,YourColor),
	canAttack(NowRow,NowCol,NewRow,NewCol,MyType,YourType1,MyColor,Board),
	boardAfterMoving(Board,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor),
	canAttack(OppoRow,OppoCol,NewRow,NewCol,YourType2,MyType,YourColor,BoardAfter),
	typeNum(MyType,TypeNum1),
	typeNum(YourType1,TypeNum2),
	calculatePoint(TypeNum2,PointList,PlusPoint),
	calculatePoint(TypeNum1,PointList,MinusPoint),
	Point is PlusPoint-MinusPoint.

canGetPoint(Point,NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,PointList,Board):-
	canGetPointHappy(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,Point,PointList,Board);
	canGetPointSad(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,Point,PointList,Board).

pointLoop(StartPoint,Point,NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,PointList,Board):-
	(
		Point = StartPoint,
		(
			canGetPointHappy(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,Point,PointList,Board);
			canGetPointSad(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,Point,PointList,Board)
		)
	)
	;
	(
		StartPoint >= -20,
		NewPoint is StartPoint-1,
		pointLoop(NewPoint,Point,NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,PointList,Board)
	).

makeCheckmate(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board):-
	opponentColor(MyColor,YourColor),
	canGo(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board),
	boardAfterMoving(Board,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor),
	checkmate(YourColor,BoardAfter).

underAttack(OppoRow,OppoCol,NowRow,NowCol,MyType,YourType,MyColor,Board):-
	opponentColor(MyColor,YourColor),
	canAttack(OppoRow,OppoCol,NowRow,NowCol,YourType,MyType,YourColor,Board),
	boardAfterMoving(Board,BoardAfter,OppoRow,OppoCol,NowRow,NowCol,YourType,YourColor),
	\+(
		canAttack(NowRow2,NowCol2,NowRow,NowCol,MyType2,YourType,MyColor,BoardAfter)
	).

escapeUnderAttack(NowRow,NowCol,NowRow2,NowCol2,NewRow,NewCol,OppoRow,OppoCol,MyType,MyType2,YourType,MyColor,Board):-
	underAttack(OppoRow,OppoCol,NowRow,NowCol,MyType,YourType,MyColor,Board),
	canGo(NowRow2,NowCol2,NewRow,NewCol,MyType2,MyColor,Board),
	boardAfterMoving(Board,BoardAfter,NowRow2,NowCol2,NewRow,NewCol,MyType2,MyColor),
	\+(
		underAttack(OppoRow,OppoCol,NowRow,NowCol,MyType,YourType,MyColor,BoardAfter)
	).

checked(OppoRow,OppoCol,NowKingRow,NowKingCol,YourType,MyColor,Board):-
	opponentColor(MyColor,YourColor),
	canAttack(OppoRow,OppoCol,NowKingRow,NowKingCol,YourType,king,YourColor,Board).

escapeCheck(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board):-
	checked(OppoRow1,OppoCol1,NowKingRow1,NowKingCol1,YourType1,MyColor,Board),
	opponentColor(MyColor,YourColor),
	boardAfterMoving(Board,BoardAfter,NowRow,NowCol,NewRow,NewCol,MyType,MyColor),
	\+(
		checked(OppoRow2,OppoCol2,NowKingRow2,NowKingCol2,YourType2,MyColor,BoardAfter)
	).

checkmate(MyColor,Board):-
	checked(OppoRow1,OppoCol1,NowKingRow1,NowKingCol1,YourType1,MyColor,Board),
	forall(
		escapeCheck(NowRow,NowCol,NewRow,NewCol,MyType,MyColor,Board),
		false
	),!.

knightCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board):-
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
	\+locate(NewRow,NewCol,_,MyColor,Board).

rookCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board):-
	(
		opponentColor(MyColor,YourColor),

		nearestRight(NowRow,NowCol,MyColor,MyColor,Nright1,Board),
		nearestRight(NowRow,NowCol,MyColor,YourColor,Nright2,Board),

		nearestLeft(NowRow,NowCol,MyColor,MyColor,Nleft1,Board),
		nearestLeft(NowRow,NowCol,MyColor,YourColor,Nleft2,Board),

		nearestUp(NowRow,NowCol,MyColor,MyColor,Nup1,Board),
		nearestUp(NowRow,NowCol,MyColor,YourColor,Nup2,Board),

		nearestDown(NowRow,NowCol,MyColor,MyColor,Ndown1,Board),
		nearestDown(NowRow,NowCol,MyColor,YourColor,Ndown2,Board),

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

bishopCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board):-
	
	opponentColor(MyColor,YourColor),

	nearestRightUp(NowRow,NowCol,MyColor,MyColor,NrightupRow1,NrightupCol1,Board),
	nearestRightUp(NowRow,NowCol,MyColor,YourColor,NrightupRow2,NrightupCol2,Board),

	nearestLeftUp(NowRow,NowCol,MyColor,MyColor,NleftupRow1,NleftupCol1,Board),
	nearestLeftUp(NowRow,NowCol,MyColor,YourColor,NleftupRow2,NleftupCol2,Board),

	nearestRightDown(NowRow,NowCol,MyColor,MyColor,NrightdownRow1,NrightdownCol1,Board),
	nearestRightDown(NowRow,NowCol,MyColor,YourColor,NrightdownRow2,NrightdownCol2,Board),

	nearestLeftDown(NowRow,NowCol,MyColor,MyColor,NleftdownRow1,NleftdownCol1,Board),
	nearestLeftDown(NowRow,NowCol,MyColor,YourColor,NleftdownRow2,NleftdownCol2,Board),
	
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

queenCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board):-
	rookCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board);
	bishopCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board).

kingCanGo(NowRow,NowCol,NewRow,NewCol,MyColor,Board):-
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
	\+locate(NewRow,NewCol,_,MyColor,Board).

pawnCanGo(NowRow,NowCol,NewRow,NewCol,black,Board):-
	(
		NewCol is NowCol,
		(
			(
				NowRow is 2,
				PRow is NowRow+1,
				NewRow is NowRow+2,
				\+locate(PRow,NewCol,_,_,Board),
				\+locate(NewRow,NewCol,_,_,Board)
			);
			(
				NewRow is NowRow+1,
				\+locate(NewRow,NewCol,_,_,Board)
			)
		),
		between(1,8,NewRow)
	);
	(
		(	
			NewCol is NowCol-1;
			NewCol is NowCol+1
		),
		NewRow is NowRow+1,
		between(1,8,NewRow),
		between(1,8,NewCol),
		locate(NewRow,NewCol,_,white,Board)
	).



pawnCanGo(NowRow,NowCol,NewRow,NewCol,white,Board):-
	(
		NewCol is NowCol,
		(
			(
				NowRow is 7,
				PRow is NowRow-1,
				NewRow is NowRow-2,
				\+locate(PRow,NewCol,_,_,Board),
				\+locate(NewRow,NewCol,_,_,Board)
			);
			(
				NewRow is NowRow-1,
				\+locate(NewRow,NewCol,_,_,Board)
			)
		),
		between(1,8,NewRow)
	);
	(
		(	
			NewCol is NowCol-1;
			NewCol is NowCol+1
		),
		NewRow is NowRow-1,
		between(1,8,NewRow),
		between(1,8,NewCol),
		locate(NewRow,NewCol,_,black,Board)
	).



nearestRight(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col+1,
		locate(Row,Pcol,_,MyColor,Board)
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 8,
		Col >= 8
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol,Board):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	nearestRight(Row,Pcol,MyColor,YourColor,Ncol,Board).




nearestRight(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col,
		locate(Row,Pcol,_,YourColor,Board)
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 8,
		Col >= 8
	),
	!.

nearestRight(Row,Col,MyColor,YourColor,Ncol,Board):-
	opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	nearestRight(Row,Pcol,MyColor,YourColor,Ncol,Board).


nearestLeft(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col-1,
		locate(Row,Pcol,_,MyColor,Board)
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 1,
		Col =< 1
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol,Board):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	nearestLeft(Row,Pcol,MyColor,YourColor,Ncol,Board).




nearestLeft(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Pcol is Col,
		locate(Row,Pcol,_,YourColor,Board)
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 1,
		Col =< 1
	),
	!.

nearestLeft(Row,Col,MyColor,YourColor,Ncol,Board):-
	opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	nearestLeft(Row,Pcol,MyColor,YourColor,Ncol,Board).

nearestUp(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row-1,
		locate(Prow,Col,_,MyColor,Board)
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is 1,
		Row =< 1
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow,Board):-
	\+opponentColor(MyColor,YourColor),
	Prow is Row-1,
	Prow >= 1,
	nearestUp(Prow,Col,MyColor,YourColor,Nrow,Board).




nearestUp(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row,
		locate(Prow,Col,_,YourColor,Board)
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is 1,
		Row =< 1
	),
	!.

nearestUp(Row,Col,MyColor,YourColor,Nrow,Board):-
	opponentColor(MyColor,YourColor),
	Prow is Row-1,
	Prow >= 1,
	nearestUp(Prow,Col,MyColor,YourColor,Nrow,Board).

nearestDown(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row+1,
		locate(Prow,Col,_,MyColor,Board)
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Nrow is 8,
		Row >= 8
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow,Board):-
	\+opponentColor(MyColor,YourColor),
	Prow is Row+1,
	Prow =< 8,
	nearestDown(Prow,Col,MyColor,YourColor,Nrow,Board).




nearestDown(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is Row,
		Prow is Row,
		locate(Prow,Col,_,YourColor,Board)
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow,Board):-
	(
		opponentColor(MyColor,YourColor),
		Nrow is 8,
		Row >= 8
	),
	!.

nearestDown(Row,Col,MyColor,YourColor,Nrow,Board):-
	opponentColor(MyColor,YourColor),
	Prow is Row+1,
	Prow =< 8,
	nearestDown(Prow,Col,MyColor,YourColor,Nrow,Board).

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col+1,
		Prow is Row+1,
		locate(Prow,Pcol,_,MyColor,Board)
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row =< 8,
		Col >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col =< 8,
		Row >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row+1,
	Prow =< 8,
	nearestRightDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor,Board)
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row =< 8,
		Col >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col =< 8,
		Row >= 8
	),
	!.

nearestRightDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row+1,
	Prow =< 8,
	nearestRightDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).




nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col+1,
		Prow is Row-1,
		locate(Prow,Pcol,_,MyColor,Board)
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row >= 1,
		Col >= 8
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col =< 8,
		Row =< 1
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row-1,
	Prow >= 1,
	nearestRightUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).




nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor,Board)
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 8,
		Nrow is Row,
		Row >= 1,
		Col >= 8
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col =< 8,
		Row =< 1
	),
	!.

nearestRightUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	opponentColor(MyColor,YourColor),
	Pcol is Col+1,
	Pcol =< 8,
	Prow is Row-1,
	Prow >= 1,
	nearestRightUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).



nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col-1,
		Prow is Row-1,
		locate(Prow,Pcol,_,MyColor,Board)
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row >= 1,
		Col =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col >= 1,
		Row =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row-1,
	Prow >= 1,
	nearestLeftUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).




nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor,Board)
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row >= 1,
		Col =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 1,
		Col >= 1,
		Row =< 1
	),
	!.

nearestLeftUp(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row-1,
	Prow >= 1,
	nearestLeftUp(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).


nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col-1,
		Prow is Row+1,
		locate(Prow,Pcol,_,MyColor,Board)
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row =< 8,
		Col =< 1
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		\+opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col >= 1,
		Row >= 8
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	\+opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row+1,
	Prow =< 8,
	nearestLeftDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).




nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is Row,
		Pcol is Col,
		Prow is Row,
		locate(Prow,Pcol,_,YourColor,Board)
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is 1,
		Nrow is Row,
		Row =< 8,
		Col =< 1
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	(
		opponentColor(MyColor,YourColor),
		Ncol is Col,
		Nrow is 8,
		Col >= 1,
		Row >= 8
	),
	!.

nearestLeftDown(Row,Col,MyColor,YourColor,Nrow,Ncol,Board):-
	opponentColor(MyColor,YourColor),
	Pcol is Col-1,
	Pcol >= 1,
	Prow is Row+1,
	Prow =< 8,
	nearestLeftDown(Prow,Pcol,MyColor,YourColor,Nrow,Ncol,Board).
