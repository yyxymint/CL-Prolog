% set_prolog_flag(answer_write_options,[max_depth(0)]).

canAttack(4,4,5,6,knight,pawn,black,[]).


dcgLocation(locate(Row,Col,Type,Color,Board)) --> wordColor(Color),wordType(Type),[at],wordRow(Row),wordCol(Col).

dcgCanAttack(canAttack(NowRow,NowCol,NewRow,NewCol,MyType,YourType,MyColor,BoardBefore)) 
--> wordColor(MyColor),wordType(MyType),[at],wordRow(NowRow),wordCol(NowCol),[can,attack],
	wordColor(YourColor),wordType(YourType),[at],wordRow(NewRow),wordCol(NewCol).




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
