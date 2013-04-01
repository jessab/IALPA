% 
% Alternative representation for the sudoku puzzle, using 
% a row in the puzzle as a variable with all possible permutations
% of [1,2,3,4,5,6,7,8,9] as domain.
%
% Author(s): Jessa Bekker, Geert Heyman
%


:- lib(ic).
:- lib(listut).
:-lib(propia).

solve(ProblemName,B) :-
	problem(ProblemName, Board),
	print_board(Board),
	sudoku(3, Board,B),
	print_board(Board).

sudoku(N, Numbers,B) :-
	N2 is N*N,
	N4 is N2*N2,
	dim(Numbers, [N2,N2]),
	Numbers[1..N2,1..N2] #:: 0..N4 - 1,
	term_variables(Numbers, Vars),
	alldifferent(Vars),
%	numbersOfBoard(Board,Numbers),
	( for(I,1,N2), param(Numbers,N2,N) do
		% The same number can't occur twice at the same position
	    Row is Numbers[I,1..N2],
		noSymmetricSolutions(Row),
		onDifferentRowPositions(Row,N2),
		onDifferentColumnPositions(Row,N2),
		withinDifferentSquares(Row,N)
		 		
	),
	search(Vars, 0, input_order, indomain_max, complete, [backtrack(B)]).	


noSymmetricSolutions(Row) :-
	( fromto(Row, [X1,X2 | Xs], [X2 | Xs], [_X]) do
		X1 #< X2
	).
	
onDifferentRowPositions(Row,N) :-
	( foreach(X,Row),foreach(RowPosition,RowPositions),
	  param(N)
		do
			RowPosition #= X div N infers most
	),
	allDifferentExpressions(RowPositions).
	
		
onDifferentColumnPositions(Row, N) :-
	( foreach(X,Row),foreach(ColumnPosition,ColumnPositions),
	  param(N)
		do
			ColumnPosition #= X mod N infers most 
			),
	allDifferentExpressions(ColumnPositions).
		
withinDifferentSquares(Row, N) :-
	( foreach(X,Row),foreach(SquarePositionExpression,SquarePositions),
	  param(N)
		do
			squarePositionExpression(X,N,SquarePositionExpression)
	),
	allDifferentExpressions(SquarePositions).
	
	
allDifferentExpressions(List) :-
	( fromto(List,[X|Tail],Tail,[])
		do
		( foreach(Y, Tail),
			param(X)
			do
				eval(X) #\= eval(Y)
		)
	).

squarePositionExpression(Position, N, SquarePositionExpr) :-
	N2 is N*N,
	RowPositionExpr #= Position div N2 infers most,
	ColumnPositionExpr #= Position mod N2 infers most,
	RowSquareExpr #= RowPositionExpr div N infers most ,
	ColumnSquareExpr #= ColumnPositionExpr div N infers most,
	SquarePositionExpr #= RowSquareExpr * N + ColumnSquareExpr.

	
% Convert the known board information to the Numbers representation:
%
numbersOfBoard(Board, Numbers) :-
	dim(Board,[N,N]),
	dim(Numbers,[N,N]),
	( for(I,1,N),
	  param(Board,Numbers,N)
		do
			positionsNumber(I,Board,Positions),
			NumbersRow is Numbers[I,1..N],
			collection_to_list(Positions,NumbersRow)
	).
	
	
%% TODO: won't work when values are unknown, may be use suspend and wake?
% channelConstraints / 2 
% Board : Matrix where the value of each element represent the number of the sudoku puzzle, the position of the element
%			in the matrix is the same as the position of the number in the sudoku puzzle.
% Numbers : Matrix where the value of each element represents the position of a number in the sudoku puzzle.
%			I.e. the elements on row i represent the N different positions of the number i in the sudoku puzzle.
% The predicate expresses the relation between the matrix elements.
channelConstraints(Board, Numbers) :-
	dim(Board,[N]),
	( multifor([I,J],1,N),
	  param(Board,Numbers)
		do
			subscript(Numbers,[I,J],Position),
			BoardRow #= Position div N,
			BoardColumn #= Position mod N,
			subscript(Board, [BoardRow, BoardColumn], I)
	).
	
	
positionsNumber(I,Board,Positions) :-
	dim(Board,[N,N]),
	dim(PositionsA,[N]),
	( multifor([J,K], 1, N), 
	  fromto(1,PositionIndex,Out,_), 
	  count(C,1,_), 
	  param(I,Board, PositionsA)
		do
			( 
				(subscript(Board,[J,K],X), X ==I) -> 
					subscript(PositionsA,[PositionIndex], C),
					Out is PositionIndex + 1
						;
					Out = PositionIndex		
			)
	),
	term_variables(PositionsA,Positions).
					
				
	
	
	
	
	

print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
	    	X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.
	
	problem(1, [](
    [](_, _, 2, _, _, 5, _, 7, 9),
    [](1, _, 5, _, _, 3, _, _, _),
    [](_, _, _, _, _, _, 6, _, _),
    [](_, 1, _, 4, _, _, 9, _, _),
    [](_, 9, _, _, _, _, _, 8, _),
    [](_, _, 4, _, _, 9, _, 1, _),
    [](_, _, 9, _, _, _, _, _, _),
    [](_, _, _, 1, _, _, 3, _, 6),
    [](6, 8, _, 3, _, _, 4, _, _))).
	
	
	problem(2, [](
    [](_, _, 3, _, _, 8, _, _, 6),
    [](_, _, _, 4, 6, _, _, _, _),
    [](_, _, _, 1, _, _, 5, 9, _),
    [](_, 9, 8, _, _, _, 6, 4, _),
    [](_, _, _, _, 7, _, _, _, _),
    [](_, 1, 7, _, _, _, 9, 5, _),
    [](_, 2, 4, _, _, 1, _, _, _),
    [](_, _, _, _, 4, 6, _, _, _),
    [](6, _, _, 5, _, _, 8, _, _))).

problem(3, [](
    [](_, _, _, 9, _, _, _, _, _),
    [](_, _, 7, _, 6, _, 5, _, _),
    [](_, _, 3, 5, _, _, _, 7, 9),
    [](4, _, 5, _, _, 9, _, _, 1),
    [](8, _, _, _, _, _, _, _, 7),
    [](1, _, _, 6, _, _, 9, _, 8),
    [](6, 4, _, _, _, 8, 7, _, _),
    [](_, _, 9, _, 1, _, 2, _, _),
    [](_, _, _, _, _, 7, _, _, _))).

problem(4, [](
    [](_, 5, _, _, _, 1, 4, _, _), 
    [](2, _, 3, _, _, _, 7, _, _), 
    [](_, 7, _, 3, _, _, 1, 8, 2), 
    [](_, _, 4, _, 5, _, _, _, 7), 
    [](_, _, _, 1, _, 3, _, _, _), 
    [](8, _, _, _, 2, _, 6, _, _), 
    [](1, 8, 5, _, _, 6, _, 9, _), 
    [](_, _, 2, _, _, _, 8, _, 3), 
    [](_, _, 6, 4, _, _, _, 7, _))).

% Problems 5-8 are harder, taken from
% http://www2.ic-net.or.jp/~takaken/auto/guest/bbs46.html
problem(5, [](
    [](_, 9, 8, _, _, _, _, _, _),
    [](_, _, _, _, 7, _, _, _, _),
    [](_, _, _, _, 1, 5, _, _, _),
    [](1, _, _, _, _, _, _, _, _),
    [](_, _, _, 2, _, _, _, _, 9),
    [](_, _, _, 9, _, 6, _, 8, 2),
    [](_, _, _, _, _, _, _, 3, _),
    [](5, _, 1, _, _, _, _, _, _),
    [](_, _, _, 4, _, _, _, 2, _))).

problem(6, [](
    [](_, _, 1, _, 2, _, 7, _, _),
    [](_, 5, _, _, _, _, _, 9, _),
    [](_, _, _, 4, _, _, _, _, _),
    [](_, 8, _, _, _, 5, _, _, _),
    [](_, 9, _, _, _, _, _, _, _),
    [](_, _, _, _, 6, _, _, _, 2),
    [](_, _, 2, _, _, _, _, _, _),
    [](_, _, 6, _, _, _, _, _, 5),
    [](_, _, _, _, _, 9, _, 8, 3))).

problem(7, [](
    [](1, _, _, _, _, _, _, _, _),
    [](_, _, 2, 7, 4, _, _, _, _),
    [](_, _, _, 5, _, _, _, _, 4),
    [](_, 3, _, _, _, _, _, _, _),
    [](7, 5, _, _, _, _, _, _, _),
    [](_, _, _, _, _, 9, 6, _, _),
    [](_, 4, _, _, _, 6, _, _, _),
    [](_, _, _, _, _, _, _, 7, 1),
    [](_, _, _, _, _, 1, _, 3, _))).

problem(8, [](
    [](1, _, 4, _, _, _, _, _, _),
    [](_, _, 2, 7, 4, _, _, _, _),
    [](_, _, _, 5, _, _, _, _, _),
    [](_, 3, _, _, _, _, _, _, _),
    [](7, 5, _, _, _, _, _, _, _),
    [](_, _, _, _, _, 9, 6, _, _),
    [](_, 4, _, _, _, 6, _, _, _),
    [](_, _, _, _, _, _, _, 7, 1),
    [](_, _, _, _, _, 1, _, 3, _))).

% this one is from http://www.skyone.co.uk/programme/pgefeature.aspx?pid=48&fid=129
problem(9, [](
    [](5, _, 6, _, 2, _, 9, _, 3),
    [](_, _, 8, _, _, _, 5, _, _),
    [](_, _, _, _, _, _, _, _, _),
    [](6, _, _, 2, 8, 5, _, _, 9),
    [](_, _, _, 9, _, 3, _, _, _),
    [](8, _, _, 7, 6, 1, _, _, 4),
    [](_, _, _, _, _, _, _, _, _),
    [](_, _, 4, _, _, _, 3, _, _),
    [](2, _, 1, _, 5, _, 6, _, 7))).

% BBC Focus magazine October 2005
problem(10, [](
    [](_, 6, _, 3, 2, _, _, 7, _),
    [](4, 7, _, _, _, _, _, 3, 2),
    [](_, _, _, 9, _, _, 1, 4, 6),
    [](2, 4, _, 8, _, _, _, _, _),
    [](_, _, 8, _, _, _, 2, _, 1),
    [](1, _, _, _, _, 2, _, _, _),
    [](_, _, 2, 4, 7, 6, 8, _, _),
    [](6, 8, 9, _, _, _, _, 5, 4),
    [](_, _, _, _, 8, _, _, _, _))).

problem(11, [](
    [](1, 8, 2, 7, 5, _, 3, _, 9),
    [](9, 5, 6, _, 3, _, _, 8, _),
    [](3, 4, 7, _, _, 9, _, 5, _),
    [](2, _, 3, _, 4, _, _, 9, 8),
    [](4, _, 8, 9, _, 2, 5, _, 3),
    [](5, 7, 9, 3, 6, 8, 1, 2, 4),
    [](_, 2, _, 4, 9, _, 8, 3, _),
    [](_, 3, _, _, 2, _, 9, _, 5),
    [](_, 9, _, _, _, 3, _, 1, _))).

	