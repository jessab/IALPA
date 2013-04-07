:- use_module(library(chr)).
-set_prolog_flag(chr_toplevel_show_store,true).
:-chr_constraint numberPosition/3, guess/0.

% numberPosition(Number, available(AvP) or numberPosition(Number, position(P))
% all numbers belong to different positions:
numberPosition(N,I,available([])) <=> numberPosition(N, I, position(0)).
numberPosition(N,_, position(P)) \ numberPosition(N, I, available(AvP)) <=> canReduceAvPos(AvP,P, NewAvP) | numberPosition(N, I,available(NewAvP)).
numberPosition(_,_, position(P)) \ numberPosition(X, I, available(AvP)) <=> select(P, AvP, NewAvP) | numberPosition(X, I,available(NewAvP)).
 
canReduceAvPos(AvP,P, NewAvP) :- 
		illegalPositionsSameNumber(P,IllegalPositions),
		subtract(AvP, IllegalPositions, NewAvP), 
		NewAvP \= AvP.
	
guess, numberPosition(N, I, available(AvP))<=>member(P,AvP),numberPosition(N, I, position(P)).
	
illegalPositionsSameNumber(Position, IllegalPositions) :-
		squareSize(N),
		 N2 is N * N,
		Row is Position // N2 + 1,
		Column is Position mod N2 + 1,

		rowPositions(Column, N2, RowPositions),
		columnPositions(Row, N2, ColumnPositions),
		
		Square is ((Row - 1) // N) * N + ((Column - 1) // N),
		squarePositions(N, Square, SquarePositions),

		append(RowPositions, ColumnPositions, OtherPos1),
		append(OtherPos1, SquarePositions, IllegalPositions).
	
	

rowPositions(Column, N, RPositions) :-
		numlist(1, N,Rows),
		maplist(position(N,Column),Rows,RPositions).
	
% positions range from 0 to N²
position(N,Column,Row,Pos) :- Pos is N * (Row - 1) + (Column - 1).
	
columnPositions(Row, N, CPositions) :-
		FirstPosInC is (Row - 1) * N,
		LastPosInC is FirstPosInC + N - 1,
		numlist(FirstPosInC, LastPosInC, CPositions).
		

squarePositions(SquareSize,Square, SPositions) :-
		N is SquareSize * SquareSize,
		N1 is N - 1,
		numlist(0, N1, PositionsInSquare),
		maplist(squarePosition(SquareSize, Square), PositionsInSquare, SPositions).
		

squarePosition(N, Square, PInSquare, GlobalPosition) :-
		N2 is N * N,
		N3 is N2 * N,
		LeftTopCorner is N3 * (Square//N) + N * (Square mod N),
		GlobalPosition is LeftTopCorner + N2 * (PInSquare//N) + (PInSquare mod N).
		
squareSize(3).


	
create :-
	create_numbers,
	loop,
	squareSize(N12),
	N is N12*N12,
	construct_puzzle(N,Puzzle),
	print_puzzle(Puzzle).
	
	create_numbers :-
	squareSize(N),
	N2 is N * N,
	create_numbers_vars(N2, 1).
	
create_numbers_vars(N,N1) :- N1 is N + 1.
create_numbers_vars(N,CurrentNb) :-
	create_number_vars(N,1,CurrentNb),
	NextNb is CurrentNb + 1,
	create_numbers_vars(N,NextNb).
	
create_number_vars(N,N1,_) :- N1 is N + 1.
create_number_vars(N,CurrentVarIndex, CurrentNb) :-
		N2 is N * N,
		N21 is N2 - 1,
		numlist(0,N21,Positions),
		numberPosition(CurrentNb, CurrentVarIndex,available(Positions)),
		NextVarIndex is CurrentVarIndex + 1,
		create_number_vars(N,NextVarIndex, CurrentNb).
		
	
loop:- find_chr_constraint(numberPosition(_,_,available(_))),guess,loop.
loop:- \+find_chr_constraint(numberPosition(_,_,available(_))),!.


construct_puzzle(N,Puzzle) :- construct_puzzle(N,1, Puzzle).

construct_puzzle(N,N1,[]) :- N1 is N + 1.
construct_puzzle(N, CurrentRowI, [CurrentRow | Rows]) :-
		construct_row(N, 1, CurrentRowI, CurrentRow),
		NextRowI is CurrentRowI + 1,
		construct_puzzle(N, NextRowI, Rows).
		
construct_row(N,N1,_,[]) :- N1 is N + 1.
construct_row(N, ColumnNb, RowNb,[CurrentElement | Elements]) :-
		position(N, ColumnNb, RowNb, P),
		find_chr_constraint(numberPosition(Nb,_,position(P))),
		CurrentElement = Nb,
		NewColumnNb is ColumnNb + 1,
		construct_row(N, NewColumnNb, RowNb, Elements).

	puzzle(P) :- P = 
	[[_,4,_,6,_,_,_,_,_],
	[2,_,_,_,_,7,9,4,6],
	[_,_,8,_,3,9,_,5,_],
	[_,3,_,_,_,_,8,_,2],
	[_,_,9,_,_,_,6,_,_],
	[_,5,2,_,_,_,_,_,_],
	[_,_,_,_,9,3,_,_,_],
	[_,_,_,_,_,4,7,3,_],
	[_,_,4,8,7,_,_,_,9]].
	
	print_puzzle([]).
	
	print_puzzle([Row | Rest]) :-
		foreach(member(E,Row), 
			(
				(nonvar(E) -> write(E) ; write('_')), 
				write(' ')
			)
		),
		write('\n'),
		print_puzzle(Rest).
	
	
% example board:
board([6,_,_,7,_,_,5,_,_,
       _,2,8,_,_,_,_,_,_,
       _,_,_,6,4,_,3,_,_,
       7,4,_,_,_,_,_,2,_,
       _,_,1,_,_,_,8,_,_,
       _,5,_,_,_,_,_,3,7,
       _,_,3,_,7,6,_,_,_,
       _,_,_,_,_,_,1,9,_,
       _,_,4,_,_,5,_,_,8]).