:-lib(ic).

send_more_money(representation2,Chars,B) :-
	Chars = [S,E,N,D,M,O,R,Y],
	Chars :: 0..9,
	Caries = [C1,C2,C3,C4],
	Caries :: 0..1,
	%constraints
	S $\= 0,
	M $\= 0,
	alldifferent(Chars),
	D + E $= 10*C1 + Y,
	C1 + N + R $= 10 * C2 + E,
	C2 + E + O $= 10 * C3 + N,
	C3 + S +M $= 10 * C4 + O,
	C4 $= M.
	%search(Chars, 0, input_order, indomain, complete, [backtrack(B)]).
	
	
send_more_money(representation1, Chars, B) :-
	Chars = [S,E,N,D,M,O,R,Y],
	Chars :: 0..9,
	S $\= 0,
	M $\= 0,
	alldifferent(Chars),
	1000 * S + 100 * E + 10 * N + D
	+ 1000 *M + 100 * O + 10 * R + E $= 
		10000*M + 1000*O + 100*N + 10*E + Y.
	%search(Chars, 0, input_order, indomain, complete, [backtrack(B)]).
