:- use_module(library(chr)).
:- chr_constraint hascolor/2, n/2.

setsem 		@ hascolor(X, Cx) \ hascolor(X, Cy) <=> Cx == Cy.
diffcolor 	@ n(X,Y), hascolor(X, Cx), hascolor(Y, Cy) ==> Cx \== Cy.


main :-
	findall(n(X,Y), neighb(X,Y), L),
	setConstraints(L).
		
setConstraints([]).

setConstraints([n(X,Y)|T]) :-
	n(X,Y),
	setColor(X),
	setColor(Y),
	setConstraints(T).
		
setColor(X) :-
	color(C),
	hascolor(X,C).
		
neighb(c1,c2).
neighb(c1,c3).
neighb(c1,c4).
neighb(c2,c3).
neighb(c3,c4).

color(red).
color(blue).
color(green).
