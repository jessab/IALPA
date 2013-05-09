:-lib(ic).
:-lib(branch_and_bound).
:-lib(lists).

gym(TimeTable, Sporthours, EqRemoval, Dailystart, Dailyfinish, B) :-
	LastSlot is Dailyfinish-1,
	assignSportSlots(Sporthours,TimeTable),
	constraints(TimeTable,EqRemoval, Dailystart, LastSlot),
	defineCost(Cost, TimeTable),
	term_variables(TimeTable, Vars),
	minimize(search(Vars, 0, most_constrained, indomain, complete, [backtrack(B)]),Cost).

% This predicate assigns slots to each sport.
% Each sport has as many slots as it has hours.
assignSportSlots(SportHours,SportSlots) :-
	(	foreach(H, SportHours),
		fromto(SportSlots, [Sport|This], This, [])
	do
		length(Sport,H)
	).
	

% This predicate enforces all constraints on the slots.
constraints(SportSlots,EqRemoval,Dailystart, LastSlot) :-

	% Each sport has it own constraints, they are defined in sportConstraints
	(	foreach(Sport,SportSlots),
		foreach(Eq, EqRemoval),
		fromto([],Rest,This,AllSlots),
		param(SportSlots)
	do
		append(Sport,Rest,This),
		sportConstraints(Sport),
		eqConstraints(Eq, Sport, SportSlots)
	),
	
	% The slots can only be on weekdays between the dailystart and the dailyfinish
	AllSlots::[100+Dailystart..100+LastSlot,
			200+Dailystart..200+LastSlot,
			300+Dailystart..300+LastSlot,
			400+Dailystart..400+LastSlot,
			500+Dailystart..500+LastSlot],
			
	% No two slots can be at the same time
	alldifferent(AllSlots).


% This predicate enforces constraints on the slots of one sport
sportConstraints(Sport) :-
	append(Sport, [601,602], Sport2),
	(	fromto(Sport2, [This, Next, X, Other |ÊRest], [Next, X, Other |ÊRest], [_,_,_])
	do
		% There can't be more than 3 slots of the same sport on one day
		(Other-This$>50),
		
		% If two following slots are on the same day they must follow each other immediately
		(Next-This $= 1 or Next-This $>50),
		
		% The slots are ordered according to their day and time
		This $< Next
	).
	
eqConstraints(Eq, Sport, SportSlots):-
	(	foreach(N, Eq),
		foreach(OtherSport, SportSlots),
		param(Sport)
	do
		(	foreach(S1, Sport),
			param(N, OtherSport)
		do
			(	foreach(S2, OtherSport),
				param(N, S1)
			do
				(S1 $= S2 or (S1$>S2 or S2-S1$>N))
			)
		)
	).

% This prediacte calculates the cost
% The cost is the day and hour of the last slot used by a sport. 
defineCost(Cost, SportSlots):-
	(	foreach(Sport, SportSlots),
		fromto(0, In, Out, Cost)
	do
		% The last slot of a sport is the first of the reverse list of slots.
		reverse(Sport, [Last|_]),
		
		% The cost is equal to the largest last slot of all sports.
		(Last $> In -> Out $= Last)
	).


	
	
