:-lib(ic).
:-lib(branch_and_bound).
:-lib(lists).

gym(TimeTable, Sporthours, Dailystart, Dailyfinish, B) :-
	LastSlot is Dailyfinish-1,
	assignSportSlots(Sporthours,TimeTable),
	constraints(TimeTable,Dailystart, LastSlot),
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
constraints(SportSlots,Dailystart, LastSlot) :-

	% Each sport has it own constraints, they are defined in sportConstraints
	(	foreach(Sport,SportSlots),
		fromto([],Rest,This,AllSlots)
	do
		append(Sport,Rest,This),
		sportConstraints(Sport)
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
	

% This prediacte calculates the cost
% The cost is the day and hour of the last slot used by a sport. 
defineCost(Cost, SportSlots):-
	(	foreach(Sport, SportSlots),
		fromto(500, In, Out, Cost)
	do
		% The last slot of a sport is the first of the reverse list of slots.
		reverse(Sport, [Last|_]),
		
		% The cost is equal to the largest last slot of all sports.
		(Last $> In -> Out $= Last)
	).


	
	