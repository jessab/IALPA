:- lib(sd).
:-lib(ic).

solveRiddle(ShoeStruct, StoreStruct,LastStore) :-
	dim(ShoeStruct,[4]),
	dim(StoreStruct, [4]),
	( 
		for(I, 1, 4), 
		param(ShoeStruct, StoreStruct)
			do
				ShoeStruct[I] &:: [ecru_espadrilles,fuchsia_flats,purple_pumps,suede_sandals],
				StoreStruct[I] &:: [shoe_palace, foot_farm, heels_in_a_handcart,tootsies]
	),
	alldifferentA(ShoeStruct),
	alldifferentA(StoreStruct),
	fuchsiaFlatsAtHeels(ShoeStruct,StoreStruct),
	storeAfterPurplePumpsNotTootsies(ShoeStruct,StoreStruct),
	twoStopsAfterShoePalaceSuede(ShoeStruct, StoreStruct),
	subscript(StoreStruct,[2], foot_farm),
	LastStore is StoreStruct[4],
	collection_to_list(StoreStruct, StoreList),
	collection_to_list(ShoeStruct, ShoeList),
	sd:labeling(ShoeList),
	sd:labeling(StoreList).
	
	
alldifferentA(Array) :-
	collection_to_list(Array,List),
	sd:alldifferent(List).
	

fuchsiaFlatsAtHeels(ShoeStruct, StoreStruct) :-
		( for(I,1,4),
	param(ShoeStruct, StoreStruct)
		do
			ShoeI is ShoeStruct[I],
			StoreI is StoreStruct[I],
			&=(ShoeI, fuchsia_flats, B),
			&=(StoreI, heels_in_a_handcart,B)
			
	).
	
	storeAfterPurplePumpsNotTootsies(ShoeStruct, StoreStruct) :- 
	( for(I,1,3),
	param(ShoeStruct, StoreStruct)
		do
			I1 is I + 1,
			ShoeI is ShoeStruct[I],
			StoreI1 is StoreStruct[I1],
			&=(ShoeI, purple_pumps, B1),
			&=(StoreI1, tootsies,B2),
			B1 => neg B2
	).
	
	twoStopsAfterShoePalaceSuede(ShoeStruct, StoreStruct) :-
	( for(I,1,2), 
	fromto(0, ConstrainedSatisfied, Out, Satisfied),
	param(ShoeStruct, StoreStruct)
		do
			I2 is I + 2,
			ShoeI2 is ShoeStruct[I2],
			StoreI is StoreStruct[I],
			&=(StoreI, shoe_palace, B),
			&=(ShoeI2, suede_sandals, B),
			or(ConstrainedSatisfied, B, Out)
	), Satisfied $= 1.