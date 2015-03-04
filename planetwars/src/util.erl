-module(util).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

-include("pw.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_message(M) when is_integer(M) ->
	<<C:2, P:30>> = <<M:32>>,
	Cmd = case C of
		1 -> im_here;
		2 -> attack;
		_ -> no_msg
	end,
	case Cmd of
		im_here ->
			#message{type = im_here, player_id = P};
		attack ->
			<<Turn:8, PlFrom:7, PlTo:7, Fleet:8 >> = <<P:30>>,
			#message{
				type = im_here,
				turn = Turn,
				id_from = PlFrom,
				id_to = PlTo,
				fleet = Fleet
			};
		_ -> #message{}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode_message(	#message{
		type = Cmd,
		player_id = PlayerId,
		turn = Turn,
		id_from = IdFrom,
		id_to = IdTo,
		fleet = Fleet
	}) ->

	<<M:32>> = case Cmd of
		im_here ->
			<<1:2, PlayerId:30>>;
		attack ->
			<<2:2, Turn:8, IdFrom:7, IdTo:7, Fleet:8>>;
		_ -> <<0:32>>
	end,
	M.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mark_planets_by_owner(OwnerId, Table, Sov) ->
	List = ets:match_object(Table, #planet{owner_id = OwnerId, _ = '_'}),
	[ets:insert(Table, X #planet{confederate = Sov}) || X <- List].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mark_planets_other_team(Allies, Table) ->
	Guards = [{'=/=', X, '$1'} || X <- Allies],
	Match = [{#planet{owner_id = '$1', _ = '_'},
		Guards,
		['$_']}],
	List = ets:select(Table, Match),
	[
		case X #planet.owner_id > 0 of
			true ->
				ets:insert(Table, X #planet{confederate = enemy});
			false ->
				ets:insert(Table, X #planet{confederate = neutral})
		end	 || X <- List].
%util:mark_planets_other_team([1,2],t1_p1_map, enemy).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_planets_by_owner(OwnerId, Table, Sov) ->
	List = ets:match_object(Table, #planet{owner_id = OwnerId, _ = '_'}),
	[ets:insert(Table, X #planet{confederate = Sov}) || X <- List].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flight_time(Pl1, Pl2, Map) when is_atom(Map) ->
	Distances = case get_param(Map, distances) of
		undefined ->
			D = calculate_distances(Map),
			set_param(Map, distances, D),
			D;
		D -> D
	end,
	flight_time_raw(Pl1, Pl2, Distances).
flight_time_raw(Pl1, Pl2, Map) when is_atom(Map) ->
	[#planet{x = X1, y = Y1} | _] = ets:lookup(Map, Pl1),
	[#planet{x = X2, y = Y2} | _] = ets:lookup(Map, Pl2),
	round(math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)));
flight_time_raw(Pl1, Pl2, _Dict) when Pl1 == Pl2 ->
	0;
flight_time_raw(Pl1, Pl2, Dict) when Pl1 > Pl2 ->
	flight_time_raw(Pl2, Pl1, Dict);
flight_time_raw(Pl1, Pl2, Dict) when Pl1 < Pl2 ->
	dict:fetch({Pl1, Pl2}, Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_distances(Map) ->
	Size = proplists:get_value(size, ets:info(Map), 0),
	F = fun(Id, Dict) ->
		F2 = fun(Id2, Dict2) ->
				Dist = flight_time_raw(Id, Id2, Map),
				dict:store({Id, Id2}, Dist, Dict2)
		end,
		lists:foldl(F2, Dict, lists:seq(Id + 1, Size))
	end,
	lists:foldl(F, dict:new(), lists:seq(1,Size)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_param(Map, Key) when is_atom(Key) ->
	case ets:match_object(Map, {Key, '_'}) of
		[] -> undefined;
		[{Key, Val} | _] -> Val
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_param(Map, Key, Value) when is_atom(Key) ->
	ets:insert(Map, {Key, Value}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shortest_path(#planet{} = P, Map, Conf) when is_atom(Conf) ->
	shortest_path(P, Map, [Conf]);
shortest_path(#planet{id = PlanetId}, Map, Confs) when is_list(Confs) ->
	Match = [{
		#planet{id = '$1',
		confederate = '$4',
		fleet = '$2',
		increment = '$3',
		_ = '_'},
		[{'==', '$4', X}], ['$_']}
	 || X <- Confs],
	Worlds = ets:select(Map, Match),
	Lengths = [{P, flight_time(PlanetId, Id, Map)}
		|| #planet{id = Id} = P <- Worlds],
	SortFun = fun
			({#planet{fleet = Fleet1}, L1},
				{#planet{fleet = Fleet2}, L1}) -> Fleet1 < Fleet2;
			({#planet{increment = Inc1}, L1},
				{#planet{increment = Inc1}, L2}) -> L1 < L2;
			({#planet{increment = Inc1}, _},
				{#planet{increment = Inc2}, _}) -> Inc1 > Inc2

	end,
	lists:sort(SortFun, Lengths).
shortest_path_rad(#planet{} = P, Map, Conf, Rad) ->
	FilterRad = fun({#planet{}, D}) -> D =< Rad end,
	lists:filter(FilterRad, shortest_path(P, Map, Conf)).
% util:shortest_path({planet,1,10,2,1,1,2,team1}, worldmap, neutral).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fleet_calculate(Map, HomeId, TargetId, Len, Strategy) ->
	[Home |_] = ets:lookup(Map, HomeId),
	[Target |_] = ets:lookup(Map, TargetId),
	HomeFleet = Home #planet.fleet,
	TargetFleet = case Target #planet.confederate of
		neutral -> Target #planet.fleet;
		_ -> Target #planet.fleet + Len*Target #planet.increment
	end,
	case HomeFleet > TargetFleet of
		true when Strategy == safe ->
			{Home #planet.id, Target #planet.id, TargetFleet + round((HomeFleet - TargetFleet)/3)};
		false when Strategy == safe ->
			{Home #planet.id, Target #planet.id, round(HomeFleet / 2)};
		true when Strategy == aggro ->
			{Home #planet.id, Target #planet.id, TargetFleet + round((HomeFleet - TargetFleet)/2)};
		false when Strategy == aggro ->
			{Home #planet.id, Target #planet.id, HomeFleet}
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fleet_total(Map, Confs) when is_atom(Confs) ->
	fleet_total(Map, [Confs]);
fleet_total(Map, Confs) when is_list(Confs) ->
	Match = [{
		#planet{confederate = '$1',
		fleet = '$2',
		_ = '_'},
		[{'==', '$1', X}], ['$2']}
	 || X <- Confs],
	Fleets = ets:select(Map, Match),
	lists:foldl(fun(X, Acc) -> X + Acc end, 0, Fleets).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shuffle(List0) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	List1 = [{random:uniform(), X} || X <- List0],
		List2 = lists:keysort(1, List1),
		[X || {_, X} <- List2].


