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
flight_time(Pl1, Pl2, Map) ->
	[#planet{x = X1, y = Y1} | _] = ets:lookup(Map, Pl1),
	[#planet{x = X2, y = Y2} | _] = ets:lookup(Map, Pl2),
	round(math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2))).

