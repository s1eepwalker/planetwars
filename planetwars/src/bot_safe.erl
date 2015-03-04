-module(bot_safe).
-behaviour(gen_server).
-compile({parse_transform, lager_transform}).
%% directly function
-export([start_link/0, start_link/1]).
-export([analyze_planet/5]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-define(SERVER, ?MODULE).

-define(MAX_TURNS, 200).
-define(SEARCH_RADIUS, 5).
-define(MIN_FLEET, 3).


-include("pw.hrl").

-record(state, {
		targets = dict:new(),
		search_radius = ?SEARCH_RADIUS
	}).



%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
	start_link(?SERVER).
start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
	{ok, #state{}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({solution, Player, Map}, _From, State) ->
	{NewPlayer, Order} = solution_handler(Player, Map, State),
	{reply, {NewPlayer, Order}, State};
handle_call(_Request, From, State) ->
	Reply = {ok, From},
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({analyze_planet, #planet{} = P, #player{id = PlayerId} = Player, Map}, State)
	when PlayerId =/= 0 ->
	NewState = analyze_planet_handler(P, self(), Player, Map, State),
	{noreply, NewState};
handle_cast({analyze_complete, {_target, _Len, #planet{id = PlanetId} = _Home} = Target},
	#state{targets = Dict} = State) ->
	% lager:critical("Target ~p", [Target]),
	NewState = State #state{
		targets = dict:store(PlanetId, Target, Dict)
	},
	{noreply, NewState};
% handle_cast({analyze_complete, no_target}, State) ->
% 	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

solution_handler(#player{attack_list = AttackList, turn = CurrentTurn} = Player, Map, State) ->
	{_Time, Commands} = timer:tc(fun analyze_planets/3, [Player, Map, State]),
	% lager:warning("~p ~p", [Player #player.id, Time]),
	% Commands = analyze_map(Player, Map, State),
	% analyze_help(Player, Map),
	{NewAttackList, FleetCommand} = case Commands of
		no_target ->
			{AttackList, wait};
		{Target, Len, Home} when (CurrentTurn + Len) < ?MAX_TURNS ->
			{AttackList ++ [{Target #planet.id, CurrentTurn + Len}],
			util:fleet_calculate(Map, Home #planet.id, Target #planet.id, Len, safe)};
		_ ->
			{AttackList, wait}
	end,

	{NewPlayer, Msg}  = make_message(Player, Map),

	{NewPlayer #player{attack_list = NewAttackList},
	#order{fleet_command = FleetCommand, message = Msg}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_message(#player{searching_ally = false} = Player, _Map) ->

	{Player, #message{}};
make_message(#player{searching_ally = true, last_message = LastMsg,
	allies = Allies, id = Id} = Player, _Map) ->
	case LastMsg of
		#message{type = no_msg} ->
			% util:mark_planets_by_owner(Id, Map, ally),
			{Player, #message{type = im_here, player_id = Id}};
		#message{type = im_here, player_id = Id} ->
			% util:mark_planets_other_team([Id] ++ Allies, Map),
			{Player #player{searching_ally = false, last_message = #message{}}, #message{}};
		#message{type = im_here, player_id = AllyID} ->
			% util:mark_planets_by_owner(AllyID, Map, ally),
			{Player #player{allies = Allies ++ [AllyID]}, LastMsg}
	end.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% analyze_map(Player, _Map,
% 	#state{targets = Dict, search_radius = SearchRadius}) ->



% 	World = analyze_planets(Player, Map, SearchRadius, self()),
% 	Worlds = lists:sort(SortFun, lists:flatten([X || {_, X} <- dict:to_list(Dict)])),

% 	FilterFun = fun({#planet{id = PlanetId}, _Len, _home}) ->
% 		case proplists:get_value(PlanetId, AttackList) of
% 			undefined -> true;
% 			Turn -> CurrentTurn > Turn
% 		end
% 	end,

% 	lists:filter(FilterFun, Worlds).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_planets(Player, Map, #state{search_radius = SearchRadius}) ->
	HomeWorlds = ets:match_object(Map, #planet{owner_id = Player #player.id, _ = '_'}),
	Analyze = fun Analyze([]) -> no_target;
		Analyze([World |Worlds]) ->
		case analyze_planet(World, Player, Map, SearchRadius, self()) of
			no_target -> Analyze(Worlds);
			Ret -> Ret
		end
	end,
	Analyze(util:shuffle(HomeWorlds)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_planet(#planet{fleet = HomeFleet} = Home, Player, Map, SearchRadius, Pid)
when HomeFleet > ?MIN_FLEET ->
	AllNeutrals = util:shortest_path(Home, Map, neutral),
	FilterFun = fun({#planet{}, D}) -> D =< SearchRadius end,
	Neutrals = lists:filter(FilterFun, AllNeutrals),
	Enemies = util:shortest_path_rad(Home, Map, enemy, SearchRadius),

	CurrentTurn = Player #player.turn,
	AttackList = Player #player.attack_list,


	IsAttacked = fun(TId) ->
		case proplists:get_value(TId, AttackList) of
			undefined -> false;
			Turn -> CurrentTurn - 1 < Turn
		end
	end,


	CheckWorld = fun CheckWorld(_, []) -> no_target;
		CheckWorld(Confs, [{#planet{id = TargetId, owner_id = 0} = P, Len} | Rest]) ->
		case util:shortest_path_rad(P, Map, Confs, SearchRadius) of
			[{_, Len2} | _] ->
				Attacked = IsAttacked(TargetId),
				case Len < Len2 andalso P #planet.fleet < Home #planet.fleet
					andalso P #planet.confederate =/= ally
					of
					true when not Attacked-> {P, Len, Home};
					_ -> CheckWorld(Confs, Rest)
				end;
			_ -> CheckWorld(Confs, Rest)
		end;
		CheckWorld(Confs, [{#planet{id = TargetId, owner_id = OId} = P, Len} | Rest]) when OId > 0 ->
		case util:shortest_path_rad(P, Map, Confs, SearchRadius) of
			[{_, Len2} | _] ->
				Attacked = IsAttacked(TargetId),
				case Len < Len2
						andalso P #planet.confederate =/= ally
						andalso (P#planet.fleet + P#planet.increment*Len) <
						Home #planet.fleet
						of
					true when not Attacked -> {P, Len, Home};
					_ -> CheckWorld(Confs, Rest)
				end;
			_ -> CheckWorld(Confs, Rest)
		end
	end,

	World = case CheckWorld([enemy, unknown], Neutrals) of
		no_target when length(AllNeutrals) > 0 ->
			{Neutral, Len} = hd(AllNeutrals),
			{Neutral, Len, Home};
		no_target ->
			case CheckWorld([enemy], Enemies) of
				no_target when length(Enemies) > 0 andalso HomeFleet > 10 ->
					{Enemy, Len} = hd(Enemies),
					{Enemy, Len, Home};
				R -> R
			end;
		R -> R
	end,

	gen_server:cast(Pid, {analyze_complete, World}),
	% lager:info("TURN=~p AttackList~p~n"
	% 	"new target ~p", [CurrentTurn, AttackList, World]),
	World;
analyze_planet(_Home, _Player, _Map, _SearchRadius, _Pid) ->
	no_target.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_planet_handler(#planet{owner_id = OwnerId} = P,
	PID, #player{id = PlayerId} = Player, Map,
	#state{search_radius = SearchRadius} = State) when OwnerId == PlayerId ->
	spawn_link(?SERVER, analyze_planet, [P, Player, Map, SearchRadius, PID]),
	State;
analyze_planet_handler(#planet{id = PlanetId}, _PID, _Player, _Map,
	#state{targets = Dict} = State) ->
	NewState = 	State #state{
		targets = dict:erase(PlanetId, Dict)
	},
	% lager:notice("~p(~p) State ~p", [_Player #player.id, PlanetId, dict:size(NewState #state.targets)]),
	NewState.











