-module(bot_aggro).
-behaviour(gen_server).
-compile({parse_transform, lager_transform}).
%% directly function
-export([start_link/0, start_link/1]).
-export([analyze_planet/4]).

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
solution_handler(#player{turn = CurrentTurn} = Player, Map, State) ->

	{_Time, {Commands, NewPlayer}} = timer:tc(fun analyze_planets/3, [Player, Map, State]),
	AttackList = NewPlayer #player.attack_list,

	{NewAttackList, FleetCommand} = case Commands of
		no_target ->
			{AttackList, wait};
		{Target, Len, Home} when (CurrentTurn + Len) < ?MAX_TURNS ->
			{AttackList ++ [{Target #planet.id, CurrentTurn + Len}],
			util:fleet_calculate(Map, Home #planet.id, Target #planet.id, Len, aggro)};
		_ ->
			{AttackList, wait}
	end,

	{NewPlayer2, Msg} = make_message(NewPlayer, {Commands, FleetCommand}),

	{NewPlayer2 #player{attack_list = NewAttackList},
	#order{fleet_command = FleetCommand, message = Msg}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_message(#player{searching_ally = false, allies = All, turn = CurrentTurn} = Player,
	{{_Target, Len, _Home}, {Pl1, Pl2, Fleet}})
	when (CurrentTurn + Len) < ?MAX_TURNS andalso Len > 1 andalso length(All) > 0 ->
			{Player #player{last_message = #message{}}, #message{
				type = attack,
				turn = CurrentTurn + Len,
				id_from = Pl1,
				id_to = Pl2,
				fleet = Fleet
			}};
make_message(#player{searching_ally = true, last_message = LastMsg,
	allies = Allies, id = Id} = Player, _) ->
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
	end;
make_message(Player, _) ->
	{Player #player{last_message = #message{}}, #message{}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_planets(#player{last_message = Msg, attack_list = AttackList} = Player,
	Map, #state{search_radius = SearchRadius}) ->
	NewPlayer = case Msg of
		#message{type = attack,	turn = Turn, id_to = Target} ->
			Player #player{attack_list = AttackList ++ [{Target, Turn}]};
		_ ->
			Player
	end,
	HomeWorlds = ets:match_object(Map, #planet{owner_id = Player #player.id, _ = '_'}),
	Analyze = fun Analyze([]) -> no_target;
		Analyze([World |Worlds]) ->
		case analyze_planet(World, NewPlayer, Map, SearchRadius) of
			no_target -> Analyze(Worlds);
			Ret -> Ret
		end
	end,
	{Analyze(util:shuffle(HomeWorlds)), NewPlayer}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_planet(#planet{fleet = HomeFleet} = Home, Player, Map, SearchRadius)
when HomeFleet > ?MIN_FLEET ->
	CurrentTurn = Player #player.turn,
	AttackList = Player #player.attack_list,

	IsAttacked = fun(TId) ->
		case proplists:get_value(TId, AttackList) of
			undefined -> false;
			Turn -> CurrentTurn - 1 < Turn
		end
	end,
	FilterAttacked = fun({#planet{id = Id}, _Len}) -> not IsAttacked(Id) end,


	AllNeutrals = lists:filter(FilterAttacked, util:shortest_path(Home, Map, neutral)),
	FilterRad = fun({#planet{}, D}) -> D =< SearchRadius end,
	Neutrals = lists:filter(FilterRad, AllNeutrals),
	Enemies = lists:filter(FilterAttacked,
			util:shortest_path(Home, Map, enemy)),





	CheckWorld = fun CheckWorld(_, []) -> no_target;
		CheckWorld(Confs, [{#planet{owner_id = 0} = P, Len} | Rest]) ->
		case util:shortest_path_rad(P, Map, Confs, SearchRadius) of
			[{_, Len2} | _] ->
				case Len < Len2 andalso P #planet.fleet < Home #planet.fleet
					andalso P #planet.confederate =/= ally
					of
					true -> {P, Len, Home};
					_ -> CheckWorld(Confs, Rest)
				end;
			_ -> CheckWorld(Confs, Rest)
		end;
		CheckWorld(Confs, [{#planet{owner_id = OId} = P, Len} | Rest]) when OId > 0 ->
		case util:shortest_path_rad(P, Map, Confs, SearchRadius) of
			[{_, Len2} | _] ->
				case Len < Len2
						andalso P #planet.confederate =/= ally
						andalso (P#planet.fleet + P#planet.increment*Len) <
						Home #planet.fleet
						of
					true -> {P, Len, Home};
					_ -> CheckWorld(Confs, Rest)
				end;
			_ -> CheckWorld(Confs, Rest)
		end
	end,

	World = case CheckWorld([enemy, unknown], Neutrals) of
		no_target when length(Enemies) > 0 ->
			{Enemy, Len} = hd(Enemies),
			{Enemy, Len, Home};
		no_target -> no_target;
		{_, _ ,_} = R -> R
	end,
	% lager:info("TURN=~p AttackList~p~n"
	% 	"new target ~p", [CurrentTurn, AttackList, World]),
	World;
analyze_planet(_Home, _Player, _Map, _SearchRadius) ->
	no_target.









