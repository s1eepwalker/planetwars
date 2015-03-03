-module(bot_safe).
-behaviour(gen_server).
-compile({parse_transform, lager_transform}).

%% directly function
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-define(SERVER, ?MODULE).
-define(SEARCH_RADIUS, 5).

-include("pw.hrl").

-record(state, {
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
	{NewPlayer, Order} = solution_handler(Player, Map),
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

solution_handler(#player{attack_list = AttackList, turn = CurrentTurn} = Player, Map) ->
	% Ret = timer:tc(fun analyze_map/2, [Player, Map]),
	% lager:warning("~p", [Ret]),
	Commands = analyze_map(Player, Map),
	% analyze_help(Player, Map),
	{NewAttackList, FleetCommand} = case Commands of
		[] ->
			{AttackList, wait};
		[{Target, Len, Home} | _] ->
			{AttackList ++ [{Target #planet.id, CurrentTurn + Len}],
			util:fleet_calculate(Home, Target, Len, safe)}
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_map(Player, Map) ->
	% lager:warning("turn = ~p", [Player #player.turn]),
	CurrentTurn = Player #player.turn,
	PlayerId = Player #player.id,
	AttackList = Player #player.attack_list,

	HomeWorlds =  ets:select(Map, [{#planet{id = '$1', owner_id = PlayerId, _ = '_'}, [], ['$_']}]),
	SortFun = fun({_, L1, _}, {_, L2, _}) -> L1 < L2 end,
	F = fun(Planet, {Easy, Moderate, Hard}) ->
		{E, M, H} = analyze_planet(Planet, Player, Map),
		{lists:sort(SortFun, Easy ++ E),
		lists:sort(SortFun, Moderate ++ M),
		lists:sort(SortFun, Hard ++ H)}
	end,
	{Easy, Moderate, Hard} = lists:foldl(F,{[], [], []}, HomeWorlds),

	FilterFun = fun({#planet{id = PlanetId}, _Len, _home}) ->
		case proplists:get_value(PlanetId, AttackList) of
			undefined -> true;
			Turn -> CurrentTurn > Turn
		end
	end,
	lists:filter(FilterFun, Easy ++ Moderate ++ Hard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_planet(Home, Player, Map) ->
	PlayerId = Player #player.id,
	Neutrals = util:shortest_path(Home, Map, neutral),
	Enemies = util:shortest_path(Home, Map, enemy),
	% lager:info("enemies = ~p", [Enemies]),

	EasyWorlds = fun({P, Len}, Acc) ->
		case util:shortest_path(P, Map, [enemy, unknown]) of
			[{_, Len2} | _] ->
				case Len < Len2 andalso P #planet.fleet < Home #planet.fleet
					andalso P #planet.confederate =/= ally
					of
					true -> Acc ++ [{P, Len, Home}];
					_ -> Acc
				end;
			_ -> Acc
		end
	end,
	Easy = lists:foldl(EasyWorlds, [], Neutrals),
	case length(Easy) > 0 of
		true ->
			lager:info("~p" ?GREEN "EasyWorlds = ~p" ?NORM, [PlayerId, Easy]);
		_ -> ok
	end,

	ModerateWorlds = fun({P, Len}, Acc) ->
		case util:shortest_path(P, Map, [unknown]) of
			[{_, Len2} | _] ->
				case Len =< Len2 andalso P #planet.fleet < Home #planet.fleet
					andalso P #planet.confederate =/= ally of
					true -> Acc ++ [{P, Len, Home}];
					_ -> Acc
				end;
			_ -> Acc
		end
	end,
	Moderate = lists:foldl(ModerateWorlds, [], Neutrals),
	case length(Moderate) > 0 of
		true ->
			lager:info("~p" ?YELLOW "ModerateWorlds" "= ~p" ?NORM, [PlayerId, Moderate]);
		_ -> ok
	end,

	EnemyWorlds = fun({P, Len}, Acc) ->
		case util:shortest_path(P, Map, enemy) of
			[_,{_, Len2} | _] ->
				case Len < Len2
						andalso P #planet.confederate =/= ally
						andalso (P#planet.fleet + P#planet.increment*Len) <
						Home #planet.fleet of
					true -> Acc ++ [{P, Len, Home}];
					_ -> Acc
				end;
			_ -> Acc
		end
	end,
	Hard = lists:foldl(EnemyWorlds, [], Enemies),
	NewHard = case length(Hard) > 0 of
		true ->
			lager:info("~p" ?RED "EnemyWorlds = ~p" ?NORM, [PlayerId, Hard]),
			Hard;
		_ when length(Enemies) > 0 andalso length(Neutrals) == 0 ->
			[{Enemy, Len} | _ ] = Enemies,
			Hard ++ [{Enemy, Len, Home}];
		_ -> Hard
	end,
	{Easy, Moderate, NewHard}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







