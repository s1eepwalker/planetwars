-module(emulator).
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
-define(MAX_TURNS, 5).

-include("pw.hrl").

-record(state, {
		team1 = []         :: [{player_id(), pid()}],
		team2 = []         :: [{player_id(), pid()}],
		turn = 1           :: turn(),
		orders = []        :: [{turn(), {planet_id(), {player_id(), fleet()}}}],
		messages = []      :: [{player_id(), #message{}}],
		wait_players = []  :: [{player_id(), pid()}]
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
	gen_server:cast(?SERVER, make_world),
	gen_server:cast(?SERVER, next_turn),
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
handle_call(_Request, From, #state{} = State) ->
	Reply = {ok, From},
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(make_world, #state{} = State) ->
	NewState = make_world_handler(State),
	{noreply, NewState};
handle_cast(send_world, State) ->
	NewState = send_world_handler(State),
	{noreply, NewState};
handle_cast(wait_decisions, #state{} = State) ->
	NewState = wait_decisions_handler(State),
	{noreply, NewState};
handle_cast(next_turn, #state{} = State) ->
	NewState = next_turn_handler(State),
	{noreply, NewState};
handle_cast({order, PlayerId, #order{} = Order}, #state{} = State) ->
	NewState = order_handler(PlayerId, Order, State),
	{noreply, NewState};
handle_cast(_Msg, #state{} = State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, #state{} = State) ->
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
make_world_handler(#state{} = State) ->
	load_map(),

	{ok, Team1} = application:get_env(planetwars, team1),
	{ok, Team2} = application:get_env(planetwars, team2),
	Sup = {universe_sup, {universe_sup, start_link, []}, permanent, 2000, supervisor, [universe_sup]},
	supervisor:start_child(planetwars_sup, Sup),

	FTeam = fun(Team, Offs) ->
		TeamName = proplists:get_value(name, Team),
		BotName = proplists:get_value(botname, Team),
		Size = proplists:get_value(size, Team),
		FPlayer = fun(Id, Acc) ->
			Name = list_to_atom(lists:flatten(
				io_lib:format("~p_p~p", [TeamName, Id + Offs]))),
			Player = {Name, {starbase, start_link, [Name, BotName]},
				permanent, 2000, worker, [starbase]},
			{ok, Pid} = supervisor:start_child(universe_sup, Player),
			Acc ++ [{Id + Offs, Pid}]
		end,
		lists:foldl(FPlayer, [], lists:seq(1, Size))
	end,
	Team1List = FTeam(Team1, 0),
	Team2List = FTeam(Team2, length(Team1List)),
	% lager:info("Team1 = ~p", [Team1List]),
	% lager:info("Team2 = ~p", [Team2List]),
	% {_, Pid} = lists:nth(1, Team1List),
	% gen_server:cast(Pid, {planetinfo, #planet{id = 1, owner_id = 1}}),
	% gen_server:cast(Pid, {planetinfo, #planet{id = 2, owner_id = 2}}),
	% gen_server:cast(Pid, {planetinfo, #planet{id = 3, owner_id = 3}}),
	% gen_server:cast(Pid, {message, #message{type = im_here, param1 = 2}}),

	State #state{team1 = Team1List, team2 = Team2List}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_string(Fd, ID, Size1, Size2) ->
	OwnerId = case ID > Size1 + Size2 of
		true -> 0;
		_ -> ID
	end,
	Sov = case ID of
		_ when ID > (Size1 + Size2) -> neutral;
		_ when ID =< Size1 -> team1;
		_ when ID > Size1 andalso ID =< (Size1 + Size2) -> team2
	end,

	case file:read_line(Fd) of
		{ok, Line} ->
			LineWithoutNL = string:strip(string:strip(Line, both, 13), both, 10),
			L = string:tokens(LineWithoutNL, " "),
			case L of
				["P" | _] when length(L) > 6 ->
					P = #planet{
						id = list_to_integer(lists:nth(2, L)),
						x = list_to_integer(lists:nth(3, L)),
						y = list_to_integer(lists:nth(4, L)),
						increment = list_to_integer(lists:nth(5, L)),
						owner_id = OwnerId,
						fleet = list_to_integer(lists:nth(7, L)),
						confederate = Sov
					},
					ets:insert(worldmap, P);
				_ -> ok
			end,
			read_string(Fd, ID + 1, Size1, Size2);
		_ -> ok
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_map() ->
	{ok, Team1} = application:get_env(planetwars, team1),
	{ok, Team2} = application:get_env(planetwars, team2),
	Size1 = proplists:get_value(size, Team1),
	Size2 = proplists:get_value(size, Team2),

	ets:new(worldmap, [public, ordered_set, named_table, {keypos, 2}]),
	{ok, File} = application:get_env(planetwars, worldmap),
	case file:open(File, [read]) of
		{ok, Fd} ->
			read_string(Fd, 1, Size1, Size2),
			file:close(Fd);
		Err ->
			lager:error("~p", [Err])
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_world_handler(#state{team1 = Team1, team2 = Team2, messages = Messages} = State) ->
	Planets = ets:match_object(worldmap, '_'),
	SendPlanets = fun({_PlayerId, Pid}) ->
		[gen_server:cast(Pid, {planetinfo, Planet}) || Planet <- Planets]
	end,
	lists:foreach(SendPlanets, Team1),
	lists:foreach(SendPlanets, Team2),
	SendMessages = fun({PlayerId, Pid}) ->
		Msg = proplists:get_value(PlayerId, Messages),
		case Msg of
			#message{} ->
				lager:info("Msg for ~p ~p M ~p", [PlayerId, Msg, util:encode_message(Msg)]),
				gen_server:cast(Pid, {message, Msg});
			_ -> ok
		end,
		gen_server:cast(Pid, {your_id, PlayerId})

	end,
	lists:foreach(SendMessages, Team1),
	lists:foreach(SendMessages, Team2),

	State #state{messages = []}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wait_decisions_handler(#state{turn = Turn, team1 = Team1, team2 = Team2} = State) ->
	Wait = fun({_PlayerId, Pid}) ->
		gen_server:cast(Pid, {wait_decision, self(), now()})
	end,
	lists:foreach(Wait, Team1 ++ Team2),
	case Turn =< ?MAX_TURNS of
		true ->
			timer:apply_after(1000, gen_server, cast, [?SERVER, next_turn]),
			State #state {wait_players = Team1 ++ Team2};
		false ->
			State #state {wait_players = []}
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
order_handler(PlayerId, #order{fleet_command = Cmd, message = Msg},
	#state{team1 = Team1, team2 = Team2, turn = CurrentTurn,
	messages = Messages, orders = Orders, wait_players = Waits} = State) ->

	{Base, Size} = case get_team(PlayerId, State) of
		team1 ->
			{B, _} = lists:nth(1, Team1),
			{B - 1, length(Team1)};
		team2 ->
			{B, _} = lists:nth(1, Team2),
			{B - 1, length(Team2)}
	end,
	PlayerToId = (PlayerId - Base) rem Size + 1 + Base,
	NewMessages = case Msg of
		#message{type = no_msg} -> Messages;
		#message{} -> Messages ++ [{PlayerToId, Msg}]
	end,
	NewOrders = case Cmd of
		wait -> Orders;
		{Pl1, Pl2, Fleet} ->
			PlInfo = ets:match_object(worldmap, #planet{id = Pl1, _ = '_'}),
			ets:insert(worldmap, PlInfo #planet{fleet = PlInfo #planet.fleet - Fleet}),
			Orders ++ [{CurrentTurn + util:flight_time(Pl1, Pl2, worldmap),
			{Pl2, {PlayerId, Fleet}}}]
	end,
	State #state{
		messages = NewMessages,
		orders = NewOrders,
		wait_players = proplists:delete(PlayerId, Waits)
	}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_turn_handler(#state{turn = Turn, orders = Orders} = State) ->
	lager:info("Turn = ~p", [Turn]),
	loose_players(State),
	fight(Turn, Orders),
	case Turn > 1 of true -> increment(); _ -> ok end,
	gen_server:cast(?SERVER, send_world),
	gen_server:cast(?SERVER, wait_decisions),
	State #state{turn = Turn +1}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_team(PlayerId, #state{team1 = Team1, team2 = Team2}) ->
	case proplists:get_value(PlayerId, Team1) of
		undefined ->
			case proplists:get_value(PlayerId, Team2) of
				undefined -> undefined;
				_ -> team2
			end;
		_ -> team1
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loose_players(State) ->
	F = fun({PlayerId, Pid}) ->
		lager:warning("Player ~p from ~p loose", [PlayerId, get_team(PlayerId, State)]),
		gen_server:cast(Pid, {you_loose, "you too slow"}),

		List = ets:match_object(worldmap, #planet{owner_id = PlayerId, _ = '_'}),
		[ets:insert(worldmap, X #planet{owner_id = 0, confederate = neutral})
			|| X <- List]
	end,
	[F(X) || X <- State #state.wait_players].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fight(Turn, Orders) ->
	% {planet_id(), {player_id(), fleet()}}
	List = proplists:get_all_values(Turn, Orders),
	orbital_fight(List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
orbital_fight([]) ->
	ok;
orbital_fight([{PlanetId, _} | _] = Planets) ->
	PlInfo = ets:match_object(worldmap,#planet{id = PlanetId, fleet = '$1', _ = '_'}),

	List = proplists:get_all_values(PlanetId, Planets) ++
		[{planet, PlInfo #planet.fleet}],
	SortFun = fun({_P1, F1}, {_P2, F2}) -> F1 > F2 end,
	[{P1, F1}, {P2, F2} | _] = lists:sort(SortFun, List),
	{Winner, RestFleet} = case F1 > F2 of
		true -> {P1, F1 - F2};
		false when F2 > F1 -> {P2, F2 - F1};
		false -> {draw, 0}
	end,
	case Winner of
		draw -> ets:insert(worldmap, PlInfo #planet{fleet = RestFleet});
		planet -> ets:insert(worldmap, PlInfo #planet{fleet = RestFleet});
		PlayerId ->
			ets:insert(worldmap, PlInfo #planet{owner_id = PlayerId, fleet = RestFleet})
	end,
	orbital_fight(Planets -- List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
increment() ->
	Match = [{#planet{owner_id = '$1', _ = '_'},[{'=/=', '$1', 0}], ['$_']}],
	List = ets:select(worldmap, Match),

	F = fun(Planet) ->
		Inc = Planet #planet.increment,
		Fleet = Planet #planet.fleet,
		ets:insert(worldmap, Planet #planet{fleet = Fleet + Inc})
	end,
	lists:foreach(F, List).



