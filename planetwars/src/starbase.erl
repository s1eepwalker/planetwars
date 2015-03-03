-module(starbase).
-behaviour(gen_server).
-compile({parse_transform, lager_transform}).

%% directly function
-export([start_link/0, start_link/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-define(SERVER, ?MODULE).

-include("pw.hrl").

-record(state, {
		player = #player{}         :: #player{},
		bot_name = bot1            :: atom(),
		bot_pid = undefined        :: pid(),
		starmap = undefined        :: atom()
	}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
	start_link(?SERVER, bot1).
start_link(Name, BotName) ->
	gen_server:start_link({local, Name}, ?MODULE, [BotName], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([BotName]) ->
	process_flag(trap_exit, true),
	Ret = erlang:process_info(self(), [registered_name]),
	RegName = proplists:get_value(registered_name, Ret),
	lager:notice("~p: Captain '~p' on the bridge ...", [RegName, BotName]),
	MapName = list_to_atom(atom_to_list(RegName) ++ "_map"),
	ets:new(MapName, [public, ordered_set, named_table, {keypos, 2}]),

	gen_server:cast(RegName, start_bot),

	{ok, #state{bot_name = BotName, starmap = MapName}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, From, State) ->
	Reply = {ok, From},
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(start_bot, #state{bot_name = BotName} = State) ->
	Ret = erlang:process_info(self(), [registered_name]),
	RegName = proplists:get_value(registered_name, Ret),
	BotN = list_to_atom(atom_to_list(BotName) ++ "_" ++ atom_to_list(RegName)),
	% Bot = {BotN, {BotName, start_link, [BotN]}, permanent, 2000, worker, [BotName]},
	% supervisor:start_child(planetwars_sup, Bot),
	{ok, Pid} = BotName:start_link(BotN),
	{noreply, State #state{bot_pid = Pid}};
handle_cast({planetinfo, #planet{} = P}, #state{starmap = T} = State) ->
	% lager:info("PLANET = ~p", [P]),
	Player = State #state.player,
	Allies = Player #player.allies,
	PlayerId = Player #player.id,
	SearchingAlly = Player #player.searching_ally,

	Pl = case P of
		#planet{owner_id = 0}  ->
			P #planet{confederate = neutral};
		#planet{owner_id = PlayerId}  ->
			P #planet{confederate = ally};
		#planet{owner_id = PLID} ->
			case length(Allies -- [PLID]) == length(Allies) of
				true when SearchingAlly -> P #planet{confederate = unknown};
				true -> P #planet{confederate = enemy};
				false -> P #planet{confederate = ally}
			end
	end,
	ets:insert(T, Pl),
	{noreply, State};
handle_cast({your_id, Id}, #state{player = Player} = State) ->
	% lager:info("ID = ~p", [Id]),
	UpdatedPlayer = Player #player{id = Id, turn = Player #player.turn + 1},
	{noreply, State #state{player = UpdatedPlayer}};
handle_cast({message, #message{} = Message}, #state{player = Player} = State) ->
	% lager:info("~p message received = ~p", [Player #player.id, Message]),
	UpdatedPlayer = Player #player{
	 	last_message = Message
	},
	{noreply, State #state{player = UpdatedPlayer}};
handle_cast({wait_decision, Pid, STime}, #state{player = Player, starmap = Map} = State) ->
	Timeout = (1000000 - timer:now_diff(now(), STime)) div 1000,
	% lager:info("Stime = ~p Timeout = ~p", [STime, Timeout]),
	{ChangedPlayer, Reply} = case catch gen_server:call(State #state.bot_pid, {solution, Player, Map}, Timeout) of
		{'EXIT', {timeout, _}} -> {Player, {timeout, Timeout}};
		{PL, #order{} = Order} -> {PL, Order};
		_ -> {Player, #order{}}
	end,
	gen_server:cast(Pid, {order, Player #player.id, Reply}),
	{noreply, State #state{player = ChangedPlayer}};
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
