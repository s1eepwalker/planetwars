-module(bot1).
-behaviour(gen_server).

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
	{NewPlayer, Order, NewState} = solution_handler(Player, Map, State),
	{reply, {NewPlayer, Order}, NewState};

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

solution_handler(#player{} = Player, Map, State) ->
	{NewPlayer, Msg, NewState} = make_message(Player, Map, State),
	{NewPlayer, #order{message = Msg}, NewState}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_message(#player{searching_ally = false} = Player, Map, State) ->

	{Player, #message{}, State};
make_message(#player{searching_ally = true, last_message = LastMsg,
	allies = Allies, id = Id} = Player, Map, State) ->
	case LastMsg of
		#message{type = no_msg} ->
			util:mark_planets_by_owner(Id, Map, ally),
			{Player, #message{type = im_here, player_id = Id}, State};
		#message{type = im_here, player_id = Id} ->
			util:mark_planets_other_team([Id] ++ Allies, Map),
			{Player #player{searching_ally = false, last_message = #message{}}, #message{}, State};
		#message{type = im_here, player_id = AllyID} ->
			util:mark_planets_by_owner(AllyID, Map, ally),
			{Player #player{allies = Allies ++ [AllyID]}, LastMsg, State}
	end.

