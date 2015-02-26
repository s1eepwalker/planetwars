%%%-------------------------------------------------------------------
%% @doc planetwars top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(planetwars_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	% Sup = {universe_sup, {universe_sup, start_link, []}, permanent, 2000, supervisor, [universe_sup]},
	{ok, Team} = application:get_env(planetwars, team1),
	BotName = proplists:get_value(botname, Team),
	Player = {starbase, {starbase, start_link, [starbase, BotName]}, permanent, 2000, worker, [starbase]},
	Emulator = {emulator, {emulator, start_link, []}, permanent, 2000, worker, [emulator]},

	ChildSpecs = [Player, Emulator],
	{ok, { {one_for_all, 0, 1}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
