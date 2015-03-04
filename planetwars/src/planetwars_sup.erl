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
	{ok, BotName} = application:get_env(planetwars, botname),
	{ok, EmuOn} = application:get_env(planetwars, emulator),
	Player = {starbase, {starbase, start_link, [starbase, BotName]}, permanent, 2000, worker, [starbase]},
	Emulator = {emulator, {emulator, start_link, []}, permanent, 2000, worker, [emulator]},

	ChildSpecs = case EmuOn of
		true -> [Emulator];
		false -> [Player]
	end,
	{ok, { {one_for_all, 0, 1}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
