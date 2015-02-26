%%%-------------------------------------------------------------------
%% @doc universe supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(universe_sup).

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
	% {ok, Team1} = application:get_env(planetwars, team1),
	% {ok, Team2} = application:get_env(planetwars, team2),
	% BotName1 = proplists:get_value(botname, Team1),
	% BotName2 = proplists:get_value(botname, Team2),
	% Size1 = proplists:get_value(size, Team1),
	% Size2 = proplists:get_value(size, Team2),

	% ChildSpecs =
	% [{get_name(X, t1),
	% 	{starbase, start_link, [get_name(X, t1), BotName1]},
	% 	permanent, 2000, worker, [starbase]} || X <- lists:seq(1, Size1)]
	% ++
	% [{get_name(X, t2),
	% 	{starbase, start_link, [get_name(X, t2), BotName2]},
	% 	permanent, 2000, worker, [starbase]} || X <- lists:seq(1, Size2)],


	% Sup = {modules_sup, {modules_sup, start, []}, permanent, 2000, supervisor, [modules_sup]},
	% supervisor:start_child(master_sup, Sup),


	{ok, { {one_for_one, 5, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
% get_name(Id, Team) ->
% 	Name = lists:flatten(io_lib:format("sb~p_~p", [Team, Id])),
% 	list_to_atom(Name).
