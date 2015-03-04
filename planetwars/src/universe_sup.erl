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
  {ok, { {one_for_one, 5, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
% get_name(Id, Team) ->
%   Name = lists:flatten(io_lib:format("sb~p_~p", [Team, Id])),
%   list_to_atom(Name).
