%%%-------------------------------------------------------------------
%% @doc planetwars public API
%% @end
%%%-------------------------------------------------------------------

-module(planetwars_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    planetwars_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
