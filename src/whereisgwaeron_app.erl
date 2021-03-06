%%%-------------------------------------------------------------------
%% @doc whereisgwaeron public API
%% @end
%%%-------------------------------------------------------------------

-module(whereisgwaeron_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_, _) ->
    whereisgwaeron_sup:start_link().

%%--------------------------------------------------------------------
stop(_) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
