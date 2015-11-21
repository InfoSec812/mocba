%%%-------------------------------------------------------------------
%% @doc mocba public API
%% @end
%%%-------------------------------------------------------------------

-module('mocba_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/epmgmt/:epname", mocba_epmgmt_http_handler, []}]}
     ]),
    cowboy:start_http(mocba_http_listener, 100, [{port, 8089}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    mocba_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
