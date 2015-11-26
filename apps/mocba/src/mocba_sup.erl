%%%-------------------------------------------------------------------
%% @doc mocba top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('mocba_sup').

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
    {ok, { {one_for_all, 0, 1}, 
           [
            {mocba_ep_sup, 
             {mocba_ep_sup, start_link, []}, 
             permanent, 
             5000, 
             supervisor, 
             [mocba_ep_sup]
            }
           ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
