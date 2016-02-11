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
    EpSupSpec = 
        {mocba_ep_sup,
             {mocba_ep_sup, start_link, []}, 
             permanent, 
             5000, 
             supervisor, 
             [mocba_ep_sup]
            },
    Notify = 
        {mocba_notify,
             {mocba_notify, start_link, [{relay, "localhost"}]}, 
             permanent, 
             5000, 
             worker, 
             [mocba_notify]
            },
    {ok, { {one_for_one, 0, 1}, 
           [ 
            EpSupSpec,
            Notify
           ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
