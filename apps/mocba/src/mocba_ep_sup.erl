-module(mocba_ep_sup).

-behaviour(supervisor).
-include("mocba.hrl").

%% API functions
-export([
         start_link/0,
         start_ep/2,
         stop_ep/1
        ]).

%% Supervisor callbacks
-export([init/1]).

child(Id, Args) -> 
    {Id, {mocba_ep, start_link, Args}, permanent, 5000, worker, [mocba_ep]}.

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_ep(term(), epstate()) -> term().

start_ep(Id, Config) ->
    stop_ep(Id),
    error_logger:info_msg("starting endpoint ~p\n", [Id]),
    supervisor:start_child(?MODULE, child(Id, [Id, Config])).

stop_ep(Id) ->
    error_logger:info_msg("removing endpoint ~p\n", [Id]),
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
 %   ignore.
    {ok, {{one_for_one, 5, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
