%%%-------------------------------------------------------------------
%% @doc cluster_sup: Supervisor for cluster management
%%
%% Manages Erlang cluster coordination:
%% - cluster_mgr: Cluster membership and node coordination
%% - node_monitor: Node health monitoring and failure detection
%%
%% Restart strategy: one_for_one (independent failure domains)
%% @end
%%%-------------------------------------------------------------------
-module(cluster_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the cluster supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, cluster_sup}, ?MODULE, []).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Init callback
-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Cluster manager
        #{
            id => cluster_mgr,
            start => {cluster_mgr, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => worker,
            modules => [cluster_mgr]
        },

        %% Node monitor
        #{
            id => node_monitor,
            start => {node_monitor, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => worker,
            modules => [node_monitor]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
