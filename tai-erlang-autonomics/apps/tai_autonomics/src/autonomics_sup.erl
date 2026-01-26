%%%-------------------------------------------------------------------
%% @doc autonomics_sup: Root supervisor for GCP Erlang Autonomics
%%
%% Restart strategy: one_for_all (if root crashes, restart entire system)
%% Max restart frequency: 5 restarts / 60 seconds
%% Shutdown timeout: 5 seconds
%%
%% Supervision tree:
%% autonomics_sup (root, one_for_all)
%% ├── governance_sup (one_for_one)
%% │   ├── entitlement_sup
%% │   ├── billing_sup
%% │   ├── product_catalog_sup
%% │   ├── subscription_sup
%% │   ├── customer_account_sup
%% │   ├── quota_sla_sup
%% │   ├── compliance_audit_sup
%% │   └── multi_tenant_sup
%% ├── receipt_ledger_sup (one_for_one)
%% │   ├── receipt_store (ETS-backed, persistent)
%% │   └── receipt_publisher (to Pub/Sub)
%% ├── cluster_sup (one_for_one)
%% │   ├── cluster_mgr
%% │   └── node_monitor
%% └── observability_sup (one_for_one)
%%     ├── metrics_collector
%%     ├── trace_handler
%%     └── alert_manager
%%
%% @end
%%%-------------------------------------------------------------------
-module(autonomics_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart strategy constants
-define(RESTART_INTENSITY, 5).        % Max 5 restarts
-define(RESTART_PERIOD, 60).           % Per 60 seconds
-define(SHUTDOWN_TIMEOUT, 5000).       % 5 seconds graceful shutdown

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the root supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Init callback - defines supervision tree
-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => ?RESTART_INTENSITY,
        period => ?RESTART_PERIOD
    },

    ChildSpecs = [
        %% Governance supervisor (autonomous governors for entitlement, billing, etc)
        #{
            id => governance_sup,
            start => {governance_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [governance_sup]
        },

        %% Receipt ledger supervisor (persistence and Pub/Sub)
        #{
            id => receipt_ledger_sup,
            start => {receipt_ledger_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [receipt_ledger_sup]
        },

        %% Cluster supervisor (node coordination)
        #{
            id => cluster_sup,
            start => {cluster_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [cluster_sup]
        },

        %% Observability supervisor (metrics, tracing, alerts)
        #{
            id => observability_sup,
            start => {observability_sup, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => supervisor,
            modules => [observability_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
