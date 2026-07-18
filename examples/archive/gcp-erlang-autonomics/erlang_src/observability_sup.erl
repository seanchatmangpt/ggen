%%%-------------------------------------------------------------------
%% @doc observability_sup: Supervisor for observability infrastructure
%%
%% Manages metrics, tracing, and alerts:
%% - metrics_collector: Collects and aggregates metrics
%% - trace_handler: Manages distributed tracing
%% - alert_manager: Triggers alerts based on thresholds
%%
%% Restart strategy: one_for_one (independent failure domains)
%% @end
%%%-------------------------------------------------------------------
-module(observability_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the observability supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, observability_sup}, ?MODULE, []).

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
        %% Metrics collector
        #{
            id => metrics_collector,
            start => {metrics_collector, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => worker,
            modules => [metrics_collector]
        },

        %% Trace handler
        #{
            id => trace_handler,
            start => {trace_handler, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => worker,
            modules => [trace_handler]
        },

        %% Alert manager
        #{
            id => alert_manager,
            start => {alert_manager, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => worker,
            modules => [alert_manager]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
