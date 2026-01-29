%%%-------------------------------------------------------------------
%%% @doc Telecom Supervisor - Carrier-Grade Fault Tolerance
%%%
%%% Demonstrates Fortune 5 capabilities:
%%% - Layered supervision trees (rest_for_one strategy)
%%% - Escalation policies with bounded restart intensity
%%% - Dependency-aware restart ordering
%%% - Graceful shutdown with configurable timeouts
%%% - Hot code upgrade support
%%%
%%% Supervision Strategy:
%%% - rest_for_one: If a child fails, restart it and all children started after it
%%% - This ensures dependency order (db_pool -> call_router -> billing_engine)
%%% - Intensity: Max 10 restarts in 60 seconds before supervisor terminates
%%%
%%% Shutdown Policy:
%%% - db_pool: 5 seconds (flush connections)
%%% - call_router: 10 seconds (complete in-flight calls)
%%% - billing_engine: infinity (wait for all transactions to complete)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(telecom_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the telecom supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    SupFlags = #{
        %% Supervision strategy: restart children in dependency order
        strategy => rest_for_one,

        %% Max restart intensity: 10 restarts in 60 seconds
        %% If exceeded, supervisor terminates (escalation to parent)
        intensity => 10,
        period => 60,

        %% Auto-shutdown: terminate when all children are done
        auto_shutdown => never
    },

    ChildSpecs = [
        %% 1. Database pool (foundational service)
        %% Must start first - other services depend on it
        #{
            id => db_pool,
            start => {db_pool, start_link, [[{pool_size, 20}]]},
            restart => permanent,  % Always restart on failure
            shutdown => 5000,      % 5 seconds to flush connections
            type => worker,
            modules => [db_pool]
        },

        %% 2. Call router (depends on db_pool for route storage)
        %% Starts after db_pool, restarts if db_pool restarts
        #{
            id => call_router,
            start => {call_router_server, start_link, [
                [{load_threshold, 150000}, {circuit_breaker, true}]
            ]},
            restart => permanent,
            shutdown => 10000,     % 10 seconds to complete in-flight calls
            type => worker,
            modules => [call_router_server]
        },

        %% 3. Billing engine (depends on call_router for CDR events)
        %% Starts last, restarts if any upstream service restarts
        #{
            id => billing_engine,
            start => {billing_engine_server, start_link, [
                [{fraud_threshold, 15000.00}]
            ]},
            restart => permanent,
            shutdown => infinity,  % Wait indefinitely for transactions to complete
            type => worker,
            modules => [billing_engine_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Example: Dynamic child management (advanced)
%% These functions demonstrate how to add/remove children at runtime

%% @doc Add a new worker to the supervision tree dynamically.
%% -spec add_worker(atom(), module(), list()) -> {ok, pid()} | {error, term()}.
%% add_worker(Id, Module, Args) ->
%%     ChildSpec = #{
%%         id => Id,
%%         start => {Module, start_link, Args},
%%         restart => permanent,
%%         shutdown => 5000,
%%         type => worker,
%%         modules => [Module]
%%     },
%%     supervisor:start_child(?SERVER, ChildSpec).

%% @doc Remove a worker from the supervision tree dynamically.
%% -spec remove_worker(atom()) -> ok | {error, term()}.
%% remove_worker(Id) ->
%%     case supervisor:terminate_child(?SERVER, Id) of
%%         ok ->
%%             supervisor:delete_child(?SERVER, Id);
%%         Error ->
%%             Error
%%     end.

%% @doc Example of a more complex supervision tree with nested supervisors.
%%
%% For Fortune 5 telecom systems, you might have:
%%
%% telecom_sup (one_for_one)
%%   ├── infrastructure_sup (rest_for_one)
%%   │   ├── db_pool
%%   │   ├── cache_cluster
%%   │   └── metrics_collector
%%   │
%%   ├── routing_sup (one_for_all)
%%   │   ├── call_router_1
%%   │   ├── call_router_2
%%   │   └── call_router_N
%%   │
%%   └── billing_sup (one_for_one)
%%       ├── billing_engine_1
%%       ├── billing_engine_2
%%       └── fraud_detector
%%
%% Each supervisor can have different strategies:
%% - one_for_one: Independent workers (billing engines)
%% - one_for_all: Interdependent workers (router cluster)
%% - rest_for_one: Dependency chain (infrastructure stack)
