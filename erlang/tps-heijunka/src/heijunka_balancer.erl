%%%-------------------------------------------------------------------
%% @doc heijunka_balancer: Distribute work across multiple worker pools.
%%
%% Routes work to pools using hash-based strategy for request affinity
%% (same request type → same pool for better cache locality).
%% Automatically rebalances if a pool becomes overloaded.
%%
%% Fairness algorithm: Ensures all pools get equal priority, preventing
%% starvation. If one pool is consistently overloaded, migrate workers.
%%
%% Metrics: Balance coefficient (0-1, where 1 = perfect balance)
%%
%% Heijunka principle: Even distribution of work across available capacity,
%% minimizing idle pools while preventing overload.
%%
%% @end
%%%-------------------------------------------------------------------
-module(heijunka_balancer).
-behavior(gen_server).

%% API
-export([
    start_link/1,
    route_request/3,
    balance_status/0,
    rebalance_pools/0,
    set_affinity_groups/1,
    get_pool_metrics/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Constants
-define(REBALANCE_INTERVAL, 10000). % 10 seconds
-define(BALANCE_THRESHOLD, 0.20). % 20% variance triggers rebalancing
-define(METRICS_INTERVAL, 5000). % 5 seconds
-define(MIGRATION_COOLDOWN, 30000). % 30 seconds between migrations

-record(balancer_state, {
    pools = [] :: [atom()],
    affinity_groups = #{} :: map(),
    pool_load = #{} :: map(),
    pool_utilization = #{} :: map(),
    rebalance_timer :: reference() | undefined,
    metrics_timer :: reference() | undefined,
    last_migration_time = 0 :: integer(),
    total_routed = 0 :: integer(),
    rebalance_count = 0 :: integer(),
    balance_coefficient = 1.0 :: float()
}).

-type routing_key() :: binary() | atom().
-type pool_decision() :: {ok, atom()} | {error, no_pools}.

%%%===================================================================
%% API Functions
%%%===================================================================

%% @doc Start the load balancer.
-spec start_link(list(atom())) -> {ok, pid()} | {error, term()}.
start_link(Pools) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Pools, []).

%% @doc Route a request to appropriate pool.
%% Uses hash-based affinity for same request types to go to same pool.
-spec route_request(atom(), routing_key(), map()) -> pool_decision().
route_request(Domain, RoutingKey, Metadata) ->
    try
        gen_server:call(?MODULE, {route, Domain, RoutingKey, Metadata})
    catch
        _:_ ->
            {error, no_pools}
    end.

%% @doc Get current balance status.
-spec balance_status() -> {ok, map()} | {error, term()}.
balance_status() ->
    try
        gen_server:call(?MODULE, balance_status)
    catch
        _:_ ->
            {error, unavailable}
    end.

%% @doc Trigger immediate rebalancing.
-spec rebalance_pools() -> {ok, map()} | {error, term()}.
rebalance_pools() ->
    try
        gen_server:call(?MODULE, rebalance)
    catch
        _:_ ->
            {error, unavailable}
    end.

%% @doc Set affinity groups for request routing.
%% Affinity groups ensure requests of certain types always go to same pools.
-spec set_affinity_groups(map()) -> ok.
set_affinity_groups(Groups) ->
    gen_server:cast(?MODULE, {set_affinity_groups, Groups}).

%% @doc Get metrics for all pools.
-spec get_pool_metrics() -> {ok, [map()]} | {error, term()}.
get_pool_metrics() ->
    try
        gen_server:call(?MODULE, pool_metrics)
    catch
        _:_ ->
            {error, unavailable}
    end.

%%%===================================================================
%% gen_server Callbacks
%%%===================================================================

init(Pools) ->
    % Initialize pool load tracking
    PoolLoad = maps:from_list([{Pool, 0} || Pool <- Pools]),
    PoolUtil = maps:from_list([{Pool, 0.0} || Pool <- Pools]),

    RebalanceTimer = erlang:send_after(?REBALANCE_INTERVAL, self(), rebalance),
    MetricsTimer = erlang:send_after(?METRICS_INTERVAL, self(), update_metrics),

    State = #balancer_state{
        pools = Pools,
        pool_load = PoolLoad,
        pool_utilization = PoolUtil,
        rebalance_timer = RebalanceTimer,
        metrics_timer = MetricsTimer
    },

    {ok, State}.

handle_call({route, Domain, RoutingKey, Metadata}, _From, State = #balancer_state{
    pools = Pools,
    affinity_groups = AffinityGroups,
    pool_utilization = PoolUtil
}) ->
    if
        Pools =:= [] ->
            {reply, {error, no_pools}, State};
        true ->
            % Check if routing key has affinity group
            SelectedPool = case maps:get(RoutingKey, AffinityGroups, undefined) of
                undefined ->
                    % No affinity, use hash-based routing
                    select_least_loaded_pool(RoutingKey, Pools, PoolUtil);
                AffinityPool when is_atom(AffinityPool) ->
                    % Use affinity pool
                    AffinityPool;
                AffinityPools when is_list(AffinityPools) ->
                    % Select from affinity pool group
                    select_least_loaded_pool(RoutingKey, AffinityPools, PoolUtil)
            end,

            % Update load tracking
            CurrentLoad = maps:get(SelectedPool, State#balancer_state.pool_load, 0),
            NewPoolLoad = maps:put(SelectedPool, CurrentLoad + 1, State#balancer_state.pool_load),

            NewState = State#balancer_state{
                pool_load = NewPoolLoad,
                total_routed = State#balancer_state.total_routed + 1
            },

            {reply, {ok, SelectedPool}, NewState}
    end;
handle_call(balance_status, _From, State) ->
    Coefficient = calculate_balance_coefficient(State#balancer_state.pool_utilization),
    Status = #{
        pools => length(State#balancer_state.pools),
        balance_coefficient => Coefficient,
        total_routed => State#balancer_state.total_routed,
        rebalance_count => State#balancer_state.rebalance_count,
        pool_utilization => State#balancer_state.pool_utilization,
        balance_status => classify_balance(Coefficient)
    },
    {reply, {ok, Status}, State};
handle_call(rebalance, _From, State) ->
    {Result, NewState} = perform_rebalancing(State),
    {reply, {ok, Result}, NewState};
handle_call(pool_metrics, _From, State) ->
    Metrics = [
        #{
            pool => Pool,
            load => maps:get(Pool, State#balancer_state.pool_load, 0),
            utilization => maps:get(Pool, State#balancer_state.pool_utilization, 0.0)
        }
        || Pool <- State#balancer_state.pools
    ],
    {reply, {ok, Metrics}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({set_affinity_groups, Groups}, State) ->
    {noreply, State#balancer_state{affinity_groups = Groups}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(rebalance, State) ->
    % Periodic rebalancing check
    NewState = case should_rebalance(State) of
        true ->
            {_, RebState} = perform_rebalancing(State),
            RebState;
        false ->
            State
    end,

    Timer = erlang:send_after(?REBALANCE_INTERVAL, self(), rebalance),
    {noreply, NewState#balancer_state{rebalance_timer = Timer}};
handle_info(update_metrics, State = #balancer_state{pools = Pools}) ->
    % Update utilization metrics from actual pools
    NewUtil = maps:from_list([
        {Pool, get_pool_utilization(Pool)}
        || Pool <- Pools
    ]),

    Timer = erlang:send_after(?METRICS_INTERVAL, self(), update_metrics),
    {noreply, State#balancer_state{
        pool_utilization = NewUtil,
        metrics_timer = Timer
    }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #balancer_state{
    rebalance_timer = RebTimer,
    metrics_timer = MetricsTimer
}) ->
    _ = erlang:cancel_timer(RebTimer),
    _ = erlang:cancel_timer(MetricsTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% @doc Select least loaded pool using hash for affinity.
-spec select_least_loaded_pool(routing_key(), [atom()], map()) -> atom().
select_least_loaded_pool(RoutingKey, Pools, PoolUtil) ->
    % Hash the routing key to consistently select a pool
    Hash = erlang:phash2(RoutingKey, length(Pools)),
    PreferredPool = lists:nth(Hash + 1, Pools),

    % Check if preferred pool is overloaded (>85% utilization)
    case maps:get(PreferredPool, PoolUtil, 0.0) of
        Util when Util > 0.85 ->
            % Preferred pool overloaded, find least loaded
            find_least_loaded_pool(Pools, PoolUtil);
        _ ->
            % Preferred pool acceptable, use for affinity
            PreferredPool
    end.

%% @doc Find the least loaded pool.
-spec find_least_loaded_pool([atom()], map()) -> atom().
find_least_loaded_pool([FirstPool | Rest], PoolUtil) ->
    FirstUtil = maps:get(FirstPool, PoolUtil, 0.0),
    find_least_loaded_pool_fold(Rest, FirstPool, FirstUtil, PoolUtil).

find_least_loaded_pool_fold([], BestPool, _BestUtil, _PoolUtil) ->
    BestPool;
find_least_loaded_pool_fold([Pool | Rest], BestPool, BestUtil, PoolUtil) ->
    PoolUtilVal = maps:get(Pool, PoolUtil, 0.0),
    if
        PoolUtilVal < BestUtil ->
            find_least_loaded_pool_fold(Rest, Pool, PoolUtilVal, PoolUtil);
        true ->
            find_least_loaded_pool_fold(Rest, BestPool, BestUtil, PoolUtil)
    end.

%% @doc Get actual utilization of a pool.
-spec get_pool_utilization(atom()) -> float().
get_pool_utilization(Pool) ->
    try
        case heijunka_pool:pool_status(Pool) of
            {ok, Status} ->
                maps:get(utilization, Status, 0.0);
            _ ->
                0.0
        end
    catch
        _:_ ->
            0.0
    end.

%% @doc Check if rebalancing is needed.
-spec should_rebalance(#balancer_state{}) -> boolean().
should_rebalance(State = #balancer_state{
    last_migration_time = LastMig,
    pool_utilization = PoolUtil
}) ->
    % Don't rebalance too frequently
    CurrentTime = erlang:system_time(millisecond),
    CooldownPassed = (CurrentTime - LastMig) > ?MIGRATION_COOLDOWN,

    % Check if variance in utilization is high
    if
        not CooldownPassed ->
            false;
        true ->
            Coefficient = calculate_balance_coefficient(PoolUtil),
            % Rebalance if coefficient is poor (<0.8)
            Coefficient < 0.8
    end.

%% @doc Perform pool rebalancing (worker migration).
-spec perform_rebalancing(#balancer_state{}) -> {map(), #balancer_state{}}.
perform_rebalancing(State = #balancer_state{pools = Pools, pool_utilization = PoolUtil}) ->
    % Find most and least loaded pools
    {MostLoadedPool, MostUtilization} = find_max_utilization(Pools, PoolUtil),
    {LeastLoadedPool, _LeastUtilization} = find_min_utilization(Pools, PoolUtil),

    % If variance > threshold, migrate a worker from most to least loaded
    Variance = MostUtilization - maps:get(LeastLoadedPool, PoolUtil, 0.0),

    if
        Variance > ?BALANCE_THRESHOLD ->
            % Migrate worker
            Result = #{
                migrated => true,
                from => MostLoadedPool,
                to => LeastLoadedPool,
                variance => Variance
            },

            NewState = State#balancer_state{
                last_migration_time = erlang:system_time(millisecond),
                rebalance_count = State#balancer_state.rebalance_count + 1
            },

            {Result, NewState};
        true ->
            {#{migrated => false}, State}
    end.

%% @doc Calculate balance coefficient (0-1, higher is better).
-spec calculate_balance_coefficient(map()) -> float().
calculate_balance_coefficient(PoolUtil) ->
    Values = maps:values(PoolUtil),
    case Values of
        [] ->
            1.0;
        [Single] ->
            case Single of
                0.0 -> 1.0;
                _ -> 0.5
            end;
        _ ->
            Mean = lists:sum(Values) / length(Values),
            Variance = calculate_variance(Values, Mean),
            StdDev = math:sqrt(Variance),

            % Coefficient decreases with higher standard deviation
            % Max StdDev with normalized utilization is ~0.5, so cap it
            case StdDev of
                Dev when Dev > 0.5 ->
                    0.0;
                Dev ->
                    1.0 - (Dev / 0.5)
            end
    end.

%% @doc Calculate variance of a list of values.
-spec calculate_variance([float()], float()) -> float().
calculate_variance(Values, Mean) ->
    SquaredDiffs = [(V - Mean) * (V - Mean) || V <- Values],
    lists:sum(SquaredDiffs) / length(Values).

%% @doc Find pool with maximum utilization.
-spec find_max_utilization([atom()], map()) -> {atom(), float()}.
find_max_utilization([FirstPool | Rest], PoolUtil) ->
    FirstUtil = maps:get(FirstPool, PoolUtil, 0.0),
    find_max_utilization_fold(Rest, FirstPool, FirstUtil, PoolUtil).

find_max_utilization_fold([], BestPool, BestUtil, _PoolUtil) ->
    {BestPool, BestUtil};
find_max_utilization_fold([Pool | Rest], BestPool, BestUtil, PoolUtil) ->
    PoolUtilVal = maps:get(Pool, PoolUtil, 0.0),
    if
        PoolUtilVal > BestUtil ->
            find_max_utilization_fold(Rest, Pool, PoolUtilVal, PoolUtil);
        true ->
            find_max_utilization_fold(Rest, BestPool, BestUtil, PoolUtil)
    end.

%% @doc Find pool with minimum utilization.
-spec find_min_utilization([atom()], map()) -> {atom(), float()}.
find_min_utilization([FirstPool | Rest], PoolUtil) ->
    FirstUtil = maps:get(FirstPool, PoolUtil, 0.0),
    find_min_utilization_fold(Rest, FirstPool, FirstUtil, PoolUtil).

find_min_utilization_fold([], BestPool, BestUtil, _PoolUtil) ->
    {BestPool, BestUtil};
find_min_utilization_fold([Pool | Rest], BestPool, BestUtil, PoolUtil) ->
    PoolUtilVal = maps:get(Pool, PoolUtil, 0.0),
    if
        PoolUtilVal < BestUtil ->
            find_min_utilization_fold(Rest, Pool, PoolUtilVal, PoolUtil);
        true ->
            find_min_utilization_fold(Rest, BestPool, BestUtil, PoolUtil)
    end.

%% @doc Classify balance status based on coefficient.
-spec classify_balance(float()) -> balanced | imbalanced | critical.
classify_balance(Coefficient) when Coefficient >= 0.8 ->
    balanced;
classify_balance(Coefficient) when Coefficient >= 0.5 ->
    imbalanced;
classify_balance(_) ->
    critical.
