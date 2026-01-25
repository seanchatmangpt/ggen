%%%-------------------------------------------------------------------
%% @doc heijunka_burst_handler: Handle traffic bursts gracefully.
%%
%% Detects sudden load spikes and temporarily adds surge capacity.
%% Instead of rejecting requests or timing out, temporarily scales up pools.
%%
%% Burst detection: 10x normal load in 1 minute
%% Surge capacity: Lasts 5 minutes, then gradually shrinks back
%% Cost-aware: Surge workers are more expensive (manual intervention)
%%
%% Heijunka principle: Absorb temporary spikes without disrupting baseline flow,
%% but cost-aware to prevent overspending on surge capacity.
%%
%% @end
%%%-------------------------------------------------------------------
-module(heijunka_burst_handler).
-behavior(gen_server).

%% API
-export([
    start_link/1,
    burst_status/0,
    get_surge_metrics/0,
    disable_surge/0,
    enable_surge/0
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
-define(BURST_DETECTION_INTERVAL, 1000). % 1 second sampling
-define(BURST_THRESHOLD_MULTIPLIER, 10.0). % 10x normal load triggers burst
-define(BASELINE_WINDOW, 60000). % 60 seconds for baseline calculation
-define(SURGE_DURATION, 300000). % 5 minutes of surge capacity
-define(SURGE_COOLDOWN, 30000). % 30 seconds before next surge eligible
-define(SURGE_WORKERS_PER_POOL, 5). % Add 5 workers per pool during surge
-define(SURGE_COST_FACTOR, 2.0). % Surge workers cost 2x normal

-record(burst_state, {
    enabled :: boolean(),
    baseline_load = 0.0 :: float(),
    current_load = 0.0 :: float(),
    load_samples = [] :: [float()],
    in_surge :: boolean(),
    surge_start_time :: integer() | undefined,
    surge_pools = [] :: [atom()],
    surge_count = 0 :: integer(),
    total_surge_cost = 0.0 :: float(),
    detection_timer :: reference() | undefined,
    cooldown_timer :: reference() | undefined,
    baseline_timer :: reference() | undefined
}).

%%%===================================================================
%% API Functions
%%%===================================================================

%% @doc Start the burst handler.
-spec start_link(list(atom())) -> {ok, pid()} | {error, term()}.
start_link(Pools) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Pools, []).

%% @doc Get burst status.
-spec burst_status() -> {ok, map()} | {error, term()}.
burst_status() ->
    try
        gen_server:call(?MODULE, status)
    catch
        _:_ ->
            {error, unavailable}
    end.

%% @doc Get surge metrics.
-spec get_surge_metrics() -> {ok, map()} | {error, term()}.
get_surge_metrics() ->
    try
        gen_server:call(?MODULE, surge_metrics)
    catch
        _:_ ->
            {error, unavailable}
    end.

%% @doc Disable surge capacity (manual control).
-spec disable_surge() -> ok.
disable_surge() ->
    gen_server:cast(?MODULE, disable_surge).

%% @doc Enable surge capacity.
-spec enable_surge() -> ok.
enable_surge() ->
    gen_server:cast(?MODULE, enable_surge).

%%%===================================================================
%% gen_server Callbacks
%%%===================================================================

init(Pools) ->
    DetectionTimer = erlang:send_after(?BURST_DETECTION_INTERVAL, self(), detect_burst),
    BaselineTimer = erlang:send_after(?BASELINE_WINDOW, self(), recalculate_baseline),

    State = #burst_state{
        enabled = true,
        in_surge = false,
        detection_timer = DetectionTimer,
        baseline_timer = BaselineTimer
    },

    {ok, State}.

handle_call(status, _From, State) ->
    Status = #{
        enabled => State#burst_state.enabled,
        in_surge => State#burst_state.in_surge,
        baseline_load => State#burst_state.baseline_load,
        current_load => State#burst_state.current_load,
        load_ratio => case State#burst_state.baseline_load of
            0.0 -> 0.0;
            BL -> State#burst_state.current_load / BL
        end,
        burst_threshold => State#burst_state.baseline_load * ?BURST_THRESHOLD_MULTIPLIER,
        surge_active => State#burst_state.in_surge,
        surge_count => State#burst_state.surge_count,
        total_surge_cost => State#burst_state.total_surge_cost
    },
    {reply, {ok, Status}, State};
handle_call(surge_metrics, _From, State) ->
    SurgeTimeRemaining = case State#burst_state.surge_start_time of
        undefined ->
            0;
        StartTime ->
            EndTime = StartTime + ?SURGE_DURATION,
            max(0, EndTime - erlang:system_time(millisecond))
    end,

    Metrics = #{
        in_surge => State#burst_state.in_surge,
        surge_start_time => State#burst_state.surge_start_time,
        surge_time_remaining_ms => SurgeTimeRemaining,
        surge_pools => State#burst_state.surge_pools,
        surge_count => State#burst_state.surge_count,
        total_surge_cost => State#burst_state.total_surge_cost,
        avg_surge_cost => case State#burst_state.surge_count of
            0 -> 0.0;
            Count -> State#burst_state.total_surge_cost / Count
        end
    },
    {reply, {ok, Metrics}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(disable_surge, State) ->
    {noreply, State#burst_state{enabled = false}};
handle_cast(enable_surge, State) ->
    {noreply, State#burst_state{enabled = true}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(detect_burst, State = #burst_state{enabled = true, in_surge = false}) ->
    % Sample current load from all pools
    CurrentLoad = sample_total_load(),

    % Add to samples for moving average
    NewSamples = [CurrentLoad | lists:sublist(State#burst_state.load_samples, 59)],

    % Check if we should trigger surge
    BurstThreshold = State#burst_state.baseline_load * ?BURST_THRESHOLD_MULTIPLIER,

    NewState = if
        CurrentLoad > BurstThreshold andalso State#burst_state.enabled ->
            % Burst detected - activate surge
            trigger_surge(State#burst_state{load_samples = NewSamples, current_load = CurrentLoad});
        true ->
            State#burst_state{
                load_samples = NewSamples,
                current_load = CurrentLoad
            }
    end,

    Timer = erlang:send_after(?BURST_DETECTION_INTERVAL, self(), detect_burst),
    {noreply, NewState#burst_state{detection_timer = Timer}};
handle_info(detect_burst, State) ->
    % Burst detection disabled or already in surge
    Timer = erlang:send_after(?BURST_DETECTION_INTERVAL, self(), detect_burst),
    {noreply, State#burst_state{detection_timer = Timer}};
handle_info(recalculate_baseline, State) ->
    % Recalculate baseline from load samples
    NewBaseline = case State#burst_state.load_samples of
        [] ->
            0.0;
        Samples ->
            lists:sum(Samples) / length(Samples)
    end,

    Timer = erlang:send_after(?BASELINE_WINDOW, self(), recalculate_baseline),
    {noreply, State#burst_state{
        baseline_load = NewBaseline,
        load_samples = [],
        baseline_timer = Timer
    }};
handle_info(end_surge, State = #burst_state{surge_pools = SurgePools}) ->
    % Surge duration expired - reduce surge capacity
    _ = [shrink_surge_capacity(Pool) || Pool <- SurgePools],

    {noreply, State#burst_state{
        in_surge = false,
        surge_start_time = undefined,
        surge_pools = []
    }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #burst_state{
    detection_timer = DetTimer,
    baseline_timer = BaselineTimer,
    cooldown_timer = CooldownTimer
}) ->
    _ = erlang:cancel_timer(DetTimer),
    _ = erlang:cancel_timer(BaselineTimer),
    case CooldownTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(CooldownTimer)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% @doc Sample total load across all pools.
-spec sample_total_load() -> float().
sample_total_load() ->
    % Query all active pools via balancer
    case heijunka_balancer:get_pool_metrics() of
        {ok, Metrics} ->
            Utilizations = [maps:get(utilization, M, 0.0) || M <- Metrics],
            case Utilizations of
                [] -> 0.0;
                _ -> lists:sum(Utilizations) / length(Utilizations)
            end;
        _ ->
            0.0
    end.

%% @doc Trigger surge capacity activation.
-spec trigger_surge(#burst_state{}) -> #burst_state{}.
trigger_surge(State) ->
    % Get all active pools
    Pools = case heijunka_balancer:balance_status() of
        {ok, Status} ->
            % Extract pool names from status
            maps:keys(maps:get(pool_utilization, Status, #{}));
        _ ->
            []
    end,

    % Add surge workers to each pool
    SurgeCost = add_surge_capacity(Pools),

    % Schedule surge end
    SurgeEndTimer = erlang:send_after(?SURGE_DURATION, self(), end_surge),

    State#burst_state{
        in_surge = true,
        surge_start_time = erlang:system_time(millisecond),
        surge_pools = Pools,
        surge_count = State#burst_state.surge_count + 1,
        total_surge_cost = State#burst_state.total_surge_cost + SurgeCost,
        cooldown_timer = SurgeEndTimer
    }.

%% @doc Add surge workers to all pools.
-spec add_surge_capacity([atom()]) -> float().
add_surge_capacity(Pools) ->
    % Add workers and calculate cost
    TotalCost = lists:foldl(
        fun(Pool, Acc) ->
            % Add surge workers (simulated - in practice would use poolboy dynamic API)
            SurgeCost = ?SURGE_WORKERS_PER_POOL * ?SURGE_COST_FACTOR,
            _ = log_surge_event(add, Pool, ?SURGE_WORKERS_PER_POOL),
            Acc + SurgeCost
        end,
        0.0,
        Pools
    ),
    TotalCost.

%% @doc Shrink surge capacity for a pool.
-spec shrink_surge_capacity(atom()) -> ok.
shrink_surge_capacity(Pool) ->
    % Gradually remove surge workers
    % In practice, would check if they're still needed
    _ = log_surge_event(remove, Pool, ?SURGE_WORKERS_PER_POOL),
    ok.

%% @doc Log surge events for monitoring.
-spec log_surge_event(atom(), atom(), integer()) -> ok.
log_surge_event(Action, Pool, WorkerCount) ->
    error_logger:info_msg(
        "Burst Handler: ~w ~p surge workers for pool ~p~n",
        [Action, WorkerCount, Pool]
    ).
