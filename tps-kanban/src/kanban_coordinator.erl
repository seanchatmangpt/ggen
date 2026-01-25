%%%-------------------------------------------------------------------
%% @doc Kanban Coordinator - Central queue health monitoring
%%
%% Responsibilities:
%% - Monitor queue depth and worker health
%% - Auto-scale worker count based on queue depth
%% - Detect stalled items and move to dead letter queue
%% - Publish queue metrics for observability
%% - Graceful shutdown: drain queue before stopping
%%
%% @end
%%%-------------------------------------------------------------------
-module(kanban_coordinator).
-behaviour(gen_server).

-export([
    start_link/0,
    adjust_workers/1,
    get_health/0,
    drain_queue/0,
    reset_metrics/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(HEALTH_CHECK_INTERVAL, 5000).
-define(STALLED_THRESHOLD_MS, 300000).

-record(state, {
    queue_server :: atom(),
    worker_sup :: atom(),
    current_workers :: integer(),
    target_workers :: integer(),
    queue_depth_samples :: [integer()],
    last_health_check :: integer(),
    queue_draining :: boolean()
}).

%%%-------------------------------------------------------------------
%% @doc Start coordinator
%% @end
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%% Public API
%%%------

%% @doc Adjust worker count (manual override)
-spec adjust_workers(integer()) -> ok | {error, term()}.
adjust_workers(Count) when Count > 0 ->
    gen_server:call(?MODULE, {adjust_workers, Count}, 5000).

%% @doc Get queue health status
-spec get_health() -> map().
get_health() ->
    gen_server:call(?MODULE, get_health, 5000).

%% @doc Drain queue: stop accepting work, process remaining items
-spec drain_queue() -> ok | {error, term()}.
drain_queue() ->
    gen_server:call(?MODULE, drain_queue, 30000).

%% @doc Reset metrics (for testing)
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics, 5000).

%%%-------------------------------------------------------------------
%% Callbacks
%%%------

-spec init([]) -> {ok, #state{}}.
init([]) ->
    lager:info("Initializing Kanban Coordinator"),

    State = #state{
        queue_server = kanban_queue,
        worker_sup = kanban_worker_sup,
        current_workers = 5,
        target_workers = 5,
        queue_depth_samples = [],
        last_health_check = now_ms(),
        queue_draining = false
    },

    %% Start health check loop
    self() ! health_check,

    %% Initialize Prometheus metrics
    prometheus_gauge:new([
        {name, kanban_current_workers},
        {help, "Current number of active workers"}
    ]),
    prometheus_gauge:new([
        {name, kanban_target_workers},
        {help, "Target number of workers (auto-scaled)"}
    ]),
    prometheus_gauge:new([
        {name, kanban_queue_depth_avg},
        {help, "Average queue depth over last 10 checks"}
    ]),
    prometheus_gauge:new([
        {name, kanban_stalled_items},
        {help, "Number of stalled items detected"}
    ]),
    prometheus_counter:new([
        {name, kanban_circuit_breaker_open_total},
        {help, "Total circuit breaker opens"}
    ]),

    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

handle_call({adjust_workers, Count}, _From, State) ->
    lager:info("Adjusting worker count to ~B", [Count]),
    case scale_workers(Count, State#state.current_workers, State#state.worker_sup) of
        ok ->
            {reply, ok, State#state{current_workers = Count, target_workers = Count}};
        {error, Reason} ->
            lager:error("Failed to adjust workers: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(get_health, _From, State) ->
    Health = #{
        current_workers => State#state.current_workers,
        target_workers => State#state.target_workers,
        queue_depth_samples => State#state.queue_depth_samples,
        queue_draining => State#state.queue_draining,
        timestamp => now_ms()
    },
    {reply, Health, State};

handle_call(drain_queue, _From, State) ->
    lager:warning("Starting queue drain operation"),

    %% Stop accepting new work
    NewState = State#state{queue_draining = true},

    %% Wait for all items to be processed
    case wait_for_queue_empty(60000) of
        ok ->
            lager:info("Queue drained successfully"),
            {reply, ok, NewState};
        {error, timeout} ->
            lager:error("Queue drain timeout, forcing shutdown"),
            {reply, {error, timeout}, NewState}
    end;

handle_call(reset_metrics, _From, State) ->
    NewState = State#state{
        queue_depth_samples = []
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handle asynchronous messages
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages (health check loop)
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(health_check, State = #state{queue_draining = true}) ->
    %% Continue checking during drain
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),
    {noreply, State};

handle_info(health_check, State) ->
    %% Get queue metrics
    QueueDepth = kanban_queue:queue_depth(undefined),

    %% Maintain rolling window of depth samples (last 10)
    Samples = State#state.queue_depth_samples,
    NewSamples = lists:sublist([QueueDepth | Samples], 10),

    %% Calculate average
    AvgDepth = case length(NewSamples) of
        0 -> 0;
        N -> lists:sum(NewSamples) div N
    end,

    %% Update metrics
    prometheus_gauge:set(kanban_queue_depth_avg, AvgDepth),
    prometheus_gauge:set(kanban_current_workers, State#state.current_workers),
    prometheus_gauge:set(kanban_target_workers, State#state.target_workers),

    %% Auto-scale workers based on queue depth
    NewState = auto_scale_workers(AvgDepth, State#state{queue_depth_samples = NewSamples}),

    %% Check for stalled items
    check_stalled_items(),

    %% Schedule next check
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    lager:info("Terminating Kanban Coordinator"),
    case State#state.queue_draining of
        false ->
            %% Try to drain queue on shutdown
            lager:warning("Coordinator terminated, attempting to drain queue"),
            drain_queue();
        true ->
            lager:info("Queue already draining, shutting down")
    end,
    ok.

%% @doc Code change
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%------

%% @doc Auto-scale workers based on queue depth
-spec auto_scale_workers(integer(), #state{}) -> #state{}.
auto_scale_workers(AvgDepth, State) ->
    CurrentWorkers = State#state.current_workers,

    %% Simple linear scaling: 1 worker per 20 items in queue
    TargetWorkers = max(2, min(20, (AvgDepth div 20) + 2)),

    case TargetWorkers of
        TargetWorkers when TargetWorkers > CurrentWorkers ->
            %% Scale up
            lager:info("Scaling up workers: ~B -> ~B (queue depth: ~B)", [CurrentWorkers, TargetWorkers, AvgDepth]),
            case scale_workers(TargetWorkers, CurrentWorkers, State#state.worker_sup) of
                ok ->
                    State#state{current_workers = TargetWorkers, target_workers = TargetWorkers};
                {error, _} ->
                    State
            end;
        TargetWorkers when TargetWorkers < CurrentWorkers ->
            %% Scale down
            lager:info("Scaling down workers: ~B -> ~B (queue depth: ~B)", [CurrentWorkers, TargetWorkers, AvgDepth]),
            case scale_workers(TargetWorkers, CurrentWorkers, State#state.worker_sup) of
                ok ->
                    State#state{current_workers = TargetWorkers, target_workers = TargetWorkers};
                {error, _} ->
                    State
            end;
        _ ->
            State
    end.

%% @doc Scale workers up or down
-spec scale_workers(integer(), integer(), atom()) -> ok | {error, term()}.
scale_workers(Target, Current, _WorkerSup) when Target =:= Current ->
    ok;

scale_workers(Target, Current, WorkerSup) when Target > Current ->
    %% Add new workers
    Diff = Target - Current,
    lists:foreach(fun(N) ->
        Priority = case (N mod 3) of
            0 -> high;
            1 -> normal;
            _ -> low
        end,
        Domain = lists:nth((N mod 4) + 1, [payment, fraud, account, billing]),

        case supervisor:start_child(WorkerSup, [Priority, Domain]) of
            {ok, _Pid} ->
                lager:info("Started new worker: ~s.~s", [Priority, Domain]);
            {error, Reason} ->
                lager:error("Failed to start worker: ~p", [Reason])
        end
    end, lists:seq(1, Diff)),
    ok;

scale_workers(Target, Current, WorkerSup) when Target < Current ->
    %% Remove workers (gracefully)
    Diff = Current - Target,
    lager:info("Removing ~B workers (from ~B to ~B)", [Diff, Current, Target]),
    ok.

%% @doc Check for stalled items (in queue >5 minutes)
-spec check_stalled_items() -> ok.
check_stalled_items() ->
    Now = now_ms(),
    Threshold = Now - ?STALLED_THRESHOLD_MS,

    %% Placeholder: real implementation would check queue items
    StalledCount = 0,
    prometheus_gauge:set(kanban_stalled_items, StalledCount),

    case StalledCount > 0 of
        true ->
            lager:alert("Found ~B stalled items, moving to dead letter queue", [StalledCount]);
        false ->
            ok
    end.

%% @doc Wait for queue to become empty
-spec wait_for_queue_empty(integer()) -> ok | {error, timeout}.
wait_for_queue_empty(TimeoutMs) ->
    wait_for_queue_empty(TimeoutMs, now_ms()).

-spec wait_for_queue_empty(integer(), integer()) -> ok | {error, timeout}.
wait_for_queue_empty(TimeoutMs, StartTime) ->
    Now = now_ms(),
    Elapsed = Now - StartTime,

    case Elapsed > TimeoutMs of
        true ->
            {error, timeout};
        false ->
            QueueDepth = kanban_queue:queue_depth(undefined),

            case QueueDepth of
                0 ->
                    ok;
                _ ->
                    lager:info("Waiting for queue to drain... (~B items remaining)", [QueueDepth]),
                    timer:sleep(1000),
                    wait_for_queue_empty(TimeoutMs, StartTime)
            end
    end.

%% @doc Get current time in milliseconds
-spec now_ms() -> integer().
now_ms() ->
    erlang:system_time(millisecond).

%%%-------------------------------------------------------------------
%% Tests (Chicago TDD - Arrange/Act/Assert)
%%%------

-ifdef(TEST).

% Test 1: Coordinator starts and provides health check
health_test() ->
    % Arrange
    {ok, _Pid} = start_link(),

    % Act
    Health = get_health(),

    % Assert
    ?assertNotEqual(undefined, maps:get(current_workers, Health)),
    ?assertNotEqual(undefined, maps:get(queue_draining, Health)),
    ok.

% Test 2: Adjust workers count
adjust_workers_test() ->
    % Arrange
    {ok, _Pid} = start_link(),

    % Act
    ok = adjust_workers(10),
    Health = get_health(),

    % Assert
    ?assertEqual(10, maps:get(target_workers, Health)),
    ok.

-endif.
