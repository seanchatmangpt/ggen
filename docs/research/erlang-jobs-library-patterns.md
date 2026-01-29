# Comprehensive Erlang/OTP Patterns for Production-Ready Jobs Library

**Research Date:** 2026-01-29
**Status:** Complete
**Researcher:** Research Agent

---

## Executive Summary

This research document provides comprehensive patterns and architectural guidance for building a production-ready jobs library in Erlang/OTP. Based on analysis of existing codebases (tps-kanban, tps-jidoka, tps-heijunka) and industry best practices, this guide covers OTP design patterns, worker pool management, testing strategies, and performance benchmarking.

**Key Findings:**
- **Pull-based architecture** (Kanban pattern) is superior to push-based for backpressure handling
- **gen_statem** provides robust state machine semantics for circuit breakers and job state
- **Chicago TDD** (state-based, real collaborators) is the standard for Erlang testing
- **Poolboy** is the de facto standard for worker pool management
- **common_test** with testcontainers provides comprehensive integration testing

---

## 1. OTP Design Patterns

### 1.1 gen_server for Job Workers

**Pattern:** Each job worker is a gen_server that pulls work from a queue.

**Example from tps-kanban:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Kanban Worker - Pull-based work processing
%% Each worker pulls N items from queue, processes them, ACKs on success
%%
%% Lifecycle: init → pull N items → process → ACK → repeat
%%%-------------------------------------------------------------------
-module(kanban_worker).
-behaviour(gen_server).

-record(state, {
    worker_id :: string(),
    priority :: atom(),
    domain :: atom(),
    pull_size :: integer(),
    process_timeout :: integer(),
    queue_server :: atom(),
    processing_count :: integer(),
    failed_count :: integer(),
    success_count :: integer(),
    circuit_breaker_open :: boolean(),
    metrics :: map()
}).

%% @doc Start worker
-spec start_link(atom(), atom()) -> {ok, pid()} | {error, term()}.
start_link(Priority, Domain) ->
    WorkerId = generate_worker_id(Priority, Domain),
    gen_server:start_link({local, list_to_atom(WorkerId)}, ?MODULE,
                          {Priority, Domain, WorkerId}, []).

%% @doc Initialize worker state
init({Priority, Domain, WorkerId}) ->
    {ok, PullSize} = application:get_env(tps_kanban, worker_pull_size),
    {ok, ProcessTimeout} = application:get_env(tps_kanban, worker_process_timeout_ms),

    State = #state{
        worker_id = WorkerId,
        priority = Priority,
        domain = Domain,
        pull_size = PullSize,
        process_timeout = ProcessTimeout,
        queue_server = kanban_queue,
        processing_count = 0,
        failed_count = 0,
        success_count = 0,
        circuit_breaker_open = false,
        metrics = #{pulled => 0, processed => 0, failed => 0, acked => 0}
    },

    %% Subscribe to work queue
    ok = kanban_queue:subscribe(Priority, Domain),

    %% Start pull loop
    self() ! pull_work,

    {ok, State}.

%% @doc Handle pull loop
handle_info(pull_work, State = #state{circuit_breaker_open = false}) ->
    case pull_work(self()) of
        {ok, []} ->
            %% No work available, try again soon
            erlang:send_after(1000, self(), pull_work),
            {noreply, State};
        {ok, Items} ->
            %% Process items
            NewState = process_items(Items, State),
            %% Continue pulling
            self() ! pull_work,
            {noreply, NewState};
        {error, Reason} ->
            %% Backoff on error
            erlang:send_after(5000, self(), pull_work),
            {noreply, State}
    end.
```

**Key Patterns:**
- **Pull-based semantics:** Workers pull work, don't wait to be pushed
- **Circuit breaker integration:** Stop pulling when failures accumulate
- **Metrics tracking:** Track pulled, processed, failed, acked counts
- **Backoff on errors:** Use `erlang:send_after/3` for exponential backoff

---

### 1.2 Supervisor Trees for Fault Tolerance

**Pattern:** one_for_one supervisor strategy for independent workers.

**Example from tps-kanban:**

```erlang
-module(tps_kanban_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    ChildSpecs = [
        #{
            id => kanban_queue,
            start => {kanban_queue, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [kanban_queue]
        },
        #{
            id => kanban_worker_sup,
            start => {kanban_worker_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [kanban_worker_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Supervision Strategies:**
- **one_for_one:** If a worker crashes, only restart that worker (use for independent workers)
- **one_for_all:** If any worker crashes, restart all workers (use for interdependent components)
- **rest_for_one:** If a worker crashes, restart it and all workers started after it
- **simple_one_for_one:** For dynamic worker pools (use with poolboy)

**Restart Intensities:**
- **intensity:** Max restarts allowed in `period` seconds (default: 5 restarts in 10 seconds)
- **shutdown:** Graceful shutdown timeout (5000ms for workers, infinity for supervisors)

---

### 1.3 gen_statem for Job State Machines

**Pattern:** Use gen_statem for circuit breakers and complex job state transitions.

**Example from tps-jidoka (Circuit Breaker):**

```erlang
%%%-------------------------------------------------------------------
%% @doc Circuit Breaker - State Machine for Failure Detection
%% States: closed (normal), open (failing), half_open (recovery)
%%%-------------------------------------------------------------------
-module(jidoka_circuit_breaker).
-behaviour(gen_statem).

-record(data, {
    threshold :: integer(),      % Failures to trigger open
    window_ms :: integer(),       % Time window for counting failures
    recovery_ms :: integer(),     % Time before half_open attempt
    failures :: list(),           % List of failure timestamps
    recovery_timer :: reference() % Timer reference for recovery
}).

%% @doc Callback mode: state_functions (one function per state)
callback_mode() -> state_functions.

%% @doc Initialize in closed state
init(Config) ->
    Threshold = maps:get(threshold, Config, 5),
    WindowMs = maps:get(window_ms, Config, 10000),
    RecoveryMs = maps:get(recovery_ms, Config, 30000),

    Data = #data{
        threshold = Threshold,
        window_ms = WindowMs,
        recovery_ms = RecoveryMs,
        failures = [],
        recovery_timer = undefined
    },

    {ok, closed, Data}.

%% @doc CLOSED state: Normal operation
closed({call, {call, Fun}}, From, Data) ->
    try
        Result = Fun(),
        {keep_state_and_data, [{reply, From, Result}]}
    catch
        Error:Reason ->
            NewData = add_failure(Data),
            case should_open(NewData) of
                true ->
                    logger:error("Circuit breaker opening: threshold exceeded"),
                    {next_state, open, start_recovery_timer(NewData),
                     [{reply, From, {error, Error}}]};
                false ->
                    {keep_state, NewData, [{reply, From, {error, Error}}]}
            end
    end.

%% @doc OPEN state: Reject all calls (fail-fast)
open({call, {call, _Fun}}, From, Data) ->
    logger:warning("Circuit breaker rejecting call: circuit is open"),
    {keep_state_and_data, [{reply, From, {error, circuit_open}}]}.

%% @doc HALF_OPEN state: Allow one test call
half_open({call, {call, Fun}}, From, Data) ->
    try
        Result = Fun(),
        logger:info("Circuit breaker test call succeeded, closing circuit"),
        {next_state, closed, Data#data{failures = []},
         [{reply, From, Result}]}
    catch
        Error:Reason ->
            logger:warning("Circuit breaker test call failed, reopening"),
            {next_state, open, start_recovery_timer(Data),
             [{reply, From, {error, Error}}]}
    end.

%% @doc Start recovery timer for transition to half_open
start_recovery_timer(#data{recovery_ms = RecoveryMs} = Data) ->
    Ref = erlang:send_after(RecoveryMs, self(),
                            {timeout, Ref, recovery_attempt}),
    Data#data{recovery_timer = Ref}.
```

**gen_statem Key Concepts:**
- **State functions:** One function per state (closed/open/half_open)
- **State transitions:** `{next_state, NewState, NewData, Actions}`
- **Timeouts:** Use `erlang:send_after/3` for state transitions
- **Actions:** List of actions to execute after transition (e.g., reply to caller)

---

### 1.4 Poolboy for Worker Pool Management

**Pattern:** Use poolboy for fixed-size worker pools with overflow control.

**Example from tps-jidoka:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Jidoka Worker Pool - Fixed-Size Request Handler Pool
%% Jidoka principle: Don't queue unbounded requests. If pool is full,
%% reject the request (fail-fast) rather than queue it indefinitely.
%%%-------------------------------------------------------------------
-module(jidoka_worker_pool).

%% @doc Start the worker pool supervisor and poolboy pool
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    PoolName = maps:get(pool_name, Config, jidoka_worker_pool),
    PoolSize = maps:get(pool_size, Config, 10),
    MaxOverflow = maps:get(max_overflow, Config, 0),  % 0 for Jidoka

    PoolboyConfig = [
        {name, {local, PoolName}},
        {worker_module, ?MODULE},
        {size, PoolSize},
        {max_overflow, MaxOverflow}
    ],

    poolboy:start_link(PoolboyConfig).

%% @doc Execute a function in the worker pool
-spec execute(fun()) -> any() | {error, pool_exhausted}.
execute(Fun) ->
    StartTime = erlang:system_time(millisecond),
    case poolboy:checkout(PoolName, false) of  % false = don't block
        full ->
            {error, pool_exhausted};  % Fail-fast
        Worker ->
            try
                Result = gen_server:call(Worker, {execute, Fun}),
                ElapsedMs = erlang:system_time(millisecond) - StartTime,
                Result
            after
                poolboy:checkin(PoolName, Worker)
            end
    end.
```

**Poolboy Configuration:**
- **size:** Fixed pool size (number of workers always running)
- **max_overflow:** Max temporary workers when pool exhausted (0 for fail-fast)
- **strategy:** `lifo` (last-in-first-out) or `fifo` (first-in-first-out)
- **worker_module:** Module implementing worker logic

**Best Practices:**
- Use **max_overflow = 0** for Jidoka (fail-fast, no queueing)
- Use **checkout(Pool, false)** to reject when pool full (non-blocking)
- Always use **try/after** to ensure checkin even on exceptions

---

### 1.5 Rate Limiting with Token Bucket

**Pattern:** Token bucket algorithm for rate limiting requests.

**Example from tps-jidoka:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Rate Limiter - Token Bucket Flow Control
%% Algorithm:
%% - Tokens refill at fixed rate (rate_limit per second)
%% - Burst capacity limits maximum tokens (burst_size)
%% - Each request consumes N tokens
%% - If insufficient tokens: reject request (fail-fast)
%%%-------------------------------------------------------------------
-module(jidoka_rate_limiter).
-behaviour(gen_server).

-record(state, {
    rate_limit :: float(),        % Tokens per second
    burst_size :: float(),        % Max tokens in bucket
    tokens :: float(),            % Current tokens
    last_refill :: integer(),     % Last refill timestamp (microseconds)
    rejected :: integer(),        % Count of rejected requests
    accepted :: integer()         % Count of accepted requests
}).

%% @doc Acquire tokens from the rate limiter
-spec acquire(pos_integer()) -> ok | {error, rate_limit_exceeded}.
acquire(Tokens) ->
    gen_server:call(?MODULE, {acquire, Tokens}, 5000).

%% @doc Handle token acquisition
handle_call({acquire, Tokens}, _From, State) ->
    NewState = refill_tokens(State),
    case has_tokens(NewState, Tokens) of
        true ->
            UpdatedState = NewState#state{
                tokens = NewState#state.tokens - Tokens,
                accepted = NewState#state.accepted + 1
            },
            {reply, ok, UpdatedState};
        false ->
            UpdatedState = NewState#state{
                rejected = NewState#state.rejected + 1
            },
            {reply, {error, rate_limit_exceeded}, UpdatedState}
    end.

%% @doc Refill tokens based on elapsed time
refill_tokens(#state{rate_limit = Rate, burst_size = Burst,
                      tokens = Tokens, last_refill = LastRefill} = State) ->
    Now = erlang:system_time(microsecond),
    ElapsedUs = Now - LastRefill,

    %% Calculate tokens to add: (elapsed_time_seconds * rate_limit)
    TokensToAdd = (ElapsedUs / 1_000_000) * Rate,
    NewTokens = min(Burst, Tokens + TokensToAdd),

    State#state{tokens = NewTokens, last_refill = Now}.
```

**Token Bucket Parameters:**
- **rate_limit:** Tokens added per second (e.g., 1000/sec)
- **burst_size:** Maximum tokens in bucket (e.g., 500 for burst tolerance)
- **refill interval:** Continuous refill based on elapsed time

---

## 2. Jobs Library Architecture Patterns

### 2.1 Queue Implementations

#### Pull-Based Queue (Kanban Pattern)

**Pattern:** Workers pull work from queue instead of being pushed.

**Advantages:**
- **Backpressure handling:** Workers pull at their own pace
- **Circuit breaker integration:** Stop pulling when circuit is open
- **Load leveling:** Workers self-regulate based on capacity

**Example from tps-kanban:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Kanban Queue - NATS-based pull-semantics work queue
%% Primary: NATS (lightweight, fast, ephemeral)
%% Fallback: RabbitMQ (persistence layer)
%%%-------------------------------------------------------------------
-module(kanban_queue).

%% @doc Publish work item to queue
%% Item = {Priority, Domain, Payload, Deadline}
-spec publish({atom(), atom(), term(), integer()}) ->
    {ok, string()} | {error, term()}.
publish({Priority, Domain, Payload, Deadline}) ->
    WorkId = generate_work_id(),
    Subject = subject_name(Priority, Domain),  % kanban.high.payment

    Item = #{
        <<"id">> => list_to_binary(WorkId),
        <<"priority">> => atom_to_binary(Priority),
        <<"domain">> => atom_to_binary(Domain),
        <<"payload">> => Payload,
        <<"deadline">> => Deadline,
        <<"created_at">> => now_ms(),
        <<"retry_count">> => 0
    },

    Msg = jiffy:encode(Item),

    case gnat:pub(Conn, Subject, Msg) of
        ok ->
            prometheus_counter:inc(kanban_published_total),
            {ok, WorkId};
        {error, Reason} ->
            fallback_publish(WorkId, Subject, Msg, State)
    end.

%% @doc Subscribe worker to priority/domain queue
-spec subscribe(atom(), atom()) -> ok | {error, term()}.
subscribe(Priority, Domain) ->
    Subject = subject_name(Priority, Domain),
    case gnat:sub(Conn, self(), Subject) of
        {ok, Sid} ->
            logger:info("Subscribed to ~s (SID: ~p)", [Subject, Sid]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
```

**NATS Subject Pattern:**
- `kanban.{priority}.{domain}` (e.g., `kanban.high.payment`)
- Allows priority-based routing
- Domain-specific worker pools

---

#### Priority Queue Implementation

**Pattern:** Multiple priority levels (high, normal, low) with separate queues.

**Example:**

```erlang
%% @doc Priority levels
-type priority() :: high | normal | low.

%% @doc NATS subject naming for priority
subject_name(Priority, Domain) ->
    lists:flatten(io_lib:format("kanban.~s.~s", [Priority, Domain])).

%% @doc Workers subscribe to specific priority/domain combinations
start_worker(Priority, Domain) ->
    kanban_worker:start_link(Priority, Domain).

%% Examples:
%% High priority payment worker: start_worker(high, payment)
%% Normal priority fraud worker: start_worker(normal, fraud)
%% Low priority billing worker: start_worker(low, billing)
```

---

#### Delayed Queue (Scheduling)

**Pattern:** Schedule work items to be published at specific times.

**Example from tps-kanban:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Kanban Scheduler - Schedule periodic work items
%%%-------------------------------------------------------------------
-module(kanban_scheduler).

-record(scheduled_job, {
    id :: string(),
    priority :: atom(),
    domain :: atom(),
    interval_ms :: integer(),
    next_scheduled :: integer(),
    payload :: map(),
    deadline_ms :: integer(),
    active :: boolean()
}).

%% @doc Schedule periodic work
-spec schedule_periodic(atom(), atom(), integer(), integer()) ->
    {ok, string()} | {error, term()}.
schedule_periodic(Priority, Domain, IntervalMs, DeadlineMs) ->
    JobId = generate_job_id(),
    Now = now_ms(),

    Job = #scheduled_job{
        id = JobId,
        priority = Priority,
        domain = Domain,
        interval_ms = IntervalMs,
        next_scheduled = Now + IntervalMs,
        payload = #{scheduled => true},
        deadline_ms = DeadlineMs,
        active = true
    },

    gen_server:call(?MODULE, {schedule_periodic, Job}).

%% @doc Check and schedule jobs that are due
handle_info(check_schedules, State) ->
    Now = now_ms(),
    NewState = maps:map(fun(_JobId, Job) ->
        case should_schedule(Job, Now) of
            true ->
                schedule_job(Job),
                Job#scheduled_job{
                    last_scheduled = Now,
                    next_scheduled = Now + Job#scheduled_job.interval_ms
                };
            false ->
                Job
        end
    end, State#state.scheduled_jobs),

    erlang:send_after(1000, self(), check_schedules),
    {noreply, NewState}.
```

---

### 2.2 Worker Pool Management

#### Dynamic Scaling (Heijunka Pattern)

**Pattern:** Automatically scale worker pool based on utilization.

**Example from tps-heijunka:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Heijunka Pool - Dynamic scaling based on utilization
%% Target utilization: 70%
%% Scale up threshold: 80%
%% Scale down threshold: 50%
%%%-------------------------------------------------------------------
-module(heijunka_pool).

-define(TARGET_UTILIZATION, 0.70).
-define(SCALE_UP_THRESHOLD, 0.80).
-define(SCALE_DOWN_THRESHOLD, 0.50).
-define(MIN_WORKERS, 2).
-define(MAX_WORKERS, 50).

%% @doc Update metrics and check if scaling is needed
update_and_scale(PoolName, State = #pool_state{worker_count = WC}) ->
    {AvailableWorkers, _Overflow} = poolboy:status(PoolName),
    Utilization = (WC - AvailableWorkers) / WC,

    NewState = if
        Utilization > ?SCALE_UP_THRESHOLD andalso WC < ?MAX_WORKERS ->
            add_worker(PoolName),
            State#pool_state{worker_count = WC + 1};
        Utilization < ?SCALE_DOWN_THRESHOLD andalso WC > ?MIN_WORKERS ->
            remove_worker(PoolName),
            State#pool_state{worker_count = WC - 1};
        true ->
            State
    end,

    NewState.
```

**Scaling Metrics:**
- **Utilization:** `(total_workers - available_workers) / total_workers`
- **Queue depth:** Number of items waiting
- **Latency:** p99 latency for job processing
- **Error rate:** Failed jobs / total jobs

---

### 2.3 Job Persistence

#### ETS-Based In-Memory Persistence

**Pattern:** Use ETS for fast, in-memory job tracking.

```erlang
%% @doc Initialize ETS table for job tracking
init_job_table() ->
    ets:new(job_tracking, [
        named_table,
        set,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

%% @doc Store job metadata
store_job(JobId, Metadata) ->
    ets:insert(job_tracking, {JobId, Metadata}).

%% @doc Retrieve job metadata
lookup_job(JobId) ->
    case ets:lookup(job_tracking, JobId) of
        [{JobId, Metadata}] -> {ok, Metadata};
        [] -> {error, not_found}
    end.

%% @doc Delete completed job
delete_job(JobId) ->
    ets:delete(job_tracking, JobId).
```

**ETS Best Practices:**
- Use `{read_concurrency, true}` for read-heavy workloads
- Use `{write_concurrency, true}` for concurrent writes
- Use `ordered_set` for range queries
- Use `set` for simple key-value lookups

---

#### Mnesia-Based Durable Persistence

**Pattern:** Use Mnesia for distributed, durable job storage.

```erlang
%% @doc Initialize Mnesia schema
init_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),

    mnesia:create_table(job_metadata, [
        {attributes, record_info(fields, job_metadata)},
        {disc_copies, [node()]},
        {type, set},
        {index, [domain, priority, status]}
    ]).

-record(job_metadata, {
    id :: string(),
    domain :: atom(),
    priority :: atom(),
    status :: pending | processing | completed | failed,
    payload :: term(),
    created_at :: integer(),
    deadline :: integer(),
    retries :: integer()
}).

%% @doc Store job in Mnesia
store_job_mnesia(Job) ->
    F = fun() ->
        mnesia:write(Job)
    end,
    mnesia:transaction(F).

%% @doc Query jobs by status
query_jobs_by_status(Status) ->
    F = fun() ->
        mnesia:index_read(job_metadata, Status, #job_metadata.status)
    end,
    {atomic, Jobs} = mnesia:transaction(F),
    Jobs.
```

---

## 3. Testing Patterns

### 3.1 Chicago TDD (State-Based Testing)

**Pattern:** Test observable state changes, use real collaborators, no mocks.

**Example from tps-kanban:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Kanban System - Comprehensive Integration Test Suite
%% Chicago TDD: Real NATS/RabbitMQ, state-based assertions, no mocks
%%%-------------------------------------------------------------------
-module(kanban_SUITE).
-behaviour(ct_suite).

%% TEST 1: Single worker pulls and processes items
%% AAA Pattern: Arrange/Act/Assert
test_single_worker_pulls_and_processes(_Config) ->
    % Arrange: Start worker
    {ok, WorkerPid} = kanban_worker:start_link(high, payment),

    % Publish 5 work items
    WorkIds = [publish_test_item(high, payment) || _ <- lists:seq(1, 5)],

    % Act: Pull work
    timer:sleep(500),
    {ok, Items} = kanban_worker:pull_work(WorkerPid),

    % Assert: Verify items were pulled
    ?assert(length(Items) >= 0),

    % Get worker status
    Status = kanban_worker:get_status(WorkerPid),
    ?assertEqual(high, maps:get(priority, Status)),
    ?assertEqual(payment, maps:get(domain, Status)),

    % Cleanup
    kanban_worker:stop(WorkerPid),
    ok.

%% TEST 2: Stress test - 10,000 items
test_stress_10000_items(_Config) ->
    % Arrange: Publish 10,000 items
    [publish_test_item(high, payment) || _ <- lists:seq(1, 1000)],
    [publish_test_item(normal, fraud) || _ <- lists:seq(1, 1000)],
    [publish_test_item(normal, account) || _ <- lists:seq(1, 1000)],
    [publish_test_item(low, billing) || _ <- lists:seq(1, 1000)],

    % Start multiple workers
    {ok, W1} = kanban_worker:start_link(high, payment),
    {ok, W2} = kanban_worker:start_link(normal, fraud),
    {ok, W3} = kanban_worker:start_link(normal, account),
    {ok, W4} = kanban_worker:start_link(low, billing),

    % Act: Process items
    timer:sleep(2000),

    % Assert: Verify metrics
    Metrics = kanban_queue:get_metrics(),
    Published = maps:get(published, Metrics, 0),
    ?assert(Published >= 4000),

    % Cleanup
    kanban_worker:stop(W1),
    kanban_worker:stop(W2),
    kanban_worker:stop(W3),
    kanban_worker:stop(W4),
    ok.
```

**Chicago TDD Principles:**
1. **State-based:** Assert on observable outputs and state changes
2. **Real collaborators:** Use actual NATS, RabbitMQ, not mocks
3. **AAA pattern:** Arrange → Act → Assert
4. **Behavior verification:** Test what the code does, not how

---

### 3.2 common_test Framework

**Pattern:** Use common_test for integration and system tests.

**Suite Configuration:**

```erlang
-module(kanban_SUITE).
-behaviour(ct_suite).

%% @doc Suite configuration
suite() ->
    [
        {timetrap, {seconds, 300}},
        {require, nats_server},
        {require, rabbitmq_server}
    ].

%% @doc Setup before all tests
init_per_suite(Config) ->
    application:ensure_all_started(gnat),
    application:ensure_all_started(amqp_client),
    application:ensure_all_started(tps_kanban),

    wait_for_nats(10),
    wait_for_rabbitmq(10),

    Config.

%% @doc Teardown after all tests
end_per_suite(_Config) ->
    application:stop(tps_kanban),
    ok.

%% @doc Setup before each test
init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p~n", [TestCase]),
    kanban_coordinator:reset_metrics(),
    Config.

%% @doc All test cases
all() ->
    [
        test_single_worker,
        test_multiple_workers,
        test_backpressure,
        test_stress_10000_items
    ].
```

**Run Tests:**

```bash
# Run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite kanban_SUITE

# Run specific test case
rebar3 ct --suite kanban_SUITE --case test_stress_10000_items
```

---

### 3.3 Testcontainers for Integration Testing

**Pattern:** Use testcontainers to spin up real NATS, RabbitMQ, etc.

**Example:**

```erlang
%% @doc Wait for NATS to be available (usually in docker-compose)
wait_for_nats(Attempts) when Attempts > 0 ->
    case gnat:start_link(["localhost:4222"]) of
        {ok, Conn} ->
            gnat:close(Conn),
            ct:log("NATS available~n", []),
            ok;
        {error, _} ->
            ct:log("Waiting for NATS (attempts left: ~B)~n", [Attempts - 1]),
            timer:sleep(1000),
            wait_for_nats(Attempts - 1)
    end;

wait_for_nats(0) ->
    ct:log("WARNING: NATS not available, continuing with RabbitMQ fallback~n", []),
    ok.

%% @doc Wait for RabbitMQ to be available
wait_for_rabbitmq(Attempts) when Attempts > 0 ->
    AmqpParams = #amqp_params_network{
        host = "localhost",
        port = 5672,
        username = <<"guest">>,
        password = <<"guest">>
    },

    case amqp_connection:start(AmqpParams) of
        {ok, Conn} ->
            amqp_connection:close(Conn),
            ct:log("RabbitMQ available~n", []),
            ok;
        {error, _} ->
            ct:log("Waiting for RabbitMQ (attempts left: ~B)~n", [Attempts - 1]),
            timer:sleep(1000),
            wait_for_rabbitmq(Attempts - 1)
    end.
```

---

## 4. Benchmarking Tools and Patterns

### 4.1 Erlang timer Module Patterns

**Pattern:** Use `timer:tc/3` for microsecond precision timing.

**Example:**

```erlang
%% @doc Measure single operation latency
measure_single_latency(Pid) ->
    {Time, _Result} = timer:tc(fun() ->
        light_governors:check_quota(Pid, <<"tenant">>, #{})
    end),
    Time / 1000.0.  % Convert to milliseconds

%% @doc Measure batch operation throughput
measure_throughput(Pid, Count) ->
    Start = erlang:system_time(millisecond),

    [do_work(Pid) || _ <- lists:seq(1, Count)],

    End = erlang:system_time(millisecond),
    ElapsedSeconds = (End - Start) / 1000,
    Count / ElapsedSeconds.  % Events per second
```

---

### 4.2 Benchee-Style Benchmarking

**Pattern:** Structured benchmark suite with warmup, samples, and percentiles.

**Example from benchmark_comparison.erl:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Comprehensive benchmark suite with warmup and percentiles
%%%-------------------------------------------------------------------

%% @doc Measure event processing latency (p99)
run_latency_test() ->
    {ok, Pid} = light_governors:start_link(quota),

    % Warm up (100 samples)
    [light_governors:check_quota(Pid, <<"tenant">>, #{})
     || _ <- lists:seq(1, 100)],

    % Measure latency (1000 samples)
    Latencies = [measure_single_latency(Pid) || _ <- lists:seq(1, 1000)],

    light_governors:halt(Pid),

    % Calculate percentiles
    Sorted = lists:sort(Latencies),
    P50Index = (1000 * 50) div 100,
    P95Index = (1000 * 95) div 100,
    P99Index = (1000 * 99) div 100,

    P50 = lists:nth(P50Index, Sorted),
    P95 = lists:nth(P95Index, Sorted),
    P99 = lists:nth(P99Index, Sorted),

    io:format("Latency results:~n"),
    io:format("  p50: ~.2fms~n", [P50]),
    io:format("  p95: ~.2fms~n", [P95]),
    io:format("  p99: ~.2fms~n", [P99]),

    {P50, P95, P99}.

%% @doc Measure throughput (events per second)
run_throughput_test() ->
    {ok, Pid} = light_governors:start_link(entitlement),

    % Run for 5 seconds
    Start = erlang:monotonic_time(millisecond),
    Count = run_throughput_loop(Pid, 0, Start),
    End = erlang:monotonic_time(millisecond),

    light_governors:halt(Pid),

    ElapsedSeconds = (End - Start) / 1000,
    Throughput = Count / ElapsedSeconds,

    io:format("Throughput: ~.1f events/sec~n", [Throughput]),
    Throughput.

run_throughput_loop(Pid, Count, Start) ->
    Current = erlang:monotonic_time(millisecond),
    case Current - Start > 5000 of
        true -> Count;
        false ->
            _ = do_work(Pid),
            run_throughput_loop(Pid, Count + 1, Start)
    end.
```

**Benchmark Metrics:**
- **Latency:** p50, p95, p99 percentiles (milliseconds)
- **Throughput:** Events per second
- **Memory:** RSS, total allocated, process count
- **CPU:** Utilization percentage

---

### 4.3 Performance Profiling

**Tools:**
- **fprof:** Function profiling (detailed, high overhead)
- **eprof:** Time profiling (lower overhead than fprof)
- **eflame:** Flame graph generation
- **recon:** Production debugging and profiling

**Example with fprof:**

```erlang
%% @doc Profile function with fprof
profile_with_fprof() ->
    fprof:trace([start, {procs, [self()]}]),

    % Run code to profile
    {ok, Pid} = kanban_worker:start_link(high, payment),
    [publish_test_item(high, payment) || _ <- lists:seq(1, 100)],
    timer:sleep(1000),
    kanban_worker:stop(Pid),

    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse([{dest, "fprof_analysis.txt"}]).
```

---

## 5. Stress Testing Patterns

### 5.1 Concurrency Stress Testing

**Pattern:** Spawn multiple concurrent workers and measure system stability.

**Example:**

```erlang
%% @doc Stress test with N concurrent workers
stress_test_concurrent_workers(NumWorkers) ->
    % Spawn workers
    Workers = [spawn_link(fun() -> worker_loop() end)
               || _ <- lists:seq(1, NumWorkers)],

    % Publish work items
    [publish_test_item(high, payment) || _ <- lists:seq(1, 10000)],

    % Monitor for crashes
    monitor_workers(Workers, 10000).

worker_loop() ->
    {ok, WorkerPid} = kanban_worker:start_link(high, payment),
    receive
        stop -> kanban_worker:stop(WorkerPid)
    after
        60000 -> worker_loop()
    end.

monitor_workers(Workers, Timeout) ->
    receive
        {'EXIT', Pid, Reason} ->
            ct:fail("Worker crashed: ~p, reason: ~p", [Pid, Reason])
    after
        Timeout ->
            [Pid ! stop || Pid <- Workers],
            ok
    end.
```

---

### 5.2 Load Testing with Tsung

**Pattern:** Use Tsung for HTTP/WebSocket/MQTT load testing.

**Example tsung.xml:**

```xml
<?xml version="1.0"?>
<!DOCTYPE tsung SYSTEM "/usr/share/tsung/tsung-1.0.dtd">
<tsung loglevel="notice" version="1.0">
  <clients>
    <client host="localhost" maxusers="10000" />
  </clients>

  <servers>
    <server host="localhost" port="8080" type="tcp" />
  </servers>

  <load>
    <arrivalphase phase="1" duration="60" unit="second">
      <users maxnumber="1000" arrivalrate="100" unit="second" />
    </arrivalphase>
  </load>

  <sessions>
    <session name="job_submission" probability="100" type="ts_http">
      <request>
        <http url="/jobs" method="POST" contents='{"priority":"high","domain":"payment"}' />
      </request>
    </session>
  </sessions>
</tsung>
```

**Run:**

```bash
tsung -f tsung.xml start
tsung_stats.pl
```

---

## 6. Project Structure (rebar3 Best Practices)

### 6.1 Standard OTP Application Structure

```
my_jobs_lib/
├── rebar.config               # Dependencies and build config
├── rebar.lock                 # Locked dependency versions
├── src/
│   ├── my_jobs_lib.app.src   # Application resource file
│   ├── my_jobs_lib_app.erl   # Application callback
│   ├── my_jobs_lib_sup.erl   # Top-level supervisor
│   ├── job_queue.erl          # Queue implementation
│   ├── job_worker.erl         # Worker implementation
│   ├── job_worker_sup.erl     # Worker supervisor
│   └── job_scheduler.erl      # Scheduler
├── include/
│   └── my_jobs_lib.hrl        # Shared header files
├── test/
│   ├── job_queue_SUITE.erl    # Integration tests
│   ├── job_worker_SUITE.erl   # Worker tests
│   └── stress_test_SUITE.erl  # Stress tests
├── config/
│   ├── sys.config             # System configuration
│   └── vm.args                # VM arguments
├── priv/
│   └── schemas/               # JSON schemas, etc.
├── doc/
│   └── overview.edoc          # Documentation
└── README.md
```

---

### 6.2 rebar.config Example

**From tps-kanban:**

```erlang
{erl_opts, [
    debug_info,
    warnings_as_errors,
    {parse_transform, lager_transform}
]}.

{deps, [
    {lager, "3.9.2"},
    {gnat, "1.5.0"},
    {amqp_client, "3.13.2"},
    {prometheus, "4.10.0"},
    {jiffy, "1.1.1"},
    {poolboy, "1.5.2"}
]}.

{profiles, [
    {dev, [
        {erl_opts, [debug_info]},
        {relx, [{dev_mode, true}, {include_erts, false}]}
    ]},

    {prod, [
        {erl_opts, [warn_missing_spec, warn_missing_spec_all]},
        {relx, [{dev_mode, false}, {include_erts, true}]}
    ]},

    {test, [
        {erl_opts, [debug_info]},
        {deps, [{proper, "1.4.0"}]}
    ]}
]}.

{ct_opts, [
    {sys_config, ["test/sys.config"]},
    {config, ["test/test.config"]}
]}.

{relx, [
    {release, {my_jobs_lib, "1.0.0"}, [
        kernel,
        stdlib,
        sasl,
        lager,
        gnat,
        prometheus,
        my_jobs_lib
    ]},
    {sys_config, "config/sys.config"},
    {vm_args, "config/vm.args"}
]}.

{plugins, [
    rebar3_lint,
    rebar3_format,
    rebar3_proper
]}.
```

---

### 6.3 Application Resource File (.app.src)

**Example from tps-kanban:**

```erlang
{application, tps_kanban,
 [
  {description, "Pull-based Kanban work queue system with NATS + RabbitMQ"},
  {vsn, "1.0.0"},
  {registered, [tps_kanban_sup]},
  {applications,
   [kernel,
    stdlib,
    sasl,
    lager,
    gnat,
    amqp_client,
    prometheus
   ]},
  {mod, {tps_kanban_app, []}},
  {env, [
      {nats_servers, ["nats://localhost:4222"]},
      {rabbitmq_host, "localhost"},
      {rabbitmq_port, 5672},
      {rabbitmq_user, "guest"},
      {rabbitmq_pass, "guest"},
      {worker_pull_size, 10},
      {worker_process_timeout_ms, 30000}
  ]}
 ]}.
```

---

### 6.4 Release Packaging (relx)

**Build release:**

```bash
# Build development release
rebar3 as dev release

# Build production release
rebar3 as prod release

# Build tarball for deployment
rebar3 as prod tar
```

**Run release:**

```bash
# Start in foreground
_build/prod/rel/my_jobs_lib/bin/my_jobs_lib foreground

# Start as daemon
_build/prod/rel/my_jobs_lib/bin/my_jobs_lib start

# Attach to running console
_build/prod/rel/my_jobs_lib/bin/my_jobs_lib remote_console

# Stop daemon
_build/prod/rel/my_jobs_lib/bin/my_jobs_lib stop
```

---

## 7. Documentation (edoc, ex_doc)

### 7.1 edoc Documentation

**Pattern:** Use edoc for inline documentation.

**Example:**

```erlang
%%%-------------------------------------------------------------------
%% @doc Job Worker - Processes jobs from the queue
%%
%% The worker pulls jobs from the queue, processes them, and reports
%% results. If a job fails, it can be retried based on retry policy.
%%
%% @end
%%%-------------------------------------------------------------------
-module(job_worker).

%% @doc Start a new worker linked to the calling process.
%% Returns `{ok, Pid}' on success or `{error, Reason}' on failure.
%%
%% Example:
%% ```
%% {ok, Worker} = job_worker:start_link(high, payment).
%% '''
-spec start_link(atom(), atom()) -> {ok, pid()} | {error, term()}.
start_link(Priority, Domain) ->
    gen_server:start_link(?MODULE, {Priority, Domain}, []).
```

**Generate docs:**

```bash
rebar3 edoc
```

Output: `doc/index.html`

---

### 7.2 ex_doc for Elixir-Style Docs

**rebar.config:**

```erlang
{plugins, [rebar3_ex_doc]}.

{ex_doc, [
    {extras, ["README.md", "CHANGELOG.md"]},
    {main, "README.md"},
    {source_url, "https://github.com/user/my_jobs_lib"}
]}.
```

**Generate:**

```bash
rebar3 ex_doc
```

---

## 8. Production Deployment Checklist

### 8.1 Configuration Management

**sys.config example:**

```erlang
[
    {my_jobs_lib, [
        {nats_servers, ["nats://prod1:4222", "nats://prod2:4222"]},
        {worker_pool_size, 50},
        {max_retries, 3},
        {circuit_breaker_threshold, 5}
    ]},

    {lager, [
        {handlers, [
            {lager_file_backend, [{file, "log/info.log"}, {level, info}]},
            {lager_file_backend, [{file, "log/error.log"}, {level, error}]}
        ]},
        {crash_log, "log/crash.log"}
    ]},

    {sasl, [
        {sasl_error_logger, {file, "log/sasl.log"}}
    ]}
].
```

---

### 8.2 VM Arguments (vm.args)

```bash
## Name of the node
-name my_jobs_lib@prod1

## Cookie for distributed Erlang
-setcookie my_secret_cookie

## Heartbeat monitoring
-heart

## Kernel poll (performance)
+K true

## Async thread pool size
+A 64

## Max processes
+P 1000000

## Max ports
+Q 65536

## Scheduler bind type
+sbt db

## Disable legacy atoms table
+MIscs 256
```

---

### 8.3 Monitoring and Observability

**Prometheus Integration:**

```erlang
%% @doc Initialize Prometheus metrics
init_metrics() ->
    prometheus_counter:new([
        {name, jobs_processed_total},
        {help, "Total jobs processed"}
    ]),

    prometheus_counter:new([
        {name, jobs_failed_total},
        {help, "Total jobs failed"}
    ]),

    prometheus_histogram:new([
        {name, job_processing_duration_seconds},
        {help, "Job processing duration"},
        {buckets, [0.01, 0.05, 0.1, 0.5, 1, 5, 10]}
    ]),

    prometheus_gauge:new([
        {name, worker_pool_utilization},
        {help, "Worker pool utilization (0-1)"}
    ]).

%% @doc Record job processing
record_job_processing(Duration, Success) ->
    prometheus_histogram:observe(job_processing_duration_seconds, Duration),
    case Success of
        true -> prometheus_counter:inc(jobs_processed_total);
        false -> prometheus_counter:inc(jobs_failed_total)
    end.
```

---

## 9. Industry Best Practices Summary

### 9.1 Popular Erlang Jobs Libraries

Based on web search, these are the established libraries in the ecosystem:

1. **sidejob** (Basho)
   - Parallel, capacity-limited request pool
   - Capacity split across workers
   - Fail-fast when pool exhausted
   - [GitHub: basho/sidejob](https://github.com/basho/sidejob)

2. **worker_pool** (Inaka)
   - Configurable worker pools
   - Used in MongooseIM, sumo_db
   - Production-proven at scale
   - [GitHub: inaka/worker_pool](https://github.com/inaka/worker_pool)

3. **gruff**
   - Petri net-based worker pool
   - Formal verification possible
   - [GitHub: joergen7/gruff](https://github.com/joergen7/gruff)

4. **simplepool** (Brigadier)
   - Simple gen_server pool
   - Minimal dependencies
   - [GitHub: brigadier/simplepool](https://github.com/brigadier/simplepool)

---

### 9.2 Recommended Architecture

Based on research, the recommended architecture is:

```
┌─────────────────────────────────────────────────────────────┐
│                   Top-Level Supervisor                      │
│                  (one_for_one strategy)                     │
└───────────────────┬─────────────────────────────────────────┘
                    │
        ┌───────────┼───────────┬───────────────┐
        │           │           │               │
        v           v           v               v
    ┌───────┐  ┌────────┐  ┌─────────┐  ┌──────────────┐
    │ Queue │  │ Worker │  │Scheduler│  │Circuit Breaker│
    │       │  │  Pool  │  │         │  │               │
    │(gen_  │  │ (pool  │  │ (gen_   │  │  (gen_statem) │
    │server)│  │  boy)  │  │ server) │  │               │
    └───────┘  └────────┘  └─────────┘  └──────────────┘
        │           │
        │           v
        │      ┌─────────────┐
        │      │Worker Pool  │
        │      │  Supervisor │
        │      │(simple_one_ │
        │      │ for_one)    │
        │      └──────┬──────┘
        │             │
        │         ┌───┴───┬───────┐
        │         v       v       v
        │     ┌──────┐ ┌────┐ ┌────┐
        │     │Worker│ │Worker│ │...│
        │     │      │ │      │ │    │
        └────>│(gen_ │ │(gen_ │ │    │
              │server)│ │server)│ │    │
              └──────┘ └──────┘ └────┘
```

**Key Components:**
1. **Queue:** gen_server managing NATS/RabbitMQ subscriptions
2. **Worker Pool:** Poolboy managing fixed-size worker pool
3. **Workers:** gen_server pulling and processing jobs
4. **Scheduler:** gen_server for periodic/delayed jobs
5. **Circuit Breaker:** gen_statem for fail-fast protection

---

### 9.3 Performance Targets

**Latency:**
- p50: < 10ms
- p95: < 50ms
- p99: < 100ms

**Throughput:**
- 10,000+ jobs/second per node
- Linear scaling with worker count

**Availability:**
- 99.9% uptime (supervisor restarts)
- Graceful degradation (circuit breakers)
- Zero message loss (RabbitMQ fallback)

---

## 10. Sources and References

This research synthesized information from:

### Codebase Analysis:
- `/home/user/ggen/tps-kanban/` - Pull-based Kanban job queue
- `/home/user/ggen/crates/tps-jidoka/` - Worker pools, rate limiting, circuit breakers
- `/home/user/ggen/erlang/tps-heijunka/` - Dynamic worker pool scaling
- `/home/user/ggen/examples/gcp-erlang-autonomics/` - Benchmarking patterns

### Web Resources:
- [Learn You Some Erlang: Building Applications With OTP](https://learnyousomeerlang.com/building-applications-with-otp)
- [Learn You Some Erlang: Building OTP Applications](https://learnyousomeerlang.com/building-otp-applications)
- [Erlang System Documentation: Design Principles](https://www.erlang.org/doc/design_principles/des_princ)
- [GitHub: basho/sidejob](https://github.com/basho/sidejob)
- [GitHub: inaka/worker_pool](https://github.com/inaka/worker_pool)
- [GitHub: joergen7/gruff](https://github.com/joergen7/gruff)
- [GitHub: brigadier/simplepool](https://github.com/brigadier/simplepool)
- [O'Reilly: Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/ch09.html)

---

## Conclusion

This comprehensive research provides all necessary patterns, code examples, and architectural guidance for building a production-ready jobs library in Erlang/OTP. The patterns identified in the existing codebase (tps-kanban, tps-jidoka, tps-heijunka) align with industry best practices and provide battle-tested implementations.

**Next Steps:**
1. Choose architecture pattern (pull-based Kanban recommended)
2. Implement core components (queue, workers, supervisor tree)
3. Add production features (circuit breakers, rate limiting, metrics)
4. Test comprehensively (Chicago TDD, stress testing, benchmarking)
5. Deploy with proper monitoring and observability

**Contact:** Research Agent
**Date:** 2026-01-29
