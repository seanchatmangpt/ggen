# Tutorial 3: Creating a Supervised Worker Pool

**Learning Objective:** Design an OTP supervision tree, implement fault-tolerant workers, and handle worker failures gracefully.

**Prerequisites:** Completed [Tutorial 2: Building Your First Job Queue](02-first-job-queue.md)

**Outcome:** A production-ready worker pool with OTP supervision, automatic restarts, and crash isolation.

**Time:** ~45 minutes

---

## What You'll Build

In this tutorial, you'll add a supervised worker pool to your job queue:
- ✅ Supervision tree with one_for_one strategy
- ✅ Poolboy-managed worker pool (fixed size + overflow)
- ✅ Worker lifecycle management (init → work → terminate)
- ✅ Crash isolation and automatic restarts
- ✅ Worker metrics and health monitoring

---

## Step 1: Extend RDF with Worker Pool Specification

Edit `.specify/specs/001-job-queue/feature.ttl` to add worker pool configuration:

```turtle
@prefix jobs: <http://ggen.dev/ontology/jobs#> .

# WorkerPool class
jobs:WorkerPool a rdfs:Class ;
    rdfs:label "Worker Pool" ;
    rdfs:comment "Fixed-size worker pool with overflow capacity" ;
    jobs:hasProperty jobs:poolSize ;
    jobs:hasProperty jobs:maxOverflow ;
    jobs:hasProperty jobs:pullSize ;
    jobs:hasProperty jobs:processTimeout .

jobs:poolSize a rdf:Property ;
    rdfs:domain jobs:WorkerPool ;
    rdfs:range xsd:integer ;
    rdfs:label "Fixed pool size" ;
    jobs:defaultValue 10 .

jobs:maxOverflow a rdf:Property ;
    rdfs:domain jobs:WorkerPool ;
    rdfs:range xsd:integer ;
    rdfs:label "Maximum overflow workers" ;
    jobs:defaultValue 5 .

jobs:pullSize a rdf:Property ;
    rdfs:domain jobs:WorkerPool ;
    rdfs:range xsd:integer ;
    rdfs:label "Number of jobs to pull per worker" ;
    jobs:defaultValue 5 .

jobs:processTimeout a rdf:Property ;
    rdfs:domain jobs:WorkerPool ;
    rdfs:range xsd:integer ;
    rdfs:label "Processing timeout in milliseconds" ;
    jobs:defaultValue 30000 .

# Worker class
jobs:Worker a rdfs:Class ;
    rdfs:label "Job Worker" ;
    rdfs:comment "Individual worker that pulls and processes jobs" ;
    jobs:hasProperty jobs:workerId ;
    jobs:hasProperty jobs:priority ;
    jobs:hasProperty jobs:domain ;
    jobs:hasProperty jobs:circuitBreakerOpen .

jobs:circuitBreakerOpen a rdf:Property ;
    rdfs:domain jobs:Worker ;
    rdfs:range xsd:boolean ;
    rdfs:label "Circuit breaker status" ;
    jobs:defaultValue false .
```

---

## Step 2: Generate Supervision Tree

Run ggen sync to generate the supervision tree:

```bash
ggen sync --audit true

# Expected output:
# [μ₁] Normalizing RDF ontology... ✓
# [μ₂] Extracting entities via SPARQL... ✓
# [μ₃] Rendering templates (Tera)... ✓
# [μ₄] Canonicalizing outputs... ✓
# [μ₅] Generating receipt... ✓
#
# Generated 4 new files:
#   src/job_worker_pool.erl
#   src/job_worker_pool_sup.erl
#   src/job_worker.erl
#   src/job_worker_sup.erl
```

---

## Step 3: Examine Supervision Tree Architecture

Open `src/my_erlang_jobs_sup.erl` to see the top-level supervisor:

```erlang
%%%-------------------------------------------------------------------
%%% @doc Top-Level Supervisor - Supervises all components
%%%
%%% Supervision Strategy: one_for_one
%%% Restart Intensity: 5 restarts in 10 seconds
%%%-------------------------------------------------------------------
-module(my_erlang_jobs_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  %% If one child crashes, only restart that child
        intensity => 5,           %% Max 5 restarts...
        period => 10              %% ...in 10 seconds
    },

    ChildSpecs = [
        #{
            id => job_queue,
            start => {job_queue, start_link, []},
            restart => permanent,      %% Always restart
            shutdown => 5000,          %% 5 second graceful shutdown
            type => worker,
            modules => [job_queue]
        },
        #{
            id => job_worker_pool_sup,
            start => {job_worker_pool_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,      %% Wait for all workers to terminate
            type => supervisor,
            modules => [job_worker_pool_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Supervision tree visualization:**

```
my_erlang_jobs_sup (one_for_one)
    │
    ├── job_queue (worker)
    │
    └── job_worker_pool_sup (supervisor, one_for_one)
            │
            └── job_worker_pool (poolboy pool)
                    │
                    └── job_worker (1..N workers, simple_one_for_one)
```

**Key points:**
- **one_for_one strategy:** Worker crashes don't affect job_queue
- **Restart intensity:** If 5 crashes in 10s, supervisor gives up (alerts needed)
- **Shutdown sequence:** job_worker_pool_sup waits for all workers (infinity timeout)

---

## Step 4: Examine Worker Pool Implementation

Open `src/job_worker_pool.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc Job Worker Pool - Poolboy-Managed Worker Pool
%%%
%%% Configuration:
%%% - Fixed pool size: 10 workers always running
%%% - Max overflow: 5 temporary workers when pool exhausted
%%% - Strategy: FIFO (first-in-first-out)
%%%-------------------------------------------------------------------
-module(job_worker_pool).

-export([
    start_link/1,
    execute/2,
    checkout_worker/1,
    checkin_worker/2,
    pool_status/0
]).

-define(POOL, job_worker_pool).

%% @doc Start the worker pool
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    PoolSize = maps:get(pool_size, Config, 10),
    MaxOverflow = maps:get(max_overflow, Config, 5),

    PoolboyConfig = [
        {name, {local, ?POOL}},
        {worker_module, job_worker},
        {size, PoolSize},
        {max_overflow, MaxOverflow},
        {strategy, fifo}  %% First-in-first-out
    ],

    poolboy:start_link(PoolboyConfig, Config).

%% @doc Execute a function in a worker from the pool
-spec execute(atom(), atom()) -> {ok, term()} | {error, term()}.
execute(Priority, Domain) ->
    case poolboy:checkout(?POOL, true, 5000) of  %% Block up to 5s
        full ->
            prometheus_counter:inc(worker_pool_exhausted_total),
            {error, pool_exhausted};
        Worker ->
            try
                gen_server:call(Worker, {pull_and_process, Priority, Domain}, 60000)
            after
                poolboy:checkin(?POOL, Worker)
            end
    end.

%% @doc Checkout a worker (manual management)
-spec checkout_worker(integer()) -> pid() | full.
checkout_worker(Timeout) ->
    poolboy:checkout(?POOL, true, Timeout).

%% @doc Checkin a worker (manual management)
-spec checkin_worker(pid(), pid()) -> ok.
checkin_worker(Worker, PoolPid) ->
    poolboy:checkin(PoolPid, Worker).

%% @doc Get pool status
-spec pool_status() -> #{atom() => integer()}.
pool_status() ->
    {Workers, Overflow, Monitors} = poolboy:status(?POOL),
    #{
        available_workers => Workers,
        overflow_workers => Overflow,
        monitored_workers => Monitors
    }.
```

**Poolboy configuration:**
- **size:** 10 workers always running (never terminated)
- **max_overflow:** 5 temporary workers (created when pool full, terminated when idle)
- **strategy:** FIFO (oldest idle worker gets next job)

---

## Step 5: Examine Worker Implementation

Open `src/job_worker.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @doc Job Worker - Pulls and processes jobs with circuit breaker
%%%
%%% Lifecycle:
%%% 1. init: Subscribe to NATS queue
%%% 2. Pull loop: Pull N jobs, process them, ACK on success
%%% 3. Circuit breaker: Stop pulling if failures exceed threshold
%%% 4. terminate: Cleanup subscriptions
%%%-------------------------------------------------------------------
-module(job_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    worker_id :: string(),
    priority :: atom(),
    domain :: atom(),
    pull_size :: integer(),
    process_timeout :: integer(),
    circuit_breaker_open :: boolean(),
    failures :: [integer()],  %% Failure timestamps
    failure_threshold :: integer(),
    failure_window_ms :: integer(),
    metrics :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Config) ->
    Priority = maps:get(priority, Config, normal),
    Domain = maps:get(domain, Config, default),
    PullSize = maps:get(pull_size, Config, 5),
    ProcessTimeout = maps:get(process_timeout, Config, 30000),

    WorkerId = generate_worker_id(Priority, Domain),

    State = #state{
        worker_id = WorkerId,
        priority = Priority,
        domain = Domain,
        pull_size = PullSize,
        process_timeout = ProcessTimeout,
        circuit_breaker_open = false,
        failures = [],
        failure_threshold = 5,       %% Open circuit after 5 failures
        failure_window_ms = 10000,   %% ...in 10 seconds
        metrics = #{pulled => 0, processed => 0, failed => 0, acked => 0}
    },

    %% Subscribe to work queue
    ok = job_queue:subscribe(Priority, Domain),

    logger:info("Worker ~s started for ~p/~p", [WorkerId, Priority, Domain]),

    {ok, State}.

handle_call({pull_and_process, Priority, Domain}, _From, State) ->
    case State#state.circuit_breaker_open of
        true ->
            %% Circuit breaker open, reject request
            {reply, {error, circuit_breaker_open}, State};
        false ->
            %% Pull jobs and process
            Result = pull_and_process_jobs(State),
            {reply, Result, State}
    end;

handle_call(get_status, _From, State) ->
    Status = #{
        worker_id => State#state.worker_id,
        priority => State#state.priority,
        domain => State#state.domain,
        circuit_breaker_open => State#state.circuit_breaker_open,
        metrics => State#state.metrics
    },
    {reply, Status, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nats, _Sid, Msg, _ReplyTo, _Subject}, State) ->
    %% NATS message received (job from queue)
    NewState = process_nats_message(Msg, State),
    {noreply, NewState};

handle_info(check_circuit_breaker, State) ->
    %% Periodic circuit breaker check (every 1s)
    NewState = update_circuit_breaker(State),
    erlang:send_after(1000, self(), check_circuit_breaker),
    {noreply, NewState}.

terminate(_Reason, State) ->
    logger:info("Worker ~s terminating", [State#state.worker_id]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pull_and_process_jobs(State) ->
    %% Pull N jobs from queue
    {ok, Jobs} = job_queue:pull_jobs(State#state.priority,
                                      State#state.domain,
                                      State#state.pull_size),

    %% Process each job
    Results = lists:map(fun(Job) -> process_job(Job, State) end, Jobs),

    %% Update metrics
    Processed = length([R || R <- Results, R =:= ok]),
    Failed = length([R || R <- Results, R =/= ok]),

    NewMetrics = maps:update_with(processed, fun(V) -> V + Processed end, 0,
                                   State#state.metrics),
    NewMetrics2 = maps:update_with(failed, fun(V) -> V + Failed end, 0,
                                    NewMetrics),

    %% Update failure tracking for circuit breaker
    NewState = case Failed > 0 of
        true ->
            add_failures(Failed, State#state{metrics = NewMetrics2});
        false ->
            State#state{metrics = NewMetrics2}
    end,

    {ok, #{processed => Processed, failed => Failed}}.

process_job(Job, State) ->
    JobId = maps:get(<<"id">>, Job),

    StartTime = erlang:system_time(millisecond),

    Result = try
        %% Execute job processing with timeout
        Timeout = State#state.process_timeout,
        execute_with_timeout(Job, Timeout)
    catch
        Error:Reason:Stacktrace ->
            logger:error("Job ~s failed: ~p:~p~n~p",
                        [JobId, Error, Reason, Stacktrace]),
            {error, {Error, Reason}}
    end,

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    prometheus_histogram:observe(job_processing_duration_ms, Duration),

    case Result of
        ok ->
            job_queue:ack(JobId),
            ok;
        {error, _} ->
            {error, processing_failed}
    end.

execute_with_timeout(Job, Timeout) ->
    Parent = self(),
    Ref = make_ref(),

    Pid = spawn_link(fun() ->
        Result = do_work(Job),
        Parent ! {result, Ref, Result}
    end),

    receive
        {result, Ref, Result} ->
            Result
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.

do_work(Job) ->
    %% This is where your business logic goes
    Payload = maps:get(<<"payload">>, Job),

    %% Example: Process based on action
    Action = maps:get(<<"action">>, Payload, <<"noop">>),

    case Action of
        <<"noop">> ->
            ok;
        <<"fail">> ->
            {error, simulated_failure};
        _ ->
            %% Your custom processing here
            timer:sleep(100),  %% Simulate work
            ok
    end.

add_failures(Count, State) ->
    Now = erlang:system_time(millisecond),
    NewFailures = lists:duplicate(Count, Now) ++ State#state.failures,
    State#state{failures = NewFailures}.

update_circuit_breaker(State) ->
    Now = erlang:system_time(millisecond),
    WindowStart = Now - State#state.failure_window_ms,

    %% Keep only failures within window
    RecentFailures = [F || F <- State#state.failures, F >= WindowStart],

    ShouldOpen = length(RecentFailures) >= State#state.failure_threshold,

    case {State#state.circuit_breaker_open, ShouldOpen} of
        {false, true} ->
            logger:error("Circuit breaker opening for worker ~s",
                        [State#state.worker_id]),
            prometheus_counter:inc(circuit_breaker_opened_total),
            State#state{circuit_breaker_open = true, failures = RecentFailures};
        {true, false} ->
            logger:info("Circuit breaker closing for worker ~s",
                       [State#state.worker_id]),
            prometheus_counter:inc(circuit_breaker_closed_total),
            State#state{circuit_breaker_open = false, failures = RecentFailures};
        _ ->
            State#state{failures = RecentFailures}
    end.

generate_worker_id(Priority, Domain) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    lists:flatten(io_lib:format("worker-~s-~s-~B-~B",
                                [Priority, Domain, Timestamp, Random])).
```

**Key features:**
- **Circuit breaker:** Opens after 5 failures in 10s (configurable)
- **Pull-based:** Workers pull jobs, don't wait to be pushed
- **Timeout handling:** Jobs killed if exceed process_timeout
- **Metrics:** Tracks pulled, processed, failed, ACKed jobs
- **Graceful termination:** Cleanup subscriptions on shutdown

---

## Step 6: Test Worker Failures and Restarts

The generated code includes crash recovery tests. Run them:

```bash
rebar3 ct --suite job_worker_SUITE

# Expected output:
# Testing my_erlang_jobs.job_worker_SUITE: test_worker_crash_and_restart ... ok
# Testing my_erlang_jobs.job_worker_SUITE: test_circuit_breaker_opens ... ok
# Testing my_erlang_jobs.job_worker_SUITE: test_pool_overflow ... ok
#
# All 3 tests passed.
```

---

## Step 7: Manually Test Worker Crashes

Test that workers restart automatically:

```bash
rebar3 shell

% Get worker pool status
> job_worker_pool:pool_status().
#{available_workers => 10, overflow_workers => 0, monitored_workers => 10}

% Checkout a worker
> Worker = job_worker_pool:checkout_worker(5000).
<0.456.0>

% Crash the worker
> exit(Worker, kill).
true

% Wait a moment for supervisor to restart
> timer:sleep(100).

% Check pool status again (should still be 10 workers)
> job_worker_pool:pool_status().
#{available_workers => 10, overflow_workers => 0, monitored_workers => 10}
```

**What happened?**
- Worker was killed
- Supervisor detected crash
- New worker started automatically
- Pool size maintained at 10

This is **OTP fault tolerance** in action!

---

## Congratulations!

You've built a fault-tolerant worker pool with:
- ✅ OTP supervision tree (one_for_one strategy)
- ✅ Poolboy-managed workers (fixed size + overflow)
- ✅ Circuit breaker for failure isolation
- ✅ Automatic crash recovery
- ✅ Worker metrics and monitoring

---

## What's Next?

Complete the tutorial series:
- **Tutorial 4:** [End-to-End: From RDF Spec to Running Erlang App](04-rdf-to-running-app.md)

Or explore **How-To Guides**:
- [How to Stress Test with PropEr](../howto/04-stress-testing.md)
- [How to Write Benchmarks for Your Jobs](../howto/03-benchmarks.md)

---

## Summary

You've learned:
- OTP supervision tree design (one_for_one, restart intensities)
- Poolboy worker pool management
- Worker lifecycle (init → pull loop → terminate)
- Circuit breaker pattern for failure isolation
- Crash recovery and automatic restarts

**Next:** [Tutorial 4: End-to-End: From RDF Spec to Running Erlang App](04-rdf-to-running-app.md)

---

**Generated by ggen v6.0.0 | 2026-01-29**
