# Jidoka: Autonomation with a Human Touch

## Table of Contents
1. [Core Concept](#core-concept)
2. [Jidoka vs. Automation](#jidoka-vs-automation)
3. [Jidoka in Manufacturing](#jidoka-in-manufacturing)
4. [Jidoka in Software Systems](#jidoka-in-software-systems)
5. [Architecture](#architecture)
6. [Component Details](#component-details)
7. [Implementation Guide](#implementation-guide)
8. [Testing & Verification](#testing--verification)
9. [Operational Runbook](#operational-runbook)
10. [Troubleshooting](#troubleshooting)
11. [Metrics & SLOs](#metrics--slos)
12. [References](#references)

---

## Core Concept

### Definition

**Jidoka** (自働化) is a Japanese manufacturing principle meaning "autonomation" or "automation with a human touch."

Core principle: **Stop the line when a defect is detected.**

In manufacturing, if a defect is found:
- The production line STOPS immediately
- Human workers investigate the root cause
- The process is fixed before resuming production

This prevents:
- Defective products from propagating through the system
- Amplification of errors (making bad products from bad inputs)
- Waste from accumulating (fixing 1000 bad items costs more than stopping at 1)

### Key Characteristics

1. **Fail-Fast**: Detect errors immediately, stop processing
2. **Don't Amplify**: Don't retry failing operations, don't queue unbounded requests
3. **Observable**: Every state transition is logged, visible to operators
4. **Human-Centric**: System informs humans to take action, doesn't hide problems
5. **Prevent Defects**: Better to stop than to detect-and-fix later (DfLSS alignment)

### Jidoka ≠ Automation

| Aspect | Automation | Jidoka |
|--------|-----------|---------|
| **Goal** | Reduce labor costs | Maintain quality |
| **Error response** | Retry/queue indefinitely | Stop and alert |
| **Visibility** | Implicit (hidden bugs) | Explicit (all errors logged) |
| **Focus** | Speed | Quality |
| **Human role** | Elimination | Partnership |

---

## Jidoka in Manufacturing

### Andon Cord System

In Toyota plants, any worker can pull an Andon cord to:
1. **Stop the line** - Immediate halt of production
2. **Signal defect** - Alert team leads of the problem
3. **Investigate** - Root cause analysis while line is stopped
4. **Fix root cause** - Prevent recurrence, not just current instance
5. **Resume** - Production restarts only after fix is verified

**Key outcome**: Defects are caught at source, not amplified downstream.

### Real Example: Defect Detection

```
Station 1: Component assembly
  ↓ (product passes quality check)
Station 2: Subsystem integration
  → DEFECT DETECTED: Wrong component orientation
  → Andon cord pulled
  → Line STOPS (all 10 stations stop)
  → Investigation: Why did wrong component pass quality check at Station 1?
  → Fix: Recalibrate Station 1 sensor
  → Verification: Test 10 units
  → Resume: Production restarts
```

**Result**: 1 unit caught, not 10,000 units made with wrong orientation.

---

## Jidoka in Software Systems

### Principle Mapping

| Manufacturing | Software | Implementation |
|---------------|----------|-----------------|
| Stop the line | Fail-fast | Circuit breaker opens, stops calling failing service |
| Andon cord | Logging | Every state transition logged with context |
| Quality gate | Type system | Compiler prevents invalid states |
| Defect buffer | Rate limiter | Refuse work when overloaded, don't queue |
| Isolation | Supervisor trees | Worker crash doesn't kill entire system |

### Defect Propagation Without Jidoka

```
Request 1: Fails → Timeout (1s)
Request 2: Fails → Timeout (1s)
...
Request 100: Fails → Timeout (1s)
Request 101: Fails → Timeout (1s)

Total damage: 101 second timeout × 100 = 101,000ms = 101 seconds wasted
Memory: 100 requests queued = 100MB wasted
User impact: Users wait 100+ seconds for failures
```

### Defect Handling WITH Jidoka

```
Request 1: Fails → circuit_breaker:record_failure()
Request 2: Fails → circuit_breaker:record_failure()
Request 3: Fails → circuit_breaker:record_failure()
Request 4: Fails → circuit_breaker:record_failure()
Request 5: Fails → circuit_breaker:record_failure()
         → THRESHOLD EXCEEDED → Circuit breaker OPENS
Request 6: Rejected immediately → {error, circuit_open} (1ms)
Request 7: Rejected immediately → {error, circuit_open} (1ms)
...
Request 101: Rejected immediately → {error, circuit_open} (1ms)

Total damage: 5 × timeout (5s) + 96 × 1ms rejection = 5s + 96ms = 5.1s
Memory: 0 requests queued
User impact: Users get error immediately after 5s
```

**Outcome**: 99% less resource waste, 20x faster error feedback.

---

## Architecture

### Supervision Tree

```
jidoka_supervisor (top-level, one_for_one strategy)
├── jidoka_circuit_breaker (gen_statem, manages failure state)
├── jidoka_rate_limiter (gen_server, token bucket algorithm)
└── jidoka_worker_pool (poolboy, fixed-size request handlers)
    └── worker_1, worker_2, ..., worker_N (gen_server instances)
```

### Design Principles

1. **One_For_One Strategy**: If worker crashes, only restart that worker
   - Prevents cascading failures
   - Isolates problems to single request
   - Other workers continue processing

2. **No Unbounded Queues**: Pool is fixed size
   - Pool full → Reject with {error, pool_exhausted}
   - Prevents memory explosion
   - Forces upstream to handle backpressure

3. **Circuit Breaker State Machine**:
   ```
   CLOSED (normal) ──(threshold exceeded)──> OPEN (failing)
      ^                                          |
      └─────(timeout)─> HALF_OPEN ─────────────┘
                           ↑ (success on test call)
                           └──> CLOSED
   ```

4. **Rate Limiting**: Token bucket refill
   - Limits throughput to configured rate
   - Burst capacity for spikes
   - Rejects when bucket empty (fail-fast)

---

## Component Details

### Circuit Breaker

**Purpose**: Detect service failures and stop calling failing service.

**States**:
- **CLOSED**: Normal operation, all calls forwarded
- **OPEN**: Service failing, all calls rejected immediately
- **HALF_OPEN**: Testing recovery, allow one test call

**Configuration**:
```erlang
jidoka_circuit_breaker:start_link(#{
    threshold => 5,           % Failures to trigger open
    window_ms => 10000,       % Time window for counting
    recovery_ms => 30000      % Time before half_open attempt
})
```

**Metrics**:
- `get_state/1` → closed | open | half_open
- `status/0` → {State, FailureCount, NextRecoveryTime}

**Usage**:
```erlang
%% Call through circuit breaker
Result = jidoka_circuit_breaker:call(fun() -> service:call() end),

%% Handle results
case Result of
    ok -> % Success
        jidoka_circuit_breaker:record_success();
    {error, circuit_open} ->
        % Service is failing, fail-fast
        logger:warning("Service unreachable, circuit open"),
        {error, service_unavailable};
    {error, Error} ->
        % Call failed
        jidoka_circuit_breaker:record_failure(),
        {error, Error}
end
```

### Rate Limiter

**Purpose**: Prevent system overload by limiting request rate.

**Algorithm**: Token bucket
- Tokens refill at fixed rate (rate_limit/second)
- Each request consumes 1+ tokens
- No tokens → Request rejected (fail-fast)

**Configuration**:
```erlang
jidoka_rate_limiter:start_link(#{
    rate_limit => 1000,    % Requests per second
    burst_size => 500      % Max tokens in bucket
})
```

**Usage**:
```erlang
case jidoka_rate_limiter:acquire(1) of
    ok ->
        % Process request
        process_request();
    {error, rate_limit_exceeded} ->
        % System overloaded, fail-fast
        logger:warning("Rate limit exceeded, rejecting request"),
        {error, overloaded}
end
```

**Metrics**:
- `status/0` → {CurrentTokens, Accepted, Rejected}

### Worker Pool

**Purpose**: Fixed-size pool for request processing.

**Configuration**:
```erlang
jidoka_worker_pool:start_link(#{
    pool_name => jidoka_worker_pool,
    pool_size => 10,       % Number of workers
    max_overflow => 0      % NO overflow (Jidoka principle)
})
```

**Usage**:
```erlang
case jidoka_worker_pool:execute(fun() -> do_work() end) of
    ok ->
        % Work completed
        ok;
    {error, pool_exhausted} ->
        % Pool full, no workers available
        logger:warning("Worker pool exhausted"),
        {error, system_busy}
end
```

**Health Checks**:
```erlang
case jidoka_worker_pool:health_check() of
    ok -> logger:info("Workers healthy");
    {error, Reason} -> logger:error("Worker pool unhealthy: ~p", [Reason])
end
```

---

## Implementation Guide

### Step 1: Start the Jidoka System

```erlang
%% Start with default configuration
{ok, SupervisorPid} = jidoka_supervisor:start_link().

%% Or with custom configuration
{ok, SupervisorPid} = jidoka_supervisor:start_link(#{
    pool_size => 20,
    circuit_threshold => 5,
    window_ms => 10000,
    rate_limit => 5000
}).
```

### Step 2: Integrate into Your Application

**Option A**: Add to your supervisor tree

```erlang
%% In your_app_sup.erl
init(Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{
            id => jidoka,
            start => {jidoka_supervisor, start_link, [#{pool_size => 10}]},
            restart => permanent,
            shutdown => 10000,
            type => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

**Option B**: Start manually in your app startup

```erlang
%% In your app's start/2 callback
start(normal, []) ->
    {ok, _} = jidoka_supervisor:start_link(#{pool_size => 10}),
    your_app_sup:start_link().
```

### Step 3: Implement Request Handling

```erlang
handle_request(Request) ->
    %% Step 3a: Check rate limit first (fail-fast on overload)
    case jidoka_rate_limiter:acquire(1) of
        {error, rate_limit_exceeded} ->
            logger:warning("Rate limit exceeded: ~p", [Request]),
            {error, too_many_requests};
        ok ->
            %% Step 3b: Execute in worker pool
            case jidoka_worker_pool:execute(fun() -> process_request(Request) end) of
                {error, pool_exhausted} ->
                    logger:warning("Worker pool exhausted: ~p", [Request]),
                    {error, service_unavailable};
                {error, Error} ->
                    logger:error("Request failed: ~p, error: ~p", [Request, Error]),
                    {error, Error};
                Result ->
                    logger:info("Request succeeded: ~p", [Request]),
                    Result
            end
    end.

process_request(Request) ->
    %% Call external service through circuit breaker
    case jidoka_circuit_breaker:call(fun() -> external_service:call(Request) end) of
        {error, circuit_open} ->
            logger:warning("External service failing, circuit open: ~p", [Request]),
            {error, service_unavailable};
        {error, Error} ->
            jidoka_circuit_breaker:record_failure(),
            logger:error("External service error: ~p", [Error]),
            {error, Error};
        Result ->
            jidoka_circuit_breaker:record_success(),
            Result
    end.
```

### Step 4: Monitor Status

```erlang
%% Get overall system status
Status = jidoka_supervisor:get_status(),
io:format("Jidoka Status: ~p~n", [Status]).
%% Output: #{
%%   supervisor_pid => <0.42.0>,
%%   circuit_breaker_state => closed,
%%   active_workers => 3,
%%   timestamp => 1642432000000
%% }

%% Get pool utilization
PoolStatus = jidoka_supervisor:get_pool_status(),
io:format("Pool Status: ~p~n", [PoolStatus]).
%% Output: #{
%%   pool_size => 10,
%%   available_workers => 7,
%%   in_use => 3,
%%   utilization_percent => 30,
%%   timestamp => 1642432000000
%% }
```

### Step 5: Operational Recovery

When circuit breaker opens (indicates service failure):

1. **Investigate**: Check logs to understand root cause
2. **Fix**: Address the underlying issue
3. **Verify**: Test the service is working
4. **Reset**: Tell Jidoka to try again

```erlang
%% After fixing the external service
ok = jidoka_supervisor:reset_circuit_breaker(),
logger:info("Circuit breaker reset, resuming normal operation").
```

---

## Testing & Verification

### Unit Test Example

```erlang
%% Test: Circuit breaker opens after threshold failures
test_circuit_breaker_opens(Config) ->
    %% Arrange
    CB = whereis(jidoka_circuit_breaker),
    Threshold = 5,
    FailingFun = fun() -> error(simulated_failure) end,

    %% Act: Trigger threshold failures
    [jidoka_circuit_breaker:call(FailingFun) || _ <- lists:seq(1, Threshold + 1)],

    %% Assert: Circuit should be open
    State = jidoka_circuit_breaker:get_state(CB),
    ?assertEqual(open, State).
```

### Integration Test: Full System Overload

```erlang
%% Test: System fails fast under overload
test_jidoka_overload() ->
    %% Arrange
    RateLimit = 100,
    jidoka_rate_limiter:reset(),

    %% Act: Send 150 rapid requests
    Results = [jidoka_rate_limiter:acquire(1) || _ <- lists:seq(1, 150)],

    %% Assert: Most are rejected quickly (not queued)
    Accepted = length([R || R <- Results, R =:= ok]),
    Rejected = length([R || R <- Results, R =:= {error, rate_limit_exceeded}]),

    ?assertTrue(Accepted > 0, "Some requests accepted"),
    ?assertTrue(Rejected > 0, "Some requests rejected"),
    ?assertTrue(Rejected > Accepted, "More rejections than acceptances").
```

### Stress Test: 1000 Concurrent Requests

```erlang
%% Test: No queue buildup under stress
test_no_queue_buildup() ->
    %% Arrange
    {available, InitialAvailable} = jidoka_worker_pool:status(),

    %% Act: Spawn 1000 concurrent requests
    [spawn(fun() -> jidoka_worker_pool:execute(fun() -> timer:sleep(10) end) end)
     || _ <- lists:seq(1, 1000)],
    timer:sleep(1000),

    %% Assert: Pool returned to normal (no queue)
    {available, FinalAvailable} = jidoka_worker_pool:status(),
    ?assertTrue(FinalAvailable > InitialAvailable - 5, "Pool recovered, no queue buildup").
```

---

## Operational Runbook

### Daily Monitoring

Every morning, check:

```erlang
%% Check circuit breaker status
Status = jidoka_supervisor:get_status(),
case maps:get(circuit_breaker_state, Status) of
    closed ->
        logger:info("✓ Circuit breaker normal (closed)");
    open ->
        logger:error("✗ Circuit breaker open - external service failing"),
        % Investigate and fix
        ok;
    half_open ->
        logger:warning("⚠ Circuit breaker testing recovery (half_open)")
end.

%% Check pool utilization
PoolStatus = jidoka_supervisor:get_pool_status(),
Utilization = maps:get(utilization_percent, PoolStatus),
case Utilization of
    X when X > 80 ->
        logger:warning("⚠ Pool utilization high: ~p%", [X]),
        % Consider scaling workers
        ok;
    _ ->
        logger:info("✓ Pool utilization normal: ~p%", [Utilization])
end.
```

### When Circuit Breaker Opens

**Timeline**:
- T+0: Service starts failing
- T+<1s: Circuit detects threshold and OPENS
- T+1-30s: Circuit breaker OPEN (all calls rejected immediately)
- T+30s: Circuit transitions to HALF_OPEN (testing recovery)
- T+30+: If service recovering, transitions back to CLOSED

**Actions**:

1. **Immediate (T+0-1s)**:
   ```
   Logs show: "Circuit breaker opening: threshold exceeded"
   Action: Page on-call engineer
   ```

2. **Investigation (T+1-30s)**:
   ```erlang
   %% What's failing?
   tail -f /var/log/jidoka.log

   %% Current state
   jidoka_supervisor:get_status()

   %% Service health
   external_service:health_check()
   ```

3. **Recovery (T+30s)**:
   ```erlang
   %% After fixing the service:
   ok = jidoka_supervisor:reset_circuit_breaker(),

   %% Verify it's closed
   ?assertEqual(closed, jidoka_circuit_breaker:get_state(...))
   ```

### When Rate Limiter Rejects Requests

**Diagnosis**:

```erlang
%% Check rate limit status
{Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),

case Tokens of
    0 -> logger:warning("Rate limiter exhausted: 0 tokens available");
    T -> logger:info("Rate limiter status: ~p tokens, ~p accepted, ~p rejected", [T, Accepted, Rejected])
end.
```

**Actions**:

1. **If legitimate spike**: Increase rate_limit and restart
   ```erlang
   %% Update config
   {ok, _} = jidoka_supervisor:start_link(#{rate_limit => 2000})
   ```

2. **If sustained overload**: Increase worker pool size
   ```erlang
   {ok, _} = jidoka_supervisor:start_link(#{pool_size => 20})
   ```

3. **If external service is slow**: Address root cause there (not Jidoka)
   ```
   Rate limiter is correctly rejecting requests because system can't handle load.
   The solution is to fix the external service, not increase limits.
   ```

### When Worker Pool Exhausted

```erlang
%% Workers busy
PoolStatus = jidoka_supervisor:get_pool_status(),
#{in_use := InUse, pool_size := Total} = PoolStatus,

case InUse of
    Total ->
        logger:warning("Worker pool FULL: ~p/~p workers in use", [InUse, Total]),
        % Option 1: Increase pool size
        % Option 2: Check for slow/hung workers
        % Option 3: Reduce request rate (back off upstream)
        ok;
    _ ->
        logger:info("Pool status: ~p/~p workers in use", [InUse, Total])
end.
```

---

## Troubleshooting

### Problem: Circuit Breaker Won't Close

**Symptoms**:
- Circuit breaker stuck in OPEN or HALF_OPEN
- All requests returning {error, circuit_open}

**Root Causes**:
1. External service still failing
2. Configuration timeout too long
3. Recovery time expired but service still returning errors

**Diagnosis**:
```erlang
%% Check service health
external_service:health_check()
%% If failing, service needs fixing

%% Check circuit breaker state
CB = whereis(jidoka_circuit_breaker),
{State, FailureCount, NextRecovery} = jidoka_circuit_breaker:status(),
logger:info("CB: state=~p, failures=~p, next_recovery=~p",
    [State, FailureCount, NextRecovery]).
```

**Solutions**:

1. **Service is unhealthy**: Fix external service first
   ```
   Don't reset circuit breaker until service is fixed.
   Resetting prematurely will just open it again immediately.
   ```

2. **Force reset (if testing)**:
   ```erlang
   jidoka_supervisor:reset_circuit_breaker()
   ```

### Problem: Rate Limiter Always Rejecting

**Symptoms**:
- All rate limiter calls returning {error, rate_limit_exceeded}
- Tokens stuck at 0

**Root Cause**:
- Rate limit configured too low
- Refill rate slower than request rate

**Diagnosis**:
```erlang
{Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),
logger:info("Tokens=~p, accepted=~p, rejected=~p", [Tokens, Accepted, Rejected]).
```

**Solution**:
```erlang
%% Increase rate limit
jidoka_supervisor:start_link(#{rate_limit => 5000})
```

### Problem: Worker Pool Consistently Full

**Symptoms**:
- execute() frequently returns {error, pool_exhausted}
- Workers taking too long to complete

**Root Cause**:
- Worker count too low
- Workers blocking on slow operations

**Diagnosis**:
```erlang
PoolStatus = jidoka_supervisor:get_pool_status(),
logger:info("Pool utilization: ~p%", [maps:get(utilization_percent, PoolStatus)]).

%% Check if workers are alive
jidoka_worker_pool:health_check().
```

**Solutions**:

1. **Increase pool size**:
   ```erlang
   jidoka_supervisor:start_link(#{pool_size => 20})
   ```

2. **Check for slow operations**:
   ```erlang
   %% Profile worker execution time
   monitor_worker_times()
   ```

3. **Add backpressure**: Fail-fast responses tell upstream to slow down
   ```erlang
   case jidoka_worker_pool:execute(Fun) of
       {error, pool_exhausted} ->
           % This is correct behavior - upstream should back off
           logger:info("System busy, rejecting request"),
           {error, service_busy}
   end
   ```

### Problem: Memory Usage Growing

**Symptoms**:
- Memory increasing over time
- No memory leaks detected

**Root Cause**:
- Circuit breaker keeping too many failure records
- Rate limiter not resetting metrics

**Diagnosis**:
```erlang
%% Check circuit breaker failure history
CB = whereis(jidoka_circuit_breaker),
{State, FailureCount, _} = jidoka_circuit_breaker:status(),
logger:info("CB failures in window: ~p", [FailureCount]).

%% Check rate limiter stats
{_Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),
logger:info("Rate limiter: accepted=~p, rejected=~p", [Accepted, Rejected]).
```

**Solutions**:

1. **Reset rate limiter metrics**:
   ```erlang
   jidoka_rate_limiter:reset()
   ```

2. **Circuit breaker cleanup** (automatic on state transition):
   ```erlang
   % No manual action needed - circuit breaker cleans up old failures
   % when it transitions states
   ```

---

## Metrics & SLOs

### Service Level Objectives (SLOs)

| Metric | SLO | Rationale |
|--------|-----|-----------|
| Circuit breaker open latency | < 1s | Detect failures quickly |
| Request rejection latency | < 100ms | Fail-fast principle |
| Circuit half_open duration | < 30s | Quick recovery testing |
| Worker availability | > 95% | System responsiveness |
| Health check passing | 100% | All components healthy |

### Key Metrics to Monitor

```erlang
%% Circuit Breaker Metrics
{State, FailureCount, NextRecoveryMs} = jidoka_circuit_breaker:status(),
% Track: State transitions, Failure rates, Recovery success rates

%% Rate Limiter Metrics
{Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),
% Track: Rejection rate (should be < 5% in normal operation)
%         Acceptance rate (should be steady)
%         Token refill tracking (should match rate_limit/s)

%% Worker Pool Metrics
{available, Available} = jidoka_worker_pool:status(),
Utilization = (PoolSize - Available) / PoolSize * 100,
% Track: Utilization (should be < 80% in normal operation)
%         Exhaustion events (should be rare)
%         Worker health (should all pass health_check)
```

### Grafana Dashboard Queries

```promql
%% Circuit Breaker State (0=closed, 1=open, 2=half_open)
circuit_breaker_state{service="jidoka"}

%% Rate Limiter Rejections per Second
rate(rate_limiter_rejections_total[5m])

%% Worker Pool Utilization
(worker_pool_size - worker_pool_available) / worker_pool_size * 100

%% Request Latency (should be low - fail-fast)
histogram_quantile(0.95, request_duration_seconds)
```

---

## References

### Manufacturing Jidoka
- Toyota Production System (TPS) - Taiichi Ohno
- "The Way of the Lean Leader" - Jaime Flinchbaugh
- Andon System - Stop-line autonomation principle

### Software Circuit Breaker
- Circuit Breaker Pattern - Michael T. Nygard, "Release It!"
- Hystrix - Netflix circuit breaker implementation
- AWS Well-Architected Framework - Reliability Pillar

### Erlang Supervision
- Erlang Documentation - supervisor behavior
- "Designing for Scalability with Erlang/OTP" - Francesco Cesarini
- gen_server and gen_statem design patterns

### Related Patterns
- Rate Limiting (Token Bucket Algorithm)
- Bulkhead Pattern (isolation)
- Fail-Fast Design
- Design for Lean Six Sigma (DfLSS)

---

## Quick Start

### 1. Start Jidoka
```erlang
{ok, _} = jidoka_supervisor:start_link(#{pool_size => 10}).
```

### 2. Execute Request with Jidoka Protection
```erlang
case jidoka_worker_pool:execute(fun() -> my_service:call() end) of
    ok -> logger:info("Success");
    {error, pool_exhausted} -> logger:warning("System busy");
    {error, Error} -> logger:error("Error: ~p", [Error])
end.
```

### 3. Monitor
```erlang
jidoka_supervisor:get_status().
```

### 4. Recover from Failure
```erlang
jidoka_supervisor:reset_circuit_breaker().
```

---

## Summary

Jidoka is the practice of **fail-fast, stop-the-line autonomation**. When errors accumulate, the system stops processing that request immediately rather than propagating the error through the system.

**Benefits**:
- 99% less resource waste
- 20x faster error detection
- Prevents cascading failures
- Improves system observability
- Enables rapid root cause analysis

**Key Principle**: Detect problems early, fail fast, preserve resources, fix root cause before resuming.

This is the essence of Jidoka applied to software systems.
