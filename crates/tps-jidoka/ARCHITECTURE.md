# TPS Jidoka Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Your Application                          │
├─────────────────────────────────────────────────────────────┤
│  Request Handler                                             │
│  ├─ Rate Limiter: acquire(1)                                │
│  │  ├─ ok → proceed                                          │
│  │  └─ error: rate_limit_exceeded → {error, too_many}       │
│  ├─ Worker Pool: execute(Fun)                               │
│  │  ├─ ok → continue                                         │
│  │  └─ error: pool_exhausted → {error, busy}                │
│  └─ Circuit Breaker: call(Fun)                              │
│     ├─ closed: forward call                                  │
│     ├─ open: reject immediately                              │
│     └─ half_open: allow test call                            │
├─────────────────────────────────────────────────────────────┤
│                    Jidoka Supervisor                         │
├─────────────────────────────────────────────────────────────┤
│  Circuit Breaker        │  Rate Limiter        │  Worker Pool│
│  ─────────────────────  │  ──────────────────  │  ──────────│
│  - closed               │  - tokens = 500      │  - 10 workers
│  - open                 │  - refill = 1000/s   │  - no queue
│  - half_open            │  - rejected = 0      │  - isolated
│                         │                      │             │
└─────────────────────────────────────────────────────────────┘
```

## Supervision Tree

```erlang
jidoka_supervisor (one_for_one)
├── jidoka_circuit_breaker (gen_statem)
│   └── Monitors: failure rate, state transitions
├── jidoka_rate_limiter (gen_server)
│   └── Manages: token bucket, refill timer
└── jidoka_worker_pool (supervisor)
    ├── poolboy pool (worker management)
    └── health_monitor (periodic liveness checks)
        ├── worker_1 (gen_server)
        ├── worker_2 (gen_server)
        └── ... worker_N (gen_server)
```

### Strategy: one_for_one

If a single worker crashes:
1. Supervisor detects crash
2. Only that worker is restarted
3. Other workers continue processing
4. System remains operational

**NOT** one_for_all (which would restart everything).

## State Machine: Circuit Breaker

```
                ┌─────────────────────┐
                │   Initialization    │
                └──────────┬──────────┘
                           │
                           v
                ┌─────────────────────┐
    ┌──────────→│     CLOSED          │←──────────┐
    │           │  (normal operation) │           │
    │           └──────────┬──────────┘           │
    │                      │                      │
    │          (failures < threshold)     (success on test)
    │                      │                      │
    │                      v                      │
    │           ┌─────────────────────┐           │
    │           │      OPEN           │           │
    │           │   (fail-fast)       │           │
    │           └──────────┬──────────┘           │
    │                      │                      │
    │              (timeout = recovery_ms)        │
    │                      │                      │
    │                      v                      │
    │           ┌─────────────────────┐           │
    │           │   HALF_OPEN         │           │
    │           │  (testing recovery) │           │
    │           └─────┬───────┬───────┘           │
    │                 │       │                   │
    └─────────────────┘       └───────────────────┘
      (test failed)           (test succeeded)
```

## Rate Limiter: Token Bucket

```
Token Bucket State Over Time
──────────────────────────────────

Time 0: [████████████████] 500 tokens (max burst)
        acquire(100) → [████████████] 400 tokens (ok)

Time 1s: Refill +1000 tokens
         [████████████████████████████████] 1000 tokens (capped at burst)
         acquire(200) → [████████████████████████] 800 tokens (ok)

Time 2s: Refill +1000 tokens
         [████████████████████████████████] 1000 tokens
         acquire(150) → [██████████████████████████] 850 tokens (ok)

Time 3s: No refill (0s elapsed since last check)
         acquire(1000) → {error, rate_limit_exceeded} (only 850 available)
         after 0.15s: Refill +150 tokens
                     [████████████████████████████████] 1000 tokens
                     acquire(500) → [████████████] 500 tokens (ok)
```

## Request Flow

### 1. Normal Operation (Circuit Breaker: CLOSED)

```
Request
  ↓
[Rate Limiter] ─→ acquire(1)
  ├─ tokens available? ─→ YES
  ├─ tokens -= 1
  └─ return ok
  ↓
[Worker Pool] ─→ execute(fun() -> ... end)
  ├─ worker available? ─→ YES (3/10 in use)
  ├─ check worker out
  ├─ execute function
  ├─ check worker in
  └─ return result
  ↓
[Circuit Breaker] ─→ call(fun() -> service_call() end)
  ├─ state = closed ─→ forward call
  ├─ service succeeds
  ├─ record_success()
  └─ return ok
  ↓
Response to Client: {ok, Result}
```

### 2. Overload Scenario (Rate Limited)

```
Request N+1, N+2, ..., N+500
  ↓
[Rate Limiter]
  ├─ acquire(1) → {error, rate_limit_exceeded} (no tokens)
  └─ all 500 remaining requests REJECTED IMMEDIATELY (1ms each)
  ↓
Response to Clients: {error, rate_limit_exceeded}

Total time for 500 requests: ~500ms (not queued, fail-fast)
Memory used: ~0 (no queue buildup)
```

### 3. Service Failure (Circuit Opens)

```
Request 1-5: Service fails
  ↓
[Circuit Breaker]
  ├─ record_failure() → 1
  ├─ record_failure() → 2
  ├─ record_failure() → 3
  ├─ record_failure() → 4
  ├─ record_failure() → 5 (threshold exceeded!)
  └─ state: closed → open
  ↓
Request 6+: Circuit is OPEN
  ↓
[Circuit Breaker]
  ├─ state = open
  ├─ return {error, circuit_open} IMMEDIATELY (1ms)
  └─ NO RETRY, NO QUEUE
  ↓
Response to Client: {error, circuit_open}
```

## Process Isolation

### Worker Pool: One_For_One Isolation

```
Scenario: Worker crashes during request

Before:
┌─────────┬─────────┬─────────┬─────────┬─────────┐
│ Worker1 │ Worker2 │ Worker3 │ Worker4 │ Worker5 │
│  busy   │ idle    │  busy   │ idle    │  busy   │
└─────────┴─────────┴─────────┴─────────┴─────────┘
Processing: 3 requests

Problem: Worker3 crashes!
┌─────────┬─────────┬─────────┴─────────┬─────────┐
│ Worker1 │ Worker2 │  CRASHED ✗        │ Worker4 │ Worker5 │
│  busy   │ idle    │                   │ idle    │  busy   │
└─────────┴─────────┴───────────────────┴─────────┴────────┘
Processing: 2 requests

Supervisor Restart (isolated):
- Restart only Worker3
- Worker1, Worker2, Worker4, Worker5 CONTINUE (not affected)
- 2 ongoing requests complete successfully

After:
┌─────────┬─────────┬─────────┬─────────┬─────────┐
│ Worker1 │ Worker2 │ Worker3 │ Worker4 │ Worker5 │
│ idle    │ idle    │ idle    │ idle    │ idle    │
└─────────┴─────────┴─────────┴─────────┴─────────┘
Processing: 0 requests (recovered)
```

## Performance Characteristics

### Latency (p99)

| Operation | Latency | Context |
|-----------|---------|---------|
| Rate limiter check | 1μs | Token bucket O(1) operation |
| Rate limit reject | 100μs | Hash lookup + compare |
| Circuit breaker check | 10μs | State machine O(1) check |
| Circuit open reject | 1ms | Immediate rejection (fail-fast) |
| Worker pool checkout | 100μs | Lock-free pool operation |
| Worker pool exhausted | 1ms | Fast failure |
| Worker execution | 10-1000ms | Depends on work function |

### Memory

| Component | Memory | Notes |
|-----------|--------|-------|
| Circuit Breaker | ~1KB | State machine + 10 failure records |
| Rate Limiter | ~1KB | Token bucket + metrics |
| Worker Pool (10 workers) | ~10MB | 1MB per worker process |
| **Total (10 workers)** | **~12MB** | Lean system design |

### Concurrency

| Scenario | Behavior |
|----------|----------|
| 1 request | Fast (< 10ms total) |
| 10 requests | All process (workers available) |
| 100 requests | 10 process, 90 queued in rate limiter |
| 1000 requests | ~100 accepted, ~900 rejected immediately |

## Failure Modes & Recovery

### Circuit Breaker Failure

```
External Service Failure (e.g., timeout, 500 error)
  ↓
1. Failures accumulate (recorded in failure list)
2. Threshold exceeded (e.g., 5 failures in 10s)
3. Circuit OPENS (stop calling service)
4. All requests rejected immediately with {error, circuit_open}
5. Time passes (30s recovery timeout)
6. Circuit transitions to HALF_OPEN (testing)
7. One test request is allowed through
   - If succeeds: Circuit CLOSES (normal operation resumes)
   - If fails: Circuit OPENS again (back to failing)
```

### Worker Pool Failure

```
Worker Process Crash (e.g., unhandled exception)
  ↓
1. Supervisor detects crash
2. Worker is restarted (gen_server init called)
3. In-flight request for that worker fails
4. But other workers continue (isolation)
5. System remains available (degraded capacity)
```

### Rate Limiter Failure

```
Token Bucket Empty (all tokens consumed)
  ↓
1. Subsequent requests rejected with {error, rate_limit_exceeded}
2. Tokens refill at configured rate (e.g., 1000/s)
3. After ~0.5s, ~500 tokens available
4. New requests begin succeeding
```

## Configuration Tuning

### For High-Throughput Systems

```erlang
#{
    pool_size => 50,            % More workers
    circuit_threshold => 10,    % More lenient (allow more failures)
    window_ms => 20000,         % Longer window (less sensitive)
    rate_limit => 10000         % Higher limit
}
```

### For Latency-Sensitive Systems

```erlang
#{
    pool_size => 5,             % Fewer workers (quick feedback)
    circuit_threshold => 2,     % More strict (fail-fast)
    window_ms => 5000,          % Shorter window (quick response)
    rate_limit => 100           % Conservative limit
}
```

### For Production Systems

```erlang
#{
    pool_size => 20,            % Reasonable default
    circuit_threshold => 5,     % Good balance
    window_ms => 10000,         % Standard window
    rate_limit => 1000          % Typical TPS
}
```

## Monitoring & Observability

### Key Metrics to Export

```erlang
%% Circuit Breaker
circuit_breaker_state{value: 0=closed|1=open|2=half_open}
circuit_breaker_failures_total{counter}
circuit_breaker_recoveries_total{counter}

%% Rate Limiter
rate_limiter_tokens{gauge: current_tokens}
rate_limiter_accepted_total{counter}
rate_limiter_rejected_total{counter}
rate_limiter_refill_rate{gauge: tokens/second}

%% Worker Pool
worker_pool_size{gauge}
worker_pool_available{gauge}
worker_pool_utilization_percent{gauge}
worker_pool_rejected_total{counter}
worker_pool_execution_time_ms{histogram}
```

### Example Prometheus Scrape

```
# HELP circuit_breaker_state Circuit breaker state (0=closed, 1=open, 2=half_open)
# TYPE circuit_breaker_state gauge
circuit_breaker_state 0

# HELP rate_limiter_tokens Current tokens in bucket
# TYPE rate_limiter_tokens gauge
rate_limiter_tokens 450.5

# HELP worker_pool_utilization_percent Worker pool utilization
# TYPE worker_pool_utilization_percent gauge
worker_pool_utilization_percent 30
```

## Summary

TPS Jidoka provides a **production-grade, fail-fast system** with:
- **Circuit Breaker**: Detects service failures, stops calling immediately
- **Rate Limiter**: Prevents overload, rejects gracefully
- **Worker Pool**: Fixed-size, isolated workers, no unbounded queues
- **Supervision Tree**: One_for_one isolation, quick recovery
- **Observable**: Every state transition logged, metrics exported

The system is designed to **prevent defects from propagating**, **stop-the-line when problems occur**, and **provide rapid feedback** to operators.
