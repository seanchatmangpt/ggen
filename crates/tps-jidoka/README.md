# TPS Jidoka - Stop-the-Line Autonomation for Erlang

Production-grade Jidoka (autonomation with human touch) implementation in Erlang using supervisor trees, circuit breakers, worker pools, and rate limiting.

## Overview

**Jidoka** is a Japanese manufacturing principle: Stop the line when a defect is detected. In software, this means:
- **Fail-fast**: Detect errors immediately, stop processing
- **Don't amplify**: Don't retry failing operations, don't queue unbounded requests
- **Observable**: Every state transition is logged
- **Prevent defects**: Better to stop than to detect-and-fix later

## Quick Start

### 1. Start Jidoka
```erlang
{ok, _} = jidoka_supervisor:start_link(#{pool_size => 10}).
```

### 2. Execute with Protection
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

## Architecture

```
jidoka_supervisor (top-level, one_for_one strategy)
├── jidoka_circuit_breaker (detects failures, stops calling)
├── jidoka_rate_limiter (token bucket, limits throughput)
└── jidoka_worker_pool (fixed-size pool, no overflow)
    └── worker_1, worker_2, ..., worker_N
```

## Components

### Circuit Breaker
Detects service failures and stops calling the failing service immediately.

**States**: closed (normal) → open (failing) → half_open (testing) → closed

```erlang
Result = jidoka_circuit_breaker:call(fun() -> service:call() end),
case Result of
    ok -> jidoka_circuit_breaker:record_success();
    {error, circuit_open} -> logger:warning("Service failing");
    {error, Error} -> jidoka_circuit_breaker:record_failure()
end.
```

### Rate Limiter
Token bucket algorithm to limit request rate and prevent overload.

```erlang
case jidoka_rate_limiter:acquire(1) of
    ok -> process_request();
    {error, rate_limit_exceeded} -> logger:warning("Rate limit exceeded")
end.
```

### Worker Pool
Fixed-size pool for request processing (no unbounded queues, no overflow).

```erlang
case jidoka_worker_pool:execute(fun() -> do_work() end) of
    ok -> logger:info("Work completed");
    {error, pool_exhausted} -> logger:warning("System busy")
end.
```

## Configuration

```erlang
Config = #{
    pool_size => 10,              % Number of workers
    circuit_threshold => 5,       % Failures to trigger open
    window_ms => 10000,           % Time window for counting failures
    rate_limit => 1000            % Requests per second
},

{ok, _} = jidoka_supervisor:start_link(Config).
```

## Key Metrics

| Metric | SLO | Rationale |
|--------|-----|-----------|
| Circuit open latency | < 1s | Detect failures quickly |
| Request rejection latency | < 100ms | Fail-fast principle |
| Worker availability | > 95% | System responsiveness |

## Testing

Run the test suite:

```bash
rebar3 ct
```

Tests include:
- Circuit breaker state transitions
- Worker pool exhaustion (fail-fast)
- Rate limiter token refill
- System overload behavior
- Stress tests (1000+ concurrent requests)

## Documentation

See `/docs/tps-reference/10-jidoka.md` for:
- Complete Jidoka principles
- Manufacturing analogies
- Implementation guide
- Operational runbook
- Troubleshooting guide

## Building

```bash
# Compile
rebar3 compile

# Run tests
rebar3 ct

# Run shell with application loaded
rebar3 shell

# Generate documentation
rebar3 edoc
```

## Dependencies

- **poolboy** (1.5.2) - Worker pool management
- **lager** (3.9.2) - Logging
- Erlang/OTP 24+ (tested on 24, 25, 26, 27)

## Production Deployment

1. **Start Jidoka in your app**:
   ```erlang
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

2. **Monitor in production**:
   ```erlang
   %% Daily check
   Status = jidoka_supervisor:get_status(),
   PoolStatus = jidoka_supervisor:get_pool_status().
   ```

3. **Recover from failures**:
   ```erlang
   %% After fixing external service
   ok = jidoka_supervisor:reset_circuit_breaker().
   ```

## License

MIT - See LICENSE file

## References

- Toyota Production System - Taiichi Ohno
- "Release It!" - Michael T. Nygard (Circuit Breaker Pattern)
- Erlang/OTP Documentation - Supervisor Trees
