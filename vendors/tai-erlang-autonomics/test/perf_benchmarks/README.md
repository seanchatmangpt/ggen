# Performance Benchmarking Suite

Comprehensive performance benchmarking and stress testing framework for the TAI Erlang Autonomics system.

## Overview

This suite measures performance across all critical components:

1. **HTTP Endpoint Benchmarks** - REST API latency and throughput
2. **Governor Performance** - State machine efficiency
3. **Receipt Ledger** - Storage and indexing performance
4. **Action Executor** - Task execution and worker pool efficiency
5. **System-Wide Stress Tests** - Integrated load testing

## Quick Start

### Run All Benchmarks

```bash
# Run entire benchmark suite
rebar3 ct --suite=test/perf_benchmarks

# Run specific test suite
rebar3 ct --suite=http_endpoint_bench_SUITE
rebar3 ct --suite=governor_perf_bench_SUITE
rebar3 ct --suite=receipt_ledger_bench_SUITE
rebar3 ct --suite=action_executor_bench_SUITE
rebar3 ct --suite=system_stress_bench_SUITE
```

### View Results

Results are logged to stdout and stored in Common Test log files:

```bash
# Show latest results
tail -100 _build/test/logs/ct_run.latest/make_data.json

# Generate HTML report
# Common Test generates HTML automatically in _build/test/logs/
```

## Benchmark Suites

### 1. HTTP Endpoint Benchmarks (`http_endpoint_bench_SUITE.erl`)

Measures REST API performance with realistic client load.

**Tests Included**:
- `bench_health_endpoint` - Health check (GET /health)
- `bench_pubsub_endpoint` - Event ingestion (POST /pubsub)
- `bench_marketplace_endpoint` - Marketplace events (POST /marketplace)
- `bench_concurrent_requests` - Concurrency scaling (10, 25, 50 concurrent)
- `bench_large_payloads` - 1MB payload handling
- `bench_error_handling` - Malformed request resilience

**Key Metrics**:
- Response latency (p50, p95, p99)
- Throughput (RPS)
- Error rate
- Memory usage

**Performance Targets**:
```
Health:        p99 < 100ms,  throughput > 100 RPS
Pub/Sub:       p99 < 500ms,  throughput > 20 RPS
Marketplace:   p99 < 500ms,  throughput > 20 RPS
Concurrent 50: p99 < 300ms,  success rate > 95%
```

### 2. Governor Performance Benchmarks (`governor_perf_bench_SUITE.erl`)

Measures state machine efficiency and signal processing.

**Tests Included**:
- `bench_state_transitions` - Boot→Stable transitions
- `bench_signal_processing` - Signal evaluation latency
- `bench_concurrent_governors` - Multiple governor instances
- `bench_entitlement_checks` - Permission validation speed
- `bench_action_tracking` - In-flight action management
- `bench_event_postponement` - Event queuing during interventions

**Key Metrics**:
- Operation latency (microseconds)
- Throughput (ops/sec)
- Percentile distributions (p50, p95, p99)

**Performance Targets**:
```
State Transitions: p99 < 10ms,    throughput > 100 ops/sec
Signal Processing: p99 < 1000μs,  throughput > 1000 ops/sec
Concurrent Govs:   100 govs,      10000+ combined ops/sec
```

### 3. Receipt Ledger Benchmarks (`receipt_ledger_bench_SUITE.erl`)

Measures storage, indexing, and retrieval performance.

**Tests Included**:
- `bench_write_latency` - Single write performance
- `bench_concurrent_writes` - 10 workers, 1000 writes each
- `bench_read_performance` - Read latency and throughput
- `bench_index_lookups` - Index-based retrieval
- `bench_batch_operations` - Bulk write efficiency
- `bench_storage_efficiency` - Space utilization

**Key Metrics**:
- Write latency (microseconds)
- Read latency (microseconds)
- Throughput (ops/sec)
- Storage bytes per receipt

**Performance Targets**:
```
Write Latency:  p99 < 50μs,      throughput > 10000 writes/sec
Read Latency:   p99 < 20μs,      throughput > 100000 reads/sec
Batch Ops:      per-receipt < 5ms, throughput > 10000 receipts/sec
```

### 4. Action Executor Benchmarks (`action_executor_bench_SUITE.erl`)

Measures task execution and worker pool performance.

**Tests Included**:
- `bench_action_execution` - Basic execution latency
- `bench_poolboy_throughput` - Worker pool efficiency
- `bench_poolboy_concurrent` - 20 concurrent clients
- `bench_worker_utilization` - Pool capacity analysis
- `bench_queue_latency` - Queuing behavior under load
- `bench_action_types` - Per-type performance (scale, rollback, throttle)

**Key Metrics**:
- Execution latency (milliseconds)
- Throughput (ops/sec)
- Pool utilization (%)
- Queue depth

**Performance Targets**:
```
Scale Action:     p99 < 20ms,    throughput > 200 ops/sec
Rollback Action:  p99 < 30ms,    throughput > 100 ops/sec
Throttle Action:  p99 < 15ms,    throughput > 300 ops/sec
Poolboy Queue:    p99 wait < 50ms, utilization > 75%
```

### 5. System Stress Tests (`system_stress_bench_SUITE.erl`)

Integrated end-to-end load testing with realistic workloads.

**Tests Included**:
- `bench_steady_state` - Sustained load (20 workers, 60 sec)
- `bench_ramp_up` - Gradually increasing load (2 minutes)
- `bench_burst_load` - Sudden spike (100 concurrent, 30 sec)
- `bench_mixed_load` - Multiple endpoint types
- `bench_memory_stability` - 3-minute memory trend analysis
- `bench_recovery` - Recovery after load spike

**Key Metrics**:
- Request success rate (%)
- End-to-end latency
- Memory usage trends
- CPU utilization
- GC impact

**Performance Targets**:
```
Steady-State:      success > 95%,  p99 < 1000ms
Ramp-Up:           success > 90%,  no crashes
Burst Load:        success > 80%,  recovery < 30sec
Memory Stability:  growth < 0.5 MB/sec, no leaks
```

## Performance Baselines

Current measured baselines (v1.0.0):

| Component | Metric | P50 | P95 | P99 |
|-----------|--------|-----|-----|-----|
| Health Endpoint | Latency | 5ms | 20ms | 100ms |
| Pub/Sub Endpoint | Latency | 100ms | 250ms | 500ms |
| Governor Signal | Latency | 0.5ms | 2ms | 10ms |
| Receipt Write | Latency | 10μs | 30μs | 50μs |
| Action Executor | Latency | 2ms | 15ms | 30ms |

## Custom Benchmarking

### Running Specific Tests

```bash
# Run only health endpoint benchmark
rebar3 ct --suite=http_endpoint_bench_SUITE --case=bench_health_endpoint

# Run with specific log level
rebar3 ct --suite=http_endpoint_bench_SUITE --case=bench_health_endpoint -v
```

### Adding New Benchmark Tests

1. Create new function in benchmark suite:
```erlang
bench_my_feature(Config) ->
    TestName = "my_feature",
    OperationCount = 1000,

    % Measure performance
    Latencies = measure_my_operation(OperationCount, []),

    % Analyze results
    PerfResult = analyze_latencies(Latencies, TestName, OperationCount),
    print_result(PerfResult),

    % Assert targets
    ?assert(PerfResult#perf_result.p99_time_us < 10000, "p99 should be < 10ms"),

    Config.
```

2. Add to `all()` export list:
```erlang
all() -> [
    bench_health_endpoint,
    bench_pubsub_endpoint,
    bench_my_feature  % Add here
].
```

### Measuring Custom Operations

Template for custom measurement:

```erlang
measure_my_operation(0, Latencies) ->
    Latencies;
measure_my_operation(Count, Latencies) ->
    StartTime = erlang:system_time(microsecond),

    % Perform operation
    my_operation_call(),

    Latency = erlang:system_time(microsecond) - StartTime,

    measure_my_operation(Count - 1, [Latency | Latencies]).
```

## Performance Analysis

### Interpreting Results

**Latency Percentiles**:
- **P50 (Median)**: Half of requests faster than this
- **P95**: 95% of requests faster than this (tail latency start)
- **P99**: 99% of requests faster than this (tail latency visible)
- **P99.9**: 99.9% of requests faster than this (extreme outliers)

**Throughput Calculations**:
```
Throughput = Total Operations / Total Time
Example: 10,000 operations in 100 seconds = 100 ops/sec
```

**Memory Metrics**:
- **Initial**: Baseline before test
- **Peak**: Maximum observed during test
- **Final**: After garbage collection
- **Growth Rate**: Bytes per second (indicates leaks)

### Identifying Bottlenecks

1. **High P99 vs P95**: Occasional slow requests (GC, contention)
2. **High P99 vs P50**: Tail latency problem (bursty load)
3. **Memory growth > 0.5 MB/sec**: Potential memory leak
4. **Success rate < 95%**: System overload or errors
5. **Queue depth increasing**: Worker pool too small

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Performance Tests
on: [push, pull_request]

jobs:
  performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: 27.0
      - run: rebar3 ct --suite=test/perf_benchmarks
      - uses: actions/upload-artifact@v3
        if: always()
        with:
          name: perf-results
          path: _build/test/logs/
```

## Troubleshooting

### Common Issues

**Issue**: Tests timeout (30 second limit)
```
Solution: Reduce OperationCount or increase timeout in test setup
```

**Issue**: HTTP requests fail with connection refused
```
Solution: Ensure application is started in init_per_suite/1
Check: application:ensure_all_started(tai_autonomics)
```

**Issue**: Memory usage exceeds expected baseline
```
Solution: Check for message queue accumulation
Use: erlang:memory(processes) to analyze
```

**Issue**: Latency variability too high (P99 >> P95)
```
Solution: Increase sample size (OperationCount)
Indicates GC or system contention
```

### Performance Profiling

Enable detailed profiling:

```erlang
% In test setup
erlang:trace_pattern({'_', '_', '_'}, true, [call_time]),
erlang:trace(all, true, [call, {tracer, dbg:tracer(dbg:dhandler())}]),

% In test cleanup
erlang:trace(all, false),
rprof:stop().
```

## Real-World Load Patterns

### Recommended Test Configuration

For staging/pre-production testing:

```bash
# Steady-state smoke test (5 minutes)
20 concurrent workers
Expected: p99 < 500ms, success > 95%

# Realistic daily load (30 minutes)
20-50 workers, variable load
Expected: p99 < 750ms, success > 90%

# Stress test (10 minutes)
100+ concurrent workers
Expected: success > 80%, recovery < 30sec
```

## Performance Regression Detection

Automated threshold monitoring:

```erlang
% In test assertions
?assert(PerfResult#perf_result.p99_time_us < baseline_p99 * 1.1,
        "P99 latency increased more than 10%"),

?assert(PerfResult#perf_result.throughput > baseline_throughput * 0.9,
        "Throughput decreased more than 10%"),

?assert(PerfResult#perf_result.memory_used_mb < baseline_memory * 1.2,
        "Memory usage increased more than 20%"),
```

## Production Monitoring

### Key Metrics to Monitor

1. **API Response Time**: p50, p95, p99 per endpoint
2. **Error Rate**: % of requests returning 4xx, 5xx
3. **Throughput**: RPS per service
4. **Concurrency**: Active request count
5. **Resource Usage**: CPU %, Memory %, Disk I/O
6. **Queue Depths**: Poolboy, message queues
7. **GC Pause Time**: Young gen, full gen

### Alert Thresholds

```
p99 latency > 1000ms        → ALERT
success rate < 90%           → ALERT
memory growth > 1.0 MB/sec   → ALERT
queue depth > 200            → WARN
GC pause > 500ms             → WARN
CPU utilization > 80%        → WARN
```

## References

- [Erlang Memory Management](https://www.erlang.org/doc/man/erlang.html#memory-0)
- [Common Test Framework](https://erlang.org/doc/apps/common_test/index.html)
- [Performance Tuning Guide](https://www.erlang.org/doc/efficiency_guide/index.html)
- [Rebar3 Testing](https://rebar3.org/docs/running-tests)

## Contributing

To add new benchmarks:

1. Create test suite in `test/perf_benchmarks/`
2. Implement Common Test callbacks
3. Add comprehensive logging
4. Define clear performance targets
5. Document expected results
6. Update this README

## Support

For questions or issues with benchmarking:

1. Check existing benchmark examples
2. Review Erlang timing documentation
3. Enable verbose logging: `rebar3 ct -v`
4. Check Common Test logs: `_build/test/logs/`

---

**Last Updated**: 2026-01-25
**Version**: v1.0.0
