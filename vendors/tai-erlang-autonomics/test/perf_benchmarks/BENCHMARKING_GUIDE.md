# TAI Erlang Autonomics - Performance Benchmarking Guide

## Quick Reference

```bash
# Run all performance benchmarks
./scripts/run_performance_benchmarks.sh all

# Run specific benchmark suite
./scripts/run_performance_benchmarks.sh http
./scripts/run_performance_benchmarks.sh governor
./scripts/run_performance_benchmarks.sh ledger
./scripts/run_performance_benchmarks.sh executor
./scripts/run_performance_benchmarks.sh stress

# Quick smoke test (5 minutes)
./scripts/run_performance_benchmarks.sh quick

# Generate HTML report
./scripts/run_performance_benchmarks.sh all --report

# Verbose output
./scripts/run_performance_benchmarks.sh all --verbose

# Custom timeout (in seconds)
./scripts/run_performance_benchmarks.sh all --timeout 1200
```

---

## Benchmark Architecture

### Test Hierarchy

```
Performance Benchmarks (test/perf_benchmarks/)
├── http_endpoint_bench_SUITE.erl
│   └── HTTP REST API latency and throughput
│       ├── Health check endpoint
│       ├── Pub/Sub event handler
│       ├── Marketplace entitlement handler
│       ├── Concurrent request scaling
│       ├── Large payload handling
│       └── Error handling resilience
│
├── governor_perf_bench_SUITE.erl
│   └── Governor state machine efficiency
│       ├── State transitions (boot→stable→...→boot)
│       ├── Signal processing pipeline
│       ├── Concurrent governor instances
│       ├── Entitlement validation
│       ├── Action tracking
│       └── Event postponement
│
├── receipt_ledger_bench_SUITE.erl
│   └── Receipt storage and indexing
│       ├── Single-record write latency
│       ├── Concurrent writes (10 workers)
│       ├── Read performance
│       ├── Index-based lookups
│       ├── Batch operations
│       └── Storage efficiency
│
├── action_executor_bench_SUITE.erl
│   └── Task execution and worker pool
│       ├── Action execution latency
│       ├── Poolboy throughput (10 workers)
│       ├── Concurrent client load (20 clients)
│       ├── Worker utilization metrics
│       ├── Queue latency under burst load
│       └── Per-action-type performance
│
└── system_stress_bench_SUITE.erl
    └── Integrated end-to-end testing
        ├── Steady-state load (20 workers, 60 sec)
        ├── Ramp-up profile (gradually increasing)
        ├── Burst load (sudden spike to 100 concurrent)
        ├── Mixed endpoint load
        ├── Memory stability (3 minute test)
        └── Recovery after load spike
```

### Common Test Integration

Each benchmark suite uses Erlang Common Test framework:

```erlang
% Standard callbacks
init_per_suite/1     - Initialize test environment
end_per_suite/1      - Clean up after all tests
init_per_testcase/2  - Before each test
end_per_testcase/2   - After each test

% Test functions
bench_*              - Individual benchmark tests
```

---

## Measurement Techniques

### Latency Measurement Pattern

All latency measurements use Erlang's high-resolution timer:

```erlang
%% Measure operation latency
measure_operation(0, Latencies) ->
    Latencies;
measure_operation(Count, Latencies) ->
    % Timestamp before operation
    StartTime = erlang:system_time(microsecond),

    % Perform the operation
    _ = my_operation(),

    % Calculate elapsed time
    Latency = erlang:system_time(microsecond) - StartTime,

    % Collect results
    measure_operation(Count - 1, [Latency | Latencies]).
```

**Timing Units**:
- **Microseconds (μs)**: For fast operations (< 1ms)
- **Milliseconds (ms)**: For I/O operations (1-1000ms)
- **Seconds (s)**: For sustained tests (system stress)

### Statistical Analysis

Results are analyzed using percentile distributions:

```erlang
%% Calculate percentile
percentile(List, Percentile, Length) when Length > 0 ->
    Index = max(1, round(Percentile * Length)),
    lists:nth(Index, lists:sort(List)).

%% Generate statistics
Statistics = #{
    count => length(Latencies),
    mean => avg(Latencies),
    p50 => percentile(Latencies, 0.50, length(Latencies)),
    p95 => percentile(Latencies, 0.95, length(Latencies)),
    p99 => percentile(Latencies, 0.99, length(Latencies)),
    max => max(Latencies)
}
```

### Resource Monitoring

Memory and resource metrics are collected via Erlang built-ins:

```erlang
%% Memory usage snapshot
erlang:memory(total)        % Total memory in bytes
erlang:memory(processes)    % Process memory
erlang:memory(system)       % System memory
erlang:memory(atom)         % Atom table memory

%% CPU metrics (estimated via process time)
erlang:statistics(runtime)  % CPU time used
erlang:statistics(wallclock) % Wall clock time

%% Garbage collection
erlang:statistics(garbage_collection) % GC stats
```

---

## Performance Targets & Thresholds

### Critical Performance SLOs

These are hard requirements for production deployment:

```
HTTP Endpoints:
  - Health check p99:        < 100ms  (MUST)
  - Pub/Sub handler p99:     < 500ms  (MUST)
  - Marketplace handler p99: < 500ms  (MUST)
  - Error rate:              < 5%     (MUST)

Governor State Machine:
  - Signal processing p99:   < 10ms   (MUST)
  - Entitlement check p99:   < 5ms    (MUST)
  - State transition time:   < 50ms   (MUST)

Receipt Ledger:
  - Write latency p99:       < 50μs   (MUST)
  - Read latency p99:        < 20μs   (MUST)
  - Batch throughput:        > 10k receipts/sec (MUST)

System Stability:
  - Sustained success rate:  > 95%    (MUST)
  - Memory growth rate:      < 0.5 MB/sec (MUST)
  - GC pause time p99:       < 500ms  (SHOULD)
  - No memory leaks:         True     (MUST)
```

### Regression Thresholds

If a metric degrades by more than these thresholds, investigate:

```
Latency Regression:   > 10% increase
Throughput Drop:      > 10% decrease
Memory Growth:        > 20% increase from baseline
Error Rate Increase:  > 2% from baseline
Success Rate Drop:    > 5% from baseline
```

---

## Interpreting Results

### Understanding Percentiles

**Example: HTTP Health Check Latency**

```
P50:  5ms   - Half of requests complete in 5ms or less
P95:  20ms  - 95% of requests complete in 20ms or less
P99:  100ms - 99% of requests complete in 100ms or less
MAX:  150ms - Slowest request took 150ms

Interpretation:
- Median performance is excellent (5ms)
- Most users see good performance (< 20ms for 95%)
- Small percentage see slow responses (tail latency)
- Slowest response is 150ms (20x median)
```

### Red Flags

**High Tail Latency (P99 >> P95)**
- Indicates occasional slowdowns
- Likely causes: garbage collection, contention, GC pauses
- Action: Enable detailed GC logging, check system load

**Increasing Latency with Concurrency**
- Linear increase: Normal contention
- Exponential increase: Queue saturation
- Action: Increase worker pool size, implement backpressure

**Memory Growth Over Time**
- Steady growth > 0.5 MB/sec: Potential memory leak
- Spiky growth: Normal GC behavior
- Action: Run heap analysis, check for accumulating messages

**High Error Rate**
- > 5% errors: System overload
- Specific error patterns: Logic bugs
- Action: Check error logs, validate payload formats

---

## Running Custom Benchmarks

### Creating a New Benchmark

1. **Create test file** in `test/perf_benchmarks/`:

```erlang
-module(my_feature_bench_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Callbacks
-export([init_per_suite/1, end_per_suite/1, all/0]).
-export([bench_my_feature/1]).

init_per_suite(Config) ->
    application:ensure_all_started(tai_autonomics),
    Config.

end_per_suite(_) ->
    application:stop(tai_autonomics).

all() -> [bench_my_feature].

bench_my_feature(Config) ->
    OperationCount = 1000,

    % Measure
    Latencies = [
        measure_operation() || _ <- lists:seq(1, OperationCount)
    ],

    % Analyze
    Result = analyze_latencies(Latencies, OperationCount),

    % Assert
    ?assert(Result#result.p99 < 100000),

    Config.

measure_operation() ->
    Start = erlang:system_time(microsecond),
    my_function(),
    erlang:system_time(microsecond) - Start.

analyze_latencies(Latencies, Count) ->
    Sorted = lists:sort(Latencies),
    #{
        p50 => percentile(Sorted, 0.50),
        p95 => percentile(Sorted, 0.95),
        p99 => percentile(Sorted, 0.99)
    }.
```

2. **Run your test**:

```bash
rebar3 ct --suite=test/perf_benchmarks/my_feature_bench_SUITE
```

### Measuring Specific Operations

**HTTP Request Latency**:

```erlang
measure_http_request(URL, Count) ->
    {ok, _} = inets:start(httpc, [{profile, bench}]),
    [
        begin
            Start = erlang:system_time(millisecond),
            {ok, {{_, Status, _}, _, _}} = httpc:request(get, {URL, []}, [], []),
            erlang:system_time(millisecond) - Start
        end || _ <- lists:seq(1, Count)
    ].
```

**Erlang Process Latency**:

```erlang
measure_process_call(Pid, Request, Count) ->
    [
        begin
            Start = erlang:system_time(microsecond),
            _ = gen_server:call(Pid, Request),
            erlang:system_time(microsecond) - Start
        end || _ <- lists:seq(1, Count)
    ].
```

**Concurrent Worker Throughput**:

```erlang
measure_concurrent(Fun, WorkerCount, OperationsPerWorker) ->
    Workers = [
        spawn(fun() ->
            [Fun() || _ <- lists:seq(1, OperationsPerWorker)]
        end)
        || _ <- lists:seq(1, WorkerCount)
    ],
    [receive after infinity -> ok end || _ <- Workers].
```

---

## Continuous Performance Monitoring

### CI/CD Integration

Add to your CI pipeline to catch regressions early:

```yaml
# .github/workflows/performance.yml
name: Performance Tests
on: [push, pull_request]

jobs:
  perf:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: 27.0

      - name: Run performance benchmarks
        run: ./scripts/run_performance_benchmarks.sh all --report

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: perf-results-${{ github.run_id }}
          path: test/perf_benchmarks/reports/
```

### Performance Regression Detection

Track performance over time:

```bash
# Baseline (on main branch)
git checkout main
./scripts/run_performance_benchmarks.sh all > baseline_results.txt

# Feature branch
git checkout feature-branch
./scripts/run_performance_benchmarks.sh all > feature_results.txt

# Compare
diff baseline_results.txt feature_results.txt
```

---

## Troubleshooting Performance Issues

### Problem: High Latency

**Diagnosis**:
```bash
# Check system load
top -l 1 | head -3

# Check Erlang memory
erl -eval 'io:format("~p~n", [erlang:memory()]), halt().'

# Run with tracing
rebar3 ct --suite=http_endpoint_bench_SUITE --verbose
```

**Common Causes**:
1. Garbage collection pauses
2. Message queue buildup
3. ETS table contention
4. Insufficient worker pool size
5. External service (Firestore) slowness

**Fixes**:
```erlang
% 1. Tune garbage collection
erl +Mea alloc_util +hmqc off

% 2. Increase worker pool size
{action_workers, {poolboy, [
    {name, action_worker_pool},
    {worker_module, tai_action_worker},
    {size, 20},  % Increased from 10
    {max_overflow, 10}
]}}

% 3. Monitor queue depth
handler:get_message_queue_len(Pid)

% 4. Reduce ETS contention
use_partitioned_ets(partition_count, 16)
```

### Problem: Memory Leak

**Detection**:
```erlang
% Run memory stability test
bench_memory_stability(Config) ->
    MemSamples = run_for_3_minutes(),
    Growth = analyze_growth_rate(MemSamples),
    ?assert(Growth < 0.5, "Memory growth > 0.5 MB/sec").
```

**Investigation**:
```erlang
% Get heap snapshot
heapdump:dump_file("heap.dump"),

% Analyze with tools
erl_crash_dump:analyze_stack_dump("heap.dump")
```

**Common Causes**:
1. Accumulating messages in process mailbox
2. ETS tables not being garbage collected
3. Circular references
4. Binary data accumulation

### Problem: Throughput Degradation

**Analysis**:
```bash
# Profile operation
rebar3 ct --suite=governor_perf_bench_SUITE --case=bench_signal_processing -v

# Check queue latency
poolboy:status(action_worker_pool)
% Output: {ready, 10, 0, 0, []}  % 10 ready, 0 busy, 0 overflow
```

**Solutions**:
1. Increase concurrency in test
2. Profile with `eprof` or `fprof`
3. Check for hot spots in code
4. Optimize hot paths with `-O2` compile flags

---

## Performance Tuning Checklist

- [ ] Run baseline benchmarks before changes
- [ ] Implement optimizations one at a time
- [ ] Verify each change with re-run
- [ ] Document performance impact
- [ ] Keep detailed notes of changes
- [ ] Run full suite before commit
- [ ] Verify no regressions
- [ ] Update performance targets if needed
- [ ] Archive baseline for comparison
- [ ] Brief team on changes

---

## Reference Metrics

### Hardware Assumptions (Baseline)

Benchmarks assume:
- Modern CPU (4+ cores, 2+ GHz)
- 8+ GB RAM
- SSD storage
- Network latency < 1ms (local/loopback)

### Erlang Version Impact

Performance varies by Erlang version:
- OTP 24: Baseline
- OTP 25: ~5% faster (scheduler improvements)
- OTP 26: ~10% faster (JIT compiler)
- OTP 27: ~15% faster (continued JIT improvements)

### Load Profile Assumptions

Benchmarks use:
- Uniform request distribution (no skew)
- No external service latency (mocked)
- Persistent connections where applicable
- Realistic payload sizes

---

## Additional Resources

### Erlang Documentation
- [Performance Tuning Guide](https://erlang.org/doc/efficiency_guide/index.html)
- [System Monitoring](https://erlang.org/doc/man/observer.html)
- [eprof Profiler](https://erlang.org/doc/man/eprof.html)

### Benchmarking Best Practices
- [Microbenchmark Pitfalls](https://easyperf.net/blog/2015/04/12/Micro-Benchmarking-is-Hard/)
- [Latency Matters](https://blog.acolyer.org/2015/04/03/latency-cannot-be-ignored/)
- [Performance Testing](https://www.infoq.com/articles/performance-testing-testing-strategies/)

---

**Last Updated**: 2026-01-25
**Version**: 1.0.0
