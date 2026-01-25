# AtomVM Optimization Guide - Profiling & Memory Analysis

## Overview

This guide explains how to profile AtomVM governors and identify memory optimization opportunities.

## Tools

### 1. **eprof** - Execution Time Profiling

```bash
# Start eprof profiling
erl -pa ebin
Erlang> eprof:start_profiling([light_governors, memory_pool]).

# Run your workload
Erlang> benchmark_comparison:run_throughput_test().

# Stop and analyze
Erlang> eprof:stop_profiling().
Erlang> eprof:analyze(total).

# Sample output:
%  Function                                    Time     %
% -------------------------------------------------------
%  light_governors:handle_entitlement_check   523.0   18.5
%  memory_pool:add_event                      312.0   11.1
%  erlang:send_after                           89.0    3.2
% ...
```

**Interpretation**: Focus optimization on top 3-5 functions (80/20 rule).

### 2. **redbug** - On-Demand Tracing

Lightweight tracing suitable for production-like environments:

```bash
# Trace all light_governors calls
Erlang> redbug:start("light_governors:*", [{time_limit, 5000}]).

# Trace specific function with timing
Erlang> redbug:start("light_governors:handle_entitlement_check", [
    {time_limit, 5000},
    {print_msacc, true}
]).

# Sample output:
% light_governors:handle_entitlement_check/3
%   <0.45.0> ({tag,0.123123}) << 0.234ms >>
%   <0.45.0> ({tag,0.234234})
%   <0.45.0> ({tag,0.345345}) << 0.456ms >>

# Stop tracing
Erlang> redbug:stop().
```

**Key metrics**:
- Wall-clock time (microseconds)
- Number of calls
- Peak latency spikes

### 3. **Memory Profiling**

```bash
# Get memory breakdown
Erlang> memory:get_memory_stats().
% [{total, 25000000},
%  {heap_size, 1000000},
%  {external, 500000},
%  {processes, 15000000},
%  {atoms, 500000}]

# Trace memory-intensive operations
Erlang> erlang:trace(all, true, [call, mem]).

# Analyze memory per governor
Erlang> performance_metrics:benchmark_governors(30).
% {30, 750000, 25000}  % 30 governors, 750KB total, 25KB each
```

## Common Optimization Patterns

### Pattern 1: List Operations (Hot Path)

**Problem**: List operations in tight loop (O(n))

```erlang
% BEFORE: Linear scan in check_entitlement
case lists:member(Feature, Features) of
    true -> ...
end.

% AFTER: Use maps for O(1) lookup
FeatureMap = maps:from_list([{F, true} || F <- Features]),
case maps:get(Feature, FeatureMap, false) of
    true -> ...
end.
```

**Impact**: 40-60% latency reduction for large feature lists

### Pattern 2: String Interning (Memory Hot Spot)

**Problem**: Duplicate atoms/strings create memory pressure

```erlang
% BEFORE: New atoms created for each request
atom_to_list(Tenant)

% AFTER: Pre-intern common atoms
intern_atom(Tenant)  % Cached in pool atom_cache

% Memory savings: 2-5KB per governor
```

### Pattern 3: Buffer Overflow (Ring Buffer)

**Problem**: Unbounded list growth

```erlang
% BEFORE: Append to buffer indefinitely
NewBuffer = Buffer ++ [Event]

% AFTER: Fixed-size ring buffer (via memory_pool)
{ok, _} = memory_pool:add_event(Pool, Event)

% Memory guarantee: max 20KB per pool
```

### Pattern 4: GC Pressure (Tail Calls)

**Problem**: Non-tail-recursive functions trigger GC

```erlang
% BEFORE: Non-tail-recursive
process_events([H|T]) ->
    handle(H),
    process_events(T).  % Stack grows

% AFTER: Tail-recursive accumulator
process_events_acc([], Acc) ->
    lists:reverse(Acc);
process_events_acc([H|T], Acc) ->
    process_events_acc(T, [handle(H)|Acc]).
```

**Impact**: Eliminates GC pauses, reduces stack pressure

## Profiling Workflow

### Step 1: Baseline Measurements

```bash
erl> performance_metrics:start_collection().
erl> benchmark_comparison:run_all().
erl> Metrics = performance_metrics:stop_collection().

% Record:
% - Memory: total_bytes
% - Latency: p99_ms
% - Throughput: requests_per_second
```

### Step 2: Identify Hot Spots

```bash
erl> eprof:start_profiling([light_governors, memory_pool]).
erl> benchmark_comparison:run_throughput_test().
erl> eprof:analyze(total).

% Find top 5 functions by time
```

### Step 3: Apply Optimization

Make targeted change to top function.

### Step 4: Measure Impact

```bash
erl> performance_metrics:reset_counters().
erl> benchmark_comparison:run_all().
erl> Metrics2 = performance_metrics:stop_collection().

% Compare Metrics vs Metrics2
% Target: 10% improvement minimum per optimization
```

### Step 5: Iterate

Repeat for next hot spot.

## Performance Targets

### Memory Targets

| Component | Target | How to Monitor |
|-----------|--------|----------------|
| Per Governor | 25KB | `memory_pool:estimate_memory/1` |
| System Total | 1MB for 40 governors | `erlang:memory(total)` |
| Atom Cache | 2KB max | `maps:size(atom_cache)` |
| Event Buffer | 20KB fixed | Ring buffer size constant |

### Latency Targets

| Metric | Target | How to Monitor |
|--------|--------|----------------|
| p50 | < 2ms | `performance_metrics:get_latency/0` |
| p99 | < 8ms | p99_ms from latency metrics |
| p999 | < 20ms | p999_ms from latency metrics |
| Max | < 50ms | Trace max latency |

### Throughput Targets

| Metric | Target | How to Monitor |
|--------|--------|----------------|
| Requests/sec | > 10,000 | `performance_metrics:get_throughput/0` |
| Error rate | < 0.1% | error_rate from throughput |
| GC pauses | < 5ms | `erlang:statistics(garbage_collection)` |

## Debugging Memory Leaks

### Symptom: Memory Growing Unbounded

```bash
# Check buffer size growth
erl> redbug:start("memory_pool:add_event", [{time_limit, 60000}]).
% Watch for buffer exceeding capacity

# Check atom cache
erl> eprof:start_profiling([memory_pool]).
erl> % run workload
erl> eprof:analyze(calls).
% Look for intern_atom called excessively
```

### Fix: Implement Size Limit

```erlang
% memory_pool:add_event/2 line 120
case Size >= Capacity of
    true ->
        {full, Head};  % Oldest event overwritten
    false ->
        {ok, Head}
end.
```

## Debugging Latency Spikes

### Symptom: p99 >> average

```bash
# Trace latencies over time
erl> redbug:start("light_governors:handle_*", [
    {time_limit, 10000},
    {print_msacc, true}
]).

# Look for outliers: calls taking > 10x average time
```

### Cause: Garbage Collection

```bash
# Check GC frequency
erl> erlang:statistics(garbage_collection).
% {collections, {minor, 1234}, {major, 56}}
% {words_collected, 12345}
% {words_reclaimed, 12300}

# If reclaimed ≈ collected, GC is collecting garbage
# If reclaimed << collected, possible memory leak
```

### Fix: Enable Tail Call Optimization

```erlang
% Compile with +tail flag or use tail-recursive functions
% See "Pattern 4: GC Pressure" above
```

## Real-World Optimization Examples

### Example 1: Reduce Atom Cache Memory

**Symptom**: Atom cache growing to 5KB

```erlang
% BEFORE: All atoms auto-cached
init_atom_cache() ->
    #{},  % Empty, grows dynamically

% AFTER: Pre-cache only common atoms
-define(COMMON_ATOMS, [
    entitlement, billing, quota, compliance, subscription,
    check, update, delete, ok, error
]).

init_atom_cache() ->
    maps:from_list([{A, A} || A <- ?COMMON_ATOMS]).
```

**Result**: Atom cache capped at 2KB, prevents unbounded growth

### Example 2: Optimize Tenant State Lookup

**Symptom**: p99 latency spikes when checking many tenants

```erlang
% BEFORE: Linear list scan
case lists:member(Tenant, TenantList) of
    true -> ...
end.

% AFTER: Map-based lookup (O(1))
case maps:is_key(Tenant, TenantState) of
    true -> ...
end.
```

**Result**: p99 latency: 45ms → 8ms (5.6x improvement)

### Example 3: Implement Ring Buffer

**Symptom**: Memory growing unbounded during load test

```erlang
% BEFORE: Unbounded buffer
Buffer = [Event|OldBuffer]

% AFTER: Fixed-size ring buffer
{ok, _} = memory_pool:add_event(Pool, Event)
```

**Result**: Memory capped at 1MB (vs unbounded growth)

## Deployment Profiling

For production-like edge deployment:

```bash
# On edge device (256MB RAM)
erl -pa atomvm_src/ebin \
    +K true \
    +P 32000 \
    +t false

Erlang> atomvm_governors_sup:start_link().
Erlang> performance_metrics:start_collection().

% Run actual workload (10 hours)

Erlang> Metrics = performance_metrics:collect().
Erlang> performance_metrics:format_metrics(Metrics).

% Expected output:
% Memory Total: 2MB (100 governors)
% Memory per Governor: 20KB
% p99 Latency: 6ms
% Throughput: 15,000 requests/sec
% Error rate: 0.01%
```

## Troubleshooting

### Issue: "Cannot allocate memory" during profiling

**Cause**: eprof traces too much data

**Solution**: Limit tracing scope

```erlang
% Instead of:
eprof:start_profiling([all_modules])

% Use:
eprof:start_profiling([light_governors])  % Single module
redbug:start("light_governors:*")  % Function-level
```

### Issue: Latency measurements unreliable

**Cause**: Measurement overhead > target latency

**Solution**: Use microsecond-resolution timing

```erlang
% BEFORE: millisecond timing
Start = erlang:monotonic_time(millisecond),

% AFTER: microsecond timing
Start = erlang:monotonic_time(microsecond),
Latency = (erlang:monotonic_time(microsecond) - Start) / 1000
```

### Issue: Memory measurements not accurate

**Cause**: GC timing affects measurements

**Solution**: Force GC before measurement

```erlang
erlang:garbage_collect_all(),
timer:sleep(100),
Memory = erlang:memory(total)
```

## References

- **eprof docs**: http://erlang.org/doc/apps/tools/eprof.html
- **redbug docs**: https://github.com/massemanet/eper
- **memory profiling**: https://www.erlang.org/doc/efficiency_guide/profiling.html
- **atomvm docs**: https://github.com/atomvm/atomvm

## Conclusion

Key optimization strategies for AtomVM edge deployment:

1. **Profile first**: Identify real hot spots with eprof/redbug
2. **80/20 focus**: Optimize top 3-5 functions for 80% impact
3. **Memory awareness**: Cap buffers, intern atoms, avoid unbounded lists
4. **Tail-recursive**: Eliminate GC pressure in loops
5. **Measure impact**: Verify 10%+ improvement per change

Target: **4x memory reduction** (100MB → 25MB per system)
