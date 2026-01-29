# Erlang/OTP Benchmark Suite

Comprehensive performance benchmarking suite for Erlang/OTP applications.

## Overview

This benchmark suite provides multiple approaches to measuring Erlang/OTP application performance:

1. **Basho Bench** - Industry-standard distributed load testing framework
2. **Throughput Benchmarks** - Custom maximum throughput measurements
3. **Latency Benchmarks** - Detailed latency distribution analysis

## Benchmark Modules

### 1. Basho Bench (`basho_bench_config.config` + `call_router_bench.erl`)

**Purpose**: Industry-standard distributed benchmarking with SLA validation.

**Features**:
- 10,000 ops/sec target throughput
- 100 concurrent workers
- P50/P95/P99/P99.9 latency tracking
- Automatic SLA violation detection
- Multiple operation types (route_call, get_metrics, get_status, update_config)

**SLA Thresholds**:
- P99 latency < 1ms (1000μs)
- P95 latency < 500μs
- P50 latency < 100μs

**Running**:
```bash
# Install Basho Bench (if not already installed)
git clone https://github.com/basho/basho_bench.git
cd basho_bench
make

# Run benchmark
./basho_bench basho_bench_config.config

# View results
open tests/current/summary.png
```

**Configuration**:
- Edit `basho_bench_config.config` to adjust:
  - `{mode, {rate, N}}` - Target throughput
  - `{concurrent, N}` - Number of workers
  - `{duration, N}` - Test duration in minutes
  - `{operations, [...]}` - Operation mix percentages

### 2. Throughput Benchmark (`throughput_bench.erl`)

**Purpose**: Measure maximum sustained throughput without SLA constraints.

**Features**:
- Configurable worker count
- Warmup phase for JIT optimization
- Real-time progress monitoring
- Detailed latency percentiles
- Error rate tracking

**Running**:
```erlang
%% Compile
c(throughput_bench).

%% Run with defaults (100 workers, 60 seconds)
throughput_bench:run(#{}).

%% Custom configuration
throughput_bench:run(#{
    workers => 200,
    duration_sec => 120,
    warmup_sec => 10
}).

%% Quick measurement
throughput_bench:measure_throughput(100, 60).
```

**Example Output**:
```
=== Benchmark Results ===
Duration: 60 seconds
Total Operations: 6542341
Throughput: 109039.02 ops/sec
Error Rate: 0.00%

Latency Percentiles (μs):
  P50:  87
  P95:  234
  P99:  512
  P99.9: 1243
```

### 3. Latency Benchmark (`latency_bench.erl`)

**Purpose**: Detailed latency distribution analysis and outlier detection.

**Features**:
- Min/Max/Mean/Median/StdDev calculation
- P50/P75/P90/P95/P99/P99.9/P99.99 percentiles
- IQR-based outlier detection
- SLA validation reporting

**Running**:
```erlang
%% Compile
c(latency_bench).

%% Run benchmark with 10,000 samples
latency_bench:run(10000).

%% Measure distribution
Result = latency_bench:measure_latency_distribution(10000).

%% Find outliers (1.5x IQR multiplier)
Samples = [120, 130, 125, 5000, 140, 128],
Outliers = latency_bench:find_outliers(Samples, 1.5).
```

**Example Output**:
```
=== Latency Analysis Results ===

Summary Statistics (μs):
  Samples:     10000
  Min:         42
  Max:         8421
  Mean:        156.34
  Median:      142
  Std Dev:     234.12

Latency Percentiles (μs):
  P50:      142
  P75:      189
  P90:      267
  P95:      342
  P99:      654
  P99.9:    1234
  P99.99:   3456

Outliers (IQR method):
  Count:       127
  Percentage:  1.27%
  Top 10 worst: [8421, 7234, 6543, 5432, 4321, 3987, 3654, 3421, 3210, 3001]

SLA Validation:
  ✓ PASS P99 < 1000μs (actual: 654, threshold: 1000)
  ✓ PASS P95 < 500μs (actual: 342, threshold: 500)
```

## Performance Targets (SLOs)

Based on production-grade Erlang/OTP systems:

| Metric | Target | Critical Threshold |
|--------|--------|-------------------|
| Throughput | >10,000 ops/sec | <5,000 ops/sec |
| P50 Latency | <100μs | >500μs |
| P95 Latency | <500μs | >2ms |
| P99 Latency | <1ms | >10ms |
| Error Rate | <0.1% | >1% |
| Memory Usage | <500MB | >2GB |
| Process Count | <10,000 | >50,000 |

## Benchmark Methodology

### 1. Warmup Phase
All benchmarks include warmup to:
- Trigger JIT compilation (BEAM optimizations)
- Warm up OS page cache
- Stabilize memory allocators
- Establish steady-state performance

**Recommendation**: 5-10 second warmup for most tests.

### 2. Sample Collection
- **Unit tests**: 1,000-10,000 samples (fast feedback)
- **Integration tests**: 100,000+ samples (statistical significance)
- **Load tests**: Sustained load for 10+ minutes (detect memory leaks)

### 3. Statistical Analysis
- Report all percentiles (P50-P99.99)
- Calculate standard deviation (detect variance)
- Identify outliers (IQR method)
- Track long-tail latencies (P99.9+)

### 4. SLA Validation
Every benchmark should validate:
1. Latency SLAs (P99 < threshold)
2. Throughput SLAs (> minimum)
3. Error rate SLAs (< maximum)
4. Resource usage SLAs (memory, CPU, processes)

## Interpreting Results

### Latency Analysis

**Good Indicators**:
- P99 < 2x P95 (predictable tail latency)
- Low standard deviation (consistent performance)
- <1% outliers (stable system)

**Warning Signs**:
- P99 > 10x P50 (high variance, investigate)
- Increasing outliers over time (memory leak, resource exhaustion)
- Bimodal distribution (two performance modes, investigate why)

### Throughput Analysis

**Scalability Patterns**:
- Linear scaling: Throughput ∝ workers (ideal)
- Diminishing returns: Throughput plateaus (contention, bottleneck)
- Negative scaling: Throughput decreases (overload, thrashing)

**CPU Utilization**:
- <70%: Underutilized (increase load)
- 70-85%: Optimal (good balance)
- >85%: Near saturation (risk of latency spikes)

## Common Bottlenecks

### 1. Process Mailbox Congestion
**Symptom**: High P99 latency, increasing with load
**Solution**:
- Implement backpressure
- Use selective receive patterns
- Pool workers

### 2. ETS Contention
**Symptom**: Throughput plateaus despite increasing workers
**Solution**:
- Use read-optimized ETS options (`{read_concurrency, true}`)
- Shard data across multiple ETS tables
- Consider `persistent_term` for read-heavy data

### 3. Binary Memory Management
**Symptom**: Gradual memory increase, GC pauses
**Solution**:
- Use binary references (not copies)
- Leverage sub-binary matching
- Monitor refc binaries

### 4. Scheduler Contention
**Symptom**: Low CPU utilization despite load
**Solution**:
- Reduce process migration (`+stbt db`)
- Use dirty schedulers for CPU-bound work
- Balance work across schedulers

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Performance Benchmarks

on:
  push:
    branches: [main]
  pull_request:

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
          rebar3-version: '3.22'

      - name: Compile
        run: rebar3 compile

      - name: Run Benchmarks
        run: |
          ./bench/run_all_benchmarks.sh

      - name: Upload Results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: bench_results/
```

### Performance Regression Detection

```bash
#!/bin/bash
# Compare current results with baseline

BASELINE_P99=1000  # μs
CURRENT_P99=$(grep "P99" bench_results/latest.txt | awk '{print $2}')

if [ "$CURRENT_P99" -gt "$BASELINE_P99" ]; then
    echo "❌ Performance regression detected!"
    echo "P99 latency: ${CURRENT_P99}μs (baseline: ${BASELINE_P99}μs)"
    exit 1
else
    echo "✅ Performance within SLA"
fi
```

## Best Practices

1. **Run benchmarks on dedicated hardware** - Avoid noisy neighbors
2. **Disable frequency scaling** - Use `performance` CPU governor
3. **Pin BEAM to cores** - Reduce context switching
4. **Use large memory pages** - Improve TLB efficiency
5. **Monitor system metrics** - CPU, memory, disk I/O during benchmarks
6. **Repeat measurements** - Average multiple runs for reliability
7. **Version control results** - Track performance over time

## Troubleshooting

### Benchmark Won't Start

**Check**:
1. All dependencies compiled: `rebar3 compile`
2. Application started: `application:ensure_all_started(your_app)`
3. Correct node name in distributed benchmarks

### Inconsistent Results

**Causes**:
- Insufficient warmup period
- Background processes interfering
- Thermal throttling (laptop)
- Swapping to disk (insufficient RAM)

**Solutions**:
- Increase warmup duration
- Close background applications
- Use desktop/server hardware
- Monitor system metrics (`htop`, `vmstat`)

### Low Throughput

**Check**:
1. Worker count vs CPU cores
2. Process pool saturation
3. ETS contention
4. Network latency (distributed benchmarks)
5. Disk I/O (if using Mnesia/DETS)

## Further Reading

- [Basho Bench Documentation](https://github.com/basho/basho_bench)
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/users_guide.html)
- [BEAM Performance Tuning](https://www.erlang.org/doc/man/erl.html#+sbt)
- [ETS Optimization](https://www.erlang.org/doc/man/ets.html#concurrency)

## Related Tools

- **Benchee** (Elixir) - Microbenchmarking
- **Tsung** - Distributed load testing
- **vmstats** - VM statistics collection
- **fprof/eprof** - Profiling tools

## Support

For issues or questions:
1. Check benchmark logs in `bench_results/`
2. Review system metrics during test
3. Consult Erlang documentation
4. File an issue with reproducible example
