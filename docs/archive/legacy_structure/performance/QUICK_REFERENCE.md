<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Performance Benchmarking Quick Reference](#performance-benchmarking-quick-reference)
  - [Running Benchmarks](#running-benchmarks)
    - [All Benchmarks](#all-benchmarks)
    - [Individual Suites](#individual-suites)
    - [Specific Benchmarks](#specific-benchmarks)
  - [Key Metrics to Monitor](#key-metrics-to-monitor)
    - [Consensus (HiveQueen)](#consensus-hivequeen)
    - [Coordination (SwarmCoordinator)](#coordination-swarmcoordinator)
    - [Primitives (Domain)](#primitives-domain)
  - [Performance Targets (80/20 Focus)](#performance-targets-8020-focus)
    - [Critical Metrics (Must Meet)](#critical-metrics-must-meet)
    - [Important Metrics (Should Meet)](#important-metrics-should-meet)
  - [Bottleneck Identification](#bottleneck-identification)
    - [CPU-Bound Indicators](#cpu-bound-indicators)
    - [Memory-Bound Indicators](#memory-bound-indicators)
    - [Lock Contention Indicators](#lock-contention-indicators)
  - [Quick Optimization Checklist](#quick-optimization-checklist)
    - [Before Optimizing](#before-optimizing)
    - [Priority 1: Hash-Based Conflict Detection](#priority-1-hash-based-conflict-detection)
    - [Priority 2: Parallel Agent Spawning](#priority-2-parallel-agent-spawning)
    - [Priority 3: Epoch-Based Snapshots](#priority-3-epoch-based-snapshots)
    - [After Optimizing](#after-optimizing)
  - [Interpreting Results](#interpreting-results)
    - [Criterion Output](#criterion-output)
    - [Performance Categories](#performance-categories)
  - [Flamegraph Profiling](#flamegraph-profiling)
    - [Generate Flamegraph](#generate-flamegraph)
    - [Interpreting Flamegraphs](#interpreting-flamegraphs)
  - [Common Issues & Solutions](#common-issues--solutions)
    - [Issue: High Variance](#issue-high-variance)
    - [Issue: Regression Not Detected](#issue-regression-not-detected)
    - [Issue: Out of Memory](#issue-out-of-memory)
  - [Environment Setup](#environment-setup)
    - [Optimal Benchmark Environment](#optimal-benchmark-environment)
    - [CI/CD Integration](#cicd-integration)
  - [Resources](#resources)
    - [Documentation](#documentation)
    - [Tools](#tools)
    - [Benchmarking Best Practices](#benchmarking-best-practices)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Performance Benchmarking Quick Reference

## Running Benchmarks

### All Benchmarks
```bash
# Run everything
cargo bench --workspace

# Generate HTML reports
cargo criterion
open target/criterion/report/index.html
```

### Individual Suites
```bash
# Hive coordination (consensus, agents, conflicts)
cargo bench --bench hive_coordination

# Swarm coordination (pipelines, stages, timeouts)
cargo bench --bench swarm_coordination

# Low-level primitives (lock-free, scheduling, proposals)
cargo bench --bench swarm_primitives
```

### Specific Benchmarks
```bash
# Run only consensus latency tests
cargo bench --bench hive_coordination -- consensus_latency

# Run only snapshot read tests
cargo bench --bench swarm_primitives -- snapshot_reads

# Run with specific sample size
cargo bench -- --sample-size 50
```

## Key Metrics to Monitor

### Consensus (HiveQueen)
- **Latency**: `consensus_latency` (p50, p95, p99)
- **Throughput**: `orchestration_throughput`
- **Conflicts**: `conflict_detection`
- **Agents**: `agent_spawning`

### Coordination (SwarmCoordinator)
- **Latency**: `pipeline_execution` (p50, p95, p99)
- **Concurrency**: `concurrent_execution`
- **Timeout**: `timeout_overhead`
- **Memory**: `swarm_memory`

### Primitives (Domain)
- **Lock-Free**: `snapshot_reads` (< 1μs target)
- **Scheduling**: `priority_scheduling` (< 1μs target)
- **Aggregation**: `commutative_aggregation`
- **Conflicts**: `conflict_detection`

## Performance Targets (80/20 Focus)

### Critical Metrics (Must Meet)
| Metric | Target | Benchmark |
|--------|--------|-----------|
| Consensus p50 | < 100ms | `consensus_latency/10` |
| Consensus p99 | < 500ms | `consensus_latency/10` |
| Agent Spawn | < 50ms | `agent_spawning/8` |
| Snapshot Read | < 1μs | `snapshot_reads/100` |
| Throughput | > 100 ops/sec | `orchestration_throughput/50` |

### Important Metrics (Should Meet)
| Metric | Target | Benchmark |
|--------|--------|-----------|
| Pipeline p50 | < 200ms | `pipeline_execution/6` |
| Conflict Detection | < 200ms | `conflict_detection/20` |
| Memory/100 agents | < 10MB | `swarm_memory/100` |

## Bottleneck Identification

### CPU-Bound Indicators
- High latency in `consensus_latency`
- Poor scaling in `parallel_agents`
- Slow `conflict_detection`

**Fix**: Parallelize with rayon, optimize algorithms

### Memory-Bound Indicators
- High values in `memory_overhead`
- Slow `swarm_memory` allocation
- GC pauses

**Fix**: Implement memory pooling, reduce allocations

### Lock Contention Indicators
- Slow `snapshot_writes`
- High latency in `semaphore_coordination`
- Poor concurrent scaling

**Fix**: Use lock-free primitives, reduce critical sections

## Quick Optimization Checklist

### Before Optimizing
- [ ] Run baseline benchmarks: `cargo bench --workspace`
- [ ] Save baseline: `cp -r target/criterion target/criterion-baseline`
- [ ] Identify top 3 bottlenecks from reports

### Priority 1: Hash-Based Conflict Detection
- [ ] Replace O(n²) with O(n) hash maps
- [ ] Expected: 40-60% latency reduction
- [ ] Validate: `cargo bench --bench hive_coordination -- conflict_detection`

### Priority 2: Parallel Agent Spawning
- [ ] Use rayon for parallel creation
- [ ] Expected: 3-5x speedup
- [ ] Validate: `cargo bench --bench hive_coordination -- agent_spawning`

### Priority 3: Epoch-Based Snapshots
- [ ] Add crossbeam-epoch dependency
- [ ] Expected: 20-30% read improvement
- [ ] Validate: `cargo bench --bench swarm_primitives -- snapshot_reads`

### After Optimizing
- [ ] Run benchmarks: `cargo bench --workspace`
- [ ] Compare: `cargo criterion --baseline baseline`
- [ ] Validate no regressions

## Interpreting Results

### Criterion Output
```
consensus_latency/10    time:   [95.234 ms 98.456 ms 101.234 ms]
                        change: [-12.34% -8.56% -4.23%] (p = 0.00 < 0.05)
                        Performance has improved.
```

- **Time**: [p50, mean, p99]
- **Change**: [best%, mean%, worst%] vs baseline
- **p-value**: Statistical significance (< 0.05 = significant)

### Performance Categories
- **Excellent**: Change < -20% (major improvement)
- **Good**: Change -10% to -20%
- **Acceptable**: Change -5% to -10%
- **Warning**: Change -5% to +5% (noise)
- **Regression**: Change > +5% (investigate!)

## Flamegraph Profiling

### Generate Flamegraph
```bash
# Install flamegraph
cargo install flamegraph

# Profile specific benchmark
cargo flamegraph --bench hive_coordination -- --bench consensus_latency

# Open flamegraph
open flamegraph.svg
```

### Interpreting Flamegraphs
- **Wide bars**: Hot path (optimize these!)
- **Deep stacks**: Recursion or deep call chains
- **Flat profile**: Good distribution

## Common Issues & Solutions

### Issue: High Variance
**Symptoms**: Large confidence intervals, unstable results
**Solutions**:
- Increase sample size: `--sample-size 200`
- Increase measurement time: `--measurement-time 30`
- Close other applications
- Run on dedicated machine

### Issue: Regression Not Detected
**Symptoms**: Performance worse but "No change detected"
**Solutions**:
- Increase measurement time for statistical power
- Check if noise > change magnitude
- Use `--save-baseline` and compare manually

### Issue: Out of Memory
**Symptoms**: Benchmark crashes, OOM errors
**Solutions**:
- Reduce benchmark scale (fewer iterations)
- Use `--sample-size 10` for quick tests
- Profile memory with `valgrind` or `heaptrack`

## Environment Setup

### Optimal Benchmark Environment
```bash
# Disable CPU frequency scaling
sudo cpupower frequency-set -g performance

# Disable turbo boost (for consistency)
echo 0 | sudo tee /sys/devices/system/cpu/cpufreq/boost

# Increase file descriptors
ulimit -n 4096

# Set high priority
nice -n -10 cargo bench
```

### CI/CD Integration
```yaml
# .github/workflows/benchmarks.yml
name: Benchmarks
on: [push, pull_request]
jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run benchmarks
        run: cargo bench --workspace
      - name: Store results
        uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'cargo'
          output-file-path: target/criterion/*/new/estimates.json
```

## Resources

### Documentation
- [Criterion.rs Guide](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Flamegraph Guide](https://www.brendangregg.com/flamegraphs.html)

### Tools
- **Criterion**: `cargo install cargo-criterion`
- **Flamegraph**: `cargo install flamegraph`
- **Valgrind**: `sudo apt-get install valgrind`
- **perf**: Linux performance profiler

### Benchmarking Best Practices
1. Run on consistent hardware
2. Close unnecessary applications
3. Use `--save-baseline` for comparisons
4. Benchmark realistic workloads
5. Focus on critical path (80/20)
6. Validate with real-world tests

---

**Quick Start**: Run `cargo bench --workspace` and check HTML reports at `target/criterion/report/index.html`

**Optimization Priority**: Hash-based conflicts → Parallel agents → Epoch snapshots (delivers 80% of gains)

**Success Metric**: Consensus p50 < 100ms, Agent spawn < 50ms, Snapshot read < 1μs
