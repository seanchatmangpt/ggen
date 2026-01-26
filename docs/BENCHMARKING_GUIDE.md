# ggen Comprehensive Benchmarking Guide

## Overview

The ggen benchmarking system provides comprehensive performance measurement, SLO tracking, and regression detection across the entire project. It's designed to catch performance regressions early and maintain production-ready performance standards.

## SLO Targets (Service Level Objectives)

### Build Performance
- **First Build**: ≤ 15 seconds (complete clean build)
- **Incremental Build**: ≤ 2 seconds (single file change)
- **Release Build**: ≤ 30 seconds (release optimization)
- **Test Build**: ≤ 10 seconds (test compilation)

### Runtime Performance
- **RDF Processing**: ≤ 5 seconds for 1k+ triples
- **CLI Scaffolding**: ≤ 3 seconds end-to-end
- **Generation Memory**: ≤ 100 MB peak usage
- **Binary Size**: ≤ 500 MB (stripped release binary)

### Test Performance
- **Full Test Suite**: ≤ 30 seconds (unit + integration)
- **Unit Tests Only**: ≤ 10 seconds
- **Integration Tests**: ≤ 20 seconds
- **Single-Threaded Tests**: ≤ 40 seconds (for determinism)

## Quick Start

### Run All Benchmarks

```bash
# Full benchmarking suite
cargo make slo-check

# Or manually:
bash scripts/slo_tracking_system.sh
```

### Check for Regressions

```bash
# Detect performance regressions
bash scripts/regression_detection.sh
```

### View Dashboard

```bash
# Generate HTML dashboard
bash scripts/benchmark_dashboard_generator.sh

# Open in browser
open .metrics/benchmark_dashboard.html
```

## Benchmark Categories

### 1. Build Time Benchmarks

**Location**: `benches/build_time_benchmarks.rs`, `benches/detailed_build_metrics.rs`

**Measures**:
- Clean build time from scratch
- Incremental build performance
- Memory usage during compilation
- Time per compilation unit
- Compilation unit analysis

**Run with**:
```bash
cargo bench --bench build_time_benchmarks
cargo bench --bench detailed_build_metrics
```

### 2. Memory Usage Benchmarks

**Location**: `benches/memory_usage_benchmarks.rs`, `benches/memory_profiling.rs`

**Measures**:
- Peak memory usage during operations
- Memory allocation patterns
- Memory growth over time
- Garbage collection impact
- Heap vs stack allocation breakdown

**Run with**:
```bash
cargo bench --bench memory_usage_benchmarks
cargo bench --bench memory_profiling
```

### 3. Binary Size Analysis

**Location**: `benches/binary_size_analysis.rs`

**Measures**:
- Total binary size (stripped/unstripped)
- Per-crate binary contribution
- Dead code analysis
- Symbol table size
- Optimization effectiveness

**Run with**:
```bash
cargo bench --bench binary_size_analysis
```

### 4. Test Execution Benchmarks

**Location**: `benches/test_execution_benchmarks.rs`

**Measures**:
- Total test suite execution time
- Unit test performance
- Integration test performance
- Single-threaded test time
- Parallelism efficiency

**Run with**:
```bash
cargo bench --bench test_execution_benchmarks
```

### 5. Performance Regression Detection

**Location**: `scripts/regression_detection.sh`

**Detects**:
- Build time regressions (>10% deviation)
- Test execution regressions
- Binary size increases
- Historical trend analysis

**Run with**:
```bash
bash scripts/regression_detection.sh
```

### 6. SLO Tracking System

**Location**: `scripts/slo_tracking_system.sh`

**Tracks**:
- Current performance against SLO targets
- Compliance status for all metrics
- Detailed metric collection
- Result storage and reporting

**Run with**:
```bash
bash scripts/slo_tracking_system.sh
```

## Core SLO Framework

The `core_slo_framework.rs` benchmark provides the foundation for SLO tracking:

```rust
pub struct SLOTracker {
    targets: SLOTargets,
    measurements: HashMap<String, Vec<Duration>>,
    memory_measurements: HashMap<String, Vec<usize>>,
    violations: Vec<SLOViolation>,
}
```

### Key Features:
- **Duration Tracking**: Records all performance measurements
- **Memory Tracking**: Monitors memory usage patterns
- **Violation Detection**: Flags metrics that miss SLO targets
- **Statistics Calculation**: Computes mean, min, max, percentiles
- **Reporting**: Generates compliance reports

### Usage Example:

```rust
let mut tracker = SLOTracker::new(SLOTargets::default());

// Record measurements
tracker.record_duration("build_time", Duration::from_secs(12));
tracker.record_memory("generation", 85_000_000);

// Check SLOs
tracker.check_build_time_slo(Duration::from_secs(12), false);
tracker.check_memory_slo(85_000_000);

// Generate report
println!("{}", tracker.report_violations());
```

## Monitoring and Trending

### Baseline Management

Baselines are stored in `.metrics/baselines/`:
```
.metrics/
├── baselines/
│   ├── first_build_time.baseline
│   ├── test_execution_time.baseline
│   └── binary_size_mb.baseline
├── results/
│   ├── first_build_YYYYMMDD_HHMMSS.txt
│   ├── test_execution_YYYYMMDD_HHMMSS.txt
│   └── binary_size_YYYYMMDD_HHMMSS.txt
└── history/
    └── metrics_history.json
```

### Trend Analysis

The regression detection system analyzes historical trends:

```bash
# Analyzes last 5 measurements
# Detects improving vs degrading trends
# Alerts on significant regressions (>10%)
bash scripts/regression_detection.sh
```

### Historical Data

All measurements are timestamped and stored:
- **Timestamp**: Unix timestamp of measurement
- **Metric**: Name of measured performance metric
- **Value**: Actual measurement value
- **Unit**: Measurement unit (seconds, bytes, etc.)
- **SLO Target**: Target value for SLO comparison
- **Status**: PASS or FAIL against SLO

## Dashboard and Visualization

The benchmark dashboard provides real-time visualization:

```bash
# Generate interactive HTML dashboard
bash scripts/benchmark_dashboard_generator.sh

# View the dashboard
open .metrics/benchmark_dashboard.html
```

### Dashboard Features:
- **Metric Cards**: Quick status overview for all key metrics
- **Trend Charts**: Historical performance visualization
- **SLO Table**: Compliance status and deviation percentage
- **Responsive Design**: Works on desktop, tablet, mobile

## Integration with CI/CD

### GitHub Actions Integration

Add to your CI pipeline:

```yaml
- name: Run SLO checks
  run: bash scripts/slo_tracking_system.sh

- name: Detect regressions
  run: bash scripts/regression_detection.sh

- name: Generate dashboard
  run: bash scripts/benchmark_dashboard_generator.sh
```

### Makefile Targets

Available targets in `Makefile.toml`:

```bash
# Run benchmarks
cargo make bench

# Check SLO compliance
cargo make slo-check

# Full benchmark suite
cargo make benchmark-full

# Quick performance check
cargo make quick-perf
```

## Performance Optimization Tips

### Build Time Optimization
1. **Use `cargo make` targets** - Enforces caching and parallelization
2. **Check incremental builds** - Watch for regressions with small changes
3. **Profile compilation** - Use `cargo build -Z timings` to identify slow crates
4. **Reduce feature matrix** - Only enable needed features during development

### Memory Optimization
1. **Monitor allocations** - Track peak memory during operations
2. **Use release builds** - Debug builds use more memory
3. **Analyze heap patterns** - Look for memory leaks or growth
4. **Profile with `perf`** - Detailed memory profiling

### Binary Size Optimization
1. **Strip symbols** - Use `strip` on release binaries
2. **Enable LTO** - Link-time optimization reduces size
3. **Analyze dead code** - Use `cargo bloat` to find unused code
4. **Compress distribution** - Consider compression for downloads

### Test Performance
1. **Parallelize tests** - Use `--test-threads` appropriately
2. **Run fast tests first** - Fail fast on quick checks
3. **Mock expensive operations** - Use real collaborators wisely
4. **Profile test fixtures** - Optimize test setup time

## Troubleshooting

### Benchmark Timeouts

If benchmarks timeout (timeout enforced by Makefile):

```bash
# Increase timeout manually
timeout 60s cargo bench --bench build_time_benchmarks

# Or run without timeout
cargo bench --bench build_time_benchmarks -- --verbose
```

### SLO Violations

When SLOs are exceeded:

1. **Identify the metric** - Which SLO is violated?
2. **Check baseline** - Is this a regression or new baseline?
3. **Profile the operation** - Use `perf`, `flamegraph`, or `cargo build -Z timings`
4. **Make targeted optimization** - Fix the specific bottleneck
5. **Re-verify** - Confirm SLO is met after fix

### Missing Metrics

If metrics aren't being collected:

```bash
# Verify metrics directory
ls -la .metrics/

# Check results
ls -la .metrics/results/

# Check baselines
ls -la .metrics/baselines/

# Run collection manually
bash scripts/slo_tracking_system.sh -v
```

## Advanced Configuration

### Custom SLO Targets

Modify in `scripts/slo_tracking_system.sh`:

```bash
# Adjust these values for your project
SLO_FIRST_BUILD_SECS=15
SLO_INCREMENTAL_BUILD_SECS=2
SLO_RDF_PROCESSING_SECS=5
SLO_GENERATION_MEMORY_MB=100
SLO_CLI_SCAFFOLDING_SECS=3
SLO_BINARY_SIZE_MB=500
```

### Regression Threshold

Adjust sensitivity in `scripts/regression_detection.sh`:

```bash
# Alert on >10% regression (or modify)
REGRESSION_THRESHOLD=10
```

### Benchmark Configuration

Modify Criterion settings in benchmark files:

```rust
let mut group = c.benchmark_group("build_metrics");
group.measurement_time(Duration::from_secs(10));
group.sample_size(50);
// ... rest of benchmark
```

## Storage and Archival

Performance data is stored in memory under:
```
swarm/benchmarking/comprehensive-suite
```

Structure:
```json
{
  "suite_name": "comprehensive-suite",
  "version": "1.0.0",
  "created": "2026-01-26T12:34:56Z",
  "slo_targets": {
    "first_build_secs": 15,
    "incremental_build_secs": 2,
    "rdf_processing_secs": 5,
    "generation_memory_mb": 100,
    "binary_size_mb": 500
  },
  "benchmark_files": [
    "core_slo_framework",
    "detailed_build_metrics",
    "test_execution_benchmarks",
    "binary_size_analysis",
    "memory_usage_benchmarks"
  ],
  "monitoring_scripts": [
    "slo_tracking_system",
    "regression_detection",
    "benchmark_dashboard_generator",
    "build_perf_monitor",
    "memory_monitor"
  ],
  "last_run": "2026-01-26T12:34:56Z",
  "status": "active"
}
```

## References

- [Criterion.rs Documentation](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [cargo-flamegraph](https://github.com/flamegraph-rs/flamegraph)
- [cargo-bloat](https://github.com/RazrFalcon/cargo-bloat)

## Support

For questions or issues with benchmarking:

1. Check existing results: `ls -la .metrics/`
2. Review logs: `cat .metrics/results/*.txt`
3. Test manually: Run specific benchmark with `-v` flag
4. Check CI logs: GitHub Actions workflow runs

## Best Practices

1. **Run benchmarks regularly** - Part of pre-commit/CI
2. **Track trends** - Monitor over weeks/months
3. **Document changes** - Note code changes affecting performance
4. **Review regressions** - Investigate all violations
5. **Set realistic targets** - SLOs should be achievable yet challenging
6. **Automate monitoring** - Use CI/CD for continuous tracking
7. **Profile before optimizing** - Measure before making changes
8. **Test on target hardware** - Run benchmarks on production-like systems

---

**Version**: 1.0.0
**Last Updated**: 2026-01-26
**Maintainer**: ggen performance team
