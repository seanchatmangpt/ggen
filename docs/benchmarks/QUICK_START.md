# ggen Benchmarks - Quick Start Guide

Get up and running with ggen performance benchmarks in 5 minutes.

## Prerequisites

```bash
# Ensure Rust is installed
rustc --version

# Verify cargo is available
cargo --version
```

## 1. Run Your First Benchmark

```bash
# Run all benchmarks (takes ~5-10 minutes)
cargo bench --bench clnrm_benchmarks

# Run a specific benchmark category
cargo bench --bench clnrm_benchmarks marketplace_search
```

**Expected Output**:
```
marketplace_search_deterministic/simple_query/1000
                        time:   [42.3 ms 43.1 ms 43.9 ms]
                        thrpt:  [23.2 Kelem/s 23.6 Kelem/s 24.0 Kelem/s]
```

## 2. View Results

```bash
# Open HTML report in browser
open target/criterion/report/index.html

# View summary in terminal
cat target/criterion/marketplace_search_deterministic/simple_query/1000/report/index.txt
```

## 3. Run Performance Tests

```bash
# Run integration tests with performance thresholds
cargo test --release --test marketplace_tests_main -- performance

# Run specific performance test
cargo test --release test_marketplace_search_performance
```

**Expected Output**:
```
test test_marketplace_search_performance ... ok
test test_version_resolution_performance ... ok
test test_lifecycle_phase_execution_performance ... ok
```

## 4. Compare Performance

```bash
# Save baseline
cargo bench --bench clnrm_benchmarks -- --save-baseline main

# Make your changes...

# Compare against baseline
cargo bench --bench clnrm_benchmarks -- --baseline main
```

**Expected Output**:
```
marketplace_search_deterministic/simple_query/1000
                        time:   [42.3 ms 43.1 ms 43.9 ms]
                        change: [-2.3% +0.5% +3.4%] (p = 0.42 > 0.05)
                        No change in performance detected.
```

## 5. Quick Performance Check

```bash
# Run baseline operations only (~30 seconds)
cargo bench --bench clnrm_benchmarks baseline_operations

# Run marketplace benchmarks only (~2 minutes)
cargo bench --bench clnrm_benchmarks marketplace

# Run lifecycle benchmarks only (~2 minutes)
cargo bench --bench clnrm_benchmarks lifecycle
```

## Common Use Cases

### Use Case 1: Pre-commit Performance Check

```bash
# Quick baseline check before committing
cargo bench --bench clnrm_benchmarks baseline_operations -- --quick
```

**Time**: ~30 seconds

### Use Case 2: Feature Performance Validation

```bash
# 1. Save baseline
cargo bench --bench clnrm_benchmarks -- --save-baseline before-feature

# 2. Implement feature

# 3. Compare
cargo bench --bench clnrm_benchmarks -- --baseline before-feature
```

**Interpretation**:
- Green (no change): < 5% difference
- Yellow (minor change): 5-10% difference
- Red (regression): > 10% difference

### Use Case 3: Stress Testing

```bash
# Run stress tests to validate under load
cargo bench --bench clnrm_benchmarks stress
```

**Time**: ~3-5 minutes

### Use Case 4: CI/CD Validation

```bash
# Run performance tests with thresholds (fail on regression)
cargo test --release --test marketplace_tests_main -- performance
```

**Exit Code**: 0 = pass, non-zero = performance regression detected

## Benchmark Categories Quick Reference

### Marketplace Operations
```bash
cargo bench --bench clnrm_benchmarks marketplace_search
cargo bench --bench clnrm_benchmarks marketplace_version_resolution
cargo bench --bench clnrm_benchmarks marketplace_cache_operations
```

**Measures**: Search speed, version resolution, cache performance

### Lifecycle Operations
```bash
cargo bench --bench clnrm_benchmarks lifecycle_phase_execution
cargo bench --bench clnrm_benchmarks lifecycle_cache_validation
cargo bench --bench clnrm_benchmarks lifecycle_state_persistence
```

**Measures**: Phase execution time, cache hits, state I/O

### Stress Tests
```bash
cargo bench --bench clnrm_benchmarks stress_concurrent_searches
cargo bench --bench clnrm_benchmarks stress_high_volume_cache
cargo bench --bench clnrm_benchmarks stress_large_registry
```

**Measures**: Concurrent operation throughput, high-load behavior

### Regression Detection
```bash
cargo bench --bench clnrm_benchmarks baseline_operations
```

**Measures**: Core operation baselines for comparison

### Cleanroom Determinism
```bash
cargo bench --bench clnrm_benchmarks cleanroom_determinism
```

**Measures**: Deterministic environment overhead and consistency

## Performance Thresholds

### ✅ Green (Good)
- Marketplace Search (1K packages): < 50ms
- Phase Execution: < 200ms
- Cache Validation (100 entries): < 50ms

### ⚠️ Yellow (Warning)
- 10% above green thresholds
- Review but not blocking

### ❌ Red (Regression)
- 20% above green thresholds
- Performance issue - investigate immediately

## Interpreting Results

### Time Measurements

```
time:   [42.3 ms 43.1 ms 43.9 ms]
         ↑        ↑        ↑
         p25      p50      p75
```

- **p50 (median)**: Typical performance
- **p25/p75**: Performance variance
- Smaller variance = more consistent performance

### Throughput

```
thrpt:  [23.2 Kelem/s 23.6 Kelem/s 24.0 Kelem/s]
```

- Elements (packages/operations) processed per second
- Higher is better

### Change Detection

```
change: [-2.3% +0.5% +3.4%] (p = 0.42 > 0.05)
         ↑        ↑      ↑      ↑
         min      mean   max    significance
```

- **p < 0.05**: Statistically significant change
- **p > 0.05**: No significant change detected

## Quick Debugging

### Benchmark Running Slowly

```bash
# Run with reduced sample size
cargo bench --bench clnrm_benchmarks -- --sample-size 10

# Run in quick mode (less accurate but faster)
cargo bench --bench clnrm_benchmarks -- --quick
```

### Non-Deterministic Results

```bash
# Verify cleanroom is working
cargo test test_deterministic_performance
```

### Want More Details

```bash
# Run with verbose output
cargo bench --bench clnrm_benchmarks -- --verbose

# Profile with flamegraph
cargo flamegraph --bench clnrm_benchmarks
```

## Next Steps

1. **Read full documentation**: [PERFORMANCE_BENCHMARKS.md](PERFORMANCE_BENCHMARKS.md)
2. **Review baseline metrics**: [BASELINE_METRICS.md](BASELINE_METRICS.md)
3. **Add custom benchmarks**: See "Adding New Benchmarks" in main docs
4. **Set up CI/CD**: See "CI/CD Integration" section

## Common Commands Cheat Sheet

```bash
# Quick baseline check (30s)
cargo bench --bench clnrm_benchmarks baseline_operations -- --quick

# Full benchmark suite (5-10m)
cargo bench --bench clnrm_benchmarks

# Marketplace only (2m)
cargo bench --bench clnrm_benchmarks marketplace

# Lifecycle only (2m)
cargo bench --bench clnrm_benchmarks lifecycle

# Stress tests (3-5m)
cargo bench --bench clnrm_benchmarks stress

# Performance tests with thresholds
cargo test --release performance

# Save baseline
cargo bench -- --save-baseline main

# Compare with baseline
cargo bench -- --baseline main

# View HTML reports
open target/criterion/report/index.html
```

## Getting Help

- **Full Documentation**: [README.md](README.md)
- **Issue Tracker**: https://github.com/seanchatmangpt/ggen/issues
- **Performance Metrics**: [BASELINE_METRICS.md](BASELINE_METRICS.md)

---

**Time to Complete**: 5 minutes
**Difficulty**: Beginner-friendly
**Prerequisites**: Rust toolchain installed
