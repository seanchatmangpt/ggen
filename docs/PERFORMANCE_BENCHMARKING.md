# Marketplace Performance Benchmarking Guide

## Overview

The GGEN marketplace includes a comprehensive performance benchmarking suite that measures and analyzes the performance of all marketplace commands across various dataset sizes and usage patterns.

## Quick Start

### Run Quick Benchmarks (5-10 minutes)

```bash
./tests/benchmarks/quick_benchmark.sh
```

### Run Full Benchmark Suite (20-30 minutes)

```bash
./tests/benchmarks/run_benchmarks.sh
```

### View Results

```bash
# Open HTML report with interactive charts
open target/criterion/report/index.html

# View markdown summary
cat tests/benchmarks/results/performance_report_*.md
```

## Performance Goals

All marketplace commands are benchmarked against these performance targets:

| Command Type | Target Latency | Target Memory | Min Success Rate |
|--------------|----------------|---------------|------------------|
| **Interactive** (search, compare) | < 500ms | < 100MB | > 99% |
| **Reports** (export, maturity assessment) | < 5s | < 100MB | > 99% |

## Benchmark Categories

### 1. Search Performance

Tests query performance across dataset sizes:

- **Text Search**: Keyword-based search
- **Category Filter**: Filter by package category
- **Tag Search**: Search by tags
- **Complex Search**: Multi-criteria search with filters

**Dataset Sizes**: 10, 100, 1000 packages

**Performance Targets**:
- 10 packages: < 10ms
- 100 packages: < 50ms
- 1000 packages: < 500ms

### 2. Maturity Assessment

Measures time to calculate package maturity scores:

- **Single Package**: Individual assessment
- **Batch Assessment**: Sequential processing
- **Parallel Assessment**: Concurrent processing with rayon

**Dataset Sizes**: 1, 10, 100 packages

**Performance Targets**:
- Single package: < 50ms
- 10 packages: < 500ms
- 100 packages: < 5s

### 3. Export Performance

Tests export time and file size for different formats:

- **CSV Export**: Comma-separated values
- **JSON Export**: Pretty-printed JSON
- **HTML Export**: Styled HTML table
- **Markdown Export**: Markdown table

**Dataset Sizes**: 10, 100 packages

**Performance Targets**:
- CSV (100 packages): < 100ms
- JSON (100 packages): < 200ms
- HTML (100 packages): < 500ms
- Markdown (100 packages): < 200ms

### 4. Comparison Performance

Measures package comparison time:

- **Two Package Comparison**: Standard comparison
- **Sequential Comparisons**: Multiple comparisons
- **Detailed Comparison**: Full comparison with all metrics

**Performance Target**: < 100ms per comparison

### 5. Recommendation Engine

Tests recommendation generation and ranking accuracy:

- **Basic Recommendations**: Preference-based filtering
- **ML Recommendations**: Machine learning-based ranking
- **Ranking Accuracy**: Validates recommendation quality

**Dataset Sizes**: 10, 100 packages

**Performance Targets**:
- 10 packages: < 50ms
- 100 packages: < 500ms

### 6. Memory Usage Analysis

Analyzes memory consumption patterns:

- **Package Loading**: Memory during data loading
- **Search & Filter**: Memory during search operations

**Dataset Sizes**: 100, 1000 packages

**Memory Target**: < 100MB peak usage

### 7. End-to-End Workflows

Complete workflow benchmarks:

- **Search → Assess → Export**: Full discovery workflow
- **Recommend → Compare → Export**: Full recommendation workflow

**Performance Target**: < 5s for complete workflow

## Understanding Benchmark Results

### Criterion Statistics

Each benchmark provides:

- **Mean**: Average execution time
- **Median**: Middle value (less affected by outliers)
- **Std Dev**: Variation in measurements
- **P95**: 95th percentile (95% of requests faster than this)
- **P99**: 99th percentile (tail latency)

### Performance Analysis

The analysis system classifies issues by severity:

- **🔴 Critical**: > 2x over goal
- **🟠 High**: 1.5-2x over goal
- **🟡 Medium**: 1.2-1.5x over goal
- **🔵 Low**: 1.0-1.2x over goal
- **ℹ️ Info**: Meeting goals

### Optimization Recommendations

For each performance issue, the analysis provides:

1. **Issue Description**: What's slow and by how much
2. **Current vs Target**: Actual vs expected performance
3. **Specific Recommendations**: Actionable optimization steps
4. **Expected Impact**: Estimated improvement percentage

## Running Specific Benchmarks

```bash
# Search benchmarks only
cargo bench --bench marketplace_performance -- "search"

# Maturity assessment benchmarks only
cargo bench --bench marketplace_performance -- "maturity"

# Export benchmarks only
cargo bench --bench marketplace_performance -- "export"

# Comparison benchmarks only
cargo bench --bench marketplace_performance -- "comparison"

# Recommendation benchmarks only
cargo bench --bench marketplace_performance -- "recommendation"

# Memory benchmarks only
cargo bench --bench marketplace_performance -- "memory"

# End-to-end benchmarks only
cargo bench --bench marketplace_performance -- "e2e"
```

## Baseline Tracking

Save and compare performance over time:

```bash
# Save current performance as baseline
cargo bench --bench marketplace_performance -- --save-baseline v1.0.0

# Compare against saved baseline
cargo bench --bench marketplace_performance -- --baseline v1.0.0

# List all saved baselines
ls target/criterion/*/base
```

## Performance Optimization Workflow

1. **Run Benchmarks**: Execute the full benchmark suite
2. **Review Results**: Check the generated performance report
3. **Identify Bottlenecks**: Focus on Critical and High severity issues
4. **Apply Optimizations**: Implement recommended fixes
5. **Re-run Benchmarks**: Verify improvements
6. **Save Baseline**: Track progress with versioned baselines

## Common Optimization Patterns

### Search Operations

When search performance is slow:

✅ **Implement full-text search indexing**
```rust
// Use tantivy for fast full-text search
use tantivy::{Index, schema::*};
```

✅ **Add result pagination**
```rust
fn search_packages(query: &str, page: usize, page_size: usize) -> Vec<Package>
```

✅ **Cache frequent queries**
```rust
use moka::future::Cache;
let cache: Cache<String, Vec<Package>> = Cache::new(1000);
```

### Maturity Assessment

When maturity calculations are slow:

✅ **Parallelize with rayon**
```rust
use rayon::prelude::*;
packages.par_iter().map(|p| assess_maturity(p)).collect()
```

✅ **Cache intermediate results**
```rust
// Cache GitHub API responses
let maturity_cache: Cache<String, MaturityScore> = Cache::new(10000);
```

### Export Operations

When exports are slow:

✅ **Stream data instead of loading all in memory**
```rust
fn export_to_csv_stream(packages: impl Iterator<Item = Package>) -> Result<()>
```

✅ **Use buffered writes**
```rust
use std::io::BufWriter;
let writer = BufWriter::new(file);
```

### Memory Optimization

When memory usage is high:

✅ **Use iterators instead of collecting**
```rust
// Instead of: packages.iter().filter(...).collect::<Vec<_>>()
packages.iter().filter(...).take(10)
```

✅ **Release allocations eagerly**
```rust
{
    let large_data = load_packages();
    process(large_data);
} // large_data dropped here
```

## CI/CD Integration

Add performance regression detection to your CI pipeline:

```yaml
# .github/workflows/performance.yml
name: Performance Benchmarks

on:
  pull_request:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run Benchmarks
        run: |
          cargo bench --bench marketplace_performance -- --save-baseline pr-${{ github.event.number }}

      - name: Compare Against Main
        run: |
          git checkout main
          cargo bench --bench marketplace_performance -- --save-baseline main
          git checkout -

      - name: Detect Regressions
        run: |
          cargo bench --bench marketplace_performance -- --baseline main

      - name: Upload Results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: target/criterion/
```

## Interpreting HTML Reports

The Criterion HTML report (`target/criterion/report/index.html`) provides:

### Overview Page

- **Summary Table**: All benchmarks with mean/median/std dev
- **Regression Warnings**: Automatic detection of performance regressions
- **Throughput Charts**: Operations per second

### Individual Benchmark Pages

- **PDF Plot**: Probability density function of measurements
- **Regression Plot**: Performance over time
- **Violin Plot**: Distribution of measurements
- **Comparison Chart**: vs baseline if available

### Key Metrics to Watch

1. **Mean Time**: Should be below target
2. **P95/P99**: Tail latencies should be reasonable (< 2x mean)
3. **Std Dev**: Low variance indicates consistent performance
4. **Throughput**: Higher is better

## Performance Monitoring Dashboard

Track performance trends over time:

```bash
# After each release
git tag v1.2.3
cargo bench --bench marketplace_performance -- --save-baseline v1.2.3

# Generate trend report
./scripts/generate_performance_trends.sh
```

## Troubleshooting

### Benchmarks Take Too Long

Reduce sample size for faster results:

```bash
cargo bench --bench marketplace_performance -- --sample-size 10
```

### Inconsistent Results

Increase measurement time for more stable results:

```bash
cargo bench --bench marketplace_performance -- --measurement-time 30
```

### Out of Memory Errors

Close other applications and ensure sufficient RAM (8GB+ recommended).

### Compilation Errors

Ensure all dependencies are installed:

```bash
cargo clean
cargo build --release
cargo bench --bench marketplace_performance
```

## Advanced Configuration

Customize benchmark behavior by editing `tests/benchmarks/marketplace_performance.rs`:

```rust
// Adjust measurement time
group.measurement_time(Duration::from_secs(10));

// Change sample size
group.sample_size(50);

// Set warmup time
group.warm_up_time(Duration::from_secs(3));

// Configure significance level
group.significance_level(0.05);
```

## Performance Goals by Dataset Size

| Operation | 10 Packages | 100 Packages | 1000 Packages | Complexity |
|-----------|-------------|--------------|---------------|------------|
| Text Search | < 10ms | < 50ms | < 500ms | O(n) |
| Category Filter | < 5ms | < 25ms | < 250ms | O(n) |
| Tag Search | < 5ms | < 25ms | < 250ms | O(n) |
| Complex Search | < 15ms | < 75ms | < 750ms | O(n) |
| Maturity Assess (seq) | < 50ms | < 500ms | < 5s | O(n) |
| Maturity Assess (par) | < 20ms | < 200ms | < 2s | O(n/cores) |
| CSV Export | < 10ms | < 100ms | < 1s | O(n) |
| JSON Export | < 20ms | < 200ms | < 2s | O(n) |
| HTML Export | < 50ms | < 500ms | < 5s | O(n) |
| Recommendations | < 50ms | < 500ms | < 5s | O(n log n) |

## Reporting Performance Issues

When reporting performance problems, include:

1. **Benchmark Results**: Attach the HTML report or markdown summary
2. **System Information**: OS, CPU, RAM, Rust version
3. **Dataset Size**: Number of packages being processed
4. **Expected vs Actual**: What you expected vs what you got
5. **Reproducibility**: Steps to reproduce the issue

## Resources

- **Criterion.rs Documentation**: https://bheisler.github.io/criterion.rs/book/
- **Rust Performance Book**: https://nnethercote.github.io/perf-book/
- **Flamegraph Profiling**: https://github.com/flamegraph-rs/flamegraph

## License

Same as parent project (MIT/Apache-2.0).
