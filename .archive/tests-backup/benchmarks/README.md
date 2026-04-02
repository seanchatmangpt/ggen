# GGEN Marketplace Performance Benchmarks

Comprehensive performance benchmarking suite for all marketplace commands.

## Overview

This benchmark suite provides detailed performance measurements across all marketplace operations:

1. **Search Performance** - Query performance across dataset sizes (10, 100, 1000 packages)
2. **Maturity Assessment** - Package maturity calculation performance
3. **Export Performance** - Data export in various formats (CSV, JSON, HTML, Markdown)
4. **Comparison** - Package comparison performance
5. **Recommendation Engine** - Recommendation generation and ranking
6. **Memory Usage** - Memory consumption analysis
7. **End-to-End Workflows** - Complete workflow performance

## Quick Start

```bash
# Run all benchmarks
./tests/benchmarks/run_benchmarks.sh

# Run specific benchmark group
cargo bench --bench marketplace_performance -- "search"

# Generate HTML report
cargo bench --bench marketplace_performance
open target/criterion/report/index.html
```

## Performance Goals

| Command Type | Latency Goal | Memory Goal | Success Rate |
|--------------|--------------|-------------|--------------|
| Interactive (search, compare) | < 500ms | < 100MB | > 99% |
| Reports (export, maturity) | < 5s | < 100MB | > 99% |

## Benchmark Structure

### 1. Search Performance Benchmarks

Tests query performance across different dataset sizes and query types:

- **Text Search**: Basic keyword search
- **Category Filter**: Search by package category
- **Tag Search**: Search by tags
- **Complex Search**: Multi-criteria search with filters

**Dataset Sizes**: 10, 100, 1000 packages

### 2. Maturity Assessment Benchmarks

Measures time to assess package maturity:

- **Single Package**: Individual package assessment
- **Batch Assessment**: Sequential assessment of multiple packages
- **Parallel Assessment**: Concurrent assessment using rayon

**Dataset Sizes**: 1, 10, 100 packages

### 3. Export Performance Benchmarks

Tests export time and file size for different formats:

- **CSV Export**: Comma-separated values
- **JSON Export**: Pretty-printed JSON
- **HTML Export**: Styled HTML table
- **Markdown Export**: Markdown table format

**Dataset Sizes**: 10, 100 packages

### 4. Comparison Benchmarks

Measures package comparison performance:

- **Two Package Comparison**: Standard comparison
- **Sequential Comparisons**: Multiple comparisons in sequence
- **Detailed Comparison**: Full comparison with all metrics

### 5. Recommendation Engine Benchmarks

Tests recommendation generation and ranking:

- **Basic Recommendations**: Simple preference-based filtering
- **ML Recommendations**: Machine learning-based ranking
- **Ranking Accuracy**: Validates ranking quality

**Dataset Sizes**: 10, 100 packages

### 6. Memory Usage Benchmarks

Analyzes memory consumption patterns:

- **Package Loading**: Memory used during data loading
- **Search & Filter**: Memory during search operations

**Dataset Sizes**: 100, 1000 packages

### 7. End-to-End Workflows

Complete workflow benchmarks:

- **Search → Assess → Export**: Full discovery workflow
- **Recommend → Compare → Export**: Full recommendation workflow

## Interpreting Results

### Criterion Output

Criterion provides detailed statistics:

- **Mean**: Average execution time
- **Median**: Middle value (50th percentile)
- **Std Dev**: Standard deviation showing variability
- **P95/P99**: 95th and 99th percentile latencies (tail latencies)

### Performance Analysis

The `performance_analysis.rs` module provides:

1. **Severity Classification**: Critical/High/Medium/Low/Info
2. **Optimization Recommendations**: Specific suggestions for improvements
3. **Scaling Analysis**: Complexity analysis (O(n), O(log n), etc.)
4. **Baseline Metrics**: Performance baseline for tracking progress

### Reading the HTML Report

Open `target/criterion/report/index.html` for:

- **Interactive Charts**: Latency distributions, throughput graphs
- **Comparison Views**: Compare against previous runs
- **Regression Detection**: Automatic performance regression alerts

## Running Specific Benchmarks

```bash
# Search benchmarks only
cargo bench --bench marketplace_performance -- "search"

# Maturity benchmarks only
cargo bench --bench marketplace_performance -- "maturity"

# Export benchmarks only
cargo bench --bench marketplace_performance -- "export"

# Comparison benchmarks only
cargo bench --bench marketplace_performance -- "comparison"

# Recommendation benchmarks only
cargo bench --bench marketplace_performance -- "recommendation"

# Memory benchmarks only
cargo bench --bench marketplace_performance -- "memory"

# E2E benchmarks only
cargo bench --bench marketplace_performance -- "e2e"
```

## Baseline Tracking

Save and compare against baselines:

```bash
# Save current results as baseline
cargo bench --bench marketplace_performance -- --save-baseline my-baseline

# Compare against baseline
cargo bench --bench marketplace_performance -- --baseline my-baseline

# List all baselines
ls target/criterion/*/base
```

## Performance Optimization Workflow

1. **Run Benchmarks**: `./tests/benchmarks/run_benchmarks.sh`
2. **Review Results**: Check `tests/benchmarks/results/performance_report_*.md`
3. **Identify Bottlenecks**: Look for Critical/High severity issues
4. **Apply Optimizations**: Implement recommended fixes
5. **Re-run Benchmarks**: Verify improvements
6. **Save Baseline**: Track progress over time

## Common Optimization Patterns

### For Search Operations

- ✅ Implement full-text search indexing (tantivy)
- ✅ Add result pagination
- ✅ Cache frequent queries
- ✅ Use lazy loading for large datasets

### For Maturity Assessment

- ✅ Parallelize calculations with rayon
- ✅ Cache intermediate results
- ✅ Batch API calls

### For Export Operations

- ✅ Stream data instead of loading all in memory
- ✅ Use buffered writes
- ✅ Consider parallel processing

### For Memory Optimization

- ✅ Use iterators instead of collecting into vectors
- ✅ Implement streaming for large data
- ✅ Release allocations eagerly
- ✅ Use smaller data structures (SmallVec)

## CI/CD Integration

Add to your CI pipeline:

```yaml
- name: Run Performance Benchmarks
  run: |
    cargo bench --bench marketplace_performance -- --save-baseline ci-baseline

- name: Check for Regressions
  run: |
    # Fail if performance regresses by > 10%
    cargo bench --bench marketplace_performance -- --baseline ci-baseline --significance-level 0.05
```

## Benchmark Configuration

Customize via Criterion:

```rust
// In marketplace_performance.rs
group.measurement_time(Duration::from_secs(10));  // Measurement duration
group.sample_size(50);                            // Number of samples
group.warm_up_time(Duration::from_secs(3));       // Warmup duration
```

## Troubleshooting

### Benchmarks Take Too Long

Reduce sample size:
```rust
group.sample_size(10);  // Default is 100
```

### Inconsistent Results

Increase measurement time:
```rust
group.measurement_time(Duration::from_secs(30));
```

### Memory Benchmarks Failing

Ensure enough system memory and close other applications.

## Performance Goals by Operation

| Operation | 10 Packages | 100 Packages | 1000 Packages |
|-----------|-------------|--------------|---------------|
| Text Search | < 10ms | < 50ms | < 500ms |
| Category Filter | < 5ms | < 25ms | < 250ms |
| Tag Search | < 5ms | < 25ms | < 250ms |
| Maturity Assessment | < 50ms | < 500ms | < 5s |
| CSV Export | < 10ms | < 100ms | < 1s |
| JSON Export | < 20ms | < 200ms | < 2s |
| HTML Export | < 50ms | < 500ms | < 5s |
| Recommendations | < 50ms | < 500ms | < 5s |

## Continuous Monitoring

Track performance over time:

```bash
# After each release
git tag v1.2.3
cargo bench --bench marketplace_performance -- --save-baseline v1.2.3

# Compare releases
cargo bench --bench marketplace_performance -- --baseline v1.2.2
```

## Contributing

When adding new benchmarks:

1. Follow the existing structure
2. Include dataset size variations
3. Test both sequential and parallel implementations
4. Document performance goals
5. Add to the benchmark runner script

## License

Same as parent project (MIT/Apache-2.0).
