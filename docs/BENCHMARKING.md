<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Performance Benchmarking Guide](#performance-benchmarking-guide)
  - [Overview](#overview)
  - [Quick Start](#quick-start)
    - [Running All Benchmarks](#running-all-benchmarks)
    - [Running Specific Benchmarks](#running-specific-benchmarks)
    - [Viewing Results](#viewing-results)
  - [Benchmark Suites](#benchmark-suites)
    - [1. Pipeline Performance (`pipeline_performance.rs`)](#1-pipeline-performance-pipeline_performancers)
      - [Ontology Parsing](#ontology-parsing)
      - [SPARQL Query Execution](#sparql-query-execution)
      - [Template Rendering](#template-rendering)
      - [File I/O Operations](#file-io-operations)
      - [Memory Usage](#memory-usage)
      - [End-to-End Generation](#end-to-end-generation)
    - [2. CLI Startup Performance (`cli_startup_performance.rs`)](#2-cli-startup-performance-cli_startup_performancers)
      - [CLI Command Startup](#cli-command-startup)
      - [Cold vs. Warm Start](#cold-vs-warm-start)
    - [3. Template Benchmarks (`template_benchmarks.rs`)](#3-template-benchmarks-template_benchmarksrs)
  - [CI/CD Integration](#cicd-integration)
    - [Automated Performance Tracking](#automated-performance-tracking)
    - [Performance Regression Detection](#performance-regression-detection)
    - [Viewing CI Results](#viewing-ci-results)
  - [Adding New Benchmarks](#adding-new-benchmarks)
    - [Creating a New Benchmark](#creating-a-new-benchmark)
    - [Benchmark Best Practices](#benchmark-best-practices)
      - [Use `black_box`](#use-black_box)
      - [Set Appropriate Throughput](#set-appropriate-throughput)
      - [Adjust Sample Size](#adjust-sample-size)
      - [Use Realistic Data](#use-realistic-data)
  - [Performance Targets](#performance-targets)
    - [Core Pipeline](#core-pipeline)
    - [CLI](#cli)
    - [Memory](#memory)
  - [Interpreting Results](#interpreting-results)
    - [Criterion Output](#criterion-output)
    - [HTML Reports](#html-reports)
  - [Troubleshooting](#troubleshooting)
    - [Benchmarks Fail to Run](#benchmarks-fail-to-run)
    - [High Variability](#high-variability)
    - [Missing Test Fixtures](#missing-test-fixtures)
  - [Continuous Improvement](#continuous-improvement)
    - [Performance Optimization Workflow](#performance-optimization-workflow)
  - [Resources](#resources)
  - [Contributing](#contributing)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Performance Benchmarking Guide

This guide explains the performance benchmarking infrastructure for ggen, including how to run benchmarks, interpret results, and contribute new benchmarks.

## Overview

The ggen project includes comprehensive performance benchmarks to track and prevent regressions in critical code paths. The benchmarking suite covers:

1. **Ontology Parsing** - Parsing performance for ontologies of various sizes (10-500+ classes)
2. **SPARQL Query Execution** - Query performance across different data sizes and complexity levels
3. **Template Rendering** - Template processing and variable substitution performance
4. **File I/O Operations** - Read, write, and append operations at various file sizes
5. **Memory Usage** - Memory consumption during large-scale generations
6. **CLI Startup Time** - Command-line interface initialization and execution time
7. **End-to-End Generation** - Complete pipeline performance from template to output

## Quick Start

### Running All Benchmarks

```bash
# Run all benchmarks
cargo bench

# Run specific benchmark suite
cargo bench --bench pipeline_performance
cargo bench --bench cli_startup_performance
cargo bench --bench template_benchmarks
```

### Running Specific Benchmarks

```bash
# Run only ontology parsing benchmarks
cargo bench --bench pipeline_performance -- ontology_parsing

# Run only SPARQL benchmarks
cargo bench --bench pipeline_performance -- sparql_query_execution

# Run only CLI startup benchmarks
cargo bench --bench cli_startup_performance -- cli_startup
```

### Viewing Results

Benchmark results are automatically generated in HTML format:

```bash
# Open the main benchmark report
open target/criterion/report/index.html

# View specific benchmark results
open target/criterion/ontology_parsing/report/index.html
```

## Benchmark Suites

### 1. Pipeline Performance (`pipeline_performance.rs`)

Comprehensive benchmarks for the core generation pipeline.

#### Ontology Parsing

Tests parsing performance for ontologies of different sizes:

- **Small**: 10 classes
- **Medium**: 50 classes
- **Large**: 150 classes
- **Very Large**: 500 classes

```bash
cargo bench --bench pipeline_performance -- ontology_parsing
```

**Performance Targets:**
- Small ontologies: < 1ms
- Medium ontologies: < 5ms
- Large ontologies: < 20ms
- Very large ontologies: < 100ms

#### SPARQL Query Execution

Tests query performance across different data sizes (10-1000 entities) and query types:

- **Simple SELECT**: Basic queries without filtering
- **Filtered SELECT**: Queries with WHERE clause filters
- **Complex SELECT**: Multi-join queries with multiple conditions

```bash
cargo bench --bench pipeline_performance -- sparql_query_execution
```

**Performance Targets:**
- 10 entities: < 1ms
- 100 entities: < 10ms
- 1000 entities: < 100ms

#### Template Rendering

Tests rendering performance with varying numbers of variables and loop iterations:

```bash
cargo bench --bench pipeline_performance -- template_rendering
```

**Performance Targets:**
- 10 vars + 10 loops: < 1ms
- 100 vars + 100 loops: < 10ms
- 500 vars + 100 loops: < 50ms

#### File I/O Operations

Tests read, write, and append operations for files of different sizes:

- 1KB, 10KB, 100KB, 1MB

```bash
cargo bench --bench pipeline_performance -- file_io_operations
```

#### Memory Usage

Compares sequential vs. batch generation approaches:

```bash
cargo bench --bench pipeline_performance -- memory_large_generations
```

**Performance Targets:**
- Sequential generation should have O(1) memory usage
- Batch generation should complete without OOM errors

#### End-to-End Generation

Tests complete pipeline execution for templates of varying complexity:

```bash
cargo bench --bench pipeline_performance -- e2e_generation
```

### 2. CLI Startup Performance (`cli_startup_performance.rs`)

Benchmarks command-line interface performance.

#### CLI Command Startup

Tests initialization time for common CLI commands:

```bash
cargo bench --bench cli_startup_performance -- cli_startup
```

**Performance Targets:**
- Help command: < 100ms
- Version command: < 50ms
- List command: < 200ms

#### Cold vs. Warm Start

Compares first execution vs. repeated executions:

```bash
cargo bench --bench cli_startup_performance -- cold_warm_start
```

### 3. Template Benchmarks (`template_benchmarks.rs`)

Additional template-specific benchmarks in `ggen-core`.

## CI/CD Integration

### Automated Performance Tracking

The project includes a GitHub Actions workflow (`.github/workflows/performance.yml`) that:

1. **Runs on Every Push to Main**: Establishes performance baselines
2. **Runs on Pull Requests**: Compares PR performance against main branch
3. **Tracks Regressions**: Alerts on performance degradation > 25%
4. **Generates Reports**: Creates HTML reports and artifacts

### Performance Regression Detection

The CI system automatically:

- ✅ Compares benchmark results against baseline
- ✅ Flags regressions exceeding 25% threshold
- ✅ Comments on PRs with performance analysis
- ✅ Stores historical benchmark data

### Viewing CI Results

1. Navigate to the **Actions** tab in GitHub
2. Select the **Performance Benchmarks** workflow
3. View the latest run
4. Download artifacts for detailed HTML reports

## Adding New Benchmarks

### Creating a New Benchmark

1. **Create the benchmark file** in `benches/`:

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_my_feature(c: &mut Criterion) {
    c.bench_function("my_feature", |b| {
        b.iter(|| {
            // Your code to benchmark
            black_box(my_feature())
        });
    });
}

criterion_group!(benches, bench_my_feature);
criterion_main!(benches);
```

2. **Add to `Cargo.toml`**:

```toml
[[bench]]
name = "my_feature_benchmark"
harness = false
```

3. **Run the benchmark**:

```bash
cargo bench --bench my_feature_benchmark
```

### Benchmark Best Practices

#### Use `black_box`

Prevent compiler optimizations from skewing results:

```rust
use criterion::black_box;

b.iter(|| {
    let result = expensive_operation(black_box(&input));
    black_box(result)
});
```

#### Set Appropriate Throughput

Help Criterion calculate throughput metrics:

```rust
group.throughput(Throughput::Elements(item_count as u64));
```

#### Adjust Sample Size

For expensive operations, reduce sample size:

```rust
group.sample_size(10);  // Default is 100
```

#### Use Realistic Data

Create fixtures that represent real-world usage:

```rust
// Good: Realistic data
let ontology = load_real_ontology();

// Bad: Trivial data
let ontology = "empty";
```

## Performance Targets

### Core Pipeline

| Operation | Target | Threshold |
|-----------|--------|-----------|
| Small ontology parse | < 1ms | 1.25ms |
| Large ontology parse (150 classes) | < 20ms | 25ms |
| SPARQL query (100 entities) | < 10ms | 12.5ms |
| Template render (100 vars) | < 10ms | 12.5ms |
| File write (1MB) | < 50ms | 62.5ms |

### CLI

| Operation | Target | Threshold |
|-----------|--------|-----------|
| CLI startup (help) | < 100ms | 125ms |
| CLI startup (version) | < 50ms | 62.5ms |
| Cold start | < 200ms | 250ms |

### Memory

| Operation | Target |
|-----------|--------|
| Sequential generation (1000 files) | O(1) memory |
| Batch generation (1000 files) | < 500MB |

## Interpreting Results

### Criterion Output

Criterion provides detailed statistical analysis:

```
ontology_parsing/large  time:   [18.234 ms 18.567 ms 18.912 ms]
                        change: [-2.1234% +0.5432% +3.2341%] (p = 0.23 > 0.05)
                        No change in performance detected.
```

- **time**: Confidence interval for execution time
- **change**: Percent change from baseline
- **p-value**: Statistical significance (p < 0.05 indicates significant change)

### HTML Reports

Open `target/criterion/report/index.html` for:

- 📊 Interactive charts
- 📈 Historical trends
- 📉 Regression analysis
- 🔍 Detailed statistics

## Troubleshooting

### Benchmarks Fail to Run

**Issue**: `error: could not compile`

**Solution**: Ensure all dependencies are installed:
```bash
cargo clean
cargo build --release
```

### High Variability

**Issue**: Benchmark results vary significantly between runs

**Solutions**:
1. Close other applications to reduce system load
2. Increase sample size: `group.sample_size(200);`
3. Use measurement time: `group.measurement_time(Duration::from_secs(20));`
4. Disable CPU frequency scaling (Linux):
   ```bash
   echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
   ```

### Missing Test Fixtures

**Issue**: `Warning: Ontology file not found`

**Solution**: Ensure benchmark fixtures exist:
```bash
ls benches/fixtures/ontologies/
```

Fixtures should include:
- `small_ontology.ttl`
- `medium_ontology.ttl`
- `large_ontology.ttl`
- `very_large_ontology.ttl`

## Continuous Improvement

### Performance Optimization Workflow

1. **Identify Bottleneck**
   ```bash
   cargo bench --bench pipeline_performance
   ```

2. **Profile the Code**
   ```bash
   cargo flamegraph --bench pipeline_performance
   ```

3. **Optimize**
   - Review hot paths
   - Reduce allocations
   - Improve algorithms

4. **Verify Improvement**
   ```bash
   cargo bench --bench pipeline_performance -- --save-baseline before
   # Make changes
   cargo bench --bench pipeline_performance -- --baseline before
   ```

5. **Commit with Benchmark Evidence**
   ```
   perf: optimize SPARQL query execution

   - Implement query plan caching
   - Results: 45% faster for queries with 100+ entities

   Benchmark comparison:
   - Before: 18.5ms
   - After: 10.2ms
   - Improvement: 44.9%
   ```

## Resources

- [Criterion.rs Documentation](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [GitHub Actions Benchmark](https://github.com/benchmark-action/github-action-benchmark)

## Contributing

When adding new features:

1. ✅ Add benchmarks for performance-critical code
2. ✅ Ensure benchmarks pass CI regression thresholds
3. ✅ Document performance characteristics
4. ✅ Include benchmark results in PR descriptions

---

**Questions?** Open an issue with the `performance` label.
