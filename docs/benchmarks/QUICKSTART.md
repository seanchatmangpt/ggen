<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace V2 Performance Benchmarking - Quick Start Guide](#marketplace-v2-performance-benchmarking---quick-start-guide)
  - [Overview](#overview)
  - [SLO Targets](#slo-targets)
  - [Benchmark Categories](#benchmark-categories)
    - [1. Comprehensive Performance (`comprehensive_performance.rs`)](#1-comprehensive-performance-comprehensive_performancers)
    - [2. V1 vs V2 Comparison (`v1_vs_v2_comparison.rs`)](#2-v1-vs-v2-comparison-v1_vs_v2_comparisonrs)
    - [3. SLO Validation (`slo_validation.rs`)](#3-slo-validation-slo_validationrs)
  - [Running Benchmarks](#running-benchmarks)
    - [Prerequisites](#prerequisites)
    - [Run All Benchmarks](#run-all-benchmarks)
    - [Run Specific Benchmark Groups](#run-specific-benchmark-groups)
    - [View Results](#view-results)
  - [Test Dataset Sizes](#test-dataset-sizes)
  - [Interpreting Results](#interpreting-results)
    - [Latency Metrics](#latency-metrics)
    - [Example Good Results](#example-good-results)
    - [Performance Improvements (V2 vs V1)](#performance-improvements-v2-vs-v1)
  - [Benchmark Configuration](#benchmark-configuration)
    - [Criterion Settings](#criterion-settings)
    - [Customization](#customization)
  - [Optimization Workflow](#optimization-workflow)
  - [Continuous Integration](#continuous-integration)
    - [GitHub Actions Example](#github-actions-example)
  - [Performance Report](#performance-report)
  - [Troubleshooting](#troubleshooting)
    - [Benchmarks Taking Too Long](#benchmarks-taking-too-long)
    - [Inconsistent Results](#inconsistent-results)
    - [Out of Memory](#out-of-memory)
  - [Next Steps](#next-steps)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace V2 Performance Benchmarking - Quick Start Guide

## Overview

Comprehensive performance benchmarking suite for ggen marketplace v2, validating production SLOs and improvements over v1.

## SLO Targets

- **Lookup latency:** <100ms (p95)
- **Search latency:** <200ms (p95)
- **Cache hit rate:** >80%
- **Installation time:** <5s (without network)
- **Dashboard generation:** <2s

## Benchmark Categories

### 1. Comprehensive Performance (`comprehensive_performance.rs`)

Tests all aspects of marketplace performance:
- ✅ **Lookup Performance:** Single package, batch, version history
- ✅ **Cache Performance:** Hot/cold queries, hit rate measurement
- ✅ **Search Performance:** Simple text, description, SPARQL queries
- ✅ **Filtered Search:** Category, author, multi-filter queries
- ✅ **Scalability:** 10, 100, 1K, 10K package datasets
- ✅ **Installation:** Single package, with dependencies
- ✅ **V3 Optimized:** RDF-backed registry performance
- ✅ **Dashboard Generation:** Aggregate stats calculation
- ✅ **Memory Efficiency:** Footprint measurement

### 2. V1 vs V2 Comparison (`v1_vs_v2_comparison.rs`)

Direct comparison of v1 and v2 implementations:
- 📊 **Lookup Comparison:** Latency improvements
- 📊 **Search Comparison:** Throughput improvements
- 📊 **Batch Operations:** Bulk insert performance
- 📊 **Filtered Search:** Advanced query capabilities
- 📊 **Concurrent Access:** Lock-free performance
- 📊 **V3 Optimizations:** RDF backend benefits
- 📊 **Memory Footprint:** Resource usage comparison
- 📊 **Feature Completeness:** Versioning, dependencies

### 3. SLO Validation (`slo_validation.rs`)

Production readiness validation:
- ✅ **Lookup Latency SLO:** p95 <100ms validation
- ✅ **Search Latency SLO:** p95 <200ms validation
- ✅ **Cache Hit Rate SLO:** >80% validation
- ✅ **Install Time SLO:** <5s validation
- ✅ **Dashboard Gen SLO:** <2s validation
- 📋 **Comprehensive Report:** All SLOs summary

## Running Benchmarks

### Prerequisites

```bash
# Install Rust (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Navigate to marketplace-v2
cd crates/ggen-marketplace
```

### Run All Benchmarks

```bash
# Use the convenience script (recommended)
../../scripts/run_benchmarks.sh

# Or run individually:
cargo bench --bench comprehensive_performance
cargo bench --bench v1_vs_v2_comparison
cargo bench --bench slo_validation
```

### Run Specific Benchmark Groups

```bash
# Only lookup benchmarks
cargo bench --bench comprehensive_performance lookup

# Only search benchmarks
cargo bench --bench comprehensive_performance search

# Only SLO validation
cargo bench --bench slo_validation
```

### View Results

```bash
# Reports are in target/criterion/
open target/criterion/*/report/index.html

# Or navigate manually
cd target/criterion
ls -l
```

## Test Dataset Sizes

Benchmarks automatically generate test datasets:

| Size | Package Count | Use Case |
|------|--------------|----------|
| Small | 10 | Quick smoke test |
| Medium | 100 | Development testing |
| Large | 1,000 | Realistic load |
| X-Large | 10,000 | Production scale |

## Interpreting Results

### Latency Metrics

- **Mean:** Average latency across all samples
- **Std Dev:** Consistency of performance
- **p50 (Median):** Typical user experience
- **p95:** 95th percentile (SLO target)
- **p99:** Worst-case (excluding outliers)

### Example Good Results

```
Lookup Performance:
  Mean: 45ms
  p50: 42ms
  p95: 78ms ✅ (SLO: <100ms)
  p99: 95ms

Search Performance:
  Mean: 120ms
  p50: 110ms
  p95: 185ms ✅ (SLO: <200ms)
  p99: 210ms
```

### Performance Improvements (V2 vs V1)

Expected improvements:
- **Lookup:** 2-3x faster (DashMap, caching)
- **Search:** 3-5x faster (optimized indexing)
- **Scalability:** 10x better at 10K+ packages
- **Memory:** 30-40% reduction (compact representations)

## Benchmark Configuration

### Criterion Settings

- **Sample size:** 50-100 iterations (statistical significance)
- **Measurement time:** 10-30s (stable results)
- **Warm-up time:** 3-10s (JIT optimization)
- **Confidence level:** 95% (industry standard)

### Customization

Edit `Cargo.toml` to adjust:

```toml
[[bench]]
name = "comprehensive_performance"
harness = false  # Use criterion instead of built-in

[dev-dependencies]
criterion = { version = "0.7", features = ["html_reports", "async_tokio"] }
```

## Optimization Workflow

1. **Baseline:** Run benchmarks to establish baseline
2. **Identify:** Find bottlenecks in reports
3. **Optimize:** Implement performance improvements
4. **Validate:** Re-run benchmarks
5. **Compare:** Use `--baseline` flag

```bash
# Save baseline
cargo bench --bench comprehensive_performance -- --save-baseline v2.0

# Make optimizations...

# Compare with baseline
cargo bench --bench comprehensive_performance -- --baseline v2.0
```

## Continuous Integration

### GitHub Actions Example

```yaml
name: Performance Benchmarks
on: [push, pull_request]

jobs:
  bench:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo bench --workspace
      - uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: target/criterion/
```

## Performance Report

Benchmarks generate:

1. **HTML Reports:** `target/criterion/*/report/index.html`
2. **JSON Data:** `target/criterion/*/base/estimates.json`
3. **CSV Exports:** `target/criterion/*/base/raw.csv`
4. **Summary Markdown:** `target/benchmark-reports/BENCHMARK_SUMMARY.md`

## Troubleshooting

### Benchmarks Taking Too Long

```bash
# Reduce sample size (in benchmark files)
config = Criterion::default().sample_size(20)  # Default: 100
```

### Inconsistent Results

```bash
# Increase warm-up time
config = Criterion::default().warm_up_time(Duration::from_secs(10))

# Close other applications
# Disable CPU power saving
# Pin to specific CPU cores
```

### Out of Memory

```bash
# Reduce test dataset sizes
# Use smaller cache sizes
# Run benchmarks sequentially
```

## Next Steps

1. **Review SLO validation results** in `slo_validation` report
2. **Compare v1 vs v2** performance improvements
3. **Identify bottlenecks** in comprehensive report
4. **Implement optimizations** based on findings
5. **Validate improvements** with re-run

## Resources

- **Criterion.rs Documentation:** https://bheisler.github.io/criterion.rs/book/
- **Rust Performance Book:** https://nnethercote.github.io/perf-book/
- **Marketplace V2 Architecture:** `../architecture/MARKETPLACE_V2.md`
- **Optimization Guide:** `../guides/PERFORMANCE_OPTIMIZATION.md`

---

**Generated by:** ggen marketplace-v2 benchmark suite
**Version:** 3.0.0
**Date:** 2025-11-18
