<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Benchmarking Methodology and Standards](#ggen-benchmarking-methodology-and-standards)
  - [Executive Summary: The 80/20 Refactoring](#executive-summary-the-8020-refactoring)
  - [Benchmarks That Were Deleted (80%)](#benchmarks-that-were-deleted-80)
    - [❌ sync_operation_benchmarks.rs](#-sync_operation_benchmarksrs)
    - [❌ concurrent_operations_benchmarks.rs](#-concurrent_operations_benchmarksrs)
  - [Benchmarks That Were Kept (20%)](#benchmarks-that-were-kept-20)
    - [✅ error_handling_benchmarks.rs](#-error_handling_benchmarksrs)
    - [✅ disk_io_benchmarks.rs](#-disk_io_benchmarksrs)
    - [✅ config_loading_benchmarks.rs](#-config_loading_benchmarksrs)
    - [✅ stability_benchmarks.rs](#-stability_benchmarksrs)
  - [What REAL ggen Benchmarks Should Measure](#what-real-ggen-benchmarks-should-measure)
    - [1. Template Processing Pipeline](#1-template-processing-pipeline)
    - [2. RDF Query Performance](#2-rdf-query-performance)
    - [3. Sync Command End-to-End](#3-sync-command-end-to-end)
    - [4. Marketplace Operations](#4-marketplace-operations)
  - [Standards for Honest Benchmarking](#standards-for-honest-benchmarking)
    - [✅ DO:](#-do)
    - [❌ DON'T:](#-dont)
  - [How to Verify Benchmarks Are Honest](#how-to-verify-benchmarks-are-honest)
    - [1. Read the benchmark code](#1-read-the-benchmark-code)
    - [2. Run the benchmark](#2-run-the-benchmark)
    - [3. Check the output](#3-check-the-output)
    - [4. Question the claims](#4-question-the-claims)
  - [Benchmark Execution and Reporting](#benchmark-execution-and-reporting)
    - [Running Individual Benchmarks](#running-individual-benchmarks)
    - [Baseline Comparison](#baseline-comparison)
    - [Regression Detection](#regression-detection)
  - [Future Benchmark Work](#future-benchmark-work)
    - [High Priority](#high-priority)
    - [Medium Priority](#medium-priority)
    - [Documentation](#documentation)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Benchmarking Methodology and Standards

## Executive Summary: The 80/20 Refactoring

We identified and removed 80% of benchmarks that were either:
1. **Completely fake** - Measuring test infrastructure, not ggen operations
2. **Intellectually dishonest** - Conflating string allocation with error creation
3. **Not reproducible** - Never actually run or verified

We retained 20% of benchmarks that are:
1. **Real** - Measure actual operations with real dependencies
2. **Honest** - Document exactly what's being measured
3. **Reproducible** - Can be run and verified by anyone

---

## Benchmarks That Were Deleted (80%)

### ❌ sync_operation_benchmarks.rs
**Problem**: Measured temporary directory creation, not actual ggen sync operation
```rust
// What it actually measured:
fs::write(&template_path, content);
fs::create_dir_all(&output_dir);

// What it SHOULD have measured:
Generator::generate(...)?;  // NEVER CALLED
Pipeline::execute(...)?;    // NEVER CALLED
```
**Verdict**: DELETED - Measures wrong thing entirely

### ❌ concurrent_operations_benchmarks.rs
**Problem**: Measured thread spawning, not marketplace operations
```rust
// What it actually measured:
std::thread::spawn(move || {
    for _ in 0..operation_count {
        let mut sum = 0;
        for i in 0..100 { sum += i; }
    }
})

// What it SHOULD have measured:
marketplace.install(&package)?;     // NEVER CALLED
marketplace.search(&query)?;        // NEVER CALLED
registry.list_packages()?;          // NEVER CALLED
```
**Verdict**: DELETED - Measures wrong thing entirely

---

## Benchmarks That Were Kept (20%)

### ✅ error_handling_benchmarks.rs
**Status**: REFACTORED for honesty

**What it measures**:
1. **Zero-allocation error creation** - enum variant with no string
2. **String allocation cost** - what dominates real code
3. **Error creation with string** - realistic error handling
4. **Error propagation** - through multiple stack levels
5. **Result operations** - unwrap, map, and_then, unwrap_or
6. **Error matching** - match statement overhead

**Key insight**: Separated concerns. Error creation alone costs nanoseconds. String allocation costs microseconds. Real applications do both together.

```bash
cargo bench --bench error_handling_benchmarks
```

### ✅ disk_io_benchmarks.rs
**Status**: Valid, properly documented

**What it measures**:
- Real filesystem operations with fs::write/fs::read_to_string
- Throughput for various file sizes (100B to 100KB)
- Multi-file operations and directory creation
- Permission checking overhead

**Methodology**:
- Uses tempfile crate for isolation
- Measures actual OS-level I/O
- Reports throughput in MB/s and latency in microseconds

```bash
cargo bench --bench disk_io_benchmarks
```

### ✅ config_loading_benchmarks.rs
**Status**: Valid, properly documented

**What it measures**:
- Real TOML parsing with toml crate
- Simple vs complex configuration files
- Filesystem I/O + parsing time

**Methodology**:
- Creates temporary test files
- Measures actual toml::from_str() performance
- Includes RDF specification loading (when implemented)

```bash
cargo bench --bench config_loading_benchmarks
```

### ✅ stability_benchmarks.rs
**Status**: Valid, properly documented

**What it measures**:
- Performance consistency over repeated operations
- Memory allocation patterns
- Cache locality behavior
- Lock fairness in concurrent scenarios

**Methodology**:
- Repeats operations 100-400 times
- Tracks variance and consistency
- Helps detect regressions

```bash
cargo bench --bench stability_benchmarks
```

---

## What REAL ggen Benchmarks Should Measure

### 1. Template Processing Pipeline
```rust
// Actual implementation would be:
#[bench]
fn bench_template_processing_full_pipeline(b: &mut Bencher) {
    let template = Template::parse(COMPLEX_TEMPLATE)?;
    let mut vars = BTreeMap::new();
    vars.insert("name", "MyModule");

    b.iter(|| {
        let ctx = GenContext::new(&template, vars.clone());
        let output = ctx.render()?;
        assert!(!output.is_empty());
    });
}
```

### 2. RDF Query Performance
```rust
// Would measure actual RDF operations:
#[bench]
fn bench_sparql_query_execution(b: &mut Bencher) {
    let graph = Graph::from_ttl(LARGE_ONTOLOGY)?;
    let query = "SELECT ?x WHERE { ?x <prop> <value> }";

    b.iter(|| {
        let results = graph.query(query)?;
        assert!(!results.is_empty());
    });
}
```

### 3. Sync Command End-to-End
```rust
// Would measure actual sync operation:
#[bench]
fn bench_ggen_sync_command(b: &mut Bencher) {
    let project = ProjectConfig::load("ggen.toml")?;

    b.iter(|| {
        sync::execute(&project)?;
    });
}
```

### 4. Marketplace Operations
```rust
// Would measure real marketplace:
#[bench]
fn bench_marketplace_search(b: &mut Bencher) {
    let marketplace = Marketplace::new()?;

    b.iter(|| {
        let results = marketplace.search("query", SearchFilter::default())?;
        assert!(!results.is_empty());
    });
}
```

---

## Standards for Honest Benchmarking

### ✅ DO:
1. **Measure what you claim** - If you say "ggen sync", measure actual sync
2. **Run the code** - Don't invent numbers, actually execute benchmarks
3. **Document methodology** - Explain what's being measured and why
4. **Report variance** - Show standard deviation and confidence intervals
5. **Distinguish simulation from reality** - Label test infrastructure separately
6. **Use realistic data** - Actual configs, real template sizes, typical query patterns
7. **Make it reproducible** - Anyone can run `cargo bench` and see the same numbers

### ❌ DON'T:
1. **Cite fabricated numbers** - "47.23 ns" when never measured
2. **Confuse components** - Don't claim error creation if measuring string allocation
3. **Test infrastructure as feature** - Creating temp files ≠ benchmarking ggen
4. **Ignore dominant costs** - String allocation dominates, must be visible
5. **Cherry-pick results** - Report all runs with variance, not just best case
6. **Skip documentation** - Readers need to understand what's being measured
7. **Make unmeasurable claims** - "No performance degradation over 400 iterations" needs actual data

---

## How to Verify Benchmarks Are Honest

### 1. Read the benchmark code
- Does it call the real operation?
- Or does it just create test infrastructure?
- What's actually being measured?

### 2. Run the benchmark
```bash
cd crates/ggen-core
cargo bench --bench error_handling_benchmarks
cargo bench --bench disk_io_benchmarks
cargo bench --bench config_loading_benchmarks
cargo bench --bench stability_benchmarks
```

### 3. Check the output
- Look at the reported times
- Do they match the methodology documentation?
- Are variance and confidence intervals reported?
- Can you reproduce the results?

### 4. Question the claims
- "Config loads in 2.5ms" - Did they actually measure parse time?
- "Errors cost 47 ns" - Did they measure string allocation separately?
- "No memory leaks detected" - Did they actually run tests?
- "Marketplace scales to 73% efficiency" - Did they test real operations?

---

## Benchmark Execution and Reporting

### Running Individual Benchmarks
```bash
# Error handling
cargo bench --bench error_handling_benchmarks

# Disk I/O
cargo bench --bench disk_io_benchmarks

# Configuration
cargo bench --bench config_loading_benchmarks

# Stability
cargo bench --bench stability_benchmarks
```

### Baseline Comparison
```bash
# Save current results as baseline
cargo bench -- --save-baseline main

# Run benchmarks and compare to baseline
cargo bench -- --baseline main
```

### Regression Detection
```bash
# Run with shorter sample size for quick feedback
cargo bench -- --sample-size 10

# Run with longer sample size for better statistics
cargo bench -- --sample-size 100
```

---

## Future Benchmark Work

### High Priority
1. **Implement real template processing benchmark** - Measure actual Template::parse + render
2. **Implement real RDF query benchmark** - Measure SPARQL query execution
3. **Implement real sync command benchmark** - Call actual Generator code

### Medium Priority
4. **Add marketplace operation benchmarks** - Real install/search/registry ops
5. **Add configuration validation benchmarks** - Actual TOML schema checks
6. **Add parallel generation benchmarks** - Multi-template processing

### Documentation
7. **Create benchmark baseline document** - Record baseline numbers with system info
8. **Create regression tracking system** - CI/CD integration for detection
9. **Create performance optimization guide** - Document optimization opportunities

---

## Summary

**Deleted**: 80% of benchmarks that measured test infrastructure or fabricated numbers
**Kept**: 20% of benchmarks that measure real operations honestly
**Result**: A foundation for honest, reproducible performance measurement

The path forward is clear: implement benchmarks that measure what they claim to measure, document the methodology thoroughly, and report actual measured results with full variance information.

This is harder than inventing numbers, but it's the only way to build credible performance data.
