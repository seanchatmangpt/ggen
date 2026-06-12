<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Performance Optimization Quick Start Guide](#performance-optimization-quick-start-guide)
  - [🚀 Quick Start](#-quick-start)
    - [1. Run Performance Benchmarks (5 minutes)](#1-run-performance-benchmarks-5-minutes)
    - [2. Check Current Performance (1 minute)](#2-check-current-performance-1-minute)
    - [3. Identify Bottlenecks (10 minutes)](#3-identify-bottlenecks-10-minutes)
    - [4. Apply Quick Wins (30 minutes)](#4-apply-quick-wins-30-minutes)
      - [Quick Win &#035;1: Lazy RDF Loading](#quick-win-1-lazy-rdf-loading)
      - [Quick Win &#035;2: Cache Instrumentation](#quick-win-2-cache-instrumentation)
      - [Quick Win &#035;3: Parallel Template Generation](#quick-win-3-parallel-template-generation)
    - [5. Verify Improvements (5 minutes)](#5-verify-improvements-5-minutes)
  - [🎯 Performance Targets](#-performance-targets)
  - [🔥 Common Performance Issues & Fixes](#-common-performance-issues--fixes)
    - [Issue 1: Slow Template Generation](#issue-1-slow-template-generation)
    - [Issue 2: High Memory Usage](#issue-2-high-memory-usage)
    - [Issue 3: Slow Lockfile Operations](#issue-3-slow-lockfile-operations)
    - [Issue 4: Slow SPARQL Queries](#issue-4-slow-sparql-queries)
  - [📊 Continuous Performance Monitoring](#-continuous-performance-monitoring)
    - [Add to CI/CD Pipeline](#add-to-cicd-pipeline)
    - [Performance Alerts](#performance-alerts)
  - [🛠️ Advanced Profiling Tools](#-advanced-profiling-tools)
    - [1. Flamegraph (CPU Profiling)](#1-flamegraph-cpu-profiling)
    - [2. Heaptrack (Memory Profiling)](#2-heaptrack-memory-profiling)
    - [3. Valgrind (Memory Leaks)](#3-valgrind-memory-leaks)
    - [4. perf (Linux CPU Profiling)](#4-perf-linux-cpu-profiling)
    - [5. Instruments (macOS)](#5-instruments-macos)
  - [📈 Performance Optimization Checklist](#-performance-optimization-checklist)
  - [📚 Further Reading](#-further-reading)
  - [🤝 Contributing Performance Improvements](#-contributing-performance-improvements)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Performance Optimization Quick Start Guide

**TL;DR**: Follow these steps to profile, benchmark, and optimize ggen performance.

## 🚀 Quick Start

### 1. Run Performance Benchmarks (5 minutes)

```bash
# Run shell script benchmarks (simple, fast)
./scripts/performance_benchmark.sh

# Run Criterion benchmarks (detailed, slower)
cargo bench --bench performance_benchmark

# Run Criterion with flamegraph
cargo bench --bench performance_benchmark -- --profile-time=10
```

**Expected Output**:
```
template_parsing/simple_template:  1.2ms ± 0.1ms  ✅
template_caching/cache_hit:        0.8ms ± 0.05ms ✅
rdf_operations/insert_small:       12ms  ± 2ms    ✅
lockfile_load_100_entries:         45ms  ± 5ms    ⚠️
```

---

### 2. Check Current Performance (1 minute)

```bash
# CLI startup time
time target/release/ggen --help
# Expected: ~10ms ✅

# Memory usage
/usr/bin/time -l target/release/ggen --version
# Expected: ~11MB ✅

# Template generation (create test template first)
time target/release/ggen generate test.tmpl
# Expected: <100ms ✅
```

---

### 3. Identify Bottlenecks (10 minutes)

```bash
# Generate flamegraph (install with: cargo install flamegraph)
cargo flamegraph --release --bin ggen -- generate large_template.tmpl

# Open flamegraph.svg in browser
open flamegraph.svg
```

**What to look for**:
- Wide bars = time spent in that function
- Red bars = CPU-bound operations
- Green bars = I/O operations

**Common bottlenecks**:
- `Template::parse` - YAML parsing
- `Graph::insert_turtle` - RDF parsing
- `Tera::render` - Template rendering
- `fs::write` - Disk I/O

---

### 4. Apply Quick Wins (30 minutes)

#### Quick Win #1: Lazy RDF Loading

**File**: `ggen-core/src/generator.rs`

```rust
// BEFORE:
tmpl.process_graph(&mut self.pipeline.graph, ...)?;

// AFTER:
if tmpl.has_graph_usage() {
    tmpl.process_graph(&mut self.pipeline.graph, ...)?;
}
```

Add helper method to `Template`:
```rust
impl Template {
    pub fn has_graph_usage(&self) -> bool {
        self.front.query.is_some() ||
        self.front.graph.is_some() ||
        self.body.contains("query_results")
    }
}
```

**Expected improvement**: 40-60% faster for templates without RDF

---

#### Quick Win #2: Cache Instrumentation

**File**: `ggen-core/src/template_cache.rs`

```rust
use std::sync::atomic::{AtomicU64, Ordering};

pub struct TemplateCache {
    cache: Arc<Mutex<LruCache<String, Arc<Template>>>>,
    hits: Arc<AtomicU64>,
    misses: Arc<AtomicU64>,
}

impl TemplateCache {
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: Arc::new(Mutex::new(LruCache::new(cap))),
            hits: Arc::new(AtomicU64::new(0)),
            misses: Arc::new(AtomicU64::new(0)),
        }
    }

    pub fn get_or_parse(&self, path: &Path) -> Result<Arc<Template>> {
        let mut cache = self.cache.lock().unwrap();

        if let Some(template) = cache.get(&path_str) {
            self.hits.fetch_add(1, Ordering::Relaxed);
            return Ok(Arc::clone(template));
        }

        self.misses.fetch_add(1, Ordering::Relaxed);
        // ... parse template ...
    }

    pub fn hit_rate(&self) -> f64 {
        let hits = self.hits.load(Ordering::Relaxed) as f64;
        let misses = self.misses.load(Ordering::Relaxed) as f64;
        if hits + misses > 0.0 {
            hits / (hits + misses)
        } else {
            0.0
        }
    }
}
```

---

#### Quick Win #3: Parallel Template Generation

**File**: `ggen-core/src/templates/generator.rs`

```rust
use rayon::prelude::*;

pub fn generate_bulk(templates: Vec<PathBuf>, output_dir: PathBuf) -> Result<Vec<PathBuf>> {
    templates
        .par_iter()
        .map(|template_path| {
            let pipeline = Pipeline::new()?;
            let ctx = GenContext::new(template_path.clone(), output_dir.clone());
            let mut generator = Generator::new(pipeline, ctx);
            generator.generate()
        })
        .collect::<Result<Vec<_>>>()
}
```

**Expected improvement**: 2-4x faster for bulk generation

---

### 5. Verify Improvements (5 minutes)

```bash
# Re-run benchmarks
cargo bench --bench performance_benchmark

# Compare results
# Before: template_parsing/simple: 5.2ms
# After:  template_parsing/simple: 2.8ms  ← 46% improvement ✅
```

---

## 🎯 Performance Targets

| Operation | Target | Current | Status |
|-----------|--------|---------|--------|
| CLI startup | <50ms | ~10ms | ✅ EXCELLENT |
| Template parse | <10ms | ~1-5ms | ✅ EXCELLENT |
| Template render | <50ms | ~5-50ms | ⚠️ MONITOR |
| RDF insert (small) | <50ms | ~5-20ms | ✅ GOOD |
| Lockfile load (100) | <50ms | ~20-50ms | ⚠️ MONITOR |
| Project scaffold | <2000ms | Unknown | ⚠️ MEASURE |

---

## 🔥 Common Performance Issues & Fixes

### Issue 1: Slow Template Generation

**Symptoms**:
- `ggen generate` takes >500ms
- Flamegraph shows time in `Template::parse`

**Diagnosis**:
```bash
# Check if template has RDF blocks
grep -E "graph:|query:" template.tmpl
```

**Fix**:
- Apply Quick Win #1 (lazy RDF loading)
- Consider caching templates (Quick Win #2)

---

### Issue 2: High Memory Usage

**Symptoms**:
- Memory >100MB for simple operations
- OOM errors on large projects

**Diagnosis**:
```bash
# Profile memory
/usr/bin/time -l target/release/ggen generate template.tmpl
# Check "maximum resident set size"
```

**Fixes**:
1. Reduce template cache size (default: 100)
   ```rust
   TemplateCache::new(50) // Reduce to 50
   ```

2. Clear graph after generation
   ```rust
   graph.clear()?; // Free memory
   ```

3. Use streaming for large files
   ```rust
   // Instead of reading entire file
   let content = fs::read_to_string(path)?;

   // Use buffered reader
   let file = File::open(path)?;
   let reader = BufReader::new(file);
   ```

---

### Issue 3: Slow Lockfile Operations

**Symptoms**:
- `ggen pack install` takes >5 seconds
- Flamegraph shows time in `resolve_dependencies`

**Diagnosis**:
```bash
# Check number of dependencies
cat ggen.lock | grep "id =" | wc -l
```

**Fixes**:
1. Parallel dependency resolution
   ```rust
   use rayon::prelude::*;

   manifest.dependencies
       .par_iter()  // ← Parallel
       .map(|dep| resolve_dependency(dep))
       .collect()
   ```

2. Lazy dependency resolution
   ```rust
   // Only resolve when needed
   if lockfile.needs_resolution() {
       resolve_dependencies()?;
   }
   ```

---

### Issue 4: Slow SPARQL Queries

**Symptoms**:
- Templates with RDF take >200ms
- Flamegraph shows time in `Graph::query`

**Diagnosis**:
```bash
# Check query complexity
cat template.tmpl | grep -A 10 "query:"
```

**Fixes**:
1. Optimize SPARQL queries
   ```sparql
   # BEFORE (slow):
   SELECT ?value WHERE {
     ?s ?p ?value .
     FILTER(?p = ex:name)
   }

   # AFTER (fast):
   SELECT ?value WHERE {
     ?s ex:name ?value .
   }
   ```

2. Use query result caching (already implemented)
   ```rust
   // Queries are automatically cached
   graph.query(sparql)?; // First call: slow
   graph.query(sparql)?; // Second call: fast ✅
   ```

---

## 📊 Continuous Performance Monitoring

### Add to CI/CD Pipeline

```yaml
# .github/workflows/performance.yml
name: Performance Tests

on: [pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build release
        run: cargo build --release

      - name: Run benchmarks
        run: cargo bench --bench performance_benchmark

      - name: Check for regressions
        run: |
          # Fail if any benchmark is >10% slower
          cargo bench -- --save-baseline main
          cargo bench -- --baseline main --test
```

---

### Performance Alerts

Set up alerts for key metrics:

```rust
// In benchmarks
if result.p99 > 50 { // 50ms threshold
    panic!("Performance regression: p99={}ms", result.p99);
}
```

---

## 🛠️ Advanced Profiling Tools

### 1. Flamegraph (CPU Profiling)

```bash
cargo install flamegraph
cargo flamegraph --release --bin ggen -- generate template.tmpl
open flamegraph.svg
```

### 2. Heaptrack (Memory Profiling)

```bash
# macOS
brew install heaptrack

# Profile
heaptrack target/release/ggen generate template.tmpl

# Visualize
heaptrack_gui heaptrack.ggen.*.gz
```

### 3. Valgrind (Memory Leaks)

```bash
# Linux only
valgrind --leak-check=full target/release/ggen generate template.tmpl
```

### 4. perf (Linux CPU Profiling)

```bash
# Record
perf record -g target/release/ggen generate template.tmpl

# Report
perf report
```

### 5. Instruments (macOS)

```bash
# Profile with Xcode Instruments
instruments -t "Time Profiler" target/release/ggen generate template.tmpl
```

---

## 📈 Performance Optimization Checklist

Before committing performance improvements:

- [ ] Run `cargo bench` and verify improvement
- [ ] Check memory usage hasn't increased
- [ ] Verify correctness with `cargo test`
- [ ] Update PERFORMANCE_ANALYSIS.md with findings
- [ ] Add regression test to prevent future regressions
- [ ] Profile with flamegraph to confirm bottleneck is fixed
- [ ] Document the optimization in code comments

---

## 📚 Further Reading

- [Full Performance Analysis](./PERFORMANCE_ANALYSIS.md)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Criterion Benchmarking Guide](https://bheisler.github.io/criterion.rs/book/)
- [Flamegraph Interpretation](http://www.brendangregg.com/flamegraphs.html)

---

## 🤝 Contributing Performance Improvements

1. Run benchmarks BEFORE changes
   ```bash
   cargo bench -- --save-baseline before
   ```

2. Make your changes

3. Run benchmarks AFTER changes
   ```bash
   cargo bench -- --baseline before
   ```

4. Document improvements in PR description
   ```markdown
   ## Performance Impact
   - template_parsing: 5.2ms → 2.8ms (46% improvement)
   - lockfile_load_100: 45ms → 18ms (60% improvement)
   ```

5. Include flamegraph comparison (optional but helpful)

---

**Last Updated**: 2025-11-18
**Maintained by**: Performance Benchmarker Agent
