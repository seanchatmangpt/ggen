# Template Benchmarks Quick Start Guide

**Created by:** OPTIMIZER (Zeta)
**For:** Template System Performance Analysis

---

## Quick Commands

### Run All Benchmarks
```bash
cargo bench --bench template_benchmarks
```

### Run Specific Benchmark Groups

```bash
# Parsing benchmarks (template parsing, frontmatter rendering)
cargo bench --bench template_benchmarks parsing_benches

# RDF benchmarks (RDF processing, SPARQL queries)
cargo bench --bench template_benchmarks rdf_benches

# Rendering benchmarks (variable substitution, file tree generation)
cargo bench --bench template_benchmarks rendering_benches

# Optimization benchmarks (caching, memory usage)
cargo bench --bench template_benchmarks optimization_benches

# End-to-end benchmarks (full template processing)
cargo bench --bench template_benchmarks e2e_benches
```

### Baseline Workflow

```bash
# 1. Establish baseline (before optimization)
cargo bench --bench template_benchmarks -- --save-baseline initial

# 2. Make optimizations...

# 3. Compare against baseline
cargo bench --bench template_benchmarks -- --baseline initial

# 4. View detailed HTML reports
open target/criterion/report/index.html
```

---

## Understanding Benchmark Output

### Sample Output
```
template_parsing/simple/10
                        time:   [234.56 µs 235.89 µs 237.34 µs]
                        change: [-5.23% -4.12% -3.01%] (p = 0.00 < 0.05)
                        Performance has improved.
```

**Interpretation:**
- **Time:** 235.89 µs (microseconds) - median execution time
- **Change:** -4.12% - improvement vs baseline
- **P-value:** < 0.05 - statistically significant

### Performance Indicators
- ✅ **Green/Improved:** Faster than baseline
- ⚠️ **Yellow/Regressed:** Slower than baseline
- ⏱️ **No change:** Within statistical noise

---

## Benchmark Categories

### 1. Parsing Benchmarks (`parsing_benches`)

**What:** Template parsing and frontmatter rendering

**Metrics:**
- Simple templates: 1, 10, 50 variables
- Complex templates: RDF + SPARQL + variables
- Frontmatter rendering: 1-100 variables

**Target:** < 1ms for simple, < 5ms for complex

### 2. RDF Benchmarks (`rdf_benches`)

**What:** RDF triple insertion and SPARQL queries

**Metrics:**
- RDF insertion: 1, 10, 50, 100 triples
- SPARQL queries: 1, 5, 10, 20 queries

**Target:** < 5ms for 10 triples, < 20ms for 10 queries

### 3. Rendering Benchmarks (`rendering_benches`)

**What:** Variable substitution and file tree generation

**Metrics:**
- Substitutions: 10, 50, 100, 500 variables
- File generation: 10, 100, 1000 files (sequential + parallel)

**Target:** < 10ms for 10 files, < 1s for 1000 files

### 4. Optimization Benchmarks (`optimization_benches`)

**What:** Template caching and memory usage

**Metrics:**
- Cache hit vs miss performance
- In-memory vs streaming generation
- 1000 templates in memory vs streaming

**Target:** Cache hit < 0.01ms, constant memory for streaming

### 5. End-to-End Benchmarks (`e2e_benches`)

**What:** Complete template processing pipeline

**Metrics:**
- Simple template (parse + render)
- Complex template (parse + RDF + SPARQL + render)
- Preprocessor integration

**Target:** < 5ms for simple, < 20ms for complex

---

## Performance Profiling

### CPU Profiling (Flamegraph)

```bash
# Install flamegraph
cargo install flamegraph

# Generate flamegraph
cargo flamegraph --bench template_benchmarks

# View SVG output
open flamegraph.svg
```

**What to look for:**
- Wide boxes = functions taking most time
- Tall stacks = deep call chains
- Hot paths = frequently called functions

### Memory Profiling (Valgrind)

```bash
# Install valgrind (macOS)
brew install valgrind

# Run memory profiling
valgrind --tool=massif cargo bench --bench template_benchmarks

# Analyze results
ms_print massif.out.<pid>
```

**What to look for:**
- Peak memory usage
- Memory growth patterns
- Allocation hotspots

### Detailed Timing Analysis

```bash
# Run with profiling enabled
cargo bench --bench template_benchmarks -- --profile-time=10

# View detailed timing
ls target/criterion/
```

---

## Interpreting Results

### Good Performance

```
template_parsing/simple/10
                        time:   [0.8 ms 0.9 ms 1.0 ms]
```
✅ Under 1ms target - excellent!

### Needs Optimization

```
template_parsing/complex/20
                        time:   [12.3 ms 12.5 ms 12.8 ms]
```
⚠️ Above 5ms target - needs optimization

### Regression Detected

```
rdf_processing/rdf_insert/100
                        time:   [8.2 ms 8.5 ms 8.9 ms]
                        change: [+15.2% +16.8% +18.4%]
```
🚨 16.8% slower than baseline - investigate!

---

## Common Optimization Strategies

### If parsing is slow:
1. Enable template caching
2. Pre-compile frequently used templates
3. Use streaming for large batches

### If RDF processing is slow:
1. Cache RDF graphs
2. Batch SPARQL queries
3. Use smaller graphs when possible

### If file generation is slow:
1. Enable parallel generation (rayon)
2. Use streaming generator for large projects
3. Reduce file I/O overhead

### If memory usage is high:
1. Switch to streaming generator
2. Release resources promptly
3. Use Arc<T> for shared data

---

## Continuous Performance Monitoring

### Add to CI/CD Pipeline

```yaml
# .github/workflows/benchmarks.yml
name: Benchmarks

on:
  push:
    branches: [main, develop]
  pull_request:

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run benchmarks
        run: |
          cargo bench --bench template_benchmarks -- --save-baseline ci

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: target/criterion/
```

### Performance Regression Tests

```bash
# Create performance baseline
cargo bench --bench template_benchmarks -- --save-baseline main

# After changes, compare
cargo bench --bench template_benchmarks -- --baseline main

# Fail if regression > 10%
# (Add to CI script)
```

---

## Troubleshooting

### Benchmarks won't compile

```bash
# Check for compilation errors
cargo build --benches

# Common fixes:
# 1. Update dependencies
cargo update

# 2. Clean build
cargo clean && cargo build --benches
```

### Inconsistent results

```bash
# Increase sample size
cargo bench --bench template_benchmarks -- --sample-size=100

# Increase measurement time
cargo bench --bench template_benchmarks -- --measurement-time=10
```

### Benchmarks too slow

```bash
# Reduce sample size
cargo bench --bench template_benchmarks -- --sample-size=10

# Quick mode (fewer iterations)
cargo bench --bench template_benchmarks -- --quick
```

---

## Best Practices

### ✅ DO
- Run benchmarks on idle system (close other apps)
- Use consistent hardware for comparisons
- Establish baseline before optimization
- Compare against baseline after changes
- Document significant performance changes

### ❌ DON'T
- Run benchmarks while system is under load
- Compare results from different machines
- Optimize without measuring first
- Ignore small regressions (they accumulate!)
- Skip baseline establishment

---

## Performance Targets Summary

| Operation | Target | Stretch Goal |
|-----------|--------|--------------|
| Parse simple template | < 1ms | < 0.5ms |
| Parse complex template | < 5ms | < 2ms |
| Generate 10 files | < 10ms | < 5ms |
| Generate 100 files | < 100ms | < 50ms |
| Generate 1000 files | < 1s | < 500ms |
| RDF processing (10 triples) | < 5ms | < 2ms |
| SPARQL queries (10 queries) | < 20ms | < 10ms |
| Template cache hit | < 0.01ms | < 0.001ms |

---

## Getting Help

### View Criterion Documentation
```bash
# Open criterion docs
cargo doc --open -p criterion
```

### Benchmark Issues
1. Check `ggen-core/benches/template_benchmarks.rs` comments
2. Review `docs/OPTIMIZATION_STRATEGY.md`
3. Consult criterion documentation

### Performance Questions
1. Review flamegraphs for hotspots
2. Check memory profiling results
3. Compare against baseline metrics

---

**Quick Reference:**
- **Run all:** `cargo bench --bench template_benchmarks`
- **Baseline:** `cargo bench -- --save-baseline NAME`
- **Compare:** `cargo bench -- --baseline NAME`
- **View reports:** `open target/criterion/report/index.html`

**OPTIMIZER (Zeta) - Performance benchmarking made simple! 🎯**
