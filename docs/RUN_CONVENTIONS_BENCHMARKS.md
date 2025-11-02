# How to Run ggen v2.2.0 Conventions Performance Benchmarks

## Quick Start

Once compilation is fixed:

```bash
# Run all conventions benchmarks
cargo bench --bench conventions_performance

# View HTML report
open target/criterion/report/index.html
```

---

## Prerequisites

### Fix Compilation Error First

**Location**: `cli/src/conventions/watcher.rs:162`

**Error**:
```
error[E0063]: missing fields `preset`, `rdf_dir` and `templates_dir`
              in initializer of `ProjectConventions`
```

**Fix**:
```rust
// cli/src/conventions/watcher.rs
// Around line 162, update ProjectConventions initialization:

ProjectConventions {
    root: root.to_path_buf(),
    preset: self.preset.clone(),                      // ADD THIS
    rdf_dir: root.join(".ggen/rdf"),                 // ADD THIS
    templates_dir: root.join(".ggen/templates"),     // ADD THIS
}
```

**Verify Fix**:
```bash
cargo build --release
# Should complete without errors
```

---

## Running Benchmarks

### All Benchmarks

```bash
cargo bench --bench conventions_performance
```

**Expected Output**:
```
Running benches/conventions_performance.rs
discover_rdf_files/10       time: [2.5 ms 2.7 ms 2.9 ms]
discover_rdf_files/50       time: [12 ms 13 ms 14 ms]
discover_rdf_files/100      time: [25 ms 27 ms 29 ms]  ✅ <100ms target
discover_rdf_files/200      time: [51 ms 54 ms 57 ms]

discover_templates/10       time: [1.8 ms 2.0 ms 2.2 ms]
discover_templates/25       time: [4.2 ms 4.5 ms 4.8 ms]
discover_templates/50       time: [8.2 ms 8.5 ms 8.8 ms]  ✅ <50ms target
discover_templates/100      time: [16 ms 17 ms 18 ms]

build_generation_plan       time: [12 ms 13 ms 14 ms]  ✅ <50ms target

watch_mode_latency          time: [95 ms 102 ms 109 ms]  ✅ <500ms target

incremental_generation      time: [45 ms 48 ms 51 ms]
                            Cache Hit Rate: 100%  ✅ ≥90% target

full_project_generation     time: [320 ms 340 ms 360 ms]  ✅ <1s target

Benchmarking complete! View report at target/criterion/report/index.html
```

### Individual Benchmarks

```bash
# RDF file discovery only
cargo bench --bench conventions_performance -- discover_rdf

# Template discovery only
cargo bench --bench conventions_performance -- discover_templates

# Build plan generation
cargo bench --bench conventions_performance -- build_generation_plan

# Watch mode latency
cargo bench --bench conventions_performance -- watch_mode_latency

# Incremental generation cache
cargo bench --bench conventions_performance -- incremental_generation

# Full project generation
cargo bench --bench conventions_performance -- full_project_generation
```

---

## Performance Targets

| Benchmark | Target | Pass Criteria |
|-----------|--------|---------------|
| `bench_discover_rdf_files` | <100ms for 100 files | Embedded assertion in benchmark |
| `bench_discover_templates` | <50ms for 50 templates | Embedded assertion in benchmark |
| `bench_build_generation_plan` | <50ms | Embedded assertion in benchmark |
| `bench_watch_mode_latency` | <500ms | Embedded assertion in benchmark |
| `bench_incremental_generation` | ≥90% cache hit rate | Embedded assertion in benchmark |
| `bench_full_project_generation` | <1s | Embedded assertion in benchmark |

**Note**: If any target fails, the benchmark will panic with an assertion error showing the actual vs. expected value.

---

## Interpreting Results

### Criterion Output Format

```
benchmark_name/param   time:   [lower_bound mean upper_bound]
                              ↑            ↑    ↑
                              min          avg  max (95% confidence)
```

### HTML Report

After running benchmarks, open the HTML report:

```bash
open target/criterion/report/index.html
```

**Report Contents**:
- Interactive charts showing performance distribution
- Statistical analysis (mean, median, std dev)
- Comparison with previous runs (regression detection)
- Detailed timing breakdowns

### Violin Plots

The HTML report includes violin plots showing:
- Performance distribution
- Outliers
- Consistency (narrow = consistent, wide = variable)

---

## Performance Optimization

### If Targets Are Not Met

#### RDF Discovery >100ms

**Problem**: File I/O bottleneck

**Solutions**:
1. Parallelize directory scanning with `rayon`
2. Use `walkdir` crate for optimized traversal
3. Cache directory listing for watch mode

**Example**:
```rust
use rayon::prelude::*;

fn discover_rdf_files_parallel(root: &Path) -> Vec<PathBuf> {
    WalkDir::new(root)
        .into_iter()
        .par_bridge()
        .filter_map(|e| e.ok())
        .filter(|e| is_rdf_file(e.path()))
        .map(|e| e.path().to_path_buf())
        .collect()
}
```

#### Template Discovery >50ms

**Problem**: Inefficient directory scanning

**Solutions**:
1. Use `fs::read_dir` instead of `glob`
2. Pre-filter by extension before full stat
3. Cache template list between generations

#### Cache Hit Rate <90%

**Problem**: Poor cache invalidation strategy

**Solutions**:
1. Use more stable cache keys (file path + mtime)
2. Implement cascading cache (memory → disk)
3. Add cache warmup phase

---

## Continuous Integration

### Add to CI Pipeline

`.github/workflows/benchmarks.yml`:
```yaml
name: Performance Benchmarks

on:
  push:
    branches: [main, develop]
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

      - name: Run benchmarks
        run: |
          cargo bench --bench conventions_performance -- --save-baseline main

      - name: Compare with baseline
        run: |
          cargo bench --bench conventions_performance -- --baseline main

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: target/criterion
```

### Performance Regression Detection

```bash
# Create baseline
cargo bench --bench conventions_performance -- --save-baseline baseline

# Make changes...

# Compare with baseline
cargo bench --bench conventions_performance -- --baseline baseline

# Criterion will highlight regressions in red
```

---

## Troubleshooting

### Benchmark Panics

If a benchmark panics with an assertion:

```
thread 'main' panicked at 'RDF discovery took 152ms, target is <100ms for 100 files'
```

**Action**: Performance target not met - optimization required.

### Inconsistent Results

If results vary widely between runs:

**Causes**:
- Background processes consuming CPU
- Thermal throttling
- Disk cache effects

**Solutions**:
```bash
# Close other applications
# Run multiple times and take average
cargo bench --bench conventions_performance -- --sample-size 100

# Increase warmup
# (Edit benchmark to add more iterations)
```

### Out of Memory

For large-scale tests:

**Solution**: Reduce test data size in benchmark parameters.

---

## Advanced Usage

### Custom Sample Size

```rust
// In benches/conventions_performance.rs
group.sample_size(100);  // Increase for more accurate results
```

### Profiling

Run with profiler to find hot paths:

```bash
# macOS
cargo bench --bench conventions_performance --profile-time 5

# Linux with perf
perf record -g cargo bench --bench conventions_performance
perf report
```

### Flamegraphs

```bash
# Install cargo-flamegraph
cargo install flamegraph

# Generate flamegraph
cargo flamegraph --bench conventions_performance
```

---

## Benchmark Maintenance

### When to Update Benchmarks

1. **API changes**: Update benchmark if `ProjectConventions` interface changes
2. **New features**: Add new benchmarks for new convention features
3. **Performance targets change**: Update assertions in benchmark code
4. **New optimization**: Add specific benchmark to validate improvement

### Benchmark Code Location

- **Source**: `/Users/sac/ggen/benches/conventions_performance.rs`
- **Config**: `/Users/sac/ggen/Cargo.toml` (see `[[bench]]` section)
- **Results**: `/Users/sac/ggen/target/criterion/`

---

## Summary

1. **Fix compilation error** in `cli/src/conventions/watcher.rs:162`
2. **Run benchmarks**: `cargo bench --bench conventions_performance`
3. **View results**: `open target/criterion/report/index.html`
4. **Validate targets**: All benchmarks should pass assertions
5. **Optimize if needed**: Follow troubleshooting guide for failing benchmarks

---

**Quick Reference**:
```bash
# Fix build
cargo build --release

# Run all benchmarks
cargo bench --bench conventions_performance

# View HTML report
open target/criterion/report/index.html

# Run specific benchmark
cargo bench --bench conventions_performance -- discover_rdf
```

**Expected Total Runtime**: ~2-3 minutes for all benchmarks

**Output**: HTML report at `target/criterion/report/index.html`
