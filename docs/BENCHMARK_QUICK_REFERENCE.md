# Benchmark Quick Reference

## Running Benchmarks

### All Benchmarks
```bash
cd ggen-core
cargo bench --bench lifecycle_benchmarks
```

### Specific Categories
```bash
# Workspace execution only
cargo bench --bench lifecycle_benchmarks -- workspace_execution

# Cache benchmarks only
cargo bench --bench lifecycle_benchmarks -- cache

# Hook benchmarks only
cargo bench --bench lifecycle_benchmarks -- hook

# State persistence only
cargo bench --bench lifecycle_benchmarks -- state
```

### Baseline Management
```bash
# Save current performance as baseline
cargo bench --bench lifecycle_benchmarks --save-baseline main

# Compare against saved baseline
cargo bench --bench lifecycle_benchmarks --baseline main

# List all saved baselines
ls target/criterion/*/*/base/
```

## Viewing Results

### HTML Reports
```bash
# Open main report index
open target/criterion/report/index.html

# Open specific benchmark
open target/criterion/workspace_execution/parallel/2/report/index.html
```

### Command Line Output
Criterion shows summary statistics after each benchmark:
```
workspace_execution/parallel/2
  time:   [113.63 ms 115.09 ms 116.28 ms]
          ^^^^^^^^   ^^^^^^^^   ^^^^^^^^
          min        mean       max
```

## Interpreting Results

### Performance Changes
```
change: [-2.5% -1.2% +0.3%]
        ^^^^^^  ^^^^^  ^^^^^
        lower   estimate upper
        bound            bound
```

### Regression Detection
```
change: [+12.5% +14.2% +15.9%] (p < 0.01)
                                 ^^^^^^^^
Performance has regressed.      statistical
                                significance
```

### Improvement Detection
```
change: [-15.9% -14.2% -12.5%] (p < 0.01)
Performance has improved.
```

## Benchmark Targets

### 1. Parallel Speedup
- **Target**: 2-5x faster than sequential
- **Current**: 1.89x for 2 workspaces ✅
- **File**: `target/criterion/workspace_execution/`

### 2. Cache Operations
- **Target**: <20 μs per operation
- **File**: `target/criterion/cache_*/`

### 3. Hook Overhead
- **Target**: <5 ms excluding command execution
- **File**: `target/criterion/hook_execution/`

### 4. State Persistence
- **Target**: <10 ms for 100 records
- **File**: `target/criterion/state_*/`

## CI Integration

### GitHub Actions Example
```yaml
name: Performance Benchmarks

on:
  push:
    branches: [main]
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
        run: cargo bench --bench lifecycle_benchmarks -- --save-baseline pr

      - name: Store benchmark results
        uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'cargo'
          output-file-path: target/criterion/*/new/estimates.json
```

## Troubleshooting

### Benchmarks Taking Too Long
- Reduce sample size: Edit `group.sample_size(10)` in benchmark code
- Run fewer iterations: `--bench lifecycle_benchmarks -- --sample-size 10`

### Noisy Results
- Close background applications
- Disable CPU frequency scaling
- Run on dedicated benchmark machine

### Build Errors
```bash
# Clean and rebuild
cargo clean
cargo bench --bench lifecycle_benchmarks
```

## Advanced Usage

### Custom Measurement Time
```bash
# Shorter warm-up and measurement (faster, less accurate)
cargo bench --bench lifecycle_benchmarks -- --warm-up-time 1 --measurement-time 5

# Longer measurement (slower, more accurate)
cargo bench --bench lifecycle_benchmarks -- --warm-up-time 5 --measurement-time 15
```

### Profiling Integration
```bash
# Generate flamegraph for specific benchmark
cargo bench --bench lifecycle_benchmarks -- --profile-time 10 workspace_execution/parallel/2
```

### Export Data
```bash
# CSV export
cargo bench --bench lifecycle_benchmarks -- --save-baseline export --export-json

# Copy JSON results
cp target/criterion/*/new/estimates.json benchmark_results.json
```

## Benchmark File Structure

```
ggen-core/benches/lifecycle_benchmarks.rs
├── workspace_execution
│   ├── create_test_workspace_make() - Setup helper
│   ├── setup_workspace_dirs() - Directory creation
│   └── bench_sequential_vs_parallel() - Main benchmark
├── cache_performance
│   ├── bench_cache_key_generation() - Key creation speed
│   ├── bench_cache_hit_vs_miss() - Validation latency
│   └── bench_cache_memory_usage() - Memory overhead
├── hook_execution
│   ├── create_make_with_hooks() - Setup helper
│   ├── bench_hook_execution() - Hook overhead
│   └── bench_hook_recursion_detection() - Recursion check
└── state_persistence
    ├── bench_state_save() - Serialization speed
    ├── bench_state_load() - Deserialization speed
    └── bench_state_size() - Storage efficiency
```

## Key Metrics

### Workspace Execution
- **Sequential (2)**: 217.54 ms
- **Parallel (2)**: 115.09 ms
- **Speedup**: 1.89x

### Cache Performance
- **Key generation**: <15 μs
- **Cache validation**: <1 μs
- **Memory per key**: ~128 bytes

### Hook Overhead
- **Recursion check**: <1 μs
- **Single hook**: ~100 ms (mostly execution)
- **10 hooks**: ~1000 ms

### State Persistence
- **100 records save**: ~2 ms
- **100 records load**: ~1.5 ms
- **File size**: ~180 bytes/record

## Links

- **Criterion Documentation**: https://bheisler.github.io/criterion.rs/book/
- **HTML Reports**: `target/criterion/report/index.html`
- **Benchmark Source**: `ggen-core/benches/lifecycle_benchmarks.rs`
- **Full Report**: `docs/BENCHMARK_REPORT.md`
