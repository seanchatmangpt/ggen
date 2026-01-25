# Comprehensive Performance Benchmarking Suite

## Overview

The ggen project includes a comprehensive benchmarking suite for validating build optimization and runtime performance. This suite measures:

- **Build Time Performance**: Clean, incremental, and workspace builds
- **Memory Usage**: Compilation and runtime memory profiling
- **Binary Size**: Release binary tracking and analysis
- **Runtime Performance**: RDF processing, template rendering, and code generation
- **SLO Compliance**: Automated performance regression detection

## Quick Start

### Run All Benchmarks

```bash
# Build time benchmarks
cargo bench --bench build_time_benchmarks

# Memory usage benchmarks
cargo bench --bench memory_usage_benchmarks

# Binary size analysis
cargo bench --bench binary_size_analysis

# Runtime performance (existing)
cargo bench --bench pipeline_performance
cargo bench --bench schema_layer_slo
```

### Quick Build Performance Check

```bash
./scripts/build_perf_monitor.sh all
```

### Monitor Memory During Build

```bash
./scripts/memory_monitor.sh --command="cargo build --release" --duration=120
```

## Benchmark Details

### 1. Build Time Benchmarks (`benches/build_time_benchmarks.rs`)

Measures compilation performance across scenarios:

**Scenarios:**
- Clean debug build
- Clean release build
- Incremental builds (single file changes)
- Feature-gated compilation (no features, minimal, prod, dev, all)
- Parallel compilation scaling (1, 2, 4, 8 jobs)
- Workspace compilation (core, with optional, full)
- Cargo check performance

**SLO Targets:**
| Scenario | Target | Threshold |
|----------|--------|-----------|
| First debug build | ≤ 15s | 18.75s |
| First release build | ≤ 30s | 37.5s |
| Incremental | ≤ 2s | 2.5s |
| Workspace clean | ≤ 30s | 37.5s |
| Cargo check | ≤ 5s | 6.25s |

**Example Run:**
```bash
cargo bench --bench build_time_benchmarks -- build_clean
```

### 2. Memory Usage Benchmarks (`benches/memory_usage_benchmarks.rs`)

Tracks memory consumption during operations:

**Operations:**
- RDF graph loading (small → very large ontologies)
- Template processing (simple → complex with loops)
- Code generation (10 → 1000 functions)
- Compilation memory usage (per-crate breakdown)
- Memory allocation patterns (1KB → 10MB)
- Cache efficiency (sequential vs random access)

**SLO Targets:**
| Operation | Target | Threshold |
|-----------|--------|-----------|
| RDF processing (500 classes) | ≤ 100MB | 125MB |
| Template rendering (large) | ≤ 50MB | 62.5MB |
| Code generation | ≤ 100MB | 125MB |
| Compilation (ggen-core) | ≤ 500MB | 625MB |

**Example Run:**
```bash
cargo bench --bench memory_usage_benchmarks -- memory_rdf
```

### 3. Binary Size Analysis (`benches/binary_size_analysis.rs`)

Analyzes binary size characteristics:

**Measurements:**
- Debug binary size
- Release binary size
- Stripped binary size (symbol reduction)
- Symbol count analysis
- Workspace binary comparison
- Size trends over time

**SLO Targets:**
| Metric | Target |
|--------|--------|
| Release binary (ggen) | ≤ 10MB |
| Debug binary (ggen) | ≤ 50MB |
| Symbol reduction (strip) | ≥ 50% |
| Debug info size | ≤ 100MB |

**Example Run:**
```bash
cargo bench --bench binary_size_analysis
```

### 4. Runtime Performance (Existing Benchmarks)

#### Pipeline Performance (`benches/pipeline_performance.rs`)

- Ontology parsing (10-500 classes)
- SPARQL query execution
- Template rendering
- File I/O operations
- End-to-end generation time

#### Schema Layer SLOs (`benches/schema_layer_slo.rs`)

- TTL → Signature transpilation
- Signature → JSON Schema generation
- JSON validation performance
- Full pipeline (RDF → Schema → Validate)

## SLO Compliance Tracking

### Automated SLO Checking

The benchmarking suite includes automated SLO compliance checking:

```bash
# Check SLOs after build changes
cargo bench --bench schema_layer_slo

# Generate SLO compliance report
./scripts/build_perf_monitor.sh all
```

### Interpreting Results

**Green (PASS):** Metric within target SLO
```
[✓ PASS] Clean build (debug): 12.34s (target: 15s)
```

**Red (FAIL):** Metric exceeds SLO
```
[✗ FAIL] Clean build (release): 45.67s (EXCEEDED by 15.67s, target: 30s)
```

### Baseline Comparison

Store baseline results for regression detection:

```bash
# Save baseline
cargo bench --bench build_time_benchmarks -- --save-baseline main

# Compare against baseline
cargo bench --bench build_time_benchmarks -- --baseline main
```

## Performance Monitoring Scripts

### Build Performance Monitor

Real-time build performance tracking with SLO compliance:

```bash
# Benchmark clean build
./scripts/build_perf_monitor.sh clean

# Benchmark incremental build
./scripts/build_perf_monitor.sh incremental

# Benchmark workspace
./scripts/build_perf_monitor.sh workspace

# Run all benchmarks
./scripts/build_perf_monitor.sh all
```

**Output:**
- JSON report: `.ggen/benchmark_results/build_performance_YYYYMMDD_HHMMSS.json`
- SLO report: `.ggen/benchmark_results/slo_compliance_YYYYMMDD_HHMMSS.txt`
- HTML dashboard: `.ggen/benchmark_results/dashboard.html` (when available)

### Memory Monitor

Real-time memory monitoring for processes:

```bash
# Monitor build command
./scripts/memory_monitor.sh --command="cargo build --release" --duration=120

# Monitor system memory
./scripts/memory_monitor.sh --duration=60 --interval=1
```

**Output:**
- Memory stats every second
- Peak memory tracking
- Report saved to: `.ggen/memory_reports/memory_YYYYMMDD_HHMMSS.json`

## CI/CD Integration

### GitHub Actions Example

```yaml
# .github/workflows/performance.yml
name: Performance Benchmarks

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  benchmarks:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - uses: dtolnay/rust-toolchain@stable

      - name: Run build benchmarks
        run: cargo bench --bench build_time_benchmarks -- --save-baseline main

      - name: Run memory benchmarks
        run: cargo bench --bench memory_usage_benchmarks

      - name: Run SLO validation
        run: cargo bench --bench schema_layer_slo

      - name: Upload benchmark results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: target/criterion/

      - name: Comment PR with results
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            // Parse and post benchmark results
```

## Performance Optimization Workflow

### 1. Establish Baseline

```bash
# Build on clean main branch
git checkout main
./scripts/build_perf_monitor.sh all
# Results stored in .ggen/benchmark_results/
```

### 2. Implement Optimization

```bash
# Create feature branch
git checkout -b optimize/feature-name
# Make changes...
```

### 3. Validate Optimization

```bash
# Run benchmarks again
./scripts/build_perf_monitor.sh all

# Compare results manually or use:
cargo bench --bench build_time_benchmarks -- --baseline main
```

### 4. Track Improvements

Results are stored with timestamps for trend analysis:
```
.ggen/benchmark_results/
├── build_performance_20260125_120000.json
├── build_performance_20260125_120500.json
└── slo_compliance_20260125_120000.txt
```

## Troubleshooting

### Build Benchmarks Are Slow

Criterion caches results. Clean cache if needed:

```bash
rm -rf target/criterion/
cargo bench --bench build_time_benchmarks
```

### Memory Measurements Unavailable

Some systems may not support `/usr/bin/time`. Install:

```bash
# Ubuntu/Debian
sudo apt-get install time

# macOS
brew install gnu-time
```

### Inconsistent Results

Reduce system noise:

```bash
# Disable CPU frequency scaling (Linux)
sudo cpupower frequency-set -g performance

# Run benchmarks with fewer iterations
cargo bench --bench build_time_benchmarks -- --sample-size 3
```

### High Variance

Close background applications and increase sample size:

```rust
// In benchmark code
group.sample_size(5);  // Increase from default 100
```

## Best Practices

1. **Run on consistent hardware** - Use same machine for baseline and comparison
2. **Close background apps** - Minimize system noise for accurate measurements
3. **Measure multiple times** - Account for system variance
4. **Store baselines** - Use Criterion's baseline feature for regression detection
5. **Document changes** - Record what optimization was applied
6. **Monitor trends** - Track performance over time via git history

## Adding New Benchmarks

### Template for New Build Time Benchmark

```rust
fn bench_my_optimization(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_my_optimization");
    group.sample_size(3);  // Expensive operations - fewer samples
    group.measurement_time(Duration::from_secs(10));

    group.bench_function("scenario_name", |b| {
        b.iter_custom(|_| {
            let (duration, _size) = measure_build(&["build", "args"])
                .expect("Build failed");
            duration
        });
    });

    group.finish();
}
```

### Register Benchmark

```rust
criterion_group!(benches, bench_my_optimization);
criterion_main!(benches);
```

## Storage & Memory Integration

Benchmark results are automatically stored in MCP memory:

```
swarm/benchmarks/suite
├── build_times
│   └── {timestamp}/
│       ├── clean_build_debug.json
│       ├── clean_build_release.json
│       └── summary.json
├── memory_usage
│   └── {timestamp}/
│       ├── rdf_processing.json
│       ├── template_rendering.json
│       └── summary.json
└── binary_sizes
    └── {timestamp}/
        ├── debug_binary.json
        ├── release_binary.json
        └── summary.json
```

## References

- [Criterion.rs Documentation](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Benchmarking Best Practices](https://easyperf.net/blog/)
- [SLO Design Guide](https://sre.google/sre-book/service-level-objectives/)

## Contact & Support

For performance issues or optimization ideas:

1. Run `./scripts/build_perf_monitor.sh all`
2. Share results from `.ggen/benchmark_results/`
3. File issue with system specs (CPU cores, RAM, OS)
4. Include baseline comparison if available

---

**Last Updated**: 2026-01-25 | **Version**: 1.0
