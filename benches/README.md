# Performance Benchmarks

This directory contains comprehensive performance benchmarks for the ggen code generation framework.

## Benchmark Suites

### Core Benchmarks

1. **`pipeline_performance.rs`** - Core generation pipeline benchmarks
   - Ontology parsing (10, 50, 150, 500 classes)
   - SPARQL query execution (10-1000 entities)
   - Template rendering performance
   - File I/O operations (1KB - 1MB)
   - Memory usage during large generations
   - End-to-end generation time
   - Multi-file generation

2. **`cli_startup_performance.rs`** - CLI performance benchmarks
   - Command startup time
   - Cold vs warm start comparison
   - CLI execution time

3. **`template_benchmarks.rs`** - Template-specific benchmarks (in `crates/ggen-core/benches/`)
   - Template parsing
   - Frontmatter rendering
   - RDF processing
   - SPARQL execution
   - Variable substitution
   - File tree generation
   - Template caching
   - Memory usage
   - Preprocessor integration

### Legacy Benchmarks

- `runtime_overhead.rs` - Runtime overhead measurements
- `async_runtime_benchmarks.rs` - Async runtime performance
- `memory_profiling.rs` - Memory profiling utilities
- `quick_runtime_validation.rs` - Quick validation tests
- `conventions_performance.rs` - Naming convention performance
- `marketplace_performance.rs` - Marketplace operations

## Test Fixtures

The `fixtures/` directory contains test data for benchmarks:

### Ontologies (`fixtures/ontologies/`)

- `small_ontology.ttl` - 10 classes
- `medium_ontology.ttl` - 50 classes
- `large_ontology.ttl` - 150 classes
- `very_large_ontology.ttl` - 500 classes

These fixtures are used to benchmark ontology parsing and SPARQL query performance with realistic data.

## Running Benchmarks

### All Benchmarks

```bash
cargo bench
```

### Specific Suite

```bash
# Core pipeline benchmarks
cargo bench --bench pipeline_performance

# CLI startup benchmarks
cargo bench --bench cli_startup_performance

# Template benchmarks
cargo bench --bench template_benchmarks
```

### Specific Test

```bash
# Run only ontology parsing benchmarks
cargo bench --bench pipeline_performance -- ontology_parsing

# Run only SPARQL benchmarks
cargo bench --bench pipeline_performance -- sparql_query_execution

# Run only CLI startup
cargo bench --bench cli_startup_performance -- cli_startup
```

### With Baseline Comparison

```bash
# Save current results as baseline
cargo bench -- --save-baseline main

# Compare against baseline
cargo bench -- --baseline main
```

## Viewing Results

### HTML Reports

Criterion generates interactive HTML reports:

```bash
# Open main report
open target/criterion/report/index.html

# Open specific benchmark
open target/criterion/ontology_parsing/report/index.html
```

Reports include:
- Statistical analysis
- Performance graphs
- Historical trends
- Regression detection

### Terminal Output

Benchmark results are printed to the terminal with:
- Execution time (mean ± std dev)
- Change from baseline
- Statistical confidence

## CI Integration

Benchmarks are automatically run in CI via `.github/workflows/performance.yml`:

- **On Push to Main**: Establishes performance baselines
- **On Pull Requests**: Compares against main branch
- **Regression Detection**: Alerts on >25% degradation
- **Artifact Storage**: HTML reports stored for 30 days

## Performance Targets

### Ontology Parsing

| Size | Classes | Target | Threshold |
|------|---------|--------|-----------|
| Small | 10 | < 1ms | 1.25ms |
| Medium | 50 | < 5ms | 6.25ms |
| Large | 150 | < 20ms | 25ms |
| Very Large | 500 | < 100ms | 125ms |

### SPARQL Queries

| Entities | Target | Threshold |
|----------|--------|-----------|
| 10 | < 1ms | 1.25ms |
| 100 | < 10ms | 12.5ms |
| 1000 | < 100ms | 125ms |

### Template Rendering

| Complexity | Target | Threshold |
|------------|--------|-----------|
| 10 vars + 10 loops | < 1ms | 1.25ms |
| 100 vars + 100 loops | < 10ms | 12.5ms |
| 500 vars + 100 loops | < 50ms | 62.5ms |

### CLI Startup

| Command | Target | Threshold |
|---------|--------|-----------|
| Help | < 100ms | 125ms |
| Version | < 50ms | 62.5ms |
| Cold Start | < 200ms | 250ms |

## Adding New Benchmarks

1. Create benchmark file in `benches/`:

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_my_feature(c: &mut Criterion) {
    c.bench_function("my_feature", |b| {
        b.iter(|| {
            black_box(my_feature())
        });
    });
}

criterion_group!(benches, bench_my_feature);
criterion_main!(benches);
```

2. Add to `Cargo.toml`:

```toml
[[bench]]
name = "my_feature"
harness = false
```

3. Add test fixtures if needed in `fixtures/`

4. Document in this README and `docs/BENCHMARKING.md`

## Best Practices

- ✅ Use `black_box()` to prevent compiler optimizations
- ✅ Set appropriate throughput metrics
- ✅ Use realistic test data
- ✅ Adjust sample size for expensive operations
- ✅ Document performance characteristics
- ✅ Include regression thresholds

## Troubleshooting

### Build Issues

If you encounter build errors related to dependencies:

```bash
cargo clean
cargo build --release --benches
```

### High Variability

To reduce variance in benchmark results:

1. Close other applications
2. Disable CPU frequency scaling (Linux):
   ```bash
   echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
   ```
3. Increase sample size in benchmark code:
   ```rust
   group.sample_size(200);
   ```

### Missing Fixtures

Ensure benchmark fixtures exist:

```bash
ls -lh benches/fixtures/ontologies/
```

If missing, regenerate using the Python scripts in the documentation.

## Further Reading

- [Full Benchmarking Guide](../docs/BENCHMARKING.md)
- [Criterion.rs Documentation](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)

## Contributing

When adding new benchmarks:

1. Follow existing patterns and naming conventions
2. Add comprehensive documentation
3. Include performance targets and thresholds
4. Ensure CI tests pass
5. Update this README
