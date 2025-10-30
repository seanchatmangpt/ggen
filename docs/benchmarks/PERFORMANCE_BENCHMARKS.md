# ggen Performance Benchmarks

Comprehensive performance benchmark suite for ggen marketplace and lifecycle systems using cleanroom (clnrm) for deterministic testing.

## Overview

This benchmark suite provides:

- **Deterministic Testing**: All benchmarks run in cleanroom environments with controlled surfaces
- **Marketplace Performance**: Search, version resolution, and cache operations
- **Lifecycle Performance**: Phase execution, state persistence, and cache validation
- **Stress Testing**: High-load scenarios and concurrent operations
- **Regression Detection**: Baseline metrics for performance tracking
- **Production Readiness**: Performance thresholds for CI/CD validation

## Running Benchmarks

### Run All Benchmarks

```bash
cargo bench --bench clnrm_benchmarks
```

### Run Specific Benchmark Groups

```bash
# Marketplace operations
cargo bench --bench clnrm_benchmarks marketplace

# Lifecycle operations
cargo bench --bench clnrm_benchmarks lifecycle

# Stress tests
cargo bench --bench clnrm_benchmarks stress

# Regression detection
cargo bench --bench clnrm_benchmarks regression

# Cleanroom determinism validation
cargo bench --bench clnrm_benchmarks cleanroom
```

### Run Integration Performance Tests

```bash
cargo test --test marketplace_tests_main --release -- performance
```

## Benchmark Categories

### 1. Marketplace Operations

**Purpose**: Measure performance of package search, version resolution, and cache operations.

**Benchmarks**:
- `marketplace_search_deterministic/simple_query/{100,1000,10000}` - Simple text search across registries
- `marketplace_search_deterministic/complex_query_with_filters/{100,1000,10000}` - Advanced search with category and keyword filters
- `marketplace_version_resolution/resolve_latest_version` - Resolve latest version of a package
- `marketplace_version_resolution/resolve_specific_version` - Resolve specific version
- `marketplace_version_resolution/resolve_semver_compatible` - Find compatible versions using semver
- `marketplace_cache_operations/cache_index_serialization` - Serialize registry index to JSON
- `marketplace_cache_operations/cache_index_deserialization` - Deserialize registry index from JSON

**Performance Targets**:
- Search 1,000 packages: < 50ms
- Search 10,000 packages: < 500ms
- Version resolution: < 10ms
- Index serialization (1,000 packages): < 100ms

### 2. Lifecycle Phase Operations

**Purpose**: Measure performance of lifecycle phase execution, caching, and state management.

**Benchmarks**:
- `lifecycle_phase_execution_deterministic/sequential_phases/{5,10,20}` - Sequential phase execution
- `lifecycle_phase_execution_deterministic/phases_with_hooks/{5,10,20}` - Phase execution with before/after hooks
- `lifecycle_cache_validation/validate_100_cache_entries` - Validate 100 cache entries
- `lifecycle_state_persistence/save_state/{100,500,1000}` - Save lifecycle state with N records
- `lifecycle_state_persistence/load_state/{100,500,1000}` - Load lifecycle state with N records

**Performance Targets**:
- Single phase execution: < 200ms
- Cache validation (100 entries): < 50ms
- State save (1,000 records): < 100ms
- State load (1,000 records): < 50ms

### 3. Stress Tests

**Purpose**: Validate system behavior under high load and concurrent operations.

**Benchmarks**:
- `stress_concurrent_searches/100_concurrent_searches` - 100 parallel search operations
- `stress_high_volume_cache/generate_10000_cache_keys` - Generate 10,000 cache keys
- `stress_large_registry/serialize_50k_packages` - Serialize 50,000 packages
- `stress_large_registry/deserialize_50k_packages` - Deserialize 50,000 packages
- `stress_large_registry/search_50k_packages` - Search across 50,000 packages

**Performance Targets**:
- 100 concurrent searches: < 2s
- 10,000 cache key generation: < 500ms
- 50k package operations: < 5s

### 4. Regression Detection

**Purpose**: Establish baseline metrics for performance tracking in CI/CD.

**Benchmarks**:
- `baseline_operations/baseline_create_1000_packages` - Create 1,000 test packages
- `baseline_operations/baseline_search_1000_packages` - Search 1,000 packages
- `baseline_operations/baseline_cache_key_generation` - Generate cache key
- `baseline_operations/baseline_lifecycle_phase` - Execute single lifecycle phase

**Usage**: Compare benchmark results against baselines to detect performance regressions.

### 5. Cleanroom Determinism

**Purpose**: Validate cleanroom environment performance and determinism guarantees.

**Benchmarks**:
- `cleanroom_determinism/deterministic_surfaces_creation` - Create deterministic surfaces
- `cleanroom_determinism/cleanroom_env_creation` - Create full cleanroom environment
- `cleanroom_determinism/determinism_score_calculation` - Calculate determinism score

## Cleanroom Integration

All benchmarks use cleanroom's deterministic surfaces:

### Deterministic Surfaces

```rust
use ggen_core::cleanroom::CleanroomCore;
use ggen_core::cleanroom::policy::Locked;

let env = CleanroomCore::<Locked>::builder()
    .time_frozen(42)        // Frozen time at seed 42
    .rng_seeded(42)         // Seeded RNG with value 42
    .fs_ephemeral()         // Ephemeral tmpfs filesystem
    .net_offline()          // No network access
    .build()
    .expect("Failed to create cleanroom");
```

### Benefits

1. **Reproducibility**: Same seed produces identical results
2. **Isolation**: No external dependencies or state
3. **Determinism**: Controlled time, RNG, filesystem, network, and process
4. **Debugging**: Forensics and attestation for failed tests

## Performance Thresholds

Performance thresholds are defined in `tests/integration/performance_benchmarks.rs`:

```rust
mod thresholds {
    use std::time::Duration;

    // Marketplace operations
    pub const SEARCH_1000_PACKAGES: Duration = Duration::from_millis(50);
    pub const SEARCH_10000_PACKAGES: Duration = Duration::from_millis(500);
    pub const VERSION_RESOLUTION: Duration = Duration::from_millis(10);

    // Lifecycle operations
    pub const PHASE_EXECUTION: Duration = Duration::from_millis(200);
    pub const CACHE_VALIDATION_100: Duration = Duration::from_millis(50);
    pub const STATE_SAVE_1000: Duration = Duration::from_millis(100);

    // Stress tests
    pub const CONCURRENT_SEARCHES_100: Duration = Duration::from_secs(2);
    pub const CACHE_KEY_GENERATION_10000: Duration = Duration::from_millis(500);
}
```

## CI/CD Integration

### GitHub Actions

```yaml
- name: Run Performance Benchmarks
  run: cargo bench --bench clnrm_benchmarks -- --output-format bencher | tee bench_output.txt

- name: Run Performance Tests
  run: cargo test --release --test marketplace_tests_main -- performance

- name: Check Performance Thresholds
  run: |
    if grep -q "FAILED" bench_output.txt; then
      echo "Performance regression detected!"
      exit 1
    fi
```

### Performance Reports

Criterion generates detailed HTML reports in `target/criterion/`:

```bash
# View HTML reports
open target/criterion/report/index.html
```

## Benchmark Results Structure

```
target/criterion/
├── marketplace_search_deterministic/
│   ├── simple_query/
│   │   ├── 100/
│   │   ├── 1000/
│   │   └── 10000/
│   └── complex_query_with_filters/
├── lifecycle_phase_execution_deterministic/
├── stress_concurrent_searches/
└── baseline_operations/
```

## Performance Metrics

### Key Metrics Collected

1. **Throughput**: Operations per second
2. **Latency**: p50, p95, p99 percentiles
3. **Memory**: Peak memory usage
4. **Determinism**: Variance between runs

### Example Output

```
marketplace_search_deterministic/simple_query/1000
                        time:   [42.3 ms 43.1 ms 43.9 ms]
                        thrpt:  [23.2 Kelem/s 23.6 Kelem/s 24.0 Kelem/s]

lifecycle_phase_execution_deterministic/sequential_phases/10
                        time:   [185 ms 190 ms 196 ms]

stress_concurrent_searches/100_concurrent_searches
                        time:   [1.45 s 1.52 s 1.59 s]
```

## Adding New Benchmarks

### 1. Add Benchmark Function

```rust
fn bench_my_operation(c: &mut Criterion) {
    let mut group = c.benchmark_group("my_operation");

    let env = create_cleanroom_env(42);

    group.bench_function("test_case", |b| {
        b.iter(|| {
            // Your benchmark code here
            black_box(my_operation());
        });
    });

    group.finish();
}
```

### 2. Add to Criterion Group

```rust
criterion_group!(
    my_benches,
    bench_my_operation,
);

criterion_main!(
    marketplace_benches,
    lifecycle_benches,
    my_benches,  // Add here
);
```

### 3. Add Performance Test

```rust
#[test]
fn test_my_operation_performance() {
    let start = Instant::now();
    my_operation();
    let duration = start.elapsed();

    assert!(duration < MY_THRESHOLD);
}
```

## Best Practices

### 1. Use Cleanroom Environments

Always use cleanroom environments for deterministic benchmarks:

```rust
let env = create_cleanroom_env(42);
// Run benchmarks in env.root()
```

### 2. Use `black_box`

Prevent compiler optimizations:

```rust
b.iter(|| {
    black_box(expensive_operation());
});
```

### 3. Set Appropriate Sample Sizes

For expensive operations:

```rust
group.sample_size(10);  // Reduce from default 100
```

### 4. Use Throughput Measurements

For batch operations:

```rust
group.throughput(Throughput::Elements(1000));
```

## Troubleshooting

### Benchmarks Running Too Slowly

1. Check sample size: `group.sample_size(10)`
2. Reduce dataset size for initial testing
3. Use `--bench clnrm_benchmarks -- --quick` for faster runs

### Non-Deterministic Results

1. Verify cleanroom surfaces are properly configured
2. Check for external dependencies (network, filesystem)
3. Ensure RNG is seeded consistently

### Performance Regression

1. Compare with baseline metrics
2. Check recent code changes
3. Profile with `cargo flamegraph`
4. Review algorithm complexity

## Resources

- **Criterion Documentation**: https://bheisler.github.io/criterion.rs/book/
- **Cleanroom Guide**: `/docs/cleanroom/OVERVIEW.md`
- **Performance Tuning**: `/docs/performance/OPTIMIZATION.md`

## Contributing

When adding benchmarks:

1. Follow naming conventions: `bench_<category>_<operation>`
2. Use cleanroom environments for determinism
3. Document performance targets
4. Add integration tests with thresholds
5. Update this documentation

## License

MIT License - See LICENSE file for details
