# ggen Performance Benchmarks Documentation

Comprehensive documentation for ggen performance benchmarking suite.

## 📚 Documentation Index

### Core Documentation

1. **[PERFORMANCE_BENCHMARKS.md](PERFORMANCE_BENCHMARKS.md)** - Complete benchmark suite documentation
   - Overview of all benchmark categories
   - How to run benchmarks
   - Cleanroom integration
   - CI/CD integration
   - Best practices

2. **[BASELINE_METRICS.md](BASELINE_METRICS.md)** - Baseline performance metrics
   - Reference performance numbers
   - Hardware scaling expectations
   - Performance thresholds (green/yellow/red)
   - Regression detection guidelines
   - Comparison with industry standards

3. **[QUICK_START.md](QUICK_START.md)** - Quick start guide
   - 5-minute benchmark overview
   - Essential commands
   - Common use cases

## 🚀 Quick Links

### Run Benchmarks

```bash
# Run all benchmarks
cargo bench --bench clnrm_benchmarks

# Run specific category
cargo bench --bench clnrm_benchmarks marketplace
cargo bench --bench clnrm_benchmarks lifecycle
cargo bench --bench clnrm_benchmarks stress

# Run performance tests with thresholds
cargo test --release --test marketplace_tests_main -- performance
```

### View Results

```bash
# Open HTML reports
open target/criterion/report/index.html

# View text output
cat target/criterion/*/*/report/index.txt
```

## 📊 Benchmark Categories

### 1. Marketplace Operations
- Package search (simple and complex queries)
- Version resolution (latest, specific, semver)
- Cache operations (serialization/deserialization)

**File**: `ggen-core/benches/clnrm_benchmarks.rs`

### 2. Lifecycle Phase Operations
- Phase execution (sequential and parallel)
- Cache validation
- State persistence (save/load)

**File**: `ggen-core/benches/clnrm_benchmarks.rs`

### 3. Stress Tests
- Concurrent searches (100+ parallel operations)
- High-volume cache operations (10K+ keys)
- Large registry operations (50K+ packages)

**File**: `ggen-core/benches/clnrm_benchmarks.rs`

### 4. Regression Detection
- Baseline operation metrics
- Historical comparison
- Performance threshold validation

**File**: `ggen-core/benches/clnrm_benchmarks.rs`

### 5. Cleanroom Determinism
- Environment creation overhead
- Determinism score calculation
- Surface validation

**File**: `ggen-core/benches/clnrm_benchmarks.rs`

## 🧪 Testing Integration

### Performance Integration Tests

**File**: `ggen-core/tests/integration/performance_benchmarks.rs`

These tests validate performance thresholds in CI:

```rust
#[test]
fn test_marketplace_search_performance() {
    // Validates search completes within threshold
}

#[test]
fn test_lifecycle_phase_execution_performance() {
    // Validates phase execution within threshold
}
```

### Run Performance Tests

```bash
# Run all performance tests
cargo test --release performance

# Run specific test
cargo test --release test_marketplace_search_performance
```

## 🏗️ Cleanroom Integration

All benchmarks use cleanroom for deterministic testing:

### Benefits

1. **Reproducibility**: Same seed = same results
2. **Isolation**: No external dependencies
3. **Determinism**: Controlled time, RNG, FS, network
4. **Debugging**: Forensics and attestation

### Example

```rust
use ggen_core::cleanroom::CleanroomCore;
use ggen_core::cleanroom::policy::Locked;

let env = CleanroomCore::<Locked>::builder()
    .time_frozen(42)
    .rng_seeded(42)
    .fs_ephemeral()
    .net_offline()
    .build()
    .expect("Failed to create cleanroom");
```

## 📈 Performance Thresholds

### Green (Pass)
- Marketplace Search (1K): < 50ms
- Phase Execution: < 200ms
- Cache Validation (100): < 50ms

### Yellow (Warning)
- 10% above green thresholds

### Red (Fail)
- 20% above green thresholds

See [BASELINE_METRICS.md](BASELINE_METRICS.md) for complete thresholds.

## 🔄 CI/CD Integration

### GitHub Actions Example

```yaml
- name: Run Performance Benchmarks
  run: |
    cargo bench --bench clnrm_benchmarks -- --save-baseline ci-${{ github.sha }}

- name: Validate Performance Thresholds
  run: |
    cargo test --release --test marketplace_tests_main -- performance
```

### Regression Detection

```bash
# Save baseline
cargo bench --bench clnrm_benchmarks -- --save-baseline main

# Compare against baseline
cargo bench --bench clnrm_benchmarks -- --baseline main

# Store results
mkdir -p benchmarks/results
cp -r target/criterion benchmarks/results/$(date +%Y%m%d)
```

## 📊 Benchmark Results

### Directory Structure

```
target/criterion/
├── marketplace_search_deterministic/
│   ├── simple_query/
│   │   ├── 100/
│   │   │   ├── base/
│   │   │   ├── new/
│   │   │   └── report/
│   │   ├── 1000/
│   │   └── 10000/
│   └── complex_query_with_filters/
├── lifecycle_phase_execution_deterministic/
├── stress_concurrent_searches/
└── baseline_operations/
```

### Output Format

```
marketplace_search_deterministic/simple_query/1000
                        time:   [42.3 ms 43.1 ms 43.9 ms]
                        change: [-2.3% +0.5% +3.4%] (p = 0.42 > 0.05)
                        thrpt:  [23.2 Kelem/s 23.6 Kelem/s 24.0 Kelem/s]
```

## 🛠️ Development Workflow

### Adding New Benchmarks

1. **Add benchmark function** in `clnrm_benchmarks.rs`
2. **Add to criterion group** at bottom of file
3. **Add performance test** in `performance_benchmarks.rs`
4. **Update documentation** in this directory
5. **Run and validate**: `cargo bench`

### Performance Optimization

1. **Profile with flamegraph**:
   ```bash
   cargo flamegraph --bench clnrm_benchmarks
   ```

2. **Identify bottlenecks**:
   ```bash
   cargo bench --bench clnrm_benchmarks -- --profile-time=10
   ```

3. **Compare before/after**:
   ```bash
   cargo bench -- --save-baseline before
   # Make changes
   cargo bench -- --baseline before
   ```

## 📝 Best Practices

### 1. Use Deterministic Environments

Always use cleanroom for consistent results:

```rust
let env = create_cleanroom_env(42);
```

### 2. Prevent Compiler Optimizations

Use `black_box` to prevent dead code elimination:

```rust
b.iter(|| {
    black_box(expensive_operation());
});
```

### 3. Set Sample Sizes Appropriately

For expensive operations:

```rust
group.sample_size(10);  // Reduce from default 100
```

### 4. Use Throughput Measurements

For batch operations:

```rust
group.throughput(Throughput::Elements(1000));
```

### 5. Document Performance Targets

Always document expected performance in tests:

```rust
const THRESHOLD: Duration = Duration::from_millis(50);
assert!(duration < THRESHOLD);
```

## 🔍 Troubleshooting

### Benchmarks Too Slow

1. Reduce sample size: `group.sample_size(10)`
2. Use smaller datasets for testing
3. Run quick mode: `--quick`

### Non-Deterministic Results

1. Verify cleanroom surfaces are configured
2. Check for external dependencies
3. Ensure RNG is seeded

### Performance Regression

1. Compare with baseline: `--baseline main`
2. Profile with flamegraph
3. Check recent commits
4. Review algorithm changes

## 📚 Additional Resources

### Internal Documentation

- **Cleanroom Guide**: `/docs/cleanroom/OVERVIEW.md`
- **Performance Tuning**: `/docs/performance/OPTIMIZATION.md`
- **Testing Strategy**: `/docs/testing/STRATEGY.md`

### External Resources

- **Criterion.rs**: https://bheisler.github.io/criterion.rs/book/
- **Rust Performance Book**: https://nnethercote.github.io/perf-book/
- **Flamegraph**: https://github.com/flamegraph-rs/flamegraph

## 🤝 Contributing

When contributing benchmarks:

1. Follow naming conventions
2. Use cleanroom environments
3. Document performance targets
4. Add integration tests with thresholds
5. Update documentation

## 📄 License

MIT License - See LICENSE file for details

---

**Last Updated**: 2025-10-17
**Maintained by**: Performance Benchmarking Engineer
