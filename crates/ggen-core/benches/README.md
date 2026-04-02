# Pattern Performance Benchmarks

Comprehensive performance benchmarking suite for all TDD patterns using Criterion.

## Quick Start

```bash
# Run all pattern benchmarks
./scripts/run_pattern_benchmarks.sh

# Run specific benchmark suites
cd crates/ggen-core
cargo bench --bench pattern_performance
cargo bench --bench memory_profiling
cargo bench --bench regression_detection

# Validate performance targets
./scripts/validate_benchmark_targets.sh
```

## Benchmark Suites

### 1. Pattern Performance (`pattern_performance.rs`)

Measures latency and throughput for all documented TDD patterns:

**Error Fix Patterns** (Target: <50ms)
- E0277 trait bound fixes
- E0308 type mismatch fixes
- E0283 type annotation fixes
- E0599 method not found fixes
- Batch error processing

**Poka-Yoke Patterns** (Target: <1μs, zero runtime cost)
- Type-safe builder construction
- Phantom type state machines
- Trait bound method resolution
- Zero-copy operations

**Lean Test Patterns** (Target: <100ms)
- TestFixtureBuilder creation (<5ms)
- Setup/teardown operations (<10ms)
- State isolation verification
- Test execution performance

**Gemba Walk Patterns** (Target: <1ms scoring, <5% overhead)
- Test quality score calculation
- Observability tracking overhead
- Quality report generation (100 tests)

**FMEA Patterns** (Target: <1μs per error)
- RPN (Risk Priority Number) calculation
- Distribution analysis (252 errors in <100μs)
- Priority ranking (<1ms)

### 2. Memory Profiling (`memory_profiling.rs`)

Tracks memory allocations and verifies zero-copy patterns:

**Allocation Tracking**
- Total bytes allocated/deallocated
- Allocation counts
- Memory leak detection (<100 bytes leaked allowed)

**Zero-Copy Verification**
- Slice operations (target: 0 allocations)
- Reference passing (target: 0 allocations)
- Comparison with allocating patterns

**Builder Overhead**
- Memory usage per builder operation
- Allocation overhead percentage (<1% target)

**Fixture Memory**
- Test fixture allocation patterns
- Cleanup verification
- Leak detection across 1000 iterations

### 3. Regression Detection (`regression_detection.rs`)

Validates all performance targets with automated assertions:

**Target Validation**
- Compares actual vs target latencies
- Fails benchmark if targets exceeded
- Provides margin of safety metrics

**Baseline Comparison**
- Establishes performance baselines
- Detects regressions in CI/CD
- Tracks performance trends over time

**Performance Guarantees**
```rust
assert!(avg_latency <= target,
    "Exceeded target: {:?} > {:?}", avg_latency, target);
```

## Performance Targets

| Category | Metric | Target | Rationale |
|----------|--------|--------|-----------|
| Error Fix | Latency | <50ms | User-perceptible delay threshold |
| Builder | Construction | <1μs per field | Near-zero overhead |
| Phantom Type | Runtime cost | Zero | Compile-time only |
| Fixture | Creation | <5ms | Test suite scalability |
| Setup/Teardown | Overhead | <10ms | Per-test overhead budget |
| Test Execution | Duration | <100ms | Fast feedback loop |
| Score Calc | Latency | <1ms | Real-time quality assessment |
| Report Gen | 100 tests | <100ms | Dashboard responsiveness |
| RPN Calc | Per error | <1μs | FMEA scalability |
| Distribution | 252 errors | <100μs | Batch analysis performance |

## Running Benchmarks

### Local Development

```bash
# Full benchmark suite with reports
./scripts/run_pattern_benchmarks.sh

# Individual suites
cargo bench --bench pattern_performance
cargo bench --bench memory_profiling
cargo bench --bench regression_detection

# Save baseline for comparison
cargo bench -- --save-baseline my_baseline

# Compare against baseline
cargo bench -- --baseline my_baseline

# Run specific benchmark
cargo bench --bench pattern_performance -- E0277
```

### Continuous Integration

GitHub Actions workflow automatically runs benchmarks on:
- Every push to main/master/develop
- Every pull request
- Weekly schedule (Mondays at 00:00 UTC)
- Manual workflow dispatch

See `.github/workflows/pattern-benchmarks.yml`

## Interpreting Results

### Criterion Output

```
E0277_trait_bound_fix
                        time:   [23.456 ms 24.123 ms 24.789 ms]
                        change: [-5.2% -3.1% -1.2%] (p = 0.001 < 0.05)
                        Performance has improved.
```

**Key metrics**:
- **time**: [lower bound, estimate, upper bound] with 95% confidence
- **change**: Performance change vs baseline
- **p-value**: Statistical significance (p < 0.05 = significant)

### Validation Output

```
✓ E0277 Fix: 23.4ms (53% under target of 50ms)
✓ Builder Construction: 743ns (26% under target of 1μs)
✗ Score Calculation: 1.2ms (20% over target of 1ms)
```

**Status indicators**:
- `✓` = Target met
- `⚠` = Warning (no data or minor issue)
- `✗` = Target exceeded (regression)

### HTML Reports

Criterion generates detailed HTML reports at:
```
crates/ggen-core/target/criterion/index.html
```

Features:
- Performance distribution charts
- Regression analysis graphs
- Statistical summaries
- Historical comparisons

## Memory Profiling

### Custom Allocator

Uses `TrackingAllocator` to monitor allocations:

```rust
#[global_allocator]
static GLOBAL: TrackingAllocator = TrackingAllocator;
```

### Metrics Collected

- **Allocated**: Total bytes allocated
- **Deallocated**: Total bytes freed
- **Count**: Number of allocations
- **Leaked**: `allocated - deallocated`

### Zero-Copy Verification

```
Zero-copy: 0 bytes allocated, 0 allocations
Allocating: 10240 bytes allocated, 1 allocations
```

Verifies zero-copy patterns have no allocations.

## Regression Detection

### Automated Assertions

Every benchmark includes assertions:

```rust
assert!(meets_target(avg_latency, TARGET_MS),
    "Exceeded {}ms target: {:?}", TARGET_MS, avg_latency);
```

**Failure behavior**:
- Benchmark fails if target exceeded
- CI/CD pipeline fails
- Performance regression blocked

### Baseline Management

```bash
# Establish baseline after optimization
cargo bench -- --save-baseline optimized_v1

# Run future benchmarks against baseline
cargo bench -- --baseline optimized_v1

# Compare two baselines
cargo bench -- --baseline before --compare-baseline after
```

## Adding New Benchmarks

1. **Identify performance-critical pattern**
2. **Define target (latency, overhead, allocations)**
3. **Add benchmark function**:

```rust
pub fn benchmark_my_pattern(c: &mut Criterion) {
    let mut group = c.benchmark_group("my_pattern");

    group.bench_function("operation_name", |b| {
        b.iter(|| {
            // Code to benchmark
            my_operation(black_box(input))
        })
    });

    group.finish();
}
```

4. **Add to criterion_group!**:

```rust
criterion_group!(
    benches,
    // ... existing benchmarks
    my_pattern::benchmark_my_pattern,
);
```

5. **Update targets in `regression_detection.rs`**:

```rust
const MY_PATTERN_TARGET_MS: u128 = 10;
```

6. **Document in `PATTERN_BENCHMARK_SPECIFICATION.md`**

## Troubleshooting

### Benchmark Compilation Errors

```bash
# Clean and rebuild
cargo clean
cargo bench --bench pattern_performance
```

### Inconsistent Results

```bash
# Increase sample size and measurement time
# In benchmark code:
group.sample_size(1000);
group.measurement_time(Duration::from_secs(10));
```

### Memory Profiling Issues

```bash
# Ensure tracking allocator is enabled
# Check for other global allocators
# Review allocation/deallocation patterns
```

### CI/CD Failures

```bash
# Check GitHub Actions logs
# Compare local vs CI results
# Verify target values are reasonable
# Check for platform-specific issues
```

## Best Practices

### Writing Benchmarks

1. **Use `black_box`** to prevent compiler optimizations:
   ```rust
   b.iter(|| my_function(black_box(input)))
   ```

2. **Isolate operations**:
   - Benchmark only the critical path
   - Exclude setup/teardown from timing

3. **Use representative data**:
   - Realistic input sizes
   - Typical usage patterns

4. **Warm up**:
   - Run operations before measuring
   - Stabilize caches and branch predictors

### Setting Targets

1. **User-perceptible delays** (UI/UX):
   - <100ms = Feels instant
   - <1s = Acceptable
   - >1s = Noticeable lag

2. **Compile-time checks**:
   - Zero runtime cost (phantom types)
   - Const evaluation only

3. **Memory allocations**:
   - Zero-copy: 0 allocations
   - Builder patterns: <1% overhead
   - Test fixtures: Minimal, no leaks

4. **Batch operations**:
   - Linear time complexity
   - Scalable to 1000s of items

### Maintaining Baselines

1. **Establish after optimization**:
   ```bash
   cargo bench -- --save-baseline after_optimization
   ```

2. **Update when justified**:
   - Sustained improvement (>10%)
   - Architectural changes
   - Algorithm improvements

3. **Document changes**:
   - Record baseline updates
   - Explain optimization rationale
   - Link to relevant commits

## Resources

- [Criterion.rs Book](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Quick Wins Report](../../../QUICK_WINS_PERFORMANCE_REPORT.md)
- [Benchmark Specification](../../../docs/PATTERN_BENCHMARK_SPECIFICATION.md)

## Support

For questions or issues:
1. Review this README and specification
2. Check Criterion HTML reports
3. Run validation script
4. Open GitHub issue with benchmark output

---

**Last Updated**: 2025-11-19
**Benchmark Suite Version**: 1.0.0
