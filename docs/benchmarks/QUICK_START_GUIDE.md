# P2P Marketplace Benchmarks - Quick Start Guide

**Last Updated**: November 2, 2025
**Status**: Ready for Execution (Pending Backend Compilation)

## TL;DR - Run Benchmarks Now

```bash
# 1. Clean build environment
cargo clean

# 2. Run baseline benchmarks (no OTEL)
cargo bench --no-default-features --features p2p \
    --bench marketplace_p2p \
    --bench marketplace_performance \
    -- --save-baseline otel-disabled

# 3. Run OTEL-enabled benchmarks
cargo bench --all-features \
    --bench marketplace_p2p \
    --bench marketplace_performance \
    -- --save-baseline otel-enabled

# 4. Calculate OTEL overhead
python3 scripts/calculate_otel_overhead.py

# 5. View HTML reports
open target/criterion/report/index.html
```

**Total Time**: 30-40 minutes (sequential), 10-15 minutes (parallel)

## What Gets Benchmarked

### P2P Operations (45+ scenarios)

| Category | Benchmarks | Target | Critical? |
|----------|------------|--------|-----------|
| **DHT Operations** | 2 network sizes | <500ms @ 1000 peers | ✅ YES |
| **Package Search** | 1 cache hit scenario | <1ms | ✅ YES |
| **Bootstrap** | 1 timing test | <2s @ 10 peers | ✅ YES |
| **Concurrent Ops** | 2 concurrency levels | Linear scaling | ⚠️ Medium |
| **SLA Validation** | 2 critical checks | Must pass | ✅ YES |

### Marketplace Operations (6 categories)

| Category | Benchmarks | Target | Critical? |
|----------|------------|--------|-----------|
| **Registry Loading** | 3 sizes (10/100/1000) | <50ms @ 1000 | ⚠️ Medium |
| **Search** | 4 types × 2 sizes | <100ms | ✅ YES |
| **Installation** | 3 dependency levels | <2s | ✅ YES |
| **Dep Resolution** | 3 scenarios | <200ms | ⚠️ Medium |
| **Cache** | 4 operations | <1ms hit | ✅ YES |
| **Concurrent** | 3 parallelism tests | Linear scaling | ⚠️ Medium |

## Performance Targets at a Glance

```
🎯 Critical SLAs (MUST PASS):
   ✅ Cache hit:        <1ms    (p99)
   ✅ DHT lookup:       <500ms  (p95 @ 1000 peers)
   ✅ Bootstrap:        <2s     (p99 @ 10 peers)
   ✅ Search:           <100ms  (p95)
   ✅ Package install:  <2s     (p95 end-to-end)
   ✅ OTEL overhead:    <10%    (average across all ops)

⚠️  Performance Targets (SHOULD PASS):
   • Registry load:     <50ms  @ 1000 packages
   • Dep resolution:    <200ms @ deep tree
   • Concurrent ops:    Linear scaling to 10x
```

## One-Command Benchmark Execution

### Option 1: Full Test Suite (Recommended)

```bash
# Run everything and generate report
./scripts/run-marketplace-benchmarks.sh
```

**Output**:
- Criterion HTML reports in `target/criterion/report/`
- OTEL overhead analysis in terminal
- Regression detection summary

### Option 2: Quick Test (2 minutes)

```bash
# Test mode - faster, no statistical analysis
cargo bench --bench marketplace_p2p -- --test
```

**Use When**: Quick sanity check during development

### Option 3: Specific Benchmark

```bash
# Just DHT operations
cargo bench --bench marketplace_p2p -- p2p_dht_lookup

# Just search performance
cargo bench --bench marketplace_performance -- search_performance

# Just SLA validation (critical tests only)
cargo bench --bench marketplace_p2p -- p2p_sla_validation
```

**Use When**: Debugging specific performance issue

## Understanding Results

### Criterion Output Format

```
p2p_dht_lookup/lookup_latency/100
                        time:   [142.3 ms 145.1 ms 148.2 ms]
                        change: [+8.2% +10.5% +12.8%] (p = 0.02 < 0.05)
                        Performance has regressed.
```

**Reading**:
- **time**: [lower bound, estimate, upper bound] at 95% confidence
- **change**: Performance delta vs baseline (v2.3.0)
- **p value**: Statistical significance (p < 0.05 = significant)

### Status Indicators

| Indicator | Meaning | Action Required |
|-----------|---------|------------------|
| ✅ `Performance has improved` | Faster than baseline | None, celebrate! 🎉 |
| ⚠️ `No change in performance detected` | Within noise threshold | None |
| ❌ `Performance has regressed` | Slower than baseline | Investigate if >10% |

## OTEL Overhead Analysis

### What It Measures

Compares performance with OpenTelemetry instrumentation **enabled** vs **disabled**:

```
Benchmark: p2p_dht_lookup
  Baseline:   145.1 ms  (OTEL disabled)
  With OTEL:  150.3 ms  (OTEL enabled)
  Overhead:   +5.2 ms   (+3.6%)
  Status:     ✅ PASS   (under 10% threshold)
```

### Interpreting Results

| Overhead | Status | Meaning |
|----------|--------|---------|
| <5% | ✅ Excellent | Negligible impact |
| 5-8% | ✅ Good | Acceptable for observability |
| 8-10% | ⚠️ Acceptable | At threshold, monitor |
| 10-15% | ⚠️ Warning | Consider optimization |
| >15% | ❌ Fail | **Must optimize before production** |

### Optimization If Overhead >10%

**Quick Fixes**:

1. **Reduce Sampling Rate**:
   ```rust
   // In telemetry.rs
   .with_sampler(Sampler::TraceIdRatioBased(0.1))  // 10% sampling
   ```

2. **Skip Hot Paths**:
   ```rust
   #[tracing::instrument(skip(large_data))]  // Don't trace large structures
   async fn hot_function(&self, large_data: Vec<u8>) { }
   ```

3. **Batch Exports**:
   ```rust
   BatchSpanProcessor::builder(exporter, runtime)
       .with_max_export_batch_size(512)       // Bigger batches
       .with_scheduled_delay(Duration::from_secs(5))  // Less frequent
   ```

## Regression Detection

### Manual Check

```bash
# After running benchmarks
./scripts/detect_regressions.sh 10  # 10% threshold
```

**Exit Codes**:
- `0`: No regressions or minor regressions <10%
- `1`: Significant regressions ≥10% detected

### Automated (CI/CD)

```yaml
# In .github/workflows/performance.yml
- name: Run benchmarks
  run: cargo bench --all-features -- --save-baseline pr

- name: Compare with main
  run: cargo bench --all-features -- --baseline main

- name: Check regressions
  run: ./scripts/detect_regressions.sh 10
```

## Common Issues & Solutions

### Issue 1: Benchmarks Won't Compile

**Error**:
```
error[E0433]: failed to resolve: use of undeclared crate or module `ggen_marketplace`
```

**Solution**:
```bash
# Clean and rebuild
cargo clean
cargo build --release --all-features

# Retry benchmarks
cargo bench
```

### Issue 2: High Variance (Unreliable Results)

**Symptom**:
```
Found 15 outliers among 100 measurements (15.00%)
  10 (10.00%) high mild
  5 (5.00%) high severe
```

**Solution**:
```bash
# 1. Close background apps
# 2. Increase sample size
# Edit benches/marketplace_p2p.rs:
group.sample_size(50);  # Increase from 10

# 3. Use relative comparison
cargo bench -- --baseline main  # Compare to baseline, not absolute
```

### Issue 3: Missing Baseline

**Error**:
```
Error: Baseline 'v2.3.0' not found
```

**Solution**:
```bash
# Create baseline from current main branch
git checkout main
cargo bench --all-features -- --save-baseline v2.3.0

# Return to feature branch
git checkout your-feature-branch
cargo bench --all-features -- --baseline v2.3.0
```

### Issue 4: OTEL Script Can't Find Results

**Error**:
```
Error: Criterion results directory not found: target/criterion
```

**Solution**:
```bash
# Run benchmarks first to generate results
cargo bench --all-features -- --save-baseline otel-enabled

# Then run overhead analysis
python3 scripts/calculate_otel_overhead.py
```

## Benchmark Development

### Adding a New Benchmark

```rust
// In benches/marketplace_p2p.rs

fn bench_my_new_feature(c: &mut Criterion) {
    let mut group = c.benchmark_group("my_feature");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(15));

    group.bench_function("operation_name", |b| {
        b.iter(|| {
            // Your benchmark code here
            black_box(my_operation())
        });
    });

    group.finish();
}

// Add to criterion_group! at bottom:
criterion_group!(
    name = benches;
    config = Criterion::default();
    targets =
        bench_dht_lookup_latency,
        bench_my_new_feature,  // <-- Add here
);
```

### Performance Target Guidelines

**For Interactive Operations** (user waits):
- Search, navigation: <100ms (p95)
- Simple queries: <500ms (p95)
- Complex operations: <2s (p95)

**For Background Operations** (async):
- Network propagation: 1-3s acceptable
- Bulk operations: 5-10s acceptable

**For Critical Paths** (SLA):
- Cache hits: <1ms (must be near-instant)
- Core operations: <2s (acceptable wait time)

## Next Steps

### After First Benchmark Run

1. ✅ **Review HTML reports**: `open target/criterion/report/index.html`
2. ✅ **Check OTEL overhead**: Should see <10% across all operations
3. ✅ **Verify no regressions**: All critical SLAs met
4. ✅ **Document results**: Update performance validation report

### If Benchmarks Pass

1. ✅ Create baseline: `cargo bench -- --save-baseline v2.4.0`
2. ✅ Commit results: `git add target/criterion/` (if tracking baselines)
3. ✅ Update changelog: Document performance characteristics
4. ✅ Enable CI/CD: Add performance checks to GitHub Actions

### If Benchmarks Fail

1. ❌ **Identify failures**: Check `./scripts/detect_regressions.sh` output
2. ❌ **Profile slow operations**: `cargo flamegraph --bench <benchmark>`
3. ❌ **Apply optimizations**: See [P2P_PERFORMANCE_ANALYSIS.md](./P2P_PERFORMANCE_ANALYSIS.md)
4. ❌ **Re-run benchmarks**: Verify improvements

## Resources

- **Full Execution Plan**: [P2P_BENCHMARK_EXECUTION_PLAN.md](./P2P_BENCHMARK_EXECUTION_PLAN.md)
- **Performance Analysis**: [P2P_PERFORMANCE_ANALYSIS.md](./P2P_PERFORMANCE_ANALYSIS.md)
- **Benchmarks Summary**: [BENCHMARKS_SUMMARY.md](./BENCHMARKS_SUMMARY.md)
- **Criterion.rs Book**: https://bheisler.github.io/criterion.rs/book/

## FAQ

### Q: How long do benchmarks take?

**A**: 30-40 minutes sequential, 10-15 minutes with parallel execution.

### Q: Can I run benchmarks on my laptop?

**A**: Yes, but results will be less stable. Use `--baseline` for relative comparisons.

### Q: What if I don't have Python for OTEL analysis?

**A**: OTEL overhead script is optional. You can manually compare Criterion reports.

### Q: Should I commit benchmark results?

**A**: No for JSON/binary results. Yes for baseline summaries in docs.

### Q: How often should benchmarks run?

**A**: On every PR (CI/CD) and weekly for trending analysis.

---

**Document Status**: Ready for Use
**Last Validated**: 2025-11-02
**Maintainer**: Performance Benchmarker Agent
