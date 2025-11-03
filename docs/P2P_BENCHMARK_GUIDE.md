# P2P Performance Benchmark Guide

**Location:** `benches/marketplace_p2p.rs`
**Runner:** `scripts/run-p2p-benchmarks.sh`
**Report:** `target/criterion/report/index.html`

---

## Overview

This benchmark suite validates the P2P marketplace implementation against documented performance targets using **real libp2p networks** (not mocks). It provides statistical analysis via Criterion, tracks memory usage, and detects performance regressions.

### What's Benchmarked

1. **DHT Lookup Latency** - Kademlia DHT operations across network sizes
2. **Package Search Throughput** - Queries per second (local + remote)
3. **Gossipsub Propagation** - Message broadcast latency
4. **Bootstrap Connection Time** - Network initialization speed
5. **Concurrent Searches** - Performance under concurrent load
6. **Memory Usage** - Per-peer memory consumption
7. **CPU Usage** - Event processing overhead
8. **SLA Validation** - Hard assertions against documented targets

---

## Performance Targets (from architecture docs)

| Metric | Target | Source |
|--------|--------|--------|
| DHT lookup (1000 peers) | 200-500ms | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |
| Gossipsub propagation | 1-3s | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |
| Local cache hit | < 1ms | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |
| Memory per peer | ~50MB | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |
| Bootstrap (10 peers) | < 2s | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |
| CLI search command | 100-500ms | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |
| CLI install command | 500ms-2s | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |
| CLI publish command | 200-400ms | [P2P_PERFORMANCE_REPORT.md](P2P_PERFORMANCE_REPORT.md) |

---

## Usage

### Basic Benchmark Run

```bash
# Run all benchmarks with default settings
cargo bench --bench marketplace_p2p

# Or use the wrapper script
./scripts/run-p2p-benchmarks.sh
```

### Save Baseline for Regression Detection

```bash
# Save current performance as baseline
./scripts/run-p2p-benchmarks.sh --baseline main

# Or directly with cargo
cargo bench --bench marketplace_p2p -- --save-baseline main
```

### Compare Against Baseline

```bash
# Compare current performance vs baseline
./scripts/run-p2p-benchmarks.sh --compare main

# With strict mode (fails on regression > 5%)
./scripts/run-p2p-benchmarks.sh --compare main --strict
```

### Quick Validation (CI-friendly)

```bash
# Reduced samples for faster CI runs
./scripts/run-p2p-benchmarks.sh --quick
```

---

## Output and Reports

### Console Output

Criterion prints summary statistics for each benchmark:

```
p2p_dht_lookup/lookup_latency/10
                        time:   [45.234 ms 47.891 ms 50.123 ms]
                        thrpt:  [19.953 elem/s 20.881 elem/s 22.109 elem/s]

p2p_package_search/local_cache_hit
                        time:   [342.21 µs 359.87 µs 378.45 µs]
```

**Key metrics:**
- **time** - Mean, lower bound, upper bound (p50, p95, p99)
- **thrpt** - Throughput (operations per second)
- **Change** - Percentage change vs baseline (if comparing)

### HTML Report

Open `target/criterion/report/index.html` for detailed analysis:

- **Violin plots** - Distribution visualization
- **Performance graphs** - Trend over time
- **Regression detection** - Highlighted regressions
- **Statistical analysis** - Outliers, confidence intervals

The script auto-opens this on macOS/Linux.

### Memory Metrics

Memory usage is measured via custom `MemoryMetrics` struct:

```rust
MemoryMetrics {
    baseline_bytes: 52428800,     // ~50MB
    current_bytes: 104857600,     // ~100MB
    peak_bytes: 125829120,        // ~120MB peak
}
```

Results show:
- **delta_mb** - Memory increase from baseline
- **peak_mb** - Peak memory during operation

---

## Benchmark Details

### 1. DHT Lookup Latency

**Test:** `bench_dht_lookup_latency`
**Measures:** Time to retrieve package metadata from DHT
**Network sizes:** 10, 50, 100, 500, 1000 peers

```bash
cargo bench --bench marketplace_p2p -- "dht_lookup"
```

**Expected:** Logarithmic scaling - O(log N) hops
- 10 peers: ~50ms
- 100 peers: ~150ms
- 1000 peers: ~300-400ms

### 2. Package Search Throughput

**Test:** `bench_package_search_throughput`
**Measures:** Search queries per second (local and remote)
**Scenarios:**
- Local cache hit (should be < 1ms)
- Remote search across 5, 10, 20 peers

```bash
cargo bench --bench marketplace_p2p -- "package_search"
```

**Expected:**
- Local: < 1ms (SLA assertion)
- Remote: 100-500ms depending on peer count

### 3. Gossipsub Propagation

**Test:** `bench_gossipsub_propagation`
**Measures:** Package announcement broadcast latency
**Network sizes:** 5, 10, 20, 50 peers

```bash
cargo bench --bench marketplace_p2p -- "gossipsub"
```

**Expected:** 1-3s for network-wide propagation
- Small networks (5 peers): ~500ms
- Medium networks (20 peers): ~1.5s
- Large networks (50 peers): ~2.5s

### 4. Bootstrap Connection Time

**Test:** `bench_bootstrap_connection`
**Measures:** Time to join network and complete DHT bootstrap
**Network sizes:** 5, 10, 20, 50 peers

```bash
cargo bench --bench marketplace_p2p -- "bootstrap"
```

**Expected:** < 2s for 10 peers (SLA assertion)

### 5. Concurrent Searches

**Test:** `bench_concurrent_searches`
**Measures:** Throughput under concurrent load
**Concurrent requests:** 1, 10, 50, 100

```bash
cargo bench --bench marketplace_p2p -- "concurrent"
```

**Expected:** Near-linear scaling up to ~50 concurrent requests

### 6. Memory Usage

**Test:** `bench_memory_usage_per_peer`
**Measures:** Memory consumption per peer and with packages
**Scenarios:**
- Single peer baseline
- 10, 50, 100, 500 packages

```bash
cargo bench --bench marketplace_p2p -- "memory"
```

**Expected:**
- Baseline: ~50MB per peer
- With packages: +100KB per package (distributed)

### 7. CPU Event Processing

**Test:** `bench_cpu_event_processing`
**Measures:** CPU overhead of processing P2P events
**Iterations:** 100 event processing loops

```bash
cargo bench --bench marketplace_p2p -- "cpu"
```

**Expected:** < 100µs per event processing cycle

### 8. SLA Validation

**Test:** `bench_performance_targets`
**Measures:** Hard assertions against documented SLAs
**Assertions:**
- Cache hit < 1ms
- DHT lookup < 500ms
- Bootstrap < 2s

```bash
cargo bench --bench marketplace_p2p -- "sla"
```

**Expected:** All assertions pass (benchmark fails otherwise)

---

## Regression Detection

Criterion automatically detects performance regressions when comparing against a baseline.

### How It Works

1. Save baseline: `--save-baseline main`
2. Make changes to P2P code
3. Compare: `--compare main`
4. Criterion reports if performance changed significantly

### Significance Levels

- **Default:** 5% significance level (95% confidence)
- **Noise threshold:** 3% (ignores changes < 3%)

### Interpreting Results

```
p2p_dht_lookup/lookup_latency/100
                        time:   [52.123 ms 54.891 ms 57.234 ms]
                        change: [+8.2134% +12.345% +16.789%] (p = 0.00 < 0.05)
                        Performance has regressed.
```

- **Change:** Percentage increase/decrease vs baseline
- **p-value:** < 0.05 means statistically significant
- **Regressed:** Performance got worse

### CI Integration

Use strict mode to fail CI on regression:

```bash
# In CI pipeline
./scripts/run-p2p-benchmarks.sh --compare main --strict --quick
```

This will:
1. Compare against `main` baseline
2. Exit with non-zero status if regression detected
3. Use reduced samples for faster CI runs

---

## Troubleshooting

### Benchmark Fails to Compile

```bash
# Check for missing dependencies
cargo check --benches --bench marketplace_p2p

# Common issues:
# - libp2p features not enabled
# - Missing criterion dependency
# - Incompatible tokio versions
```

### High Variance in Results

```bash
# Increase sample size for more stable results
cargo bench --bench marketplace_p2p -- --sample-size 50

# Increase measurement time
cargo bench --bench marketplace_p2p -- --measurement-time 60
```

### Memory Measurement Not Working

Memory measurement uses `/proc/self/status` on Linux. On other platforms, it falls back to estimates.

For accurate memory profiling:
- Use Linux or WSL
- Run with `sudo` if permission denied
- Consider using `valgrind` or `heaptrack` for detailed analysis

### SLA Assertions Failing

If SLA assertions fail (e.g., cache hit > 1ms), investigate:

```bash
# Run specific SLA test with detailed output
cargo bench --bench marketplace_p2p -- "sla_cache_hit" --verbose

# Check for:
# - System load (close other applications)
# - Disk I/O contention
# - Network issues (even localhost loopback)
```

---

## Continuous Monitoring

### Local Development

```bash
# Before making changes
./scripts/run-p2p-benchmarks.sh --baseline before-optimization

# After changes
./scripts/run-p2p-benchmarks.sh --compare before-optimization

# If faster, update main baseline
./scripts/run-p2p-benchmarks.sh --baseline main
```

### CI/CD Pipeline

```yaml
# .github/workflows/benchmarks.yml
- name: Run P2P Benchmarks
  run: |
    # Fetch baseline from main branch
    git fetch origin main:main
    git checkout main -- target/criterion
    git checkout -

    # Compare current branch vs main
    ./scripts/run-p2p-benchmarks.sh --compare main --strict --quick
```

### Performance Dashboard

Consider integrating with:
- **Criterion Dashboard** - Track trends over time
- **GitHub Actions Benchmark Action** - Auto-comment on PRs
- **Grafana** - Real-time monitoring in production

---

## Extending the Benchmarks

### Adding New Scenarios

```rust
// In benches/marketplace_p2p.rs

fn bench_new_scenario(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_new_scenario");
    group.sample_size(20);

    group.bench_function("my_test", |b| {
        b.to_async(&rt).iter(|| async {
            // Your benchmark code here
            black_box(result)
        });
    });

    group.finish();
}

// Add to criterion_group!
criterion_group!(
    benches,
    // ... existing benchmarks
    bench_new_scenario,
);
```

### Custom Metrics

```rust
// Add custom memory tracking
let mut metrics = MemoryMetrics::new();

// ... perform operations ...

metrics.measure();
println!("Memory delta: {:.2} MB", metrics.delta_mb());
println!("Peak memory: {:.2} MB", metrics.peak_mb());
```

---

## References

- [P2P Performance Report](P2P_PERFORMANCE_REPORT.md) - Detailed analysis and targets
- [P2P Architecture](P2P_REFERENCES_SUMMARY.md) - Implementation overview
- [Criterion Documentation](https://bheisler.github.io/criterion.rs/book/) - Benchmark framework
- [libp2p Specs](https://github.com/libp2p/specs) - Protocol specifications

---

**Last Updated:** 2025-11-02
**Benchmark Version:** v2.4.0
**Status:** ✅ Production Ready
