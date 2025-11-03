# P2P Benchmark Quick Reference

## 🚀 Quick Start

```bash
# Run all benchmarks
./scripts/run-p2p-benchmarks.sh

# Save baseline
./scripts/run-p2p-benchmarks.sh --baseline main

# Compare vs baseline
./scripts/run-p2p-benchmarks.sh --compare main --strict
```

## 📊 Performance Targets

| Metric | Target | Command |
|--------|--------|---------|
| **DHT Lookup** | 200-500ms @ 1000 peers | `cargo bench -- "dht_lookup"` |
| **Gossipsub** | 1-3s propagation | `cargo bench -- "gossipsub"` |
| **Cache Hit** | < 1ms | `cargo bench -- "sla_cache"` |
| **Memory** | ~50MB/peer | `cargo bench -- "memory"` |
| **Bootstrap** | < 2s @ 10 peers | `cargo bench -- "bootstrap"` |

## 🔍 Benchmark Categories

1. **DHT Lookup Latency** - `bench_dht_lookup_latency`
   - Tests: 10, 50, 100, 500, 1000 peers
   - Expected: Logarithmic scaling

2. **Package Search** - `bench_package_search_throughput`
   - Local cache: < 1ms
   - Remote: 100-500ms

3. **Gossipsub Propagation** - `bench_gossipsub_propagation`
   - Tests: 5, 10, 20, 50 peers
   - Expected: 1-3s

4. **Bootstrap** - `bench_bootstrap_connection`
   - Tests: 5, 10, 20, 50 peers
   - Expected: < 2s @ 10 peers

5. **Concurrent Searches** - `bench_concurrent_searches`
   - Tests: 1, 10, 50, 100 concurrent
   - Expected: Near-linear scaling

6. **Memory Usage** - `bench_memory_usage_per_peer`
   - Baseline: ~50MB
   - With packages: +100KB each

7. **CPU Usage** - `bench_cpu_event_processing`
   - Expected: < 100µs/event

8. **SLA Validation** - `bench_performance_targets`
   - Hard assertions on all SLAs
   - Fails if targets not met

## 📈 Key Commands

```bash
# Run specific benchmark
cargo bench --bench marketplace_p2p -- "dht_lookup"

# Quick CI mode (reduced samples)
./scripts/run-p2p-benchmarks.sh --quick

# Strict mode (fail on regression)
./scripts/run-p2p-benchmarks.sh --compare main --strict

# View HTML report
open target/criterion/report/index.html
```

## 🎯 CI Integration

```yaml
# .github/workflows/benchmarks.yml
- run: ./scripts/run-p2p-benchmarks.sh --compare main --strict --quick
```

## 📋 Interpreting Results

### Console Output
```
p2p_dht_lookup/lookup_latency/100
    time:   [52.1 ms 54.9 ms 57.2 ms]  # [p50, mean, p95]
    change: [+8.2% +12.3% +16.8%]      # vs baseline
    Performance has regressed.         # if p < 0.05
```

### What to Look For
- ✅ Mean time within target range
- ✅ Low std deviation (< 10% of mean)
- ✅ Few outliers (< 5%)
- ✅ No significant regressions

## 🐛 Troubleshooting

| Issue | Fix |
|-------|-----|
| High variance | `--sample-size 50` |
| SLA fails | Close other apps, check system load |
| Memory = 0 | Run on Linux or use `sudo` |
| Compilation fails | `cargo clean && cargo build --bench marketplace_p2p` |

## 📚 Documentation

- **Full Guide:** `docs/P2P_BENCHMARK_GUIDE.md`
- **Summary:** `docs/P2P_CRITERION_BENCHMARKS_SUMMARY.md`
- **Performance Report:** `docs/P2P_PERFORMANCE_REPORT.md`
- **Implementation:** `benches/marketplace_p2p.rs`

## 🎓 Criterion Features

- **p50/p95/p99 latencies** - Percentile analysis
- **Confidence intervals** - Statistical bounds
- **Outlier detection** - Anomaly identification
- **Regression detection** - Baseline comparison
- **HTML reports** - Violin plots, trends
- **Throughput** - Operations/second

---

**Quick Reference Card** | Version 2.4.0 | 2025-11-02
