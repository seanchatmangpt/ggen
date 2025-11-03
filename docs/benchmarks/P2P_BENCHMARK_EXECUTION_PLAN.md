# P2P Marketplace Benchmark Execution Plan

**Date**: November 2, 2025
**Agent**: Performance Benchmarker
**Status**: Ready for Execution (Pending Backend Compilation)

## Executive Summary

Comprehensive benchmark execution strategy for P2P marketplace validation covering 45+ performance scenarios across 8 categories. This plan defines execution order, success criteria, OTEL overhead measurement, and regression detection protocols.

## Performance Targets (SLA Commitments)

### Tier 1: Critical Performance Requirements

| Metric | Target | Rationale | Test Coverage |
|--------|--------|-----------|---------------|
| **Search Latency (Local)** | <100ms | Interactive UX requirement | `bench_package_search_throughput` |
| **DHT Query Latency** | <500ms @ 1000 peers | Network scalability target | `bench_dht_lookup_latency` |
| **Package Retrieval** | <2s end-to-end | User install experience | `bench_cli_commands` |
| **Bootstrap Time** | <2s @ 10 peers | Fast network join | `bench_bootstrap_connection` |
| **Cache Hit Performance** | <1ms | Zero overhead requirement | `bench_performance_targets` |

### Tier 2: Scalability Requirements

| Metric | Target | Rationale | Test Coverage |
|--------|--------|-----------|---------------|
| **Concurrent Operations** | 100+ peers | Production network size | `bench_concurrent_searches` |
| **Gossipsub Propagation** | 1-3s network-wide | Eventual consistency acceptable | `bench_gossipsub_propagation` |
| **Memory per Peer** | <200MB | Resource efficiency | `bench_memory_usage` |
| **Network Scalability** | 1000 peers | Future-proofing | `bench_network_scalability` |

### Tier 3: Quality Requirements

| Metric | Target | Rationale | Test Coverage |
|--------|--------|-----------|---------------|
| **OTEL Overhead** | <10% | Observability cost | Custom OTEL benchmarks |
| **Peer Reputation Calc** | <10ms | Minimal selection delay | `bench_peer_reputation` |
| **No Performance Regression** | vs v2.3.0 | Continuous improvement | Criterion baseline comparison |

## Benchmark Suite Architecture

### Category 1: DHT Operations (P2P Core)

**File**: `benches/marketplace_p2p.rs`
**Lines**: 78-119 (DHT lookup latency)

```rust
// Benchmark: bench_dht_lookup_latency
Network sizes: [10, 100] peers
Expected latency: 200-500ms @ 1000 peers (logarithmic scaling)
Criterion config: 10 samples, 15s measurement time
```

**Test Scenarios**:
1. **DHT Lookup Scaling**: 10 → 100 → 1000 peers
2. **Expected Complexity**: O(log N) hop count
3. **Latency per Hop**: ~20ms network simulation
4. **Success Criteria**:
   - 10 peers: <100ms (3 hops * 20ms = 60ms)
   - 100 peers: <200ms (7 hops * 20ms = 140ms)
   - 1000 peers: <500ms (10 hops * 20ms = 200ms)

**Execution Command**:
```bash
cargo bench --bench marketplace_p2p -- p2p_dht_lookup
```

### Category 2: Package Search Performance (P2P + Local)

**File**: `benches/marketplace_p2p.rs`
**Lines**: 121-158 (Search throughput)

```rust
// Benchmark: bench_package_search_throughput
Scenarios:
- Local cache hit (<1ms)
- Network search (100-500ms)
```

**Test Scenarios**:
1. **Local Cache Hit**: Pre-populated local registry search
2. **Cold Start Search**: No cache, query P2P network
3. **Parallel Search**: 5 peers queried concurrently
4. **Deduplication**: Results from multiple peers

**Success Criteria**:
- Cache hit: <1ms (99th percentile)
- Network search: <500ms @ 20 peers with 500 packages
- Deduplication overhead: <10ms

**Execution Command**:
```bash
cargo bench --bench marketplace_p2p -- p2p_package_search
```

### Category 3: Network Bootstrap & Connectivity

**File**: `benches/marketplace_p2p.rs`
**Lines**: 160-191 (Bootstrap time)

```rust
// Benchmark: bench_bootstrap_connection
Target: <2s for 10 peer connections
Mesh topology: 10 connections per peer
```

**Test Scenarios**:
1. **Cold Bootstrap**: Start node from scratch
2. **Connection Setup**: Establish mesh connections
3. **DHT Initialization**: Publish node info to DHT
4. **Gossipsub Join**: Subscribe to package announcement topic

**Success Criteria**:
- Total bootstrap: <2s @ 10 peers
- Per-connection setup: <200ms average
- DHT publish: <100ms
- Gossipsub join: <50ms

**Execution Command**:
```bash
cargo bench --bench marketplace_p2p -- p2p_bootstrap
```

### Category 4: Concurrent Operations

**File**: `benches/marketplace_p2p.rs`
**Lines**: 193-248 (Concurrent searches)

```rust
// Benchmark: bench_concurrent_searches
Concurrency levels: [1, 10]
Load: 100 packages across network
```

**Test Scenarios**:
1. **Sequential Baseline**: 1 search operation
2. **Low Concurrency**: 10 concurrent searches
3. **High Concurrency**: 50 concurrent searches (stress test)
4. **Mixed Operations**: Search + publish + retrieve

**Success Criteria**:
- Linear scaling up to 10 concurrent ops
- <2x slowdown at 50 concurrent ops
- No deadlocks or panics under load

**Execution Command**:
```bash
cargo bench --bench marketplace_p2p -- p2p_concurrent_searches
```

### Category 5: Performance Target Validation (SLA Enforcement)

**File**: `benches/marketplace_p2p.rs`
**Lines**: 250-321 (SLA validation)

```rust
// Benchmark: bench_performance_targets
Critical SLA checks:
- sla_cache_hit_under_1ms
- sla_bootstrap_under_2s
```

**Test Scenarios**:
1. **Cache Hit SLA**: Validate <1ms requirement
2. **Bootstrap SLA**: Validate <2s requirement
3. **Regression Detection**: Compare vs v2.3.0 baseline
4. **Statistical Significance**: 95% confidence intervals

**Success Criteria**:
- **FAIL** if any SLA violation detected
- Emit warnings to stderr for near-violations
- Criterion regression detection enabled (p < 0.05)

**Execution Command**:
```bash
cargo bench --bench marketplace_p2p -- p2p_sla_validation
```

### Category 6: Registry & Marketplace Operations

**File**: `benches/marketplace_performance.rs`
**Lines**: 156-547 (6 benchmark groups)

**Benchmark Groups**:
1. **Registry Loading** (156-180): Index deserialization
2. **Search Performance** (186-268): Keyword/tag/fuzzy search
3. **Installation Performance** (274-374): Package extraction
4. **Dependency Resolution** (380-466): Tree traversal
5. **Cache Performance** (472-547): Cache hit/miss/write
6. **Concurrent Operations** (553-681): Parallel search/install

**Success Criteria** (see detailed targets in [BENCHMARKS_SUMMARY.md](./BENCHMARKS_SUMMARY.md)):
- All operations meet documented targets
- No regressions vs v2.3.0
- Linear scaling for concurrent ops

**Execution Command**:
```bash
cargo bench --bench marketplace_performance
```

## OTEL Overhead Measurement Strategy

### Objective
Validate that OpenTelemetry instrumentation adds <10% overhead to critical path operations.

### Methodology

#### Phase 1: Baseline Measurement (OTEL Disabled)

```bash
# Disable OTEL in feature flags
cargo bench --no-default-features --features p2p --bench marketplace_p2p -- --save-baseline otel-disabled

# Store baseline results
mkdir -p target/criterion/baselines/otel-disabled
```

**Metrics to Capture**:
- DHT lookup latency (p50, p95, p99)
- Search latency
- Bootstrap time
- Package retrieval time

#### Phase 2: OTEL Enabled Measurement

```bash
# Enable OTEL with default features
cargo bench --all-features --bench marketplace_p2p -- --save-baseline otel-enabled

# Compare with baseline
cargo bench --all-features --bench marketplace_p2p -- --baseline otel-disabled
```

**Metrics to Capture** (same as Phase 1):
- DHT lookup latency
- Search latency
- Bootstrap time
- Package retrieval time

#### Phase 3: Overhead Calculation

```python
# Automated overhead calculation script
# File: scripts/calculate_otel_overhead.py

import json
import sys

def calculate_overhead(baseline_file, otel_file):
    """
    Calculate percentage overhead introduced by OTEL.

    Overhead % = ((otel_time - baseline_time) / baseline_time) * 100
    """

    with open(baseline_file) as f:
        baseline = json.load(f)

    with open(otel_file) as f:
        otel = json.load(f)

    results = {}
    for benchmark_name in baseline.keys():
        baseline_time = baseline[benchmark_name]['mean']['point_estimate']
        otel_time = otel[benchmark_name]['mean']['point_estimate']

        overhead_pct = ((otel_time - baseline_time) / baseline_time) * 100

        results[benchmark_name] = {
            'baseline_ms': baseline_time / 1_000_000,  # ns -> ms
            'otel_ms': otel_time / 1_000_000,
            'overhead_pct': overhead_pct,
            'status': 'PASS' if overhead_pct < 10 else 'FAIL'
        }

    return results

if __name__ == '__main__':
    results = calculate_overhead(
        'target/criterion/baselines/otel-disabled.json',
        'target/criterion/baselines/otel-enabled.json'
    )

    print("OTEL Overhead Analysis")
    print("=" * 80)

    for benchmark, data in results.items():
        status_emoji = "✅" if data['status'] == 'PASS' else "❌"
        print(f"{status_emoji} {benchmark}:")
        print(f"   Baseline: {data['baseline_ms']:.2f}ms")
        print(f"   With OTEL: {data['otel_ms']:.2f}ms")
        print(f"   Overhead: {data['overhead_pct']:.1f}%")
        print()

    # Exit with error if any benchmark fails
    if any(d['status'] == 'FAIL' for d in results.values()):
        sys.exit(1)
```

**Success Criteria**:
- **PASS**: All benchmarks show <10% overhead
- **WARN**: Any benchmark between 8-10% overhead
- **FAIL**: Any benchmark >10% overhead

### OTEL Instrumentation Points to Test

Based on P2P architecture, measure overhead at:

1. **DHT Operations**:
   - Span creation for DHT put/get
   - Trace propagation across network hops
   - Metric collection for DHT latency

2. **Gossipsub Message Handling**:
   - Span per message publish/receive
   - Message size metric collection
   - Propagation delay tracking

3. **Package Search**:
   - Span for search operation
   - Cache hit/miss metrics
   - Result count histogram

4. **Bootstrap Process**:
   - Span for entire bootstrap workflow
   - Per-connection span creation
   - Connection count metrics

### Expected Overhead Breakdown

| Component | Expected Overhead | Mitigation Strategy |
|-----------|-------------------|---------------------|
| Span creation | 1-2% | Use `#[tracing::instrument(skip)]` on hot paths |
| Metric collection | 2-3% | Sample at 10% for high-frequency events |
| Context propagation | 1-2% | Reuse context objects, avoid clones |
| Exporting | 0-1% | Batch exports, async processing |
| **Total** | **4-8%** | Should be well under 10% target |

## Execution Workflow

### Pre-Execution Checklist

```bash
# 1. Clean build artifacts
cargo clean

# 2. Verify dependencies
cargo check --all-features

# 3. Ensure P2P backend compiles
cargo build --release --features p2p

# 4. Create baseline directories
mkdir -p target/criterion/baselines/{v2.3.0,otel-disabled,otel-enabled}
```

### Execution Order (Sequential)

#### Step 1: Baseline Benchmarks (No OTEL)

```bash
# Run all benchmarks without OTEL
cargo bench --no-default-features --features p2p \
    --bench marketplace_p2p \
    --bench marketplace_performance \
    -- --save-baseline otel-disabled

# Estimated time: 15-20 minutes
```

#### Step 2: OTEL-Enabled Benchmarks

```bash
# Run all benchmarks with OTEL
cargo bench --all-features \
    --bench marketplace_p2p \
    --bench marketplace_performance \
    -- --save-baseline otel-enabled

# Estimated time: 15-20 minutes
```

#### Step 3: Compare Results

```bash
# Generate comparison report
cargo bench --all-features \
    --bench marketplace_p2p \
    --bench marketplace_performance \
    -- --baseline otel-disabled

# View HTML reports
open target/criterion/report/index.html
```

#### Step 4: OTEL Overhead Analysis

```bash
# Run overhead calculation script
python3 scripts/calculate_otel_overhead.py

# Expected output:
# ✅ p2p_dht_lookup: Overhead 3.2%
# ✅ p2p_package_search: Overhead 4.1%
# ✅ p2p_bootstrap: Overhead 2.8%
# ...
```

### Parallel Execution (for CI/CD)

```bash
# Use GNU parallel for faster execution
parallel --jobs 4 ::: \
    "cargo bench --bench marketplace_p2p -- p2p_dht_lookup" \
    "cargo bench --bench marketplace_p2p -- p2p_package_search" \
    "cargo bench --bench marketplace_performance -- registry_loading" \
    "cargo bench --bench marketplace_performance -- search_performance"

# Estimated time: 5-8 minutes (4x speedup)
```

## Validation Criteria

### Category 1: Absolute Performance (SLA Compliance)

**PASS Conditions**:
- ✅ Cache hit: <1ms (p99)
- ✅ DHT lookup @ 1000 peers: <500ms (p95)
- ✅ Bootstrap @ 10 peers: <2s (p99)
- ✅ Search latency: <100ms (p95)
- ✅ Package retrieval: <2s end-to-end (p95)

**FAIL Conditions**:
- ❌ Any SLA target exceeded by >20%
- ❌ Any critical operation >2x target latency

### Category 2: Relative Performance (Regression Detection)

**PASS Conditions**:
- ✅ No regression >10% vs v2.3.0 baseline
- ✅ Criterion statistical significance (p < 0.05)
- ✅ Throughput maintained or improved

**FAIL Conditions**:
- ❌ Regression >15% on any benchmark
- ❌ Throughput degradation >10%

### Category 3: OTEL Overhead

**PASS Conditions**:
- ✅ All critical paths: <10% overhead
- ✅ Average overhead: <5%
- ✅ No single operation >15% overhead

**FAIL Conditions**:
- ❌ Any critical path >10% overhead
- ❌ Average overhead >8%

### Category 4: Scalability

**PASS Conditions**:
- ✅ DHT lookup: O(log N) scaling observed
- ✅ Concurrent ops: Linear scaling up to 10 concurrent
- ✅ Memory growth: Sub-linear with peer count

**FAIL Conditions**:
- ❌ Super-linear scaling observed
- ❌ Concurrent ops show contention issues
- ❌ Memory leak detected (linear growth)

## Performance Regression Protocol

### Detection Method

```bash
# Save v2.3.0 baseline
git checkout v2.3.0
cargo bench --all-features -- --save-baseline v2.3.0

# Compare current version
git checkout main
cargo bench --all-features -- --baseline v2.3.0
```

### Criterion Regression Output

```
p2p_dht_lookup/lookup_latency/100
                        time:   [142.3 ms 145.1 ms 148.2 ms]
                        change: [+8.2% +10.5% +12.8%] (p = 0.02 < 0.05)
                        Performance has regressed.
```

**Interpretation**:
- **change**: +10.5% slowdown vs baseline
- **p value**: 0.02 (statistically significant regression)
- **Action Required**: Investigate and fix before merge

### Regression Analysis Script

```bash
#!/bin/bash
# File: scripts/detect_regressions.sh

THRESHOLD=10  # 10% regression threshold

# Run benchmarks and capture output
cargo bench --all-features -- --baseline v2.3.0 > bench_results.txt

# Parse for regressions
REGRESSIONS=$(grep "Performance has regressed" bench_results.txt | wc -l)

if [ "$REGRESSIONS" -gt 0 ]; then
    echo "❌ $REGRESSIONS performance regressions detected!"
    grep -A 5 "Performance has regressed" bench_results.txt
    exit 1
else
    echo "✅ No performance regressions detected"
    exit 0
fi
```

## Output & Reporting

### Criterion HTML Reports

**Generated at**: `target/criterion/report/index.html`

**Report Sections**:
1. **Summary Page**: All benchmarks with violin plots
2. **Detailed Benchmarks**: Per-benchmark analysis with:
   - PDF plot (probability density function)
   - Regression scatter plot
   - Iteration times histogram
3. **Comparison Page**: Baseline vs current
4. **History Page**: Trend over time

### Custom Performance Report

**File**: `docs/benchmarks/P2P_PERFORMANCE_VALIDATION_REPORT.md`

**Template**:
```markdown
# P2P Marketplace Performance Validation Report

**Date**: 2025-11-02
**Version**: v2.4.0
**Baseline**: v2.3.0

## Summary

| Category | Status | Details |
|----------|--------|---------|
| SLA Compliance | ✅ PASS | All targets met |
| Regression Check | ✅ PASS | No significant regressions |
| OTEL Overhead | ✅ PASS | 4.2% average overhead |
| Scalability | ✅ PASS | O(log N) scaling confirmed |

## Detailed Results

### DHT Operations
- **Lookup Latency (100 peers)**: 145ms (target: <200ms) ✅
- **Lookup Latency (1000 peers)**: 387ms (target: <500ms) ✅
- **Scaling Factor**: O(log N) confirmed ✅

### Search Performance
- **Local Cache Hit**: 0.8ms (target: <1ms) ✅
- **Network Search (20 peers)**: 87ms (target: <100ms) ✅
- **Parallel Search (5 peers)**: 32ms (5x speedup) ✅

### OTEL Overhead Analysis
- **DHT Operations**: +3.2% overhead ✅
- **Search Operations**: +4.1% overhead ✅
- **Bootstrap**: +2.8% overhead ✅
- **Average**: +4.2% overhead ✅

## Recommendations
1. ✅ All performance targets met
2. ⚠️ Consider caching optimization for 1000+ peer networks
3. ✅ OTEL overhead acceptable for production
```

## Troubleshooting Guide

### Issue: Benchmarks Fail to Compile

**Symptoms**:
```
error: linking with `cc` failed: exit status: 1
  = note: ld: library not found for -lggen_marketplace
```

**Resolution**:
```bash
# Clean build cache
cargo clean

# Rebuild dependencies
cargo build --release --all-features

# Retry benchmarks
cargo bench --bench marketplace_p2p
```

### Issue: High Variance in Results

**Symptoms**:
```
Found 15 outliers among 100 measurements (15.00%)
  10 (10.00%) high mild
  5 (5.00%) high severe
```

**Resolution**:
```bash
# Increase sample size
# Edit benches/marketplace_p2p.rs:
group.sample_size(50);  # Increase from 10 to 50

# Isolate CPU cores (Linux)
taskset -c 0,1 cargo bench --bench marketplace_p2p
```

### Issue: OTEL Overhead Exceeds 10%

**Symptoms**:
```
❌ p2p_dht_lookup: Overhead 12.3%
```

**Resolution**:
1. **Reduce Sampling Rate**:
   ```rust
   // In telemetry configuration
   .with_sampler(Sampler::TraceIdRatioBased(0.1))  // 10% sampling
   ```

2. **Skip Hot Paths**:
   ```rust
   #[tracing::instrument(skip(self, large_data))]
   async fn hot_path(&self, large_data: Vec<u8>) {
       // Skip tracing large data structures
   }
   ```

3. **Batch Exports**:
   ```rust
   BatchSpanProcessor::builder(exporter, runtime)
       .with_max_export_batch_size(512)  // Larger batches
       .with_scheduled_delay(Duration::from_secs(5))  // Less frequent exports
   ```

## Continuous Performance Monitoring

### CI/CD Integration

```yaml
# .github/workflows/performance.yml
name: Performance Benchmarks

on:
  pull_request:
    branches: [main]
  schedule:
    - cron: '0 0 * * 0'  # Weekly

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0  # Need history for baselines

      - name: Setup Rust
        uses: dtolnay/rust-toolchain@stable

      - name: Cache cargo registry
        uses: actions/cache@v3
        with:
          path: ~/.cargo/registry
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}

      - name: Benchmark (baseline)
        run: |
          git checkout main
          cargo bench --all-features -- --save-baseline main

      - name: Benchmark (PR)
        run: |
          git checkout ${{ github.sha }}
          cargo bench --all-features -- --baseline main

      - name: Check for regressions
        run: ./scripts/detect_regressions.sh

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: target/criterion/
```

## Next Steps

### Immediate (Post-Compilation)

1. ✅ Execute baseline benchmarks (no OTEL)
2. ✅ Execute OTEL-enabled benchmarks
3. ✅ Calculate OTEL overhead
4. ✅ Generate performance validation report

### Short-Term Optimizations

1. **Parallel Package Search** (if overhead >100ms):
   ```rust
   // Replace sequential search
   let handles: Vec<_> = peers.iter().map(|peer| {
       tokio::spawn(async move { peer.search_local(query).await })
   }).collect();
   let results = futures::join_all(handles).await;
   ```

2. **DHT Query Pipelining** (if latency >500ms @ 1000 peers):
   ```rust
   // Pipeline DHT queries instead of sequential hops
   let futures: Vec<_> = (0..hops).map(|hop| {
       tokio::spawn(async move { dht_query(hop).await })
   }).collect();
   ```

3. **Cache Pre-warming** (if cache misses frequent):
   ```rust
   async fn prewarm_cache(&self, popular_packages: &[String]) {
       for pkg in popular_packages {
           self.cache.load(pkg).await;
       }
   }
   ```

### Long-Term Enhancements

1. **Real Network Testing**: Replace mocks with actual libp2p
2. **10K+ Peer Tests**: Validate extreme scalability
3. **Failure Scenario Tests**: Network partitions, peer churn
4. **Production Metrics**: Integrate with Prometheus/Grafana

## Success Metrics

**Benchmark Execution Complete When**:
- ✅ All 45+ benchmarks executed successfully
- ✅ All SLA targets met (Tier 1 requirements)
- ✅ OTEL overhead <10% validated
- ✅ No regressions vs v2.3.0
- ✅ HTML reports generated
- ✅ Performance validation report created

**Ready for Production When**:
- ✅ All success metrics achieved
- ✅ CI/CD integration complete
- ✅ Performance monitoring enabled
- ✅ Regression detection automated

---

**Document Status**: Ready for Execution
**Blocking Issues**: P2P backend compilation (being addressed by Code Analyzer)
**Estimated Execution Time**: 30-40 minutes (sequential), 10-15 minutes (parallel)
**Expected Completion**: Within 1 hour of backend compilation success

**Related Documentation**:
- [P2P Performance Analysis](./P2P_PERFORMANCE_ANALYSIS.md)
- [Benchmarks Summary](./BENCHMARKS_SUMMARY.md)
- [P2P Minimal Architecture](/docs/P2P_MINIMAL_ARCHITECTURE.md)
