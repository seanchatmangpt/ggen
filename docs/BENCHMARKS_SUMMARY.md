# Marketplace Performance Benchmarks - Summary

## Quick Reference

### Running Benchmarks

```bash
# All benchmarks with report
./scripts/run-marketplace-benchmarks.sh

# Specific benchmark group
cargo bench --bench marketplace_performance -- registry_loading

# Test mode (faster, no statistical analysis)
cargo bench --bench marketplace_performance -- --test
```

### Benchmark Coverage

| Category | Benchmark | Target | What It Measures |
|----------|-----------|--------|------------------|
| **Registry** | load_index/10 | <10ms | JSON parsing (small) |
| **Registry** | load_index/100 | <25ms | JSON parsing (medium) |
| **Registry** | load_index/1000 | <50ms | JSON parsing (large) |
| **Search** | keyword_search/100 | <50ms | String matching |
| **Search** | keyword_search/1000 | <100ms | String matching at scale |
| **Search** | tag_filter/100 | <50ms | Collection filtering |
| **Search** | tag_filter/1000 | <100ms | Collection filtering at scale |
| **Search** | fuzzy_search/100 | <75ms | Partial string matching |
| **Search** | fuzzy_search/1000 | <150ms | Partial matching at scale |
| **Search** | combined_filters/100 | <75ms | Multiple filters |
| **Search** | combined_filters/1000 | <150ms | Multiple filters at scale |
| **Install** | install_small_no_deps | <500ms | Basic package install |
| **Install** | install_medium_with_deps | <1000ms | Package + 2-3 deps |
| **Install** | install_large_many_deps | <2000ms | Package + 5+ deps |
| **Deps** | resolve_shallow_tree | <50ms | 1-2 level dependencies |
| **Deps** | resolve_deep_tree | <200ms | 10 level dependencies |
| **Deps** | resolve_50_packages | <200ms | Large flat dependency list |
| **Cache** | cache_hit | <1ms | File exists check |
| **Cache** | cache_miss | <1ms | File not found check |
| **Cache** | cache_write | <50ms | Write new cache entry |
| **Cache** | cache_cleanup | <10ms | Scan cache directory |
| **Concurrent** | parallel_searches_10 | <200ms | 10 concurrent searches |
| **Concurrent** | parallel_installs_5 | <2500ms | 5 concurrent installs |
| **Concurrent** | mixed_operations_10 | <1500ms | Mixed search + install |

## Performance Targets Explained

### Registry Loading
- **Goal**: Fast CLI startup
- **Critical Path**: Initial load before any marketplace operation
- **Optimization**: Consider binary format (MessagePack) for large registries

### Search Performance
- **Goal**: Interactive search experience
- **Critical Path**: User waits for search results
- **Optimization**: Add inverted index for tags/keywords

### Installation Performance
- **Goal**: Acceptable wait time for package installation
- **Critical Path**: Network + disk I/O bottleneck
- **Optimization**: Parallel downloads, streaming extraction

### Dependency Resolution
- **Goal**: Fast resolution for typical projects
- **Critical Path**: Must complete before installation begins
- **Optimization**: Cache resolved trees, better data structures

### Cache Performance
- **Goal**: Fast cache lookups don't add overhead
- **Critical Path**: Every install checks cache first
- **Optimization**: Keep cache directory organized

### Concurrent Operations
- **Goal**: Efficient resource utilization
- **Critical Path**: Users may search while installing
- **Optimization**: Minimize lock contention

## How to Use Benchmarks

### 1. Performance Testing

```bash
# Test current performance
cargo bench --bench marketplace_performance

# Save baseline
cargo bench --bench marketplace_performance -- --save-baseline main

# After optimization
cargo bench --bench marketplace_performance -- --baseline main
```

### 2. Regression Detection

```bash
# CI workflow
cargo bench --bench marketplace_performance -- --save-baseline pr-123

# Compare with main branch
git checkout main
cargo bench --bench marketplace_performance -- --baseline pr-123
```

### 3. Profiling

```bash
# Find slow operations
cargo bench --bench marketplace_performance -- --profile-time=5

# Generate flamegraph
cargo flamegraph --bench marketplace_performance
```

## Reading Results

### Criterion Output

```
registry_loading/load_index/1000
                        time:   [42.123 ms 43.456 ms 44.789 ms]
                        thrpt:  [22.3K elem/s 23.0K elem/s 23.7K elem/s]
```

- **time**: [lower bound, estimate, upper bound] with 95% confidence
- **thrpt**: Throughput (operations per second)

### Performance Variance

```
Found 2 outliers among 100 measurements (2.00%)
  2 (2.00%) high mild
```

- Low variance = consistent performance
- High variance = investigate environmental factors

### Regression Detection

```
registry_loading/load_index/1000
                        time:   [42.123 ms 43.456 ms 44.789 ms]
                        change: [+15.2% +18.3% +21.4%] (p = 0.00 < 0.05)
                        Performance has regressed.
```

- **change**: Performance delta vs baseline
- **p value**: Statistical significance

## Common Issues

### Slow Registry Loading

**Symptoms**: load_index benchmarks >50ms for 1000 packages

**Diagnosis**:
```bash
# Profile JSON parsing
cargo bench --bench marketplace_performance -- registry_loading --profile-time=10
```

**Solutions**:
1. Use `simd-json` for faster parsing
2. Switch to binary format (bincode, MessagePack)
3. Implement lazy loading

### Slow Search

**Symptoms**: search benchmarks >100ms

**Diagnosis**:
```bash
# Check if it's string matching or iteration
cargo bench --bench marketplace_performance -- search_performance
```

**Solutions**:
1. Build inverted index for tags
2. Use `rayon` for parallel search
3. Cache popular queries

### Slow Installation

**Symptoms**: install benchmarks >500ms for no deps

**Diagnosis**:
```bash
# Profile file I/O
cargo bench --bench marketplace_performance -- installation_performance
```

**Solutions**:
1. Use buffered writes
2. Batch file operations
3. Optimize directory creation

### High Variance

**Symptoms**: Large confidence intervals, many outliers

**Diagnosis**:
- Background processes consuming CPU
- Thermal throttling
- Disk cache inconsistency

**Solutions**:
1. Close unnecessary applications
2. Use `--baseline` for relative comparisons
3. Increase sample size: `group.sample_size(200)`

## Integration with CI

### GitHub Actions Example

```yaml
name: Performance Benchmarks

on:
  pull_request:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0  # Need history for baseline

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: |
            ~/.cargo
            target
          key: ${{ runner.os }}-cargo-bench-${{ hashFiles('**/Cargo.lock') }}

      - name: Run benchmarks (PR)
        run: cargo bench --bench marketplace_performance -- --save-baseline pr

      - name: Checkout main
        run: git checkout main

      - name: Run benchmarks (main)
        run: cargo bench --bench marketplace_performance -- --save-baseline main

      - name: Compare results
        run: cargo bench --bench marketplace_performance -- --baseline main
```

## Next Steps

### Short Term
- [ ] Add more test scenarios (edge cases)
- [ ] Profile memory allocations
- [ ] Test with real registry data

### Medium Term
- [ ] Add network latency simulation
- [ ] Benchmark error handling paths
- [ ] Test with concurrent users

### Long Term
- [ ] Integration with production monitoring
- [ ] Automated performance regression alerts
- [ ] Continuous performance tracking dashboard

## Resources

- [Full Documentation](./MARKETPLACE_BENCHMARKS.md)
- [Criterion.rs Book](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
