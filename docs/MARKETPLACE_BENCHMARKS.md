# Marketplace Performance Benchmarks

Comprehensive performance benchmarking suite for all marketplace operations in ggen.

## Overview

The marketplace benchmarks measure the performance of critical operations:

- **Registry Index Loading**: Parsing and loading package metadata
- **Search Performance**: Keyword, tag, and fuzzy search capabilities
- **Installation Performance**: Package extraction and dependency installation
- **Dependency Resolution**: Resolving shallow and deep dependency trees
- **Cache Performance**: Cache hits, misses, and cleanup operations
- **Concurrent Operations**: Parallel search and installation workloads

## Running Benchmarks

### Quick Start

```bash
# Run all marketplace benchmarks
./scripts/run-marketplace-benchmarks.sh

# Or use cargo directly
cargo bench --bench marketplace_performance
```

### Advanced Usage

```bash
# Run specific benchmark group
cargo bench --bench marketplace_performance -- registry_loading

# Save baseline for future comparisons
cargo bench --bench marketplace_performance -- --save-baseline main

# Compare against baseline
cargo bench --bench marketplace_performance -- --baseline main

# Generate HTML reports
cargo bench --bench marketplace_performance
open target/criterion/report/index.html
```

## Benchmark Categories

### 1. Registry Index Loading

**Purpose**: Measure time to load and parse registry index files of varying sizes.

**Test Cases**:
- 10 packages
- 100 packages
- 1000 packages

**Performance Target**: <50ms for 1000 packages

**What it measures**:
- JSON deserialization performance
- File I/O throughput
- Memory allocation patterns

### 2. Search Performance

**Purpose**: Measure search query response times across different search types.

**Test Cases**:
- Keyword search (exact match)
- Tag filtering
- Fuzzy search (partial matches)
- Combined filters (tags + author + dependencies)

**Performance Target**: <100ms for any query type

**What it measures**:
- String matching algorithms
- Filter pipeline efficiency
- Iterator performance

### 3. Installation Performance

**Purpose**: Measure package installation time with varying complexity.

**Test Cases**:
- Small package (no dependencies)
- Medium package (2-3 dependencies)
- Large package (5+ dependencies)

**Performance Target**: <500ms for installation without dependencies

**What it measures**:
- File system operations
- Directory creation overhead
- Metadata serialization

### 4. Dependency Resolution

**Purpose**: Measure dependency resolution algorithm performance.

**Test Cases**:
- Shallow tree (1-2 levels)
- Deep tree (10 levels)
- Large flat list (50 packages)

**Performance Target**: <200ms for 50 packages

**What it measures**:
- Graph traversal algorithms
- Deduplication efficiency
- Memory usage patterns

### 5. Cache Performance

**Purpose**: Measure cache hit/miss performance and cleanup operations.

**Test Cases**:
- Cache hit (package exists)
- Cache miss (package not found)
- Cache write (new package)
- Cache cleanup (directory scanning)

**What it measures**:
- File system metadata queries
- Directory traversal performance
- Write throughput

### 6. Concurrent Operations

**Purpose**: Measure performance under concurrent workloads.

**Test Cases**:
- 10 parallel searches
- 5 parallel installations
- 10 mixed operations (search + install)

**What it measures**:
- Thread synchronization overhead
- Lock contention
- Async runtime performance

## Performance Targets Summary

| Operation | Target | Rationale |
|-----------|--------|-----------|
| Registry load (1000 packages) | <50ms | Fast startup critical for CLI UX |
| Search (any query) | <100ms | Interactive search requires low latency |
| Install (no deps) | <500ms | Acceptable for network + disk I/O |
| Dependency resolution (50 packages) | <200ms | Most projects have <50 dependencies |
| Cache hit | <1ms | File system metadata query only |
| Parallel searches (10) | <200ms | Should scale linearly |

## Interpreting Results

### Criterion Output

Criterion provides statistical analysis including:
- **Mean**: Average execution time
- **Median**: Middle value (less affected by outliers)
- **Std Dev**: Variability in measurements
- **Throughput**: Operations per second

### HTML Reports

Open `target/criterion/report/index.html` for:
- Performance graphs over time
- Comparison with baselines
- Distribution plots
- Regression detection

### Example Output

```
registry_loading/load_index/10
                        time:   [1.2345 ms 1.2567 ms 1.2789 ms]
                        thrpt:  [7.8K elem/s 8.0K elem/s 8.2K elem/s]

search_performance/keyword_search/1000
                        time:   [45.234 µs 46.123 µs 47.012 µs]

installation_performance/install_small_no_deps
                        time:   [234.56 ms 245.67 ms 256.78 ms]
```

## Optimization Guidelines

### If Registry Loading is Slow

1. **Consider lazy loading**: Don't parse full index upfront
2. **Use binary format**: Switch from JSON to MessagePack/Bincode
3. **Index partitioning**: Split large registries into shards

### If Search is Slow

1. **Add indexing**: Build inverted index for tags/keywords
2. **Cache results**: Memoize common queries
3. **Use Rayon**: Parallelize search across packages

### If Installation is Slow

1. **Parallel downloads**: Fetch dependencies concurrently
2. **Streaming extraction**: Extract while downloading
3. **Optimize disk I/O**: Use buffered writes

### If Dependency Resolution is Slow

1. **Cache resolution results**: Store resolved dependency trees
2. **Use better data structures**: Replace Vec with HashMap for lookups
3. **Prune early**: Skip already-resolved dependencies immediately

## Continuous Performance Monitoring

### CI Integration

Add to `.github/workflows/benchmarks.yml`:

```yaml
name: Performance Benchmarks

on:
  push:
    branches: [main]
  pull_request:

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Run benchmarks
        run: cargo bench --bench marketplace_performance -- --save-baseline pr
      - name: Compare with main
        run: cargo bench --bench marketplace_performance -- --baseline main
```

### Local Development

```bash
# Before making changes
cargo bench --bench marketplace_performance -- --save-baseline before

# After making changes
cargo bench --bench marketplace_performance -- --baseline before

# Check for regressions
critcmp before after
```

## Benchmark Implementation Details

### Test Data Generation

Benchmarks use synthetic test data to ensure:
- **Reproducibility**: Same data across runs
- **Diversity**: Mix of package types and dependency patterns
- **Scalability**: Configurable size for stress testing

### Isolation

Each benchmark:
- Uses temporary directories (auto-cleaned)
- Runs in isolated processes
- Has no shared state between iterations

### Warmup

Criterion automatically:
- Runs warmup iterations to eliminate cold-start effects
- Adjusts sample size for statistical significance
- Detects and handles outliers

## Troubleshooting

### Benchmarks Taking Too Long

Reduce sample size in benchmark code:

```rust
group.sample_size(10); // Default is 100
```

### Inconsistent Results

Common causes:
- Background processes consuming CPU
- Thermal throttling on laptops
- Disk cache state varying between runs

**Solutions**:
- Close unnecessary applications
- Use `--baseline` for relative comparisons
- Run benchmarks multiple times

### Out of Memory

For large benchmarks, increase available memory or reduce test data size:

```rust
let registry = setup_test_registry(100, &temp_dir); // Instead of 1000
```

## Contributing

### Adding New Benchmarks

1. Add benchmark function to `benches/marketplace_performance.rs`
2. Register in `criterion_group!` macro
3. Document performance targets
4. Update this README

### Benchmark Quality Checklist

- [ ] Uses representative test data
- [ ] Has clear performance target
- [ ] Runs in <10 seconds (local)
- [ ] Uses `black_box()` to prevent optimization
- [ ] Cleans up resources (temp directories)
- [ ] Documents what it measures

## References

- [Criterion.rs Documentation](https://bheisler.github.io/criterion.rs/book/)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Benchmarking Best Practices](https://easyperf.net/blog/2018/08/26/Microbenchmarking)

## License

Same as ggen project (MIT)
