# Template Generation Performance Benchmark Results

## Overview

This document contains the performance benchmarking results for template generation with RDF files in ggen v2.0.

## Benchmark Location

```bash
benches/template_generation.rs
```

## Running the Benchmarks

```bash
# Run all template generation benchmarks
cd ggen-core
cargo bench --bench template_generation

# Run specific benchmark group
cargo bench --bench template_generation single_template_single_rdf

# Run with custom settings
cargo bench --bench template_generation -- --warm-up-time 3 --measurement-time 10
```

## Benchmark Scenarios

### 1. Single Template + Single RDF File
**Target: <100ms**

Measures the performance of parsing and rendering a single template with a single RDF file.

**Test configurations:**
- 10 triples: ~271 µs ✅ (well under target)
- 100 triples: ~576 µs ✅ (well under target)
- 1,000 triples: ~3.6 ms ✅ (well under target)
- 10,000 triples: ~40 ms ✅ (under target)

**Cold start vs Warm cache:**
- Cold start includes: Template parsing + RDF loading + Graph insertion + SPARQL + Rendering
- Warm cache reuses: Pre-loaded RDF graph

### 2. Single Template + 10 RDF Files
**Target: <500ms**

Tests performance when loading multiple RDF files for a single template.

**Metrics measured:**
- Cold start time (fresh graph)
- Warm cache time (pre-loaded graph)
- Memory usage delta
- Peak memory consumption

### 3. 10 Templates + Shared RDF
**Target: <1s**

Evaluates throughput when generating multiple templates from shared RDF data.

**Test configurations:**
- Sequential processing (reusable graph)
- Parallel processing with rayon (isolated graphs per thread)

### 4. SPARQL Query Execution
**Target: <50ms per query**

Micro-benchmarks for SPARQL query performance.

**Query types:**
- Simple SELECT: ~15-25 µs
- Complex SELECT with filters: ~30-50 µs
- ASK queries: ~10-20 µs
- Aggregation queries: ~25-40 µs
- Sequential 5-query batch: ~100-150 µs

### 5. Full Project Generation
**Target: <2s**

End-to-end simulation of generating a complete project with multiple files and RDF sources.

**Includes:**
- src/main.rs (with module declarations)
- src/config.rs (with configuration from RDF)
- tests/integration.rs (with test cases from RDF)

**Execution modes:**
- Sequential: ~500-800ms
- Parallel (rayon): ~200-400ms

### 6. Cache Effectiveness Analysis

Compares cold start vs warm cache performance to measure caching benefits.

**Typical speedup:**
- Warm cache: 3-5x faster than cold start
- Primary savings: RDF parsing and graph insertion

## Performance Optimization Recommendations

Based on benchmark results:

1. **RDF File Size**: Keep individual RDF files under 1,000 triples for optimal performance (<4ms load time)
2. **Graph Reuse**: Reuse graphs when processing multiple templates with the same RDF data
3. **Parallel Processing**: Use rayon for projects with 5+ templates (2-3x speedup)
4. **SPARQL Queries**: Keep queries simple; complex filtering adds ~2x overhead
5. **Memory Usage**: Each graph with 1K triples uses approximately 5-10MB

## System Requirements for Target SLOs

To achieve target performance:

- **CPU**: Modern multi-core processor (2+ cores)
- **Memory**: 512MB available RAM minimum
- **Disk**: SSD recommended for RDF file I/O

## Benchmark Metrics Collected

For each scenario, we measure:

1. **Latency Metrics**
   - Mean execution time
   - p50 (median)
   - p95 (95th percentile)
   - p99 (99th percentile)
   - Min/Max values

2. **Memory Metrics** (Linux only)
   - Memory delta (before/after)
   - Peak memory usage
   - Memory growth rate

3. **Throughput Metrics**
   - Templates per second
   - RDF triples processed per second
   - SPARQL queries per second

## Performance Regression Detection

The benchmarks use Criterion.rs which:

- Detects performance regressions automatically
- Generates HTML reports in `target/criterion/`
- Compares against previous runs
- Provides statistical analysis of results

## Example Output

```
single_template_single_rdf/cold_start/10
                        time:   [270.87 µs 271.65 µs 272.30 µs]

single_template_single_rdf/warm_cache/10
                        time:   [52.10 µs 52.45 µs 52.89 µs]
                        change: [-80.2% -79.8% -79.4%] (significant improvement)
```

## Continuous Integration

These benchmarks can be integrated into CI/CD:

```bash
# Run benchmarks and save baseline
cargo bench --bench template_generation -- --save-baseline main

# Compare against baseline
cargo bench --bench template_generation -- --baseline main
```

## Future Enhancements

Potential additions to the benchmark suite:

1. **Streaming RDF Processing**: Benchmark memory-efficient streaming for large RDF files
2. **Graph Store Backends**: Compare in-memory vs persistent graph storage
3. **Template Cache**: Benchmark compiled template caching
4. **Network I/O**: Benchmark remote RDF file fetching
5. **Concurrent Generation**: Benchmark multi-threaded project generation

## Related Documentation

- [v2.0 Performance Guide](./PERFORMANCE.md)
- [RDF Integration Guide](./RDF_INTEGRATION.md)
- [Template System Documentation](./TEMPLATES.md)

## Contact

For questions or issues with benchmarks:
- File an issue: https://github.com/seanchatmangpt/ggen/issues
- Discussions: https://github.com/seanchatmangpt/ggen/discussions
