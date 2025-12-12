# Performance Characteristics of ggen

This document details the performance characteristics, benchmarks, and optimization guidelines for ggen.

## Executive Summary

- **Kernel Decision Time**: ≤ 8ms (τ ≤ 8)
- **Code Generation**: 100-500ms depending on template complexity
- **RDF Parsing**: 10-50ms for typical ontologies
- **Watch Mode**: Event detection within 100ms
- **Memory Usage**: ~50-200MB for typical projects

## Running Performance Checks (make-only)

- `cargo make slo-check`: verify SLO thresholds with enforced timeouts
- `cargo make check`, `cargo make test`, `cargo make lint`: baseline health before measuring
- Add or extend Makefile targets for profiling/benchmarks; avoid raw `cargo bench`, `cargo flamegraph`, or `perf` so timeout guards stay active

## Kernel Decision Performance (μ(O))

The MAPE-K kernel is the heart of ggen's autonomous decision-making system.

### Decision Time Budget

```
Total Kernel Time: ≤ 8ms

├── Observation Ingestion: 0.5-1.5ms
│   ├── Schema validation
│   └── Content hashing
├── Analysis Phase: 2-3ms
│   ├── Ontology graph traversal
│   ├── Constraint checking
│   └── History lookup
├── Planning Phase: 2-2.5ms
│   ├── Action selection
│   ├── Dependency analysis
│   └── Conflict detection
└── Execution Phase: 0.5-1ms
    └── Action queueing
```

### Benchmark Results

Typical results (Intel i7-9700K, Ubuntu 22.04):

| Operation | Time | Notes |
|-----------|------|-------|
| Simple decision | 2.1ms | Single observation |
| Complex decision | 6.8ms | 50 observations |
| Batch decision | 7.9ms | 100 observations (near limit) |

## Code Generation Performance

### Template Processing

| Template Size | Generation Time | Notes |
|---|---|---|
| Small (< 5KB) | 50-100ms | Simple substitution |
| Medium (5-50KB) | 100-300ms | Multiple loops/conditions |
| Large (50KB+) | 300-500ms | Complex inheritance chains |
| Very Large (> 1MB) | 500ms-2s | Edge case; consider splitting |

### RDF Ontology Performance

| Ontology Size | Parse Time | Render Time | Total |
|---|---|---|---|
| Small (< 1KB) | 1-2ms | 5-10ms | 6-12ms |
| Medium (1-100KB) | 5-15ms | 20-50ms | 25-65ms |
| Large (100KB-1MB) | 20-50ms | 100-200ms | 120-250ms |
| Very Large (> 1MB) | 50-100ms | 300-500ms | 350-600ms |

### Full Generation Pipeline

```bash
# Time a complete code generation
time ggen generate --ontology large-ontology.ttl \
                   --template complex-template.jinja2 \
                   --output generated/
```

Expected timeline for medium project:
```
Ontology parsing:      30ms
Template compilation:  20ms
Graph analysis:        40ms
Code generation:       150ms
Writing output:        20ms
Verification:          40ms
─────────────────────
Total:                 300ms
```

## Memory Profiling

### Resident Set Size (RSS)

Typical memory usage during operations:

| Operation | Memory | Growth |
|-----------|--------|--------|
| Startup | 15MB | Baseline |
| Load ontology (100KB) | 45MB | +30MB |
| Load templates (20 files) | 65MB | +20MB |
| Generate code (10 files) | 85MB | +20MB |
| Watch mode (idle) | 55MB | Reduced after gen |

### Heap Memory by Crate

From a make-managed release build and heaptrack run (wrapped with timeout):

```
ggen-ai:         12MB (14%) - LLM integration, caching
ggen-marketplace: 8MB  (10%) - Package registry
ggen-core:       15MB (18%) - Lifecycle, conventions
ggen-domain:     25MB (30%) - Ontology, MAPE-K
ggen-cli:        10MB (12%) - CLI layer
Other:           13MB (16%) - Dependencies
─────────────────────────────
Total:           83MB
```

## Optimization Guidelines

### For Code Generation

1. **Use smaller templates**: Split large templates into includes
2. **Enable caching**: Generated code is cached by default
3. **Batch operations**: Process multiple files in one command
4. **Profile frequently**: Add a Makefile flamegraph target for hot paths (keep timeout + Andon guards)

### For Ontology Processing

1. **Minimize ontology size**: Remove unused properties
2. **Use entity references**: Link to shared definitions
3. **Enable lazy loading**: Only load required namespaces
4. **Compress large files**: Use gzip for storage (ggen decompresses)

### For Watch Mode

1. **Configure debouncing**: 100-500ms interval recommended
2. **Limit template count**: 100-200 templates per watch session
3. **Disable unused checks**: Only enable necessary invariants
4. **Use file pattern filters**: Exclude unnecessary paths

## Benchmarking

### Running Benchmarks

Use Makefile-managed targets for any benchmarking so timeout and Andon guards stay active; if a new benchmark is needed, add a dedicated task in `Makefile.toml` rather than invoking raw `cargo bench`.

### Benchmark Suites

Currently available:

```
ggen-cli/benches/
├── command_performance.rs      # CLI command timing
└── template_compilation.rs     # Template processing

ggen-core/benches/
├── kernel_decision_time.rs     # MAPE-K kernel
├── ontology_parsing.rs         # RDF processing
├── convention_matching.rs       # Pattern matching
└── manifest_loading.rs         # Manifest I/O
```

### Creating New Benchmarks

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_my_function(c: &mut Criterion) {
    c.bench_function("my_function", |b| {
        b.iter(|| my_function(black_box(input)))
    });
}

criterion_group!(benches, benchmark_my_function);
criterion_main!(benches);
```

## Performance Profiling

### Flame Graphs

Wrap flamegraph runs in a Makefile task with timeout enforcement to avoid hanging profilers; prefer crate-specific targets over raw `cargo flamegraph`.

### Memory Profiling

Use Makefile-managed wrappers (with timeouts) around heaptrack invocations to ensure profiling does not bypass Andon enforcement. Capture commands in a dedicated task before running.

### Perf Analysis

Add a Makefile target for perf record/report so profiling respects timeout + SLO gates; avoid invoking `perf` directly from the CLI.

## Scaling Characteristics

How ggen scales with input size:

| Metric | Growth | Notes |
|--------|--------|-------|
| Ontology size | Linear (O(n)) | Parsing time ∝ size |
| Template count | Linear (O(n)) | Each compiled once |
| Generated files | Linear (O(n)) | Output time ∝ count |
| Watch paths | Linear (O(n)) | Events per file |
| Invariant count | Linear (O(n)) | Checks are sequential |

### Tested Limits

Successful tests up to:

- **Ontology**: 5MB RDF files
- **Templates**: 500+ template files
- **Generated**: 1000+ output files
- **Watch paths**: 10,000+ paths monitored
- **Invariants**: 100+ constraints checked

Performance degrades gracefully; scaling beyond tested limits may require optimization.

## Known Performance Issues

### Current Limitations

1. **Large RDF files (>10MB)**: Consider splitting into fragments
2. **Deeply nested templates**: Increase parse time; limit nesting depth
3. **Very large watch sessions**: >10,000 files may miss some events
4. **Complex invariant combinations**: >50 invariants; consider grouping

### Mitigation Strategies

```bash
# Split large ontology
ggen generate --ontology base.ttl --include-ontology domain-specific.ttl

# Limit watch scope
ggen watch --include-pattern "src/**/*.rs" --exclude-pattern "**/test/**"

# Pre-filter invariants
ggen generate --ontology ontology.ttl --skip-invariants

# Use caching
ggen generate --cache-dir /tmp/ggen-cache
```

## Performance Goals for v3.1

- [ ] Kernel decisions < 5ms (currently ≤ 8ms)
- [ ] Parallel template compilation (currently sequential)
- [ ] Incremental code generation (cache unchanged sections)
- [ ] Watch event batching optimization
- [ ] Memory usage reduction by 20%

## Reporting Performance Issues

If you observe performance problems:

1. **Collect baseline**: Run `cargo make slo-check` and any Makefile benchmark targets for comparison (add a target if needed)
2. **Profile the issue**: Use Makefile-wrapped flamegraph/perf tasks to identify hot paths
3. **Report with data**: Include timing measurements and system info
4. **Provide reproduction**: Include ontology and template files

```bash
# Example reproduction report
ggen generate --ontology large.ttl \
              --template complex.jinja2 \
              --explain-timing
```

## Performance Tips for Users

### Code Generation

```bash
# ✗ Slow: Individual files
for file in *.ttl; do
    ggen generate --ontology "$file"
done

# ✓ Fast: Batch processing
ggen generate --ontology *.ttl
```

### Watch Mode

```bash
# ✗ Slow: Watch everything
ggen watch

# ✓ Fast: Watch specific patterns
ggen watch --include-pattern "src/**/*.rs" \
           --include-pattern "ontologies/**/*.ttl"
```

### Template Development

```bash
# ✗ Slow: Full regeneration each time
ggen generate --template new.jinja2

# ✓ Fast: Use test fixtures
ggen generate --template new.jinja2 \
              --ontology test-fixtures/minimal.ttl
```

## Questions?

For performance-related questions:
- Check [Performance Discussion](https://github.com/seanchatmangpt/ggen/discussions?discussions_q=label%3Aperformance)
- Open an [Issue](https://github.com/seanchatmangpt/ggen/issues) with `[PERF]` prefix
