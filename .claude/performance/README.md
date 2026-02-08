# Performance Optimization Tools

Performance monitoring, profiling, and optimization utilities for ggen v6.0.0.

## 📊 SLO Targets

| Metric | Target | Command |
|--------|--------|---------|
| First build | ≤ 15s | `cargo make check` |
| Incremental | ≤ 2s | `cargo make check` |
| RDF processing | ≤ 5s/1k+ triples | `ggen sync --dry_run true` |
| Generation memory | ≤ 100MB | Monitor during `ggen sync` |
| CLI scaffolding | ≤ 3s | `time ggen init` |
| Test suite | ≤ 30s | `cargo make test` |

## 🔧 Tools

### 1. Caching Configuration

**File**: `cache/config.json`

Controls caching behavior across the project:
- **Search**: ripgrep cache (5 min TTL)
- **Prompt Caching**: LLM prompt cache (15 min TTL)
- **Compilation**: Incremental compilation settings
- **RDF Processing**: SPARQL query cache (10 min TTL)

**Usage**: Tools automatically read this config. No manual intervention needed.

### 2. Flamegraph Profiler

**File**: `profiling/flamegraph.sh`

Generates interactive SVG flamegraphs showing CPU hotspots.

**Prerequisites**:
```bash
cargo install flamegraph
# Linux: may need perf access
echo 0 | sudo tee /proc/sys/kernel/perf_event_paranoid
```

**Usage**:
```bash
# Profile full sync
./profiling/flamegraph.sh sync

# Profile with arguments
./profiling/flamegraph.sh sync --dry_run true

# View results
firefox flamegraph.svg
```

**Output**: `flamegraph.svg` - Interactive visualization of function call stack and time spent.

**Timeout**: 60s (configurable in script)

### 3. Criterion Benchmarks

**File**: `profiling/criterion.sh`

Runs Criterion benchmarks with SLO validation.

**Usage**:
```bash
# Run all benchmarks
./profiling/criterion.sh

# Run specific benchmark
./profiling/criterion.sh rdf_parse

# Create baseline for comparison
./profiling/criterion.sh --baseline main

# Compare against baseline
cargo bench -- --baseline main
```

**Output**:
- Terminal: Statistical summary
- HTML: `target/criterion/report/index.html`

**Timeout**: 300s (5 minutes)

## 🎯 Optimization Workflow

### 1. Identify Bottlenecks

```bash
# Run benchmarks to establish baseline
./profiling/criterion.sh --baseline before

# Generate flamegraph
./profiling/flamegraph.sh sync

# Check SLOs
cargo make slo-check
```

### 2. Analyze

**Flamegraph Analysis**:
- Width = time spent (wider = slower)
- Look for unexpected wide blocks
- Focus on top 20% (Pareto principle)

**Criterion Analysis**:
- Check mean/median times
- Look at variance (consistency)
- Compare against SLO targets

### 3. Optimize

**Common Patterns**:
- **Allocations**: Use `&str` instead of `String` where possible
- **Cloning**: Use references or `Rc<T>`/`Arc<T>`
- **Hot paths**: Inline small functions (`#[inline]`)
- **Serialization**: Use `serde` zero-copy where possible
- **RDF**: Cache SPARQL queries, batch operations

**Type-First Optimization**:
```rust
// Before: Runtime allocation
fn process(data: String) -> String { ... }

// After: Zero-cost with generics
fn process<S: AsRef<str>>(data: S) -> Cow<'static, str> { ... }
```

### 4. Verify

```bash
# Run benchmarks again
./profiling/criterion.sh --baseline after

# Compare results
cargo bench -- --baseline before

# Verify SLOs still met
cargo make slo-check

# Full validation
cargo make pre-commit
```

## 📈 Continuous Monitoring

### In CI/CD

```yaml
# .github/workflows/performance.yml
- name: Run benchmarks
  run: |
    ./.claude/performance/profiling/criterion.sh --baseline main

- name: Check SLOs
  run: cargo make slo-check
```

### Local Development

```bash
# Before major changes
./profiling/criterion.sh --baseline before

# After changes
./profiling/criterion.sh

# Compare
cargo bench -- --baseline before
```

## 🚨 Andon Signals

| Signal | Trigger | Action |
|--------|---------|--------|
| 🔴 CRITICAL | SLO exceeded by >50% | STOP - Investigate immediately |
| 🟡 HIGH | SLO exceeded by >20% | Review optimization before merge |
| 🟢 GREEN | All SLOs met | Proceed |

**Example**:
```
RDF processing: 8.2s/1k triples (target: ≤5s)
🔴 CRITICAL: Exceeded by 64% - STOP THE LINE
```

## 🔍 Common Issues

### Slow Compilation

**Symptom**: First build >15s, incremental >2s

**Solutions**:
1. Check `cache/config.json` - ensure incremental compilation enabled
2. Reduce `codegen-units` in dev profile
3. Use `cargo-chef` for Docker builds
4. Enable `sccache` or `mold` linker

```toml
# Cargo.toml
[profile.dev]
incremental = true
codegen-units = 16  # Balance: 1=slow+small, 16=fast+large
```

### Slow RDF Processing

**Symptom**: >5s per 1k triples

**Solutions**:
1. Enable SPARQL query caching in `cache/config.json`
2. Batch operations instead of per-triple
3. Use `oxigraph` bulk loading
4. Pre-compute common queries

```rust
// Before: Per-triple
for triple in triples {
    store.insert(triple)?;
}

// After: Bulk
store.bulk_insert(triples.iter())?;
```

### High Memory Usage

**Symptom**: >100MB during generation

**Solutions**:
1. Stream large files instead of loading fully
2. Drop intermediate buffers early
3. Use `Cow<'_, str>` to defer allocation
4. Profile with `valgrind --tool=massif`

## 📚 Further Reading

- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Criterion.rs User Guide](https://bheisler.github.io/criterion.rs/book/)
- [Flamegraph Interpretation](https://www.brendangregg.com/flamegraphs.html)
- [ggen SLO Documentation](../../docs/performance/slo.md)

## 🔑 Key Principles

- **Measure First**: Never optimize without profiling
- **80/20 Rule**: Focus on hottest 20% of code paths
- **Type-First**: Use types to enforce zero-cost abstractions
- **SLOs**: All optimizations must maintain or improve SLOs
- **Regression Prevention**: Benchmark in CI to catch slowdowns early

---

**Last Updated**: 2026-02-08 | v6.0.0
