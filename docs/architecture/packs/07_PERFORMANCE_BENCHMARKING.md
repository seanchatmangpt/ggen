# Pack System: Performance Targets and Benchmarking Strategy

## Overview

This document defines performance targets, benchmarking methodology, and optimization strategies for the packs system.

---

## Performance Targets

### 1. Discovery Operations

| Operation | Cold Cache | Warm Cache | Max Acceptable | Percentile |
|-----------|-----------|------------|----------------|------------|
| `pack list` | < 50ms | < 10ms | 100ms | p95 |
| `pack search` | < 200ms | < 50ms | 500ms | p95 |
| `pack show` | < 100ms | < 20ms | 250ms | p95 |
| `pack info` | < 50ms | < 10ms | 100ms | p95 |

**Rationale**: Discovery operations are high-frequency and user-facing. Sub-100ms response feels instantaneous.

---

### 2. Management Operations

| Operation | Target | Max Acceptable | Notes |
|-----------|--------|----------------|-------|
| `pack install` | < 5s | 15s | Network-dependent, show progress |
| `pack uninstall` | < 1s | 3s | Local operation only |
| `pack update` | < 5s | 15s | Per pack, network-dependent |
| `pack clean` | < 2s | 5s | Depends on cache size |

**Rationale**: Management operations are less frequent and users expect some latency for network operations.

---

### 3. Generation Operations

| Operation | Target | Max Acceptable | Complexity |
|-----------|--------|----------------|------------|
| Single pack, 1 template | < 2s | 5s | Simple |
| Single pack, 5 templates | < 8s | 15s | Medium |
| Single pack, 10 templates | < 15s | 30s | Complex |
| Composition, 2 packs | < 12s | 25s | Medium |
| Composition, 3 packs | < 20s | 40s | Complex |
| Composition, 5 packs | < 35s | 60s | Very complex |

**Rationale**: Generation is the most time-consuming operation. Users tolerate longer waits for complex operations if progress is shown.

---

### 4. Validation Operations

| Operation | Target | Max Acceptable | Scope |
|-----------|--------|----------------|-------|
| `pack validate` | < 500ms | 1s | Full validation |
| `pack lint` | < 300ms | 800ms | Quick checks |
| `pack check` | < 200ms | 500ms | Minimal validation |
| Compatibility check | < 400ms | 1s | 2-pack comparison |

**Rationale**: Validation is often in the critical path (pre-publish). Must be fast enough to run frequently.

---

### 5. Composition Operations

| Operation | Target | Max Acceptable | Complexity |
|-----------|--------|----------------|------------|
| Dependency resolution | < 2s | 5s | 10 dependencies |
| Conflict detection | < 1s | 3s | 100 files |
| Variable merging | < 500ms | 1s | 50 variables |
| Template ordering | < 300ms | 800ms | 20 templates |
| `pack plan` | < 3s | 8s | Full composition analysis |

**Rationale**: Composition analysis happens before generation. Users expect this to be reasonably fast.

---

### 6. Benchmarking Operations

| Operation | Target | Max Acceptable | Iterations |
|-----------|--------|----------------|------------|
| Generation benchmark | < 2 min | 5 min | 10 iterations |
| Quality score | < 800ms | 2s | Full analysis |
| Performance profiling | < 5 min | 10 min | Detailed metrics |

**Rationale**: Benchmarking is developer-facing and infrequent. Accuracy more important than speed.

---

## Throughput Targets

### Pack Operations Per Second

| Operation | Target | Infrastructure |
|-----------|--------|----------------|
| List operations | 100 ops/s | Single-threaded |
| Search operations | 50 ops/s | With caching |
| Install operations | 5 ops/s | Network-limited |
| Generate operations | 2 ops/s | CPU-bound |
| Validate operations | 20 ops/s | CPU-bound |

**Rationale**: Support reasonable concurrent usage (10-20 users).

---

## Resource Utilization Targets

### Memory

| Operation | Target | Max | Notes |
|-----------|--------|-----|-------|
| Pack list | < 50 MB | 100 MB | Pagination |
| Pack install | < 100 MB | 200 MB | Buffer size |
| Pack generate | < 300 MB | 500 MB | Template caching |
| Pack compose (3 packs) | < 400 MB | 800 MB | Multi-pack state |

**Constraint**: Total memory usage across all operations < 1 GB.

---

### Disk I/O

| Operation | Target | Max | Notes |
|-----------|--------|-----|-------|
| Sequential read | > 100 MB/s | N/A | Template loading |
| Sequential write | > 80 MB/s | N/A | File generation |
| Random read | > 10 MB/s | N/A | Manifest parsing |
| IOPS | > 1000 | N/A | Small file ops |

**Strategy**: Minimize small random writes, batch operations.

---

### CPU

| Operation | Target CPU % | Max Sustained | Parallelism |
|-----------|-------------|---------------|-------------|
| Template rendering | < 70% | 90% | Per-template |
| SPARQL execution | < 60% | 80% | Per-query |
| Validation | < 50% | 70% | Parallel checks |
| Dependency resolution | < 40% | 60% | Graph algorithms |

**Strategy**: Use parallel processing for independent operations.

---

## Benchmarking Methodology

### 1. Benchmark Suite Structure

```rust
// Benchmark categories
mod discovery_benchmarks {
    // pack list, search, show
}

mod management_benchmarks {
    // pack install, uninstall, update
}

mod generation_benchmarks {
    // Single-pack generation with varying complexity
}

mod composition_benchmarks {
    // Multi-pack composition
}

mod validation_benchmarks {
    // Validation, lint, compatibility
}

mod end_to_end_benchmarks {
    // Full user workflows
}
```

---

### 2. Benchmark Fixtures

```toml
# benchmarks/fixtures/packs.toml

[[packs]]
name = "minimal-pack"
templates = 1
queries = 0
variables = 3
size = "10KB"
complexity = "simple"

[[packs]]
name = "medium-pack"
templates = 5
queries = 2
variables = 15
size = "500KB"
complexity = "medium"

[[packs]]
name = "complex-pack"
templates = 10
queries = 5
variables = 30
size = "5MB"
complexity = "complex"

[[packs]]
name = "mega-pack"
templates = 50
queries = 20
variables = 100
size = "50MB"
complexity = "extreme"
```

---

### 3. Criterion.rs Benchmarks

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use ggen_domain::pack::*;

fn bench_pack_list(c: &mut Criterion) {
    let mut group = c.benchmark_group("pack_list");

    for size in [10, 50, 100, 500, 1000] {
        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &size,
            |b, &size| {
                let registry = create_test_registry(size);
                b.iter(|| {
                    black_box(registry.list(PackFilter::default()))
                });
            },
        );
    }

    group.finish();
}

fn bench_pack_generate(c: &mut Criterion) {
    let mut group = c.benchmark_group("pack_generate");

    for complexity in ["simple", "medium", "complex"] {
        group.bench_with_input(
            BenchmarkId::from_parameter(complexity),
            &complexity,
            |b, &complexity| {
                let pack = load_fixture_pack(complexity);
                let generator = PackGenerator::new();
                b.iter(|| {
                    black_box(generator.generate(&pack, GenerateOptions::default()))
                });
            },
        );
    }

    group.finish();
}

fn bench_pack_compose(c: &mut Criterion) {
    let mut group = c.benchmark_group("pack_compose");

    for pack_count in [2, 3, 5] {
        group.bench_with_input(
            BenchmarkId::from_parameter(pack_count),
            &pack_count,
            |b, &pack_count| {
                let packs = load_fixture_packs(pack_count);
                let composer = PackComposer::new();
                b.iter(|| {
                    black_box(composer.compose(packs.clone()))
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_pack_list,
    bench_pack_generate,
    bench_pack_compose,
);
criterion_main!(benches);
```

---

### 4. End-to-End Benchmarks

```rust
#[bench]
fn e2e_first_time_user_workflow() {
    // Scenario: New user generates project from pack
    let registry = PackRegistry::new();

    let start = Instant::now();

    // 1. Search for pack
    let results = registry.search("startup microservice", SearchOptions::default())?;
    let search_time = start.elapsed();

    // 2. Show pack details
    let pack = registry.get(&results[0].name, None)?;
    let show_time = start.elapsed() - search_time;

    // 3. Install pack
    registry.install(&pack.metadata.name, None)?;
    let install_time = start.elapsed() - search_time - show_time;

    // 4. Generate project
    let generator = PackGenerator::new();
    generator.generate(&pack, GenerateOptions {
        output_dir: PathBuf::from("/tmp/test-project"),
        ..Default::default()
    })?;
    let generate_time = start.elapsed() - search_time - show_time - install_time;

    let total_time = start.elapsed();

    // Assert performance targets
    assert!(search_time < Duration::from_millis(200), "Search too slow");
    assert!(show_time < Duration::from_millis(100), "Show too slow");
    assert!(install_time < Duration::from_secs(10), "Install too slow");
    assert!(generate_time < Duration::from_secs(15), "Generate too slow");
    assert!(total_time < Duration::from_secs(30), "E2E workflow too slow");

    println!("E2E Workflow Timing:");
    println!("  Search:   {:?}", search_time);
    println!("  Show:     {:?}", show_time);
    println!("  Install:  {:?}", install_time);
    println!("  Generate: {:?}", generate_time);
    println!("  Total:    {:?}", total_time);
}
```

---

### 5. Stress Testing

```rust
#[test]
fn stress_concurrent_generations() {
    let pack = load_fixture_pack("medium");
    let generator = Arc::new(PackGenerator::new());

    let handles: Vec<_> = (0..10)
        .map(|i| {
            let gen = Arc::clone(&generator);
            let pack = pack.clone();
            thread::spawn(move || {
                let output_dir = PathBuf::from(format!("/tmp/stress-test-{}", i));
                gen.generate(&pack, GenerateOptions {
                    output_dir,
                    ..Default::default()
                })
            })
        })
        .collect();

    let start = Instant::now();
    for handle in handles {
        handle.join().unwrap().unwrap();
    }
    let duration = start.elapsed();

    // Should complete in reasonable time even with contention
    assert!(duration < Duration::from_secs(120), "Concurrent stress test too slow");
}

#[test]
fn stress_large_registry() {
    let registry = create_test_registry(1000); // 1000 packs

    let start = Instant::now();
    let results = registry.list(PackFilter::default())?;
    let duration = start.elapsed();

    assert_eq!(results.len(), 1000);
    assert!(duration < Duration::from_millis(50), "Large registry list too slow");
}

#[test]
fn stress_deep_dependency_tree() {
    // Create pack with 10 levels of dependencies
    let pack = create_deep_dependency_pack(10);
    let resolver = DependencyResolver::new();

    let start = Instant::now();
    let resolved = resolver.resolve(&pack)?;
    let duration = start.elapsed();

    assert!(duration < Duration::from_secs(5), "Deep dependency resolution too slow");
    assert_eq!(resolved.len(), 10);
}
```

---

## Performance Profiling

### 1. Flamegraph Generation

```bash
# Profile pack generation
cargo flamegraph --bench generation_benchmarks -- --bench

# Profile dependency resolution
cargo flamegraph --bench composition_benchmarks -- --bench

# View results
open flamegraph.svg
```

---

### 2. Memory Profiling

```bash
# Use valgrind for memory profiling
valgrind --tool=massif --massif-out-file=massif.out \
    cargo bench --bench generation_benchmarks

# Analyze results
ms_print massif.out

# Use heaptrack (Linux)
heaptrack cargo bench --bench generation_benchmarks
heaptrack_gui heaptrack.*.gz
```

---

### 3. Tracing and Instrumentation

```rust
use tracing::{instrument, info, span, Level};

#[instrument(level = "info", skip(self))]
pub async fn generate(&self, pack: &Pack, options: GenerateOptions) -> Result<GenerateResult> {
    let _span = span!(Level::INFO, "pack_generate", pack = %pack.metadata.name);

    info!("Starting generation for pack: {}", pack.metadata.name);

    let start = Instant::now();

    // ... generation logic ...

    let duration = start.elapsed();
    info!("Generation completed in {:?}", duration);

    Ok(result)
}

// Enable tracing subscriber
tracing_subscriber::fmt()
    .with_max_level(Level::INFO)
    .init();
```

---

## Optimization Strategies

### 1. Caching

**Registry Metadata Cache**:
```rust
pub struct CachedPackRegistry {
    inner: Box<dyn PackRepository>,
    cache: Arc<RwLock<LruCache<String, Pack>>>,
    ttl: Duration,
}

impl CachedPackRegistry {
    async fn get(&self, name: &str, version: Option<&Version>) -> Result<Pack> {
        let cache_key = format!("{}@{}", name, version.map(|v| v.to_string()).unwrap_or_else(|| "latest".to_string()));

        // Check cache
        {
            let cache = self.cache.read().unwrap();
            if let Some(pack) = cache.get(&cache_key) {
                return Ok(pack.clone());
            }
        }

        // Cache miss, fetch from inner repository
        let pack = self.inner.get(name, version).await?;

        // Update cache
        {
            let mut cache = self.cache.write().unwrap();
            cache.put(cache_key, pack.clone());
        }

        Ok(pack)
    }
}
```

**Template Resolution Cache**:
- Cache resolved template paths
- Avoid repeated marketplace lookups
- TTL: 15 minutes

**SPARQL Query Result Cache**:
- Cache query results by query hash
- Invalidate on TTL update
- TTL: 5 minutes

---

### 2. Parallelization

**Parallel Template Rendering**:
```rust
use rayon::prelude::*;

pub fn execute_templates_parallel(
    templates: &[PackTemplate],
    output_dir: &Path,
    variables: &HashMap<String, String>,
) -> Result<Vec<PathBuf>> {
    templates
        .par_iter()
        .map(|template| {
            execute_template(template, output_dir, variables)
        })
        .collect()
}
```

**Parallel Validation**:
```rust
pub fn validate_parallel(pack: &Pack) -> Result<ValidationResult> {
    let (metadata_result, template_result, query_result, variable_result) = rayon::join(
        || validate_metadata(&pack.metadata),
        || validate_templates(&pack.templates),
        || validate_queries(&pack.queries),
        || validate_variables(&pack.variables),
    );

    // Combine results
    combine_validation_results(vec![
        metadata_result?,
        template_result?,
        query_result?,
        variable_result?,
    ])
}
```

---

### 3. Lazy Loading

**Lazy Pack Manifest Loading**:
```rust
pub struct LazyPack {
    metadata: PackMetadata,
    manifest_path: PathBuf,
    loaded: Arc<RwLock<Option<Pack>>>,
}

impl LazyPack {
    pub fn full_pack(&self) -> Result<Pack> {
        // Check if already loaded
        {
            let loaded = self.loaded.read().unwrap();
            if let Some(pack) = loaded.as_ref() {
                return Ok(pack.clone());
            }
        }

        // Load from disk
        let pack = Pack::from_manifest(&self.manifest_path)?;

        // Cache for future use
        {
            let mut loaded = self.loaded.write().unwrap();
            *loaded = Some(pack.clone());
        }

        Ok(pack)
    }
}
```

---

### 4. Incremental Operations

**Incremental Generation**:
```rust
pub struct GenerationState {
    pub completed_templates: HashSet<String>,
    pub completed_queries: HashSet<String>,
    pub generated_files: HashSet<PathBuf>,
}

pub fn generate_incremental(
    pack: &Pack,
    state: &mut GenerationState,
    options: GenerateOptions,
) -> Result<GenerateResult> {
    // Skip already-completed templates
    let remaining_templates: Vec<_> = pack.templates
        .iter()
        .filter(|t| !state.completed_templates.contains(&t.alias.as_ref().unwrap_or(&t.source.to_string())))
        .collect();

    // Generate only remaining templates
    for template in remaining_templates {
        execute_template(template, &options.output_dir, &options.variables)?;
        state.completed_templates.insert(template.alias.clone().unwrap_or_else(|| template.source.to_string()));
    }

    Ok(GenerateResult { /* ... */ })
}
```

---

### 5. Database Indexing

**Pack Registry Database Schema**:
```sql
CREATE TABLE packs (
    name VARCHAR(255) PRIMARY KEY,
    version VARCHAR(50),
    title VARCHAR(255),
    description TEXT,
    category VARCHAR(100),
    downloads INTEGER DEFAULT 0,
    rating REAL DEFAULT 0.0,
    created_at TIMESTAMP,
    updated_at TIMESTAMP
);

-- Indexes for fast queries
CREATE INDEX idx_packs_category ON packs(category);
CREATE INDEX idx_packs_downloads ON packs(downloads DESC);
CREATE INDEX idx_packs_rating ON packs(rating DESC);
CREATE INDEX idx_packs_updated ON packs(updated_at DESC);

-- Full-text search index
CREATE VIRTUAL TABLE packs_fts USING fts5(
    name,
    title,
    description,
    category,
    content='packs'
);
```

---

## Continuous Performance Monitoring

### 1. CI/CD Integration

```yaml
# .github/workflows/performance.yml
name: Performance Benchmarks

on:
  pull_request:
  push:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Run benchmarks
        run: cargo bench --bench pack_benchmarks -- --save-baseline main

      - name: Compare with baseline
        run: cargo bench --bench pack_benchmarks -- --baseline main

      - name: Upload results
        uses: actions/upload-artifact@v2
        with:
          name: benchmark-results
          path: target/criterion/
```

---

### 2. Performance Regression Detection

```rust
#[test]
fn test_no_performance_regression() {
    // Load baseline from previous run
    let baseline = load_baseline_metrics();

    // Run current benchmarks
    let current = run_benchmarks();

    // Compare
    for (operation, current_time) in &current {
        let baseline_time = baseline.get(operation).unwrap();
        let regression = (current_time - baseline_time) / baseline_time;

        assert!(
            regression < 0.1, // Max 10% regression
            "Performance regression detected for {}: {:?} -> {:?} ({:.1}% slower)",
            operation,
            baseline_time,
            current_time,
            regression * 100.0
        );
    }
}
```

---

## Performance Dashboard

### Metrics to Track

1. **Operation Latency** (p50, p95, p99)
2. **Throughput** (ops/second)
3. **Memory Usage** (peak, average)
4. **CPU Utilization** (average, peak)
5. **Cache Hit Rate** (registry, templates, queries)
6. **Error Rate** (per operation type)
7. **Generation Success Rate** (%)

### Dashboard Example

```
┌─────────────────────────────────────────────────────────────┐
│ Pack System Performance Dashboard                           │
│ Last Updated: 2025-11-17 14:30:00                          │
├─────────────────────────────────────────────────────────────┤
│ Operation Latency (p95)                                     │
│   pack list:      12ms  ████░░░░░░  (target: 50ms)         │
│   pack search:    89ms  ████████░░  (target: 200ms)        │
│   pack generate:  6.2s  ██████████  (target: 10s)          │
│   pack compose:   18s   █████████░  (target: 30s)          │
├─────────────────────────────────────────────────────────────┤
│ Throughput                                                  │
│   List ops/s:     142   ██████████  (target: 100)          │
│   Generate ops/s: 3.1   ██████████  (target: 2)            │
├─────────────────────────────────────────────────────────────┤
│ Resource Usage                                              │
│   Memory (avg):   240MB █████░░░░░  (target: 300MB)        │
│   CPU (avg):      45%   █████░░░░░  (target: 70%)          │
├─────────────────────────────────────────────────────────────┤
│ Cache Performance                                           │
│   Registry hit:   87%   █████████░                          │
│   Template hit:   92%   ██████████                          │
│   Query hit:      78%   ████████░░                          │
├─────────────────────────────────────────────────────────────┤
│ Success Rates                                               │
│   Generation:     97.2% ██████████                          │
│   Validation:     99.8% ██████████                          │
└─────────────────────────────────────────────────────────────┘
```

---

## Conclusion

Performance targets are **aggressive but achievable** with proper optimization:

1. **Sub-100ms discovery** operations via caching and indexing
2. **Sub-10s single-pack generation** via parallel rendering
3. **Sub-30s multi-pack composition** via dependency optimization
4. **Comprehensive benchmarking** with Criterion.rs
5. **Continuous monitoring** in CI/CD pipeline

These targets ensure a **responsive, production-ready packs system** that scales to 1000+ packs and complex compositions.
