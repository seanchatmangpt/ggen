# V3OptimizedRegistry Implementation Guide

## Overview

V3OptimizedRegistry is a production-grade marketplace registry with advanced caching, parallel search, and performance monitoring capabilities. It extends the baseline AsyncRepository trait with optimizations designed to achieve:

- **Search latency**: <50ms for typical queries (vs ~100ms baseline)
- **Cache hit rate**: >70% on repeated queries
- **Batch operations**: Linear scaling with number of packages
- **Memory overhead**: <10% compared to standard Registry

## Architecture

### Three-Layer Cache System

```
┌─────────────────────────────────────────┐
│  Query Cache (moka::future::Cache)      │
│  - Hot query results                    │
│  - 1000 entries max                     │
│  - 5 minute TTL                         │
└────────────────┬────────────────────────┘
                 │
┌────────────────v────────────────────────┐
│ Metadata Cache (moka::future::Cache)    │
│ - Package metadata                      │
│ - 5000 entries max                      │
│ - 1 hour TTL                            │
└────────────────┬────────────────────────┘
                 │
┌────────────────v────────────────────────┐
│  Query Plan Cache (LRU)                 │
│ - Compiled SPARQL query plans           │
│ - 256 most-recent plans                 │
│ - No TTL (LRU eviction)                 │
└────────────────┬────────────────────────┘
                 │
┌────────────────v────────────────────────┐
│  RDF Store (oxigraph)                   │
│  - Source of truth                      │
│  - Full package data                    │
└─────────────────────────────────────────┘
```

### Indexing & Search

- **Full-text search index**: In-memory IndexMap mapping terms to package URIs
- **Parallel search**: Uses rayon for parallel token processing on multi-core systems
- **Incremental updates**: `update_search_index()` for adding individual packages

### Metrics Collection

Prometheus-style metrics collected via `V3Metrics`:

```rust
pub struct V3Metrics {
    pub cache_hits: AtomicU64,           // Total cache hits
    pub cache_misses: AtomicU64,         // Total cache misses
    pub batch_ops: AtomicU64,            // Batch operations count
    pub parallel_searches: AtomicU64,    // Parallel searches count
    pub cache_size: AtomicU64,           // Gauge: current cache size
    pub search_index_entries: AtomicU64, // Gauge: index entries
    pub latency_buckets: [AtomicU64; 8], // Histogram: latency ranges
}
```

Latency buckets:
- [0] <10us
- [1] <50us
- [2] <100us
- [3] <500us
- [4] <1ms
- [5] <5ms
- [6] <10ms
- [7] >=10ms

## Public API

### Core Methods

#### `new(primary_store: Arc<Store>) -> Result<Self>`
Creates a new V3OptimizedRegistry with initialized caches and search index.

```rust
let store = Arc::new(Store::new()?);
let registry = V3OptimizedRegistry::new(store)?;
```

#### `async fn batch_insert(&self, manifests: Vec<Manifest>) -> Result<Vec<Package>>`
Atomically inserts multiple packages (all-or-nothing semantics).

```rust
let results = registry.batch_insert(manifests).await?;
```

#### `async fn batch_delete(&self, ids: Vec<PackageId>) -> Result<u64>`
Atomically deletes multiple packages and invalidates affected caches.

```rust
let deleted = registry.batch_delete(vec![id1, id2, id3]).await?;
println!("Deleted {} packages", deleted);
```

#### `fn search_parallel(&self, term: &str) -> Vec<String>`
Parallel search using rayon; splits search terms and processes each in parallel.

```rust
let results = registry.search_parallel("test package");
```

#### `fn update_search_index(&self, package_id: &str, package_name: &str) -> Result<()>`
Updates the full-text search index for a single package.

```rust
registry.update_search_index("pkg-123", "My Package Name")?;
```

#### `fn stats(&self) -> V3RegistryStats`
Returns aggregated statistics for monitoring and debugging.

```rust
let stats = registry.stats();
println!("Cache hit rate: {:.2}%", stats.cache_hit_rate * 100.0);
println!("Average latency: {}us", stats.avg_latency_us);
```

#### `fn metrics_snapshot(&self) -> V3MetricsSnapshot`
Returns a snapshot of all Prometheus-style metrics.

```rust
let snapshot = registry.metrics_snapshot();
println!("Cache hits: {}, misses: {}", snapshot.cache_hits, snapshot.cache_misses);
```

#### `fn record_latency(&self, latency_us: u64)`
Records operation latency in the appropriate histogram bucket.

```rust
let start = Instant::now();
// ... operation ...
let elapsed = start.elapsed().as_micros() as u64;
registry.record_latency(elapsed);
```

#### `fn reset_stats(&self)`
Resets all statistics and metrics to zero.

```rust
registry.reset_stats();
```

### AsyncRepository Trait Implementation

V3OptimizedRegistry implements all AsyncRepository methods with caching:

```rust
#[async_trait]
impl AsyncRepository for V3OptimizedRegistry {
    async fn get_package(&self, id: &PackageId) -> Result<Package>;
    async fn get_package_version(&self, id: &PackageId, version: &PackageVersion) -> Result<Package>;
    async fn all_packages(&self) -> Result<Vec<Package>>;
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<PackageVersion>>;
    async fn package_exists(&self, id: &PackageId) -> Result<bool>;
}
```

## Performance Characteristics

### Cache Hit Rate

With typical usage patterns:
- **Hot query cache** (5 min TTL): 40-60% hit rate on frequently accessed packages
- **Metadata cache** (1 hour TTL): 30-50% hit rate on version queries
- **Query plan cache** (LRU, unbounded): 60-80% hit rate on repeated SPARQL queries
- **Combined hit rate target**: >70%

### Latency SLOs

Measured end-to-end latencies (including cache lookup + RDF query):

| Operation | Target | Notes |
|-----------|--------|-------|
| Cache hit | <10us | In-memory lookup, no I/O |
| Metadata cache hit | <50us | Async moka cache with TTL checks |
| RDF query | <100ms | Full store query, no cache |
| Batch insert (10 pkgs) | <200ms | Transaction overhead + indexing |
| Batch delete (10 pkgs) | <150ms | Cache invalidation included |
| Parallel search (1000 index entries) | <50ms | rayon parallelism on multi-core |

### Memory Overhead

- **Hot query cache**: ~100KB per 1000 entries
- **Metadata cache**: ~500KB per 1000 entries
- **Search index**: ~1MB per 10,000 index terms
- **Total per 10,000 packages**: <10MB overhead vs RDF store

## Benchmarks

Run criterion benchmarks to measure performance:

```bash
# Run all v3 benchmarks
cargo bench -p ggen-marketplace --bench v3_registry

# Run specific benchmark
cargo bench -p ggen-marketplace --bench v3_registry -- cache_hit

# Generate HTML report
cargo bench -p ggen-marketplace --bench v3_registry -- --output-format bencher
# Results: target/criterion/report/index.html
```

### Available Benchmarks

| Benchmark | What | Purpose |
|-----------|------|---------|
| `v3_cache_hit_metadata_cache` | Metadata cache hit | Measure cache lookup latency |
| `v3_search_sequential` | Sequential search | Baseline for parallel comparison |
| `v3_search_parallel` | Parallel search (rayon) | Measure parallelism benefit |
| `v3_stats_collection` | Statistics snapshot | Measure atomic operations overhead |
| `v3_metrics_snapshot` | Metrics snapshot | Measure metrics collection cost |
| `v3_latency_recording` | Latency histogram | Measure histogram bucketing overhead |
| `v3_search_index_update` | Index update | Measure incremental index performance |
| `v3_query_plan_cache_lookup` | Query plan cache | Measure plan cache hit latency |
| `v3_batch_operations` | Batch delete (10, 50, 100, 500) | Measure batch operation scaling |

## Testing

### Unit Tests

Located in `tests/v3_registry_test.rs`:

```bash
# Run all v3 tests
cargo test -p ggen-marketplace v3_registry

# Run specific test
cargo test -p ggen-marketplace v3_registry_test::test_v3_parallel_search
```

Available tests:
- `test_v3_registry_creation` - Basic initialization
- `test_v3_stats_reset` - Statistics reset
- `test_v3_parallel_search` - Parallel search execution
- `test_v3_metrics_snapshot` - Metrics collection
- `test_v3_latency_recording` - Histogram bucketing
- `test_v3_batch_operations_metric` - Batch operation counting
- `test_v3_search_index_update` - Index updates
- `test_v3_display_formatting` - Display impl
- `test_v3_multiple_searches` - Metrics accumulation

### Integration Tests

Tests for AsyncRepository trait implementation:

```bash
cargo test -p ggen-marketplace --test v3_registry_test
```

## Example Usage

### Basic Registry Operations

```rust
use ggen_marketplace::v3::V3OptimizedRegistry;
use oxigraph::store::Store;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize
    let store = Arc::new(Store::new()?);
    let registry = V3OptimizedRegistry::new(store)?;

    // Search
    let results = registry.search_parallel("database");
    println!("Found {} results", results.len());

    // Check package existence
    let exists = registry.package_exists(&package_id).await?;
    println!("Package exists: {}", exists);

    // Get metrics
    let metrics = registry.metrics_snapshot();
    println!("Cache hits: {}, misses: {}", metrics.cache_hits, metrics.cache_misses);

    Ok(())
}
```

### Performance Monitoring

```rust
use ggen_marketplace::v3::V3OptimizedRegistry;
use std::time::Instant;

// Monitor operation latency
let start = Instant::now();
let _package = registry.get_package(&id).await?;
let elapsed = start.elapsed().as_micros() as u64;
registry.record_latency(elapsed);

// Check statistics
let stats = registry.stats();
println!("Total queries: {}", stats.total_queries);
println!("Hit rate: {:.2}%", stats.cache_hit_rate * 100.0);
println!("Average latency: {}us", stats.avg_latency_us);
```

### Batch Operations

```rust
use ggen_marketplace::models::PackageId;

// Batch delete with atomic semantics
let ids = vec![id1, id2, id3];
match registry.batch_delete(ids).await {
    Ok(deleted) => println!("Deleted {} packages", deleted),
    Err(e) => println!("Deletion failed: {}", e),
}

// Verify cache invalidation
let snapshot = registry.metrics_snapshot();
println!("Batch operations: {}", snapshot.batch_ops);
```

## Future Optimizations

Potential enhancements for V4:

1. **Distributed caching**: Redis-backed cache across multiple instances
2. **Adaptive TTLs**: Dynamically adjust cache TTLs based on hit rate
3. **Query result streaming**: Support for large result sets
4. **Bloom filters**: Quick existence checks before cache lookups
5. **Compression**: Compress cached metadata to reduce memory
6. **Sharding**: Partition registry for horizontal scaling
7. **Write-through cache**: Synchronize cache with RDF store on writes
8. **Replica consistency**: Multi-replica consensus for HA

## Troubleshooting

### Low cache hit rate

If cache hit rate is <50%:
1. Check cache capacity settings (default: 1000 hot, 5000 metadata)
2. Verify TTL values match query patterns
3. Monitor search_parallel vs metadata hits separately
4. Consider adjusting LRU capacity for query plans

### High latency spikes

If latencies occasionally exceed 100ms:
1. Check RDF store query complexity
2. Profile parallel search term distribution
3. Monitor GC pauses in async runtime
4. Consider batch operations instead of individual queries

### Memory bloat

If cache memory grows unexpectedly:
1. Monitor cache_size gauge in metrics snapshot
2. Check for memory leaks in RDF store
3. Verify cache eviction (TTL/LRU) is working
4. Review search_index_entries - consider term pruning

## References

- [AsyncRepository Trait](../traits.rs)
- [Models](../models.rs)
- [Benchmark Suite](../../benches/v3_registry.rs)
- [Unit Tests](../../tests/v3_registry_test.rs)
