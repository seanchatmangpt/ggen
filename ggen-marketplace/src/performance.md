# Performance Considerations

This document outlines performance optimization strategies, benchmarking approaches, and best practices for the ggen-marketplace library.

## Table of Contents

1. [Performance Goals](#performance-goals)
2. [Async/Await Optimization](#asyncawait-optimization)
3. [Memory Management](#memory-management)
4. [Database Performance](#database-performance)
5. [Storage Optimization](#storage-optimization)
6. [Search Performance](#search-performance)
7. [Network Optimization](#network-optimization)
8. [Benchmarking](#benchmarking)
9. [Profiling](#profiling)
10. [Scalability](#scalability)

---

## Performance Goals

### Target Metrics

| Operation | Target | Excellent | Notes |
|-----------|--------|-----------|-------|
| Package Search | < 50ms | < 20ms | Full-text search with 10k packages |
| Metadata Lookup | < 10ms | < 5ms | Single package metadata retrieval |
| Package Download | > 50 MB/s | > 100 MB/s | Limited by network/disk |
| Package Upload | > 30 MB/s | > 50 MB/s | Includes checksum computation |
| Dependency Resolution | < 100ms | < 50ms | For package with 50 dependencies |
| Signature Verification | < 5ms | < 2ms | Ed25519 signature |
| Registry Sync | > 1000 pkg/s | > 5000 pkg/s | Initial sync from remote |

### Throughput Goals

- **Concurrent Users**: Support 1000+ concurrent connections
- **Package Publish Rate**: 100+ packages/second
- **Search QPS**: 10,000+ queries/second (with caching)
- **Download Bandwidth**: 10+ Gbps aggregate

---

## Async/Await Optimization

### Best Practices

#### 1. Avoid Blocking Operations

```rust
// ❌ BAD: Blocking operation in async context
async fn bad_example() {
    let data = std::fs::read("file.txt").unwrap(); // Blocks executor!
}

// ✅ GOOD: Use async file operations
async fn good_example() -> Result<Vec<u8>> {
    let data = tokio::fs::read("file.txt").await?;
    Ok(data)
}
```

#### 2. Use `spawn_blocking` for CPU-Intensive Work

```rust
use tokio::task::spawn_blocking;

async fn compute_checksum(data: Vec<u8>) -> Result<ContentHash> {
    // CPU-intensive hashing in blocking thread pool
    let hash = spawn_blocking(move || {
        use sha2::{Sha256, Digest};
        let mut hasher = Sha256::new();
        hasher.update(&data);
        ContentHash::from(hasher.finalize())
    }).await?;

    Ok(hash)
}
```

#### 3. Concurrent Operations with `join!` and `try_join!`

```rust
use tokio::try_join;

async fn fetch_package_info(id: &PackageId) -> Result<PackageInfo> {
    // Fetch metadata and stats concurrently
    let (metadata, stats, downloads) = try_join!(
        registry.get(id),
        registry.get_stats(id),
        analytics.get_downloads(id),
    )?;

    Ok(PackageInfo {
        metadata,
        stats,
        downloads,
    })
}
```

#### 4. Use `Stream` for Large Result Sets

```rust
use futures::stream::{Stream, StreamExt};

async fn list_all_packages(
    registry: &impl Registry,
) -> Result<impl Stream<Item = Result<PackageMetadata>>> {
    // Stream packages instead of loading all into memory
    let stream = registry.stream_packages().await?;
    Ok(stream)
}

// Usage
let mut stream = list_all_packages(&registry).await?;
while let Some(package) = stream.next().await {
    let package = package?;
    process_package(package).await?;
}
```

#### 5. Implement Cancellation with `select!`

```rust
use tokio::{select, time::{sleep, Duration}};

async fn download_with_timeout(
    store: &impl PackageStore,
    hash: &ContentHash,
) -> Result<Bytes> {
    select! {
        result = store.get(hash) => result,
        _ = sleep(Duration::from_secs(30)) => {
            Err(anyhow::anyhow!("Download timeout"))
        }
    }
}
```

### Async Runtime Configuration

```rust
// Configure Tokio runtime for optimal performance
use tokio::runtime::Builder;

fn create_runtime() -> tokio::runtime::Runtime {
    Builder::new_multi_thread()
        .worker_threads(num_cpus::get())
        .thread_name("marketplace-worker")
        .thread_stack_size(3 * 1024 * 1024) // 3MB stack
        .enable_all()
        .build()
        .unwrap()
}
```

---

## Memory Management

### Zero-Copy Optimizations

#### 1. Use `Bytes` for Efficient Buffer Management

```rust
use bytes::{Bytes, BytesMut};

pub struct PackageData {
    // Bytes allows cheap cloning via reference counting
    content: Bytes,
    metadata: PackageMetadata,
}

impl PackageData {
    pub fn content(&self) -> Bytes {
        // This is a cheap clone (just increments ref count)
        self.content.clone()
    }

    pub fn slice(&self, start: usize, end: usize) -> Bytes {
        // Zero-copy slice
        self.content.slice(start..end)
    }
}
```

#### 2. Use `Cow<str>` for Flexible String Handling

```rust
use std::borrow::Cow;

pub struct PackageMetadata {
    pub name: Cow<'static, str>,
    pub description: Cow<'static, str>,
}

impl PackageMetadata {
    // For static strings (e.g., from constants)
    pub fn with_static_name(name: &'static str) -> Self {
        Self {
            name: Cow::Borrowed(name),
            description: Cow::Borrowed(""),
        }
    }

    // For dynamic strings
    pub fn with_owned_name(name: String) -> Self {
        Self {
            name: Cow::Owned(name),
            description: Cow::Borrowed(""),
        }
    }
}
```

#### 3. Avoid Unnecessary Allocations

```rust
// ❌ BAD: Multiple allocations
fn format_package_id_bad(id: &PackageId) -> String {
    let mut result = String::new();
    result.push_str(&id.name);
    result.push_str(":");
    result.push_str(&id.version.to_string());
    result
}

// ✅ GOOD: Single allocation with capacity hint
fn format_package_id_good(id: &PackageId) -> String {
    let version_str = id.version.to_string();
    let mut result = String::with_capacity(id.name.len() + 1 + version_str.len());
    result.push_str(&id.name);
    result.push(':');
    result.push_str(&version_str);
    result
}

// ✅ BETTER: Use format! macro (optimized by compiler)
fn format_package_id_best(id: &PackageId) -> String {
    format!("{}:{}", id.name, id.version)
}
```

### Memory Pooling

```rust
use bytes::BytesMut;
use std::sync::Arc;
use parking_lot::Mutex;

pub struct BufferPool {
    pools: Vec<Arc<Mutex<Vec<BytesMut>>>>,
    sizes: Vec<usize>,
}

impl BufferPool {
    pub fn new() -> Self {
        let sizes = vec![4096, 8192, 16384, 65536];
        let pools = sizes.iter()
            .map(|_| Arc::new(Mutex::new(Vec::with_capacity(100))))
            .collect();

        Self { pools, sizes }
    }

    pub fn get(&self, size: usize) -> BytesMut {
        // Find appropriate pool
        let pool_idx = self.sizes.iter()
            .position(|&s| s >= size)
            .unwrap_or(self.sizes.len() - 1);

        let mut pool = self.pools[pool_idx].lock();
        pool.pop().unwrap_or_else(|| BytesMut::with_capacity(self.sizes[pool_idx]))
    }

    pub fn release(&self, mut buf: BytesMut) {
        let capacity = buf.capacity();

        // Find appropriate pool
        if let Some(pool_idx) = self.sizes.iter().position(|&s| s == capacity) {
            buf.clear();
            let mut pool = self.pools[pool_idx].lock();
            if pool.len() < 100 {
                pool.push(buf);
            }
        }
    }
}
```

---

## Database Performance

### Connection Pooling

```rust
use sqlx::postgres::PgPoolOptions;
use std::time::Duration;

pub async fn create_registry_pool(database_url: &str) -> Result<sqlx::PgPool> {
    PgPoolOptions::new()
        .max_connections(50) // Adjust based on workload
        .min_connections(5)
        .acquire_timeout(Duration::from_secs(30))
        .idle_timeout(Duration::from_secs(600))
        .max_lifetime(Duration::from_secs(1800))
        .connect(database_url)
        .await
        .map_err(Into::into)
}
```

### Query Optimization

#### 1. Use Prepared Statements

```rust
use sqlx::{PgPool, query_as};

pub struct LocalRegistry {
    pool: PgPool,
}

impl LocalRegistry {
    pub async fn get(&self, id: &PackageId) -> Result<PackageMetadata> {
        // sqlx automatically prepares and caches this query
        let metadata = query_as!(
            PackageMetadata,
            r#"
            SELECT name, version, description, authors, license
            FROM packages
            WHERE name = $1 AND version = $2
            "#,
            id.name,
            id.version.to_string()
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(metadata)
    }
}
```

#### 2. Batch Operations

```rust
// ❌ BAD: N+1 query problem
async fn get_multiple_packages_bad(
    registry: &LocalRegistry,
    ids: &[PackageId],
) -> Result<Vec<PackageMetadata>> {
    let mut results = Vec::new();
    for id in ids {
        results.push(registry.get(id).await?);
    }
    Ok(results)
}

// ✅ GOOD: Single batch query
async fn get_multiple_packages_good(
    pool: &PgPool,
    ids: &[PackageId],
) -> Result<Vec<PackageMetadata>> {
    let names: Vec<_> = ids.iter().map(|id| &id.name).collect();
    let versions: Vec<_> = ids.iter().map(|id| id.version.to_string()).collect();

    let metadata = query_as!(
        PackageMetadata,
        r#"
        SELECT name, version, description, authors, license
        FROM packages
        WHERE (name, version) = ANY(SELECT unnest($1::text[]), unnest($2::text[]))
        "#,
        &names as &[&str],
        &versions as &[String]
    )
    .fetch_all(pool)
    .await?;

    Ok(metadata)
}
```

#### 3. Use Appropriate Indexes

```sql
-- Package lookup by name and version
CREATE INDEX idx_packages_name_version ON packages(name, version);

-- Package search by keywords
CREATE INDEX idx_packages_keywords ON packages USING GIN(keywords);

-- Download statistics
CREATE INDEX idx_downloads_package_date ON downloads(package_id, downloaded_at);

-- Composite index for common queries
CREATE INDEX idx_packages_category_downloads
ON packages(category, download_count DESC);
```

### Caching Layer

```rust
use moka::future::Cache;
use std::time::Duration;

pub struct CachedRegistry<R: Registry> {
    inner: R,
    metadata_cache: Cache<PackageId, Arc<PackageMetadata>>,
    version_cache: Cache<String, Arc<Vec<Version>>>,
}

impl<R: Registry> CachedRegistry<R> {
    pub fn new(inner: R) -> Self {
        Self {
            inner,
            metadata_cache: Cache::builder()
                .max_capacity(10_000)
                .time_to_live(Duration::from_secs(300))
                .build(),
            version_cache: Cache::builder()
                .max_capacity(5_000)
                .time_to_live(Duration::from_secs(60))
                .build(),
        }
    }
}

#[async_trait]
impl<R: Registry> Registry for CachedRegistry<R> {
    async fn get(&self, id: &PackageId) -> Result<PackageMetadata> {
        // Try cache first
        if let Some(cached) = self.metadata_cache.get(id).await {
            return Ok((*cached).clone());
        }

        // Cache miss - fetch from inner registry
        let metadata = self.inner.get(id).await?;
        self.metadata_cache.insert(id.clone(), Arc::new(metadata.clone())).await;

        Ok(metadata)
    }

    async fn list_versions(&self, name: &str, namespace: Option<&str>) -> Result<Vec<Version>> {
        let cache_key = format!("{}:{:?}", name, namespace);

        if let Some(cached) = self.version_cache.get(&cache_key).await {
            return Ok((*cached).clone());
        }

        let versions = self.inner.list_versions(name, namespace).await?;
        self.version_cache.insert(cache_key, Arc::new(versions.clone())).await;

        Ok(versions)
    }

    // Other methods delegate to inner registry
    // with appropriate cache invalidation
}
```

---

## Storage Optimization

### Streaming Large Files

```rust
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::fs::File;
use futures::stream::{Stream, StreamExt};

pub async fn stream_upload(
    store: &impl PackageStore,
    file_path: &Path,
) -> Result<ContentHash> {
    let file = File::open(file_path).await?;
    let size = file.metadata().await?.len();

    // Stream upload with progress
    let hash = store.upload_stream(file, Some(size)).await?;

    Ok(hash)
}

pub async fn stream_download(
    store: &impl PackageStore,
    hash: &ContentHash,
    dest_path: &Path,
) -> Result<()> {
    let mut stream = store.stream(hash).await?;
    let mut dest = File::create(dest_path).await?;

    while let Some(chunk) = stream.next().await {
        let chunk = chunk?;
        dest.write_all(&chunk).await?;
    }

    dest.sync_all().await?;

    Ok(())
}
```

### Content Deduplication

```rust
use std::collections::HashMap;

pub struct DeduplicatingStore<S: PackageStore> {
    inner: S,
    refcounts: Arc<RwLock<HashMap<ContentHash, usize>>>,
}

#[async_trait]
impl<S: PackageStore> PackageStore for DeduplicatingStore<S> {
    async fn store(&self, package_bytes: Bytes) -> Result<ContentHash> {
        // Compute hash first
        let hash = ContentHash::sha256(&package_bytes);

        // Check if already stored
        {
            let mut refcounts = self.refcounts.write().await;
            if let Some(count) = refcounts.get_mut(&hash) {
                *count += 1;
                return Ok(hash);
            }
        }

        // Store new content
        let stored_hash = self.inner.store(package_bytes).await?;

        {
            let mut refcounts = self.refcounts.write().await;
            refcounts.insert(stored_hash.clone(), 1);
        }

        Ok(stored_hash)
    }

    async fn delete(&self, hash: &ContentHash) -> Result<()> {
        let should_delete = {
            let mut refcounts = self.refcounts.write().await;
            if let Some(count) = refcounts.get_mut(hash) {
                *count -= 1;
                if *count == 0 {
                    refcounts.remove(hash);
                    true
                } else {
                    false
                }
            } else {
                false
            }
        };

        if should_delete {
            self.inner.delete(hash).await?;
        }

        Ok(())
    }

    // Other methods delegate to inner store
}
```

### Compression

```rust
use flate2::write::GzEncoder;
use flate2::Compression;

pub async fn compress_package(bytes: &[u8]) -> Result<Bytes> {
    // Use spawn_blocking for CPU-intensive compression
    let compressed = tokio::task::spawn_blocking({
        let bytes = bytes.to_vec();
        move || {
            let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
            encoder.write_all(&bytes)?;
            encoder.finish()
        }
    }).await??;

    Ok(Bytes::from(compressed))
}
```

---

## Search Performance

### Index Optimization

```rust
use tantivy::schema::*;
use tantivy::{Index, IndexWriter};

pub fn create_optimized_index() -> Result<Index> {
    let mut schema_builder = Schema::builder();

    // ID field (stored, not indexed)
    schema_builder.add_text_field("id", STRING | STORED);

    // Name field (indexed with position for phrase queries)
    schema_builder.add_text_field("name", TEXT | STORED);

    // Description (indexed, not stored to save space)
    schema_builder.add_text_field("description", TEXT);

    // Keywords (indexed with fast field for faceting)
    schema_builder.add_text_field("keywords", TEXT);

    // Downloads (fast field for sorting)
    schema_builder.add_u64_field("downloads", FAST);

    // Published date (fast field for filtering)
    schema_builder.add_date_field("published_at", FAST);

    let schema = schema_builder.build();

    // Create index with appropriate settings
    let index = Index::create_in_ram(schema);

    Ok(index)
}

pub async fn optimize_index(writer: &mut IndexWriter) -> Result<()> {
    // Merge segments for better query performance
    writer.commit()?;

    // This is CPU-intensive, run in blocking thread
    tokio::task::spawn_blocking(move || {
        writer.merge(&MergePolicyOptions::default())
    }).await??;

    Ok(())
}
```

### Query Caching

```rust
use moka::future::Cache;

pub struct CachedSearchEngine<S: SearchEngine> {
    inner: S,
    query_cache: Cache<String, Arc<Vec<SearchResult>>>,
}

impl<S: SearchEngine> CachedSearchEngine<S> {
    pub fn new(inner: S) -> Self {
        Self {
            inner,
            query_cache: Cache::builder()
                .max_capacity(1_000)
                .time_to_live(Duration::from_secs(60))
                .build(),
        }
    }
}

#[async_trait]
impl<S: SearchEngine> SearchEngine for CachedSearchEngine<S> {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchResult>> {
        // Create cache key from query
        let cache_key = format!("{:?}", query);

        if let Some(cached) = self.query_cache.get(&cache_key).await {
            return Ok((*cached).clone());
        }

        let results = self.inner.search(query).await?;
        self.query_cache.insert(cache_key, Arc::new(results.clone())).await;

        Ok(results)
    }

    // Other methods...
}
```

---

## Network Optimization

### HTTP/2 and Connection Reuse

```rust
use reqwest::Client;

pub struct RemoteRegistry {
    client: Client,
    base_url: Url,
}

impl RemoteRegistry {
    pub fn new(base_url: Url) -> Result<Self> {
        let client = Client::builder()
            .http2_prior_knowledge() // Use HTTP/2
            .pool_max_idle_per_host(10) // Connection pooling
            .pool_idle_timeout(Duration::from_secs(90))
            .timeout(Duration::from_secs(30))
            .build()?;

        Ok(Self { client, base_url })
    }
}
```

### Batch API Requests

```rust
// ❌ BAD: Multiple sequential requests
async fn fetch_packages_bad(ids: &[PackageId]) -> Result<Vec<PackageMetadata>> {
    let mut results = Vec::new();
    for id in ids {
        let metadata = fetch_package(id).await?;
        results.push(metadata);
    }
    Ok(results)
}

// ✅ GOOD: Single batch request
async fn fetch_packages_good(client: &Client, ids: &[PackageId]) -> Result<Vec<PackageMetadata>> {
    let response = client
        .post("https://api.marketplace.com/v1/packages/batch")
        .json(&json!({ "ids": ids }))
        .send()
        .await?;

    let metadata: Vec<PackageMetadata> = response.json().await?;
    Ok(metadata)
}
```

### Compression

```rust
use reqwest::Client;

pub fn create_client_with_compression() -> Result<Client> {
    Client::builder()
        .gzip(true) // Enable gzip compression
        .brotli(true) // Enable brotli compression
        .deflate(true) // Enable deflate compression
        .build()
        .map_err(Into::into)
}
```

---

## Benchmarking

### Criterion Benchmarks

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn bench_registry_operations(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let registry = rt.block_on(async {
        LocalRegistry::new(":memory:").await.unwrap()
    });

    let mut group = c.benchmark_group("registry");

    // Benchmark package registration
    group.bench_function("register", |b| {
        b.to_async(&rt).iter(|| async {
            let metadata = create_test_metadata();
            let hash = ContentHash::default();
            registry.register(black_box(&metadata), hash).await.unwrap()
        });
    });

    // Benchmark metadata lookup
    group.bench_function("get", |b| {
        let id = PackageId::new("test", "1.0.0");
        b.to_async(&rt).iter(|| async {
            registry.get(black_box(&id)).await.unwrap()
        });
    });

    group.finish();
}

fn bench_search_operations(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let search = rt.block_on(async {
        TantivySearchEngine::new("/tmp/bench-search").await.unwrap()
    });

    let mut group = c.benchmark_group("search");

    for size in [100, 1000, 10000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            size,
            |b, &size| {
                b.to_async(&rt).iter(|| async {
                    let query = SearchQuery {
                        text: "web framework".to_string(),
                        limit: size,
                        ..Default::default()
                    };
                    search.search(black_box(&query)).await.unwrap()
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, bench_registry_operations, bench_search_operations);
criterion_main!(benches);
```

### Load Testing

```rust
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub async fn load_test_marketplace(
    marketplace: &MarketplaceClient,
    num_requests: usize,
    concurrency: usize,
) -> Result<LoadTestResults> {
    let start = Instant::now();
    let success_count = Arc::new(AtomicUsize::new(0));
    let error_count = Arc::new(AtomicUsize::new(0));

    let mut tasks = Vec::new();

    for _ in 0..concurrency {
        let marketplace = marketplace.clone();
        let success_count = success_count.clone();
        let error_count = error_count.clone();
        let requests_per_task = num_requests / concurrency;

        let task = tokio::spawn(async move {
            for _ in 0..requests_per_task {
                match marketplace.search("test").await {
                    Ok(_) => {
                        success_count.fetch_add(1, Ordering::Relaxed);
                    }
                    Err(_) => {
                        error_count.fetch_add(1, Ordering::Relaxed);
                    }
                }
            }
        });

        tasks.push(task);
    }

    // Wait for all tasks
    for task in tasks {
        task.await?;
    }

    let duration = start.elapsed();
    let successes = success_count.load(Ordering::Relaxed);
    let errors = error_count.load(Ordering::Relaxed);

    Ok(LoadTestResults {
        duration,
        total_requests: num_requests,
        successful: successes,
        failed: errors,
        qps: successes as f64 / duration.as_secs_f64(),
    })
}
```

---

## Profiling

### CPU Profiling with `pprof`

```rust
#[cfg(feature = "profiling")]
use pprof::ProfilerGuard;

#[tokio::main]
async fn main() -> Result<()> {
    #[cfg(feature = "profiling")]
    let guard = ProfilerGuard::new(100)?;

    // Run workload
    run_marketplace_operations().await?;

    #[cfg(feature = "profiling")]
    {
        if let Ok(report) = guard.report().build() {
            let file = std::fs::File::create("flamegraph.svg")?;
            report.flamegraph(file)?;
        }
    }

    Ok(())
}
```

### Memory Profiling

```rust
use jemalloc_ctl::{stats, epoch};

pub fn print_memory_stats() -> Result<()> {
    // Trigger stats update
    epoch::mib()?.advance()?;

    let allocated = stats::allocated::mib()?.read()?;
    let resident = stats::resident::mib()?.read()?;

    println!("Memory allocated: {} MB", allocated / 1024 / 1024);
    println!("Memory resident: {} MB", resident / 1024 / 1024);

    Ok(())
}
```

---

## Scalability

### Horizontal Scaling

```rust
// Load balancer distributes requests across multiple instances
pub struct LoadBalancedRegistry {
    registries: Vec<Arc<dyn Registry>>,
    current: AtomicUsize,
}

impl LoadBalancedRegistry {
    pub fn new(registries: Vec<Arc<dyn Registry>>) -> Self {
        Self {
            registries,
            current: AtomicUsize::new(0),
        }
    }

    fn next_registry(&self) -> Arc<dyn Registry> {
        let index = self.current.fetch_add(1, Ordering::Relaxed) % self.registries.len();
        self.registries[index].clone()
    }
}

#[async_trait]
impl Registry for LoadBalancedRegistry {
    async fn get(&self, id: &PackageId) -> Result<PackageMetadata> {
        self.next_registry().get(id).await
    }

    // Other methods use round-robin selection
}
```

### Vertical Scaling

- **CPU**: Increase Tokio worker threads
- **Memory**: Increase cache sizes
- **Disk**: Use faster SSDs or NVMe drives
- **Network**: Use 10GbE or higher network interfaces

---

## Summary

Key performance optimizations:

1. **Async/Await**: Use non-blocking I/O throughout
2. **Memory**: Zero-copy with `Bytes` and `Cow`
3. **Database**: Connection pooling, prepared statements, caching
4. **Storage**: Streaming, compression, deduplication
5. **Search**: Index optimization, query caching
6. **Network**: HTTP/2, connection reuse, batching
7. **Benchmarking**: Regular performance testing with Criterion
8. **Profiling**: CPU and memory profiling to identify bottlenecks
9. **Scalability**: Horizontal and vertical scaling strategies

For integration examples, see [Integration Guide](integration.md).
