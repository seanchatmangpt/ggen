<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Performance Strategy, SLOs, and Monitoring](#performance-strategy-slos-and-monitoring)
  - [Overview](#overview)
  - [Service Level Objectives (SLOs)](#service-level-objectives-slos)
    - [Search Performance](#search-performance)
    - [Installation Performance](#installation-performance)
    - [Publishing Performance](#publishing-performance)
    - [Backend Adapter Overhead](#backend-adapter-overhead)
  - [Performance Optimization Strategies](#performance-optimization-strategies)
    - [1. SPARQL Query Optimization](#1-sparql-query-optimization)
    - [2. Caching Strategy](#2-caching-strategy)
    - [3. RDF Store Optimization](#3-rdf-store-optimization)
    - [4. Parallel Processing](#4-parallel-processing)
    - [5. Signature Verification Optimization](#5-signature-verification-optimization)
  - [Performance Monitoring](#performance-monitoring)
    - [OpenTelemetry Instrumentation](#opentelemetry-instrumentation)
    - [Performance Dashboard (Grafana)](#performance-dashboard-grafana)
  - [Performance Testing](#performance-testing)
    - [Benchmark Suite](#benchmark-suite)
    - [Load Testing](#load-testing)
  - [Performance Optimization Checklist](#performance-optimization-checklist)
    - [Pre-Launch Optimization](#pre-launch-optimization)
    - [Post-Launch Monitoring](#post-launch-monitoring)
    - [Continuous Improvement](#continuous-improvement)
  - [Success Criteria](#success-criteria)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Performance Strategy, SLOs, and Monitoring

## Overview

The performance strategy ensures marketplace-v2 meets or exceeds v1 performance while adding new capabilities like RDF search and cryptographic signing.

## Service Level Objectives (SLOs)

### Search Performance

| Metric | V1 Baseline | V2 Target | Measurement |
|--------|-------------|-----------|-------------|
| Search latency (p50) | 45ms | <50ms | Response time for simple queries |
| Search latency (p95) | 120ms | <100ms | Response time for complex queries |
| Search latency (p99) | 250ms | <200ms | Worst-case response time |
| Throughput | 100 req/s | ≥100 req/s | Concurrent search requests |
| Result quality | Baseline | ≥Baseline | Relevance score comparison |

### Installation Performance

| Metric | V1 Baseline | V2 Target | Measurement |
|--------|-------------|-----------|-------------|
| Package lookup | 20ms | <25ms | Time to locate package |
| Dependency resolution | 150ms | <200ms | Time to resolve all dependencies |
| Download + install | 2.5s | <3s | Total installation time (small package) |
| Signature verification | N/A | <10ms | Ed25519 verification time |
| Total install time (p95) | 3s | <5s | End-to-end installation |

### Publishing Performance

| Metric | V1 Baseline | V2 Target | Measurement |
|--------|-------------|-----------|-------------|
| Manifest validation | 50ms | <75ms | Validate gpack.yaml |
| Package upload | 1.5s | <2s | Upload package archive |
| RDF indexing | N/A | <100ms | Generate and store RDF triples |
| Signature generation | N/A | <5ms | Ed25519 signing |
| Total publish time | 2s | <3s | End-to-end publishing |

### Backend Adapter Overhead

| Metric | Target | Measurement |
|--------|--------|-------------|
| Adapter overhead | <5% | Additional latency from adapter layer |
| Fallback time | <100ms | Time to detect failure and fall back to V1 |
| Memory overhead | <10MB | Additional memory for dual backend |

## Performance Optimization Strategies

### 1. SPARQL Query Optimization

```rust
// ggen-marketplace/src/search/query_optimizer.rs

pub struct SparqlQueryOptimizer {
    query_cache: Arc<Mutex<LruCache<String, String>>>,
    prepared_statements: Arc<DashMap<String, PreparedQuery>>,
}

impl SparqlQueryOptimizer {
    /// Optimize SPARQL query for common patterns
    pub fn optimize(&self, query: &str) -> String {
        // 1. Add FILTER push-down
        let with_filters = self.push_down_filters(query);

        // 2. Reorder triple patterns for selectivity
        let reordered = self.reorder_triple_patterns(&with_filters);

        // 3. Add LIMIT early for pagination
        let with_limit = self.add_early_limit(&reordered);

        // 4. Use property paths efficiently
        let optimized = self.optimize_property_paths(&with_limit);

        optimized
    }

    /// Push filters down to reduce intermediate results
    fn push_down_filters(&self, query: &str) -> String {
        // Move FILTER clauses closer to relevant triple patterns
        // Example: FILTER(CONTAINS(?name, "rust")) next to ?pkg pkg:name ?name
        query.to_string()  // Implementation details
    }

    /// Reorder triple patterns by selectivity (most selective first)
    fn reorder_triple_patterns(&self, query: &str) -> String {
        // Order: literal values > URIs > variables
        // Example: pkg:version "1.0.0" before pkg:name ?name
        query.to_string()  // Implementation details
    }

    /// Use prepared statements for common queries
    pub fn prepare(&mut self, query_template: &str) -> PreparedQuery {
        let compiled = self.compile_query_template(query_template);
        let id = uuid::Uuid::new_v4().to_string();
        self.prepared_statements.insert(id.clone(), compiled.clone());
        compiled
    }
}

/// Prepared SPARQL query for reuse
#[derive(Clone)]
pub struct PreparedQuery {
    pub template: String,
    pub parameters: Vec<String>,
}

impl PreparedQuery {
    pub fn bind(&self, params: &HashMap<String, String>) -> String {
        let mut query = self.template.clone();
        for (key, value) in params {
            query = query.replace(&format!("${{{}}}", key), value);
        }
        query
    }
}
```

### 2. Caching Strategy

```rust
// ggen-marketplace/src/cache.rs

use moka::future::Cache;
use std::time::Duration;

pub struct MarketplaceCacheV2 {
    /// Search results cache (query → results)
    search_cache: Cache<String, SearchResults>,

    /// Package metadata cache (package_id → metadata)
    metadata_cache: Cache<PackageId, PackageMetadata>,

    /// SPARQL query cache (query → RDF results)
    sparql_cache: Cache<String, Vec<Triple>>,

    /// Signature verification cache (package_id → bool)
    signature_cache: Cache<PackageId, bool>,
}

impl MarketplaceCacheV2 {
    pub fn new() -> Self {
        Self {
            // Search results cache: 1000 entries, 5 minute TTL
            search_cache: Cache::builder()
                .max_capacity(1000)
                .time_to_live(Duration::from_secs(300))
                .build(),

            // Metadata cache: 5000 entries, 1 hour TTL
            metadata_cache: Cache::builder()
                .max_capacity(5000)
                .time_to_live(Duration::from_secs(3600))
                .build(),

            // SPARQL cache: 500 entries, 10 minute TTL
            sparql_cache: Cache::builder()
                .max_capacity(500)
                .time_to_live(Duration::from_secs(600))
                .build(),

            // Signature cache: 10000 entries, 24 hour TTL
            signature_cache: Cache::builder()
                .max_capacity(10000)
                .time_to_live(Duration::from_secs(86400))
                .build(),
        }
    }

    /// Get cached search results
    pub async fn get_search_results(&self, query: &str) -> Option<SearchResults> {
        self.search_cache.get(query).await
    }

    /// Cache search results
    pub async fn cache_search_results(&self, query: String, results: SearchResults) {
        self.search_cache.insert(query, results).await;
    }

    /// Get cached package metadata
    pub async fn get_metadata(&self, id: &PackageId) -> Option<PackageMetadata> {
        self.metadata_cache.get(id).await
    }

    /// Invalidate cache entries (on publish)
    pub async fn invalidate_package(&self, id: &PackageId) {
        self.metadata_cache.invalidate(id).await;
        // Invalidate all search results (conservative)
        self.search_cache.invalidate_all();
    }
}
```

### 3. RDF Store Optimization

```rust
// ggen-marketplace/src/registry/store_config.rs

use oxigraph::store::Store;

pub struct RdfStoreConfig {
    /// Enable bulk insert mode for faster initial loading
    pub bulk_insert: bool,

    /// Number of triples to batch in bulk insert
    pub bulk_batch_size: usize,

    /// Enable SPARQL query optimization
    pub optimize_queries: bool,

    /// Enable statistics collection for query planning
    pub collect_statistics: bool,

    /// Cache size for RDF store (in MB)
    pub cache_size_mb: usize,
}

impl Default for RdfStoreConfig {
    fn default() -> Self {
        Self {
            bulk_insert: false,
            bulk_batch_size: 10000,
            optimize_queries: true,
            collect_statistics: true,
            cache_size_mb: 256,
        }
    }
}

impl RdfStoreConfig {
    pub fn create_store(&self, path: &Path) -> Result<Store> {
        let mut store = Store::open(path)?;

        if self.bulk_insert {
            // Use bulk loading for better performance
            store.optimize()?;
        }

        Ok(store)
    }

    /// Production-optimized configuration
    pub fn production() -> Self {
        Self {
            bulk_insert: false,
            bulk_batch_size: 10000,
            optimize_queries: true,
            collect_statistics: true,
            cache_size_mb: 512,  // Larger cache for production
        }
    }

    /// Development configuration (faster iteration)
    pub fn development() -> Self {
        Self {
            bulk_insert: true,
            bulk_batch_size: 5000,
            optimize_queries: false,  // Skip optimization for faster loading
            collect_statistics: false,
            cache_size_mb: 128,
        }
    }
}
```

### 4. Parallel Processing

```rust
// ggen-marketplace/src/search/parallel_search.rs

use rayon::prelude::*;

pub struct ParallelSearchEngine {
    sparql_engine: SparqlSearchEngine,
    num_shards: usize,
}

impl ParallelSearchEngine {
    /// Search across multiple shards in parallel
    pub async fn search_parallel(&self, query: &SearchQuery) -> Result<SearchResults> {
        let start = Instant::now();

        // Split query into sub-queries for each shard
        let sub_queries: Vec<_> = (0..self.num_shards)
            .map(|shard| self.build_shard_query(query, shard))
            .collect();

        // Execute sub-queries in parallel
        let results: Vec<_> = sub_queries
            .par_iter()
            .map(|sub_query| self.execute_shard_query(sub_query))
            .collect::<Result<Vec<_>>>()?;

        // Merge and rank results
        let merged = self.merge_shard_results(results);
        let ranked = self.rank_results(merged, query);

        Ok(SearchResults {
            packages: ranked.into_iter().take(query.limit).collect(),
            total_count: ranked.len(),
            query_time_ms: start.elapsed().as_millis() as u64,
            backend_used: BackendType::V2,
        })
    }

    /// Merge results from multiple shards
    fn merge_shard_results(&self, results: Vec<Vec<Package>>) -> Vec<Package> {
        // Flatten and deduplicate
        let mut merged: HashMap<PackageId, Package> = HashMap::new();
        for shard_results in results {
            for pkg in shard_results {
                merged.insert(pkg.id.clone(), pkg);
            }
        }
        merged.into_values().collect()
    }
}
```

### 5. Signature Verification Optimization

```rust
// ggen-marketplace/src/crypto/batch_verify.rs

use ed25519_dalek::{Signature, PublicKey, Verifier};
use rayon::prelude::*;

pub struct BatchSignatureVerifier {
    cache: Arc<Mutex<LruCache<PackageId, bool>>>,
}

impl BatchSignatureVerifier {
    /// Verify multiple package signatures in parallel
    pub fn verify_batch(&self, packages: &[SignedPackage]) -> Vec<bool> {
        packages
            .par_iter()
            .map(|pkg| self.verify_single(pkg))
            .collect()
    }

    /// Verify single signature with caching
    fn verify_single(&self, pkg: &SignedPackage) -> bool {
        // Check cache first
        if let Some(cached) = self.cache.lock().unwrap().get(&pkg.id) {
            return *cached;
        }

        // Verify signature
        let public_key = match PublicKey::from_bytes(&pkg.public_key_bytes) {
            Ok(key) => key,
            Err(_) => return false,
        };

        let signature = match Signature::from_bytes(&pkg.signature_bytes) {
            Ok(sig) => sig,
            Err(_) => return false,
        };

        let message = pkg.compute_message_hash();
        let result = public_key.verify(&message, &signature).is_ok();

        // Cache result
        self.cache.lock().unwrap().put(pkg.id.clone(), result);

        result
    }
}
```

## Performance Monitoring

### OpenTelemetry Instrumentation

```rust
// ggen-marketplace/src/metrics.rs

use opentelemetry::{
    metrics::{Counter, Histogram, ObservableGauge},
    KeyValue,
};

pub struct PerformanceMetrics {
    // Latency histograms
    search_latency: Histogram<f64>,
    install_latency: Histogram<f64>,
    publish_latency: Histogram<f64>,
    sparql_query_latency: Histogram<f64>,

    // Throughput counters
    search_count: Counter<u64>,
    install_count: Counter<u64>,
    publish_count: Counter<u64>,

    // Cache metrics
    cache_hit_rate: ObservableGauge<f64>,
    cache_size: ObservableGauge<u64>,

    // Resource usage
    rdf_store_size: ObservableGauge<u64>,
    memory_usage: ObservableGauge<u64>,
}

impl PerformanceMetrics {
    pub fn new() -> Self {
        let meter = opentelemetry::global::meter("marketplace-v2");

        Self {
            search_latency: meter
                .f64_histogram("marketplace.search.latency")
                .with_description("Search operation latency in milliseconds")
                .with_unit("ms")
                .init(),

            install_latency: meter
                .f64_histogram("marketplace.install.latency")
                .with_description("Installation latency in milliseconds")
                .with_unit("ms")
                .init(),

            publish_latency: meter
                .f64_histogram("marketplace.publish.latency")
                .with_description("Publishing latency in milliseconds")
                .with_unit("ms")
                .init(),

            sparql_query_latency: meter
                .f64_histogram("marketplace.sparql.latency")
                .with_description("SPARQL query latency in milliseconds")
                .with_unit("ms")
                .init(),

            search_count: meter
                .u64_counter("marketplace.search.count")
                .with_description("Number of search operations")
                .init(),

            install_count: meter
                .u64_counter("marketplace.install.count")
                .with_description("Number of install operations")
                .init(),

            publish_count: meter
                .u64_counter("marketplace.publish.count")
                .with_description("Number of publish operations")
                .init(),

            cache_hit_rate: meter
                .f64_observable_gauge("marketplace.cache.hit_rate")
                .with_description("Cache hit rate (0-1)")
                .init(),

            cache_size: meter
                .u64_observable_gauge("marketplace.cache.size")
                .with_description("Number of entries in cache")
                .init(),

            rdf_store_size: meter
                .u64_observable_gauge("marketplace.rdf.store_size")
                .with_description("RDF store size in bytes")
                .init(),

            memory_usage: meter
                .u64_observable_gauge("marketplace.memory.usage")
                .with_description("Memory usage in bytes")
                .init(),
        }
    }

    /// Record search operation
    pub fn record_search(&self, latency_ms: f64, backend: BackendType) {
        self.search_latency.record(
            latency_ms,
            &[KeyValue::new("backend", backend.as_str())],
        );
        self.search_count.add(1, &[KeyValue::new("backend", backend.as_str())]);
    }

    /// Record installation operation
    pub fn record_install(&self, latency_ms: f64, has_signature: bool) {
        self.install_latency.record(
            latency_ms,
            &[KeyValue::new("signed", has_signature.to_string())],
        );
        self.install_count.add(
            1,
            &[KeyValue::new("signed", has_signature.to_string())],
        );
    }
}
```

### Performance Dashboard (Grafana)

```yaml
# grafana-dashboard.json (excerpt)

{
  "dashboard": {
    "title": "Marketplace V2 Performance",
    "panels": [
      {
        "title": "Search Latency (p50, p95, p99)",
        "targets": [
          {
            "expr": "histogram_quantile(0.50, marketplace_search_latency)",
            "legendFormat": "p50"
          },
          {
            "expr": "histogram_quantile(0.95, marketplace_search_latency)",
            "legendFormat": "p95"
          },
          {
            "expr": "histogram_quantile(0.99, marketplace_search_latency)",
            "legendFormat": "p99"
          }
        ],
        "alert": {
          "name": "High Search Latency",
          "conditions": [
            {
              "evaluator": { "type": "gt", "params": [100] },
              "query": { "model": "p95" }
            }
          ]
        }
      },
      {
        "title": "Backend Comparison (V1 vs V2)",
        "targets": [
          {
            "expr": "rate(marketplace_search_count{backend=\"v1\"}[5m])",
            "legendFormat": "V1 Search Rate"
          },
          {
            "expr": "rate(marketplace_search_count{backend=\"v2\"}[5m])",
            "legendFormat": "V2 Search Rate"
          }
        ]
      },
      {
        "title": "Cache Hit Rate",
        "targets": [
          {
            "expr": "marketplace_cache_hit_rate",
            "legendFormat": "Hit Rate"
          }
        ],
        "alert": {
          "name": "Low Cache Hit Rate",
          "conditions": [
            {
              "evaluator": { "type": "lt", "params": [0.7] },
              "query": { "model": "Hit Rate" }
            }
          ]
        }
      }
    ]
  }
}
```

## Performance Testing

### Benchmark Suite

```rust
// benches/marketplace_v2_performance.rs

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn benchmark_search_v1_vs_v2(c: &mut Criterion) {
    let v1_adapter = setup_v1_adapter();
    let v2_adapter = setup_v2_adapter();

    let queries = vec![
        "rust web framework",
        "microservice template",
        "machine learning",
    ];

    let mut group = c.benchmark_group("search_comparison");

    for query in &queries {
        group.bench_with_input(
            BenchmarkId::new("V1", query),
            query,
            |b, q| b.to_async(Runtime::new().unwrap()).iter(|| v1_adapter.search(q)),
        );

        group.bench_with_input(
            BenchmarkId::new("V2", query),
            query,
            |b, q| b.to_async(Runtime::new().unwrap()).iter(|| v2_adapter.search(q)),
        );
    }

    group.finish();
}

fn benchmark_signature_verification(c: &mut Criterion) {
    let packages = create_signed_packages(100);
    let verifier = BatchSignatureVerifier::new();

    c.bench_function("verify_batch_100", |b| {
        b.iter(|| verifier.verify_batch(black_box(&packages)))
    });
}

criterion_group!(
    benches,
    benchmark_search_v1_vs_v2,
    benchmark_signature_verification
);
criterion_main!(benches);
```

### Load Testing

```bash
#!/bin/bash
# load-test.sh

# Search load test (1000 requests/second for 60 seconds)
echo "Running search load test..."
wrk -t12 -c400 -d60s --latency \
  -s scripts/search_load.lua \
  http://localhost:8080/api/marketplace/search

# Installation load test (100 requests/second for 30 seconds)
echo "Running installation load test..."
wrk -t4 -c100 -d30s --latency \
  -s scripts/install_load.lua \
  http://localhost:8080/api/marketplace/install

# Generate report
python scripts/analyze_load_test.py results.json
```

## Performance Optimization Checklist

### Pre-Launch Optimization

- [ ] SPARQL query optimization (filter push-down, selectivity ordering)
- [ ] Cache implementation (search, metadata, SPARQL results)
- [ ] RDF store configuration tuning
- [ ] Parallel processing for search and signature verification
- [ ] Prepared statement optimization for common queries
- [ ] Index tuning in Oxigraph store

### Post-Launch Monitoring

- [ ] Real-time latency monitoring (p50, p95, p99)
- [ ] Cache hit rate tracking
- [ ] Backend comparison metrics (V1 vs V2)
- [ ] Resource usage monitoring (memory, disk, CPU)
- [ ] Error rate tracking
- [ ] Fallback frequency monitoring

### Continuous Improvement

- [ ] Weekly performance review
- [ ] Query pattern analysis
- [ ] Cache tuning based on usage patterns
- [ ] Index optimization based on query patterns
- [ ] A/B testing of optimization strategies
- [ ] Capacity planning based on growth trends

## Success Criteria

| Criterion | Target | Measurement |
|-----------|--------|-------------|
| Search latency (p95) | <100ms | Benchmark + production metrics |
| Install latency (p95) | <5s | End-to-end installation time |
| Adapter overhead | <5% | Comparison with direct backend calls |
| Cache hit rate | >70% | Production cache metrics |
| Throughput | ≥V1 baseline | Concurrent request handling |
| Memory overhead | <10MB | Dual backend memory usage |
| No performance regression | 100% | V2 ≥ V1 for all operations |
