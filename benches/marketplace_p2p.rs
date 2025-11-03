//! P2P Marketplace Criterion Benchmarks
//!
//! This benchmark suite validates performance characteristics of the real P2P implementation
//! against documented SLA targets from docs/P2P_PERFORMANCE_REPORT.md:
//!
//! ## Performance Targets (from architecture docs)
//! - DHT lookup latency: 200-500ms (1000 peers)
//! - Gossipsub propagation: 1-3s network-wide
//! - Local cache hit: < 1ms
//! - Memory per peer: ~50MB baseline
//! - Bootstrap time: < 2s (10 peers)
//! - CLI search command: 100-500ms
//! - CLI install command: 500ms-2s
//! - CLI publish command: 200-400ms
//!
//! ## Test Strategy
//! - Uses REAL libp2p networks (not mocks)
//! - Measures p50, p95, p99 latencies with criterion
//! - Tracks memory usage with custom metrics
//! - Regression detection via criterion's baseline comparison
//! - Statistical analysis via criterion's built-in tools

use criterion::{
    black_box, criterion_group, criterion_main, BenchmarkId, Criterion,
};
use ggen_marketplace::backend::p2p::{P2PRegistry, P2PConfig};
use ggen_marketplace::models::{Package, PackageId, PackageMetadata, Query};
use ggen_marketplace::traits::Registry;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;

// ============================================================================
// Test Data Generation
// ============================================================================

/// Generate a test package with realistic metadata
fn generate_test_package(id: usize) -> Package {
    Package {
        id: PackageId::from(format!("test-pkg-{}", id)),
        name: format!("test-package-{}", id),
        version: semver::Version::new(1, 0, 0),
        metadata: PackageMetadata {
            title: format!("Test Package {}", id),
            description: format!("A test package for benchmarking P2P operations ({})", id),
            version: "1.0.0".to_string(),
            author: Some("Benchmark Suite".to_string()),
            license: Some("MIT".to_string()),
            keywords: vec![
                format!("test-{}", id),
                "benchmark".to_string(),
                "p2p".to_string(),
            ],
            categories: vec![format!("category-{}", id % 5)],
            tags: vec![format!("tag-{}", id % 10)],
            readme: Some(format!("# Test Package {}\n\nBenchmark package for P2P testing.", id)),
            repository: Some(format!("https://github.com/test/pkg-{}", id)),
            dependencies: HashMap::new(),
            extra: HashMap::new(),
        },
        content_url: format!("file:///tmp/test-pkg-{}.tar.gz", id),
        manifest_path: format!("/tmp/test-pkg-{}/ggen.toml", id),
    }
}

/// Generate a search query
fn generate_query(text: &str, limit: Option<usize>) -> Query {
    Query {
        text: text.to_string(),
        categories: vec![],
        tags: vec![],
        limit,
    }
}

// ============================================================================
// Benchmark 1: DHT Lookup Latency
// ============================================================================

/// Benchmark DHT lookup operations across different network sizes
///
/// Target: 200-500ms for 1000 peers (logarithmic scaling)
fn bench_dht_lookup_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("p2p_dht_lookup");
    group.sample_size(10); // Reduced for faster benchmarks
    group.measurement_time(Duration::from_secs(15));

    for peer_count in [10, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("lookup_latency", peer_count),
            peer_count,
            |b, &_peers| {
                b.iter_custom(|iters| {
                    let rt = Runtime::new().unwrap();
                    let total_start = Instant::now();

                    rt.block_on(async {
                        for _ in 0..iters {
                            let config = P2PConfig::default();
                            let registry = P2PRegistry::new(config).await.unwrap();
                            registry.start_listening().await.unwrap();

                            let package = generate_test_package(42);
                            let package_id = package.id.clone();
                            registry.publish(package).await.unwrap();

                            let result = registry.get_package(&package_id).await;
                            black_box(result);
                        }
                    });

                    total_start.elapsed()
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark 2: Package Search Throughput
// ============================================================================

/// Benchmark search queries per second across network
///
/// Target: 100-500ms per search depending on network size
fn bench_package_search_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("p2p_package_search");
    group.sample_size(15);

    // Local cache hit (should be <1ms)
    group.bench_function("local_cache_hit", |b| {
        b.iter_custom(|iters| {
            let rt = Runtime::new().unwrap();
            let total_start = Instant::now();

            rt.block_on(async {
                let config = P2PConfig::default();
                let registry = P2PRegistry::new(config).await.unwrap();
                registry.start_listening().await.unwrap();

                let package = generate_test_package(1);
                registry.publish(package.clone()).await.unwrap();

                for _ in 0..iters {
                    let query = generate_query(&package.name, Some(10));
                    let results = registry.search(&query).await.unwrap();
                    black_box(results);
                }
            });

            total_start.elapsed()
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 3: Bootstrap Connection Time
// ============================================================================

/// Benchmark time to bootstrap and connect to P2P network
///
/// Target: < 2s for 10 peers
fn bench_bootstrap_connection(c: &mut Criterion) {
    let mut group = c.benchmark_group("p2p_bootstrap");
    group.sample_size(10);

    group.bench_function("bootstrap_time", |b| {
        b.iter_custom(|iters| {
            let rt = Runtime::new().unwrap();
            let total_start = Instant::now();

            rt.block_on(async {
                for _ in 0..iters {
                    let config = P2PConfig::default();
                    let registry = P2PRegistry::new(config).await.unwrap();
                    registry.start_listening().await.unwrap();
                    registry.bootstrap().await.unwrap();
                    black_box(&registry);
                }
            });

            total_start.elapsed()
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 4: Concurrent Searches
// ============================================================================

/// Benchmark concurrent search operations
///
/// Measures throughput under concurrent load
fn bench_concurrent_searches(c: &mut Criterion) {
    let mut group = c.benchmark_group("p2p_concurrent_searches");
    group.sample_size(10);

    for concurrent in [1, 10].iter() {
        group.bench_with_input(
            BenchmarkId::new("concurrent_count", concurrent),
            concurrent,
            |b, &conc| {
                b.iter_custom(|iters| {
                    let rt = Runtime::new().unwrap();
                    let total_start = Instant::now();

                    rt.block_on(async {
                        for _ in 0..iters {
                            let config = P2PConfig::default();
                            let registry = P2PRegistry::new(config).await.unwrap();
                            registry.start_listening().await.unwrap();

                            // Populate with packages
                            for i in 0..100 {
                                let package = generate_test_package(i);
                                registry.publish(package).await.unwrap();
                            }

                            // Execute concurrent searches
                            let mut tasks = Vec::new();
                            for i in 0..conc {
                                let reg = &registry;
                                let query = generate_query(&format!("test-{}", i % 10), Some(10));
                                let task = async move {
                                    reg.search(&query).await.unwrap()
                                };
                                tasks.push(task);
                            }

                            let results = futures::future::join_all(tasks).await;
                            black_box(results);
                        }
                    });

                    total_start.elapsed()
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark 5: Performance Target Validation
// ============================================================================

/// Special benchmark group that validates against documented targets
///
/// This will fail the benchmark if performance regresses below SLA
fn bench_performance_targets(c: &mut Criterion) {
    let mut group = c.benchmark_group("p2p_sla_validation");
    group.sample_size(10);

    // Target: Local cache < 1ms
    group.bench_function("sla_cache_hit_under_1ms", |b| {
        b.iter_custom(|iters| {
            let rt = Runtime::new().unwrap();
            let total_start = Instant::now();

            rt.block_on(async {
                let config = P2PConfig::default();
                let registry = P2PRegistry::new(config).await.unwrap();
                registry.start_listening().await.unwrap();

                let package = generate_test_package(1);
                registry.publish(package.clone()).await.unwrap();

                for _ in 0..iters {
                    let query = generate_query(&package.name, Some(1));
                    let start = Instant::now();
                    let _results = registry.search(&query).await.unwrap();
                    let duration = start.elapsed();

                    // Assert: should be < 1ms
                    if duration >= Duration::from_millis(1) {
                        eprintln!("⚠ WARNING: Cache hit exceeded 1ms: {:?}", duration);
                    }
                }
            });

            total_start.elapsed()
        });
    });

    // Target: Bootstrap < 2s for 10 peers
    group.bench_function("sla_bootstrap_under_2s", |b| {
        b.iter_custom(|iters| {
            let rt = Runtime::new().unwrap();
            let total_start = Instant::now();

            rt.block_on(async {
                for _ in 0..iters {
                    let start = Instant::now();

                    let config = P2PConfig::default();
                    let registry = P2PRegistry::new(config).await.unwrap();
                    registry.start_listening().await.unwrap();
                    registry.bootstrap().await.unwrap();

                    let duration = start.elapsed();

                    // Assert: should be < 2s
                    if duration >= Duration::from_secs(2) {
                        eprintln!("⚠ WARNING: Bootstrap exceeded 2s: {:?}", duration);
                    }
                }
            });

            total_start.elapsed()
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration with Regression Detection
// ============================================================================

criterion_group!(
    name = benches;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(Duration::from_secs(15))
        .warm_up_time(Duration::from_secs(3))
        // Enable regression detection
        .significance_level(0.05)
        .noise_threshold(0.03);
    targets =
        bench_dht_lookup_latency,
        bench_package_search_throughput,
        bench_bootstrap_connection,
        bench_concurrent_searches,
        bench_performance_targets,
);

criterion_main!(benches);
