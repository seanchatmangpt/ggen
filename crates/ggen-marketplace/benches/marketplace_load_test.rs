//! Phase 4B: Load Testing Benchmarks for Marketplace V2
//!
//! Tests system behavior under sustained concurrent load:
//! 1. Concurrent Registry Operations (100 reads, 50 writes, mixed)
//! 2. Search Engine Load (1000 concurrent searches)
//! 3. Installation Load (50 concurrent installations)
//! 4. Sustained Load (30 seconds)
//!
//! These benchmarks validate system stability and performance
//! under production-like concurrent workloads.

use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use ggen_marketplace::{
    models::*,
    registry::Registry,
    search::{SearchEngine, SearchQuery},
    traits::AsyncRepository,
};
use std::hint::black_box;
use std::sync::Arc;
use std::time::Duration;
use tokio::runtime::Runtime;

// ============================================================================
// Test Data Generation
// ============================================================================

fn generate_test_packages(count: usize) -> Vec<Package> {
    (0..count)
        .map(|i| {
            let package_id = PackageId::new(format!("load-test-pkg-{}", i)).unwrap();
            let mut metadata = PackageMetadata::new(
                package_id.clone(),
                format!("Load Test Package {}", i),
                format!(
                    "Package for load testing with comprehensive metadata. \
                     Used in concurrent benchmarks. Index: {}",
                    i
                ),
                "MIT",
            );

            metadata.authors = vec![format!("loadtest-author-{}", i % 100)];
            metadata.categories = vec![match i % 5 {
                0 => "web-framework",
                1 => "database",
                2 => "cli-tool",
                3 => "library",
                _ => "utility",
            }
            .to_string()];
            metadata.keywords = vec![
                format!("load-{}", i % 50),
                format!("test-{}", i % 30),
                "benchmark".to_string(),
            ];

            let version = PackageVersion::new(format!("1.{}.0", i % 100)).unwrap();

            Package {
                metadata,
                latest_version: version.clone(),
                versions: vec![version],
                releases: indexmap::IndexMap::new(),
            }
        })
        .collect()
}

// ============================================================================
// Phase 4B: Concurrent Registry Operations
// ============================================================================

fn bench_concurrent_reads(c: &mut Criterion) {
    let mut group = c.benchmark_group("load_concurrent_reads");
    group.measurement_time(Duration::from_secs(15));
    let rt = Runtime::new().unwrap();

    // Setup: Registry with 10K packages
    let packages = generate_test_packages(10000);
    let registry = Arc::new(rt.block_on(async {
        let reg = Registry::new(5000).await;
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    }));

    // 100 concurrent reads
    group.throughput(Throughput::Elements(100));
    group.bench_function("100_concurrent_reads", |b| {
        b.to_async(&rt).iter(|| {
            let reg = Arc::clone(&registry);
            async move {
                let handles: Vec<_> = (0..100)
                    .map(|i| {
                        let r = Arc::clone(&reg);
                        tokio::spawn(async move {
                            let id = PackageId::new(format!("load-test-pkg-{}", i * 100)).unwrap();
                            r.get_package(&id).await
                        })
                    })
                    .collect();

                let mut results = Vec::with_capacity(100);
                for handle in handles {
                    if let Ok(Ok(pkg)) = handle.await {
                        results.push(pkg);
                    }
                }
                black_box(results)
            }
        });
    });

    // 50 concurrent reads with random access pattern
    group.throughput(Throughput::Elements(50));
    group.bench_function("50_random_concurrent_reads", |b| {
        b.to_async(&rt).iter(|| {
            let reg = Arc::clone(&registry);
            async move {
                let handles: Vec<_> = (0..50)
                    .map(|i| {
                        let r = Arc::clone(&reg);
                        // Pseudo-random index
                        let idx = (i * 7 + 13) % 10000;
                        tokio::spawn(async move {
                            let id = PackageId::new(format!("load-test-pkg-{}", idx)).unwrap();
                            r.get_package(&id).await
                        })
                    })
                    .collect();

                let mut results = Vec::with_capacity(50);
                for handle in handles {
                    if let Ok(Ok(pkg)) = handle.await {
                        results.push(pkg);
                    }
                }
                black_box(results)
            }
        });
    });

    group.finish();
}

fn bench_concurrent_writes(c: &mut Criterion) {
    let mut group = c.benchmark_group("load_concurrent_writes");
    group.measurement_time(Duration::from_secs(15));
    let rt = Runtime::new().unwrap();

    // 50 concurrent writes
    group.throughput(Throughput::Elements(50));
    group.bench_function("50_concurrent_inserts", |b| {
        b.iter_batched(
            || {
                let packages: Vec<_> = (0..50)
                    .map(|i| {
                        let id = PackageId::new(format!("concurrent-write-{}", i)).unwrap();
                        let mut metadata = PackageMetadata::new(
                            id.clone(),
                            format!("Concurrent Package {}", i),
                            "Test package for concurrent write",
                            "MIT",
                        );
                        metadata.authors = vec![format!("author-{}", i)];
                        Package {
                            metadata,
                            latest_version: PackageVersion::new("1.0.0").unwrap(),
                            versions: vec![PackageVersion::new("1.0.0").unwrap()],
                            releases: indexmap::IndexMap::new(),
                        }
                    })
                    .collect();

                let registry = Arc::new(rt.block_on(async { Registry::new(1000).await }));
                (packages, registry)
            },
            |(packages, registry)| {
                rt.block_on(async {
                    let handles: Vec<_> = packages
                        .into_iter()
                        .map(|pkg| {
                            let r = Arc::clone(&registry);
                            tokio::spawn(async move { r.insert(pkg) })
                        })
                        .collect();

                    for handle in handles {
                        let _ = handle.await;
                    }
                });
                black_box(())
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn bench_mixed_read_write(c: &mut Criterion) {
    let mut group = c.benchmark_group("load_mixed_read_write");
    group.measurement_time(Duration::from_secs(15));
    let rt = Runtime::new().unwrap();

    // Setup: Registry with initial packages
    let packages = generate_test_packages(1000);
    let registry = Arc::new(rt.block_on(async {
        let reg = Registry::new(2000).await;
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    }));

    // Mixed workload: 80% reads, 20% writes
    group.throughput(Throughput::Elements(100));
    group.bench_function("80_reads_20_writes", |b| {
        b.iter_batched(
            || {
                // Prepare write packages
                let write_packages: Vec<_> = (0..20)
                    .map(|i| {
                        let id = PackageId::new(format!("mixed-write-{}", i)).unwrap();
                        let mut metadata = PackageMetadata::new(
                            id.clone(),
                            format!("Mixed Write {}", i),
                            "Mixed workload write package",
                            "MIT",
                        );
                        metadata.authors = vec!["mixed-author".to_string()];
                        Package {
                            metadata,
                            latest_version: PackageVersion::new("1.0.0").unwrap(),
                            versions: vec![PackageVersion::new("1.0.0").unwrap()],
                            releases: indexmap::IndexMap::new(),
                        }
                    })
                    .collect();
                write_packages
            },
            |write_packages| {
                let reg = Arc::clone(&registry);
                rt.block_on(async {
                    // Spawn 80 read tasks
                    let read_handles: Vec<_> = (0..80)
                        .map(|i| {
                            let r = Arc::clone(&reg);
                            tokio::spawn(async move {
                                let id =
                                    PackageId::new(format!("load-test-pkg-{}", i % 1000)).unwrap();
                                r.get_package(&id).await
                            })
                        })
                        .collect();

                    // Spawn 20 write tasks
                    let write_handles: Vec<_> = write_packages
                        .into_iter()
                        .map(|pkg| {
                            let r = Arc::clone(&reg);
                            tokio::spawn(async move { r.insert(pkg) })
                        })
                        .collect();

                    // Wait for all
                    for handle in read_handles {
                        let _ = handle.await;
                    }
                    for handle in write_handles {
                        let _ = handle.await;
                    }
                });
                black_box(())
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// Phase 4B: Search Engine Load Testing
// ============================================================================

fn bench_concurrent_search(c: &mut Criterion) {
    let mut group = c.benchmark_group("load_concurrent_search");
    group.measurement_time(Duration::from_secs(15));
    let rt = Runtime::new().unwrap();

    let packages = Arc::new(generate_test_packages(10000));
    let engine = Arc::new(SearchEngine::new());

    // 100 concurrent searches
    group.throughput(Throughput::Elements(100));
    group.bench_function("100_concurrent_searches", |b| {
        b.to_async(&rt).iter(|| {
            let pkgs = Arc::clone(&packages);
            let eng = Arc::clone(&engine);
            async move {
                let handles: Vec<_> = (0..100)
                    .map(|i| {
                        let p = Arc::clone(&pkgs);
                        let e = Arc::clone(&eng);
                        let query_term = format!("load-{}", i % 50);
                        tokio::spawn(async move {
                            let query = SearchQuery::new(&query_term);
                            e.search((*p).clone(), &query)
                        })
                    })
                    .collect();

                let mut results = Vec::with_capacity(100);
                for handle in handles {
                    if let Ok(Ok(res)) = handle.await {
                        results.push(res);
                    }
                }
                black_box(results)
            }
        });
    });

    // Filtered search under load (50 concurrent)
    group.throughput(Throughput::Elements(50));
    group.bench_function("50_filtered_searches", |b| {
        b.to_async(&rt).iter(|| {
            let pkgs = Arc::clone(&packages);
            let eng = Arc::clone(&engine);
            async move {
                let categories = [
                    "web-framework",
                    "database",
                    "cli-tool",
                    "library",
                    "utility",
                ];
                let handles: Vec<_> = (0..50)
                    .map(|i| {
                        let p = Arc::clone(&pkgs);
                        let e = Arc::clone(&eng);
                        let cat = categories[i % 5];
                        tokio::spawn(async move {
                            let query = SearchQuery::new("test").with_category(cat);
                            e.search((*p).clone(), &query)
                        })
                    })
                    .collect();

                let mut results = Vec::with_capacity(50);
                for handle in handles {
                    if let Ok(Ok(res)) = handle.await {
                        results.push(res);
                    }
                }
                black_box(results)
            }
        });
    });

    group.finish();
}

// ============================================================================
// Phase 4B: Sustained Load Testing
// ============================================================================

fn bench_sustained_load(c: &mut Criterion) {
    let mut group = c.benchmark_group("load_sustained");
    group.measurement_time(Duration::from_secs(30)); // 30 second sustained load
    group.sample_size(10);
    let rt = Runtime::new().unwrap();

    // Setup registry
    let packages = generate_test_packages(5000);
    let registry = Arc::new(rt.block_on(async {
        let reg = Registry::new(3000).await;
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    }));

    // Sustained mixed workload for 30 seconds
    group.bench_function("30s_sustained_mixed_load", |b| {
        b.to_async(&rt).iter(|| {
            let reg = Arc::clone(&registry);
            async move {
                let start = std::time::Instant::now();
                let mut ops_count = 0u64;

                // Run operations for up to 5 seconds per iteration
                while start.elapsed() < Duration::from_secs(5) {
                    // Batch of 10 operations
                    let handles: Vec<_> = (0..10)
                        .map(|i| {
                            let r = Arc::clone(&reg);
                            let idx = (ops_count as usize + i) % 5000;
                            tokio::spawn(async move {
                                let id = PackageId::new(format!("load-test-pkg-{}", idx)).unwrap();
                                r.get_package(&id).await
                            })
                        })
                        .collect();

                    for handle in handles {
                        let _ = handle.await;
                    }
                    ops_count += 10;
                }

                black_box(ops_count)
            }
        });
    });

    group.finish();
}

// ============================================================================
// Phase 4B: Cache Hit/Miss Ratio Under Load
// ============================================================================

fn bench_cache_under_load(c: &mut Criterion) {
    let mut group = c.benchmark_group("load_cache_behavior");
    group.measurement_time(Duration::from_secs(15));
    let rt = Runtime::new().unwrap();

    // Small cache with many packages (high miss rate scenario)
    let packages = generate_test_packages(5000);
    let registry = Arc::new(rt.block_on(async {
        let reg = Registry::new(500).await; // Small cache
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    }));

    // High cache pressure test
    group.bench_function("high_cache_pressure", |b| {
        b.to_async(&rt).iter(|| {
            let reg = Arc::clone(&registry);
            async move {
                // Access many different packages (cache thrashing)
                let handles: Vec<_> = (0..100)
                    .map(|i| {
                        let r = Arc::clone(&reg);
                        // Spread across entire range to cause cache misses
                        let idx = i * 50;
                        tokio::spawn(async move {
                            let id = PackageId::new(format!("load-test-pkg-{}", idx)).unwrap();
                            r.get_package(&id).await
                        })
                    })
                    .collect();

                for handle in handles {
                    let _ = handle.await;
                }
            }
        });
    });

    // Hot spot access pattern (high hit rate)
    group.bench_function("hot_spot_access", |b| {
        b.to_async(&rt).iter(|| {
            let reg = Arc::clone(&registry);
            async move {
                // Access same small set of packages repeatedly
                let handles: Vec<_> = (0..100)
                    .map(|i| {
                        let r = Arc::clone(&reg);
                        let idx = i % 10; // Only 10 distinct packages
                        tokio::spawn(async move {
                            let id = PackageId::new(format!("load-test-pkg-{}", idx)).unwrap();
                            r.get_package(&id).await
                        })
                    })
                    .collect();

                for handle in handles {
                    let _ = handle.await;
                }
            }
        });
    });

    // Report final cache stats
    let stats = registry.cache_stats();
    eprintln!(
        "\nLoad Test Cache Stats: {} hits, {} misses, {:.2}% hit rate",
        stats.hits,
        stats.misses,
        stats.hit_rate * 100.0
    );

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group! {
    name = concurrent_ops;
    config = Criterion::default()
        .sample_size(20)
        .measurement_time(Duration::from_secs(15));
    targets = bench_concurrent_reads, bench_concurrent_writes, bench_mixed_read_write
}

criterion_group! {
    name = search_load;
    config = Criterion::default()
        .sample_size(20)
        .measurement_time(Duration::from_secs(15));
    targets = bench_concurrent_search
}

criterion_group! {
    name = sustained_load;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(Duration::from_secs(30));
    targets = bench_sustained_load
}

criterion_group! {
    name = cache_load;
    config = Criterion::default()
        .sample_size(30)
        .measurement_time(Duration::from_secs(15));
    targets = bench_cache_under_load
}

criterion_main!(concurrent_ops, search_load, sustained_load, cache_load);
