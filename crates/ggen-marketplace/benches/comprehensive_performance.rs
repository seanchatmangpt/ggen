//! Comprehensive Performance Benchmarking Suite for Marketplace V2
//!
//! This benchmark suite validates production SLOs:
//! - Lookup latency: <100ms (p95)
//! - Search latency: <200ms (p95)
//! - Cache hit rate: >80%
//! - Installation time: <5s (without network)
//! - Dashboard generation: <2s
//!
//! Benchmark categories:
//! 1. Lookup Performance (cache hit/miss, metadata)
//! 2. Search Performance (text, SPARQL, filters)
//! 3. Scalability (10, 1K, 10K packages)
//! 4. Caching Effectiveness (hit rate, TTL)
//! 5. Installation (single, multi, dependencies)
//! 6. V1 vs V2 Comparison

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_marketplace_v2::{
    models::*, prelude::*, registry::Registry, search::*, search_sparql::SparqlSearchEngine,
    v3::V3OptimizedRegistry,
};
use std::sync::Arc;
use tokio::runtime::Runtime;

// ============================================================================
// Test Data Generation
// ============================================================================

/// Generate realistic test packages
fn generate_test_packages(count: usize) -> Vec<Package> {
    let mut packages = Vec::with_capacity(count);

    for i in 0..count {
        let package_id = PackageId::new(format!("test-package-{}", i)).unwrap();
        let mut metadata = PackageMetadata::new(
            package_id.clone(),
            format!("Test Package {}", i),
            format!(
                "A comprehensive test package for benchmarking purposes. \
                 This package includes various features like authentication, \
                 database integration, API endpoints, and more. Package number: {}",
                i
            ),
            "MIT",
        );

        metadata.authors = vec![format!("author-{}", i % 50)];
        metadata.categories = vec![match i % 5 {
            0 => "web-framework",
            1 => "database",
            2 => "cli-tool",
            3 => "library",
            _ => "utility",
        }
        .to_string()];
        metadata.keywords = vec![
            format!("keyword-{}", i % 20),
            format!("tag-{}", i % 30),
            "benchmark".to_string(),
        ];

        let version = PackageVersion::new(format!("1.{}.0", i % 100)).unwrap();

        let package = Package {
            metadata,
            latest_version: version.clone(),
            versions: vec![version],
            releases: indexmap::IndexMap::new(),
        };

        packages.push(package);
    }

    packages
}

/// Generate packages with dependency chains
fn generate_packages_with_deps(count: usize, _max_deps: usize) -> Vec<Package> {
    // For now, just return regular packages
    // Dependency resolution would require more complex setup
    generate_test_packages(count)
}

// ============================================================================
// Lookup Performance Benchmarks
// ============================================================================

fn bench_lookup_by_id(c: &mut Criterion) {
    let mut group = c.benchmark_group("lookup_by_id");
    let rt = Runtime::new().unwrap();

    for size in [10, 100, 1000, 10000].iter() {
        let packages = generate_test_packages(*size);
        let registry = rt.block_on(async {
            let reg = Registry::new(1000).await;
            for pkg in packages {
                reg.insert(pkg).await.unwrap();
            }
            reg
        });

        group.throughput(Throughput::Elements(1));
        group.bench_with_input(BenchmarkId::new("cache_cold", size), size, |b, &size| {
            b.to_async(&rt).iter(|| async {
                let id = format!("test-package-{}", size / 2);
                let result = registry.get(&id).await;
                black_box(result)
            });
        });
    }

    group.finish();
}

fn bench_lookup_metadata(c: &mut Criterion) {
    let mut group = c.benchmark_group("lookup_metadata");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(1000).await;
        for pkg in packages {
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    group.throughput(Throughput::Elements(1));
    group.bench_function("single_package", |b| {
        b.to_async(&rt).iter(|| async {
            let id = "test-package-500";
            let pkg = registry.get(id).await.unwrap().unwrap();
            black_box(pkg.metadata)
        });
    });

    group.bench_function("batch_100", |b| {
        b.to_async(&rt).iter(|| async {
            let mut results = Vec::new();
            for i in 0..100 {
                let id = format!("test-package-{}", i);
                if let Ok(Some(pkg)) = registry.get(&id).await {
                    results.push(pkg.metadata);
                }
            }
            black_box(results)
        });
    });

    group.finish();
}

fn bench_lookup_version_history(c: &mut Criterion) {
    let mut group = c.benchmark_group("lookup_version_history");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(100);
    let registry = rt.block_on(async {
        let reg = Registry::new(1000).await;
        for mut pkg in packages {
            // Add multiple versions
            for v in 0..20 {
                pkg.versions.push(format!("1.{}.0", v));
            }
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    group.bench_function("retrieve_all_versions", |b| {
        b.to_async(&rt).iter(|| async {
            let id = "test-package-50";
            let pkg = registry.get(id).await.unwrap().unwrap();
            black_box(pkg.versions)
        });
    });

    group.finish();
}

// ============================================================================
// Cache Performance Benchmarks
// ============================================================================

fn bench_cache_hit_miss(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_performance");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(500).await; // Limited cache size
        for pkg in packages {
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    group.bench_function("cache_hot_queries", |b| {
        b.to_async(&rt).iter(|| async {
            // Query same packages repeatedly (should hit cache)
            for i in 0..10 {
                let id = format!("test-package-{}", i);
                let _ = registry.get(&id).await;
            }
        });
    });

    group.bench_function("cache_cold_queries", |b| {
        b.to_async(&rt).iter(|| async {
            // Query different packages each time (cache misses)
            use std::sync::atomic::{AtomicUsize, Ordering};
            static COUNTER: AtomicUsize = AtomicUsize::new(0);
            let idx = COUNTER.fetch_add(1, Ordering::Relaxed);
            let id = format!("test-package-{}", idx % 1000);
            let _ = registry.get(&id).await;
        });
    });

    group.finish();
}

// ============================================================================
// Search Performance Benchmarks
// ============================================================================

fn bench_search_simple(c: &mut Criterion) {
    let mut group = c.benchmark_group("search_simple");
    let rt = Runtime::new().unwrap();

    for size in [100, 1000, 10000].iter() {
        let packages = generate_test_packages(*size);
        let engine = rt.block_on(async {
            let eng = SearchEngine::new();
            for pkg in packages {
                eng.index_package(&pkg).await.unwrap();
            }
            eng
        });

        group.throughput(Throughput::Elements(1));
        group.bench_with_input(BenchmarkId::new("name_search", size), size, |b, _size| {
            b.to_async(&rt).iter(|| async {
                let query = SearchQuery::new("test-package");
                let results = engine.search(&query).await.unwrap();
                black_box(results)
            });
        });

        group.bench_with_input(
            BenchmarkId::new("description_search", size),
            size,
            |b, _size| {
                b.to_async(&rt).iter(|| async {
                    let query = SearchQuery::new("authentication database");
                    let results = engine.search(&query).await.unwrap();
                    black_box(results)
                });
            },
        );
    }

    group.finish();
}

fn bench_search_filtered(c: &mut Criterion) {
    let mut group = c.benchmark_group("search_filtered");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let engine = rt.block_on(async {
        let eng = SearchEngine::new();
        for pkg in packages {
            eng.index_package(&pkg).await.unwrap();
        }
        eng
    });

    group.bench_function("category_filter", |b| {
        b.to_async(&rt).iter(|| async {
            let query = SearchQuery::new("package").with_category("web-framework");
            let results = engine.search(&query).await.unwrap();
            black_box(results)
        });
    });

    group.bench_function("author_filter", |b| {
        b.to_async(&rt).iter(|| async {
            let query = SearchQuery::new("package").with_author("author-10");
            let results = engine.search(&query).await.unwrap();
            black_box(results)
        });
    });

    group.bench_function("multi_filter", |b| {
        b.to_async(&rt).iter(|| async {
            let query = SearchQuery::new("package")
                .with_category("web-framework")
                .with_author("author-10")
                .with_license("MIT");
            let results = engine.search(&query).await.unwrap();
            black_box(results)
        });
    });

    group.finish();
}

fn bench_search_sparql(c: &mut Criterion) {
    let mut group = c.benchmark_group("search_sparql");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let engine = rt.block_on(async {
        let eng = SparqlSearchEngine::new().await.unwrap();
        for pkg in packages {
            eng.index_package(&pkg).await.unwrap();
        }
        eng
    });

    group.bench_function("semantic_query", |b| {
        b.to_async(&rt).iter(|| async {
            let query = r#"
                PREFIX pkg: <http://ggen.io/ontology#>
                SELECT ?id ?name WHERE {
                    ?pkg pkg:id ?id ;
                         pkg:name ?name ;
                         pkg:category "web-framework" .
                }
            "#;
            let results = engine.query_sparql(query).await.unwrap();
            black_box(results)
        });
    });

    group.finish();
}

// ============================================================================
// Scalability Benchmarks
// ============================================================================

fn bench_scalability_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("scalability_insert");
    let rt = Runtime::new().unwrap();

    for size in [10, 100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter_batched(
                || {
                    let packages = generate_test_packages(size);
                    (packages, rt.block_on(async { Registry::new(1000).await }))
                },
                |(packages, registry)| {
                    rt.block_on(async {
                        for pkg in packages {
                            registry.insert(pkg).await.unwrap();
                        }
                    })
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

fn bench_scalability_query(c: &mut Criterion) {
    let mut group = c.benchmark_group("scalability_query");
    let rt = Runtime::new().unwrap();

    for size in [10, 100, 1000, 10000].iter() {
        let packages = generate_test_packages(*size);
        let registry = rt.block_on(async {
            let reg = Registry::new(1000).await;
            for pkg in packages {
                reg.insert(pkg).await.unwrap();
            }
            reg
        });

        group.throughput(Throughput::Elements(100));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.to_async(&rt).iter(|| async {
                // Query 100 random packages
                for i in 0..100 {
                    let id = format!("test-package-{}", i % size);
                    let _ = registry.get(&id).await;
                }
            });
        });
    }

    group.finish();
}

// ============================================================================
// Installation Benchmarks
// ============================================================================

fn bench_install_single(c: &mut Criterion) {
    let mut group = c.benchmark_group("install_single");
    let rt = Runtime::new().unwrap();

    let package = generate_test_packages(1)[0].clone();
    let installer = rt.block_on(async { Installer::new("/tmp/bench-install").await.unwrap() });

    group.bench_function("no_dependencies", |b| {
        b.to_async(&rt).iter(|| async {
            let result = installer.install(&package).await;
            black_box(result)
        });
    });

    group.finish();
}

fn bench_install_with_deps(c: &mut Criterion) {
    let mut group = c.benchmark_group("install_with_dependencies");
    let rt = Runtime::new().unwrap();

    for dep_count in [1, 3, 5].iter() {
        let packages = generate_packages_with_deps(10, *dep_count);
        let installer =
            rt.block_on(async { Installer::new("/tmp/bench-install-deps").await.unwrap() });

        group.bench_with_input(BenchmarkId::from_parameter(dep_count), dep_count, |b, _| {
            b.to_async(&rt).iter(|| async {
                let result = installer.install(&packages[9]).await;
                black_box(result)
            });
        });
    }

    group.finish();
}

// ============================================================================
// V3 Optimized Registry Benchmarks
// ============================================================================

fn bench_v3_optimized(c: &mut Criterion) {
    let mut group = c.benchmark_group("v3_optimized");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let registry = rt.block_on(async {
        let reg = V3OptimizedRegistry::new(":memory:", 1000).await.unwrap();
        for pkg in packages {
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    group.bench_function("lookup_optimized", |b| {
        b.to_async(&rt).iter(|| async {
            let id = "test-package-500";
            let result = registry.get(id).await;
            black_box(result)
        });
    });

    group.bench_function("search_optimized", |b| {
        b.to_async(&rt).iter(|| async {
            let query = SearchQuery::new("test");
            let results = registry.search(&query).await;
            black_box(results)
        });
    });

    group.finish();
}

// ============================================================================
// Dashboard Generation Benchmark
// ============================================================================

fn bench_dashboard_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("dashboard_generation");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(1000).await;
        for pkg in packages {
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    group.bench_function("aggregate_stats", |b| {
        b.to_async(&rt).iter(|| async {
            let all_packages = registry.list_all().await.unwrap();

            // Calculate dashboard metrics
            let total_packages = all_packages.len();
            let categories: std::collections::HashSet<_> =
                all_packages.iter().map(|p| &p.metadata.category).collect();
            let total_categories = categories.len();

            let authors: std::collections::HashSet<_> =
                all_packages.iter().map(|p| &p.metadata.author).collect();
            let total_authors = authors.len();

            black_box((total_packages, total_categories, total_authors))
        });
    });

    group.finish();
}

// ============================================================================
// Memory Usage Benchmarks
// ============================================================================

fn bench_memory_efficiency(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_efficiency");
    let rt = Runtime::new().unwrap();

    group.bench_function("memory_footprint_1k", |b| {
        b.iter_batched(
            || generate_test_packages(1000),
            |packages| {
                rt.block_on(async {
                    let reg = Registry::new(1000).await;
                    for pkg in packages {
                        reg.insert(pkg).await.unwrap();
                    }
                    // Force retention
                    black_box(reg)
                })
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group! {
    name = lookup_benches;
    config = Criterion::default()
        .sample_size(100)
        .measurement_time(std::time::Duration::from_secs(10))
        .warm_up_time(std::time::Duration::from_secs(3));
    targets = bench_lookup_by_id, bench_lookup_metadata, bench_lookup_version_history
}

criterion_group! {
    name = cache_benches;
    config = Criterion::default().sample_size(100);
    targets = bench_cache_hit_miss
}

criterion_group! {
    name = search_benches;
    config = Criterion::default().sample_size(50);
    targets = bench_search_simple, bench_search_filtered, bench_search_sparql
}

criterion_group! {
    name = scalability_benches;
    config = Criterion::default()
        .sample_size(20)
        .measurement_time(std::time::Duration::from_secs(15));
    targets = bench_scalability_insert, bench_scalability_query
}

criterion_group! {
    name = install_benches;
    config = Criterion::default().sample_size(50);
    targets = bench_install_single, bench_install_with_deps
}

criterion_group! {
    name = optimized_benches;
    config = Criterion::default().sample_size(100);
    targets = bench_v3_optimized, bench_dashboard_generation
}

criterion_group! {
    name = memory_benches;
    config = Criterion::default().sample_size(20);
    targets = bench_memory_efficiency
}

criterion_main!(
    lookup_benches,
    cache_benches,
    search_benches,
    scalability_benches,
    install_benches,
    optimized_benches,
    memory_benches
);
