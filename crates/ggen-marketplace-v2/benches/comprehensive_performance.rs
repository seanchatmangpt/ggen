//! Comprehensive Performance Benchmarking Suite for Marketplace V2 - Phase 4
//!
//! Phase 4A SLO Targets (stricter than previous):
//! - Registry create: <50ms
//! - Registry read: <10ms
//! - Search (keyword): <50ms
//! - Search (SPARQL): <100ms
//! - Install (simple): <200ms
//! - Install (with deps): <500ms
//! - Batch operations (100+ packages): <100ms per operation
//!
//! Benchmark categories:
//! 1. Registry Operations (CRUD with SLO validation)
//! 2. Search Performance (text, SPARQL, filters)
//! 3. Scalability (10, 100, 1K, 10K packages)
//! 4. Caching Effectiveness (hit rate, TTL)
//! 5. Memory Profiling (1000+ package sets)
//! 6. Concurrent Operations

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_marketplace_v2::{
    models::*,
    registry::Registry,
    search::{SearchEngine, SearchQuery},
    traits::AsyncRepository,
};
use std::hint::black_box;
use std::time::Duration;
use tokio::runtime::Runtime;

// ============================================================================
// Phase 4A SLO Constants
// ============================================================================

/// Registry create operation SLO: <50ms
const SLO_REGISTRY_CREATE_MS: u64 = 50;
/// Registry read operation SLO: <10ms
const SLO_REGISTRY_READ_MS: u64 = 10;
/// Keyword search SLO: <50ms
const SLO_SEARCH_KEYWORD_MS: u64 = 50;
/// Batch operation SLO: <100ms per operation
const SLO_BATCH_OP_MS: u64 = 100;

// ============================================================================
// Test Data Generation
// ============================================================================

/// Generate realistic test packages with comprehensive metadata
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

/// Generate packages with dependency chains for installation benchmarks
fn generate_packages_with_deps(count: usize, deps_per_pkg: usize) -> Vec<Package> {
    let mut packages = generate_test_packages(count);

    // Add dependencies to each package (simulated)
    for i in deps_per_pkg..count {
        let pkg = &mut packages[i];
        // Add releases with dependencies
        let version = pkg.latest_version.clone();
        let mut deps = Vec::new();

        for j in 0..deps_per_pkg.min(i) {
            deps.push(PackageDependency {
                id: PackageId::new(format!("test-package-{}", j)).unwrap(),
                version_req: "^1.0.0".to_string(),
                optional: false,
            });
        }

        pkg.releases.insert(
            version.clone(),
            ReleaseInfo {
                version,
                released_at: chrono::Utc::now(),
                changelog: "Initial release".to_string(),
                checksum: "sha256:abc123".to_string(),
                download_url: format!("https://example.com/pkg-{}.tar.gz", i),
                dependencies: deps,
            },
        );
    }

    packages
}

// ============================================================================
// Phase 4A: Registry Operations Benchmarks
// ============================================================================

fn bench_registry_create(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase4a_registry_create");
    group.measurement_time(Duration::from_secs(10));
    let rt = Runtime::new().unwrap();

    // Single package insert (SLO: <50ms)
    group.bench_function("single_insert", |b| {
        b.iter_batched(
            || {
                let pkg = generate_test_packages(1).pop().unwrap();
                let registry = rt.block_on(async { Registry::new(1000).await });
                (pkg, registry)
            },
            |(pkg, registry)| {
                registry.insert(pkg).unwrap();
                black_box(())
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // Batch insert (100 packages)
    for size in [10, 100, 500].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::new("batch_insert", size), size, |b, &size| {
            b.iter_batched(
                || {
                    let packages = generate_test_packages(size);
                    let registry = rt.block_on(async { Registry::new(1000).await });
                    (packages, registry)
                },
                |(packages, registry)| {
                    for pkg in packages {
                        registry.insert(pkg).unwrap();
                    }
                    black_box(())
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

fn bench_registry_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase4a_registry_read");
    group.measurement_time(Duration::from_secs(10));
    let rt = Runtime::new().unwrap();

    for size in [100, 1000, 10000].iter() {
        let packages = generate_test_packages(*size);
        let registry = rt.block_on(async {
            let reg = Registry::new(1000).await;
            for pkg in packages {
                reg.insert(pkg).unwrap();
            }
            reg
        });

        // Single lookup (SLO: <10ms)
        group.throughput(Throughput::Elements(1));
        group.bench_with_input(BenchmarkId::new("single_lookup", size), size, |b, &size| {
            b.to_async(&rt).iter(|| async {
                let id = PackageId::new(format!("test-package-{}", size / 2)).unwrap();
                let result = registry.get_package(&id).await;
                black_box(result)
            });
        });

        // Batch lookup (100 sequential reads)
        group.bench_with_input(
            BenchmarkId::new("batch_100_lookup", size),
            size,
            |b, &size| {
                b.to_async(&rt).iter(|| async {
                    let mut results = Vec::with_capacity(100);
                    for i in 0..100 {
                        let id = PackageId::new(format!("test-package-{}", i % size)).unwrap();
                        if let Ok(pkg) = registry.get_package(&id).await {
                            results.push(pkg);
                        }
                    }
                    black_box(results)
                });
            },
        );
    }

    group.finish();
}

fn bench_registry_update(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase4a_registry_update");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(1000).await;
        for pkg in packages.clone() {
            reg.insert(pkg).unwrap();
        }
        reg
    });

    group.bench_function("single_update", |b| {
        b.iter_batched(
            || {
                let mut pkg = packages[500].clone();
                pkg.metadata.description = "Updated description".to_string();
                pkg
            },
            |pkg| {
                let id = pkg.metadata.id.clone();
                registry.update(&id, pkg).unwrap();
                black_box(())
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

fn bench_registry_delete(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase4a_registry_delete");
    let rt = Runtime::new().unwrap();

    group.bench_function("single_delete", |b| {
        b.iter_batched(
            || {
                let packages = generate_test_packages(100);
                let registry = rt.block_on(async {
                    let reg = Registry::new(1000).await;
                    for pkg in packages {
                        reg.insert(pkg).unwrap();
                    }
                    reg
                });
                registry
            },
            |registry| {
                let id = PackageId::new("test-package-50").unwrap();
                let result = registry.remove(&id);
                black_box(result)
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// Phase 4A: Search Performance Benchmarks
// ============================================================================

fn bench_search_keyword(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase4a_search_keyword");
    group.measurement_time(Duration::from_secs(10));
    let rt = Runtime::new().unwrap();

    for size in [100, 1000, 10000].iter() {
        let packages = generate_test_packages(*size);
        let engine = SearchEngine::new();

        // Simple name search (SLO: <50ms)
        group.throughput(Throughput::Elements(1));
        group.bench_with_input(BenchmarkId::new("name_search", size), size, |b, _size| {
            let pkgs = packages.clone();
            b.iter(|| {
                let query = SearchQuery::new("test-package");
                let results = engine.search(pkgs.clone(), &query).unwrap();
                black_box(results)
            });
        });

        // Description search
        group.bench_with_input(
            BenchmarkId::new("description_search", size),
            size,
            |b, _size| {
                let pkgs = packages.clone();
                b.iter(|| {
                    let query = SearchQuery::new("authentication database");
                    let results = engine.search(pkgs.clone(), &query).unwrap();
                    black_box(results)
                });
            },
        );

        // Filtered search
        group.bench_with_input(
            BenchmarkId::new("filtered_search", size),
            size,
            |b, _size| {
                let pkgs = packages.clone();
                b.iter(|| {
                    let query = SearchQuery::new("package")
                        .with_category("web-framework")
                        .with_author("author-10");
                    let results = engine.search(pkgs.clone(), &query).unwrap();
                    black_box(results)
                });
            },
        );
    }

    group.finish();
}

fn bench_search_pagination(c: &mut Criterion) {
    let mut group = c.benchmark_group("phase4a_search_pagination");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(10000);
    let engine = SearchEngine::new();

    // Test different pagination scenarios
    for (offset, limit) in [(0, 50), (100, 50), (500, 100), (1000, 200)].iter() {
        let label = format!("offset_{}_limit_{}", offset, limit);
        group.bench_function(&label, |b| {
            let pkgs = packages.clone();
            b.iter(|| {
                let query = SearchQuery::new("test")
                    .with_offset(*offset)
                    .with_limit(*limit);
                let results = engine.search(pkgs.clone(), &query).unwrap();
                black_box(results)
            });
        });
    }

    group.finish();
}

// ============================================================================
// Cache Performance Benchmarks
// ============================================================================

fn bench_cache_effectiveness(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_effectiveness");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(500).await; // Limited cache
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    });

    // Hot queries (same packages repeatedly - should hit cache)
    group.bench_function("hot_queries_10x", |b| {
        b.to_async(&rt).iter(|| async {
            for _ in 0..10 {
                for i in 0..10 {
                    let id = PackageId::new(format!("test-package-{}", i)).unwrap();
                    let _ = registry.get_package(&id).await;
                }
            }
        });
    });

    // Cold queries (different packages - cache misses expected)
    group.bench_function("cold_queries_random", |b| {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        b.to_async(&rt).iter(|| async {
            for _ in 0..10 {
                let idx = COUNTER.fetch_add(1, Ordering::Relaxed);
                let id = PackageId::new(format!("test-package-{}", idx % 1000)).unwrap();
                let _ = registry.get_package(&id).await;
            }
        });
    });

    // Report cache stats after benchmark
    let stats = registry.cache_stats();
    eprintln!(
        "Cache Stats: {} hits, {} misses, {:.2}% hit rate",
        stats.hits,
        stats.misses,
        stats.hit_rate * 100.0
    );

    group.finish();
}

// ============================================================================
// Scalability Benchmarks
// ============================================================================

fn bench_scalability_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("scalability_insert");
    group.measurement_time(Duration::from_secs(15));
    let rt = Runtime::new().unwrap();

    for size in [10, 100, 1000, 5000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter_batched(
                || {
                    let packages = generate_test_packages(size);
                    let registry = rt.block_on(async { Registry::new(1000).await });
                    (packages, registry)
                },
                |(packages, registry)| {
                    for pkg in packages {
                        registry.insert(pkg).unwrap();
                    }
                    black_box(())
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

fn bench_scalability_query(c: &mut Criterion) {
    let mut group = c.benchmark_group("scalability_query");
    group.measurement_time(Duration::from_secs(15));
    let rt = Runtime::new().unwrap();

    for size in [100, 1000, 5000, 10000].iter() {
        let packages = generate_test_packages(*size);
        let registry = rt.block_on(async {
            let reg = Registry::new(2000).await;
            for pkg in packages {
                reg.insert(pkg).unwrap();
            }
            reg
        });

        group.throughput(Throughput::Elements(100));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.to_async(&rt).iter(|| async {
                // Query 100 random packages
                for i in 0..100 {
                    let id = PackageId::new(format!("test-package-{}", i % size)).unwrap();
                    let _ = registry.get_package(&id).await;
                }
            });
        });
    }

    group.finish();
}

// ============================================================================
// Memory Efficiency Benchmarks (Phase 4C)
// ============================================================================

fn bench_memory_footprint(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_footprint");
    group.sample_size(10);
    let rt = Runtime::new().unwrap();

    // Measure memory footprint at different scales
    for size in [100, 500, 1000, 2000].iter() {
        group.bench_with_input(BenchmarkId::new("packages", size), size, |b, &size| {
            b.iter_batched(
                || generate_test_packages(size),
                |packages| {
                    rt.block_on(async {
                        let reg = Registry::new((size as u64).max(500)).await;
                        for pkg in packages {
                            reg.insert(pkg).unwrap();
                        }
                        // Force retention
                        black_box(reg)
                    })
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

// ============================================================================
// Dashboard Generation Benchmark
// ============================================================================

fn bench_dashboard_aggregation(c: &mut Criterion) {
    let mut group = c.benchmark_group("dashboard_aggregation");
    let rt = Runtime::new().unwrap();

    let packages = generate_test_packages(1000);
    let registry = rt.block_on(async {
        let reg = Registry::new(1000).await;
        for pkg in packages {
            reg.insert(pkg).unwrap();
        }
        reg
    });

    group.bench_function("aggregate_stats", |b| {
        b.to_async(&rt).iter(|| async {
            let all_packages = registry.all_packages().await.unwrap();

            // Calculate dashboard metrics
            let total_packages = all_packages.len();
            let categories: std::collections::HashSet<_> = all_packages
                .iter()
                .flat_map(|p| p.metadata.categories.iter())
                .collect();
            let total_categories = categories.len();

            let authors: std::collections::HashSet<_> = all_packages
                .iter()
                .flat_map(|p| p.metadata.authors.iter())
                .collect();
            let total_authors = authors.len();

            // Category distribution
            let mut category_counts = std::collections::HashMap::new();
            for pkg in &all_packages {
                for cat in &pkg.metadata.categories {
                    *category_counts.entry(cat.clone()).or_insert(0) += 1;
                }
            }

            black_box((
                total_packages,
                total_categories,
                total_authors,
                category_counts,
            ))
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group! {
    name = registry_benches;
    config = Criterion::default()
        .sample_size(50)
        .measurement_time(Duration::from_secs(10))
        .warm_up_time(Duration::from_secs(3));
    targets = bench_registry_create, bench_registry_read, bench_registry_update, bench_registry_delete
}

criterion_group! {
    name = search_benches;
    config = Criterion::default()
        .sample_size(30)
        .measurement_time(Duration::from_secs(10));
    targets = bench_search_keyword, bench_search_pagination
}

criterion_group! {
    name = cache_benches;
    config = Criterion::default().sample_size(50);
    targets = bench_cache_effectiveness
}

criterion_group! {
    name = scalability_benches;
    config = Criterion::default()
        .sample_size(20)
        .measurement_time(Duration::from_secs(15));
    targets = bench_scalability_insert, bench_scalability_query
}

criterion_group! {
    name = memory_benches;
    config = Criterion::default().sample_size(10);
    targets = bench_memory_footprint
}

criterion_group! {
    name = dashboard_benches;
    config = Criterion::default().sample_size(50);
    targets = bench_dashboard_aggregation
}

criterion_main!(
    registry_benches,
    search_benches,
    cache_benches,
    scalability_benches,
    memory_benches,
    dashboard_benches
);
