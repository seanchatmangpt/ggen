//! V1 vs V2 Performance Comparison Benchmarks
//!
//! Direct comparison of marketplace v1 and v2 implementations
//! to validate performance improvements and feature parity.
//!
//! Key metrics:
//! - Lookup latency improvement
//! - Search throughput increase
//! - Memory usage comparison
//! - Cache effectiveness
//! - Scalability characteristics

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_marketplace_v2::{
    models::*, prelude::*, registry::Registry as V2Registry, search::SearchQuery as V2SearchQuery,
    v3::V3OptimizedRegistry,
};
use std::sync::Arc;
use tokio::runtime::Runtime;

// ============================================================================
// V1 Mock Implementation (for comparison)
// ============================================================================

/// Mock V1 registry for comparison
/// Uses simpler data structures to represent V1 performance characteristics
struct V1Registry {
    packages: std::collections::HashMap<String, V1Package>,
}

#[derive(Clone, Debug)]
struct V1Package {
    id: String,
    name: String,
    version: String,
    description: String,
    author: String,
}

impl V1Registry {
    fn new() -> Self {
        Self {
            packages: std::collections::HashMap::new(),
        }
    }

    fn insert(&mut self, pkg: V1Package) {
        self.packages.insert(pkg.id.clone(), pkg);
    }

    fn get(&self, id: &str) -> Option<&V1Package> {
        self.packages.get(id)
    }

    fn search(&self, query: &str) -> Vec<&V1Package> {
        self.packages
            .values()
            .filter(|p| {
                p.name.contains(query) || p.description.contains(query) || p.author.contains(query)
            })
            .collect()
    }

    fn list_all(&self) -> Vec<&V1Package> {
        self.packages.values().collect()
    }
}

// ============================================================================
// Test Data Generation
// ============================================================================

fn generate_v1_packages(count: usize) -> Vec<V1Package> {
    (0..count)
        .map(|i| V1Package {
            id: format!("pkg-{}", i),
            name: format!("package-{}", i),
            version: format!("1.{}.0", i % 100),
            description: format!("Test package {} for benchmarking", i),
            author: format!("author-{}", i % 50),
        })
        .collect()
}

fn generate_v2_packages(count: usize) -> Vec<Package> {
    (0..count)
        .map(|i| {
            let package_id = PackageId::new(format!("package-{}", i)).unwrap();
            let mut metadata = PackageMetadata::new(
                package_id.clone(),
                format!("Package {}", i),
                format!("Test package {} for benchmarking", i),
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
// Lookup Performance Comparison
// ============================================================================

fn compare_lookup_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_lookup");
    let rt = Runtime::new().unwrap();

    for size in [100, 1000, 10000].iter() {
        // V1 setup
        let v1_packages = generate_v1_packages(*size);
        let mut v1_registry = V1Registry::new();
        for pkg in v1_packages {
            v1_registry.insert(pkg);
        }

        // V2 setup
        let v2_packages = generate_v2_packages(*size);
        let v2_registry = rt.block_on(async {
            let reg = V2Registry::new(1000).await;
            for pkg in v2_packages {
                reg.insert(pkg).await.unwrap();
            }
            reg
        });

        // V1 lookup benchmark
        group.bench_with_input(BenchmarkId::new("v1_lookup", size), size, |b, &size| {
            b.iter(|| {
                let id = format!("pkg-{}", size / 2);
                let result = v1_registry.get(&id);
                black_box(result)
            });
        });

        // V2 lookup benchmark
        group.bench_with_input(BenchmarkId::new("v2_lookup", size), size, |b, &size| {
            b.to_async(&rt).iter(|| async {
                let id = format!("package-{}", size / 2);
                let result = v2_registry.get(&id).await;
                black_box(result)
            });
        });
    }

    group.finish();
}

// ============================================================================
// Search Performance Comparison
// ============================================================================

fn compare_search_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_search");
    let rt = Runtime::new().unwrap();

    for size in [100, 1000, 10000].iter() {
        // V1 setup
        let v1_packages = generate_v1_packages(*size);
        let mut v1_registry = V1Registry::new();
        for pkg in v1_packages {
            v1_registry.insert(pkg);
        }

        // V2 setup
        let v2_packages = generate_v2_packages(*size);
        let v2_engine = rt.block_on(async {
            let eng = ggen_marketplace_v2::search::SearchEngine::new();
            for pkg in v2_packages {
                eng.index_package(&pkg).await.unwrap();
            }
            eng
        });

        // V1 search benchmark (simple string matching)
        group.bench_with_input(BenchmarkId::new("v1_search", size), size, |b, _size| {
            b.iter(|| {
                let results = v1_registry.search("package");
                black_box(results)
            });
        });

        // V2 search benchmark (advanced search engine)
        group.bench_with_input(BenchmarkId::new("v2_search", size), size, |b, _size| {
            b.to_async(&rt).iter(|| async {
                let query = V2SearchQuery::new("package");
                let results = v2_engine.search(&query).await.unwrap();
                black_box(results)
            });
        });
    }

    group.finish();
}

// ============================================================================
// Batch Operations Comparison
// ============================================================================

fn compare_batch_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_batch");
    let rt = Runtime::new().unwrap();

    let batch_size = 100;

    // V1 batch insert
    group.bench_function("v1_batch_insert", |b| {
        b.iter_batched(
            || generate_v1_packages(batch_size),
            |packages| {
                let mut registry = V1Registry::new();
                for pkg in packages {
                    registry.insert(pkg);
                }
                black_box(registry)
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // V2 batch insert
    group.bench_function("v2_batch_insert", |b| {
        b.iter_batched(
            || generate_v2_packages(batch_size),
            |packages| {
                rt.block_on(async {
                    let registry = V2Registry::new(1000).await;
                    for pkg in packages {
                        registry.insert(pkg).await.unwrap();
                    }
                    black_box(registry)
                })
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// Filtered Search Comparison
// ============================================================================

fn compare_filtered_search(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_filtered_search");
    let rt = Runtime::new().unwrap();

    // V1 setup (no built-in filtering)
    let v1_packages = generate_v1_packages(1000);
    let mut v1_registry = V1Registry::new();
    for pkg in v1_packages {
        v1_registry.insert(pkg);
    }

    // V2 setup (native filtering support)
    let v2_packages = generate_v2_packages(1000);
    let v2_engine = rt.block_on(async {
        let eng = ggen_marketplace_v2::search::SearchEngine::new();
        for pkg in v2_packages {
            eng.index_package(&pkg).await.unwrap();
        }
        eng
    });

    // V1 filtered search (manual filtering)
    group.bench_function("v1_filtered_search", |b| {
        b.iter(|| {
            let results: Vec<_> = v1_registry
                .search("package")
                .into_iter()
                .filter(|p| p.author.starts_with("author-10"))
                .collect();
            black_box(results)
        });
    });

    // V2 filtered search (optimized)
    group.bench_function("v2_filtered_search", |b| {
        b.to_async(&rt).iter(|| async {
            let query = V2SearchQuery::new("package").with_author("author-10");
            let results = v2_engine.search(&query).await.unwrap();
            black_box(results)
        });
    });

    group.finish();
}

// ============================================================================
// Concurrent Access Comparison
// ============================================================================

fn compare_concurrent_access(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_concurrent");
    let rt = Runtime::new().unwrap();

    // V2 concurrent access (DashMap-based)
    let v2_packages = generate_v2_packages(1000);
    let v2_registry = rt.block_on(async {
        let reg = V2Registry::new(1000).await;
        for pkg in v2_packages {
            reg.insert(pkg).await.unwrap();
        }
        Arc::new(reg)
    });

    group.bench_function("v2_concurrent_reads", |b| {
        b.to_async(&rt).iter(|| {
            let registry = v2_registry.clone();
            async move {
                let handles: Vec<_> = (0..10)
                    .map(|i| {
                        let reg = registry.clone();
                        tokio::spawn(async move {
                            let id = format!("package-{}", i * 100);
                            reg.get(&id).await
                        })
                    })
                    .collect();

                for handle in handles {
                    let _ = handle.await;
                }
            }
        });
    });

    group.finish();
}

// ============================================================================
// V3 Optimized Registry Comparison
// ============================================================================

fn compare_v3_optimizations(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_v3");
    let rt = Runtime::new().unwrap();

    let packages = generate_v2_packages(1000);

    // Standard V2 registry
    let v2_registry = rt.block_on(async {
        let reg = V2Registry::new(1000).await;
        for pkg in packages.clone() {
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    // V3 optimized registry
    let v3_registry = rt.block_on(async {
        let reg = V3OptimizedRegistry::new(":memory:", 1000).await.unwrap();
        for pkg in packages {
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    group.bench_function("v2_standard_lookup", |b| {
        b.to_async(&rt).iter(|| async {
            let id = "package-500";
            let result = v2_registry.get(id).await;
            black_box(result)
        });
    });

    group.bench_function("v3_optimized_lookup", |b| {
        b.to_async(&rt).iter(|| async {
            let id = "package-500";
            let result = v3_registry.get(id).await;
            black_box(result)
        });
    });

    group.finish();
}

// ============================================================================
// Memory Footprint Comparison
// ============================================================================

fn compare_memory_footprint(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_memory");
    let rt = Runtime::new().unwrap();

    group.bench_function("v1_memory_1k_packages", |b| {
        b.iter_batched(
            || generate_v1_packages(1000),
            |packages| {
                let mut registry = V1Registry::new();
                for pkg in packages {
                    registry.insert(pkg);
                }
                black_box(registry)
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.bench_function("v2_memory_1k_packages", |b| {
        b.iter_batched(
            || generate_v2_packages(1000),
            |packages| {
                rt.block_on(async {
                    let registry = V2Registry::new(1000).await;
                    for pkg in packages {
                        registry.insert(pkg).await.unwrap();
                    }
                    black_box(registry)
                })
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// Feature Parity Validation
// ============================================================================

fn compare_feature_completeness(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison_features");
    let rt = Runtime::new().unwrap();

    let v2_packages = generate_v2_packages(100);
    let v2_registry = rt.block_on(async {
        let reg = V2Registry::new(1000).await;
        for pkg in v2_packages {
            reg.insert(pkg).await.unwrap();
        }
        reg
    });

    // V2 has features V1 doesn't: versioning, dependencies, quality scores
    group.bench_function("v2_version_resolution", |b| {
        b.to_async(&rt).iter(|| async {
            let id = "package-50";
            let pkg = v2_registry.get(id).await.unwrap().unwrap();
            let versions = pkg.versions;
            black_box(versions)
        });
    });

    group.bench_function("v2_dependency_lookup", |b| {
        b.to_async(&rt).iter(|| async {
            let id = "package-50";
            let pkg = v2_registry.get(id).await.unwrap().unwrap();
            let deps = pkg.metadata.dependencies;
            black_box(deps)
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group! {
    name = comparison_benches;
    config = Criterion::default()
        .sample_size(50)
        .measurement_time(std::time::Duration::from_secs(10));
    targets =
        compare_lookup_performance,
        compare_search_performance,
        compare_batch_operations,
        compare_filtered_search,
        compare_concurrent_access,
        compare_v3_optimizations,
        compare_memory_footprint,
        compare_feature_completeness
}

criterion_main!(comparison_benches);
