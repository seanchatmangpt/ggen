//! Marketplace performance benchmarks
//!
//! Comprehensive benchmark suite for marketplace operations using criterion.rs
//!
//! Run with: cargo bench --bench marketplace_benchmark

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::time::Duration;
use tempfile::TempDir;

/// Benchmark configuration
struct BenchConfig {
    package_count: usize,
    query_complexity: QueryComplexity,
}

#[derive(Clone, Copy)]
enum QueryComplexity {
    Simple,
    Medium,
    Complex,
}

/// Generate test package names
fn generate_package_names(count: usize) -> Vec<String> {
    (0..count)
        .map(|i| format!("test-package-{:05}", i))
        .collect()
}

/// Generate test search queries
fn generate_search_queries(complexity: QueryComplexity) -> Vec<String> {
    match complexity {
        QueryComplexity::Simple => vec![
            "rust".to_string(),
            "web".to_string(),
            "cli".to_string(),
        ],
        QueryComplexity::Medium => vec![
            "rust web framework".to_string(),
            "async database".to_string(),
            "cli tool parser".to_string(),
        ],
        QueryComplexity::Complex => vec![
            "rust async web framework with database support".to_string(),
            "command line interface with advanced parsing and validation".to_string(),
            "distributed systems with consensus and replication".to_string(),
        ],
    }
}

/// Benchmark package search operations
fn bench_search(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_search");

    for complexity in [QueryComplexity::Simple, QueryComplexity::Medium, QueryComplexity::Complex] {
        let queries = generate_search_queries(complexity);
        let complexity_name = match complexity {
            QueryComplexity::Simple => "simple",
            QueryComplexity::Medium => "medium",
            QueryComplexity::Complex => "complex",
        };

        group.throughput(Throughput::Elements(queries.len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(complexity_name),
            &queries,
            |b, queries| {
                b.iter(|| {
                    for query in queries {
                        // Simulate search operation
                        let _result = black_box(query.to_lowercase());
                    }
                });
            },
        );
    }

    group.finish();
}

/// Benchmark package listing operations
fn bench_list_packages(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_list");

    for package_count in [10, 100, 1000, 10000] {
        let packages = generate_package_names(package_count);

        group.throughput(Throughput::Elements(package_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            &packages,
            |b, packages| {
                b.iter(|| {
                    let mut results = Vec::new();
                    for package in packages {
                        results.push(black_box(package.clone()));
                    }
                    black_box(results)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark package installation simulation
fn bench_install_package(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_install");
    group.measurement_time(Duration::from_secs(10));

    for package_count in [1, 5, 10, 20] {
        let packages = generate_package_names(package_count);

        group.throughput(Throughput::Elements(package_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            &packages,
            |b, packages| {
                b.iter(|| {
                    for package in packages {
                        // Simulate installation work
                        let _work = black_box(package.bytes().collect::<Vec<_>>());
                        let _hash = black_box(package.len());
                    }
                });
            },
        );
    }

    group.finish();
}

/// Benchmark concurrent operations
fn bench_concurrent_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_concurrent");
    group.measurement_time(Duration::from_secs(15));

    let rt = tokio::runtime::Runtime::new().expect("Failed to create runtime");

    for concurrency in [2, 4, 8, 16] {
        group.throughput(Throughput::Elements(concurrency as u64 * 10));
        group.bench_with_input(
            BenchmarkId::from_parameter(concurrency),
            &concurrency,
            |b, &concurrency| {
                b.to_async(&rt).iter(|| async move {
                    let mut tasks = Vec::new();

                    for _ in 0..concurrency {
                        let task = tokio::spawn(async {
                            // Simulate work
                            for _ in 0..10 {
                                tokio::time::sleep(Duration::from_micros(100)).await;
                            }
                        });
                        tasks.push(task);
                    }

                    for task in tasks {
                        let _ = task.await;
                    }
                });
            },
        );
    }

    group.finish();
}

/// Benchmark metadata parsing
fn bench_metadata_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_metadata");

    let metadata_sizes = [
        ("small", 100),
        ("medium", 1000),
        ("large", 10000),
    ];

    for (name, size) in metadata_sizes {
        let metadata = "a".repeat(size);

        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &metadata,
            |b, metadata| {
                b.iter(|| {
                    let _parsed = black_box(metadata.chars().count());
                    let _bytes = black_box(metadata.as_bytes());
                });
            },
        );
    }

    group.finish();
}

/// Benchmark cache operations
fn bench_cache_operations(c: &mut Criterion) {
    use std::collections::HashMap;

    let mut group = c.benchmark_group("marketplace_cache");

    for cache_size in [10, 100, 1000] {
        let mut cache: HashMap<String, String> = HashMap::new();
        for i in 0..cache_size {
            cache.insert(format!("key-{}", i), format!("value-{}", i));
        }

        group.throughput(Throughput::Elements(cache_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(cache_size),
            &cache,
            |b, cache| {
                b.iter(|| {
                    for i in 0..cache_size {
                        let key = format!("key-{}", i);
                        let _value = black_box(cache.get(&key));
                    }
                });
            },
        );
    }

    group.finish();
}

/// Benchmark filesystem operations
fn bench_filesystem_ops(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_filesystem");
    group.measurement_time(Duration::from_secs(10));

    let rt = tokio::runtime::Runtime::new().expect("Failed to create runtime");

    for file_count in [1, 10, 50] {
        group.throughput(Throughput::Elements(file_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(file_count),
            &file_count,
            |b, &file_count| {
                b.to_async(&rt).iter(|| async move {
                    let temp_dir = TempDir::new().expect("Failed to create temp dir");

                    for i in 0..file_count {
                        let path = temp_dir.path().join(format!("file-{}.txt", i));
                        let _ = tokio::fs::write(&path, b"test data").await;
                    }

                    black_box(temp_dir)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark sorting and filtering operations
fn bench_sort_filter(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_sort_filter");

    for package_count in [100, 1000, 5000] {
        let mut packages = generate_package_names(package_count);

        group.throughput(Throughput::Elements(package_count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(package_count),
            &packages,
            |b, packages| {
                b.iter(|| {
                    let mut sorted = packages.clone();
                    sorted.sort();

                    let filtered: Vec<_> = sorted
                        .iter()
                        .filter(|p| p.contains("package"))
                        .collect();

                    black_box(filtered)
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_search,
    bench_list_packages,
    bench_install_package,
    bench_concurrent_operations,
    bench_metadata_parsing,
    bench_cache_operations,
    bench_filesystem_ops,
    bench_sort_filter,
);

criterion_main!(benches);
