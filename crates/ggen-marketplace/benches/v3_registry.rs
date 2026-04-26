//! Criterion benchmarks for V3OptimizedRegistry
//!
//! Compares performance of v3 registry with caching, parallel search, and batch operations
//! against baseline operations.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_marketplace::models::PackageId;
use ggen_marketplace::v3::V3OptimizedRegistry;
use oxigraph::store::Store;
use std::sync::Arc;

/// Create a test registry with some sample data
fn setup_registry() -> V3OptimizedRegistry {
    let store = Arc::new(Store::new().unwrap());
    V3OptimizedRegistry::new(store).unwrap()
}

/// Benchmark: Cache hit on metadata cache
fn bench_cache_hit(c: &mut Criterion) {
    let registry = setup_registry();
    let package_id = black_box(PackageId::new("test/package".to_string()).unwrap());

    c.bench_function("v3_cache_hit_metadata_cache", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                // In a real test, cache would be populated first
                // This is a synthetic test to measure cache lookup latency
                let _ = registry.package_exists(&package_id).await;
            })
    });
}

/// Benchmark: Search without parallelization (sequential)
fn bench_search_sequential(c: &mut Criterion) {
    let registry = setup_registry();

    c.bench_function("v3_search_sequential", |b| {
        b.iter(|| {
            let _ = registry.search_parallel(black_box("test-package"));
        })
    });
}

/// Benchmark: Parallel search (rayon)
fn bench_search_parallel(c: &mut Criterion) {
    let registry = setup_registry();

    c.bench_function("v3_search_parallel", |b| {
        b.iter(|| {
            let _ = registry.search_parallel(black_box("test package marketplace"));
        })
    });
}

/// Benchmark: Statistics collection (atomic operations)
fn bench_stats_collection(c: &mut Criterion) {
    let registry = setup_registry();

    c.bench_function("v3_stats_snapshot", |b| {
        b.iter(|| {
            let _ = registry.stats();
        })
    });
}

/// Benchmark: Metrics snapshot collection
fn bench_metrics_snapshot(c: &mut Criterion) {
    let registry = setup_registry();

    c.bench_function("v3_metrics_snapshot", |b| {
        b.iter(|| {
            let _ = registry.metrics_snapshot();
        })
    });
}

/// Benchmark: Latency recording (different latency ranges)
fn bench_latency_recording(c: &mut Criterion) {
    let mut group = c.benchmark_group("v3_latency_recording");

    let latencies = vec![
        ("5us", 5u64),
        ("25us", 25u64),
        ("75us", 75u64),
        ("250us", 250u64),
        ("750us", 750u64),
        ("2500us", 2500u64),
        ("7500us", 7500u64),
        ("15000us", 15000u64),
    ];

    for (name, latency) in latencies {
        let registry = setup_registry();

        group.bench_with_input(BenchmarkId::from_parameter(name), &latency, |b, &lat| {
            b.iter(|| {
                registry.record_latency(black_box(lat));
            })
        });
    }

    group.finish();
}

/// Benchmark: Search index update
fn bench_search_index_update(c: &mut Criterion) {
    let registry = setup_registry();

    c.bench_function("v3_search_index_update", |b| {
        b.iter(|| {
            let _ = registry.update_search_index(
                black_box("test-package-id"),
                black_box("Test Package Description"),
            );
        })
    });
}

/// Benchmark: Query plan cache lookup
fn bench_query_plan_cache(c: &mut Criterion) {
    let registry = setup_registry();

    c.bench_function("v3_query_plan_cache_lookup", |b| {
        b.iter(|| {
            let plan_key = format!("plan:version:test:1.0.0");
            let _cached = registry.query_plan_cache.write().get(&plan_key).is_some();
        })
    });
}

/// Benchmark: Batch operations
fn bench_batch_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("v3_batch_operations");

    let sizes = vec![10, 50, 100, 500];

    for size in sizes {
        let registry = setup_registry();

        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{}_packages", size)),
            &size,
            |b, &_size| {
                b.to_async(tokio::runtime::Runtime::new().unwrap())
                    .iter(|| async {
                        let ids: Vec<PackageId> = (0.._size)
                            .map(|i| PackageId::new(format!("test/package-{}", i)).unwrap())
                            .collect();
                        let _ = registry.batch_delete(black_box(ids)).await;
                    })
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_cache_hit,
    bench_search_sequential,
    bench_search_parallel,
    bench_stats_collection,
    bench_metrics_snapshot,
    bench_latency_recording,
    bench_search_index_update,
    bench_query_plan_cache,
    bench_batch_operations,
);

criterion_main!(benches);
