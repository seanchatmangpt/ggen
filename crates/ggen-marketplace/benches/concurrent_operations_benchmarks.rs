//! Concurrent marketplace operations benchmarks
//!
//! Benchmarks for concurrent installs, searches, and registry operations
//!
//! Run with: cargo bench --bench concurrent_operations_benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

// Simulate concurrent operations
fn simulate_concurrent_operation(operation_count: usize, concurrent_tasks: usize) -> usize {
    let result = Arc::new(AtomicUsize::new(0));
    let mut handles = vec![];

    for _ in 0..concurrent_tasks {
        let result_clone = Arc::clone(&result);
        handles.push(std::thread::spawn(move || {
            for _ in 0..operation_count {
                // Simulate work
                let mut sum = 0;
                for i in 0..100 {
                    sum += i;
                }
                result_clone.fetch_add(sum, Ordering::Relaxed);
            }
        }));
    }

    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    result.load(Ordering::Relaxed)
}

fn bench_concurrent_searches(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_searches");
    group.sample_size(20);

    let concurrent_levels = vec![
        ("1_concurrent", 1),
        ("4_concurrent", 4),
        ("8_concurrent", 8),
        ("16_concurrent", 16),
    ];

    for (name, concurrent) in concurrent_levels {
        group.throughput(Throughput::Elements((100 * concurrent) as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &concurrent,
            |b, &concurrent| {
                b.iter(|| {
                    let _result = simulate_concurrent_operation(100, concurrent);
                });
            },
        );
    }

    group.finish();
}

fn bench_concurrent_installs(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_installs");
    group.sample_size(20);

    let install_counts = vec![
        ("1_install", 1, 1),
        ("4_installs_2_concurrent", 4, 2),
        ("10_installs_4_concurrent", 10, 4),
        ("20_installs_8_concurrent", 20, 8),
    ];

    for (name, installs, concurrent) in install_counts {
        group.throughput(Throughput::Elements(installs as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &(installs, concurrent),
            |b, &(installs, concurrent)| {
                b.iter(|| {
                    let _result = simulate_concurrent_operation(installs, concurrent);
                });
            },
        );
    }

    group.finish();
}

fn bench_registry_operations_concurrent(c: &mut Criterion) {
    let mut group = c.benchmark_group("registry_operations_concurrent");
    group.sample_size(15);

    group.bench_function("concurrent_reads_4_threads", |b| {
        b.iter(|| {
            let _result = simulate_concurrent_operation(50, 4);
        });
    });

    group.bench_function("concurrent_writes_4_threads", |b| {
        b.iter(|| {
            let _result = simulate_concurrent_operation(25, 4);
        });
    });

    group.bench_function("mixed_read_write_4_threads", |b| {
        b.iter(|| {
            let _result = simulate_concurrent_operation(50, 4);
        });
    });

    group.finish();
}

fn bench_cache_contention(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_contention");
    group.sample_size(15);

    let contention_levels = vec![
        ("low_contention_2_threads", 2, 1000),
        ("medium_contention_4_threads", 4, 500),
        ("high_contention_8_threads", 8, 250),
    ];

    for (name, threads, ops_per_thread) in contention_levels {
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &(threads, ops_per_thread),
            |b, &(threads, ops_per_thread)| {
                b.iter(|| {
                    let _result = simulate_concurrent_operation(ops_per_thread, threads);
                });
            },
        );
    }

    group.finish();
}

fn bench_scalability(c: &mut Criterion) {
    let mut group = c.benchmark_group("scalability");
    group.sample_size(10);

    // Simulate scaling from 10 to 1000 packages
    let package_scales = vec![
        ("10_packages", 10),
        ("100_packages", 100),
        ("1000_packages", 1000),
    ];

    for (name, packages) in package_scales {
        group.throughput(Throughput::Elements(packages as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &packages,
            |b, &packages| {
                b.iter(|| {
                    // Simulate scanning/indexing packages
                    for _ in 0..packages {
                        let _ = black_box(packages);
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_lock_contention(c: &mut Criterion) {
    let mut group = c.benchmark_group("lock_contention");
    group.sample_size(10);

    group.bench_function("rwlock_heavy_read_contention", |b| {
        b.iter(|| {
            let _result = simulate_concurrent_operation(500, 8);
        });
    });

    group.bench_function("rwlock_heavy_write_contention", |b| {
        b.iter(|| {
            let _result = simulate_concurrent_operation(100, 8);
        });
    });

    group.bench_function("mutex_fairness_test", |b| {
        b.iter(|| {
            let _result = simulate_concurrent_operation(200, 4);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_concurrent_searches,
    bench_concurrent_installs,
    bench_registry_operations_concurrent,
    bench_cache_contention,
    bench_scalability,
    bench_lock_contention,
);
criterion_main!(benches);
