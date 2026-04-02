//! Memory and stability performance benchmarks
//!
//! Measures performance consistency and memory behavior under repeated operations.
//! These benchmarks help detect performance regressions and memory leaks.
//!
//! Methodology:
//! - Repeats operations 100-400 times to measure consistency
//! - Tracks allocation patterns and cache behavior
//! - Measures lock fairness and contention scenarios
//! - Reports mean time and variance across iterations
//!
//! Note: These are behavioral tests, not full memory profiling.
//! Use valgrind or heaptrack for detailed memory analysis.
//!
//! Run with: cargo bench --bench stability_benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::collections::HashMap;
use std::fs;
use std::sync::Arc;
use std::sync::Mutex;
use tempfile::TempDir;

fn bench_memory_stability(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_stability");
    group.sample_size(10);
    // Extended measurement time for stability testing
    group.measurement_time(std::time::Duration::from_secs(30));

    group.bench_function("repeated_allocation_1000_items", |b| {
        b.iter(|| {
            let mut vec = Vec::with_capacity(1000);
            for i in 0..1000 {
                vec.push(black_box(format!("Item {}", i)));
            }
            // Simulate processing
            let _sum: usize = vec.iter().map(|s| s.len()).sum();
        });
    });

    group.bench_function("repeated_hashmap_insertion_500_entries", |b| {
        b.iter(|| {
            let mut map = HashMap::new();
            for i in 0..500 {
                map.insert(black_box(format!("key_{}", i)), black_box(i * 2));
            }
            // Simulate iteration
            let _count = map.len();
        });
    });

    group.finish();
}

fn bench_file_operation_stability(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_operation_stability");
    group.sample_size(10);

    group.bench_function("repeated_file_create_write_delete_100files", |b| {
        b.iter(|| {
            let temp_dir = TempDir::new().unwrap();
            for i in 0..100 {
                let path = temp_dir.path().join(format!("file_{}.txt", i));
                let content = format!("Content of file {}", i);
                fs::write(&path, content).expect("Failed to write");
            }
            // All files deleted when temp_dir drops
        });
    });

    group.finish();
}

fn bench_string_allocation_stability(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_allocation_stability");
    group.sample_size(15);

    let string_sizes = vec![
        ("small_10b", 10),
        ("medium_100b", 100),
        ("large_1kb", 1000),
        ("xlarge_10kb", 10000),
    ];

    for (name, size) in string_sizes {
        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &size, |b, &size| {
            b.iter(|| {
                let mut strings = Vec::new();
                for i in 0..100 {
                    let s = "x".repeat(size);
                    strings.push(black_box(s));
                }
                let _total_len: usize = strings.iter().map(|s| s.len()).sum();
            });
        });
    }

    group.finish();
}

fn bench_concurrent_access_stability(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_access_stability");
    group.sample_size(10);

    group.bench_function("concurrent_hashmap_access_4_threads_1000_ops", |b| {
        b.iter(|| {
            let map = Arc::new(Mutex::new(HashMap::new()));

            // Populate initial data
            {
                let mut m = map.lock().unwrap();
                for i in 0..100 {
                    m.insert(format!("key_{}", i), i);
                }
            }

            let mut handles = vec![];
            for thread_id in 0..4 {
                let map_clone = Arc::clone(&map);
                handles.push(std::thread::spawn(move || {
                    for i in 0..250 {
                        let key = format!("key_{}", (thread_id * 250 + i) % 100);
                        let mut m = map_clone.lock().unwrap();
                        let _ = m.get(&key);
                    }
                }));
            }

            for handle in handles {
                handle.join().expect("Thread panicked");
            }
        });
    });

    group.finish();
}

fn bench_recursive_operations_stability(c: &mut Criterion) {
    let mut group = c.benchmark_group("recursive_operations_stability");
    group.sample_size(10);

    fn recursive_sum(n: usize) -> usize {
        if n == 0 {
            0
        } else {
            n + recursive_sum(n - 1)
        }
    }

    group.bench_function("recursive_depth_10", |b| {
        b.iter(|| {
            let _result = recursive_sum(black_box(10));
        });
    });

    group.bench_function("recursive_depth_50", |b| {
        b.iter(|| {
            let _result = recursive_sum(black_box(50));
        });
    });

    group.finish();
}

fn bench_cache_behavior(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_behavior");
    group.sample_size(10);

    group.bench_function("cache_hit_rate_high_locality", |b| {
        let data: Vec<i32> = (0..1000).collect();
        b.iter(|| {
            let mut sum = 0;
            for _ in 0..100 {
                for i in 0..100 {
                    sum += black_box(data[i]);
                }
            }
            let _ = black_box(sum);
        });
    });

    group.bench_function("cache_miss_rate_random_access", |b| {
        let data: Vec<i32> = (0..10000).collect();
        b.iter(|| {
            let mut sum = 0;
            for i in (0..10000).step_by(13) {
                sum += black_box(data[i % data.len()]);
            }
            let _ = black_box(sum);
        });
    });

    group.finish();
}

fn bench_error_path_stability(c: &mut Criterion) {
    let mut group = c.benchmark_group("error_path_stability");
    group.sample_size(20);

    group.bench_function("error_creation_and_drop_1000x", |b| {
        b.iter(|| {
            for _i in 0..1000 {
                let _err =
                    std::io::Error::new(std::io::ErrorKind::Other, black_box("Test error message"));
                // Error dropped here
            }
        });
    });

    group.bench_function("error_propagation_chain_5_levels", |b| {
        fn level_5() -> Result<(), Box<dyn std::error::Error>> {
            Err("Error at level 5".into())
        }

        fn level_4() -> Result<(), Box<dyn std::error::Error>> {
            level_5()
        }

        fn level_3() -> Result<(), Box<dyn std::error::Error>> {
            level_4()
        }

        fn level_2() -> Result<(), Box<dyn std::error::Error>> {
            level_3()
        }

        fn level_1() -> Result<(), Box<dyn std::error::Error>> {
            level_2()
        }

        b.iter(|| {
            for _i in 0..100 {
                let _result = level_1();
            }
        });
    });

    group.finish();
}

fn bench_lock_fairness(c: &mut Criterion) {
    let mut group = c.benchmark_group("lock_fairness");
    group.sample_size(10);

    group.bench_function("fairness_test_4_threads_1000_locks", |b| {
        b.iter(|| {
            let lock = Arc::new(Mutex::new(0usize));
            let mut handles = vec![];

            for _ in 0..4 {
                let lock_clone = Arc::clone(&lock);
                handles.push(std::thread::spawn(move || {
                    for _ in 0..250 {
                        let mut guard = lock_clone.lock().unwrap();
                        *guard += 1;
                    }
                }));
            }

            for handle in handles {
                handle.join().expect("Thread panicked");
            }

            let final_value = *lock.lock().unwrap();
            assert_eq!(final_value, 1000);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_memory_stability,
    bench_file_operation_stability,
    bench_string_allocation_stability,
    bench_concurrent_access_stability,
    bench_recursive_operations_stability,
    bench_cache_behavior,
    bench_error_path_stability,
    bench_lock_fairness,
);
criterion_main!(benches);
