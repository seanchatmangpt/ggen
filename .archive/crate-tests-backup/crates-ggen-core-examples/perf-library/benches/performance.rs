//! Comprehensive performance benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use perf_library::{FastHashMap, LockFreeQueue, LockFreeStack, CompactStorage};
use std::collections::HashMap;
use rayon::prelude::*;

fn bench_hashmap_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("hashmap_insert");

    for size in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*size as u64));

        group.bench_with_input(BenchmarkId::new("FastHashMap", size), size, |b, &size| {
            b.iter(|| {
                let mut map = FastHashMap::new();
                for i in 0..size {
                    map.insert(black_box(i), black_box(i * 2));
                }
                map
            });
        });

        group.bench_with_input(BenchmarkId::new("HashMap", size), size, |b, &size| {
            b.iter(|| {
                let mut map = HashMap::new();
                for i in 0..size {
                    map.insert(black_box(i), black_box(i * 2));
                }
                map
            });
        });
    }

    group.finish();
}

fn bench_hashmap_get(c: &mut Criterion) {
    let mut group = c.benchmark_group("hashmap_get");

    for size in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*size as u64));

        let mut fast_map = FastHashMap::new();
        let mut std_map = HashMap::new();
        for i in 0..*size {
            fast_map.insert(i, i * 2);
            std_map.insert(i, i * 2);
        }

        group.bench_with_input(BenchmarkId::new("FastHashMap", size), size, |b, &size| {
            b.iter(|| {
                for i in 0..size {
                    black_box(fast_map.get(&i));
                }
            });
        });

        group.bench_with_input(BenchmarkId::new("HashMap", size), size, |b, &size| {
            b.iter(|| {
                for i in 0..size {
                    black_box(std_map.get(&i));
                }
            });
        });
    }

    group.finish();
}

fn bench_queue_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("queue_operations");

    for size in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*size as u64));

        group.bench_with_input(BenchmarkId::new("push", size), size, |b, &size| {
            b.iter(|| {
                let queue = LockFreeQueue::new();
                for i in 0..size {
                    queue.push(black_box(i));
                }
                queue
            });
        });

        group.bench_with_input(BenchmarkId::new("push_pop", size), size, |b, &size| {
            b.iter(|| {
                let queue = LockFreeQueue::new();
                for i in 0..size {
                    queue.push(black_box(i));
                }
                for _ in 0..size {
                    black_box(queue.pop());
                }
            });
        });
    }

    group.finish();
}

fn bench_concurrent_queue(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_queue");

    for threads in [2, 4, 8].iter() {
        group.bench_with_input(
            BenchmarkId::new("parallel_push", threads),
            threads,
            |b, &threads| {
                b.iter(|| {
                    let queue = LockFreeQueue::new();
                    (0..threads).into_par_iter().for_each(|thread_id| {
                        for i in 0..1000 {
                            queue.push(black_box(thread_id * 1000 + i));
                        }
                    });
                    queue
                });
            },
        );
    }

    group.finish();
}

fn bench_stack_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("stack_operations");

    for size in [100, 1000, 10000].iter() {
        group.throughput(Throughput::Elements(*size as u64));

        group.bench_with_input(BenchmarkId::new("push_pop", size), size, |b, &size| {
            b.iter(|| {
                let stack = LockFreeStack::new();
                for i in 0..size {
                    stack.push(black_box(i));
                }
                for _ in 0..size {
                    black_box(stack.pop());
                }
            });
        });
    }

    group.finish();
}

fn bench_storage_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("storage_operations");

    for size in [10, 100, 1000].iter() {
        group.throughput(Throughput::Elements(*size as u64));

        group.bench_with_input(BenchmarkId::new("store", size), size, |b, &size| {
            b.iter(|| {
                let mut storage = CompactStorage::new();
                for i in 0..size {
                    let data = vec![i; 10];
                    storage.store(black_box(data));
                }
                storage
            });
        });

        group.bench_with_input(BenchmarkId::new("store_retrieve", size), size, |b, &size| {
            b.iter(|| {
                let mut storage = CompactStorage::new();
                let mut ids = Vec::new();
                for i in 0..size {
                    let data = vec![i; 10];
                    let id = storage.store(black_box(data));
                    ids.push(id);
                }
                for id in ids {
                    black_box(storage.retrieve(id));
                }
            });
        });
    }

    group.finish();
}

fn bench_storage_compact(c: &mut Criterion) {
    let mut group = c.benchmark_group("storage_compact");

    for size in [100, 1000].iter() {
        group.bench_with_input(BenchmarkId::new("compact", size), size, |b, &size| {
            let mut storage = CompactStorage::new();
            for i in 0..size {
                storage.store(vec![i; 10]);
            }
            // Remove half the items
            for i in (0..size).step_by(2) {
                storage.remove(i);
            }

            b.iter(|| {
                let mut s = storage.clone();
                s.compact();
                s
            });
        });
    }

    group.finish();
}

fn bench_parallel_workload(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallel_workload");

    group.bench_function("mixed_operations", |b| {
        b.iter(|| {
            let queue = LockFreeQueue::new();

            // Simulate mixed workload
            (0..4).into_par_iter().for_each(|thread_id| {
                match thread_id % 3 {
                    0 => {
                        // Producer
                        for i in 0..1000 {
                            queue.push(black_box(i));
                        }
                    }
                    1 => {
                        // Consumer
                        for _ in 0..800 {
                            black_box(queue.pop());
                        }
                    }
                    _ => {
                        // Mixed
                        for i in 0..500 {
                            queue.push(black_box(i));
                            black_box(queue.pop());
                        }
                    }
                }
            });
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_hashmap_insert,
    bench_hashmap_get,
    bench_queue_operations,
    bench_concurrent_queue,
    bench_stack_operations,
    bench_storage_operations,
    bench_storage_compact,
    bench_parallel_workload
);
criterion_main!(benches);
