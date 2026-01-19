use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use perf_library::{FastHashMap, ConcurrentCounter, parallel_process};
use std::collections::HashMap;

fn benchmark_hash_map_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("HashMap Insert");

    for size in [100, 1000, 10000].iter() {
        group.bench_with_input(BenchmarkId::new("FastHashMap", size), size, |b, &size| {
            b.iter(|| {
                let mut map = FastHashMap::new();
                for i in 0..size {
                    map.insert(black_box(i), black_box(i * 2));
                }
            });
        });

        group.bench_with_input(BenchmarkId::new("StdHashMap", size), size, |b, &size| {
            b.iter(|| {
                let mut map = HashMap::new();
                for i in 0..size {
                    map.insert(black_box(i), black_box(i * 2));
                }
            });
        });
    }

    group.finish();
}

fn benchmark_hash_map_lookup(c: &mut Criterion) {
    let mut fast_map = FastHashMap::new();
    let mut std_map = HashMap::new();

    for i in 0..10000 {
        fast_map.insert(i, i * 2);
        std_map.insert(i, i * 2);
    }

    let mut group = c.benchmark_group("HashMap Lookup");

    group.bench_function("FastHashMap", |b| {
        b.iter(|| {
            for i in 0..1000 {
                black_box(fast_map.get(&black_box(i)));
            }
        });
    });

    group.bench_function("StdHashMap", |b| {
        b.iter(|| {
            for i in 0..1000 {
                black_box(std_map.get(&black_box(i)));
            }
        });
    });

    group.finish();
}

fn benchmark_concurrent_counter(c: &mut Criterion) {
    c.bench_function("ConcurrentCounter increment", |b| {
        let counter = ConcurrentCounter::new();
        b.iter(|| {
            for _ in 0..1000 {
                black_box(counter.increment());
            }
        });
    });
}

fn benchmark_parallel_processing(c: &mut Criterion) {
    let items: Vec<i32> = (0..10000).collect();

    let mut group = c.benchmark_group("Parallel Processing");

    group.bench_function("parallel_process", |b| {
        b.iter(|| {
            parallel_process(&items, |x| black_box(x * x + x))
        });
    });

    group.bench_function("sequential", |b| {
        b.iter(|| {
            items.iter().map(|x| black_box(x * x + x)).collect::<Vec<_>>()
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_hash_map_insert,
    benchmark_hash_map_lookup,
    benchmark_concurrent_counter,
    benchmark_parallel_processing
);
criterion_main!(benches);
