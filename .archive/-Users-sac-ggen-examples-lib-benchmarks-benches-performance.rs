use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use perf_lib::*;

fn bench_deduplicate(c: &mut Criterion) {
    let mut group = c.benchmark_group("deduplicate");

    for size in [100, 1000, 10000].iter() {
        let items: Vec<String> = (0..*size)
            .map(|i| format!("item_{}", i % (size / 2)))
            .collect();

        group.bench_with_input(
            BenchmarkId::new("parallel", size),
            &items,
            |b, items| b.iter(|| deduplicate_parallel(black_box(items))),
        );

        group.bench_with_input(
            BenchmarkId::new("sequential", size),
            &items,
            |b, items| b.iter(|| deduplicate_sequential(black_box(items))),
        );
    }

    group.finish();
}

fn bench_process_batch(c: &mut Criterion) {
    let items: Vec<String> = (0..1000).map(|i| format!("test_{}", i)).collect();

    c.bench_function("process_batch_1000", |b| {
        b.iter(|| process_batch(black_box(&items)))
    });
}

criterion_group!(benches, bench_deduplicate, bench_process_batch);
criterion_main!(benches);
