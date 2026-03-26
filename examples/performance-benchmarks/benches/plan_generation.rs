//! Benchmark plan generation

use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};

fn plan_generation_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("plan_generation");
    
    for num_steps in &[5, 10, 20] {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_steps),
            num_steps,
            |b, &steps| {
                b.iter(|| {
                    // Simulate plan generation
                    let _plan: Vec<_> = (0..steps)
                        .map(|i| {
                            (
                                format!("step_{}", i),
                                uuid::Uuid::new_v4(),
                            )
                        })
                        .collect();
                })
            },
        );
    }
    group.finish();
}

criterion_group!(benches, plan_generation_benchmark);
criterion_main!(benches);
