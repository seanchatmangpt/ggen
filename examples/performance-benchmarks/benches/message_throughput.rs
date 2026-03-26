//! Benchmark message throughput

use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};

fn message_throughput_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("message_throughput");
    
    for num_messages in &[100, 1000, 10000] {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_messages),
            num_messages,
            |b, &count| {
                b.iter(|| {
                    // Simulate message queue processing
                    let _messages: Vec<_> = (0..count)
                        .map(|i| {
                            (
                                uuid::Uuid::new_v4(),
                                format!("msg_{}", i),
                            )
                        })
                        .collect();
                })
            },
        );
    }
    group.finish();
}

criterion_group!(benches, message_throughput_benchmark);
criterion_main!(benches);
