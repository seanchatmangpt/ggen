use criterion::{criterion_group, criterion_main, Criterion};
use tai_testing::StateInvariant;

fn bench_circuit_breaker_invariant(c: &mut Criterion) {
    c.bench_function("circuit_breaker_invariant", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            let inv = StateInvariant::circuit_breaker_never_stuck_open().with_iterations(100);
            let _ = inv.verify().await;
        });
    });
}

fn bench_queue_preservation_invariant(c: &mut Criterion) {
    c.bench_function("queue_preservation_invariant", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            let inv = StateInvariant::queue_message_preservation().with_iterations(100);
            let _ = inv.verify().await;
        });
    });
}

fn bench_latency_bounded_invariant(c: &mut Criterion) {
    c.bench_function("latency_bounded_invariant", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            let inv = StateInvariant::latency_bounded().with_iterations(100);
            let _ = inv.verify().await;
        });
    });
}

criterion_group!(
    benches,
    bench_circuit_breaker_invariant,
    bench_queue_preservation_invariant,
    bench_latency_bounded_invariant
);
criterion_main!(benches);
