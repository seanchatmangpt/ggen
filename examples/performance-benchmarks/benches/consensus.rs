use criterion::{criterion_group, criterion_main, Criterion};

fn benchmark(c: &mut Criterion) {
    c.bench_function("placeholder", |b| {
        b.iter(|| 42)
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
