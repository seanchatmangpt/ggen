use criterion::{criterion_group, criterion_main, Criterion};

fn noop_score() {
    // Minimal placeholder to satisfy workspace bench registration.
    let _ = 1 + 1;
}

fn bench_value_scoring(c: &mut Criterion) {
    c.bench_function("noop_score", |b| b.iter(noop_score));
}

criterion_group!(benches, bench_value_scoring);
criterion_main!(benches);
