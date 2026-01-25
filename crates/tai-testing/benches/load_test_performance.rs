use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::time::Duration;
use tai_testing::LoadTest;

fn bench_ramp_up_load_test(c: &mut Criterion) {
    c.bench_function("ramp_up_load_test", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            let test = LoadTest::new(black_box("http://bench.example.com"))
                .with_ramp_up(black_box(10), black_box(100))
                .with_duration(Duration::from_secs(5));
            let _ = test.run().await;
        });
    });
}

fn bench_spike_load_test(c: &mut Criterion) {
    c.bench_function("spike_load_test", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            let test = LoadTest::new(black_box("http://bench.example.com"))
                .with_spike(black_box(10), black_box(500))
                .with_duration(Duration::from_secs(5));
            let _ = test.run().await;
        });
    });
}

fn bench_soak_load_test(c: &mut Criterion) {
    c.bench_function("soak_load_test", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            let test = LoadTest::new(black_box("http://bench.example.com"))
                .with_soak(black_box(100))
                .with_duration(Duration::from_secs(5));
            let _ = test.run().await;
        });
    });
}

criterion_group!(
    benches,
    bench_ramp_up_load_test,
    bench_spike_load_test,
    bench_soak_load_test
);
criterion_main!(benches);
