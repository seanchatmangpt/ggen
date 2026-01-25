//! Benchmarks for rate limiting performance

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tai_gateway::ratelimit::{ClientId, SlidingWindowLimiter, TokenBucketLimiter};

fn rate_limiting_benchmark(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("token_bucket_check", |b| {
        b.to_async(&rt).iter(|| async {
            let limiter = TokenBucketLimiter::new(1000.0, 100);
            let client = black_box(ClientId::from_ip("192.168.1.1"));

            limiter.check(&client).await
        })
    });

    c.bench_function("sliding_window_check", |b| {
        b.to_async(&rt).iter(|| async {
            let limiter = SlidingWindowLimiter::new(100, 60);
            let client = black_box(ClientId::from_ip("192.168.1.1"));

            limiter.check(&client).await
        })
    });

    c.bench_function("multiple_clients_token_bucket", |b| {
        b.to_async(&rt).iter(|| async {
            let limiter = TokenBucketLimiter::new(1000.0, 100);

            for i in 0..10 {
                let client = black_box(ClientId::from_ip(&format!("192.168.1.{}", i)));
                limiter.check(&client).await.ok();
            }
        })
    });

    c.bench_function("client_id_creation", |b| {
        b.iter(|| {
            let _ip_client = black_box(ClientId::from_ip("192.168.1.1"));
            let _api_key_client = black_box(ClientId::from_api_key("sk-secret"));
            let _user_client = black_box(ClientId::from_user_id("user-123"));
        })
    });
}

criterion_group!(benches, rate_limiting_benchmark);
criterion_main!(benches);
