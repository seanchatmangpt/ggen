//! Benchmarks for gateway routing performance

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tai_gateway::{routing::RouteConfig, routing::RouteRegistry, routing::Upstream, routing::UpstreamPool, Router};

fn routing_benchmark(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("route_exact_path", |b| {
        b.to_async(&rt).iter(|| async {
            let registry = RouteRegistry::new();
            let pool = UpstreamPool::new();
            let router = Router::new(registry.clone(), pool.clone());

            let route = RouteConfig::new(
                "test-route",
                black_box("/api/v1/users"),
                vec!["GET".to_string()],
                vec!["upstream-1".to_string()],
            );
            registry.register(route).await.ok();

            let upstream = Upstream::new("upstream-1", "http://localhost:8080");
            pool.register(upstream).await.ok();

            let instance = pool.get("upstream-1").await.ok();
            if let Some(inst) = instance {
                inst.set_health_status(tai_gateway::routing::upstream::HealthStatus::Healthy)
                    .await;
            }

            router
                .route(black_box("GET"), black_box("/api/v1/users"))
                .await
        })
    });

    c.bench_function("route_wildcard_path", |b| {
        b.to_async(&rt).iter(|| async {
            let registry = RouteRegistry::new();
            let pool = UpstreamPool::new();
            let router = Router::new(registry.clone(), pool.clone());

            let route = RouteConfig::new(
                "wildcard-route",
                black_box("/api/v*/users/*"),
                vec!["GET".to_string()],
                vec!["upstream-1".to_string()],
            );
            registry.register(route).await.ok();

            let upstream = Upstream::new("upstream-1", "http://localhost:8080");
            pool.register(upstream).await.ok();

            let instance = pool.get("upstream-1").await.ok();
            if let Some(inst) = instance {
                inst.set_health_status(tai_gateway::routing::upstream::HealthStatus::Healthy)
                    .await;
            }

            router
                .route(black_box("GET"), black_box("/api/v1/users/123"))
                .await
        })
    });

    c.bench_function("pattern_matching_cache_hit", |b| {
        b.to_async(&rt).iter(|| async {
            let registry = RouteRegistry::new();
            let pool = UpstreamPool::new();
            let router = Router::new(registry.clone(), pool.clone());

            let route = RouteConfig::new(
                "cached-route",
                black_box("/api/v1/orders/*"),
                vec!["GET".to_string()],
                vec!["upstream-1".to_string()],
            );
            registry.register(route).await.ok();

            let upstream = Upstream::new("upstream-1", "http://localhost:8080");
            pool.register(upstream).await.ok();

            let instance = pool.get("upstream-1").await.ok();
            if let Some(inst) = instance {
                inst.set_health_status(tai_gateway::routing::upstream::HealthStatus::Healthy)
                    .await;
            }

            // Warm up cache
            router
                .route(black_box("GET"), black_box("/api/v1/orders/1"))
                .await
                .ok();

            // Benchmark cached lookup
            router
                .route(black_box("GET"), black_box("/api/v1/orders/2"))
                .await
        })
    });
}

criterion_group!(benches, routing_benchmark);
criterion_main!(benches);
