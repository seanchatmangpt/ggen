//! Performance benchmarks for ggen-mcp server
//!
//! Run with: cargo bench

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use ggen_mcp::server::GgenMcpServer;
use serde_json::json;

fn benchmark_server_creation(c: &mut Criterion) {
    c.bench_function("server_creation", |b| {
        b.iter(|| {
            let server = GgenMcpServer::new();
            black_box(server);
        });
    });
}

fn benchmark_template_parsing(c: &mut Criterion) {
    let template = json!({
        "name": "test-template",
        "version": "1.0.0",
        "files": (0..10).map(|i| json!({
            "path": format!("file{}.rs", i),
            "content": format!("// File {}", i)
        })).collect::<Vec<_>>()
    });

    c.bench_function("template_parsing", |b| {
        b.iter(|| {
            let parsed = black_box(template.clone());
            black_box(parsed);
        });
    });
}

fn benchmark_template_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_sizes");

    for size in [10, 50, 100, 500].iter() {
        let template = json!({
            "name": "test-template",
            "files": (0..*size).map(|i| json!({
                "path": format!("file{}.rs", i),
                "content": format!("// File content {}", i)
            })).collect::<Vec<_>>()
        });

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.iter(|| {
                let parsed = black_box(template.clone());
                black_box(parsed);
            });
        });
    }

    group.finish();
}

fn benchmark_concurrent_requests(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("concurrent_10_requests", |b| {
        b.to_async(&runtime).iter(|| async {
            let handles: Vec<_> = (0..10)
                .map(|_| {
                    tokio::spawn(async {
                        let server = GgenMcpServer::new();
                        black_box(server);
                    })
                })
                .collect();

            for handle in handles {
                let _ = handle.await;
            }
        });
    });
}

fn benchmark_memory_allocation(c: &mut Criterion) {
    c.bench_function("server_memory_footprint", |b| {
        b.iter(|| {
            let servers: Vec<_> = (0..100)
                .map(|_| GgenMcpServer::new())
                .collect();
            black_box(servers);
        });
    });
}

criterion_group!(
    benches,
    benchmark_server_creation,
    benchmark_template_parsing,
    benchmark_template_sizes,
    benchmark_concurrent_requests,
    benchmark_memory_allocation
);

criterion_main!(benches);
