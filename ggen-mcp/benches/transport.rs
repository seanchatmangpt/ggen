use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use serde_json::{json, Value};
use tokio::runtime::Runtime;

/// Benchmark JSON serialization/deserialization overhead
fn benchmark_json_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("json_operations");

    // Small payload
    let small_payload = json!({
        "method": "project/gen",
        "params": {
            "template": "rust-lib",
            "vars": {"name": "test"}
        }
    });

    group.bench_function("serialize_small", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&small_payload).unwrap())
        });
    });

    group.bench_function("deserialize_small", |b| {
        let json_str = serde_json::to_string(&small_payload).unwrap();
        b.iter(|| {
            black_box(serde_json::from_str::<Value>(&json_str).unwrap())
        });
    });

    // Medium payload (typical marketplace response)
    let medium_payload = json!({
        "templates": (0..20).map(|i| json!({
            "id": format!("template_{}", i),
            "name": format!("Template {}", i),
            "description": "A test template with some metadata",
            "stars": i * 10,
            "downloads": i * 100,
            "tags": vec!["rust", "web", "api"]
        })).collect::<Vec<_>>(),
        "total": 20
    });

    group.bench_function("serialize_medium", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&medium_payload).unwrap())
        });
    });

    group.bench_function("deserialize_medium", |b| {
        let json_str = serde_json::to_string(&medium_payload).unwrap();
        b.iter(|| {
            black_box(serde_json::from_str::<Value>(&json_str).unwrap())
        });
    });

    // Large payload (bulk operations)
    let large_payload = json!({
        "results": (0..500).map(|i| json!({
            "id": i,
            "data": format!("Data for item {}", i),
            "metadata": {
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-10T00:00:00Z"
            }
        })).collect::<Vec<_>>()
    });

    group.bench_function("serialize_large", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&large_payload).unwrap())
        });
    });

    group.bench_function("deserialize_large", |b| {
        let json_str = serde_json::to_string(&large_payload).unwrap();
        b.iter(|| {
            black_box(serde_json::from_str::<Value>(&json_str).unwrap())
        });
    });

    group.finish();
}

/// Benchmark message batching strategies
fn benchmark_message_batching(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("message_batching");
    group.throughput(Throughput::Elements(100));

    // Sequential message processing
    group.bench_function("sequential_100", |b| {
        b.to_async(&rt).iter(|| async {
            for i in 0..100 {
                let message = json!({
                    "id": i,
                    "method": "market/search",
                    "params": {"query": "test"}
                });
                black_box(serde_json::to_string(&message).unwrap());
            }
        });
    });

    // Batched message processing
    group.bench_function("batched_100", |b| {
        b.to_async(&rt).iter(|| async {
            let messages: Vec<_> = (0..100).map(|i| {
                json!({
                    "id": i,
                    "method": "market/search",
                    "params": {"query": "test"}
                })
            }).collect();

            let batch = json!({
                "batch": messages
            });
            black_box(serde_json::to_string(&batch).unwrap());
        });
    });

    // Parallel message processing
    group.bench_function("parallel_100", |b| {
        b.to_async(&rt).iter(|| async {
            let tasks: Vec<_> = (0..100).map(|i| {
                tokio::spawn(async move {
                    let message = json!({
                        "id": i,
                        "method": "market/search",
                        "params": {"query": "test"}
                    });
                    serde_json::to_string(&message).unwrap()
                })
            }).collect();

            for task in tasks {
                let _ = task.await;
            }
        });
    });

    group.finish();
}

/// Benchmark connection pooling efficiency
fn benchmark_connection_pooling(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("connection_pooling");

    // Simulate connection creation overhead
    async fn create_connection() -> usize {
        tokio::time::sleep(tokio::time::Duration::from_micros(100)).await;
        42 // Simulated connection handle
    }

    // Without pooling - create new connection each time
    group.bench_function("no_pooling", |b| {
        b.to_async(&rt).iter(|| async {
            for _ in 0..10 {
                let _conn = create_connection().await;
                black_box(_conn);
            }
        });
    });

    // With pooling - reuse connections
    group.bench_function("with_pooling", |b| {
        b.to_async(&rt).iter(|| async {
            // Simulate pool with pre-created connections
            let pool: Vec<usize> = vec![42; 5];

            for i in 0..10 {
                let _conn = pool[i % pool.len()];
                black_box(_conn);
            }
        });
    });

    group.finish();
}

/// Benchmark request/response overhead
fn benchmark_request_response(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("request_response");

    // Minimal request/response
    group.bench_function("minimal_roundtrip", |b| {
        b.to_async(&rt).iter(|| async {
            let request = json!({
                "method": "ping",
                "id": 1
            });

            let request_str = serde_json::to_string(&request).unwrap();
            black_box(&request_str);

            let response = json!({
                "result": "pong",
                "id": 1
            });

            let response_str = serde_json::to_string(&response).unwrap();
            black_box(response_str);
        });
    });

    // Typical tool call roundtrip
    group.bench_function("tool_call_roundtrip", |b| {
        b.to_async(&rt).iter(|| async {
            let request = json!({
                "method": "market/search",
                "params": {
                    "query": "authentication",
                    "limit": 10
                },
                "id": 1
            });

            let request_str = serde_json::to_string(&request).unwrap();
            black_box(&request_str);

            // Simulate processing delay
            tokio::time::sleep(tokio::time::Duration::from_micros(50)).await;

            let response = json!({
                "result": {
                    "templates": [],
                    "total": 0
                },
                "id": 1
            });

            let response_str = serde_json::to_string(&response).unwrap();
            black_box(response_str);
        });
    });

    // Bulk operation roundtrip
    group.bench_function("bulk_roundtrip", |b| {
        b.to_async(&rt).iter(|| async {
            let request = json!({
                "method": "market/list",
                "params": {
                    "limit": 100
                },
                "id": 1
            });

            let request_str = serde_json::to_string(&request).unwrap();
            black_box(&request_str);

            // Simulate processing delay
            tokio::time::sleep(tokio::time::Duration::from_micros(200)).await;

            let results: Vec<_> = (0..100).map(|i| json!({
                "id": i,
                "name": format!("item_{}", i)
            })).collect();

            let response = json!({
                "result": {
                    "templates": results,
                    "total": 100
                },
                "id": 1
            });

            let response_str = serde_json::to_string(&response).unwrap();
            black_box(response_str);
        });
    });

    group.finish();
}

/// Benchmark pipeline optimization
fn benchmark_pipeline(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("pipeline");

    // Sequential pipeline
    group.bench_function("sequential", |b| {
        b.to_async(&rt).iter(|| async {
            let mut results = Vec::new();

            for i in 0..10 {
                let request = json!({"id": i});
                let request_str = serde_json::to_string(&request).unwrap();
                tokio::time::sleep(tokio::time::Duration::from_micros(10)).await;
                let response = json!({"result": i});
                let response_str = serde_json::to_string(&response).unwrap();
                results.push(response_str);
            }

            black_box(results);
        });
    });

    // Pipelined requests (don't wait for response before sending next)
    group.bench_function("pipelined", |b| {
        b.to_async(&rt).iter(|| async {
            let tasks: Vec<_> = (0..10).map(|i| {
                tokio::spawn(async move {
                    let request = json!({"id": i});
                    let request_str = serde_json::to_string(&request).unwrap();
                    tokio::time::sleep(tokio::time::Duration::from_micros(10)).await;
                    let response = json!({"result": i});
                    serde_json::to_string(&response).unwrap()
                })
            }).collect();

            let results: Vec<_> = futures::future::join_all(tasks)
                .await
                .into_iter()
                .filter_map(|r| r.ok())
                .collect();

            black_box(results);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_json_operations,
    benchmark_message_batching,
    benchmark_connection_pooling,
    benchmark_request_response,
    benchmark_pipeline
);
criterion_main!(benches);
