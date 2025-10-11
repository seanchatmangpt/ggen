use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_mcp::tools::{graph, market, project, template};
use serde_json::json;
use tokio::runtime::Runtime;

fn benchmark_project_gen(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("project_gen");

    // Cold start benchmark
    group.bench_function("cold_start", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "template": "rust-lib",
                "vars": {
                    "name": "test-project",
                    "version": "0.1.0"
                }
            });
            black_box(project::gen(params).await)
        });
    });

    // Hot path benchmark (simulating cached scenarios)
    group.bench_function("hot_path", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "template": "rust-lib",
                "vars": {
                    "name": "test-project",
                    "version": "0.1.0"
                }
            });
            black_box(project::gen(params).await)
        });
    });

    // Varying complexity
    for size in [1, 10, 100].iter() {
        group.bench_with_input(BenchmarkId::new("variables", size), size, |b, &size| {
            b.to_async(&rt).iter(|| async move {
                let mut vars = json!({});
                for i in 0..size {
                    vars[format!("var_{}", i)] = json!(format!("value_{}", i));
                }
                let params = json!({
                    "template": "rust-lib",
                    "vars": vars
                });
                black_box(project::gen(params).await)
            });
        });
    }

    group.finish();
}

fn benchmark_market_search(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("market_search");

    // Cold start
    group.bench_function("cold_start", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "query": "authentication",
                "limit": 10
            });
            black_box(market::search(params).await)
        });
    });

    // Hot path (should benefit from caching)
    group.bench_function("hot_path", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "query": "authentication",
                "limit": 10
            });
            black_box(market::search(params).await)
        });
    });

    // Fuzzy search overhead
    group.bench_function("fuzzy_search", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "query": "authen",
                "fuzzy": "true",
                "limit": 10
            });
            black_box(market::search(params).await)
        });
    });

    // Advanced filtering
    group.bench_function("advanced_filtering", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "query": "rust",
                "category": "web",
                "min_stars": 10,
                "min_downloads": 100,
                "limit": 10
            });
            black_box(market::search(params).await)
        });
    });

    group.finish();
}

fn benchmark_graph_query(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("graph_query");

    // Simple query
    group.bench_function("simple_query", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
            });
            black_box(graph::query(params).await)
        });
    });

    // Complex query with multiple patterns
    group.bench_function("complex_query", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "sparql": "SELECT ?s WHERE { ?s <http://example.org/type> ?type . ?s <http://example.org/hasProperty> ?prop . FILTER(?type = 'Entity') } LIMIT 100"
            });
            black_box(graph::query(params).await)
        });
    });

    // With named graph
    group.bench_function("named_graph", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10",
                "graph": "http://example.org/graph1"
            });
            black_box(graph::query(params).await)
        });
    });

    group.finish();
}

fn benchmark_template_list(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("template_list");

    group.bench_function("cold_start", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({});
            black_box(market::list(params).await)
        });
    });

    group.bench_function("hot_path", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({});
            black_box(market::list(params).await)
        });
    });

    // Different limits to test pagination overhead
    for limit in [10, 50, 100, 500].iter() {
        group.bench_with_input(BenchmarkId::new("limit", limit), limit, |b, &limit| {
            b.to_async(&rt).iter(|| async move {
                let params = json!({
                    "limit": limit
                });
                black_box(market::list(params).await)
            });
        });
    }

    group.finish();
}

fn benchmark_concurrent_load(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("concurrent_load");
    group.throughput(Throughput::Elements(100));

    // 100 parallel market searches
    group.bench_function("market_search_100", |b| {
        b.to_async(&rt).iter(|| async {
            let queries = vec![
                "authentication",
                "authorization",
                "database",
                "api",
                "web",
                "cli",
                "template",
                "graphql",
                "rest",
                "crud",
            ];

            let tasks: Vec<_> = (0..100)
                .map(|i| {
                    let query = queries[i % queries.len()];
                    let params = json!({
                        "query": query,
                        "limit": 5
                    });
                    tokio::spawn(async move { market::search(params).await })
                })
                .collect();

            for task in tasks {
                let _ = task.await;
            }
        });
    });

    // 100 parallel project generations
    group.bench_function("project_gen_100", |b| {
        b.to_async(&rt).iter(|| async {
            let tasks: Vec<_> = (0..100)
                .map(|i| {
                    let params = json!({
                        "template": "rust-lib",
                        "vars": {
                            "name": format!("test-{}", i)
                        }
                    });
                    tokio::spawn(async move { project::gen(params).await })
                })
                .collect();

            for task in tasks {
                let _ = task.await;
            }
        });
    });

    // Mixed workload
    group.bench_function("mixed_workload_100", |b| {
        b.to_async(&rt).iter(|| async {
            let tasks: Vec<_> = (0..100)
                .map(|i| {
                    tokio::spawn(async move {
                        match i % 4 {
                            0 => {
                                let params = json!({"query": "test", "limit": 5});
                                market::search(params).await
                            }
                            1 => {
                                let params = json!({"template": "rust-lib"});
                                project::gen(params).await
                            }
                            2 => {
                                let params = json!({});
                                market::list(params).await
                            }
                            _ => {
                                let params =
                                    json!({"sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 5"});
                                graph::query(params).await
                            }
                        }
                    })
                })
                .collect();

            for task in tasks {
                let _ = task.await;
            }
        });
    });

    group.finish();
}

fn benchmark_latency_distribution(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("latency_distribution");
    group.sample_size(1000); // More samples for better percentile calculation

    group.bench_function("market_search_p50", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "query": "auth",
                "limit": 10
            });
            black_box(market::search(params).await)
        });
    });

    group.bench_function("project_gen_p95", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "template": "rust-lib",
                "vars": {"name": "test"}
            });
            black_box(project::gen(params).await)
        });
    });

    group.bench_function("graph_query_p99", |b| {
        b.to_async(&rt).iter(|| async {
            let params = json!({
                "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
            });
            black_box(graph::query(params).await)
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_project_gen,
    benchmark_market_search,
    benchmark_graph_query,
    benchmark_template_list,
    benchmark_concurrent_load,
    benchmark_latency_distribution
);
criterion_main!(benches);
