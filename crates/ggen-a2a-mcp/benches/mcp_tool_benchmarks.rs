//! Criterion benchmarks for all MCP tools, resources, and prompts
//!
//! Measures latency and throughput for:
//! - validate (TTL parsing)
//! - list_generators (static list)
//! - list_examples (filesystem scan)
//! - get_example (file reads)
//! - query_ontology (SPARQL execution)
//! - search (marketplace lookup)
//! - scaffold_from_example (filesystem copy)
//! - list_resources, read_resource (resource access)
//! - list_prompts, get_prompt (prompt access)
//! - complete (completion suggestions)

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

mod bench_helper;
use bench_helper::*;

/// Minimal client handler for benchmarking
#[derive(Debug, Clone, Default)]
struct BenchClientHandler;

impl ClientHandler for BenchClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

/// Helper to start a server and return a client with a shared runtime.
async fn start_bench_server() -> anyhow::Result<RunningService<RoleClient, BenchClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(65_536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = BenchClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

fn bench_validate(c: &mut Criterion) {
    let mut group = c.benchmark_group("validate");

    for size in [10, 50, 100, 500].iter() {
        let ttl = black_box(format!(
            "@prefix ex: <http://example.org/> .\n{}\n",
            (0..*size)
                .map(|i| format!("ex:s{} a ex:Class .", i))
                .collect::<Vec<_>>()
                .join("\n")
        ));

        group.throughput(Throughput::Bytes(ttl.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| {
                let ttl_copy = ttl.clone();
                async move {
                    if let Ok(client) = start_bench_server().await {
                        let args = serde_json::json!({"ttl": &ttl_copy})
                            .as_object()
                            .unwrap()
                            .clone();
                        let _ =
                            client.call_tool(CallToolRequestParams::new("validate").with_arguments(args)).await;
                        let _ = client.cancel().await;
                    }
                }
            });
        });
    }
    group.finish();
}

fn bench_list_generators(c: &mut Criterion) {
    c.bench_function("list_generators", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let _ = client.call_tool(CallToolRequestParams::new("list_generators")).await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_list_examples(c: &mut Criterion) {
    c.bench_function("list_examples", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let _ = client.call_tool(CallToolRequestParams::new("list_examples")).await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_list_resources(c: &mut Criterion) {
    c.bench_function("list_resources", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let _ = client.list_resources(None).await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_list_prompts(c: &mut Criterion) {
    c.bench_function("list_prompts", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let _ = client.list_prompts(None).await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_search(c: &mut Criterion) {
    c.bench_function("search", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let args = serde_json::json!({"query": "test"})
                    .as_object()
                    .unwrap()
                    .clone();
                let _ = client.call_tool(CallToolRequestParams::new("search").with_arguments(args)).await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_query_ontology(c: &mut Criterion) {
    c.bench_function("query_ontology", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let args = serde_json::json!({
                    "ttl": VALID_TTL_100,
                    "sparql": SPARQL_SELECT_ALL
                })
                .as_object()
                .unwrap()
                .clone();
                let _ = client
                    .call_tool(CallToolRequestParams::new("query_ontology").with_arguments(args))
                    .await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_get_example(c: &mut Criterion) {
    c.bench_function("get_example", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let args = serde_json::json!({"name": BENCHMARK_EXAMPLE})
                    .as_object()
                    .unwrap()
                    .clone();
                let _ = client
                    .call_tool(CallToolRequestParams::new("get_example").with_arguments(args))
                    .await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_scaffold_from_example(c: &mut Criterion) {
    c.bench_function("scaffold_from_example", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let args = serde_json::json!({
                    "example_name": BENCHMARK_EXAMPLE,
                    "destination": "/tmp/bench_scaffold"
                })
                .as_object()
                .unwrap()
                .clone();
                let _ = client
                    .call_tool(CallToolRequestParams::new("scaffold_from_example").with_arguments(args))
                    .await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_complete(c: &mut Criterion) {
    c.bench_function("complete_example_name", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let args = serde_json::json!({
                    "argument": "example_name",
                    "value": "a2a"
                })
                .as_object()
                .unwrap()
                .clone();
                let _ = client
                    .call_tool(CallToolRequestParams::new("complete").with_arguments(args))
                    .await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_read_resource(c: &mut Criterion) {
    c.bench_function("read_resource", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let uri = format!("ggen://example/{}/ttl", BENCHMARK_EXAMPLE);
                let _ = client.read_resource(&uri).await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_get_prompt(c: &mut Criterion) {
    c.bench_function("get_prompt", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
            if let Ok(client) = start_bench_server().await {
                let args = serde_json::json!({"name": "explain-rdf-schema"})
                    .as_object()
                    .unwrap()
                    .clone();
                let _ = client
                    .call_tool(CallToolRequestParams::new("get_prompt").with_arguments(args))
                    .await;
                let _ = client.cancel().await;
            }
        })
    });
}

criterion_group!(
    benches,
    bench_validate,
    bench_list_generators,
    bench_list_examples,
    bench_get_example,
    bench_list_resources,
    bench_read_resource,
    bench_list_prompts,
    bench_get_prompt,
    bench_search,
    bench_query_ontology,
    bench_scaffold_from_example,
    bench_complete
);
criterion_main!(benches);
