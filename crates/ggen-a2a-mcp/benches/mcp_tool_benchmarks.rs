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

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};
use std::hint::black_box;

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

    let client = BenchClientHandler::default()
        .serve(client_transport)
        .await?;
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
            b.to_async(tokio::runtime::Runtime::new().unwrap())
                .iter(|| {
                    let ttl_copy = ttl.clone();
                    async move {
                        if let Ok(client) = start_bench_server().await {
                            let args = serde_json::json!({"ttl": &ttl_copy})
                                .as_object()
                                .unwrap()
                                .clone();
                            let _ = client
                                .call_tool(
                                    CallToolRequestParams::new("validate").with_arguments(args),
                                )
                                .await;
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
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let _ = client
                        .call_tool(CallToolRequestParams::new("list_generators"))
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

fn bench_list_examples(c: &mut Criterion) {
    c.bench_function("list_examples", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let _ = client
                        .call_tool(CallToolRequestParams::new("list_examples"))
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

fn bench_list_resources(c: &mut Criterion) {
    c.bench_function("list_resources", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let _ = client.list_resources(None).await;
                    let _ = client.cancel().await;
                }
            })
    });
}

fn bench_list_prompts(c: &mut Criterion) {
    c.bench_function("list_prompts", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let _ = client.list_prompts(None).await;
                    let _ = client.cancel().await;
                }
            })
    });
}

fn bench_search(c: &mut Criterion) {
    c.bench_function("search", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let args = serde_json::json!({"query": "test"})
                        .as_object()
                        .unwrap()
                        .clone();
                    let _ = client
                        .call_tool(CallToolRequestParams::new("search").with_arguments(args))
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

fn bench_query_ontology(c: &mut Criterion) {
    c.bench_function("query_ontology", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let args = serde_json::json!({
                        "ttl": VALID_TTL_100,
                        "sparql": SPARQL_SELECT_ALL
                    })
                    .as_object()
                    .unwrap()
                    .clone();
                    let _ = client
                        .call_tool(
                            CallToolRequestParams::new("query_ontology").with_arguments(args),
                        )
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

fn bench_get_example(c: &mut Criterion) {
    c.bench_function("get_example", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
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
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let args = serde_json::json!({
                        "example_name": BENCHMARK_EXAMPLE,
                        "target_dir": "/tmp/bench_scaffold"
                    })
                    .as_object()
                    .unwrap()
                    .clone();
                    let _ = client
                        .call_tool(
                            CallToolRequestParams::new("scaffold_from_example")
                                .with_arguments(args),
                        )
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

fn bench_complete(c: &mut Criterion) {
    c.bench_function("complete_example_name", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    use rmcp::model::{ArgumentInfo, Reference};
                    let params = CompleteRequestParams::new(
                        Reference::for_prompt("explain-rdf-schema"),
                        ArgumentInfo {
                            name: "example_name".into(),
                            value: "a2a".into(),
                        },
                    );
                    let _ = client.complete(params).await;
                    let _ = client.cancel().await;
                }
            })
    });
}

// fn bench_read_resource(c: &mut Criterion) {
//     c.bench_function("read_resource", |b| {
//         b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async {
//             if let Ok(client) = start_bench_server().await {
//                 let uri = format!("ggen://example/{}/ttl", BENCHMARK_EXAMPLE);
//                 let _ = client.read_resource(None).await;
//                 let _ = client.cancel().await;
//             }
//         })
//     });
// }

fn bench_get_prompt(c: &mut Criterion) {
    c.bench_function("get_prompt", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let _ = client
                        .get_prompt(GetPromptRequestParams::new("explain-rdf-schema"))
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

// ---------------------------------------------------------------------------
// Additional benchmarks — comprehensive MCP capability coverage
// ---------------------------------------------------------------------------

/// MCP `tools/list` protocol call — measures tool discovery latency.
/// Unlike existing benchmarks which call individual tools, this exercises
/// the MCP protocol-level tool listing that clients use on connect.
fn bench_tools_list(c: &mut Criterion) {
    c.bench_function("tools_list", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let _ = client.list_tools(None).await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// MCP `resources/read` call — reads a specific resource URI.
/// The existing `bench_list_resources` only lists; this reads actual content.
fn bench_read_resource(c: &mut Criterion) {
    c.bench_function("read_resource", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let uri = format!("ggen://example/{}/ttl", BENCHMARK_EXAMPLE);
                    let params = ReadResourceRequestParams::new(uri);
                    let _ = client.read_resource(params).await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// MCP `prompts/get` with arguments — exercises prompt rendering with real args.
/// The existing `bench_get_prompt` calls `explain-rdf-schema` with no args;
/// this version passes `ttl_content` to test template rendering.
fn bench_get_prompt_with_args(c: &mut Criterion) {
    c.bench_function("get_prompt_with_args", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let args = serde_json::json!({"ttl_content": VALID_TTL_100});
                    let params = GetPromptRequestParams::new("explain-rdf-schema")
                        .with_arguments(args.as_object().unwrap().clone());
                    let _ = client.get_prompt(params).await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// MCP `completion/complete` for the `generator` argument — exercises
/// completion against the static GENERATORS list.
/// The existing `bench_complete` tests `example_name`; this covers the other
/// completion branch.
fn bench_complete_generator(c: &mut Criterion) {
    c.bench_function("complete_generator", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    use rmcp::model::{ArgumentInfo, Reference};
                    let params = CompleteRequestParams::new(
                        Reference::for_prompt("generate-from-example"),
                        ArgumentInfo {
                            name: "generator".into(),
                            value: "ru".into(),
                        },
                    );
                    let _ = client.complete(params).await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// `search` tool with category filter — exercises the marketplace category path.
/// The existing `bench_search` uses no category filter.
fn bench_search_with_category(c: &mut Criterion) {
    c.bench_function("search_with_category", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let args = serde_json::json!({"query": "class", "category": "ontology"})
                        .as_object()
                        .unwrap()
                        .clone();
                    let _ = client
                        .call_tool(CallToolRequestParams::new("search").with_arguments(args))
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// `list_examples` tool with category filter — exercises filesystem scan + filter.
/// The existing `bench_list_examples` has no filter.
fn bench_list_examples_filtered(c: &mut Criterion) {
    c.bench_function("list_examples_filtered", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let args = serde_json::json!({"category": "ontology"})
                        .as_object()
                        .unwrap()
                        .clone();
                    let _ = client
                        .call_tool(CallToolRequestParams::new("list_examples").with_arguments(args))
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// `query_ontology` with large TTL input — measures SPARQL performance at scale.
/// The existing `bench_query_ontology` uses VALID_TTL_100 (10 triples).
fn bench_query_ontology_large(c: &mut Criterion) {
    c.bench_function("query_ontology_large", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let large_ttl = black_box(format!(
                        "@prefix ex: <http://example.org/> .\n{}\n",
                        (0..500)
                            .map(|i| format!(
                                "ex:s{} a ex:Class{} ; ex:prop{} ex:val{} .",
                                i, i, i, i
                            ))
                            .collect::<Vec<_>>()
                            .join("\n")
                    ));
                    let sparql =
                        black_box("SELECT ?s ?p ?o WHERE { ?s a ?o } LIMIT 50".to_string());
                    let args = serde_json::json!({"ttl": &large_ttl, "sparql": &sparql})
                        .as_object()
                        .unwrap()
                        .clone();
                    let _ = client
                        .call_tool(
                            CallToolRequestParams::new("query_ontology").with_arguments(args),
                        )
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// `validate` tool with malformed TTL — measures error-path latency.
/// The existing `bench_validate` only tests valid inputs.
fn bench_validate_malformed(c: &mut Criterion) {
    c.bench_function("validate_malformed", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let malformed = black_box(
                        "@prefix ex: <http://example.org/> .\nex:s1 a ex:Class .\n{invalid\n",
                    );
                    let args = serde_json::json!({"ttl": malformed})
                        .as_object()
                        .unwrap()
                        .clone();
                    let _ = client
                        .call_tool(CallToolRequestParams::new("validate").with_arguments(args))
                        .await;
                    let _ = client.cancel().await;
                }
            })
    });
}

/// `scaffold_from_example` with unique temp dir per iteration — avoids
/// "target directory already exists" errors by using a fresh dir each time.
/// The existing `bench_scaffold_from_example` reuses `/tmp/bench_scaffold`
/// which fails after the first iteration.
fn bench_scaffold_unique_dir(c: &mut Criterion) {
    c.bench_function("scaffold_unique_dir", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let dir = format!(
                        "/tmp/bench_scaffold_{}",
                        std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap()
                            .as_nanos()
                    );
                    let args = serde_json::json!({
                        "example_name": BENCHMARK_EXAMPLE,
                        "target_dir": &dir
                    })
                    .as_object()
                    .unwrap()
                    .clone();
                    let _ = client
                        .call_tool(
                            CallToolRequestParams::new("scaffold_from_example")
                                .with_arguments(args),
                        )
                        .await;
                    // Clean up
                    let _ = std::fs::remove_dir_all(&dir);
                    let _ = client.cancel().await;
                }
            })
    });
}

/// Concurrent `call_tool` calls — measures parallel throughput by firing
/// multiple tool calls simultaneously using tokio::join!.
fn bench_concurrent_tools(c: &mut Criterion) {
    c.bench_function("concurrent_tools", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                if let Ok(client) = start_bench_server().await {
                    let c1 = client.list_tools(None);
                    let c2 = client.list_resources(None);
                    let c3 = client.list_prompts(None);
                    let _ = tokio::join!(c1, c2, c3);
                    let _ = client.cancel().await;
                }
            })
    });
}

// ---------------------------------------------------------------------------
// Criterion groups — original + extended
// ---------------------------------------------------------------------------

criterion_group!(
    benches,
    // Original benchmarks
    bench_validate,
    bench_list_generators,
    bench_list_examples,
    bench_get_example,
    bench_list_resources,
    bench_list_prompts,
    bench_get_prompt,
    bench_search,
    bench_query_ontology,
    bench_scaffold_from_example,
    bench_complete,
    // Extended benchmarks — comprehensive MCP capability coverage
    bench_tools_list,
    bench_read_resource,
    bench_get_prompt_with_args,
    bench_complete_generator,
    bench_search_with_category,
    bench_list_examples_filtered,
    bench_query_ontology_large,
    bench_validate_malformed,
    bench_scaffold_unique_dir,
    bench_concurrent_tools,
);
criterion_main!(benches);
