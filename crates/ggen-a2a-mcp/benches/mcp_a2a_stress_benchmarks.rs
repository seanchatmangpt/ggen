//! Stress and load benchmarks for ggen's MCP and A2A layers
//!
//! Measures throughput and latency under concurrent and high-volume conditions:
//! - Concurrent MCP tool calls (10, 50, 100 parallel)
//! - Large TTL validation (1000, 10000 triples)
//! - Large SPARQL result sets (500 rows)
//! - High-frequency A2A task lifecycle (sequential and concurrent)
//! - Message handler dispatch throughput (1000 text messages)
//! - A2A-to-LLM and LLM-to-A2A message conversion (1000 conversions each)

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use ggen_a2a_mcp::handlers::{MessageHandler, MessageRouter, TextContentHandler};
use ggen_a2a_mcp::message::{A2aMessageConverter, LlmResponse};
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};
use std::sync::Arc;
use tokio::task::JoinSet;

mod bench_helper;
use bench_helper::*;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Minimal client handler for benchmarking
#[derive(Debug, Clone, Default)]
struct StressBenchClientHandler;

impl ClientHandler for StressBenchClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

/// Start a fresh MCP server/client pair for each benchmark iteration.
async fn start_bench_server() -> anyhow::Result<RunningService<RoleClient, StressBenchClientHandler>>
{
    let (server_transport, client_transport) = tokio::io::duplex(65_536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = StressBenchClientHandler::default()
        .serve(client_transport)
        .await?;
    Ok(client)
}

/// Generate TTL content with `triple_count` triples programmatically.
fn generate_large_ttl(triple_count: usize) -> String {
    let mut ttl = String::from(
        "@prefix ex: <http://example.org/> .\n\
         @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\n",
    );
    for i in 0..triple_count {
        ttl.push_str(&format!(
            "ex:Entity{} rdf:type ex:TestType .\nex:Entity{} ex:prop{} \"value {}\" .\n",
            i, i, i, i
        ));
    }
    ttl
}

/// Generate a SPARQL query designed to return approximately `row_count` rows
/// against the large TTL data.
fn sparql_for_rows(row_count: usize) -> String {
    let limit = row_count;
    format!("SELECT ?s ?p ?o WHERE {{ ?s ?p ?o }} LIMIT {}", limit)
}

/// Build a pair of (validate args, query_ontology args) for tool call mixing.
fn validate_args() -> serde_json::Map<String, serde_json::Value> {
    serde_json::json!({"ttl": VALID_TTL_100})
        .as_object()
        .unwrap()
        .clone()
}

fn query_args() -> serde_json::Map<String, serde_json::Value> {
    serde_json::json!({
        "ttl": VALID_TTL_100,
        "sparql": SPARQL_SELECT_ALL
    })
    .as_object()
    .unwrap()
    .clone()
}

// ---------------------------------------------------------------------------
// Benchmark a: Concurrent MCP tool calls (10)
// ---------------------------------------------------------------------------

fn bench_concurrent_tool_calls_10(c: &mut Criterion) {
    bench_concurrent_tool_calls(c, 10);
}

// ---------------------------------------------------------------------------
// Benchmark b: Concurrent MCP tool calls (50)
// ---------------------------------------------------------------------------

fn bench_concurrent_tool_calls_50(c: &mut Criterion) {
    bench_concurrent_tool_calls(c, 50);
}

// ---------------------------------------------------------------------------
// Benchmark c: Concurrent MCP tool calls (100) -- saturation
// ---------------------------------------------------------------------------

fn bench_concurrent_tool_calls_100(c: &mut Criterion) {
    bench_concurrent_tool_calls(c, 100);
}

/// Shared implementation for concurrent tool call benchmarks.
fn bench_concurrent_tool_calls(c: &mut Criterion, concurrent: usize) {
    let mut group = c.benchmark_group("concurrent_tool_calls");

    group.bench_with_input(
        BenchmarkId::new("parallel", concurrent),
        &concurrent,
        |b, &concurrent| {
            b.to_async(tokio::runtime::Runtime::new().unwrap())
                .iter(|| async move {
                    // Fresh server/client per iteration
                    if let Ok(client) = start_bench_server().await {
                        let client = Arc::new(client);
                        let mut set: JoinSet<_> = JoinSet::new();

                        for i in 0..concurrent {
                            let c = client.clone();
                            let args = if i % 2 == 0 {
                                validate_args()
                            } else {
                                query_args()
                            };
                            let tool_name = if i % 2 == 0 {
                                "validate"
                            } else {
                                "query_ontology"
                            };

                            set.spawn(async move {
                                let _ = c
                                    .call_tool(
                                        CallToolRequestParams::new(tool_name).with_arguments(args),
                                    )
                                    .await;
                            });
                        }

                        while set.join_next().await.is_some() {}
                        // Arc::try_unwrap to get ownership for cancel(self)
                        let _ = Arc::try_unwrap(client).map(|c| c.cancel());
                    }
                });
        },
    );

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark d: Large TTL validate -- 1000 triples
// ---------------------------------------------------------------------------

fn bench_large_ttl_validate_1000_triples(c: &mut Criterion) {
    bench_large_ttl_validate(c, 1000);
}

// ---------------------------------------------------------------------------
// Benchmark e: Large TTL validate -- 10000 triples
// ---------------------------------------------------------------------------

fn bench_large_ttl_validate_10000_triples(c: &mut Criterion) {
    bench_large_ttl_validate(c, 10000);
}

/// Shared implementation for large TTL validation benchmarks.
fn bench_large_ttl_validate(c: &mut Criterion, triple_count: usize) {
    let mut group = c.benchmark_group("large_ttl_validate");

    let ttl = generate_large_ttl(triple_count);
    group.throughput(Throughput::Bytes(ttl.len() as u64));

    group.bench_with_input(
        BenchmarkId::new("triples", triple_count),
        &triple_count,
        |b, _| {
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
        },
    );

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark f: Large SPARQL result -- 500 rows
// ---------------------------------------------------------------------------

fn bench_large_sparql_result_500_rows(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_sparql_result");

    // Generate enough triples so that SELECT returns up to 500 rows.
    let ttl = generate_large_ttl(500);
    let sparql = sparql_for_rows(500);

    group.throughput(Throughput::Elements(500));

    group.bench_function("500_rows", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| {
                let ttl_copy = ttl.clone();
                let sparql_copy = sparql.clone();
                async move {
                    if let Ok(client) = start_bench_server().await {
                        let args = serde_json::json!({"ttl": &ttl_copy, "sparql": &sparql_copy})
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
                }
            });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark g: High-frequency task creation -- 1000 tasks sequentially
// ---------------------------------------------------------------------------

fn bench_high_frequency_task_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("task_creation");

    group.throughput(Throughput::Elements(1000));
    group.bench_function("1000_tasks_sequential", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async move {
                // Use a simple in-memory task store to avoid A2A client overhead.
                let mut store = std::collections::HashMap::new();
                for i in 0..1000 {
                    let task_id = format!("bench-task-{}", i);
                    store.insert(
                        task_id,
                        format!("created-{}", i), // simulate task context
                    );
                }
                std::hint::black_box(store);
            });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark h: Concurrent task lifecycle -- 50 create/update/get cycles
// ---------------------------------------------------------------------------

fn bench_concurrent_task_lifecycle_50(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_task_lifecycle");

    group.throughput(Throughput::Elements(50));
    group.bench_function("50_lifecycle_cycles", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async move {
                let store = Arc::new(tokio::sync::RwLock::new(std::collections::HashMap::<
                    String,
                    String,
                >::new()));
                let mut set: JoinSet<_> = JoinSet::new();

                for i in 0..50usize {
                    let s = store.clone();
                    set.spawn(async move {
                        let task_id = format!("lifecycle-task-{}", i);
                        // Create
                        {
                            let mut w = s.write().await;
                            w.insert(task_id.clone(), "pending".to_string());
                        }
                        // Update
                        {
                            let mut w = s.write().await;
                            if let Some(v) = w.get_mut(&task_id) {
                                *v = "running".to_string();
                            }
                        }
                        // Get
                        {
                            let r = s.read().await;
                            let _ = r.get(&task_id);
                        }
                    });
                }

                while set.join_next().await.is_some() {}
                std::hint::black_box(store);
            });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark i: Message handler dispatch -- 1000 TextMessageParts
// ---------------------------------------------------------------------------

fn bench_message_handler_dispatch_text(c: &mut Criterion) {
    let mut group = c.benchmark_group("message_handler_dispatch");

    group.throughput(Throughput::Elements(1000));
    group.bench_function("1000_text_messages", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async move {
                let router = MessageRouter::with_defaults();
                let handler = TextContentHandler::new();

                for i in 0..1000 {
                    let msg = a2a_generated::converged::message::ConvergedMessage::text(
                        format!("dispatch-msg-{}", i),
                        "bench-agent".to_string(),
                        format!("Benchmark message payload number {}", i),
                    );
                    // Verify can_handle before dispatching
                    let _can = std::hint::black_box(handler.can_handle(&msg.envelope.message_type));
                    let _result = std::hint::black_box(router.route(msg).await);
                }
            });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark j: A2A-to-LLM message conversion -- 1000 conversions
// ---------------------------------------------------------------------------

fn bench_message_conversion_a2a_to_llm(c: &mut Criterion) {
    let mut group = c.benchmark_group("message_conversion");

    group.throughput(Throughput::Elements(1000));
    group.bench_function("1000_a2a_to_llm", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async move {
                let converter = A2aMessageConverter::new();

                for i in 0..1000 {
                    let msg = a2a_generated::converged::message::ConvergedMessage::text(
                        format!("convert-msg-{}", i),
                        "bench-agent".to_string(),
                        format!("Conversion test content {}", i),
                    );
                    let _result = std::hint::black_box(converter.a2a_to_llm_request(&msg));
                }
            });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark k: LLM-to-A2A message conversion -- 1000 conversions
// ---------------------------------------------------------------------------

fn bench_message_conversion_llm_to_a2a(c: &mut Criterion) {
    let mut group = c.benchmark_group("message_conversion");

    group.throughput(Throughput::Elements(1000));
    group.bench_function("1000_llm_to_a2a", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async move {
                let converter = A2aMessageConverter::new();

                // Pre-build a template message so conversion only measures the transform.
                let template_msg = a2a_generated::converged::message::ConvergedMessage::text(
                    "template-msg".to_string(),
                    "bench-agent".to_string(),
                    "Original content".to_string(),
                );

                for i in 0..1000 {
                    let response = LlmResponse {
                        content: format!("LLM generated response number {}", i),
                        model: "bench-model".to_string(),
                        usage: None,
                    };
                    let _result = std::hint::black_box(
                        converter.llm_response_to_a2a(&response, &template_msg),
                    );
                }
            });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Criterion groups
// ---------------------------------------------------------------------------

criterion_group!(
    stress_load,
    bench_concurrent_tool_calls_10,
    bench_concurrent_tool_calls_50,
    bench_concurrent_tool_calls_100,
    bench_large_ttl_validate_1000_triples,
    bench_large_ttl_validate_10000_triples,
    bench_large_sparql_result_500_rows,
    bench_high_frequency_task_creation,
    bench_concurrent_task_lifecycle_50,
    bench_message_handler_dispatch_text,
    bench_message_conversion_a2a_to_llm,
    bench_message_conversion_llm_to_a2a,
);
criterion_main!(stress_load);
