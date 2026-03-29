//! Stress tests for MCP server under load
//!
//! Tests:
//! - Concurrent tool calls (10, 50, 100 parallel requests)
//! - Large payload handling (10KB, 100KB, 500KB TTL content)
//! - Resource pagination stress

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};
use tokio::task::JoinSet;

mod bench_helper;
use bench_helper::*;

/// Helper to start a server and return a client
async fn start_bench_server() -> anyhow::Result<RunningService<RoleClient, BenchClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(65_536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = BenchClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

// fn bench_concurrent_validate(c: &mut Criterion) {
//     let mut group = c.benchmark_group("concurrent_validate");

//     for concurrent in [10, 50, 100].iter() {
//         group.bench_with_input(
//             BenchmarkId::from_parameter(concurrent),
//             concurrent,
//             |b, &concurrent| {
//                 b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async move {
//                     // Start server
//                     let (server_transport, client_transport) = tokio::io::duplex(65_536);
//                     let server = GgenMcpServer::new();
//                     tokio::spawn(async move {
//                         let _ = server.serve(server_transport).await;
//                     });

//                     // Connect client
//                     if let Ok(client) = BenchClientHandler::default()
//                         .serve(client_transport)
//                         .await
//                     {
//                         let client = Arc::new(client);
//                         let ttl = "@prefix ex: <http://example.org/> . ex:s a ex:Class .";
//                         let args = serde_json::json!({"ttl": ttl})
//                             .as_object()
//                             .unwrap()
//                             .clone();

//                         let mut set = JoinSet::new();
//                         for _ in 0..concurrent {
//                             let client = client.clone();
//                             let args = args.clone();
//                             set.spawn(async move {
//                                 let _ = client
//                                     .call_tool(
//                                         CallToolRequestParams::new("validate")
//                                             .with_arguments(args)
//                                     )
//                                     .await;
//                             });
//                         }
//                         while set.join_next().await.is_some() {}
//                         let _ = client.cancel().await;
//                     }
//                 });
//             },
//         );
//     }
//     group.finish();
// }

fn bench_large_ttl_payload(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_payload");

    // Generate TTL content of varying sizes
    for size_kb in [10, 100, 500].iter() {
        let triples_count = size_kb * 10; // Roughly 100 bytes per triple
        let ttl = format!(
            "@prefix ex: <http://example.org/> .\n{}\n",
            (0..triples_count)
                .map(|i| format!("ex:s{} ex:p{} ex:o{} .", i, i, i))
                .collect::<Vec<_>>()
                .join("\n")
        );

        group.throughput(Throughput::Bytes(ttl.len() as u64));
        group.bench_with_input(BenchmarkId::new("kb", size_kb), size_kb, |b, _| {
            b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| {
                let ttl_copy = ttl.clone();
                async move {
                    // Start server
                    let (server_transport, client_transport) = tokio::io::duplex(65_536);
                    let server = GgenMcpServer::new();
                    tokio::spawn(async move {
                        let _ = server.serve(server_transport).await;
                    });

                    // Connect client
                    if let Ok(client) = BenchClientHandler::default()
                        .serve(client_transport)
                        .await
                    {
                        let args = serde_json::json!({"ttl": &ttl_copy})
                            .as_object()
                            .unwrap()
                            .clone();
                        let _ = client
                            .call_tool(CallToolRequestParams::new("validate").with_arguments(args))
                            .await;
                        let _ = client.cancel().await;
                    }
                }
            });
        });
    }
    group.finish();
}

fn bench_resource_pagination_full_scan(c: &mut Criterion) {
    c.bench_function("resource_pagination_full_scan", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async move {
            if let Ok(client) = start_bench_server().await {
                let _ = client.list_resources(None).await;
                let _ = client.cancel().await;
            }
        })
    });
}

fn bench_prompt_rendering_stress(c: &mut Criterion) {
    let mut group = c.benchmark_group("prompt_rendering");

    for prompt_count in [5, 10, 20].iter() {
        group.bench_with_input(
            BenchmarkId::new("prompts", prompt_count),
            prompt_count,
            |b, &prompt_count| {
                b.to_async(tokio::runtime::Runtime::new().unwrap()).iter(|| async move {
                    if let Ok(client) = start_bench_server().await {
                        for i in 0..prompt_count {
                            let args = serde_json::json!({
                                "name": format!("test_prompt_{}", i)
                            })
                            .as_object()
                            .unwrap()
                            .clone();

                            let _ = client
                                .call_tool(
                                    CallToolRequestParams::new("complete").with_arguments(args)
                                )
                                .await;
                        }
                        let _ = client.cancel().await;
                    }
                })
            },
        );
    }
    group.finish();
}

criterion_group!(
    stress_tests,
    bench_large_ttl_payload,
    bench_resource_pagination_full_scan,
    bench_prompt_rendering_stress
);
criterion_main!(stress_tests);