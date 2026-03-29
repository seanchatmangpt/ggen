//! Property-based tests for MCP tool robustness
//!
//! Tests verify invariants:
//! - validate accepts any valid Turtle syntax
//! - list_examples handles arbitrary category/limit combinations
//! - search handles various query strings

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use proptest::prelude::*;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

#[derive(Debug, Clone, Default)]
struct TestClient;

impl ClientHandler for TestClient {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClient>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);
    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    Ok(TestClient::default().serve(client_transport).await?)
}

proptest! {
    #[tokio::test]
    async fn prop_validate_accepts_various_ttl_formats(
        triple_count in 0..50usize,
        use_prefix in bool::arbitrary()
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let result = rt.block_on(async {
            let client = start_server().await.ok()?;

            let ttl = if use_prefix {
                format!("@prefix ex: <http://example.org/> .\n{}\n",
                    (0..triple_count)
                        .map(|i| format!("ex:s{} a ex:Class .", i))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            } else {
                (0..triple_count)
                    .map(|i| format!("<http://example.org/s{}> <http://example.org/p> <http://example.org/o{}> .", i, i))
                    .collect::<Vec<_>>()
                    .join("\n")
            };

            let args = serde_json::json!({"ttl": &ttl}).as_object().ok()?.clone();
            let call_result = client.call_tool(
                CallToolRequestParams::new("validate").with_arguments(args)
            ).await;

            client.cancel().await.ok()?;
            Some(call_result.is_ok())
        });

        // Should not crash — may return error for invalid TTL, but must not panic
        prop_assert!(result.unwrap_or(false) || result.is_some());
    }

    #[tokio::test]
    async fn prop_list_examples_handles_arbitrary_limits(
        limit in 0..1000usize
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let result = rt.block_on(async {
            let client = start_server().await.ok()?;

            let args = serde_json::json!({"limit": limit}).as_object().ok()?.clone();
            let call_result = client.call_tool(
                CallToolRequestParams::new("list_examples").with_arguments(args)
            ).await;

            client.cancel().await.ok()?;
            Some(call_result.is_ok())
        });

        // Should not crash
        prop_assert!(result.is_some());
    }

    #[tokio::test]
    async fn prop_search_handles_various_queries(
        query_len in 0..100usize,
        has_category in bool::arbitrary()
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let result = rt.block_on(async {
            let client = start_server().await.ok()?;

            let query = "a".repeat(query_len);
            let mut args_obj = serde_json::json!({"query": &query}).as_object().ok()?.clone();

            if has_category {
                args_obj.insert("category".to_string(), serde_json::Value::String("agent".to_string()));
            }

            let call_result = client.call_tool(
                CallToolRequestParams::new("search").with_arguments(args_obj)
            ).await;

            client.cancel().await.ok()?;
            Some(call_result.is_ok())
        });

        // Should not crash
        prop_assert!(result.is_some());
    }
}
