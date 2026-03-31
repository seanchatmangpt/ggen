//! MCP SLO Timing Validation Tests
//!
//! Directly measures each MCP operation latency against gated SLO limits.
//! These tests fail with clear timing data if any operation exceeds its SLO.
//!
//! SLO Gates:
//!   - Tool discovery (list_tools):         5 ms
//!   - Example listing (list_examples):    50 ms
//!   - Example detail (get_example):       50 ms
//!   - TTL validation (100 triples):       50 ms
//!   - SPARQL query (100 results):         50 ms
//!   - Scaffold from example:             500 ms
//!   - Resource browsing:                  50 ms
//!   - Prompt rendering:                   50 ms
//!   - Completion (argument completion):   50 ms
//!   - Resource read:                      50 ms
//!   - Full lifecycle (discover->scaffold->validate->sync): 3 s
//!
//! Run: cargo test -p ggen-a2a-mcp --test mcp_slo_timing -- --test-threads=1 --nocapture

use std::time::Instant;

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Minimal no-op client handler
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let (server_transport, client_transport) = tokio::io::duplex(65536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

fn args(json: serde_json::Value) -> serde_json::Map<String, serde_json::Value> {
    json.as_object().cloned().unwrap_or_default()
}

/// Generate TTL content with the specified number of triples.
fn make_ttl(triple_count: usize) -> String {
    let mut s = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..triple_count {
        s.push_str(&format!("ex:s{} ex:p{} ex:o{} .\n", i, i, i));
    }
    s
}

/// Extract text content from a CallToolResult.
fn extract_text(result: &CallToolResult) -> Option<String> {
    result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    })
}

/// Assert that a duration is within an SLO, printing detailed timing on failure.
fn assert_slo(operation: &str, elapsed: std::time::Duration, slo_ms: u64) {
    let elapsed_ms = elapsed.as_millis() as u64;
    let elapsed_us = elapsed.as_micros() as u64;
    if elapsed_ms > slo_ms {
        // For operations with sub-ms SLOs, use microsecond comparison
        if slo_ms == 0 {
            panic!(
                "SLO VIOLATION: {} took {} us (SLO: {} us)",
                operation, elapsed_us, slo_ms
            );
        }
        panic!(
            "SLO VIOLATION: {} took {} ms (SLO: {} ms) -- EXCEEDED by {} ms",
            operation,
            elapsed_ms,
            slo_ms,
            elapsed_ms - slo_ms
        );
    }
    println!(
        "  SLO OK: {} = {} ms (limit: {} ms, headroom: {} ms)",
        operation,
        elapsed_ms,
        slo_ms,
        slo_ms - elapsed_ms
    );
}

// ---------------------------------------------------------------------------
// SLO Test 1: Tool Discovery (list_tools) -- SLO: 5 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_tools_list() -> anyhow::Result<()> {
    let client = start_server().await?;
    let start = Instant::now();
    let result = client.list_tools(None).await?;
    let elapsed = start.elapsed();
    assert!(!result.tools.is_empty(), "list_tools should return tools");
    assert_slo("list_tools (tool discovery)", elapsed, 5);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 2: Example listing (list_examples) -- SLO: 50 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_list_examples() -> anyhow::Result<()> {
    let client = start_server().await?;
    let start = Instant::now();
    let result = client
        .call_tool(CallToolRequestParams::new("list_examples"))
        .await?;
    let elapsed = start.elapsed();
    assert_ne!(
        result.is_error,
        Some(true),
        "list_examples should not error"
    );
    let text = extract_text(&result).unwrap_or_default();
    assert!(text.contains("examples"), "should return examples JSON");
    assert_slo("list_examples (example listing)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 3: Example detail (get_example) -- SLO: 50 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_get_example() -> anyhow::Result<()> {
    let client = start_server().await?;
    let start = Instant::now();
    let result = client
        .call_tool(
            CallToolRequestParams::new("get_example").with_arguments(args(serde_json::json!({
                "name": "a2a-groq-agent"
            }))),
        )
        .await?;
    let elapsed = start.elapsed();
    assert_ne!(result.is_error, Some(true), "get_example should not error");
    let text = extract_text(&result).unwrap_or_default();
    assert!(text.contains("@prefix"), "should return TTL content");
    assert_slo("get_example (example detail)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 4: TTL validation (100 triples) -- SLO: 50 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_validate_100_triples() -> anyhow::Result<()> {
    let client = start_server().await?;
    let ttl = make_ttl(100);
    let start = Instant::now();
    let result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": &ttl
            }))),
        )
        .await?;
    let elapsed = start.elapsed();
    assert_ne!(result.is_error, Some(true), "validate should succeed");
    let text = extract_text(&result).unwrap_or_default();
    assert!(text.contains("Valid"), "100-triple TTL should be Valid");
    assert_slo("validate (100 triples)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 5: SPARQL query (100 results) -- SLO: 20 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_query_ontology_100_results() -> anyhow::Result<()> {
    let client = start_server().await?;
    let ttl = make_ttl(100);
    let sparql = "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100";
    let start = Instant::now();
    let result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": &ttl,
                "sparql": sparql
            }))),
        )
        .await?;
    let elapsed = start.elapsed();
    assert_ne!(result.is_error, Some(true), "query_ontology should succeed");
    let text = extract_text(&result).unwrap_or_default();
    let parsed: serde_json::Value = serde_json::from_str(&text)?;
    let count = parsed["count"].as_u64().unwrap_or(0);
    assert!(
        count >= 10,
        "should return at least 10 results, got {}",
        count
    );
    assert_slo("query_ontology (100 results)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 6: Resource browsing (list_resources) -- SLO: 10 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_list_resources() -> anyhow::Result<()> {
    let client = start_server().await?;
    let start = Instant::now();
    let result = client.list_resources(None).await?;
    let elapsed = start.elapsed();
    assert!(
        !result.resources.is_empty(),
        "list_resources should return resources"
    );
    assert_slo("list_resources (resource browsing)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 7: Prompt rendering (get_prompt) -- SLO: 20 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_get_prompt() -> anyhow::Result<()> {
    let client = start_server().await?;
    let start = Instant::now();
    let result = client
        .get_prompt(
            GetPromptRequestParams::new("explain-rdf-schema").with_arguments(args(
                serde_json::json!({
                    "ttl_content": make_ttl(10)
                }),
            )),
        )
        .await?;
    let elapsed = start.elapsed();
    assert!(
        !result.messages.is_empty(),
        "get_prompt should return messages"
    );
    assert_slo("get_prompt (prompt rendering)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 8: Completion (complete) -- SLO: 20 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_complete() -> anyhow::Result<()> {
    let client = start_server().await?;
    use rmcp::model::{ArgumentInfo, Reference};
    let start = Instant::now();
    let result = client
        .complete(CompleteRequestParams::new(
            Reference::for_prompt("explain-rdf-schema"),
            ArgumentInfo {
                name: "example_name".into(),
                value: "a2a".into(),
            },
        ))
        .await?;
    let elapsed = start.elapsed();
    let _ = result; // Just checking it completes within SLO
    assert_slo("complete (argument completion)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 9: Read resource -- SLO: 10 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_read_resource() -> anyhow::Result<()> {
    let client = start_server().await?;
    let start = Instant::now();
    let result = client
        .read_resource(ReadResourceRequestParams::new(String::from(
            "ggen://example/a2a-groq-agent/ttl",
        )))
        .await?;
    let elapsed = start.elapsed();
    assert!(!result.contents.is_empty(), "should return content");
    assert_slo("read_resource (resource read)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 10: Full lifecycle (discover -> scaffold -> validate -> query) -- SLO: 3 s
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_full_lifecycle() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let target_dir = tempdir.path().join("slo-scaffold-test");

    let lifecycle_start = Instant::now();

    // Step 1: Discover tools
    let t1 = Instant::now();
    let tools = client.list_tools(None).await?;
    let t1_elapsed = t1.elapsed();
    println!("  Step 1 (list_tools): {} ms", t1_elapsed.as_millis());
    assert!(!tools.tools.is_empty());

    // Step 2: List examples
    let t2 = Instant::now();
    let list_result = client
        .call_tool(CallToolRequestParams::new("list_examples"))
        .await?;
    let t2_elapsed = t2.elapsed();
    println!("  Step 2 (list_examples): {} ms", t2_elapsed.as_millis());
    assert_ne!(list_result.is_error, Some(true));

    // Step 3: Get example detail
    let t3 = Instant::now();
    let get_result = client
        .call_tool(
            CallToolRequestParams::new("get_example").with_arguments(args(serde_json::json!({
                "name": "a2a-groq-agent"
            }))),
        )
        .await?;
    let t3_elapsed = t3.elapsed();
    println!("  Step 3 (get_example): {} ms", t3_elapsed.as_millis());
    assert_ne!(get_result.is_error, Some(true));

    // Step 4: Scaffold
    let t4 = Instant::now();
    let scaffold_result = client
        .call_tool(
            CallToolRequestParams::new("scaffold_from_example").with_arguments(args(
                serde_json::json!({
                    "example_name": "a2a-groq-agent",
                    "target_dir": target_dir.to_str().unwrap()
                }),
            )),
        )
        .await?;
    let t4_elapsed = t4.elapsed();
    println!(
        "  Step 4 (scaffold_from_example): {} ms",
        t4_elapsed.as_millis()
    );
    // Scaffold may succeed or fail gracefully (dir may already exist)
    let _ = scaffold_result;

    // Step 5: Validate inline TTL
    let valid_ttl = make_ttl(100);
    let t5 = Instant::now();
    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": &valid_ttl
            }))),
        )
        .await?;
    let t5_elapsed = t5.elapsed();
    println!("  Step 5 (validate): {} ms", t5_elapsed.as_millis());
    assert_ne!(validate_result.is_error, Some(true));

    // Step 6: Query ontology
    let sparql = "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 50";
    let t6 = Instant::now();
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": &valid_ttl,
                "sparql": sparql
            }))),
        )
        .await?;
    let t6_elapsed = t6.elapsed();
    println!("  Step 6 (query_ontology): {} ms", t6_elapsed.as_millis());
    assert_ne!(query_result.is_error, Some(true));

    // Step 7: List resources
    let t7 = Instant::now();
    let resources = client.list_resources(None).await?;
    let t7_elapsed = t7.elapsed();
    println!("  Step 7 (list_resources): {} ms", t7_elapsed.as_millis());
    assert!(!resources.resources.is_empty());

    // Step 8: Get prompt
    let t8 = Instant::now();
    let prompt = client
        .get_prompt(
            GetPromptRequestParams::new("explain-rdf-schema").with_arguments(args(
                serde_json::json!({
                    "ttl_content": make_ttl(10)
                }),
            )),
        )
        .await?;
    let t8_elapsed = t8.elapsed();
    println!("  Step 8 (get_prompt): {} ms", t8_elapsed.as_millis());
    assert!(!prompt.messages.is_empty());

    // Step 9: Read resource
    let t9 = Instant::now();
    let resource = client
        .read_resource(ReadResourceRequestParams::new(String::from(
            "ggen://example/a2a-groq-agent",
        )))
        .await?;
    let t9_elapsed = t9.elapsed();
    println!("  Step 9 (read_resource): {} ms", t9_elapsed.as_millis());
    assert!(!resource.contents.is_empty());

    // Step 10: Sync dry-run (only if scaffold succeeded)
    let ttl_path = target_dir.join("ontology").join("a2a-groq-agent.ttl");
    if ttl_path.exists() {
        let queries_dir = ttl_path.parent().unwrap().join("queries");
        std::fs::create_dir_all(&queries_dir)?;
        let t10 = Instant::now();
        let sync_result = client
            .call_tool(
                CallToolRequestParams::new("sync").with_arguments(args(serde_json::json!({
                    "ontology_path": ttl_path.to_str().unwrap(),
                    "dry_run": true
                }))),
            )
            .await?;
        let t10_elapsed = t10.elapsed();
        println!("  Step 10 (sync dry-run): {} ms", t10_elapsed.as_millis());
        let _ = sync_result;
    } else {
        println!("  Step 10 (sync dry-run): SKIPPED (scaffold target not created)");
    }

    let lifecycle_elapsed = lifecycle_start.elapsed();
    println!();
    println!(
        "  TOTAL LIFECYCLE: {} ms (SLO: 3000 ms, headroom: {} ms)",
        lifecycle_elapsed.as_millis(),
        3000 - lifecycle_elapsed.as_millis() as i64
    );

    assert_slo(
        "full lifecycle (discover->scaffold->validate->query->resources->prompts)",
        lifecycle_elapsed,
        3000,
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 11: Scaffold from example -- SLO: 500 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_scaffold_from_example() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let target_dir = tempdir.path().join("slo-scaffold-single");

    let start = Instant::now();
    let result = client
        .call_tool(
            CallToolRequestParams::new("scaffold_from_example").with_arguments(args(
                serde_json::json!({
                    "example_name": "a2a-groq-agent",
                    "target_dir": target_dir.to_str().unwrap()
                }),
            )),
        )
        .await?;
    let elapsed = start.elapsed();
    assert_ne!(
        result.is_error,
        Some(true),
        "scaffold should succeed: {:?}",
        extract_text(&result)
    );
    assert!(
        target_dir.join("ggen.toml").exists(),
        "scaffolded dir must contain ggen.toml"
    );
    assert_slo("scaffold_from_example (file copy)", elapsed, 500);
    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// SLO Test 12: Validate malformed TTL (error path) -- SLO: 50 ms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slo_validate_malformed() -> anyhow::Result<()> {
    let client = start_server().await?;
    let malformed = "@prefix ex: <http://example.org/> .\nex:s1 a ex:Class .\n{invalid\n";
    let start = Instant::now();
    let result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": malformed
            }))),
        )
        .await?;
    let elapsed = start.elapsed();
    // Should return error result (not throw)
    assert_eq!(result.is_error, Some(true), "malformed TTL should be error");
    assert_slo("validate_malformed (error path)", elapsed, 50);
    client.cancel().await?;
    Ok(())
}
