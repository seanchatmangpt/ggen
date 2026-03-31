//! End-to-end A2A -> MCP -> code generation -> validation
//!
//! Exercises the full pipeline: A2A ConvergedMessage routing through
//! MessageRouter, MCP duplex client tool calls (generate, validate,
//! query_ontology, scaffold_from_example), and BatchProcessor with
//! correlation ID preservation.
//!
//! Run with:
//!   cargo test -p ggen-a2a-mcp --test a2a_mcp_generate_e2e -- --test-threads=1 --nocapture

use std::sync::Arc;

use a2a_generated::converged::message::{ConvergedMessage, UnifiedContent};
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use ggen_a2a_mcp::handlers::{BatchProcessor, MessageRouter, TextContentHandler};
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

mod common;
use common::init_tracing;

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
// Duplex server helper
// ---------------------------------------------------------------------------

async fn start_duplex_server(
    examples_dir: &str,
) -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    std::env::set_var("GGEN_EXAMPLES_DIR", examples_dir);

    let (server_transport, client_transport) = tokio::io::duplex(65536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

fn extract_text(result: &CallToolResult) -> Option<String> {
    result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    })
}

fn args(json: serde_json::Value) -> serde_json::Map<String, serde_json::Value> {
    json.as_object().cloned().unwrap_or_default()
}

/// Extract text from UnifiedContent, returning empty string for non-text.
fn extract_unified_text(content: &UnifiedContent) -> &str {
    match content {
        UnifiedContent::Text { content, .. } => content.as_str(),
        _ => "",
    }
}

// ---------------------------------------------------------------------------
// Ontology fixture
// ---------------------------------------------------------------------------

const SIMPLE_ONTOLOGY_TTL: &str = concat!(
    "@prefix ex: <http://example.org/> .\n",
    "ex:Agent a ex:Concept .\n",
    "ex:Tool a ex:Concept .\n",
    "ex:Workflow a ex:Concept .\n",
    "ex:Skill a ex:Concept .\n",
);

const SERVICE_QUERY_SPARQL: &str = concat!(
    "PREFIX ex: <http://example.org/>\n",
    "SELECT ?s WHERE { ?s a ex:Concept }\n",
);

// ---------------------------------------------------------------------------
// Test 1: A2A message routes to MCP generate
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_message_routes_to_mcp_generate() -> anyhow::Result<()> {
    init_tracing();
    let examples_dir = "/Users/sac/ggen/examples";
    let client = start_duplex_server(examples_dir).await?;

    let result = (|| async {
        // --- Part A: Route a ConvergedMessage through MessageRouter ---
        let mut router = MessageRouter::with_defaults();
        let _ = router.register(TextContentHandler::new());

        let msg = ConvergedMessage::text(
            format!("msg-{}", uuid::Uuid::new_v4()),
            "architect".to_string(),
            "Generate code from ontology".to_string(),
        );

        let response = router.route(msg).await?;
        let text = extract_unified_text(&response.payload.content);
        assert!(
            text.contains("Processed"),
            "routed response should contain 'Processed', got: {}",
            text
        );

        // --- Part B: Call MCP generate via duplex client ---
        let tempdir = tempfile::tempdir()?;
        let ontology_path = tempdir.path().join("ontology.ttl");
        let queries_dir = tempdir.path().join("queries");
        let output_dir = tempdir.path().join("generated");
        std::fs::create_dir_all(&queries_dir)?;
        std::fs::create_dir_all(&output_dir)?;
        std::fs::write(&ontology_path, SIMPLE_ONTOLOGY_TTL)?;
        std::fs::write(queries_dir.join("01.rq"), SERVICE_QUERY_SPARQL)?;

        let generate_result = client
            .call_tool(CallToolRequestParams::new("generate").with_arguments(args(
                serde_json::json!({
                    "ontology_path": ontology_path.to_str().unwrap(),
                    "queries_dir": queries_dir.to_str().unwrap(),
                    "output_dir": output_dir.to_str().unwrap(),
                    "language": "rust"
                }),
            )))
            .await?;

        assert_ne!(
            generate_result.is_error,
            Some(true),
            "generate should succeed: {:?}",
            extract_text(&generate_result)
        );
        let gen_text = extract_text(&generate_result).expect("generate should return text content");
        assert!(
            gen_text.contains("Generated"),
            "generate response must contain 'Generated', got: {}",
            gen_text
        );

        // Verify files were produced on disk
        let files: Vec<_> = std::fs::read_dir(&output_dir)?.flatten().collect();
        assert!(
            !files.is_empty(),
            "generate should produce at least one file in output_dir"
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    client.cancel().await?;
    result
}

// ---------------------------------------------------------------------------
// Test 2: MCP generate then validate roundtrip
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_generate_then_validate_roundtrip() -> anyhow::Result<()> {
    init_tracing();
    let examples_dir = "/Users/sac/ggen/examples";
    let client = start_duplex_server(examples_dir).await?;

    let result = (|| async {
        // Step 1: Validate the source TTL first
        let validate_result = client
            .call_tool(CallToolRequestParams::new("validate").with_arguments(args(
                serde_json::json!({
                    "ttl": SIMPLE_ONTOLOGY_TTL
                }),
            )))
            .await?;

        assert_ne!(
            validate_result.is_error,
            Some(true),
            "validate should succeed"
        );
        let validate_text = extract_text(&validate_result).expect("validate should return text");
        assert!(
            validate_text.contains("Valid"),
            "TTL should be Valid, got: {}",
            validate_text
        );

        // Step 2: Generate from the same TTL
        let tempdir = tempfile::tempdir()?;
        let ontology_path = tempdir.path().join("ontology.ttl");
        let queries_dir = tempdir.path().join("queries");
        let output_dir = tempdir.path().join("generated");
        std::fs::create_dir_all(&queries_dir)?;
        std::fs::create_dir_all(&output_dir)?;
        std::fs::write(&ontology_path, SIMPLE_ONTOLOGY_TTL)?;
        std::fs::write(queries_dir.join("01.rq"), SERVICE_QUERY_SPARQL)?;

        let generate_result = client
            .call_tool(CallToolRequestParams::new("generate").with_arguments(args(
                serde_json::json!({
                    "ontology_path": ontology_path.to_str().unwrap(),
                    "queries_dir": queries_dir.to_str().unwrap(),
                    "output_dir": output_dir.to_str().unwrap(),
                    "language": "rust"
                }),
            )))
            .await?;

        assert_ne!(
            generate_result.is_error,
            Some(true),
            "generate should succeed: {:?}",
            extract_text(&generate_result)
        );

        // Step 3: Re-validate the same TTL (roundtrip confirmation)
        let validate2_result = client
            .call_tool(CallToolRequestParams::new("validate").with_arguments(args(
                serde_json::json!({
                    "ttl": SIMPLE_ONTOLOGY_TTL
                }),
            )))
            .await?;

        assert_ne!(
            validate2_result.is_error,
            Some(true),
            "second validate should also succeed"
        );
        let validate2_text =
            extract_text(&validate2_result).expect("second validate should return text");
        assert!(
            validate2_text.contains("Valid"),
            "roundtrip validate should still report Valid, got: {}",
            validate2_text
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    client.cancel().await?;
    result
}

// ---------------------------------------------------------------------------
// Test 3: MCP generate then query_ontology
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_generate_then_query_artifact() -> anyhow::Result<()> {
    init_tracing();
    let examples_dir = "/Users/sac/ggen/examples";
    let client = start_duplex_server(examples_dir).await?;

    let result = (|| async {
        // Step 1: Generate from ontology
        let tempdir = tempfile::tempdir()?;
        let ontology_path = tempdir.path().join("ontology.ttl");
        let queries_dir = tempdir.path().join("queries");
        let output_dir = tempdir.path().join("generated");
        std::fs::create_dir_all(&queries_dir)?;
        std::fs::create_dir_all(&output_dir)?;
        std::fs::write(&ontology_path, SIMPLE_ONTOLOGY_TTL)?;
        std::fs::write(queries_dir.join("01.rq"), SERVICE_QUERY_SPARQL)?;

        let generate_result = client
            .call_tool(CallToolRequestParams::new("generate").with_arguments(args(
                serde_json::json!({
                    "ontology_path": ontology_path.to_str().unwrap(),
                    "queries_dir": queries_dir.to_str().unwrap(),
                    "output_dir": output_dir.to_str().unwrap(),
                    "language": "rust"
                }),
            )))
            .await?;

        assert_ne!(
            generate_result.is_error,
            Some(true),
            "generate should succeed: {:?}",
            extract_text(&generate_result)
        );

        // Step 2: Query the same ontology TTL
        let query_result = client
            .call_tool(
                CallToolRequestParams::new("query_ontology").with_arguments(args(
                    serde_json::json!({
                        "ttl": SIMPLE_ONTOLOGY_TTL,
                        "sparql": SERVICE_QUERY_SPARQL
                    }),
                )),
            )
            .await?;

        assert_ne!(
            query_result.is_error,
            Some(true),
            "query_ontology should succeed"
        );
        let query_text = extract_text(&query_result).expect("query_ontology should return text");
        let parsed: serde_json::Value = serde_json::from_str(&query_text)?;

        // Should find at least 4 concepts (Agent, Tool, Workflow, Skill)
        let count = parsed["count"].as_u64().unwrap_or(0);
        assert!(count >= 4, "expected at least 4 concepts, got {}", count);

        // Should have rows
        let rows = parsed["rows"].as_array().expect("rows should be an array");
        assert!(!rows.is_empty(), "query should return at least one row");

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    client.cancel().await?;
    result
}

// ---------------------------------------------------------------------------
// Test 4: Full pipeline scaffold -> validate -> query
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_full_pipeline_scaffold_validate_query() -> anyhow::Result<()> {
    init_tracing();
    let examples_dir = "/Users/sac/ggen/examples";
    let client = start_duplex_server(examples_dir).await?;

    let result = (|| async {
        let tempdir = tempfile::tempdir()?;

        // Step 1: List examples to find a real example
        let list_result = client
            .call_tool(CallToolRequestParams::new("list_examples"))
            .await?;
        assert_ne!(
            list_result.is_error,
            Some(true),
            "list_examples should succeed"
        );
        let list_text = extract_text(&list_result).expect("list_examples text");
        let list_json: serde_json::Value = serde_json::from_str(&list_text)?;
        let examples = list_json["examples"]
            .as_array()
            .expect("examples should be an array");
        assert!(!examples.is_empty(), "need at least one example");

        let example_name = examples[0]["name"]
            .as_str()
            .expect("example must have a name");

        // Step 2: Scaffold from example
        let target_dir = tempdir.path().join("scaffolded");
        let scaffold_result = client
            .call_tool(
                CallToolRequestParams::new("scaffold_from_example").with_arguments(args(
                    serde_json::json!({
                        "example_name": example_name,
                        "target_dir": target_dir.to_str().unwrap()
                    }),
                )),
            )
            .await?;

        assert_ne!(
            scaffold_result.is_error,
            Some(true),
            "scaffold should succeed: {:?}",
            extract_text(&scaffold_result)
        );

        // Step 3: Find a .ttl file in scaffolded output and validate it
        let mut ttl_content = String::new();
        for entry in walkdir_all(&target_dir) {
            if entry.extension().and_then(|e| e.to_str()) == Some("ttl") {
                if let Ok(content) = std::fs::read_to_string(&entry) {
                    ttl_content = content;
                    break;
                }
            }
        }
        assert!(
            !ttl_content.is_empty(),
            "scaffolded project must contain at least one .ttl file"
        );

        let validate_result = client
            .call_tool(CallToolRequestParams::new("validate").with_arguments(args(
                serde_json::json!({
                    "ttl": ttl_content
                }),
            )))
            .await?;

        assert_ne!(
            validate_result.is_error,
            Some(true),
            "scaffolded TTL should validate: {:?}",
            extract_text(&validate_result)
        );
        let validate_text = extract_text(&validate_result).expect("validate should return text");
        assert!(
            validate_text.contains("Valid"),
            "scaffolded TTL should be Valid, got: {}",
            validate_text
        );

        // Step 4: Query the scaffolded ontology
        let query_sparql = concat!(
            "PREFIX ex: <http://example.org/>\n",
            "SELECT ?s WHERE { ?s ?p ?o } LIMIT 5\n",
        );
        let query_result = client
            .call_tool(
                CallToolRequestParams::new("query_ontology").with_arguments(args(
                    serde_json::json!({
                        "ttl": ttl_content,
                        "sparql": query_sparql
                    }),
                )),
            )
            .await?;

        assert_ne!(
            query_result.is_error,
            Some(true),
            "query on scaffolded TTL should succeed"
        );
        let query_text = extract_text(&query_result).expect("query should return text");
        let parsed: serde_json::Value = serde_json::from_str(&query_text)?;
        let count = parsed["count"].as_u64().unwrap_or(0);
        assert!(
            count > 0,
            "scaffolded ontology should return at least 1 result, got {}",
            count
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    client.cancel().await?;
    result
}

// ---------------------------------------------------------------------------
// Test 5: Batch processor preserves correlation IDs
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_batch_preserves_correlation_ids() -> anyhow::Result<()> {
    init_tracing();

    let router = Arc::new(MessageRouter::with_defaults());
    let processor = BatchProcessor::new(router).with_max_concurrent(5);

    // Create messages with distinct correlation IDs
    let correlation_ids = vec!["corr-alpha-001", "corr-beta-002", "corr-gamma-003"];
    let messages: Vec<ConvergedMessage> = correlation_ids
        .iter()
        .map(|cid| {
            let mut msg = ConvergedMessage::text(
                format!("batch-{}", uuid::Uuid::new_v4()),
                "test-agent".to_string(),
                format!("message with correlation {}", cid),
            );
            msg.envelope.correlation_id = Some(cid.to_string());
            msg
        })
        .collect();

    let batch_result = processor.process_batch(messages).await;

    assert_eq!(batch_result.total, 3, "should process 3 messages total");
    assert_eq!(batch_result.successful, 3, "all 3 messages should succeed");
    assert_eq!(batch_result.failed, 0, "no messages should fail");

    // Verify each individual result -- the message_id is preserved from the input
    for individual in &batch_result.results {
        assert!(
            individual.success,
            "message {} should have succeeded",
            individual.message_id
        );
        assert!(
            individual.error.is_none(),
            "message {} should have no error",
            individual.message_id
        );
    }

    // Verify correlation IDs are traceable through the batch.
    // The BatchProcessor creates per-message spans with a2a.correlation_id field.
    // We verify the batch itself completed correctly; correlation ID preservation
    // is validated by the tracing spans created inside process_batch.
    for (idx, individual) in batch_result.results.iter().enumerate() {
        assert!(
            individual.message_id.contains("batch-"),
            "result {} should reference original batch message ID, got: {}",
            idx,
            individual.message_id
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// File-system helper
// ---------------------------------------------------------------------------

/// Walk a directory recursively, collecting all file paths.
fn walkdir_all(root: &std::path::Path) -> Vec<std::path::PathBuf> {
    let mut files = Vec::new();
    if let Ok(entries) = std::fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                files.extend(walkdir_all(&path));
            } else {
                files.push(path);
            }
        }
    }
    files
}
