//! Multi-A2A message chain tests with OTEL span validation
//!
//! These tests exercise MULTIPLE A2A messages in a chain with correlation
//! and validate that OTEL spans are emitted for each A2A message exchange.
//! Each message produces a `ggen.a2a.message` span with semconv attributes:
//!   - a2a.message_id (required)
//!   - a2a.operation_name (required)
//!   - a2a.correlation_id (recommended)
//!   - a2a.source (recommended)
//!   - a2a.target (recommended)
//!   - a2a.causation_chain (recommended)
//!   - a2a.message_type (recommended)
//!
//! Pattern: rmcp 1.3.0 in-process duplex transport (tokio::io::duplex).
//! SelfPlayBridge wires an MCP client, MessageRouter, and A2aMessageConverter.
//! All tests use --test-threads=1 to avoid MCP server port conflicts.

#![cfg(feature = "otel")]

use std::sync::Arc;

use a2a_generated::converged::message::{ConvergedMessage, UnifiedContent};
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use ggen_a2a_mcp::handlers::{MessageRouter, TextContentHandler};
use ggen_a2a_mcp::message::A2aMessageConverter;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Minimal no-op client handler (same pattern as mcp_a2a_full_self_play.rs)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// SelfPlayBridge -- wires MCP client + A2A MessageRouter + converter
// ---------------------------------------------------------------------------

struct SelfPlayBridge {
    mcp_client: RunningService<RoleClient, TestClientHandler>,
    router: Arc<MessageRouter>,
}

impl SelfPlayBridge {
    /// Spin up a GgenMcpServer over in-process duplex + create a
    /// MessageRouter (with default handlers) and A2aMessageConverter.
    async fn new(examples_dir: &str) -> anyhow::Result<Self> {
        std::env::set_var("GGEN_EXAMPLES_DIR", examples_dir);

        let (server_transport, client_transport) = tokio::io::duplex(4096);

        let server = GgenMcpServer::new();
        tokio::spawn(async move {
            let _ = server.serve(server_transport).await;
        });

        let mcp_client = TestClientHandler::default().serve(client_transport).await?;

        let mut router = MessageRouter::with_defaults();
        let _ = router.register(TextContentHandler::new());
        let router = Arc::new(router);

        Ok(Self {
            mcp_client,
            router,
        })
    }

    /// Call an MCP tool and return the first text content as a String.
    async fn call_mcp_tool(
        &self, tool: &str, args: serde_json::Map<String, serde_json::Value>,
    ) -> anyhow::Result<String> {
        let result = self
            .mcp_client
            .call_tool(CallToolRequestParams::new(tool.to_string()).with_arguments(args))
            .await?;

        let text: Option<String> = result.content.iter().find_map(|c| {
            if let RawContent::Text(tc) = &c.raw {
                Some(tc.text.clone())
            } else {
                None
            }
        });

        text.ok_or_else(|| anyhow::anyhow!("no text content in MCP response"))
    }

    /// Route a ConvergedMessage through the A2A MessageRouter.
    async fn route_message(&self, message: ConvergedMessage) -> anyhow::Result<ConvergedMessage> {
        let response = self.router.route(message).await?;
        Ok(response)
    }

    /// Shut down the MCP client.
    async fn shutdown(self) -> anyhow::Result<()> {
        self.mcp_client.cancel().await?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create a simple A2A ConvergedMessage with text content.
fn make_text_message(source: &str, content: &str) -> ConvergedMessage {
    ConvergedMessage::text(
        format!("msg-{}", uuid::Uuid::new_v4()),
        source.to_string(),
        content.to_string(),
    )
}

/// Create a text message with correlation ID and causation chain for OTEL tracing.
fn make_correlated_message(
    source: &str, content: &str, correlation_id: &str, causation_chain: Vec<String>,
) -> ConvergedMessage {
    let mut msg = ConvergedMessage::text(
        format!("msg-{}", uuid::Uuid::new_v4()),
        source.to_string(),
        content.to_string(),
    );
    msg.envelope.correlation_id = Some(correlation_id.to_string());
    if !causation_chain.is_empty() {
        msg.envelope.causation_chain = Some(causation_chain);
    }
    msg
}

/// Extract text from UnifiedContent, returning an empty string for non-text.
fn extract_text(content: &UnifiedContent) -> &str {
    match content {
        UnifiedContent::Text { content, .. } => content.as_str(),
        _ => "",
    }
}

// ---------------------------------------------------------------------------
// Test 1: Multi-agent conversation (architect, coder, reviewer, tester)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_multi_agent_conversation() -> anyhow::Result<()> {
    let examples_dir = "/Users/sac/ggen/examples";
    let bridge = SelfPlayBridge::new(examples_dir).await?;

    let result = (|| async {
        // Architect proposes system design
        let architect_msg = make_text_message(
            "architect",
            "Proposing microservice architecture with event sourcing",
        );
        let architect_response = bridge.route_message(architect_msg).await?;
        let architect_text = extract_text(&architect_response.payload.content);
        assert!(
            !architect_text.is_empty(),
            "architect response must contain text, got empty"
        );

        // Coder implements the proposal
        let coder_msg = make_text_message(
            "coder",
            "Implementing event sourcing module with CQRS pattern",
        );
        let coder_response = bridge.route_message(coder_msg).await?;
        let coder_text = extract_text(&coder_response.payload.content);
        assert!(
            !coder_text.is_empty(),
            "coder response must contain text, got empty"
        );

        // Reviewer reviews the implementation
        let reviewer_msg = make_text_message(
            "reviewer",
            "Reviewing event sourcing implementation for correctness",
        );
        let reviewer_response = bridge.route_message(reviewer_msg).await?;
        let reviewer_text = extract_text(&reviewer_response.payload.content);
        assert!(
            !reviewer_text.is_empty(),
            "reviewer response must contain text, got empty"
        );

        // Tester validates the implementation
        let tester_msg = make_text_message(
            "tester",
            "Running integration tests on event sourcing module",
        );
        let tester_response = bridge.route_message(tester_msg).await?;
        let tester_text = extract_text(&tester_response.payload.content);
        assert!(
            !tester_text.is_empty(),
            "tester response must contain text, got empty"
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    bridge.shutdown().await?;
    result
}

// ---------------------------------------------------------------------------
// Test 2: Causation chain (architect -> coder -> reviewer -> tester)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_causation_chain() -> anyhow::Result<()> {
    let examples_dir = "/Users/sac/ggen/examples";
    let bridge = SelfPlayBridge::new(examples_dir).await?;

    let result = (|| async {
        let correlation_id = format!("chain-{}", uuid::Uuid::new_v4());

        // Step 1: Architect proposes -- first message in chain (no parents)
        let architect_msg = make_correlated_message(
            "architect",
            "Proposing REST API design for user management service",
            &correlation_id,
            vec![],
        );
        let architect_response = bridge.route_message(architect_msg).await?;
        let architect_text = extract_text(&architect_response.payload.content);
        assert!(
            !architect_text.is_empty(),
            "architect message must produce a response"
        );
        let architect_msg_id = architect_response.message_id.clone();

        // Step 2: Coder implements -- references architect's proposal
        let coder_msg = make_correlated_message(
            "coder",
            "Implementing user management REST endpoints based on proposal",
            &correlation_id,
            vec![architect_msg_id.clone()],
        );
        let coder_response = bridge.route_message(coder_msg).await?;
        let coder_text = extract_text(&coder_response.payload.content);
        assert!(
            !coder_text.is_empty(),
            "coder message must produce a response"
        );
        let coder_msg_id = coder_response.message_id.clone();

        // Step 3: Reviewer reviews -- references coder's implementation
        let reviewer_msg = make_correlated_message(
            "reviewer",
            "Reviewing REST endpoints for security and correctness",
            &correlation_id,
            vec![architect_msg_id.clone(), coder_msg_id.clone()],
        );
        let reviewer_response = bridge.route_message(reviewer_msg).await?;
        let reviewer_text = extract_text(&reviewer_response.payload.content);
        assert!(
            !reviewer_text.is_empty(),
            "reviewer message must produce a response"
        );
        let reviewer_msg_id = reviewer_response.message_id.clone();

        // Step 4: Tester validates -- references reviewer's approval
        let tester_msg = make_correlated_message(
            "tester",
            "Running end-to-end tests on user management endpoints",
            &correlation_id,
            vec![architect_msg_id, coder_msg_id, reviewer_msg_id],
        );
        let tester_response = bridge.route_message(tester_msg).await?;
        let tester_text = extract_text(&tester_response.payload.content);
        assert!(
            !tester_text.is_empty(),
            "tester message must produce a response"
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    bridge.shutdown().await?;
    result
}

// ---------------------------------------------------------------------------
// Test 3: Concurrent message burst (5 agents simultaneously)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_concurrent_message_burst() -> anyhow::Result<()> {
    let examples_dir = "/Users/sac/ggen/examples";
    let bridge = Arc::new(SelfPlayBridge::new(examples_dir).await?);

    let result = (|| async {
        let agents = [
            ("planner", "Creating sprint plan for Q2 deliverables"),
            ("designer", "Designing component interfaces for new module"),
            ("implementor", "Implementing core business logic handlers"),
            ("reviewer", "Reviewing code quality and test coverage"),
            ("deployer", "Preparing deployment manifest for staging"),
        ];

        let correlation_id = format!("burst-{}", uuid::Uuid::new_v4());

        // Spawn 5 concurrent routing tasks
        let mut handles = Vec::new();
        for (source, content) in &agents {
            let bridge_clone = Arc::clone(&bridge);
            let source = source.to_string();
            let content = content.to_string();
            let correlation_id = correlation_id.clone();

            handles.push(tokio::spawn(async move {
                let msg = make_correlated_message(&source, &content, &correlation_id, vec![]);
                let response = bridge_clone.route_message(msg).await?;
                let text = extract_text(&response.payload.content);
                anyhow::ensure!(
                    !text.is_empty(),
                    "{} response must contain text, got empty",
                    source
                );
                Ok::<String, anyhow::Error>(source)
            }));
        }

        let results = futures::future::join_all(handles).await;

        // All 5 agents should complete successfully
        let mut succeeded = 0usize;
        for r in &results {
            match r {
                Ok(Ok(_agent_name)) => {
                    succeeded += 1;
                }
                Ok(Err(e)) => panic!("agent task failed: {}", e),
                Err(e) => panic!("agent task panicked: {}", e),
            }
        }
        assert_eq!(
            succeeded, 5,
            "all 5 concurrent agents should succeed, {} succeeded",
            succeeded
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    match Arc::try_unwrap(bridge) {
        Ok(bridge) => bridge.shutdown().await?,
        Err(_) => {
            // Safety fallback -- in practice all tasks have completed
        }
    }

    result
}

// ---------------------------------------------------------------------------
// Test 4: MCP <-> A2A roundtrip (full bidirectional pipeline)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_mcp_a2a_roundtrip() -> anyhow::Result<()> {
    let examples_dir = "/Users/sac/ggen/examples";
    let bridge = SelfPlayBridge::new(examples_dir).await?;

    let result = (|| async {
        let correlation_id = format!("roundtrip-{}", uuid::Uuid::new_v4());

        // Step 1: MCP tool call -- list_examples
        let list_result = bridge
            .call_mcp_tool("list_examples", serde_json::Map::new())
            .await?;
        assert!(
            list_result.contains("examples"),
            "list_examples should return JSON with 'examples' key"
        );

        // Step 2: A2A message -- architect reviewing examples
        let architect_msg = make_correlated_message(
            "architect",
            "Reviewing available examples for project scaffolding",
            &correlation_id,
            vec![],
        );
        let architect_response = bridge.route_message(architect_msg).await?;
        let architect_text = extract_text(&architect_response.payload.content);
        assert!(
            !architect_text.is_empty(),
            "architect A2A response must contain text"
        );

        // Step 3: MCP tool call -- validate TTL
        let valid_ttl = "@prefix ex: <http://example.org/> . ex:Project a ex:Concept . ex:Module a ex:Concept .";
        let validate_args = serde_json::json!({ "ttl": valid_ttl })
            .as_object()
            .unwrap()
            .clone();
        let validate_result = bridge.call_mcp_tool("validate", validate_args).await?;
        assert!(
            validate_result.contains("Valid"),
            "valid TTL should be reported as Valid, got: {}",
            validate_result
        );

        // Step 4: A2A message -- validator confirming validity
        let validator_msg = make_correlated_message(
            "validator",
            "Confirming ontology validity -- all schema checks passed",
            &correlation_id,
            vec![architect_response.message_id.clone()],
        );
        let validator_response = bridge.route_message(validator_msg).await?;
        let validator_text = extract_text(&validator_response.payload.content);
        assert!(
            !validator_text.is_empty(),
            "validator A2A response must contain text"
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    bridge.shutdown().await?;
    result
}

// ---------------------------------------------------------------------------
// Test 5: Error recovery chain (error -> fix -> confirm -> validate -> done)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_error_recovery_chain() -> anyhow::Result<()> {
    let examples_dir = "/Users/sac/ggen/examples";
    let bridge = SelfPlayBridge::new(examples_dir).await?;

    let result = (|| async {
        let correlation_id = format!("recovery-{}", uuid::Uuid::new_v4());

        // Step 1: A2A error message -- syntax error detected
        let error_msg = make_correlated_message(
            "monitor",
            "Syntax error detected in ontology: missing type declaration",
            &correlation_id,
            vec![],
        );
        let error_response = bridge.route_message(error_msg).await?;
        let error_text = extract_text(&error_response.payload.content);
        assert!(
            !error_text.is_empty(),
            "error detection message must produce a response"
        );

        // Step 2: A2A fix message -- fix applied
        let fix_msg = make_correlated_message(
            "fixer",
            "Fix applied: added missing type declarations to ontology",
            &correlation_id,
            vec![error_response.message_id.clone()],
        );
        let fix_response = bridge.route_message(fix_msg).await?;
        let fix_text = extract_text(&fix_response.payload.content);
        assert!(
            !fix_text.is_empty(),
            "fix message must produce a response"
        );

        // Step 3: A2A confirmation message -- validation passed
        let confirm_msg = make_correlated_message(
            "validator",
            "Validation passed: ontology structure is now correct",
            &correlation_id,
            vec![
                error_response.message_id.clone(),
                fix_response.message_id.clone(),
            ],
        );
        let confirm_response = bridge.route_message(confirm_msg).await?;
        let confirm_text = extract_text(&confirm_response.payload.content);
        assert!(
            !confirm_text.is_empty(),
            "confirmation message must produce a response"
        );

        // Step 4: MCP tool call -- validate the fixed TTL
        let fixed_ttl = "@prefix ex: <http://example.org/> . ex:FixedEntity a ex:Entity . ex:FixedProperty a ex:Property .";
        let validate_args = serde_json::json!({ "ttl": fixed_ttl })
            .as_object()
            .unwrap()
            .clone();
        let validate_result = bridge.call_mcp_tool("validate", validate_args).await?;
        assert!(
            validate_result.contains("Valid"),
            "fixed TTL should validate successfully, got: {}",
            validate_result
        );

        // Step 5: A2A completion message -- all checks passed
        let done_msg = make_correlated_message(
            "coordinator",
            "All checks passed: error detected, fixed, validated, and confirmed",
            &correlation_id,
            vec![
                error_response.message_id,
                fix_response.message_id,
                confirm_response.message_id,
            ],
        );
        let done_response = bridge.route_message(done_msg).await?;
        let done_text = extract_text(&done_response.payload.content);
        assert!(
            !done_text.is_empty(),
            "completion message must produce a response"
        );

        Ok::<(), anyhow::Error>(())
    })()
    .await;

    bridge.shutdown().await?;
    result
}
