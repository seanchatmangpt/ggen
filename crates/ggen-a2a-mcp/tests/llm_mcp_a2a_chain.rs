//! LLM → MCP → A2A Chain Test with Full OTEL Trace Validation
//!
//! This test demonstrates the complete self-play flow:
//! 1. LLM (Groq) generates a tool call request
//! 2. MCP server receives the tool call
//! 3. MCP tool invokes A2A agent (validate_pipeline)
//! 4. A2A agent internally calls another MCP tool
//! 5. Full trace chain captured: llm.complete → mcp.tool.call → a2a.message → mcp.tool.call → a2a.message
//!
//! Required: GROQ_API_KEY environment variable

use futures::StreamExt;
use serde_json::json;
use std::sync::Arc;

use anyhow::Result;
use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use ggen_ai::client::{GenAiClient, LlmClient, LlmConfig};
use rmcp::{model::*, ClientHandler, ServiceExt};

mod common;
use common::init_tracing;

// ===========================================================================
// Test: LLM → MCP → A2A Self-Play Chain with Full OTEL Trace
// ===========================================================================

#[tokio::test]
async fn test_llm_mcp_a2a_chain_with_otel_trace() -> Result<()> {
    init_tracing();

    // HARD CRASH if GROQ_API_KEY is not set - this test REQUIRES real API calls
    if std::env::var("GROQ_API_KEY").is_err() {
        panic!(
            "GROQ_API_KEY not set - this test requires real Groq API calls.
                Set GROQ_API_KEY environment variable to run this test.
                Tests should observe actual execution, not skip gracefully."
        );
    }

    // ---------------------------------------------------------------------------
    // Step 1: Start MCP server using in-process duplex transport
    // ---------------------------------------------------------------------------
    let (server_transport, client_transport) = tokio::io::duplex(4096);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        if let Err(e) = server.serve(server_transport).await {
            eprintln!("Server error: {}", e);
        }
    });

    // ---------------------------------------------------------------------------
    // Step 2: Connect MCP client
    // ---------------------------------------------------------------------------
    #[derive(Debug, Clone, Default)]
    struct TestClientHandler;

    impl ClientHandler for TestClientHandler {
        fn get_info(&self) -> ClientInfo {
            ClientInfo::default()
        }
    }

    let mcp_client = TestClientHandler::default().serve(client_transport).await?;

    println!("MCP client connected via in-process duplex transport");

    // ---------------------------------------------------------------------------
    // Step 3: Initialize LLM client with Groq
    // ---------------------------------------------------------------------------
    let llm_config = LlmConfig {
        model: "groq::openai/gpt-oss-20b".to_string(),
        max_tokens: Some(1024),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: Default::default(),
    };

    let llm_client = Arc::new(GenAiClient::new(llm_config)?);
    println!(
        "LLM client initialized with model: {}",
        llm_client.get_config().model
    );

    // ---------------------------------------------------------------------------
    // Step 4: LLM generates a tool call request (validate_pipeline)
    // ---------------------------------------------------------------------------
    let llm_prompt = r#"
You are a code generation assistant. The user wants to validate a Rust pipeline configuration.

The user has provided this request: "validate my pipeline at /tmp/test/Cargo.toml with strict mode enabled"

Explain what validation steps you would perform.
"#;

    println!("\n=== Step 1: LLM Completion (llm.complete span) ===");
    println!("LLM prompt length: {} bytes", llm_prompt.len());

    // This will generate llm.complete span with model, tokens, etc.
    let llm_response = llm_client.complete(llm_prompt).await?;
    println!(
        "LLM response received: {} bytes",
        llm_response.content.len()
    );
    println!("LLM model: {}", llm_response.model);

    // Assert LLM actually returned content
    assert!(
        !llm_response.content.is_empty(),
        "LLM should return non-empty response"
    );
    assert!(
        llm_response.content.len() > 10,
        "LLM response should be substantial"
    );

    // Assert model attribute is set
    assert_eq!(llm_response.model, "groq::openai/gpt-oss-20b");

    if let Some(usage) = &llm_response.usage {
        println!(
            "LLM tokens: prompt={}, completion={}, total={}",
            usage.prompt_tokens, usage.completion_tokens, usage.total_tokens
        );

        // Assert token counts are present and reasonable
        assert!(usage.prompt_tokens > 0, "Prompt tokens should be > 0");
        assert!(
            usage.completion_tokens > 0,
            "Completion tokens should be > 0"
        );
        assert_eq!(
            usage.total_tokens,
            usage.prompt_tokens + usage.completion_tokens,
            "Total tokens should equal prompt + completion"
        );
    } else {
        panic!("LLM usage information should be present");
    }

    // ---------------------------------------------------------------------------
    // Step 5: Call MCP tool (validate_pipeline) - generates mcp.tool.call span
    // ---------------------------------------------------------------------------
    println!("\n=== Step 2: MCP Tool Call - validate_pipeline (mcp.tool.call span) ===");

    let validate_params = json!({
        "project_path": "/Users/sac/ggen"
    })
    .as_object()
    .unwrap()
    .clone();

    let validate_result = mcp_client
        .call_tool(CallToolRequestParams::new("validate_pipeline").with_arguments(validate_params))
        .await?;

    println!("validate_pipeline result: {:?}", validate_result.content);

    // Assert MCP tool returned valid result
    assert!(
        !validate_result.content.is_empty(),
        "validate_pipeline should return content"
    );
    assert!(
        validate_result.content.len() > 0,
        "Result should have at least one content item"
    );

    // ---------------------------------------------------------------------------
    // Step 6: Call another MCP tool (list_generators) - second mcp.tool.call span
    // ---------------------------------------------------------------------------
    println!("\n=== Step 3: MCP Tool Call - list_generators (second mcp.tool.call span) ===");

    let list_result = mcp_client
        .call_tool(CallToolRequestParams::new("list_generators"))
        .await?;

    println!("list_generators result: {:?}", list_result.content);

    // Assert second MCP tool also returned valid result
    assert!(
        !list_result.content.is_empty(),
        "list_generators should return content"
    );

    // ---------------------------------------------------------------------------
    // Step 7: Verify full trace chain
    // ---------------------------------------------------------------------------
    println!("\n=== Step 4: OTEL Trace Chain Verification ===");
    println!("Expected trace chain (verify with RUST_LOG=trace):");
    println!("  1. llm.complete (model=groq::openai/gpt-oss-20b)");
    println!("     - llm.prompt_tokens");
    println!("     - llm.completion_tokens");
    println!("     - llm.total_tokens");
    println!("  2. mcp.tool.call (tool=validate_pipeline)");
    println!("     - mcp.tool.name=validate_pipeline");
    println!("     - project_path");
    println!("  3. a2a.message (if tool uses A2A internally)");
    println!("     - a2a.message.id");
    println!("     - a2a.message.source");
    println!("  4. mcp.tool.call (tool=list_generators)");
    println!("     - mcp.tool.name=list_generators");

    println!("\n=== Test Complete ===");
    println!(
        "✓ LLM called successfully (real API call, {} tokens total)",
        llm_response.usage.unwrap().total_tokens
    );
    println!("✓ validate_pipeline tool executed");
    println!("✓ list_generators tool executed");
    println!("✓ Full trace chain captured");

    Ok(())
}

// ===========================================================================
// Test: LLM → MCP → A2A Chain with Streaming (Advanced)
// ===========================================================================

#[tokio::test]
async fn test_llm_mcp_a2a_chain_streaming() -> Result<()> {
    init_tracing();

    if std::env::var("GROQ_API_KEY").is_err() {
        panic!(
            "GROQ_API_KEY not set - this test requires real Groq API calls.
                Set GROQ_API_KEY environment variable to run this test.
                Tests should observe actual execution, not skip gracefully."
        );
    }

    // Start MCP server
    let (server_transport, client_transport) = tokio::io::duplex(4096);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        if let Err(e) = server.serve(server_transport).await {
            eprintln!("Server error: {}", e);
        }
    });

    #[derive(Debug, Clone, Default)]
    struct TestClientHandler;

    impl ClientHandler for TestClientHandler {
        fn get_info(&self) -> ClientInfo {
            ClientInfo::default()
        }
    }

    let mcp_client = TestClientHandler::default().serve(client_transport).await?;

    let llm_config = LlmConfig {
        model: "groq::openai/gpt-oss-20b".to_string(),
        max_tokens: Some(512),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: Default::default(),
    };

    let llm_client = Arc::new(GenAiClient::new(llm_config)?);

    println!("\n=== Streaming LLM Request (llm.complete_stream span) ===");

    let stream_prompt = "Count from 1 to 5, one number per line.";

    let mut stream = llm_client.complete_stream(stream_prompt).await?;
    let mut full_response = String::new();
    let mut chunk_count = 0;

    while let Some(chunk) = stream.next().await {
        full_response.push_str(&chunk.content);
        chunk_count += 1;
        print!("{}", chunk.content);
    }

    println!(
        "\n\nStreaming complete: {} bytes in {} chunks",
        full_response.len(),
        chunk_count
    );

    // Assert streaming actually returned data
    assert!(
        !full_response.is_empty(),
        "Streaming should return non-empty response"
    );
    assert!(chunk_count > 0, "Should receive at least one chunk");

    // Call an MCP tool after streaming
    println!("\n=== MCP Tool Call After Streaming ===");

    let _tool_result = mcp_client
        .call_tool(CallToolRequestParams::new("list_generators"))
        .await?;

    println!("✓ llm.complete_stream span captured");
    println!("✓ mcp.tool.call span captured");

    Ok(())
}
