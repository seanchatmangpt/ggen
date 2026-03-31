//! MCP Client Example: Call validate_pipeline tool
//!
//! This example demonstrates how to:
//! 1. Create an in-memory GgenMcpServer
//! 2. Call the validate_pipeline tool via CallToolRequest
//! 3. Print the 6 quality gate results

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{
    model::{CallToolRequest, CallToolResult, ClientCapabilities},
    protocol::JsonRpcClient,
    server::RpcClient,
};
use std::sync::Arc;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info,mcp_pipeline_example=debug,ggen_a2a_mcp=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    println!("🔧 MCP Client Example: validate_pipeline Tool Call");
    println!("==================================================\n");

    // Step 1: Create GgenMcpServer instance
    let server = Arc::new(GgenMcpServer::new());
    println!("✅ Created GgenMcpServer instance");

    // Step 2: Get server info (initialization)
    let server_info = server.get_info();
    println!(
        "✅ Server info: {} v{}\n",
        server_info.server_info.name, server_info.server_info.version
    );

    // Step 3: Create CallToolRequest for validate_pipeline
    let project_path = std::env::current_dir()
        .map(|p| p.parent().unwrap().parent().unwrap().display().to_string())
        .unwrap_or_else(|_| ".".to_string());

    println!("📂 Project path: {}\n", project_path);

    // Build the JSON-RPC params for validate_pipeline
    let params = serde_json::json!({
        "project_path": project_path
    });

    let request = CallToolRequest::new("validate_pipeline".to_string(), Some(params));

    println!("🔨 Calling validate_pipeline tool...\n");

    // Step 4: Call the tool directly on the server
    let result = call_tool_directly(server.clone(), request).await?;

    // Step 5: Print results
    print_tool_result(result);

    println!("\n✅ Example complete!");

    Ok(())
}

/// Call a tool directly on GgenMcpServer (bypassing transport layer)
async fn call_tool_directly(
    server: Arc<GgenMcpServer>, request: CallToolRequest,
) -> anyhow::Result<CallToolResult> {
    use rmcp::handler::server::wrapper::Parameters;

    // Extract params from request
    let params_value = request.params.unwrap_or(serde_json::json!({}));

    // Match tool name and call appropriate method
    match request.name.as_str() {
        "validate_pipeline" => {
            // Parse ValidatePipelineParams
            #[derive(serde::Deserialize)]
            struct ValidatePipelineParams {
                project_path: String,
            }

            let params: ValidatePipelineParams = serde_json::from_value(params_value)
                .map_err(|e| anyhow::anyhow!("Failed to parse params: {}", e))?;

            // Call the tool method directly
            let result = server
                .validate_pipeline(rmcp::handler::server::wrapper::Parameters(params))
                .await?;

            Ok(result)
        }
        _ => Err(anyhow::anyhow!("Unknown tool: {}", request.name)),
    }
}

/// Print the tool result in a formatted way
fn print_tool_result(result: CallToolResult) {
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("QUALITY GATE VALIDATION RESULTS");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

    if result.is_error {
        println!("❌ VALIDATION FAILED\n");
        for content in &result.content {
            if let Some(text) = content.extract_text() {
                println!("Error details:\n{}", text);
            }
        }
    } else {
        println!("✅ VALIDATION PASSED\n");
        for content in &result.content {
            if let Some(text) = content.extract_text() {
                // Parse and format the checkpoint results
                print_quality_gate_results(text);
            }
        }
    }

    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
}

/// Print formatted quality gate results
fn print_quality_gate_results(text: &str) {
    // Extract checkpoint count
    if let Some(checkpoint_line) = text.lines().find(|l| l.contains("checkpoints")) {
        println!("{}", checkpoint_line);
        println!();
    }

    // Extract individual checkpoints
    let checkpoint_start = text.find("Passed checks:");
    if let Some(start) = checkpoint_start {
        let checks_section = &text[start..];
        println!("Quality Gate Checkpoints:\n");

        for line in checks_section.lines() {
            if line.contains("✓") {
                println!("  {}", line.trim());
            }
        }
    }

    // If no structured format found, just print the text
    if checkpoint_start.is_none() {
        println!("{}", text);
    }
}
