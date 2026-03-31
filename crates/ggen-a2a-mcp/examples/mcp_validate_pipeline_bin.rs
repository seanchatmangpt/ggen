//! MCP Client Example: Call validate_pipeline tool
//!
//! This example demonstrates how to:
//! 1. Create an in-memory GgenMcpServer
//! 2. Call the validate_pipeline tool directly
//! 3. Print the 6 quality gate results

use ggen_a2a_mcp::ggen_server::{GgenMcpServer, ValidatePipelineParams};
use rmcp::handler::server::wrapper::Parameters;
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    println!("🔧 MCP Client Example: validate_pipeline Tool Call");
    println!("==================================================\n");

    // Step 1: Create GgenMcpServer instance
    let rt = tokio::runtime::Runtime::new()?;
    let server = Arc::new(GgenMcpServer::new());
    println!("✅ Created GgenMcpServer instance");

    // Step 2: Get server info (initialization)
    let server_info = server.get_info();
    println!(
        "✅ Server info: {} v{}\n",
        server_info.server_info.name, server_info.server_info.version
    );

    // Step 3: Get project path (current ggen project)
    let project_path = std::env::current_dir()
        .map(|p| {
            p.parent()
                .unwrap()
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .display()
                .to_string()
        })
        .unwrap_or_else(|_| ".".to_string());

    println!("📂 Project path: {}\n", project_path);

    // Step 4: Call validate_pipeline tool
    println!("🔨 Calling validate_pipeline tool...\n");

    let result = rt.block_on(async {
        let params = ValidatePipelineParams {
            project_path: project_path.clone(),
        };

        server.validate_pipeline(Parameters(params)).await
    })?;

    // Step 5: Print results
    print_tool_result(&result);

    println!("\n✅ Example complete!");

    Ok(())
}

/// Print the tool result in a formatted way
fn print_tool_result(result: &rmcp::model::CallToolResult) {
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
