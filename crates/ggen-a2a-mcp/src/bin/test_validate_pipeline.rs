//! Direct test binary for validate_pipeline MCP tool
//!
//! Usage: cargo run --bin test_validate_pipeline

use std::path::PathBuf;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("ggen_a2a_mcp=info".parse()?)
                .add_directive("ggen_core=info".parse()?),
        )
        .init();

    println!("Testing validate_pipeline MCP tool directly");
    println!("============================================\n");

    // Get the current directory (should be /Users/sac/ggen)
    let current_dir = std::env::current_dir()
        .expect("Failed to get current directory");

    println!("Current directory: {}", current_dir.display());

    // Navigate to ggen root if we're in the bin directory
    let project_path = if current_dir.ends_with("crates/ggen-a2a-mcp/src/bin") ||
                         current_dir.ends_with("crates/ggen-a2a-mcp") {
        current_dir
            .ancestors()
            .nth(2) // Go up from crates/ggen-a2a-mcp to ggen root
            .unwrap_or_else(|| PathBuf::from("."))
            .to_path_buf()
    } else if current_dir.ends_with("ggen") {
        current_dir.clone()
    } else {
        PathBuf::from("..")
    };

    println!("Project path: {}\n", project_path.display());

    // Check if ggen.toml exists
    let manifest_path = project_path.join("ggen.toml");
    if !manifest_path.exists() {
        println!("❌ ERROR: ggen.toml not found at {}", manifest_path.display());
        println!("This test requires being run from the ggen root directory");
        anyhow::bail!("ggen.toml not found");
    }

    println!("✓ Found ggen.toml at {}", manifest_path.display());
    println!("✓ Calling validate_pipeline tool...\n");

    // Call the validate_pipeline tool
    let result = test_validate_pipeline(&project_path).await?;

    println!("\n{}", result);

    Ok(())
}

async fn test_validate_pipeline(project_path: &PathBuf) -> anyhow::Result<String> {
    use ggen_a2a_mcp::ggen_server::{GgenMcpServer, ValidatePipelineParams};
    use rmcp::handler::server::wrapper::Parameters;

    // Create the MCP server instance
    let server = GgenMcpServer::new();

    // Create parameters for validate_pipeline
    let params = ValidatePipelineParams {
        project_path: project_path.to_string_lossy().to_string(),
    };

    // Call the validate_pipeline tool
    let result = server.validate_pipeline(Parameters(params)).await?;

    // Extract the text content from the result
    if let Some(content) = result.content.first() {
        if let Some(text) = content.as_text() {
            return Ok(text.clone());
        }
    }

    anyhow::bail!("No text content in validate_pipeline result")
}
