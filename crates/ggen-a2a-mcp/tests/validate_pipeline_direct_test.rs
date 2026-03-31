//! Direct integration test of validate_pipeline MCP tool
//! Tests the tool without going through the full MCP protocol

use ggen_a2a_mcp::ggen_server::{GgenMcpServer, ValidatePipelineParams};

#[tokio::test]
async fn test_validate_pipeline_on_current_project() {
    // Initialize tracing for output
    let subscriber = tracing_subscriber::fmt()
        .with_test_writer()
        .with_max_level(tracing::Level::INFO)
        .try_init();

    // Create server instance
    let server = GgenMcpServer::new();

    // Test on current ggen project
    let project_path = std::env::var("CARGO_MANIFEST_DIR")
        .map(|p| {
            std::path::PathBuf::from(p)
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .display()
                .to_string()
        })
        .unwrap_or_else(|_| ".".to_string());

    println!("\n=== Testing validate_pipeline on: {} ===\n", project_path);

    // Call the tool
    let params = ValidatePipelineParams {
        project_path: project_path.clone(),
    };

    let result = server.validate_pipeline(params).await;

    // Print detailed results
    match result {
        Ok(response) => {
            println!("\n=== validate_pipeline Result ===\n");
            for content in &response.content {
                if let Some(text) = content.as_text() {
                    println!("{}", text);
                }
            }
            println!("\nis_success: {}", response.is_success);
        }
        Err(e) => {
            println!("\n=== validate_pipeline Error ===\n");
            println!("Error: {:?}", e);
        }
    }
}
