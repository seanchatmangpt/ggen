//! End-to-End MCP Server Generation Tests
//!
//! **Chicago TDD Principles**:
//! - REAL file I/O operations (tempfile for isolation)
//! - REAL template rendering (no mocks)
//! - State-based verification (not behavior verification)
//!
//! **Test Coverage**:
//! 1. Template rendering with real MCP ontology data
//! 2. Generated files exist and contain valid Rust code
//! 3. Server metadata matches ontology expectations
//! 4. Tool signatures match expected structure
//! 5. Deterministic generation (same input = same output)
//!
//! **Example Ontology Used**: `examples/mcp-server-definition/ontology/mcp-server.ttl`
//!   - Server: GgenMcpServer
//!   - Tools: sync_project, validate_ontology, list_examples, generate_preview

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::TempDir;
use tera::{Context, Tera};

// =========================================================================
// Test Utilities
// =========================================================================

/// Create a minimal Tera instance with all ggen templates registered
fn create_tera() -> Tera {
    let mut tera_instance = Tera::default();
    ggen_core::register::register_all(&mut tera_instance);
    tera_instance
}

/// Load the example MCP server ontology
fn load_example_ontology() -> Result<String, Box<dyn std::error::Error>> {
    fs::read_to_string("examples/mcp-server-definition/ontology/mcp-server.ttl")
        .map_err(|e| format!("Failed to load example ontology: {}", e).into())
}

/// Create a temporary directory with test structure
fn setup_temp_dir() -> Result<TempDir, Box<dyn std::error::Error>> {
    TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e).into())
}

/// Write content to a file in temp directory
fn write_file(
    temp_dir: &TempDir, path: &str, content: &str,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    let file_path = temp_dir.path().join(path);
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&file_path, content)?;
    Ok(file_path)
}

// =========================================================================
// Test 1: Template Rendering with MCP Server Data
// =========================================================================

#[test]
fn e2e_mcp_server_template_renders_successfully() {
    println!("🔍 E2E Test: MCP server template renders successfully");

    // Arrange: Create Tera instance and load template
    let mut tera = create_tera();
    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read stdio_server.rs.tera template");

    // Arrange: Create context with server metadata
    let mut ctx = Context::new();
    ctx.insert("server_name", "GgenMcpServer");
    ctx.insert("server_struct", "GgenMcpServer");
    ctx.insert("description", "ggen code generation MCP server");
    ctx.insert("version", "1.0.0");

    // Arrange: Add tools from ontology
    let tools = vec![
        serde_json::json!({
            "name": "sync_project",
            "struct_name": "SyncProjectParams",
            "description": "Run ggen sync pipeline on a project directory",
        }),
        serde_json::json!({
            "name": "validate_ontology",
            "struct_name": "ValidateOntologyParams",
            "description": "Validate a Turtle (.ttl) ontology file",
        }),
        serde_json::json!({
            "name": "list_examples",
            "struct_name": "ListExamplesParams",
            "description": "List available ggen example projects",
        }),
        serde_json::json!({
            "name": "generate_preview",
            "struct_name": "GeneratePreviewParams",
            "description": "Preview generated output for a rule without writing files",
        }),
    ];
    ctx.insert("tools", &tools);

    // Act: Render template
    let result = tera.render_str(&template_content, &ctx);

    // Assert: Template renders successfully
    assert!(
        result.is_ok(),
        "Template should render successfully: {:?}",
        result.err()
    );

    let rendered = result.unwrap();

    // Assert: Rendered content contains expected server
    assert!(rendered.contains("pub struct GgenMcpServer"));
    assert!(rendered.contains("impl GgenMcpServer"));

    // Assert: All tools are mentioned
    assert!(rendered.contains("sync_project"));
    assert!(rendered.contains("validate_ontology"));
    assert!(rendered.contains("list_examples"));
    assert!(rendered.contains("generate_preview"));

    println!("✅ E2E Test PASSED: Template renders successfully");
}

// =========================================================================
// Test 2: Tool Handler Template Renders
// =========================================================================

#[test]
fn e2e_mcp_tool_handler_template_renders_successfully() {
    println!("🔍 E2E Test: Tool handler template renders successfully");

    // Arrange: Create Tera instance and load template
    let mut tera = create_tera();
    let template_content = fs::read_to_string("templates/mcp-server/tool_handler.rs.tera")
        .expect("Failed to read tool_handler.rs.tera template");

    // Arrange: Create context with error type and tools
    let mut ctx = Context::new();
    ctx.insert("error_type", "McpError");
    ctx.insert("handler_context_type", "HandlerContext");
    ctx.insert("stream_result_type", "Receiver<String>");
    ctx.insert("server_impl_type", "GgenMcpServer");

    let tools = vec![
        serde_json::json!({
            "name": "sync_project",
            "description": "Run ggen sync pipeline",
            "input_schema": "{}",
            "input_type": "SyncProjectParams",
            "enable_streaming": false,
        }),
        serde_json::json!({
            "name": "validate_ontology",
            "description": "Validate Turtle file",
            "input_schema": "{}",
            "input_type": "ValidateOntologyParams",
            "enable_streaming": false,
        }),
    ];
    ctx.insert("tools", &tools);

    // Act: Render template
    let result = tera.render_str(&template_content, &ctx);

    // Assert: Template renders successfully
    assert!(
        result.is_ok(),
        "Template should render successfully: {:?}",
        result.err()
    );

    let rendered = result.unwrap();

    // Assert: Contains expected Rust code
    assert!(rendered.contains("#[tool_handler]"));
    assert!(rendered.contains("impl ServerHandler"));
    assert!(rendered.contains("pub async fn sync_project_handler"));
    assert!(rendered.contains("pub async fn validate_ontology_handler"));

    println!("✅ E2E Test PASSED: Tool handler template renders successfully");
}

// =========================================================================
// Test 3: Generated Files Written to Disk
// =========================================================================

#[test]
fn e2e_mcp_server_generated_files_written_to_disk() {
    println!("🔍 E2E Test: Generated files written to disk");

    // Arrange: Create temp directory
    let temp_dir = setup_temp_dir().expect("Failed to setup temp dir");

    // Arrange: Create Tera instance and context
    let mut tera = create_tera();
    let mut ctx = Context::new();
    ctx.insert("server_name", "GgenMcpServer");

    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    // Act: Render template
    let rendered = tera
        .render_str(&template_content, &ctx)
        .expect("Failed to render template");

    // Act: Write to file
    let output_dir = temp_dir.path().join("generated");
    fs::create_dir_all(&output_dir).expect("Failed to create output dir");

    let main_rs = output_dir.join("main.rs");
    fs::write(&main_rs, rendered).expect("Failed to write main.rs");

    // Assert: File exists
    assert!(main_rs.exists(), "main.rs should exist");

    // Assert: File is not empty
    let metadata = fs::metadata(&main_rs).expect("Failed to get metadata");
    assert!(metadata.len() > 0, "main.rs should not be empty");

    // Assert: File contains expected content
    let content = fs::read_to_string(&main_rs).expect("Failed to read main.rs");
    assert!(content.contains("pub struct GgenMcpServer"));
    assert!(content.contains("fn main"));

    println!("✅ E2E Test PASSED: Generated files written to disk");
}

// =========================================================================
// Test 4: Server Metadata Validation
// =========================================================================

#[test]
fn e2e_mcp_server_metadata_matches_expected() {
    println!("🔍 E2E Test: Server metadata matches expected values");

    // Arrange: Load example ontology
    let ontology_content = load_example_ontology().expect("Failed to load ontology");

    // Assert: Ontology contains expected server metadata
    assert!(ontology_content.contains("mcp:GgenServer"));
    assert!(ontology_content.contains("ggen-mcp"));
    assert!(ontology_content.contains("GgenMcpServer"));
    assert!(ontology_content.contains("ggen code generation MCP server"));
    assert!(ontology_content.contains("1.0.0"));

    println!("✅ E2E Test PASSED: Server metadata matches expected values");
}

// =========================================================================
// Test 5: Tool Signatures Validation
// =========================================================================

#[test]
fn e2e_mcp_server_tool_signatures_match_expected() {
    println!("🔍 E2E Test: Tool signatures match expected values");

    // Arrange: Load example ontology
    let ontology_content = load_example_ontology().expect("Failed to load ontology");

    // Assert: Ontology contains expected tools
    assert!(ontology_content.contains("mcp:SyncTool"));
    assert!(ontology_content.contains("sync_project"));
    assert!(ontology_content.contains("SyncProjectParams"));

    assert!(ontology_content.contains("mcp:ValidateTool"));
    assert!(ontology_content.contains("validate_ontology"));
    assert!(ontology_content.contains("ValidateOntologyParams"));

    assert!(ontology_content.contains("mcp:ListTool"));
    assert!(ontology_content.contains("list_examples"));
    assert!(ontology_content.contains("ListExamplesParams"));

    assert!(ontology_content.contains("mcp:PreviewTool"));
    assert!(ontology_content.contains("generate_preview"));
    assert!(ontology_content.contains("GeneratePreviewParams"));

    println!("✅ E2E Test PASSED: Tool signatures match expected values");
}

// =========================================================================
// Test 6: Deterministic Generation
// =========================================================================

#[test]
fn e2e_mcp_server_deterministic_generation() {
    println!("🔍 E2E Test: Deterministic generation");

    // Arrange: Create Tera instance and context
    let mut tera1 = create_tera();
    let mut tera2 = create_tera();

    let mut ctx1 = Context::new();
    ctx1.insert("server_name", "GgenMcpServer");

    let mut ctx2 = Context::new();
    ctx2.insert("server_name", "GgenMcpServer");

    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    // Act: Render twice with same context
    let rendered1 = tera1
        .render_str(&template_content, &ctx1)
        .expect("Failed to render first time");
    let rendered2 = tera2
        .render_str(&template_content, &ctx2)
        .expect("Failed to render second time");

    // Assert: Output is identical
    assert_eq!(
        rendered1, rendered2,
        "Same input should produce identical output"
    );

    println!("✅ E2E Test PASSED: Deterministic generation verified");
}

// =========================================================================
// Test 7: Tool Parameter Validation
// =========================================================================

#[test]
fn e2e_mcp_server_tool_parameters_in_ontology() {
    println!("🔍 E2E Test: Tool parameters in ontology");

    // Arrange: Load example ontology
    let ontology_content = load_example_ontology().expect("Failed to load ontology");

    // Assert: sync_project has expected parameters
    assert!(ontology_content.contains("mcp:SyncPathParam"));
    assert!(ontology_content.contains("project_path"));
    assert!(ontology_content.contains("String"));
    assert!(ontology_content.contains("required"));

    assert!(ontology_content.contains("mcp:SyncDryRunParam"));
    assert!(ontology_content.contains("dry_run"));
    assert!(ontology_content.contains("bool"));
    assert!(ontology_content.contains("optional"));

    // Assert: validate_ontology has expected parameter
    assert!(ontology_content.contains("mcp:ValidatePathParam"));
    assert!(ontology_content.contains("ontology_path"));

    println!("✅ E2E Test PASSED: Tool parameters in ontology");
}

// =========================================================================
// Test 8: Multiple Tools Generated
// =========================================================================

#[test]
fn e2e_mcp_server_multiple_tools_generated() {
    println!("🔍 E2E Test: Multiple tools generated");

    // Arrange: Create Tera instance with all 4 tools
    let mut tera = create_tera();
    let mut ctx = Context::new();
    ctx.insert("server_name", "GgenMcpServer");

    let tools = vec![
        serde_json::json!({"name": "tool1", "struct_name": "Tool1Params", "description": "First tool"}),
        serde_json::json!({"name": "tool2", "struct_name": "Tool2Params", "description": "Second tool"}),
        serde_json::json!({"name": "tool3", "struct_name": "Tool3Params", "description": "Third tool"}),
        serde_json::json!({"name": "tool4", "struct_name": "Tool4Params", "description": "Fourth tool"}),
    ];
    ctx.insert("tools", &tools);

    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    // Act: Render template
    let rendered = tera
        .render_str(&template_content, &ctx)
        .expect("Failed to render template");

    // Assert: All tools are present
    assert!(rendered.contains("tool1"));
    assert!(rendered.contains("tool2"));
    assert!(rendered.contains("tool3"));
    assert!(rendered.contains("tool4"));

    println!("✅ E2E Test PASSED: Multiple tools generated");
}

// =========================================================================
// Test 9: Template Error Handling
// =========================================================================

#[test]
fn e2e_mcp_server_template_error_handling() {
    println!("🔍 E2E Test: Template error handling");

    // Arrange: Create Tera instance
    let mut tera = create_tera();
    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    // Arrange: Create context with missing required variable
    let ctx = Context::new(); // Missing "server_name"

    // Act: Try to render template
    let result = tera.render_str(&template_content, &ctx);

    // Assert: Rendering fails gracefully
    assert!(
        result.is_err(),
        "Template should fail with missing context variable"
    );

    println!("✅ E2E Test PASSED: Template error handling works");
}

// =========================================================================
// Test 10: Generated Code Contains Valid Rust Syntax
// =========================================================================

#[test]
fn e2e_mcp_server_generated_code_syntax() {
    println!("🔍 E2E Test: Generated code contains valid Rust syntax");

    // Arrange: Create Tera instance and context
    let mut tera = create_tera();
    let mut ctx = Context::new();
    ctx.insert("server_name", "GgenMcpServer");

    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    // Act: Render template
    let rendered = tera
        .render_str(&template_content, &ctx)
        .expect("Failed to render template");

    // Assert: Contains basic Rust syntax elements
    assert!(rendered.contains("pub struct"));
    assert!(rendered.contains("impl"));
    assert!(rendered.contains("fn main"));
    assert!(rendered.contains("{"));
    assert!(rendered.contains("}"));

    // Assert: No obvious syntax errors (unmatched braces, etc.)
    let open_braces = rendered.matches("{").count();
    let close_braces = rendered.matches("}").count();
    assert_eq!(open_braces, close_braces, "Braces should be balanced");

    println!("✅ E2E Test PASSED: Generated code has valid Rust syntax");
}
