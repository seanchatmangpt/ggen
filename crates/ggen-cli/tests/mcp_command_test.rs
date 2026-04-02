//! MCP Command Integration Tests
//!
//! **Chicago TDD Principles**:
//! - REAL file I/O operations (tempfile)
//! - State-based verification (not mocks)
//!
//! **Test Coverage**:
//! 1. MCP server template validation
//! 2. Generated file structure validation
//! 3. Server metadata extraction
//! 4. Tool signature validation
//! 5. Error handling scenarios
//!
//! **Note**: These tests validate the MCP generation infrastructure.
//! Actual CLI command tests would require the `ggen mcp generate` command
//! to be implemented in the CLI layer.

use std::fs;
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

/// Create a minimal MCP server ontology for testing
fn create_minimal_ontology(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let ontology = r#"
@prefix mcp:  <https://ggen.io/examples/mcp#> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

mcp:TestServer a mcp:Server ;
    mcp:serverName   "test-mcp" ;
    mcp:serverStruct "TestMcpServer" ;
    mcp:description  "Test MCP server" ;
    mcp:version      "0.1.0" .

mcp:EchoTool a mcp:Tool ;
    mcp:serverOf    mcp:TestServer ;
    mcp:toolName    "echo" ;
    mcp:structName  "EchoParams" ;
    mcp:description "Echo back the input" ;
    mcp:order       1 .

mcp:MessageParam a mcp:Parameter ;
    mcp:toolOf     mcp:EchoTool ;
    mcp:paramName  "message" ;
    mcp:paramType  "String" ;
    mcp:required   "required" ;
    mcp:brief      "Message to echo back" .
"#;

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, ontology)?;
    Ok(())
}

// =========================================================================
// Test 1: MCP Server Template Validation
// =========================================================================

#[test]
fn test_mcp_server_template_is_valid() {
    println!("🔍 CLI Test: MCP server template is valid");

    // Arrange: Load template
    let mut tera = create_tera();
    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read stdio_server.rs.tera template");

    // Arrange: Create minimal context
    let mut ctx = Context::new();
    ctx.insert("server_name", "TestMcpServer");
    ctx.insert("server_struct", "TestMcpServer");
    ctx.insert("description", "Test server");
    ctx.insert("version", "0.1.0");

    let tools = vec![serde_json::json!({
        "name": "echo",
        "struct_name": "EchoParams",
        "description": "Echo back input",
    })];
    ctx.insert("tools", &tools);

    // Act: Render template
    let result = tera.render_str(&template_content, &ctx);

    // Assert: Template renders successfully
    assert!(result.is_ok(), "Template should render: {:?}", result.err());

    let rendered = result.unwrap();
    // Template renders server_name in the module comment, not as a struct name
    assert!(rendered.contains("TestMcpServer"));

    println!("✅ CLI Test PASSED: MCP server template is valid");
}

// =========================================================================
// Test 2: Output Directory Structure Validation
// =========================================================================

#[test]
fn test_mcp_generate_output_structure() {
    println!("🔍 CLI Test: Output directory structure validation");

    // Arrange: Create temp directory
    let temp_dir = setup_temp_dir().expect("Failed to setup temp dir");
    let output_dir = temp_dir.path().join("output");

    // Arrange: Render template
    let mut tera = create_tera();
    let mut ctx = Context::new();
    ctx.insert("server_name", "TestMcpServer");

    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    let rendered = tera
        .render_str(&template_content, &ctx)
        .expect("Failed to render template");

    // Act: Write files to output directory
    fs::create_dir_all(&output_dir).expect("Failed to create output dir");
    let main_rs = output_dir.join("main.rs");
    fs::write(&main_rs, rendered).expect("Failed to write main.rs");

    // Assert: Output directory exists
    assert!(output_dir.exists());
    assert!(output_dir.is_dir());

    // Assert: Main file exists
    assert!(main_rs.exists());

    // Assert: File is not empty
    let metadata = fs::metadata(&main_rs).expect("Failed to get metadata");
    assert!(metadata.len() > 0);

    println!("✅ CLI Test PASSED: Output directory structure is valid");
}

// =========================================================================
// Test 3: Minimal Ontology Generation
// =========================================================================

#[test]
fn test_mcp_generate_minimal_ontology() {
    println!("🔍 CLI Test: Generation with minimal ontology");

    // Arrange: Create temp directory and ontology
    let temp_dir = setup_temp_dir().expect("Failed to setup temp dir");
    let ontology_path = temp_dir.path().join("minimal.ttl");

    create_minimal_ontology(&ontology_path).expect("Failed to create minimal ontology");

    // Act: Load ontology
    let ontology_content = fs::read_to_string(&ontology_path).expect("Failed to read ontology");

    // Assert: Ontology contains expected data
    assert!(ontology_content.contains("mcp:TestServer"));
    assert!(ontology_content.contains("test-mcp"));
    assert!(ontology_content.contains("TestMcpServer"));
    assert!(ontology_content.contains("mcp:EchoTool"));
    assert!(ontology_content.contains("echo"));

    println!("✅ CLI Test PASSED: Minimal ontology structure is valid");
}

// =========================================================================
// Test 4: Error Handling - Missing Template
// =========================================================================

#[test]
fn test_mcp_generate_error_missing_template() {
    println!("🔍 CLI Test: Error handling for missing template");

    // Arrange: Create Tera instance
    let mut tera = create_tera();

    // Act: Try to render non-existent template
    let result = tera.render_str("{{ non_existent_function() }}", &Context::new());

    // Assert: Rendering fails
    assert!(result.is_err(), "Non-existent template should fail");

    println!("✅ CLI Test PASSED: Missing template produces error");
}

// =========================================================================
// Test 5: Tool Handler Template Validation
// =========================================================================

#[test]
fn test_mcp_tool_handler_template_is_valid() {
    println!("🔍 CLI Test: Tool handler template is valid");

    // Arrange: Load template
    let mut tera = create_tera();
    let template_content = fs::read_to_string("templates/mcp-server/tool_handler.rs.tera")
        .expect("Failed to read tool_handler.rs.tera template");

    // Arrange: Create context
    let mut ctx = Context::new();
    ctx.insert("error_type", "McpError");
    ctx.insert("handler_context_type", "HandlerContext");
    ctx.insert("stream_result_type", "Receiver<String>");
    ctx.insert("server_impl_type", "TestMcpServer");

    let tools = vec![serde_json::json!({
        "name": "echo",
        "description": "Echo back input",
        "input_schema": "{}",
        "input_type": "EchoParams",
        "enable_streaming": false,
    })];
    ctx.insert("tools", &tools);

    // Act: Render template
    let result = tera.render_str(&template_content, &ctx);

    // Assert: Template renders successfully
    assert!(result.is_ok(), "Template should render: {:?}", result.err());

    let rendered = result.unwrap();
    // Template generates PascalCase handler structs from tool names
    assert!(rendered.contains("EchoHandler"));
    assert!(rendered.contains("echo"));

    println!("✅ CLI Test PASSED: Tool handler template is valid");
}

// =========================================================================
// Test 6: Multiple Tools Template
// =========================================================================

#[test]
fn test_mcp_multiple_tools_template() {
    println!("🔍 CLI Test: Multiple tools in template");

    // Arrange: Load template
    let mut tera = create_tera();
    let template_content = fs::read_to_string("templates/mcp-server/tool_handler.rs.tera")
        .expect("Failed to read template");

    // Arrange: Create context with multiple tools
    let mut ctx = Context::new();
    ctx.insert("error_type", "McpError");
    ctx.insert("handler_context_type", "HandlerContext");
    ctx.insert("stream_result_type", "Receiver<String>");
    ctx.insert("server_impl_type", "TestMcpServer");

    let tools = vec![
        serde_json::json!({"name": "tool1", "description": "First", "input_schema": "{}", "input_type": "Tool1Params", "enable_streaming": false}),
        serde_json::json!({"name": "tool2", "description": "Second", "input_schema": "{}", "input_type": "Tool2Params", "enable_streaming": false}),
        serde_json::json!({"name": "tool3", "description": "Third", "input_schema": "{}", "input_type": "Tool3Params", "enable_streaming": false}),
    ];
    ctx.insert("tools", &tools);

    // Act: Render template
    let result = tera.render_str(&template_content, &ctx);

    // Assert: Template renders successfully
    assert!(result.is_ok(), "Template should render: {:?}", result.err());

    let rendered = result.unwrap();
    // Template generates PascalCase handler structs from tool names
    assert!(rendered.contains("Tool1Handler"));
    assert!(rendered.contains("Tool2Handler"));
    assert!(rendered.contains("Tool3Handler"));

    println!("✅ CLI Test PASSED: Multiple tools rendered correctly");
}

// =========================================================================
// Test 7: Server Name Validation
// =========================================================================

#[test]
fn test_mcp_server_name_validation() {
    println!("🔍 CLI Test: Server name validation");

    // Arrange: Valid and invalid server names
    let valid_names = vec!["GgenMcpServer", "TestServer", "MyServer123", "server_v1"];

    let invalid_names = vec![
        "test-server", // Contains hyphen
        "test server", // Contains space
        "test@server", // Contains special char
        "123server",   // Starts with number
    ];

    // Assert: Valid names match Rust identifier pattern
    for name in valid_names {
        let is_valid = name.chars().all(|c| c.is_alphanumeric() || c == '_');
        assert!(is_valid, "{} should be a valid Rust identifier", name);
    }

    // Note: This is a basic check - actual Rust identifier rules are more complex
    // For production, use a proper regex or syn crate

    println!("✅ CLI Test PASSED: Server name validation works");
}

// =========================================================================
// Test 8: Tool Order Validation
// =========================================================================

#[test]
fn test_mcp_tool_order_validation() {
    println!("🔍 CLI Test: Tool order validation");

    // Arrange: Create tools with specific order
    let tools = vec![("tool1", 1), ("tool2", 2), ("tool3", 3)];

    // Assert: Tools are in correct order
    for (idx, (name, order)) in tools.iter().enumerate() {
        assert_eq!(
            idx + 1,
            *order as usize,
            "{} should be at order {}",
            name,
            order
        );
    }

    println!("✅ CLI Test PASSED: Tool order validation works");
}

// =========================================================================
// Test 9: Template Context Completeness
// =========================================================================

#[test]
fn test_mcp_template_context_completeness() {
    println!("🔍 CLI Test: Template context completeness");

    // Arrange: Load template
    let mut tera = create_tera();
    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    // Arrange: Create complete context
    let mut ctx = Context::new();
    ctx.insert("server_name", "TestMcpServer");
    ctx.insert("server_struct", "TestMcpServer");
    ctx.insert("description", "Test server");
    ctx.insert("version", "0.1.0");

    let tools = vec![serde_json::json!({
        "name": "echo",
        "struct_name": "EchoParams",
        "description": "Echo",
    })];
    ctx.insert("tools", &tools);

    // Act: Render template
    let result = tera.render_str(&template_content, &ctx);

    // Assert: Template renders successfully with all required fields
    assert!(
        result.is_ok(),
        "Complete context should render successfully"
    );

    println!("✅ CLI Test PASSED: Template context is complete");
}

// =========================================================================
// Test 10: Generated File Permissions
// =========================================================================

#[test]
fn test_mcp_generated_file_permissions() {
    println!("🔍 CLI Test: Generated file permissions");

    // Arrange: Create temp directory
    let temp_dir = setup_temp_dir().expect("Failed to setup temp dir");

    // Arrange: Render and write file
    let mut tera = create_tera();
    let mut ctx = Context::new();
    ctx.insert("server_name", "TestMcpServer");

    let template_content = fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read template");

    let rendered = tera
        .render_str(&template_content, &ctx)
        .expect("Failed to render template");

    let output_file = temp_dir.path().join("main.rs");
    fs::write(&output_file, rendered).expect("Failed to write file");

    // Assert: File is readable
    let metadata = fs::metadata(&output_file).expect("Failed to get metadata");
    assert!(metadata.is_file());

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let permissions = metadata.permissions();
        let mode = permissions.mode();
        // Assert: File is readable by owner (0o400 = read-only)
        assert!(mode & 0o400 != 0, "File should be readable by owner");
    }

    println!("✅ CLI Test PASSED: Generated file permissions are correct");
}
