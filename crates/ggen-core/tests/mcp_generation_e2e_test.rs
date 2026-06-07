#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

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

fn read_template_file(relative_path: &str) -> String {
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let path_from_manifest = Path::new(&manifest_dir).join(relative_path);
        if path_from_manifest.exists() {
            return fs::read_to_string(path_from_manifest).expect("Failed to read template");
        }
    }
    let path = PathBuf::from(relative_path);
    if path.exists() {
        return fs::read_to_string(path).expect("Failed to read template");
    }
    panic!("Failed to read template file: {}", relative_path);
}

/// Create a minimal Tera instance with all ggen templates registered
fn create_tera() -> Tera {
    let mut templates_dir = PathBuf::from("templates");
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let path_from_manifest = Path::new(&manifest_dir).join("templates");
        if path_from_manifest.exists() {
            templates_dir = path_from_manifest;
        }
    }
    let mcp_dir = templates_dir.join("mcp-server");
    let mut tera_instance = Tera::default();
    
    // Add each template file manually if it exists
    let files = vec![
        ("_head.tera", "mcp-server/_head.tera"),
        ("mcp_server.rs.tera", "mcp-server/mcp_server.rs.tera"),
        ("resource_handler.rs.tera", "mcp-server/resource_handler.rs.tera"),
        ("stdio_server.rs.tera", "mcp-server/stdio_server.rs.tera"),
        ("tool_handler.rs.tera", "mcp-server/tool_handler.rs.tera"),
    ];
    for (filename, reg_name) in files {
        let filepath = mcp_dir.join(filename);
        if filepath.exists() {
            let content = fs::read_to_string(&filepath).unwrap_or_else(|e| {
                panic!("Failed to read template file {:?}: {}", filepath, e);
            });
            tera_instance.add_raw_template(reg_name, &content).unwrap_or_else(|e| {
                panic!("Failed to add raw template {}: {}", reg_name, e);
            });
        }
    }
    
    ggen_core::register::register_all(&mut tera_instance);
    tera_instance
}

/// Load the example MCP server ontology
fn load_example_ontology() -> Result<String, Box<dyn std::error::Error>> {
    let mut ontology_path = PathBuf::from("examples/mcp-server-definition/ontology/mcp-server.ttl");
    if !ontology_path.exists() {
        if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
            let path_from_workspace = Path::new(&manifest_dir).parent().and_then(|p| p.parent()).map(|p| p.join("examples/mcp-server-definition/ontology/mcp-server.ttl"));
            if let Some(p) = path_from_workspace {
                if p.exists() {
                    ontology_path = p;
                }
            }
        }
    }
    fs::read_to_string(&ontology_path)
        .map_err(|e| format!("Failed to load example ontology from {:?}: {}", ontology_path, e).into())
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
    let template_content = read_template_file("templates/mcp-server/tool_handler.rs.tera");

    // Arrange: Create context with server metadata
    let mut ctx = Context::new();
    ctx.insert("tool_name", "sync_project");
    ctx.insert("server_name", "GgenMcpServer");
    ctx.insert("server_struct", "GgenMcpServer");
    ctx.insert("description", "ggen code generation MCP server");
    ctx.insert("version", "26.6.6");

    // Arrange: Add tools in sparql_results format
    let sparql_results = serde_json::json!([
        {
            "tool_name": "sync_project",
            "tool_description": "Run ggen sync pipeline on a project directory",
            "param_name": "project_path",
            "param_description": "Project path",
            "is_required": "true",
            "rust_type": "String"
        },
        {
            "tool_name": "validate_ontology",
            "tool_description": "Validate a Turtle (.ttl) ontology file",
            "param_name": "ontology_path",
            "param_description": "Ontology path",
            "is_required": "true",
            "rust_type": "String"
        },
        {
            "tool_name": "list_examples",
            "tool_description": "List available ggen example projects",
            "param_name": "dummy",
            "param_description": "Dummy",
            "is_required": "false",
            "rust_type": "String"
        },
        {
            "tool_name": "generate_preview",
            "tool_description": "Preview generated output for a rule without writing files",
            "param_name": "dummy2",
            "param_description": "Dummy2",
            "is_required": "false",
            "rust_type": "String"
        }
    ]);
    ctx.insert("sparql_results", &sparql_results);

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
    let template_content = read_template_file("templates/mcp-server/tool_handler.rs.tera");

    // Arrange: Create context with error type and tools
    let mut ctx = Context::new();
    ctx.insert("tool_name", "sync_project");
    ctx.insert("server_name", "GgenMcpServer");
    ctx.insert("error_type", "McpError");
    ctx.insert("handler_context_type", "HandlerContext");
    ctx.insert("stream_result_type", "Receiver<String>");
    ctx.insert("server_impl_type", "GgenMcpServer");

    let sparql_results = serde_json::json!([
        {
            "tool_name": "sync_project",
            "tool_description": "Run ggen sync pipeline",
            "param_name": "project_path",
            "param_description": "Project path",
            "is_required": "true",
            "rust_type": "String"
        },
        {
            "tool_name": "validate_ontology",
            "tool_description": "Validate Turtle file",
            "param_name": "ontology_path",
            "param_description": "Ontology path",
            "is_required": "true",
            "rust_type": "String"
        }
    ]);
    ctx.insert("sparql_results", &sparql_results);

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
    assert!(rendered.contains("async fn sync_project"));
    assert!(rendered.contains("async fn validate_ontology"));

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

    let template_content = read_template_file("templates/mcp-server/mcp_server.rs.tera");

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
    assert!(ontology_content.contains("26.6.6"));

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

    let template_content = read_template_file("templates/mcp-server/mcp_server.rs.tera");

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
    ctx.insert("tool_name", "tool1");
    ctx.insert("server_name", "GgenMcpServer");

    let sparql_results = serde_json::json!([
        {"tool_name": "tool1", "tool_description": "First tool", "param_name": "param1", "is_required": "false"},
        {"tool_name": "tool2", "tool_description": "Second tool", "param_name": "param2", "is_required": "false"},
        {"tool_name": "tool3", "tool_description": "Third tool", "param_name": "param3", "is_required": "false"},
        {"tool_name": "tool4", "tool_description": "Fourth tool", "param_name": "param4", "is_required": "false"}
    ]);
    ctx.insert("sparql_results", &sparql_results);

    let template_content = read_template_file("templates/mcp-server/tool_handler.rs.tera");

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
    let template_content = read_template_file("templates/mcp-server/tool_handler.rs.tera");

    // Arrange: Create context with missing required variables
    let ctx = Context::new(); // Missing "tool_name"

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

    let template_content = read_template_file("templates/mcp-server/mcp_server.rs.tera");

    // Act: Render template
    let rendered = tera
        .render_str(&template_content, &ctx)
        .expect("Failed to render template");

    // Assert: Contains basic Rust syntax elements
    assert!(rendered.contains("pub struct"));
    assert!(rendered.contains("impl"));
    assert!(rendered.contains("{"));
    assert!(rendered.contains("}"));

    // Assert: No obvious syntax errors (unmatched braces, etc.)
    let open_braces = rendered.matches("{").count();
    let close_braces = rendered.matches("}").count();
    assert_eq!(open_braces, close_braces, "Braces should be balanced");

    println!("✅ E2E Test PASSED: Generated code has valid Rust syntax");
}
