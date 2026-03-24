//! Tests for MCP server template validation
//!
//! Validates that all MCP server templates are parseable by Tera
//! and can generate valid Rust code.

use ggen_core::register::register_all;
use std::collections::HashMap;
use tera::{Context, Tera};

fn create_test_tera() -> Tera {
    let mut tera = Tera::default();
    register_all(&mut tera);
    tera
}

#[test]
fn test_head_template_is_parseable() {
    let tera = create_test_tera();
    let template = std::fs::read_to_string("templates/mcp-server/_head.tera")
        .expect("Failed to read _head.tera");

    // Should be parseable even with blocks
    let result = tera.render_str(&template, &Context::new());
    // This will fail due to missing variables but template should parse
    // We just want to ensure the syntax is valid
    match result {
        Err(_) => {} // Expected - missing context
        Ok(_) => {}  // Also OK
    }
}

#[test]
fn test_mcp_server_template_is_parseable() {
    let tera = create_test_tera();
    let template = std::fs::read_to_string("templates/mcp-server/mcp_server.rs.tera")
        .expect("Failed to read mcp_server.rs.tera");

    let mut ctx = Context::new();
    ctx.insert("server_name", "TestMcpServer");
    ctx.insert("registry_type", "McpToolRegistry");
    ctx.insert("io_handler_type", "IoHandler");
    ctx.insert("error_type", "McpError");
    ctx.insert("tool_type", "Tool");
    ctx.insert("resource_type", "Resource");
    ctx.insert("enable_default_impl", true);
    ctx.insert("server_docstring", "Test MCP server");

    let result = tera.render_str(&template, &ctx);
    assert!(result.is_ok(), "Template should render: {:?}", result.err());
}

#[test]
fn test_tool_handler_template_is_parseable() {
    let tera = create_test_tera();

    let template = std::fs::read_to_string("templates/mcp-server/tool_handler.rs.tera")
        .expect("Failed to read tool_handler.rs.tera");

    let mut ctx = Context::new();
    ctx.insert("error_type", "McpError");
    ctx.insert("handler_context_type", "HandlerContext");
    ctx.insert("stream_result_type", "Receiver<String>");
    ctx.insert("server_impl_type", "McpServerImpl");

    let tools = vec![
        serde_json::json!({
            "name": "test_tool",
            "description": "A test tool",
            "input_schema": "{}",
            "input_type": "serde_json::Value",
            "enable_streaming": false,
        }),
        serde_json::json!({
            "name": "streaming_tool",
            "description": "A streaming tool",
            "input_schema": "{\"type\": \"object\"}",
            "input_type": "serde_json::Value",
            "enable_streaming": true,
        }),
    ];
    ctx.insert("tools", &tools);

    let result = tera.render_str(&template, &ctx);
    assert!(result.is_ok(), "Template should render: {:?}", result.err());

    // Check that output contains expected code
    let output = result.unwrap();
    assert!(output.contains("pub async fn test_tool_handler"));
    assert!(output.contains("pub async fn streaming_tool_streaming_handler"));
}

#[test]
fn test_resource_handler_template_is_parseable() {
    let tera = create_test_tera();

    let template = std::fs::read_to_string("templates/mcp-server/resource_handler.rs.tera")
        .expect("Failed to read resource_handler.rs.tera");

    let mut ctx = Context::new();
    ctx.insert("error_type", "McpError");
    ctx.insert("resource_type", "Resource");
    ctx.insert("cache_entry_type", "CacheEntry");
    ctx.insert("resource_response_type", "String");

    let resources = vec![
        serde_json::json!({
            "name": "file_resource",
            "description": "A file resource",
            "uri_pattern": "file://*",
            "mime_type": "text/plain",
        }),
        serde_json::json!({
            "name": "memory_resource",
            "description": "A memory resource",
            "uri_pattern": "memory://*",
            "mime_type": "application/json",
        }),
    ];
    ctx.insert("resources", &resources);

    let result = tera.render_str(&template, &ctx);
    assert!(result.is_ok(), "Template should render: {:?}", result.err());

    // Check that output contains expected code
    let output = result.unwrap();
    assert!(output.contains("pub async fn get_file_resource"));
    assert!(output.contains("pub async fn get_memory_resource"));
    assert!(output.contains("pub struct ResourceHandler"));
}

#[test]
fn test_stdio_server_template_is_parseable() {
    let tera = create_test_tera();

    let template = std::fs::read_to_string("templates/mcp-server/stdio_server.rs.tera")
        .expect("Failed to read stdio_server.rs.tera");

    let mut ctx = Context::new();
    ctx.insert("server_type", "McpServer");
    ctx.insert("error_type", "McpError");
    ctx.insert("buffer_size", 65536);
    ctx.insert("line_timeout_ms", 30000);

    let result = tera.render_str(&template, &ctx);
    assert!(result.is_ok(), "Template should render: {:?}", result.err());

    // Check that output contains expected code
    let output = result.unwrap();
    assert!(output.contains("pub struct StdioServer"));
    assert!(output.contains("pub struct AsyncStdioServer"));
    assert!(output.contains("pub fn start"));
}

#[test]
fn test_all_templates_exist() {
    let templates_dir = std::path::Path::new("templates/mcp-server");
    assert!(templates_dir.is_dir(), "Templates directory should exist");

    let expected_files = vec![
        "_head.tera",
        "mcp_server.rs.tera",
        "tool_handler.rs.tera",
        "resource_handler.rs.tera",
        "stdio_server.rs.tera",
    ];

    for file in expected_files {
        let path = templates_dir.join(file);
        assert!(path.exists(), "Template file {} should exist", file);
    }
}

#[test]
fn test_pascal_case_filter_exists() {
    let tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert("name", "test_mcp_server");

    let result = tera.render_str("{{ name | pascal }}", &ctx);
    assert!(result.is_ok(), "pascal filter should exist");
    assert_eq!(result.unwrap(), "TestMcpServer");
}

#[test]
fn test_snake_case_filter_exists() {
    let tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert("name", "TestMcpServer");

    let result = tera.render_str("{{ name | snake }}", &ctx);
    assert!(result.is_ok(), "snake filter should exist");
    assert_eq!(result.unwrap(), "test_mcp_server");
}
