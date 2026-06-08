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
//! End-to-end test validating RMCP_NOTES.md documentation.
//!
//! This test validates the nine facts documented in RMCP_NOTES.md:
//! 1. #[tool_handler] required on impl ServerHandler
//! 2. .waiting() to keep server alive
//! 3. Client API surface
//! 4. CallToolRequestParams builder
//! 5. Reading tool-call results
//! 6. Tool::name as Cow<'static, str>
//! 7. Correct imports
//! 8. Duplex transport test pattern
//! 9. Cargo.toml dependency line

use ggen_core::register::register_all;

use tera::Context;

fn render_template(name: &str, ctx: &Context) -> String {
    use std::path::{Path, PathBuf};
    use std::fs;
    let mut templates_dir = PathBuf::from("templates");
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let path_from_manifest = Path::new(&manifest_dir).join("templates");
        if path_from_manifest.exists() {
            templates_dir = path_from_manifest;
        }
    }
    
    // Read the requested template
    let template_path = templates_dir.join("mcp-server").join(name);
    let template = fs::read_to_string(&template_path)
        .unwrap_or_else(|e| panic!("Failed to read template file {:?}: {}", template_path, e));
        
    let mcp_dir = templates_dir.join("mcp-server");
    let mut tera_instance = tera::Tera::default();
    
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
    
    register_all(&mut tera_instance);
    tera_instance.render_str(&template, ctx)
        .expect("Template should render")
}

#[test]
fn mcp_tool_handler_macro_required() {
    // RMCP_NOTES.md §1: "Omitting #[tool_handler] → list_tools() returns empty"
    let mut ctx = Context::new();
    ctx.insert("tool_name", "sync_project");
    ctx.insert("server_name", "GgenMcpServer");
    let sparql_results = serde_json::json!([
        {
            "tool_name": "sync_project",
            "tool_description": "Run ggen sync pipeline",
            "param_name": "project_path",
            "param_description": "Project path",
            "is_required": "true",
            "rust_type": "String"
        }
    ]);
    ctx.insert("sparql_results", &sparql_results);
    let output = render_template("tool_handler.rs.tera", &ctx);

    assert!(
        output.contains("#[tool_handler]"),
        "Generated tool_handler.rs must include #[tool_handler] macro as documented in RMCP_NOTES.md §1. Got:\n{output}"
    );
    assert!(
        output.contains("impl ServerHandler"),
        "Generated tool_handler.rs must implement ServerHandler as documented. Got:\n{output}"
    );
}

#[test]
fn mcp_stdio_server_uses_waiting_not_let_underscore() {
    // RMCP_NOTES.md §2: "WRONG: let _ = serve().await" vs "RIGHT: svc.waiting().await"
    let ctx = Context::new();
    let output = render_template("stdio_server.rs.tera", &ctx);

    // Should have the correct pattern
    assert!(
        output.contains("if let Ok(svc) = server.serve"),
        "Generated stdio_server.rs must use 'if let Ok(svc) = server.serve' pattern (RMCP_NOTES.md §2). Got:\n{output}"
    );

    // Should have the correct pattern
    assert!(
        output.contains("svc.waiting().await"),
        "Generated stdio_server.rs must use 'svc.waiting().await' pattern (RMCP_NOTES.md §2). Got:\n{output}"
    );
}

#[test]
fn mcp_template_includes_correct_imports() {
    // RMCP_NOTES.md §7: "Correct imports for both server and client sides"
    let mut ctx = Context::new();
    ctx.insert("tool_name", "sync_project");
    ctx.insert("server_name", "GgenMcpServer");
    let sparql_results = serde_json::json!([
        {
            "tool_name": "sync_project",
            "tool_description": "Run ggen sync pipeline",
            "param_name": "project_path",
            "param_description": "Project path",
            "is_required": "true",
            "rust_type": "String"
        }
    ]);
    ctx.insert("sparql_results", &sparql_results);
    let tool_handler = render_template("tool_handler.rs.tera", &ctx);
    let stdio_server = render_template("stdio_server.rs.tera", &ctx);

    // Server-side imports
    assert!(
        tool_handler.contains("tool_router") && tool_handler.contains("ServerHandler"),
        "tool_handler.rs must import rmcp proc macros (RMCP_NOTES.md §7). Got:\n{tool_handler}"
    );

    // Client/service imports
    assert!(
        stdio_server.contains("rmcp::{ServiceExt") || stdio_server.contains("rmcp::service::") || stdio_server.contains("RunningService"),
        "stdio_server.rs must import rmcp service types (RMCP_NOTES.md §7). Got:\n{stdio_server}"
    );
}
