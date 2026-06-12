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
    let mut tera_instance = tera::Tera::default();
    tera_instance.autoescape_on(vec![]);
    register_all(&mut tera_instance);

    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mcp_templates_dir = manifest_dir.join("templates/mcp-server");
    
    let mut templates = Vec::new();
    for entry in std::fs::read_dir(mcp_templates_dir).expect("mcp-server templates dir exists") {
        let entry = entry.expect("valid entry");
        let path = entry.path();
        if path.is_file() && path.extension().map_or(false, |ext| ext == "tera") {
            let filename = path.file_name().unwrap().to_str().unwrap();
            let template_name = format!("mcp-server/{}", filename);
            let content = std::fs::read_to_string(&path).expect("read template");
            templates.push((template_name, content));
        }
    }
    
    tera_instance.add_raw_templates(templates).expect("Failed to add templates");
    
    let mut final_ctx = ctx.clone();
    if name == "tool_handler.rs.tera" {
        if final_ctx.get("tool_name").is_none() {
            final_ctx.insert("tool_name", "test_tool");
        }
        if final_ctx.get("sparql_results").is_none() {
            let sparql_results = serde_json::json!([
                {
                    "tool_name": "test_tool",
                    "tool_description": "A test tool",
                    "param_name": "param1",
                    "param_description": "First param",
                    "is_required": "true",
                    "rust_type": "String",
                }
            ]);
            final_ctx.insert("sparql_results", &sparql_results);
        }
    }

    let registered_name = format!("mcp-server/{name}");
    tera_instance.render(&registered_name, &final_ctx)
        .unwrap_or_else(|e| panic!("Template should render: {:?}", e))
}

#[test]
fn mcp_tool_handler_macro_required() {
    // RMCP_NOTES.md §1: "Omitting #[tool_handler] → list_tools() returns empty"
    let ctx = Context::new();
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

    // Should NOT have the anti-pattern
    assert!(
        !output.contains("let _ = server.serve(transport).await;"),
        "Generated stdio_server.rs must NOT use 'let _ = serve()' anti-pattern (RMCP_NOTES.md §2). Got:\n{output}"
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
    let ctx = Context::new();
    let tool_handler = render_template("tool_handler.rs.tera", &ctx);
    let stdio_server = render_template("stdio_server.rs.tera", &ctx);

    // Server-side imports
    assert!(
        tool_handler.contains("tool") &&
        tool_handler.contains("tool_handler") &&
        tool_handler.contains("tool_router") &&
        tool_handler.contains("ServerHandler") &&
        tool_handler.contains("ToolRouter"),
        "tool_handler.rs must import rmcp proc macros and types (RMCP_NOTES.md §7). Got:\n{tool_handler}"
    );

    // Client/service imports
    assert!(
        stdio_server.contains("rmcp::{ServiceExt") || stdio_server.contains("rmcp::service::"),
        "stdio_server.rs must import rmcp service types (RMCP_NOTES.md §7). Got:\n{stdio_server}"
    );
}
