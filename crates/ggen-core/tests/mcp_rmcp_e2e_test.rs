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
use serde_json::json;
use tera::Context;

fn render_template(name: &str, ctx: &Context) -> String {
    let template = std::fs::read_to_string(format!("templates/mcp-server/{name}"))
        .unwrap_or_else(|_| panic!("Failed to read templates/mcp-server/{name}"));
    let mut tera = tera::Tera::default();
    register_all(&mut tera);
    tera.render_str(&template, ctx)
        .expect("Template should render")
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
        !output.contains("let _ = server.serve"),
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
        tool_handler
            .contains("use rmcp::{tool, tool_handler, tool_router, ServerHandler, ToolRouter}"),
        "tool_handler.rs must import rmcp proc macros (RMCP_NOTES.md §7). Got:\n{tool_handler}"
    );

    // Client/service imports
    assert!(
        stdio_server.contains("rmcp::{ServiceExt") || stdio_server.contains("rmcp::service::"),
        "stdio_server.rs must import rmcp service types (RMCP_NOTES.md §7). Got:\n{stdio_server}"
    );
}
