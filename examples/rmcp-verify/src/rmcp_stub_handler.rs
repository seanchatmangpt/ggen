//! A REAL, compiling `impl rmcp::ServerHandler` exercising the macro-based
//! subset of `rmcp-pack`'s cataloged surface against the actual `rmcp`
//! crate -- not a mock, not a subprocess shim (rmcp ships no binary to
//! shell out to, unlike mcpp-pack's `mcpp_dispatch.rs`).
//!
//! This exact `#[tool_router]`/`#[tool]`/`#[tool_handler]` invocation
//! pattern was proven live during `rmcp-pack`'s grounding pass: compiled
//! and run with this workspace's own pinned nightly, `RmcpStubHandler::
//! tool_router().list_all()` returned a real `rmcp::model::Tool { name:
//! "echo", description: Some("Echo back the input string"), .. }`, not a
//! placeholder. See `tests/rmcp_stub_handler_proof.rs` (same pack) for that
//! exact assertion, generated alongside this module.
//!
//! Deliberately NOT mechanically derived per-`RMCP_MACROS`-row: there is no
//! ontology-modeled per-row business logic to project (rmcp:Macro records
//! each macro's real doc-table fields, not an application's tool
//! semantics) -- inventing one here would fabricate behavior the real
//! source does not specify. What IS ontology-driven below are the doc
//! citations naming exactly which real macro (and its real source
//! location) each attribute below corresponds to.
//!
//! - `tool` — rmcp-macros-2.2.0/src/lib.rs:37-42 (doc 14-36); target AST confirmed via src/tool.rs:222 syn::parse2::<ImplItemFn>
//! - `tool_handler` — rmcp-macros-2.2.0/src/lib.rs:194-199 (doc 133-193); target AST confirmed via src/tool_handler.rs:42 syn::parse2::<ItemImpl>; generates call_tool/list_tools/get_tool/get_info
//! - `tool_router` — rmcp-macros-2.2.0/src/lib.rs:126-131 (doc 44-125); target AST confirmed via src/tool_router.rs:39 syn::parse2::<ItemImpl>


use rmcp::{ServerHandler, model::ServerInfo, tool, tool_handler, tool_router};

/// Minimal real MCP server handler: one `#[tool]`-annotated method, routed
/// by a `#[tool_router]`-generated `ToolRouter`, wired into
/// `rmcp::ServerHandler::{call_tool,list_tools,get_tool}` by
/// `#[tool_handler]`.
#[derive(Debug, Clone, Default)]
pub struct RmcpStubHandler;

// `vis = "pub"` is required (not decorative): the real default is private
// visibility (rmcp-macros' own documented `vis` field on `#[tool_router]`,
// "Defaults to empty" -- see RMCP_MACROS' macro-tool-router row), which
// compiles fine when called from within this same module but fails
// E0624 ("private associated function") from `tests/rmcp_stub_handler_proof.rs`,
// a separate integration-test binary. Found live during this pack's own
// `cargo test` verification pass, not assumed.
#[tool_router(vis = "pub")]
impl RmcpStubHandler {
    /// Echoes a fixed string back to the caller -- proves the tool actually
    /// dispatches through the real `rmcp` call path, not just that it is
    /// listed.
    #[tool(description = "Echo back the input string")]
    async fn echo(&self) -> String {
        "hello-from-real-rmcp".to_string()
    }
}

#[tool_handler]
impl ServerHandler for RmcpStubHandler {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::default()
    }
}
