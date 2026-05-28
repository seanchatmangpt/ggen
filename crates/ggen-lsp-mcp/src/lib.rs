//! MCP server exposing the ggen-lsp repair-route engine as a tool.
//!
//! This is the third delivery channel (after editor CodeActions and the headless
//! `ggen lsp check --with-routes` JSON): non-LSP coding agents call the
//! `ggen.lsp.repair_route` MCP tool to get POWL `RoutePlan`s for a file's
//! diagnostics. The SAME `ggen_lsp` route engine backs all three channels.
//!
//! Lives in a dedicated leaf crate (depends on `ggen-lsp`, nothing depends back)
//! so it sidesteps the `ggen-core → ggen-a2a-mcp` cycle that blocks adding the
//! tool to `ggen-a2a-mcp` directly. It can be bridged into A2A via
//! `a2a-mcp::AgentToMcpBridge` if A2A exposure is wanted.

use std::sync::Arc;

use rmcp::{
    model::*, service::RequestContext, ErrorData as McpError, RoleServer, ServerHandler,
};

/// MCP server with the single `ggen.lsp.repair_route` tool.
#[derive(Clone)]
pub struct RepairRouteServer {
    tools: Arc<Vec<Tool>>,
}

impl Default for RepairRouteServer {
    fn default() -> Self {
        Self::new()
    }
}

impl RepairRouteServer {
    /// Construct the server with its tool schema.
    #[must_use]
    pub fn new() -> Self {
        let input_schema = serde_json::from_value(serde_json::json!({
            "type": "object",
            "properties": {
                "file_path": { "type": "string", "description": "Law-surface path; extension selects the analyzer (.ttl/.rq/.tera/ggen.toml)" },
                "file_content": { "type": "string", "description": "Full file content; analyzed in-memory" }
            },
            "required": ["file_path", "file_content"]
        }))
        .unwrap_or_default();
        let tool = Tool::new(
            "ggen.lsp.repair_route",
            "Analyze a ggen law-surface file and return POWL repair routes (RoutePlan) \
             for its diagnostics — the same routes editors and the headless gate use.",
            Arc::new(input_schema),
        );
        Self {
            tools: Arc::new(vec![tool]),
        }
    }
}

/// Build the repair-route result for a file. Emits the canonical
/// [`ggen_lsp::RouteEnvelope`] per routed diagnostic (byte-equivalent to the LSP
/// CodeAction `data`, the headless gate, and the A2A bridge) plus the shared
/// [`ggen_lsp::RouteRefusal`] shape for uncovered diagnostics — so an agent never
/// reads an empty list as "all clear", and the route object is identical across
/// every channel.
#[must_use]
pub fn build_repair_routes(file_path: &str, file_content: &str) -> serde_json::Value {
    let Some(analyzer) = ggen_lsp::analyzers::build_analyzer(file_path, file_content) else {
        return serde_json::json!({
            "file_path": file_path,
            "is_law_surface": false,
            "envelopes": [],
            "refusals": [],
        });
    };
    // Seeds + promoted routes (cwd = project root) → same routes as editor/headless.
    let registry = ggen_lsp::RouteRegistry::seeded()
        .with_pack_routes(&ggen_lsp::route::default_pack_routes_path(std::path::Path::new(".")));
    let diagnostics = analyzer.diagnostics();
    let mut envelopes = Vec::new();
    let mut refusals = Vec::new();
    for d in &diagnostics {
        match ggen_lsp::envelope_for_diagnostic(&registry, d, file_content, file_path) {
            Some(env) => envelopes.push(env),
            None => refusals.push(ggen_lsp::RouteRefusal::from_target(
                &ggen_lsp::route::DiagnosticRef::from_diagnostic(d),
            )),
        }
    }
    serde_json::json!({
        "file_path": file_path,
        "is_law_surface": true,
        "envelopes": envelopes,
        "refusals": refusals,
    })
}

impl ServerHandler for RepairRouteServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
            .with_protocol_version(ProtocolVersion::V_2024_11_05)
            .with_server_info(Implementation::new("ggen-lsp-mcp", "26.5.21"))
            .with_instructions("ggen-lsp repair-route MCP server")
    }

    fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<ListToolsResult, McpError>> + Send + '_ {
        async move {
            Ok(ListToolsResult {
                tools: (*self.tools).clone(),
                next_cursor: None,
                meta: None,
            })
        }
    }

    fn call_tool(
        &self,
        CallToolRequestParams {
            name, arguments, ..
        }: CallToolRequestParams,
        _ctx: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<CallToolResult, McpError>> + Send + '_ {
        async move {
            if name != "ggen.lsp.repair_route" {
                return Err(McpError::invalid_params(format!("unknown tool: {name}"), None));
            }
            let args =
                arguments.ok_or_else(|| McpError::invalid_params("missing arguments", None))?;
            let file_path = args
                .get("file_path")
                .and_then(|v| v.as_str())
                .ok_or_else(|| McpError::invalid_params("missing 'file_path'", None))?;
            let file_content = args
                .get("file_content")
                .and_then(|v| v.as_str())
                .ok_or_else(|| McpError::invalid_params("missing 'file_content'", None))?;

            tracing::info!(
                "OCEL: {}",
                serde_json::json!({
                    "event": "mcp.tool.invoked",
                    "objects": { "tool": "ggen.lsp.repair_route", "file": file_path }
                })
            );

            let result = build_repair_routes(file_path, file_content);
            Ok(CallToolResult::success(vec![Content::text(
                serde_json::to_string_pretty(&result).unwrap_or_default(),
            )]))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repair_routes_for_invalid_enum() {
        let v = build_repair_routes("ggen.toml", "[logging]\nlevel = \"verbose\"\n");
        assert_eq!(v["is_law_surface"], serde_json::json!(true));
        let envelopes = v["envelopes"].as_array().expect("envelopes array");
        assert!(!envelopes.is_empty(), "the invalid enum value should be routed (advisory)");
        // The envelope carries the canonical contract fields.
        let e = &envelopes[0];
        assert_eq!(e["diagnostic_code"], serde_json::json!("E0023"));
        assert!(e["case_id"].as_str().is_some_and(|s| s.starts_with("c:")));
        assert!(e["route_id"].as_str().is_some());
        assert!(e["compact_trace"].is_object());
    }

    #[test]
    fn ggen_does_not_route_llm_sections() {
        // No diagnostics for an [ai] section → no envelopes; ggen isn't in LLM config.
        let v = build_repair_routes("ggen.toml", "[ai]\nprovider = \"openai\"\n");
        assert_eq!(v["envelopes"].as_array().map(Vec::len), Some(0));
    }

    #[test]
    fn non_law_surface_is_flagged() {
        let v = build_repair_routes("notes.md", "# hi");
        assert_eq!(v["is_law_surface"], serde_json::json!(false));
    }
}
