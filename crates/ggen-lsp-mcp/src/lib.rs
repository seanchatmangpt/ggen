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

use std::path::PathBuf;
use std::sync::Arc;

use rmcp::{
    model::*, service::RequestContext, ErrorData as McpError, RoleServer, ServerHandler,
};
use schemars::JsonSchema;
use serde::Deserialize;

/// Max `file_content` accepted by the route tool (1 MiB) — a non-editor agent
/// must not be able to wedge the server with an unbounded payload.
const MAX_CONTENT_BYTES: usize = 1 << 20;
/// Max `file_path` length accepted.
const MAX_PATH_BYTES: usize = 4096;

/// Typed parameters for `ggen.lsp.repair_route`. The JSON schema advertised in
/// `list_tools` is DERIVED from this struct (single source of truth).
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct RepairRouteParams {
    /// Law-surface path; extension selects the analyzer (.ttl/.rq/.tera/ggen.toml).
    pub file_path: String,
    /// Full file content; analyzed in-memory.
    pub file_content: String,
    /// Optional project root for promoted-route pack discovery. Defaults to the
    /// current working directory — set this so route authority is explicit, not
    /// dependent on where the server process happens to run.
    #[serde(default)]
    pub root: Option<String>,
}

/// MCP server with the `ggen.lsp.repair_route` tool (+ replay/metrics in MCP-REPLAY-1).
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
    /// Construct the server with its (schemars-derived) tool schema.
    #[must_use]
    pub fn new() -> Self {
        let input_schema = serde_json::to_value(schemars::schema_for!(RepairRouteParams))
            .ok()
            .and_then(|v| v.as_object().cloned())
            .unwrap_or_default();
        let tool = Tool::new(
            "ggen.lsp.repair_route",
            "Analyze a ggen law-surface file and return the canonical RouteEnvelope \
             for each diagnostic — the same routes editors and the headless gate use.",
            Arc::new(input_schema),
        );
        Self {
            tools: Arc::new(vec![tool]),
        }
    }
}

/// Validate arguments and produce the route result, or a structured refusal.
/// Extracted from `call_tool` so the validation is unit-testable without an rmcp
/// `RequestContext`. Oversized/missing/garbled input → `McpError::invalid_params`
/// (never a panic, never string soup).
///
/// # Errors
/// Returns `McpError::invalid_params` for missing/oversized/garbled arguments.
pub fn repair_route_result(
    arguments: Option<serde_json::Map<String, serde_json::Value>>,
) -> Result<serde_json::Value, McpError> {
    let args = arguments.ok_or_else(|| McpError::invalid_params("missing arguments", None))?;
    let params: RepairRouteParams = serde_json::from_value(serde_json::Value::Object(args))
        .map_err(|e| McpError::invalid_params(format!("invalid params: {e}"), None))?;
    if params.file_path.is_empty() {
        return Err(McpError::invalid_params("empty 'file_path'", None));
    }
    if params.file_path.len() > MAX_PATH_BYTES {
        return Err(McpError::invalid_params("'file_path' too long", None));
    }
    if params.file_content.len() > MAX_CONTENT_BYTES {
        return Err(McpError::invalid_params(
            format!("'file_content' exceeds {MAX_CONTENT_BYTES} bytes"),
            None,
        ));
    }
    Ok(build_repair_routes_in(
        params.root.as_deref(),
        &params.file_path,
        &params.file_content,
    ))
}

/// Build the repair-route result for a file. Emits the canonical
/// [`ggen_lsp::RouteEnvelope`] per routed diagnostic (byte-equivalent to the LSP
/// CodeAction `data`, the headless gate, and the A2A bridge) plus the shared
/// [`ggen_lsp::RouteRefusal`] shape for uncovered diagnostics — so an agent never
/// reads an empty list as "all clear", and the route object is identical across
/// every channel.
#[must_use]
pub fn build_repair_routes(file_path: &str, file_content: &str) -> serde_json::Value {
    build_repair_routes_in(None, file_path, file_content)
}

/// Like [`build_repair_routes`], but loads the promoted-route pack from an explicit
/// project `root` (default: cwd). Root-aware discovery keeps route authority from
/// silently depending on the server's working directory.
#[must_use]
pub fn build_repair_routes_in(
    root: Option<&str>,
    file_path: &str,
    file_content: &str,
) -> serde_json::Value {
    let Some(analyzer) = ggen_lsp::analyzers::build_analyzer(file_path, file_content) else {
        return serde_json::json!({
            "file_path": file_path,
            "is_law_surface": false,
            "envelopes": [],
            "refusals": [],
        });
    };
    // Seeds + promoted routes (explicit root, default cwd) → same routes as editor/headless.
    let pack_root = root.map_or_else(|| PathBuf::from("."), PathBuf::from);
    let registry = ggen_lsp::RouteRegistry::seeded()
        .with_pack_routes(&ggen_lsp::route::default_pack_routes_path(&pack_root));
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
            tracing::info!(
                "OCEL: {}",
                serde_json::json!({
                    "event": "mcp.tool.invoked",
                    "objects": { "tool": "ggen.lsp.repair_route" }
                })
            );
            let result = repair_route_result(arguments)?;
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
