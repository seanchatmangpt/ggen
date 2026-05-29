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

use std::path::{Path, PathBuf};
use std::sync::Arc;

use rmcp::{model::*, service::RequestContext, ErrorData as McpError, RoleServer, ServerHandler};
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
    /// dependent on where the server process happens to run. When set, the request
    /// also leaves attributed field evidence (transport=mcp).
    #[serde(default)]
    pub root: Option<String>,
    /// Optional agent identity for field-evidence attribution (default "mcp").
    #[serde(default)]
    pub agent_id: Option<String>,
}

/// Parameters for `ggen.lsp.replay_case`.
#[derive(Debug, Clone, Default, Deserialize, JsonSchema)]
pub struct ReplayCaseParams {
    /// Project root (default: cwd).
    #[serde(default)]
    pub root: Option<String>,
    /// Episode/case id to reconstruct. When omitted, the promotion binding is
    /// verified instead (tamper check).
    #[serde(default)]
    pub case_id: Option<String>,
}

/// Parameters for `ggen.lsp.metrics`.
#[derive(Debug, Clone, Default, Deserialize, JsonSchema)]
pub struct MetricsParams {
    /// Project root (default: cwd).
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
    /// Construct the server with its three (schemars-derived) tools: route +
    /// replay + metrics — the full route+proof+replay surface, not route vending.
    #[must_use]
    pub fn new() -> Self {
        let tools = vec![
            make_tool(
                "ggen.lsp.repair_route",
                "Analyze a ggen law-surface file and return the canonical RouteEnvelope \
                 for each diagnostic — the same routes editors and the headless gate use.",
                serde_json::to_value(schemars::schema_for!(RepairRouteParams)).unwrap_or_default(),
            ),
            make_tool(
                "ggen.lsp.replay_case",
                "Reconstruct a recorded episode (case_id) from the project's OCEL log + \
                 receipts — or verify the promotion binding (tamper check) when case_id is omitted.",
                serde_json::to_value(schemars::schema_for!(ReplayCaseParams)).unwrap_or_default(),
            ),
            make_tool(
                "ggen.lsp.metrics",
                "Compute the IMPROVE-1 metrics + earned verdict for the project \
                 (insufficient_evidence where the backing events are absent).",
                serde_json::to_value(schemars::schema_for!(MetricsParams)).unwrap_or_default(),
            ),
        ];
        Self {
            tools: Arc::new(tools),
        }
    }

    /// Serve the route tools over stdio (the transport editors and non-editor
    /// agents use). This is what `ggen lsp serve --protocol mcp` launches.
    ///
    /// # Errors
    /// Returns an error if the stdio transport fails to initialize or serve.
    pub async fn start_stdio() -> anyhow::Result<()> {
        use rmcp::ServiceExt;
        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
        let running = Self::new().serve((stdin, stdout)).await?;
        running.waiting().await?;
        Ok(())
    }
}

/// Build an rmcp `Tool` from a name, description, and a schemars-derived schema value.
fn make_tool(name: &'static str, description: &'static str, schema: serde_json::Value) -> Tool {
    Tool::new(
        name,
        description,
        Arc::new(schema.as_object().cloned().unwrap_or_default()),
    )
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
    let params = parse_repair_params(arguments)?;
    Ok(build_repair_routes_in(
        params.root.as_deref(),
        &params.file_path,
        &params.file_content,
    ))
}

/// Parse + validate the repair-route arguments into typed params.
fn parse_repair_params(
    arguments: Option<serde_json::Map<String, serde_json::Value>>,
) -> Result<RepairRouteParams, McpError> {
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
    Ok(params)
}

/// Like [`repair_route_result`], but ALSO leaves attributed field evidence
/// (transport=mcp) when a real `root` is supplied — the field-evidence gauge so
/// MCP route requests during daily work accumulate into the OCEL log. Pure
/// projection (no `root`) stays side-effect-free. This is what the live server's
/// `call_tool` invokes.
///
/// # Errors
/// Returns `McpError::invalid_params` for missing/oversized/garbled arguments.
pub fn repair_route_captured(
    arguments: Option<serde_json::Map<String, serde_json::Value>>,
) -> Result<serde_json::Value, McpError> {
    let params = parse_repair_params(arguments)?;
    let result = build_repair_routes_in(
        params.root.as_deref(),
        &params.file_path,
        &params.file_content,
    );
    if let Some(root) = params.root.as_deref() {
        let agent = params.agent_id.as_deref().unwrap_or("mcp");
        ggen_lsp::capture_request(
            Path::new(root),
            &params.file_path,
            &params.file_content,
            &ggen_lsp::Attribution::request(agent, "mcp"),
        );
    }
    Ok(result)
}

/// Validate a `root` path input the same way [`parse_repair_params`] validates
/// `file_path`: an empty value (including the empty-string default once the caller
/// has applied defaulting) is refused, and an over-length value is refused with the
/// same [`MAX_PATH_BYTES`] bound. Closes the asymmetric fail-open where
/// `replay_case`/`metrics` accepted any `root` while `repair_route` bounded its
/// path. Returns the validated `root` so the happy path is unchanged.
///
/// # Errors
/// Returns `McpError::invalid_params` for an empty or oversized `root`.
fn validate_root(root: String) -> Result<String, McpError> {
    if root.is_empty() {
        return Err(McpError::invalid_params("empty 'root'", None));
    }
    if root.len() > MAX_PATH_BYTES {
        return Err(McpError::invalid_params("'root' too long", None));
    }
    Ok(root)
}

/// Validate an optional `case_id` the same way `repair_route` bounds its path
/// inputs: when present it must be non-empty and within [`MAX_PATH_BYTES`].
/// Absent stays absent (verify-promotion path).
///
/// # Errors
/// Returns `McpError::invalid_params` for an empty or oversized `case_id`.
fn validate_case_id(case_id: Option<String>) -> Result<Option<String>, McpError> {
    if let Some(id) = case_id.as_deref() {
        if id.is_empty() {
            return Err(McpError::invalid_params("empty 'case_id'", None));
        }
        if id.len() > MAX_PATH_BYTES {
            return Err(McpError::invalid_params("'case_id' too long", None));
        }
    }
    Ok(case_id)
}

/// Replay a recorded episode (or verify the promotion binding when `case_id` is
/// omitted). MCP agents can challenge the route/result after the fact, not just
/// receive routes.
///
/// # Errors
/// Returns `McpError::invalid_params` for garbled, empty, or oversized arguments
/// (`root`/`case_id`) — symmetric with `repair_route`'s path-input discipline.
pub fn replay_case_result(
    arguments: Option<serde_json::Map<String, serde_json::Value>>,
) -> Result<serde_json::Value, McpError> {
    let params: ReplayCaseParams = match arguments {
        Some(a) => serde_json::from_value(serde_json::Value::Object(a))
            .map_err(|e| McpError::invalid_params(format!("invalid params: {e}"), None))?,
        None => ReplayCaseParams::default(),
    };
    let root = validate_root(params.root.unwrap_or_else(|| ".".to_string()))?;
    let case_id = validate_case_id(params.case_id)?;
    let value = match case_id {
        Some(id) => serde_json::to_value(ggen_lsp::replay_case(Path::new(&root), &id)),
        None => serde_json::to_value(ggen_lsp::verify_promotion(Path::new(&root))),
    };
    value.map_err(|e| McpError::internal_error(format!("serialization failed: {e}"), None))
}

/// Compute the IMPROVE-1 metrics + earned verdict for the project. Metrics lacking
/// their backing events report `insufficient_evidence`; the verdict is `improving`
/// only when the evidence earns it.
///
/// # Errors
/// Returns `McpError::invalid_params` for garbled, empty, or oversized `root`
/// arguments — symmetric with `repair_route`'s path-input discipline.
pub fn metrics_result(
    arguments: Option<serde_json::Map<String, serde_json::Value>>,
) -> Result<serde_json::Value, McpError> {
    let params: MetricsParams = match arguments {
        Some(a) => serde_json::from_value(serde_json::Value::Object(a))
            .map_err(|e| McpError::invalid_params(format!("invalid params: {e}"), None))?,
        None => MetricsParams::default(),
    };
    let root = validate_root(params.root.unwrap_or_else(|| ".".to_string()))?;
    serde_json::to_value(ggen_lsp::compute_metrics(Path::new(&root)))
        .map_err(|e| McpError::internal_error(format!("serialization failed: {e}"), None))
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
    root: Option<&str>, file_path: &str, file_content: &str,
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
    // Bind the route response to the specific pack it was served from (if any),
    // so a remote agent's result is provenance-anchored.
    let pack_hash = ggen_lsp::pack_hash_at(&pack_root);
    serde_json::json!({
        "file_path": file_path,
        "is_law_surface": true,
        "pack_hash": pack_hash,
        "envelopes": envelopes,
        "refusals": refusals,
    })
}

impl ServerHandler for RepairRouteServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
            .with_protocol_version(ProtocolVersion::V_2024_11_05)
            .with_server_info(Implementation::new("ggen-lsp-mcp", "26.5.28"))
            .with_instructions("ggen-lsp repair-route MCP server")
    }

    async fn list_tools(
        &self, _request: Option<PaginatedRequestParams>, _ctx: RequestContext<RoleServer>,
    ) -> Result<ListToolsResult, McpError> {
        Ok(ListToolsResult {
            tools: (*self.tools).clone(),
            next_cursor: None,
            meta: None,
        })
    }

    async fn call_tool(
        &self,
        CallToolRequestParams {
            name, arguments, ..
        }: CallToolRequestParams,
        _ctx: RequestContext<RoleServer>,
    ) -> Result<CallToolResult, McpError> {
        tracing::info!(
            "OCEL: {}",
            serde_json::json!({
                "event": "mcp.tool.invoked",
                "objects": { "tool": name }
            })
        );
        let result = match name.as_ref() {
            // The live server captures field evidence (transport=mcp) when a
            // root is given; the pure repair_route_result stays for parity tests.
            "ggen.lsp.repair_route" => repair_route_captured(arguments)?,
            "ggen.lsp.replay_case" => replay_case_result(arguments)?,
            "ggen.lsp.metrics" => metrics_result(arguments)?,
            other => {
                return Err(McpError::invalid_params(
                    format!("unknown tool: {other}"),
                    None,
                ))
            }
        };
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result).unwrap_or_default(),
        )]))
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
        assert!(
            !envelopes.is_empty(),
            "the invalid enum value should be routed (advisory)"
        );
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
