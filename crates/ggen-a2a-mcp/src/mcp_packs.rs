//! Pack + marketplace tools for the agent surface (MCP and A2A).
//!
//! This module is the wire-protocol layer over [`ggen_core::agent::PackAgent`].
//! It follows the project's no-drift discipline: every operation is a single
//! *pure result function* (`*_result`) returning `serde_json::Value`, and both
//! transports call the same function —
//!
//! - the **MCP** tools in `mcp_server.rs` wrap the result in a `CallToolResult`;
//! - the **A2A** [`PackToolsAdapter`] wraps it via [`dispatch_pack_tool`].
//!
//! There is therefore exactly one implementation of each pack operation; the
//! MCP and A2A surfaces cannot disagree. Inputs are typed (`schemars`-derived
//! JSON Schemas) and outputs are the structured, evidence-bearing
//! [`ggen_core::agent::types`] contract. Failures are surfaced as typed errors,
//! never swallowed into a fake success.

use std::collections::HashMap;

use async_trait::async_trait;
use ggen_core::agent::{AgentError, InstallRequest, PackAgent};
use rmcp::model::{CallToolResult, Content, ErrorData};
use serde::Deserialize;
use serde_json::{json, Value};

use crate::a2a_generated::adapter::{Adapter, AdapterCapabilities, AdapterError, AdapterErrorType};

/// The pack tool names this module exposes, in invocation order. Shared by the
/// MCP router registration and the A2A agent card so the two never diverge.
pub const PACK_TOOLS: &[&str] = &[
    "ggen.packs.capabilities",
    "ggen.packs.search",
    "ggen.packs.list",
    "ggen.packs.show",
    "ggen.packs.resolve",
    "ggen.packs.compatibility",
    "ggen.packs.status",
    "ggen.packs.verify",
    "ggen.packs.install",
    "ggen.packs.remove",
];

// ── Tool parameter schemas ──────────────────────────────────────────────────

fn default_true() -> bool {
    true
}

/// Parameters for `ggen.packs.search`.
#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct PackSearchParams {
    /// Free-text query matched against pack name, id, and description.
    pub query: String,
    /// Maximum number of hits to return (default 20).
    #[serde(default)]
    pub limit: Option<usize>,
}

/// Parameters for `ggen.packs.list`.
#[derive(Debug, Clone, Default, Deserialize, schemars::JsonSchema)]
pub struct PackListParams {
    /// Optional category filter.
    #[serde(default)]
    pub category: Option<String>,
}

/// Parameters for `ggen.packs.show`.
#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct PackShowParams {
    /// Pack identifier (e.g. `mcp-rust`).
    pub pack_id: String,
}

/// Parameters for `ggen.packs.resolve`.
#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct PackResolveParams {
    /// Capability surface (e.g. `mcp`, `web`, `devops`).
    pub surface: String,
    /// Optional projection narrowing (e.g. `rust`, `fullstack`).
    #[serde(default)]
    pub projection: Option<String>,
    /// Optional runtime narrowing (e.g. `stdio`, `axum`).
    #[serde(default)]
    pub runtime: Option<String>,
}

/// Parameters for `ggen.packs.compatibility`.
#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct PackCompatibilityParams {
    /// The set of pack IDs to check for composition conflicts.
    pub pack_ids: Vec<String>,
}

/// Parameters for `ggen.packs.status`.
#[derive(Debug, Clone, Default, Deserialize, schemars::JsonSchema)]
pub struct PackStatusParams {
    /// Project root to read `.ggen/packs.lock` from (default: current dir).
    #[serde(default)]
    pub root: Option<String>,
}

/// Parameters for `ggen.packs.verify`.
#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct PackVerifyParams {
    /// Path to the receipt JSON to verify.
    pub receipt_path: String,
    /// Project root holding `.ggen/keys/public.pem` (default: current dir).
    #[serde(default)]
    pub root: Option<String>,
}

/// Parameters for `ggen.packs.install`.
#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct PackInstallParams {
    /// Pack identifier to install.
    pub pack_id: String,
    /// Reinstall over an existing install path.
    #[serde(default)]
    pub force: bool,
    /// Preview only: resolve and report without writing the lockfile/receipt.
    #[serde(default)]
    pub dry_run: bool,
    /// Emit a signed provenance receipt on success (ignored for `dry_run`).
    #[serde(default = "default_true")]
    pub emit_receipt: bool,
}

/// Parameters for `ggen.packs.remove`.
#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct PackRemoveParams {
    /// Pack identifier to remove from the project lockfile.
    pub pack_id: String,
}

/// Parameters for `ggen.packs.capabilities` (no inputs).
#[derive(Debug, Clone, Default, Deserialize, schemars::JsonSchema)]
pub struct PackCapabilitiesParams {}

// ── Pure result functions (the single implementation per operation) ─────────

fn to_value<T: serde::Serialize>(v: &T) -> Result<Value, AgentError> {
    serde_json::to_value(v).map_err(|e| AgentError::Internal(e.to_string()))
}

fn agent_at(root: Option<&str>) -> Result<PackAgent, AgentError> {
    match root {
        Some(r) => Ok(PackAgent::at_root(r)),
        None => PackAgent::new(),
    }
}

/// `ggen.packs.capabilities` — describe operations + capability surfaces.
pub fn capabilities_result(_p: PackCapabilitiesParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    to_value(&agent.capabilities())
}

/// `ggen.packs.search` — relevance-ranked registry search.
pub fn search_result(p: PackSearchParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    let hits = agent.search(&p.query, p.limit)?;
    Ok(json!({
        "query": p.query,
        "total": hits.len(),
        "results": to_value(&hits)?,
    }))
}

/// `ggen.packs.list` — list registry packs, optionally by category.
pub fn list_result(p: PackListParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    let packs = agent.list(p.category.as_deref())?;
    Ok(json!({
        "total": packs.len(),
        "packs": to_value(&packs)?,
    }))
}

/// `ggen.packs.show` — full detail + validation for one pack.
pub fn show_result(p: PackShowParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    to_value(&agent.show(&p.pack_id)?)
}

/// `ggen.packs.resolve` — capability surface → concrete pack IDs.
pub fn resolve_result(p: PackResolveParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    let outcome =
        agent.resolve_capability(&p.surface, p.projection.as_deref(), p.runtime.as_deref())?;
    to_value(&outcome)
}

/// `ggen.packs.compatibility` — check whether a set of packs composes without
/// conflicts. Async: loading each pack's metadata performs real I/O.
pub async fn compatibility_result(p: PackCompatibilityParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    to_value(&agent.check_compatibility(&p.pack_ids).await?)
}

/// `ggen.packs.status` — installed packs from the project lockfile.
pub fn status_result(p: PackStatusParams) -> Result<Value, AgentError> {
    let agent = agent_at(p.root.as_deref())?;
    to_value(&agent.status()?)
}

/// `ggen.packs.verify` — verify a provenance receipt (fail-closed).
pub fn verify_result(p: PackVerifyParams) -> Result<Value, AgentError> {
    let agent = agent_at(p.root.as_deref())?;
    to_value(&agent.verify(&p.receipt_path))
}

/// `ggen.packs.install` — install a pack, writing the lockfile and emitting a
/// signed receipt. Async: the underlying pipeline performs real I/O.
pub async fn install_result(p: PackInstallParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    let req = InstallRequest {
        pack_id: p.pack_id,
        force: p.force,
        dry_run: p.dry_run,
        emit_receipt: p.emit_receipt,
    };
    to_value(&agent.install(req).await?)
}

/// `ggen.packs.remove` — remove a pack from the project lockfile (fail-closed).
pub fn remove_result(p: PackRemoveParams) -> Result<Value, AgentError> {
    let agent = PackAgent::new()?;
    to_value(&agent.remove(&p.pack_id)?)
}

// ── MCP helpers ─────────────────────────────────────────────────────────────

/// Log an OCEL boundary-crossing event for a pack tool invocation, mirroring the
/// `ggen.construct` tool. Object identity is the tool plus its primary subject.
pub fn ocel_invoked(tool: &str, subject: &str) {
    tracing::info!(
        "OCEL: {}",
        json!({
            "event": "a2a.mcp.tool.invoked",
            "objects": { "tool": tool, "pack": subject }
        })
    );
}

/// Wrap a successful result `Value` as an MCP `CallToolResult`: pretty JSON text
/// plus a `ggen_result` meta payload carrying the structured value.
pub fn mcp_ok(value: Value) -> CallToolResult {
    let text = serde_json::to_string_pretty(&value).unwrap_or_else(|_| value.to_string());
    let mut meta = rmcp::model::Meta::default();
    meta.insert("ggen_result".to_string(), value);
    CallToolResult::success(vec![Content::text(text)]).with_meta(Some(meta))
}

/// Map a facade [`AgentError`] to an MCP [`ErrorData`], preserving the structured
/// `{kind, detail}` body so an agent can branch on the failure mode. Client-side
/// errors map to `invalid_params`; everything else to `internal_error`.
pub fn mcp_err(e: AgentError) -> ErrorData {
    let data = serde_json::to_value(&e).ok();
    match e {
        AgentError::InvalidRequest(_)
        | AgentError::PackNotFound(_)
        | AgentError::NotInstalled(_) => ErrorData::invalid_params(e.to_string(), data),
        _ => ErrorData::internal_error(e.to_string(), data),
    }
}

// ── A2A surface (same functions, second transport) ──────────────────────────

/// The A2A agent card advertising the pack tools. Mirrors the MCP tool set
/// exactly (`PACK_TOOLS`) so an A2A host and an MCP host see the same surface.
#[must_use]
pub fn pack_agent_card() -> Value {
    json!({
        "name": "ggen-packs",
        "version": env!("CARGO_PKG_VERSION"),
        "description": "ggen packs + marketplace over A2A/MCP: discover, search, inspect, resolve, install, verify, and remove packs with signed provenance.",
        "capabilities": {
            "ggen.packs.capabilities": "Describe available operations and capability surfaces.",
            "ggen.packs.search": "Relevance-rank packs in the local registry by a text query.",
            "ggen.packs.list": "List registry packs, optionally by category.",
            "ggen.packs.show": "Full detail for one pack, including dependencies and validation.",
            "ggen.packs.resolve": "Resolve a capability surface to concrete pack IDs.",
            "ggen.packs.compatibility": "Check whether a set of packs can be composed without conflicts.",
            "ggen.packs.status": "Report installed packs from the project lockfile.",
            "ggen.packs.verify": "Verify a provenance receipt against its signing key.",
            "ggen.packs.install": "Install a pack: write the lockfile and emit a signed receipt.",
            "ggen.packs.remove": "Remove a pack from the project lockfile."
        }
    })
}

fn parse_args<T: for<'de> Deserialize<'de>>(args: &Value) -> Result<T, AgentError> {
    serde_json::from_value(args.clone())
        .map_err(|e| AgentError::InvalidRequest(format!("invalid arguments: {}", e)))
}

/// Dispatch an A2A `{ tool, arguments }` invocation to the matching pure result
/// function. Async because `ggen.packs.install` performs real I/O. This is the
/// single A2A entry point and shares its implementation with the MCP tools.
pub async fn dispatch_pack_tool(tool: &str, args: &Value) -> Result<Value, AgentError> {
    match tool {
        "ggen.packs.capabilities" => capabilities_result(parse_args(args)?),
        "ggen.packs.search" => search_result(parse_args(args)?),
        "ggen.packs.list" => list_result(parse_args(args)?),
        "ggen.packs.show" => show_result(parse_args(args)?),
        "ggen.packs.resolve" => resolve_result(parse_args(args)?),
        "ggen.packs.compatibility" => compatibility_result(parse_args(args)?).await,
        "ggen.packs.status" => status_result(parse_args(args)?),
        "ggen.packs.verify" => verify_result(parse_args(args)?),
        "ggen.packs.install" => install_result(parse_args(args)?).await,
        "ggen.packs.remove" => remove_result(parse_args(args)?),
        other => Err(AgentError::InvalidRequest(format!(
            "unknown tool: {}",
            other
        ))),
    }
}

fn agent_to_adapter_err(e: AgentError) -> AdapterError {
    let details = serde_json::to_value(&e).ok();
    let kind = match e {
        AgentError::InvalidRequest(_)
        | AgentError::PackNotFound(_)
        | AgentError::NotInstalled(_) => AdapterErrorType::ConversionFailed,
        _ => AdapterErrorType::Unknown,
    };
    let mut err = AdapterError::new(e.to_string(), kind);
    if let Some(d) = details {
        err = err.with_details(d);
    }
    err
}

/// A2A adapter exposing the pack tools. `from_a2a` actuates: an A2A task
/// `{ tool, arguments }` is executed against [`dispatch_pack_tool`] and its
/// structured result returned.
#[derive(Debug, Default, Clone)]
pub struct PackToolsAdapter {
    initialized: bool,
}

impl PackToolsAdapter {
    #[must_use]
    pub fn new() -> Self {
        Self { initialized: false }
    }
}

#[async_trait]
impl Adapter for PackToolsAdapter {
    fn name(&self) -> &str {
        "ggen-packs"
    }

    fn version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }

    async fn initialize(&mut self, _config: Value) -> Result<(), AdapterError> {
        self.initialized = true;
        Ok(())
    }

    fn can_handle(&self, format: &str) -> bool {
        PACK_TOOLS.contains(&format)
    }

    async fn to_a2a(&self, message: &Value) -> Result<Value, AdapterError> {
        // A tool result is already A2A-shaped JSON; pass through.
        Ok(message.clone())
    }

    async fn from_a2a(&self, message: &Value) -> Result<Value, AdapterError> {
        let tool = message.get("tool").and_then(Value::as_str).ok_or_else(|| {
            AdapterError::new(
                "missing 'tool'".to_string(),
                AdapterErrorType::ConversionFailed,
            )
        })?;
        let arguments = message
            .get("arguments")
            .cloned()
            .unwrap_or_else(|| json!({}));
        ocel_invoked(tool, subject_of(&arguments));
        dispatch_pack_tool(tool, &arguments)
            .await
            .map_err(agent_to_adapter_err)
    }

    fn capabilities(&self) -> AdapterCapabilities {
        let mut caps = HashMap::new();
        caps.insert("agent_card".to_string(), pack_agent_card());
        AdapterCapabilities {
            supported_formats: PACK_TOOLS.iter().map(|s| (*s).to_string()).collect(),
            max_message_size: 1 << 20,
            supports_encryption: false,
            supports_compression: false,
            capabilities: caps,
        }
    }

    async fn shutdown(&mut self) -> Result<(), AdapterError> {
        self.initialized = false;
        Ok(())
    }
}

/// Best-effort primary subject of a pack tool call for OCEL object identity.
fn subject_of(args: &Value) -> &str {
    args.get("pack_id")
        .or_else(|| args.get("surface"))
        .or_else(|| args.get("query"))
        .or_else(|| args.get("receipt_path"))
        .and_then(Value::as_str)
        .unwrap_or("-")
}
