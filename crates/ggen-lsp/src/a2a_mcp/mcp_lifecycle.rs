//! MCP/A2A JSON adapters over `lifecycle`.

pub use super::lifecycle::{
    apply, dry_run, receipt_verify, LifecycleApplyParams, LifecycleApplyResult,
    LifecycleDryRunParams, LifecycleDryRunResult, LifecycleError,
};

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

pub const LIFECYCLE_TOOLS: &[&str] = &[
    "ggen.lifecycle.dry_run",
    "ggen.lifecycle.apply",
    "ggen.lifecycle.receipt_verify",
    "ggen.lifecycle.ea_plan",
    "ggen.lifecycle.ea_admit",
];

#[derive(Debug, Clone, Default, Deserialize, schemars::JsonSchema)]
pub struct LifecycleReceiptVerifyParams {
    #[serde(default = "dot")]
    pub root: String,
}

#[derive(Debug, Clone, Default, Deserialize, schemars::JsonSchema)]
pub struct LifecycleEaPlanParams {
    pub graph: Option<String>,
    pub abb: Option<String>,
    pub authority: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize, schemars::JsonSchema)]
pub struct LifecycleEaAdmitParams {
    pub graph: Option<String>,
    pub abb: Option<String>,
    pub sbb: Option<String>,
    pub authority: Option<String>,
    pub expected_graph_digest: Option<String>,
    pub generation: Option<String>,
    pub baseline_generation: Option<String>,
}

fn dot() -> String {
    ".".into()
}

pub fn dry_run_result(p: LifecycleDryRunParams) -> Result<Value, McpLifecycleError> {
    let r = dry_run(&p).map_err(|e| McpLifecycleError(e.0))?;
    Ok(serde_json::to_value(r).unwrap())
}

pub fn apply_result(p: LifecycleApplyParams) -> Result<Value, McpLifecycleError> {
    let r = apply(&p).map_err(|e| McpLifecycleError(e.0))?;
    Ok(serde_json::to_value(r).unwrap())
}

pub fn receipt_verify_result(p: LifecycleReceiptVerifyParams) -> Result<Value, McpLifecycleError> {
    let valid = receipt_verify(&p.root).map_err(|e| McpLifecycleError(e.0))?;
    Ok(json!({ "valid": valid }))
}

pub fn ea_plan_result(p: LifecycleEaPlanParams) -> Result<Value, McpLifecycleError> {
    let path = p.graph.as_deref().unwrap_or("./ea.graph.json");
    let raw = std::fs::read_to_string(path).map_err(|e| McpLifecycleError(e.to_string()))?;
    let g = ggen_abb_sbb::parse_graph(&raw).map_err(|r| McpLifecycleError(r.to_string()))?;
    let authority = parse_authority(p.authority.as_deref())?;
    let abb = p.abb.unwrap_or_else(|| "abb:event-ingest".to_string());
    let decision =
        ggen_abb_sbb::plan(&g, &abb, authority).map_err(|r| McpLifecycleError(r.to_string()))?;
    Ok(json!({
        "ok": true,
        "verb": "plan",
        "result": decision,
    }))
}

pub fn ea_admit_result(p: LifecycleEaAdmitParams) -> Result<Value, McpLifecycleError> {
    let path = p.graph.as_deref().unwrap_or("./ea.graph.json");
    let raw = std::fs::read_to_string(path).map_err(|e| McpLifecycleError(e.to_string()))?;
    let g = ggen_abb_sbb::parse_graph(&raw).map_err(|r| McpLifecycleError(r.to_string()))?;
    let req = ggen_abb_sbb::Request {
        abb: p.abb.unwrap_or_else(|| "abb:event-ingest".to_string()),
        sbb: p.sbb.unwrap_or_else(|| "sbb:ingest-0000".to_string()),
        requested_authority: parse_authority(p.authority.as_deref())?,
        expected_graph_digest: p.expected_graph_digest.clone(),
        receipt_generation: parse_generation(p.generation.as_deref())?,
        artifact_baseline_generation: parse_generation(p.baseline_generation.as_deref())?,
    };
    let ad = ggen_abb_sbb::admit(&g, &req).map_err(|r| McpLifecycleError(r.to_string()))?;
    Ok(json!({
        "ok": true,
        "verb": "admit",
        "result": {
            "graph_digest": ad.graph_digest(),
            "sbb": ad.sbb().id,
            "sbb_digest": ad.sbb_digest(),
        }
    }))
}

fn parse_authority(raw: Option<&str>) -> Result<ggen_abb_sbb::Authority, McpLifecycleError> {
    match raw.unwrap_or("CONSTRUCT").to_ascii_uppercase().as_str() {
        "NONE" => Ok(ggen_abb_sbb::Authority::None),
        "SELECT" => Ok(ggen_abb_sbb::Authority::Select),
        "CONSTRUCT" => Ok(ggen_abb_sbb::Authority::Construct),
        "DO" => Ok(ggen_abb_sbb::Authority::Do),
        _ => Err(McpLifecycleError("invalid authority".into())),
    }
}

fn parse_generation(raw: Option<&str>) -> Result<u64, McpLifecycleError> {
    match raw {
        None => Ok(0),
        Some(s) => s
            .parse()
            .map_err(|e: std::num::ParseIntError| McpLifecycleError(e.to_string())),
    }
}

#[derive(Debug, Clone, thiserror::Error)]
#[error("{0}")]
pub struct McpLifecycleError(pub String);

pub fn ocel_invoked(tool: &str, subject: &str) {
    tracing::info!("OCEL: {}", json!({"event": "a2a.mcp.tool.invoked", "tool": tool, "lifecycle": subject}));
}

pub fn mcp_ok(value: Value) -> rmcp::model::CallToolResult {
    let text = serde_json::to_string_pretty(&value).unwrap_or_else(|_| value.to_string());
    let mut meta = rmcp::model::MetaObject::new();
    meta.0.insert("ggen_result".to_string(), value);
    rmcp::model::CallToolResult::success(vec![rmcp::model::ContentBlock::text(text)]).with_meta(Some(meta))
}

pub fn mcp_err(e: McpLifecycleError) -> rmcp::model::ErrorData {
    rmcp::model::ErrorData::invalid_params(e.to_string(), None)
}

pub fn dispatch_lifecycle_tool(tool: &str, args: &Value) -> Result<Value, McpLifecycleError> {
    match tool {
        "ggen.lifecycle.dry_run" => dry_run_result(serde_json::from_value(args.clone()).map_err(|e| McpLifecycleError(e.to_string()))?),
        "ggen.lifecycle.apply" => apply_result(serde_json::from_value(args.clone()).map_err(|e| McpLifecycleError(e.to_string()))?),
        "ggen.lifecycle.receipt_verify" => receipt_verify_result(serde_json::from_value(args.clone()).map_err(|e| McpLifecycleError(e.to_string()))?),
        "ggen.lifecycle.ea_plan" => ea_plan_result(serde_json::from_value(args.clone()).map_err(|e| McpLifecycleError(e.to_string()))?),
        "ggen.lifecycle.ea_admit" => ea_admit_result(serde_json::from_value(args.clone()).map_err(|e| McpLifecycleError(e.to_string()))?),
        other => Err(McpLifecycleError(format!("unknown tool: {other}"))),
    }
}
