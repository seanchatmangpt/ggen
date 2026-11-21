//! Shared EA graph helpers for ggen-mcp tools.
use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;
use ggen_abb_sbb::{Authority, Refusal};
use serde::Serialize;
use std::path::{Path, PathBuf};
pub const DEFAULT_GRAPH_REL: &str = "ea.graph.json";
#[derive(Debug, Serialize)]
pub struct EaRefusalBody {
    pub ok: bool,
    pub refusal: String,
    pub detail: String,
}
pub fn refusal_body(refusal: &Refusal) -> EaRefusalBody {
    let value = serde_json::to_value(refusal).unwrap_or_else(
        |e| serde_json::json!({"refusal":"SERIALIZATION_FAILURE","detail":e.to_string()}),
    );
    let tag = value
        .get("refusal")
        .and_then(|v| v.as_str())
        .unwrap_or("UNKNOWN_REFUSAL")
        .to_string();
    EaRefusalBody {
        ok: false,
        refusal: tag,
        detail: refusal.to_string(),
    }
}
pub fn resolve_graph_path(root: &Path, graph_path: Option<&str>) -> Result<PathBuf, McpError> {
    let rel = graph_path.unwrap_or(DEFAULT_GRAPH_REL);
    let abs = if Path::new(rel).is_absolute() {
        PathBuf::from(rel)
    } else {
        root.join(rel)
    };
    if !abs.is_file() {
        return Err(McpError::new(
            ErrorCategory::NotFound,
            format!("EA graph not found at {}", abs.display()),
        ));
    }
    Ok(abs)
}
pub fn load_graph_from_root(
    root: &str, graph_path: Option<&str>,
) -> Result<ggen_abb_sbb::EaGraph, McpError> {
    let root = resolve_root(root)?;
    let path = resolve_graph_path(&root, graph_path)?;
    let raw = std::fs::read_to_string(&path).map_err(|e| {
        McpError::new(
            ErrorCategory::NotFound,
            format!("cannot read EA graph {}: {e}", path.display()),
        )
    })?;
    ggen_abb_sbb::parse_graph(&raw)
        .map_err(|r| McpError::new(ErrorCategory::GraphLoadError, refusal_body(&r).detail))
}
pub fn parse_authority(raw: Option<&str>) -> Result<Authority, McpError> {
    match raw.unwrap_or("CONSTRUCT").to_ascii_uppercase().as_str() {
        "NONE" => Ok(Authority::None),
        "SELECT" => Ok(Authority::Select),
        "CONSTRUCT" => Ok(Authority::Construct),
        "DO" => Ok(Authority::Do),
        other => Err(McpError::new(
            ErrorCategory::Unsupported,
            format!("invalid authority '{other}'"),
        )),
    }
}
pub fn parse_generation(raw: Option<&str>, flag: &str) -> Result<u64, McpError> {
    match raw {
        None => Ok(0),
        Some(s) => s.parse().map_err(|e: std::num::ParseIntError| {
            McpError::new(
                ErrorCategory::Unsupported,
                format!("invalid {flag} '{s}': {e}"),
            )
        }),
    }
}
