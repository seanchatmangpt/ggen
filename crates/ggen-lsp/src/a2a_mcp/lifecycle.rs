//! Core sync lifecycle (engine-backed); MCP layer in `mcp_lifecycle`.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions, RECEIPT_REL_PATH};
use serde::{Deserialize, Serialize};

const MAX_PATH_BYTES: usize = 4096;

#[derive(Debug, thiserror::Error)]
pub enum LifecycleError {
    #[error("{0}")]
    Refused(String),
}

#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct LifecycleDryRunParams {
    #[serde(default = "default_dot")]
    pub root: String,
}

fn default_dot() -> String {
    ".".to_string()
}

#[derive(Debug, Clone, Serialize)]
pub struct LifecycleDryRunResult {
    pub graph_hash: String,
    pub decisions: BTreeMap<String, String>,
    pub write_count: usize,
    pub skip_count: usize,
}

#[derive(Debug, Clone, Deserialize, schemars::JsonSchema)]
pub struct LifecycleApplyParams {
    #[serde(default = "default_dot")]
    pub root: String,
    pub confirm: bool,
    pub expected_graph_hash: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct LifecycleApplyResult {
    pub graph_hash: String,
    pub write_count: usize,
    pub skip_count: usize,
    pub receipt_path: String,
}

fn resolve_root(root: &str) -> Result<PathBuf, LifecycleError> {
    if root.len() > MAX_PATH_BYTES {
        return Err(LifecycleError::Refused("root too long".into()));
    }
    Path::new(root)
        .canonicalize()
        .map_err(|e| LifecycleError::Refused(format!("root: {e}")))
}

pub fn dry_run(p: &LifecycleDryRunParams) -> Result<LifecycleDryRunResult, LifecycleError> {
    let root = resolve_root(&p.root)?;
    let report = sync(
        &root,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .map_err(|e| LifecycleError::Refused(e.to_string()))?;
    let write_count = report
        .decisions
        .values()
        .filter(|d| !d.starts_with("skipped"))
        .count();
    let skip_count = report.decisions.len() - write_count;
    Ok(LifecycleDryRunResult {
        graph_hash: report.graph_hash_hex,
        decisions: report.decisions,
        write_count,
        skip_count,
    })
}

pub fn apply(p: &LifecycleApplyParams) -> Result<LifecycleApplyResult, LifecycleError> {
    if !p.confirm {
        return Err(LifecycleError::Refused(
            "ggen.lifecycle.apply requires confirm:true".into(),
        ));
    }
    if p.expected_graph_hash.trim().is_empty() {
        return Err(LifecycleError::Refused(
            "missing expected_graph_hash".into(),
        ));
    }
    let root = resolve_root(&p.root)?;
    let pre = sync(
        &root,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .map_err(|e| LifecycleError::Refused(e.to_string()))?;
    if pre.graph_hash_hex != p.expected_graph_hash {
        return Err(LifecycleError::Refused(
            "expected_graph_hash mismatch".into(),
        ));
    }
    let report = sync(
        &root,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .map_err(|e| LifecycleError::Refused(e.to_string()))?;
    Ok(LifecycleApplyResult {
        graph_hash: report.graph_hash_hex,
        write_count: report.written.len(),
        skip_count: report.skipped.len(),
        receipt_path: RECEIPT_REL_PATH.to_string(),
    })
}

pub fn corroborate_graph_hash(
    base_path: &Path, expected_graph_hash: &str, confirm: bool,
) -> Result<(), String> {
    if !confirm {
        return Err(
            "REFUSED:MISSING_CONFIRM — construct/apply requires confirm: true.".to_string(),
        );
    }
    if expected_graph_hash.trim().is_empty() {
        return Err(
            "REFUSED:MISSING_GRAPH_HASH — expected_graph_hash from a prior dry_run is required."
                .to_string(),
        );
    }
    let pre = sync(
        base_path,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .map_err(|e| format!("REFUSED:PREFLIGHT_FAILED — {e}"))?;
    if pre.graph_hash_hex != expected_graph_hash {
        return Err(format!(
            "REFUSED:GRAPH_HASH_MISMATCH — expected {expected_graph_hash}, current {}.",
            pre.graph_hash_hex
        ));
    }
    Ok(())
}

pub fn receipt_verify(root: &str) -> Result<bool, LifecycleError> {
    let root = resolve_root(root)?;
    match ggen_engine::verbs::handlers::handle_receipt_verify_in(&root) {
        Ok(v) => Ok(v.get("valid").and_then(|x| x.as_bool()).unwrap_or(false)),
        Err(_) => Ok(false),
    }
}
