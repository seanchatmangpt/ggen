//! `ggen_check_project` — run ggen-lsp's cross-surface diagnostic pass over
//! a whole project.
//!
//! Closes a verified gap: the 13 `GGEN-*` / `E00xx` codes (unbound template
//! variable, output-path escape, competing authority, `SELECT *` blindspot,
//! ...) are implemented and tested in `ggen-lsp`, but were reachable from NO
//! MCP client -- ggen-lsp's own MCP tool calls the single-file
//! `build_analyzer`, never `check_files_in_root`, so every cross-surface
//! code was invisible to agents.
//!
//! This is a thin pass-through to `ggen_lsp::check_files_in_root`, which is
//! already `Serialize`. It computes nothing itself.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::{resolve_relative, resolve_root};

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CheckProjectParams {
    /// Project root directory.
    pub root: String,
    /// Optional explicit file list (relative to `root`). When omitted,
    /// every law surface under `root` is discovered and checked.
    #[serde(default)]
    pub paths: Option<Vec<String>>,
    /// Include repair routes for each diagnostic.
    #[serde(default)]
    pub with_routes: bool,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct CheckProjectResult {
    pub ok: bool,
    pub error_count: usize,
    pub warning_count: usize,
    /// `true` when at least one ERROR-severity diagnostic fired -- the
    /// refusal signal a gate would act on.
    pub has_errors: bool,
    /// Number of law-surface files actually checked.
    pub files_checked: usize,
    /// The full `CheckReport`, serialized as-is. Diagnostic positions are
    /// anchored at line 0 for most codes -- a known ggen-lsp MVP
    /// limitation, reported as-is rather than papered over here.
    pub report: serde_json::Value,
}

/// Run the cross-surface check.
///
/// # Errors
/// `ErrorCategory::PathTraversal` for an unresolvable `root` or a `paths`
/// entry that escapes it; `ErrorCategory::Internal` if the report cannot be
/// serialized.
pub fn check_project(params: &CheckProjectParams) -> Result<CheckProjectResult, McpError> {
    let root = resolve_root(&params.root)?;

    let paths = match &params.paths {
        Some(list) => {
            let mut resolved = Vec::with_capacity(list.len());
            for rel in list {
                resolved.push(resolve_relative(&root, rel)?);
            }
            resolved
        }
        None => ggen_lsp::discover_law_surfaces(&root),
    };

    let report = ggen_lsp::check_files_in_root(&root, &paths, params.with_routes);
    let value = serde_json::to_value(&report).map_err(|e| {
        McpError::new(
            ErrorCategory::Internal,
            format!("report serialization failed: {e}"),
        )
    })?;

    Ok(CheckProjectResult {
        ok: true,
        error_count: report.error_count,
        warning_count: report.warning_count,
        has_errors: report.has_errors(),
        files_checked: report.files.len(),
        report: value,
    })
}
