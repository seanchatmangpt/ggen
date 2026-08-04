//! `ggen_sync_dry_run` — run the pipeline in dry-run mode and report what
//! WOULD be written, with **typed** skip reasons.
//!
//! Closes a verified friction point: `sync --dry-run` records a reason for
//! every skip, but only as a free-text string in a `decisions` map, so
//! "skipped because the `when:` ASK guard was false" and "skipped because
//! the SELECT returned zero rows" are indistinguishable without string
//! matching -- and the second of those is exactly the failure class this
//! whole crate exists to make loud.
//!
//! This tool classifies the engine's own reason strings into a closed enum
//! ONCE, here, so callers never string-match. Anything it cannot classify
//! is reported as `other` WITH the raw reason preserved -- never silently
//! bucketed into a wrong category.

use ggen_engine::sync::{sync, SyncOptions};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;
use crate::tools::skip_classify::classify;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct SyncDryRunParams {
    /// Project root directory.
    pub root: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct PlannedWrite {
    pub path: String,
    /// The engine's own decision string for this output ("written",
    /// "injected", "planned: write (dry-run)", ...), carried verbatim.
    pub decision: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct PlannedSkip {
    pub path: String,
    /// Typed reason -- `when_false`, `zero_rows`, `unchanged`,
    /// `exists_no_overwrite`, `skip_empty`, or `other`. The first two are
    /// the ones today's output cannot distinguish.
    pub reason: String,
    /// The engine's raw reason string, always preserved so a caller can
    /// see what was classified (and catch a misclassification).
    pub raw_reason: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct SyncDryRunResult {
    pub ok: bool,
    pub would_write: Vec<PlannedWrite>,
    pub would_skip: Vec<PlannedSkip>,
    pub write_count: usize,
    pub skip_count: usize,
    /// BLAKE3 of the post-Enrich canonical graph state -- proves which
    /// graph this plan was computed against.
    pub graph_hash: String,
}

/// Execute a dry-run sync against `root`.
///
/// Writes nothing: `SyncOptions::dry_run` is forced `true` here and is not
/// caller-controllable, so this tool cannot mutate a project regardless of
/// input. Applying a plan is a separate, explicitly destructive tool.
///
/// # Errors
/// `ErrorCategory::PathTraversal` for an unresolvable `root`;
/// `ErrorCategory::GraphLoadError` for any pipeline refusal (the engine's
/// own typed message is carried through verbatim).
pub fn sync_dry_run(params: &SyncDryRunParams) -> Result<SyncDryRunResult, McpError> {
    let root = resolve_root(&params.root)?;
    let opts = SyncOptions {
        dry_run: true,
        ..Default::default()
    };
    let report = sync(&root, opts)
        .map_err(|e| McpError::new(ErrorCategory::GraphLoadError, e.to_string()))?;

    let mut would_write = Vec::new();
    let mut would_skip = Vec::new();
    for (path, decision) in &report.decisions {
        if decision.starts_with("skipped") {
            would_skip.push(PlannedSkip {
                path: path.clone(),
                reason: classify(decision).to_string(),
                raw_reason: decision.clone(),
            });
        } else {
            would_write.push(PlannedWrite {
                path: path.clone(),
                decision: decision.clone(),
            });
        }
    }

    Ok(SyncDryRunResult {
        ok: true,
        write_count: would_write.len(),
        skip_count: would_skip.len(),
        would_write,
        would_skip,
        graph_hash: report.graph_hash_hex,
    })
}
