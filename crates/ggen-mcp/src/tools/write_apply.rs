//! `ggen_write_apply` — the one destructive tool: actually run the sync
//! pipeline and write its outputs.
//!
//! Split from `ggen_sync_dry_run` as a SEPARATE tool rather than a mode
//! flag on a shared one, per the MCP tool-design rule that read and write
//! must never be the same tool distinguished by a boolean. This tool
//! declares `destructiveHint: true`; the dry-run tool declares
//! `readOnlyHint: true`. A client can therefore gate on the annotation
//! alone, without inspecting arguments.
//!
//! Requires an explicit `confirm: true`. That is deliberately redundant
//! with the tool annotation -- an agent that reaches this tool by accident
//! (wrong tool name, hallucinated arguments) fails closed on the missing
//! confirmation rather than mutating a project.

use ggen_engine::sync::{sync, SyncOptions};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct WriteApplyParams {
    /// Project root directory.
    pub root: String,
    /// Must be literally `true`. Any other value refuses without writing.
    pub confirm: bool,
    /// The `graph_hash` field from a real, prior `ggen_sync_dry_run` call
    /// against this same `root`. Gall checkpoint CP17: `confirm: true` alone
    /// was a caller-supplied boolean with zero independent corroboration (a
    /// 2026-08-04 safety audit found an in-process JSON-RPC bypass already
    /// constructing `{confirm: true}` directly). This field forces a real
    /// link to an actual prior review: `write_apply` independently
    /// recomputes the CURRENT graph hash via its own dry-run pass before
    /// writing anything, and refuses if it does not match what the caller
    /// claims to have reviewed -- catching both a fabricated hash and a
    /// stale one (the project changed between dry-run and apply).
    pub expected_graph_hash: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct WrittenFile {
    pub path: String,
    /// BLAKE3 of the bytes now on disk, read back AFTER the write -- so the
    /// caller has evidence of what actually landed, not merely a claim that
    /// a write was attempted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blake3: Option<String>,
    /// Present when the file could not be read back for hashing. The write
    /// itself still succeeded (the engine reported it); this records that
    /// verification was unavailable rather than silently omitting evidence.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_error: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct WriteApplyResult {
    pub ok: bool,
    pub written: Vec<WrittenFile>,
    pub skipped: Vec<SkippedFile>,
    pub write_count: usize,
    pub skip_count: usize,
    /// BLAKE3 of the post-Enrich canonical graph state.
    pub graph_hash: String,
    /// Where the signed sync receipt for this run was written, relative to
    /// the project root. A real sync always produces one; its absence would
    /// mean the run did not complete normally.
    pub receipt_path: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct SkippedFile {
    pub path: String,
    pub reason: String,
}

/// Run a real (non-dry-run) sync.
///
/// # Errors
/// `ErrorCategory::Unsupported` when `confirm` is not `true` -- refused
/// before any pipeline work; `ErrorCategory::PathTraversal` for an
/// unresolvable `root`; `ErrorCategory::GraphLoadError` carrying the
/// engine's typed refusal for any pipeline failure.
pub fn write_apply(params: &WriteApplyParams) -> Result<WriteApplyResult, McpError> {
    if !params.confirm {
        return Err(McpError::new(
            ErrorCategory::Unsupported,
            "ggen_write_apply writes files and requires `confirm: true`. Run \
             ggen_sync_dry_run first to see what would be written.",
        ));
    }
    if params.expected_graph_hash.trim().is_empty() {
        return Err(McpError::new(
            ErrorCategory::Unsupported,
            "ggen_write_apply requires `expected_graph_hash`, the `graph_hash` \
             field from a real prior ggen_sync_dry_run call against this root. \
             Run ggen_sync_dry_run first and pass its graph_hash back here.",
        ));
    }
    let root = resolve_root(&params.root)?;

    // CP17: independently recompute the CURRENT graph hash via a real dry-run
    // pass before writing anything -- proves the caller's claimed review was
    // against the same graph state, not fabricated or stale.
    let preflight = sync(
        &root,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .map_err(|e| McpError::new(ErrorCategory::GraphLoadError, e.to_string()))?;
    if preflight.graph_hash_hex != params.expected_graph_hash {
        return Err(McpError::new(
            ErrorCategory::Unsupported,
            format!(
                "expected_graph_hash does not match the project's current graph \
                 state (expected {}, current {}). Either this hash was not from \
                 a real ggen_sync_dry_run call against this root, or the \
                 project changed since that dry-run ran. Re-run \
                 ggen_sync_dry_run and pass its fresh graph_hash.",
                params.expected_graph_hash, preflight.graph_hash_hex
            ),
        ));
    }

    let opts = SyncOptions {
        dry_run: false,
        ..Default::default()
    };
    let report = sync(&root, opts)
        .map_err(|e| McpError::new(ErrorCategory::GraphLoadError, e.to_string()))?;

    let written: Vec<WrittenFile> = report
        .written
        .iter()
        .map(|rel| {
            let abs = root.join(rel);
            match std::fs::read(&abs) {
                Ok(bytes) => WrittenFile {
                    path: rel.display().to_string(),
                    blake3: Some(blake3::hash(&bytes).to_hex().to_string()),
                    verification_error: None,
                },
                Err(e) => WrittenFile {
                    path: rel.display().to_string(),
                    blake3: None,
                    verification_error: Some(e.to_string()),
                },
            }
        })
        .collect();

    let skipped: Vec<SkippedFile> = report
        .skipped
        .iter()
        .map(|(path, reason)| SkippedFile {
            path: path.display().to_string(),
            reason: reason.clone(),
        })
        .collect();

    Ok(WriteApplyResult {
        ok: true,
        write_count: written.len(),
        skip_count: skipped.len(),
        written,
        skipped,
        graph_hash: report.graph_hash_hex,
        receipt_path: ggen_engine::sync::RECEIPT_REL_PATH.to_string(),
    })
}
