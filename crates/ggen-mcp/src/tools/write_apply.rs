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
    let root = resolve_root(&params.root)?;

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
