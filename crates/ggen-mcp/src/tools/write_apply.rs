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

/// Gall CP38: an answer to "was this specific caller authorized to trigger
/// this write," a real, separate question from CP17's hash-corroboration
/// gate (which only ever answers "is this write correct/fresh"). Threaded
/// straight into the resulting receipt's `origin` field (CP37).
///
/// This enum alone provides no enforcement -- any code in this crate could
/// write `CallerOrigin::UnattendedDispatch` directly if nothing else
/// restricted it. The real enforcement point is `WriteApplyParams`'s
/// private `caller_origin` field plus the narrow, visibility-restricted
/// constructors below.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum CallerOrigin {
    /// Every real MCP JSON-RPC tool call. This is also `WriteApplyParams`'s
    /// `Deserialize` default (see `caller_origin`'s `#[serde(skip, ...)]`
    /// below) -- an external caller's JSON payload cannot claim any other
    /// origin even if it tries, because this field is never read from JSON
    /// at all.
    ExternalMcp,
    /// Set only by `crate::tools::unattended_dispatch`'s own
    /// `try_unattended_apply` -- compiler-enforced: `WriteApplyParams::
    /// for_unattended_dispatch` is `pub(in crate::tools::unattended_dispatch)`,
    /// so no other module in this crate can call it, and therefore no other
    /// code can ever produce this variant.
    UnattendedDispatch,
    /// Set only by `crate::selfplay::board`'s in-process test-harness call
    /// sites, via `WriteApplyParams::for_self_play_harness` -- states that
    /// file's own identity honestly instead of looking like every other
    /// anonymous in-process caller.
    SelfPlayHarness,
}

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
    /// CP38: never populated from an external caller's JSON (`#[serde(skip)]`
    /// -- the field simply isn't read from the wire at all, regardless of
    /// what an external caller's payload contains), always
    /// `CallerOrigin::ExternalMcp` for a real MCP tool call. Private: only
    /// this module's own narrow constructors (or `Default`, for real MCP
    /// deserialization) can set it, so a caller cannot self-report a
    /// stronger authorization than it actually has.
    #[serde(skip, default = "default_caller_origin")]
    caller_origin: CallerOrigin,
}

fn default_caller_origin() -> CallerOrigin {
    CallerOrigin::ExternalMcp
}

impl WriteApplyParams {
    /// Construct params exactly as a real MCP JSON-RPC call would produce
    /// via `Deserialize` -- `caller_origin` is always `ExternalMcp`. `pub`
    /// (not `pub(crate)`) so integration tests under `tests/`, which
    /// compile as a separate crate linking against this one, can exercise
    /// the same ordinary path a real external caller takes.
    pub fn new(root: String, confirm: bool, expected_graph_hash: String) -> Self {
        Self {
            root,
            confirm,
            expected_graph_hash,
            caller_origin: CallerOrigin::ExternalMcp,
        }
    }

    /// Intended for `crate::tools::unattended_dispatch` alone -- `pub(crate)`,
    /// not compiler-restricted to that one module. An earlier version of
    /// this used a private-field capability token
    /// (`unattended_dispatch::DispatchAuthority`) to make the restriction
    /// compiler-enforced; that was deliberately dropped (Gall R1) in favor
    /// of this ordinary `pub(crate)` trust boundary -- any code in
    /// `ggen-mcp` COULD call this, but only `unattended_dispatch.rs` does,
    /// checkable by grep/code review like any other `pub(crate)` fn in this
    /// codebase. Do not widen this to `pub`: an external caller must never
    /// be able to self-report the `UnattendedDispatch` origin.
    pub(crate) fn for_unattended_dispatch(root: String, expected_graph_hash: String) -> Self {
        Self {
            root,
            confirm: true,
            expected_graph_hash,
            caller_origin: CallerOrigin::UnattendedDispatch,
        }
    }

    /// Used by `crate::selfplay::board`'s in-process test-harness call
    /// sites to declare their own identity honestly instead of constructing
    /// a params value indistinguishable from every other in-process caller.
    pub(crate) fn for_self_play_harness(
        root: String, confirm: bool, expected_graph_hash: String,
    ) -> Self {
        Self {
            root,
            confirm,
            expected_graph_hash,
            caller_origin: CallerOrigin::SelfPlayHarness,
        }
    }
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

    // CP37/38: thread this call's authorized origin onto the resulting
    // receipt. `ExternalMcp`/`SelfPlayHarness` leave the receipt's `origin`
    // field `None` (today's exact behavior) -- only `UnattendedDispatch`
    // gets a real, distinguishable tag.
    let receipt_origin = match params.caller_origin {
        CallerOrigin::UnattendedDispatch => Some("unattended-dispatch"),
        CallerOrigin::ExternalMcp | CallerOrigin::SelfPlayHarness => None,
    };
    let opts = SyncOptions {
        dry_run: false,
        receipt_origin,
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
