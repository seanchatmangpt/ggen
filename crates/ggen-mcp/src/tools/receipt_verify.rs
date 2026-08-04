//! `ggen_receipt_verify` — read `.ggen-v2/receipt.json`, recompute the
//! BLAKE3 chain hash via `praxis-core`, and check the ed25519 signature
//! when present.
//!
//! Wraps `ggen_engine::verbs::handlers::handle_receipt_verify_in`, which is
//! already a pure, read-only, root-parameterized function (its own doc
//! comment confirms it was refactored specifically so a non-cwd-bound
//! caller like this one could call it without mutating process state).
//! This tool adds nothing to that check itself; it only adapts the result
//! to this crate's typed params/result/`McpError` shape, mirroring
//! `sync_dry_run.rs`.
//!
//! ## FM-CHAIN code extraction — an honest, verified scope note
//!
//! `AppError` has no typed FM-code field anywhere (confirmed by reading
//! `ggen-engine/src/error.rs:171-262`): every `fm_chain`/`fm_graph`/...
//! constructor just formats a `[FM-XXX-0NN]` prefix into a plain string.
//! `fm_code` below extracts that prefix from the error's `Display` text via
//! a regex, so a caller gets a typed field when one is present rather than
//! having to string-match `message` itself.
//!
//! **What was actually verified this session, reading
//! `handle_receipt_verify_in` end to end
//! (`ggen-engine/src/verbs/handlers.rs:417-516`):** that function's own
//! error paths (missing/malformed receipt, unsupported schema version,
//! payload-hash mismatch, chain-hash mismatch, malformed/wrong-length/
//! non-verifying signature) are ALL plain `"receipt invalid: ..."` strings
//! with **no** `FM-CHAIN-0NN` prefix. The `FM-CHAIN-*` codes that DO have
//! real call sites (002/004/005/006/007/009/010/011) live in `sync.rs`'s
//! receipt-writing path and in `handle_receipt_history` (handlers.rs
//! 519+) — a sibling function this tool does not call. So for the tamper
//! scenarios this tool can actually observe (a hand-edited
//! `.ggen-v2/receipt.json`), `fm_code` is expected to be `None` today; it
//! is still implemented as a real, generic extraction (not hardcoded to
//! "always None") because a future call site inside the verify path, or a
//! caller-supplied receipt whose message happens to embed a `FM-CHAIN-0NN`
//! substring, must still be picked up without a code change here.

use regex::Regex;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::sync::LazyLock;

use crate::error::McpError;
use crate::project_root::resolve_root;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ReceiptVerifyParams {
    /// Project root directory (must contain `.ggen-v2/receipt.json`).
    pub root: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct ReceiptVerifyResult {
    pub valid: bool,
    /// Present only when `valid` is `false`: the engine's own refusal
    /// message, verbatim.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_message: Option<String>,
    /// A `FM-CHAIN-0NN`-shaped code extracted from `error_message`, when
    /// one is present. See the module doc for why this is commonly `None`
    /// for this specific tool's own error paths today — an honest gap,
    /// not a bug in the extraction.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fm_code: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub chain_hash: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub payload_hash: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub graph_hash: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub outputs: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signed: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature_valid: Option<bool>,
}

/// Matches any `FM-<UPPER-WORD>-0NN` failure-mode code (e.g.
/// `FM-CHAIN-007`), not just `FM-CHAIN-*` — the extraction is generic
/// because `AppError` has no typed code field to key off of; grabbing only
/// the `CHAIN` family would silently drop a real code from another family
/// if one is ever embedded in a receipt-verify message.
static FM_CODE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"FM-[A-Z]+-\d{3}").expect("static FM code regex is valid"));

/// Extract the first `FM-*-0NN` code from an error's `Display` text, if any.
fn extract_fm_code(message: &str) -> Option<String> {
    FM_CODE_RE.find(message).map(|m| m.as_str().to_string())
}

/// Verify the sync receipt at `root/.ggen-v2/receipt.json`.
///
/// Never mutates anything: this is a read + recompute + compare, exactly
/// like the underlying `ggen receipt verify` CLI verb.
///
/// # Errors
/// `ErrorCategory::PathTraversal` for an unresolvable `root`. A failed
/// receipt check is NOT surfaced as an `Err` here — it is reported as
/// `Ok(ReceiptVerifyResult { valid: false, .. })` so a caller can inspect
/// `fm_code`/`error_message` without unwrapping an error path, mirroring
/// how `ggen receipt verify`'s own JSON output is either a success object
/// or (on a real I/O failure to even read the file) a hard error — the
/// distinction preserved here is "the receipt exists and was checked" vs.
/// "the check could not even run".
pub fn receipt_verify(params: &ReceiptVerifyParams) -> Result<ReceiptVerifyResult, McpError> {
    let root = resolve_root(&params.root)?;

    match ggen_engine::verbs::handlers::handle_receipt_verify_in(&root) {
        Ok(value) => Ok(ReceiptVerifyResult {
            valid: value
                .get("valid")
                .and_then(serde_json::Value::as_bool)
                .unwrap_or(false),
            error_message: None,
            fm_code: None,
            chain_hash: value
                .get("chain_hash")
                .and_then(serde_json::Value::as_str)
                .map(str::to_string),
            payload_hash: value
                .get("payload_hash")
                .and_then(serde_json::Value::as_str)
                .map(str::to_string),
            graph_hash: value
                .get("graph_hash")
                .and_then(serde_json::Value::as_str)
                .map(str::to_string),
            outputs: value
                .get("outputs")
                .and_then(serde_json::Value::as_u64)
                .map(|n| n as usize),
            signed: value.get("signed").and_then(serde_json::Value::as_bool),
            signature_valid: value
                .get("signature_valid")
                .and_then(serde_json::Value::as_bool),
        }),
        Err(e) => {
            let message = e.to_string();
            let fm_code = extract_fm_code(&message);
            Ok(ReceiptVerifyResult {
                valid: false,
                error_message: Some(message),
                fm_code,
                chain_hash: None,
                payload_hash: None,
                graph_hash: None,
                outputs: None,
                signed: None,
                signature_valid: None,
            })
        }
    }
}
