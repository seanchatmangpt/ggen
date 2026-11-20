//! `ggen ea verify-receipt` — validate a receipt file's digest/identity fields.
//!
//! Usage example:
//! `ggen ea verify-receipt --receipt ./receipt.json --json`

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::Value;

use super::{ok_json, refuse_and_exit, usage_error_and_exit, DEFAULT_RECEIPT_PATH};

/// Domain helper for the `verify-receipt` verb: self-digest + identity check
/// without re-manufacturing.
///
/// # Errors
///
/// Exit 1 with typed refusal JSON on stderr (RECEIPT_TAMPERED, REPLAY_MISMATCH,
/// AUTHORITY_EXCEEDED); exit 2 for unreadable/malformed receipt files.
pub(crate) fn run(receipt: Option<String>) -> Result<Value> {
    let receipt_path = receipt.unwrap_or_else(|| DEFAULT_RECEIPT_PATH.to_string());
    let raw = match std::fs::read_to_string(&receipt_path) {
        Ok(r) => r,
        Err(e) => usage_error_and_exit(format!("cannot read receipt file {receipt_path}: {e}")),
    };
    let r: ggen_abb_sbb::ManufactureReceipt = match serde_json::from_str(&raw) {
        Ok(r) => r,
        Err(e) => usage_error_and_exit(format!("malformed receipt JSON in {receipt_path}: {e}")),
    };
    match ggen_abb_sbb::verify_receipt(&r) {
        Ok(()) => {
            let payload = serde_json::json!({
                "receipt_digest": r.receipt_digest,
                "graph_digest": r.graph_digest,
                "abb": r.abb,
                "sbb": r.sbb,
                "generation": r.generation,
            });
            ok_json("verify-receipt", payload)
        }
        Err(refusal) => refuse_and_exit(&refusal),
    }
}

/// Thin verb wrapper: verify a receipt file's digest/identity fields.
///
/// # Errors
///
/// Exit 1 with typed refusal JSON on stderr for kernel refusals; exit 2 for
/// unreadable/malformed receipt files.
#[verb("verify-receipt", "ea")]
pub fn verify_receipt(json: bool, receipt: Option<String>) -> Result<Value> {
    let _ = json;
    run(receipt)
}
