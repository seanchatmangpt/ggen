//! `ggen ea replay` — replay a prior manufacture receipt and compare digests.
//!
//! Usage example:
//! `ggen ea replay --graph ./ea.graph.json --receipt ./receipt.json --json`

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::Value;

use super::{
    load_graph, ok_json, refuse_and_exit, usage_error_and_exit, DEFAULT_GENERATOR_ID,
    DEFAULT_GENERATOR_VERSION, DEFAULT_RECEIPT_PATH,
};

/// Domain helper for the `replay` verb: verify + re-admit + re-manufacture the
/// receipt at `--receipt` against the graph at `--graph`.
///
/// # Errors
///
/// Exit 1 with typed refusal JSON on stderr (REPLAY_MISMATCH, RECEIPT_TAMPERED,
/// kernel admission refusals); exit 2 for unreadable/malformed inputs.
pub(crate) fn run(
    graph: Option<String>,
    receipt: Option<String>,
    generator_id: Option<String>,
    generator_version: Option<String>,
) -> Result<Value> {
    let g = load_graph(graph.as_deref().unwrap_or(super::DEFAULT_GRAPH_PATH));
    let receipt_path = receipt.unwrap_or_else(|| DEFAULT_RECEIPT_PATH.to_string());
    let raw = match std::fs::read_to_string(&receipt_path) {
        Ok(r) => r,
        Err(e) => usage_error_and_exit(format!("cannot read receipt file {receipt_path}: {e}")),
    };
    let r: ggen_abb_sbb::ManufactureReceipt =
        match serde_json::from_str(&raw) {
            Ok(r) => r,
            Err(e) => usage_error_and_exit(format!(
                "malformed receipt JSON in {receipt_path}: {e}"
            )),
        };
    let gen = ggen_abb_sbb::Generator {
        id: generator_id.unwrap_or_else(|| DEFAULT_GENERATOR_ID.to_string()),
        version: generator_version.unwrap_or_else(|| DEFAULT_GENERATOR_VERSION.to_string()),
    };
    match ggen_abb_sbb::replay(&r, &g, &gen) {
        Ok(m) => {
            let payload = serde_json::json!({
                "receipt_digest": m.receipt.receipt_digest,
                "graph_digest": m.receipt.graph_digest,
                "sbb": m.receipt.sbb,
                "artifacts": m.artifacts.len(),
            });
            ok_json("replay", payload)
        }
        Err(refusal) => refuse_and_exit(&refusal),
    }
}

/// Thin verb wrapper: verify + replay the receipt, print digests.
///
/// # Errors
///
/// Exit 1 with typed refusal JSON on stderr for kernel refusals; exit 2 for
/// unreadable/malformed inputs.
#[verb("replay", "ea")]
pub fn replay(
    graph: Option<String>,
    json: bool,
    receipt: Option<String>,
    generator_id: Option<String>,
    generator_version: Option<String>,
) -> Result<Value> {
    let _ = json;
    run(graph, receipt, generator_id, generator_version)
}
