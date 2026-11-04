//! `ggen ea manufacture` — execute an admitted plan against the graph.
//!
//! Usage example:
//! `ggen ea manufacture --graph ./ea.graph.json --abb abb:event-ingest --sbb sbb:ingest-0000 --receipt-out ./receipt.json --json`

use std::io::Write;

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::{json, Value};

use super::{
    load_graph, ok_json, parse_authority, parse_generation, refuse_and_exit, usage_error_and_exit,
    DEFAULT_GENERATOR_ID, DEFAULT_GENERATOR_VERSION,
};

/// Domain helper for the `manufacture` verb.
///
/// # Errors
///
/// Exits 1 with typed refusal JSON on stderr for kernel refusals; exits 2 for
/// usage/input errors.
pub(crate) fn run(
    graph: Option<String>,
    abb: Option<String>,
    sbb: Option<String>,
    authority: Option<String>,
    expected_graph_digest: Option<String>,
    generation: Option<String>,
    baseline_generation: Option<String>,
    generator_id: Option<String>,
    generator_version: Option<String>,
    receipt_out: Option<String>,
) -> Result<Value> {
    let g = load_graph(graph.as_deref().unwrap_or(super::DEFAULT_GRAPH_PATH));
    let authority = parse_authority(authority.as_deref());
    let generation = parse_generation(generation.as_deref(), "--generation");
    let baseline = parse_generation(baseline_generation.as_deref(), "--baseline-generation");
    let gen = ggen_abb_sbb::Generator {
        id: generator_id.unwrap_or_else(|| DEFAULT_GENERATOR_ID.to_string()),
        version: generator_version.unwrap_or_else(|| DEFAULT_GENERATOR_VERSION.to_string()),
    };
    let req = ggen_abb_sbb::Request {
        abb: abb.unwrap_or_else(|| "abb:event-ingest".to_string()),
        sbb: sbb.unwrap_or_else(|| "sbb:ingest-0000".to_string()),
        requested_authority: authority,
        expected_graph_digest,
        receipt_generation: generation,
        artifact_baseline_generation: baseline,
    };
    let ad = match ggen_abb_sbb::admit(&g, &req) {
        Ok(ad) => ad,
        Err(refusal) => refuse_and_exit(&refusal),
    };
    let m = match ggen_abb_sbb::manufacture(&ad, &gen) {
        Ok(m) => m,
        Err(refusal) => refuse_and_exit(&refusal),
    };
    let receipt_path = receipt_out.unwrap_or_else(|| super::DEFAULT_RECEIPT_PATH.to_string());
    let receipt_json = match serde_json::to_string_pretty(&m.receipt) {
        Ok(s) => s,
        Err(e) => {
            return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                "cannot serialize receipt: {e}"
            )))
        }
    };
    // Trailing newline keeps the receipt file a clean single JSON document.
    if let Err(e) = std::fs::write(&receipt_path, format!("{receipt_json}\n")) {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "cannot write receipt to {receipt_path}: {e}"
        )));
    }
    let payload = json!({
        "receipt_path": receipt_path,
        "receipt_digest": m.receipt.receipt_digest,
        "artifacts": m.artifacts.iter().map(|a| json!({
            "path": a.path,
            "bytes": a.bytes.len(),
            "digest": a.digest,
        })).collect::<Vec<_>>(),
    });
    ok_json("manufacture", payload)
}

/// Thin verb wrapper: admit, manufacture, write receipt, print artifact digests.
///
/// # Errors
///
/// Exit 1 with typed refusal JSON on stderr for kernel refusals; exit 2 for
/// usage/input errors.
#[verb("manufacture", "ea")]
pub fn manufacture(
    graph: Option<String>,
    json: bool,
    abb: Option<String>,
    sbb: Option<String>,
    authority: Option<String>,
    expected_graph_digest: Option<String>,
    generation: Option<String>,
    baseline_generation: Option<String>,
    generator_id: Option<String>,
    generator_version: Option<String>,
    receipt_out: Option<String>,
) -> Result<Value> {
    let _ = json;
    run(
        graph,
        abb,
        sbb,
        authority,
        expected_graph_digest,
        generation,
        baseline_generation,
        generator_id,
        generator_version,
        receipt_out,
    )
}
