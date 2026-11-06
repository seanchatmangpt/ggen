//! `ggen ea plan` — SELECT existing SBB || MANUFACTURE missing realization.
//!
//! Usage example:
//! `ggen ea plan --graph ./ea.graph.json --abb abb:event-ingest --authority SELECT --json`

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::Value;

use super::{load_graph, ok_json, parse_authority, refuse_and_exit, usage_error_and_exit};

/// Domain helper for the `plan` verb (kept out of the `#[verb]` fn to satisfy
/// the Poka-Yoke complexity ceiling on CLI-layer functions).
///
/// # Errors
///
/// Exits 1 with typed refusal JSON on stderr for kernel refusals; exits 2 for
/// usage/input errors.
pub(crate) fn run(
    graph: Option<String>, abb: Option<String>, authority: Option<String>,
) -> Result<Value> {
    let g = load_graph(graph.as_deref().unwrap_or(super::DEFAULT_GRAPH_PATH));
    let authority = parse_authority(authority.as_deref());
    let abb = abb.unwrap_or_else(|| "abb:event-ingest".to_string());
    match ggen_abb_sbb::plan(&g, &abb, authority) {
        Ok(decision) => {
            let v = serde_json::to_value(&decision)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
            ok_json("plan", v)
        }
        Err(refusal) => refuse_and_exit(&refusal),
    }
}

/// Run kernel planning over the EA graph and print the Decision as JSON.
///
/// # Errors
///
/// Exit 1 with typed refusal JSON on stderr for kernel refusals; exit 2 for
/// usage/input errors (unreadable graph, malformed JSON, bad authority value).
#[verb("plan", "ea")]
pub fn plan(
    graph: Option<String>, json: bool, abb: Option<String>, authority: Option<String>,
) -> Result<Value> {
    let _ = json;
    run(graph, abb, authority)
}
