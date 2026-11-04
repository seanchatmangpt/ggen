//! `ggen ea admit` — run the kernel admission gate over an EA graph.
//!
//! Usage example:
//! `ggen ea admit --graph ./ea.graph.json --abb abb:event-ingest --sbb sbb:ingest-0000 --authority CONSTRUCT --json`

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::Value;

use super::{
    load_graph, ok_json, parse_authority, parse_generation, refuse_and_exit, usage_error_and_exit,
};

/// Flag bundle for the admission verbs (`admit`/`manufacture`), built from the
/// shared WO-02 request flags.
pub(crate) struct AdmitFlags {
    pub graph: Option<String>,
    pub abb: Option<String>,
    pub sbb: Option<String>,
    pub authority: Option<String>,
    pub expected_graph_digest: Option<String>,
    pub generation: Option<String>,
    pub baseline_generation: Option<String>,
}

/// Domain helper shared by `admit` and `manufacture`: build the kernel
/// Request from flags, run the admission gate, return the sealed value.
///
/// # Errors
///
/// Exits 1 with typed refusal JSON on stderr for kernel refusals; exits 2 for
/// usage/input errors.
pub(crate) fn admit_core(flags: AdmitFlags) -> Result<ggen_abb_sbb::Admitted> {
    let g = load_graph(flags.graph.as_deref().unwrap_or(super::DEFAULT_GRAPH_PATH));
    let authority = parse_authority(flags.authority.as_deref());
    let generation = parse_generation(flags.generation.as_deref(), "--generation");
    let baseline = parse_generation(flags.baseline_generation.as_deref(), "--baseline-generation");
    let abb = flags.abb.unwrap_or_else(|| "abb:event-ingest".to_string());
    let sbb = flags.sbb.unwrap_or_else(|| "sbb:ingest-0000".to_string());
    let req = ggen_abb_sbb::Request {
        abb,
        sbb,
        requested_authority: authority,
        expected_graph_digest: flags.expected_graph_digest.clone(),
        receipt_generation: generation,
        artifact_baseline_generation: baseline,
    };
    match ggen_abb_sbb::admit(&g, &req) {
        Ok(ad) => Ok(ad),
        Err(refusal) => refuse_and_exit(&refusal),
    }
}

/// Shape the sealed admission into JSON.
pub(crate) fn admitted_json(ad: &ggen_abb_sbb::Admitted) -> Value {
    serde_json::json!({
        "graph_digest": ad.graph_digest(),
        "strategy": ad.strategy(),
        "capability": ad.capability(),
        "abb": ad.abb().id,
        "contract": ad.contract().id,
        "contract_digest": ad.contract_digest(),
        "sbb": ad.sbb().id,
        "sbb_digest": ad.sbb_digest(),
        "qualification": ad.qualification(),
        "generation": ad.generation(),
    })
}

/// Thin verb wrapper: parse flags, admit, print.
///
/// # Errors
///
/// Exit 1 with typed refusal JSON on stderr for kernel refusals; exit 2 for
/// usage/input errors.
#[verb("admit", "ea")]
pub fn admit(
    graph: Option<String>,
    json: bool,
    abb: Option<String>,
    sbb: Option<String>,
    authority: Option<String>,
    expected_graph_digest: Option<String>,
    generation: Option<String>,
    baseline_generation: Option<String>,
) -> Result<Value> {
    let _ = json;
    let ad = admit_core(AdmitFlags {
        graph,
        abb,
        sbb,
        authority,
        expected_graph_digest,
        generation,
        baseline_generation,
    })?;
    ok_json("admit", admitted_json(&ad))
}
