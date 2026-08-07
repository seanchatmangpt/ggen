//! Real conformance check of GymAct's declared model against a real run --
//! van der Aalst items 2-5 ("what would he do next" after the
//! gymact-bridge-pack's static, SHACL-validated capability catalog: build a
//! real OCEL log, discover the actual DFG, conformance-check it against the
//! declared model, look at variants).
//!
//! **This is a standalone conformance CONSUMER of `wasm4pm-compat`, not part
//! of ggen's generation pipeline.** Per the process-intelligence boundary
//! (`.claude/rules/architecture.md`: "ggen EMITS process evidence. ggen does
//! NOT analyse it."), this test lives in ggen's test tree only because
//! that's where the Rust toolchain and the existing cross-repo
//! `gymact_bridge_pack_e2e.rs` test already are -- not because ggen itself
//! performs OCEL discovery/conformance/variant analysis. Every actual
//! analysis call below (`discover_ocel_dfg`, `dfg_fitness`, `dfg_precision`,
//! `extract_ocel_variants`) is `wasm4pm_compat`'s own real, published API,
//! invoked directly; nothing here reimplements it.
//!
//! CROSS-REPO, same convention as `gymact_bridge_pack_e2e.rs`: reads the
//! real captured fixture at `$GYMACT_REPO/tests/fixtures/real_episode.ocel.json`
//! (default `$HOME/gymact`), produced by a real GymAct episode run
//! (`gymact/tests/test_core.py::test_real_episode_produces_a_valid_ocel_log_and_writes_the_conformance_fixture`).
//! Skips loudly with a named `BLOCKED:` reason if that fixture is absent --
//! never a silent pass, never a fabricated log.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::PathBuf;

use wasm4pm_compat::dfg::{discover_ocel_dfg, dfg_fitness, dfg_precision, extract_ocel_variants};
use wasm4pm_compat::ocel::OCEL;

/// Resolves the real fixture written by GymAct's own real episode test.
/// Returns `None` (never a panic) if absent -- caller must skip loudly.
fn real_episode_fixture_path() -> Option<PathBuf> {
    let gymact_repo = std::env::var("GYMACT_REPO").unwrap_or_else(|_| {
        format!(
            "{}/gymact",
            std::env::var("HOME").expect("HOME must be set")
        )
    });
    let path = PathBuf::from(gymact_repo).join("tests/fixtures/real_episode.ocel.json");
    path.is_file().then_some(path)
}

macro_rules! require_real_episode_fixture {
    () => {
        match real_episode_fixture_path() {
            Some(path) => path,
            None => {
                eprintln!(
                    "BLOCKED:GYMACT_OCEL_FIXTURE_ABSENT -- no real_episode.ocel.json found at \
                     $GYMACT_REPO/tests/fixtures/real_episode.ocel.json (default $HOME/gymact/\
                     tests/fixtures/real_episode.ocel.json). Run \
                     `uv run pytest tests/test_core.py::test_real_episode_produces_a_valid_ocel_log_and_writes_the_conformance_fixture` \
                     in ~/gymact to generate it, and/or set GYMACT_REPO; skipping, not failing, \
                     since this is a real external repo artifact, not a bug in ggen."
                );
                return;
            }
        }
    };
}

/// The bridge's own declared normative sequencing for a real GymAct episode
/// lifecycle (`materialize -> act -> act -> teardown`, matching the real
/// fixture's own operation sequence -- `verify()` issues no `Receipt` in the
/// current runtime, so it carries no OCEL event and no normative arc here).
/// This is hand-declared from the real lifecycle documented in
/// `~/gymact/src/gymact/runtime.py`, not re-derived from RDF: the bridge
/// ontology has no sequencing facts today (out of scope to add here).
fn normative_arcs() -> Vec<(String, String)> {
    vec![
        ("materialize".to_string(), "act".to_string()),
        ("act".to_string(), "act".to_string()),
        ("act".to_string(), "teardown".to_string()),
    ]
}

#[test]
fn real_gymact_episode_conforms_to_the_declared_lifecycle_model() {
    let fixture_path = require_real_episode_fixture!();
    let raw = std::fs::read_to_string(&fixture_path).expect("read real OCEL fixture");
    let ocel: OCEL = serde_json::from_str(&raw).expect("deserialize real OCEL 2.0 log");

    assert!(
        !ocel.events.is_empty(),
        "real fixture must carry real events, not an empty log"
    );

    // Item 2: discover the actual DFG from the real run -- not the declared one.
    let dfg = discover_ocel_dfg(&ocel);
    assert!(
        !dfg.edges.is_empty(),
        "a real 4-event single-episode run must discover at least one real arc: {dfg:?}"
    );

    // Item 3: conformance-check the discovered DFG against the bridge's own
    // declared normative model. A single real, uncorrupted episode run is
    // expected to fit perfectly (every normative arc appears; no unexpected
    // transition appears) -- both assert to the real, computed 1.0, not a
    // hardcoded stand-in.
    let arcs = normative_arcs();
    let fitness = dfg_fitness(&dfg, &arcs);
    let precision = dfg_precision(&dfg, &arcs);
    assert!(
        (fitness - 1.0).abs() < f64::EPSILON,
        "every declared normative arc must appear in the real run's discovered DFG: \
         fitness={fitness}, dfg={dfg:?}, normative_arcs={arcs:?}"
    );
    assert!(
        (precision - 1.0).abs() < f64::EPSILON,
        "the real run must introduce no transition outside the declared normative model: \
         precision={precision}, dfg={dfg:?}, normative_arcs={arcs:?}"
    );

    // Item 5: real variant extraction over the real log. `extract_ocel_variants`
    // returns one sequence per OCEL object id -- the `episode`/`environment`
    // objects each carry the full 4-event sequence, while per-capability
    // objects (delete/increment/materialize/teardown each have distinct
    // `capability_ref`s) carry only their own single event. The real
    // episode's full lifecycle is the longest such sequence, unambiguously.
    let variants = extract_ocel_variants(&ocel);
    assert!(
        !variants.is_empty(),
        "a real single-episode run must yield at least one real variant"
    );
    let episode_variant = variants
        .iter()
        .max_by_key(|v| v.len())
        .expect("at least one real variant must exist");
    assert_eq!(
        episode_variant,
        &vec![
            "materialize".to_string(),
            "act".to_string(),
            "act".to_string(),
            "teardown".to_string(),
        ],
        "the real episode's variant must match its real, in-order operation sequence"
    );
}
