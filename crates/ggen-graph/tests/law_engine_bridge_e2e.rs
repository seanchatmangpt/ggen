//! Consumer-side proof of the RDF engine bridge seam
//! (`docs/jira/v26.7.16/03-RDF-ENGINE-BRIDGE-DESIGN.md`, "Definition of done" item 2:
//! "At least one consumer (`ggen-graph` or `ggen-marketplace`) demonstrated calling
//! `materialize`/`validate_shacl`/`check_denials` and folding the N-Triples result back
//! into its own oxigraph store").
//!
//! `ggen-engine`'s own `tests/law_engine_test.rs` already proves the trait works from
//! *inside* the engine crate. What it cannot prove is the part this file exists for: that
//! the seam is crossable by an oxigraph-based consumer that never links `oxrdf`/`spargebra`.
//! Here every fact leaves this crate's `DeterministicGraph` as an N-Triples **string**, is
//! evaluated by `praxis-graphlaw` inside `ggen-engine`, and comes back as N-Triples
//! **strings** that are re-parsed and inserted into the same oxigraph store. No model type
//! crosses the boundary in either direction.
//!
//! Chicago-school: real `GraphLawEngine`, real oxigraph `DeterministicGraph`, real SPARQL
//! evaluation. Assertions are on final store state (what SPARQL can see after the fold-back),
//! never on "was this called".

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::law_engine::{GraphLawEngine, LawEngine};
use ggen_graph::graph::parse::parse_ntriples;
use ggen_graph::graph::serialize::serialize_to_string;
use ggen_graph::DeterministicGraph;
use oxigraph::io::RdfFormat;
use oxigraph::sparql::QueryResults;

/// `ex:rex` is asserted as a `Dog` only. `ex:rex a ex:Animal` exists nowhere in this
/// input — it can only ever enter the store as a fact derived by [`RULE_N3`] and carried
/// back across the seam.
const DOG_TTL: &str = "@prefix ex: <http://example.org/> . ex:rex a ex:Dog .";

/// N3 rule: every `Dog` is an `Animal`.
const RULE_N3: &str = "@prefix ex: <http://example.org/>. {?s a ex:Dog} => {?s a ex:Animal}.";

/// SPARQL, evaluated by oxigraph against this crate's own store.
const ASK_REX_IS_ANIMAL: &str =
    "ASK { <http://example.org/rex> a <http://example.org/Animal> }";

/// Load Turtle into a fresh oxigraph-backed store.
fn store_with(ttl: &str) -> DeterministicGraph {
    let graph = DeterministicGraph::new().expect("oxigraph store");
    let parsed = ggen_graph::parse_turtle_located(ttl);
    assert!(
        parsed.diagnostics.is_empty(),
        "fixture Turtle must parse cleanly: {:?}",
        parsed.diagnostics
    );
    for quad in &parsed.quads {
        graph.insert_quad(quad).expect("insert fixture quad");
    }
    graph
}

/// Serialize the store's full contents out as N-Triples — the only form in which facts
/// are allowed to cross into `ggen-engine`.
fn facts_ntriples(graph: &DeterministicGraph) -> String {
    let quads = graph.all_quads().expect("all_quads");
    serialize_to_string(&quads, RdfFormat::NTriples).expect("serialize N-Triples")
}

/// Fold N-Triples lines returned by a [`LawEngine`] call back into the consumer's own
/// oxigraph store. This is the step the bridge design assigns to the *caller*: the engine
/// never reaches into this store. `MaterializeOutcome::derived` lines carry no terminating
/// ` .`, so it is restored here before re-parsing.
fn fold_back(graph: &DeterministicGraph, derived: &[String]) -> usize {
    let doc = derived
        .iter()
        .map(|line| format!("{line} .\n"))
        .collect::<String>();
    let quads = parse_ntriples(&doc).expect("derived N-Triples must re-parse");
    for quad in &quads {
        graph.insert_quad(quad).expect("insert derived quad");
    }
    quads.len()
}

fn ask(graph: &DeterministicGraph, query: &str) -> bool {
    match graph.query(query).expect("sparql ask") {
        QueryResults::Boolean(b) => b,
        // `QueryResults` does not implement `Debug`, so name the variant explicitly
        // rather than formatting the value.
        QueryResults::Solutions(_) => panic!("expected a boolean ASK result, got Solutions"),
        QueryResults::Graph(_) => panic!("expected a boolean ASK result, got Graph"),
    }
}

/// THE load-bearing test. Negative control first (the derived fact is genuinely absent
/// before the bridge runs), then the round trip, then the same SPARQL query answering
/// differently — which is only possible if real facts crossed the seam in both directions.
#[test]
fn materialize_across_seam_lands_derived_fact_in_oxigraph_store() {
    let graph = store_with(DOG_TTL);

    // Negative control: oxigraph alone does no N3 reasoning, so the rule-implied fact
    // must be absent. Without this, the assertion below could pass on a fixture typo.
    assert!(
        !ask(&graph, ASK_REX_IS_ANIMAL),
        "ex:rex a ex:Animal must NOT be present before materialization -- if it is, the \
         fixture already contains the fact and the round trip below proves nothing"
    );
    let quads_before = graph.all_quads().expect("all_quads").len();

    // Out across the seam as a string, evaluated by praxis-graphlaw inside ggen-engine.
    let engine = GraphLawEngine::new();
    let outcome = engine
        .materialize(&facts_ntriples(&graph), RULE_N3)
        .expect("materialize across the LawEngine seam");

    assert_eq!(outcome.rules_loaded, 1, "the N3 rule must have been loaded");
    assert!(
        outcome
            .derived
            .iter()
            .any(|l| l.contains("example.org/rex") && l.contains("example.org/Animal")),
        "engine must derive ex:rex a ex:Animal: {:?}",
        outcome.derived
    );

    // Back across the seam: caller-owned re-ingestion into this crate's own store.
    let folded = fold_back(&graph, &outcome.derived);
    assert!(folded > 0, "fold-back must insert at least one derived quad");

    assert!(
        ask(&graph, ASK_REX_IS_ANIMAL),
        "after fold-back the derived fact must be visible to oxigraph SPARQL"
    );
    assert!(
        graph.all_quads().expect("all_quads").len() > quads_before,
        "the store must have genuinely grown by the derived facts"
    );
}

/// A no-op or passthrough bridge would return the same `derived` set with and without
/// rules. Proving the empty-rules case stays empty is what makes the test above meaningful.
#[test]
fn materialize_across_seam_with_no_rules_changes_nothing_in_the_store() {
    let graph = store_with(DOG_TTL);
    let quads_before = graph.all_quads().expect("all_quads").len();

    let engine = GraphLawEngine::new();
    let outcome = engine
        .materialize(&facts_ntriples(&graph), "")
        .expect("materialize with no rules must still succeed");

    assert_eq!(outcome.rules_loaded, 0);
    assert!(
        outcome.derived.is_empty(),
        "no rules loaded -> nothing new can be derived: {:?}",
        outcome.derived
    );

    fold_back(&graph, &outcome.derived);
    assert_eq!(
        graph.all_quads().expect("all_quads").len(),
        quads_before,
        "an empty derived set must leave the consumer's store byte-identical in size"
    );
    assert!(
        !ask(&graph, ASK_REX_IS_ANIMAL),
        "no rule was loaded, so the rule-implied fact must still be absent"
    );
}

/// `check_denials` over facts serialized out of this crate's own store. The denial fires
/// on a fact that is really in the oxigraph store, not on a literal handed to the engine.
#[test]
fn check_denials_across_seam_reports_violation_from_store_facts() {
    let clean = store_with(DOG_TTL);
    let engine = GraphLawEngine::new();
    const DENIAL_N3: &str =
        "@prefix ex: <http://example.org/>. {?s a ex:Forbidden} => false.";

    // Negative control: the same rule over a store with no Forbidden individual.
    let none = engine
        .check_denials(&facts_ntriples(&clean), DENIAL_N3)
        .expect("check_denials on clean facts");
    assert!(
        none.is_empty(),
        "no ex:Forbidden individual exists, so no denial may fire: {none:?}"
    );

    // Now a store that really does violate the denial.
    let dirty = store_with("@prefix ex: <http://example.org/> . ex:x a ex:Forbidden .");
    let denials = engine
        .check_denials(&facts_ntriples(&dirty), DENIAL_N3)
        .expect("check_denials on violating facts");
    assert_eq!(denials.len(), 1, "exactly one violated denial: {denials:?}");
    assert!(
        denials[0].contains("DENIED"),
        "denial line must name the refusal: {denials:?}"
    );
}

/// `validate_shacl` over facts serialized out of this crate's own store.
///
/// Per the bridge design, this does NOT replace `ggen_graph::validate_shacl`
/// (`src/shacl.rs`, re-exported from `src/lib.rs`), which stays authoritative for this
/// crate's existing non-law callers. The seam's SHACL is for the mu-pipeline's law gate
/// only; both are exercised here on the same facts so the two validators cannot silently
/// diverge on the conforming case without this test noticing.
#[test]
fn validate_shacl_across_seam_flags_violation_from_store_facts() {
    const SHAPES_TTL: &str = r#"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
ex:DogShape a sh:NodeShape ;
    sh:targetClass ex:Dog ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] .
"#;

    let engine = GraphLawEngine::new();

    // ex:rex is a Dog with no ex:name -> the minCount 1 constraint must fail.
    let missing_name = store_with(DOG_TTL);
    let bad = engine
        .validate_shacl(&facts_ntriples(&missing_name), SHAPES_TTL)
        .expect("validate_shacl across the seam");
    assert!(
        !bad.conforms,
        "ex:rex has no ex:name, so the shapes graph must not conform"
    );
    assert!(
        bad.violations.iter().any(|v| v.contains("example.org/rex")),
        "the violation must name ex:rex as its focus node: {:?}",
        bad.violations
    );

    // Same shape, same class, but the required property is present.
    let with_name =
        store_with("@prefix ex: <http://example.org/> . ex:rex a ex:Dog ; ex:name \"Rex\" .");
    let good = engine
        .validate_shacl(&facts_ntriples(&with_name), SHAPES_TTL)
        .expect("validate_shacl across the seam");
    assert!(
        good.conforms,
        "ex:rex now has an ex:name, so the shapes graph must conform: {:?}",
        good.violations
    );
    assert!(good.violations.is_empty(), "{:?}", good.violations);
}
