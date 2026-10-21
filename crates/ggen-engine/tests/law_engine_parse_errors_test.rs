//! E-PARSE structured parse-refusal surface (v26.10.20 dialect slice 1, V15).
//!
//! Proves that parse refusals crossing the `LawEngine` seam carry the
//! machine-readable `[E-PARSE-{DIALECT}]` prefix with position and message,
//! instead of the previous lossy `{:?}` dumps. Real graphlaw engines over
//! real malformed documents — no doubles.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: unwrap/expect/panic allowed in test code

use ggen_engine::law_engine::{GraphLawEngine, LawEngine};

#[test]
fn n3_formula_fed_as_turtle_hook_pack_carries_e_parse_turtle() {
    // N3 formula syntax is not Turtle: the kh hook-pack path parses Turtle
    // through GraphLawStore (which wraps the same law_engine kernel).
    let store = ggen_engine::graph::GraphLawStore::new().expect("store constructs");
    let n3_formula = "{ <http://ex/s> <http://ex/p> <http://ex/o> } => false .";
    let err = GraphEngine::load_hook_pack(&store, n3_formula).expect_err("must refuse");
    let msg = format!("{err}");
    assert!(
        msg.contains("[E-PARSE-TURTLE]"),
        "expected E-PARSE-TURTLE prefix in: {msg}"
    );
    assert!(
        msg.to_uppercase().contains("TURTLE"),
        "expected dialect name in: {msg}"
    );
}

use ggen_engine::graph::GraphEngine;

#[test]
fn genuine_n3_syntax_error_carries_e_parse_n3_with_message() {
    let engine = GraphLawEngine::new();
    // Valid N-Triples facts, garbage rule text: parse_n3 must refuse.
    let facts = "<http://ex/s> <http://ex/p> <http://ex/o> .\n";
    let bad_rules = "@prefix : <http://ex/> .\n:s :p >>>not n3<<< .\n";
    let err = engine
        .materialize(facts, bad_rules)
        .expect_err("must refuse");
    let msg = format!("{err}");
    eprintln!("witnessed refusal: {msg}");
    assert!(
        msg.contains("[E-PARSE-N3]"),
        "expected E-PARSE-N3 prefix in: {msg}"
    );
    // The refusal must carry the engine's diagnostic message, not just kind.
    assert!(
        msg.len() > "[FM-LAW-010] GraphLaw rule/fact load refused: [E-PARSE-N3] ".len(),
        "expected a real diagnostic message in: {msg}"
    );
}

#[test]
fn n3_formula_fed_as_ntriples_facts_carries_e_parse_ntriples() {
    let engine = GraphLawEngine::new();
    let formula_as_ntriples = "{ <http://ex/s> <http://ex/p> <http://ex/o> } .";
    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .\n";
    let err = engine
        .validate_shacl(formula_as_ntriples, shapes)
        .expect_err("must refuse");
    let msg = format!("{err}");
    assert!(
        msg.contains("[E-PARSE-NTRIPLES]"),
        "expected E-PARSE-NTRIPLES prefix in: {msg}"
    );
}

#[test]
fn valid_documents_still_succeed_after_refusal_formatting() {
    let engine = GraphLawEngine::new();
    let facts = "<http://ex/s> <http://ex/p> <http://ex/o> .\n";
    let rules =
        "@prefix : <http://ex/> .\n{ ?s <http://ex/p> ?o . } => { ?s <http://ex/q> ?o . } .\n";
    let out = engine
        .materialize(facts, rules)
        .expect("valid documents must still derive");
    assert!(
        out.derived.iter().any(|l| l.contains("<http://ex/q>")),
        "expected derived q-triple in: {:?}",
        out.derived
    );
}
