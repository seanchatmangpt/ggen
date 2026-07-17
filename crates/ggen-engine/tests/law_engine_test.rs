//! Load-bearing proof for the [`ggen_engine::law_engine::LawEngine`] trait
//! (`specs/014-ggen-core-replacement/contracts/law-engine-trait.md`): the
//! `graphlaw_e2e.rs` pattern applied to the new stateless seam — the same
//! fact set derives nothing extra with no rules loaded, and derives the
//! rule-implied fact once `rules_n3` is supplied. A no-op/decorative bridge
//! would return the same (empty) `derived` set either way; this test proves
//! it does not.

use ggen_engine::law_engine::{GraphLawEngine, LawEngine};

/// `ex:rex` is asserted as a `Dog` only — `ex:rex a ex:Animal` can only ever
/// exist as a fact derived by the N3 rule below, never as an input fact.
const FACTS_NTRIPLES: &str = "<http://example.org/rex> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Dog> .\n";

/// N3 rule: every `Dog` is an `Animal`.
const RULE_N3: &str = "@prefix ex: <http://example.org/>. {?s a ex:Dog} => {?s a ex:Animal}.";

#[test]
fn materialize_without_rules_derives_nothing_new() {
    let engine = GraphLawEngine::new();
    let outcome = engine
        .materialize(FACTS_NTRIPLES, "")
        .expect("materialize with no rules must still succeed");
    assert_eq!(outcome.rules_loaded, 0);
    assert!(
        outcome.derived.is_empty(),
        "no rules loaded -> nothing new can be derived: {:?}",
        outcome.derived
    );
}

/// THE load-bearing proof: loading `RULE_N3` over the identical fact set
/// derives `ex:rex a ex:Animal` — a fact present nowhere in
/// `FACTS_NTRIPLES`. This can only pass if `GraphLawEngine::materialize`
/// actually round-trips through the `praxis_graphlaw::TripleStore` reasoner,
/// not a passthrough.
#[test]
fn materialize_with_n3_rule_derives_new_fact() {
    let engine = GraphLawEngine::new();
    let outcome = engine
        .materialize(FACTS_NTRIPLES, RULE_N3)
        .expect("materialize with a loaded rule must succeed");
    assert_eq!(outcome.rules_loaded, 1);
    assert!(
        outcome
            .derived
            .iter()
            .any(|l| l.contains("example.org/rex") && l.contains("example.org/Animal")),
        "expected a derived `rex a Animal` fact, got: {:?}",
        outcome.derived
    );
}

/// A SHACL shape the asserted facts violate (missing a required property)
/// is reported as non-conforming, naming the focus node — proving
/// `validate_shacl` reaches the real `praxis_graphlaw` SHACL validator, not
/// a stub that always reports conformance.
#[test]
fn validate_shacl_reports_real_violation() {
    const SHAPES_TTL: &str = r#"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
ex:DogShape a sh:NodeShape ;
    sh:targetClass ex:Dog ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] .
"#;
    let engine = GraphLawEngine::new();
    let outcome = engine
        .validate_shacl(FACTS_NTRIPLES, SHAPES_TTL)
        .expect("SHACL validation call must succeed even when facts don't conform");
    assert!(
        !outcome.conforms,
        "rex has no ex:name -- must NOT conform: {outcome:?}"
    );
    assert!(
        outcome.violations.iter().any(|v| v.contains("rex")),
        "violation must name the focus node: {:?}",
        outcome.violations
    );
}

/// A violated denial rule (`{ body } => false.`) is reported by
/// `check_denials` after materialization — proving denials are evaluated
/// against the post-materialization fact state, not just the raw input.
#[test]
fn check_denials_reports_violated_denial_after_materialization() {
    const DENIAL_N3: &str =
        "@prefix ex: <http://example.org/>. {?s a ex:Animal} => false.";
    let rules = format!("{RULE_N3}\n{DENIAL_N3}");
    let engine = GraphLawEngine::new();
    let denials = engine
        .check_denials(FACTS_NTRIPLES, &rules)
        .expect("check_denials call must succeed");
    assert!(
        !denials.is_empty(),
        "the derived `rex a Animal` fact must trip the denial rule: {denials:?}"
    );
}
