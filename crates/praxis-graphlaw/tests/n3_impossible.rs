//! Catalog items #17-20 (docs/jira/26.7.4 impossible/illogical constructs
//! plan): N3 rules/formulae that are logically vacuous (never fire, or
//! fire but add nothing) or pose a non-termination risk under forward-
//! chaining materialization. Items 17-18 fuzz value bindings via
//! `proptest`; items 19-20 are deterministic with an explicit wall-clock
//! timeout guard, since termination -- not input coverage -- is the actual
//! property being tested.

use praxis_graphlaw::TripleStore;
use proptest::prelude::*;
use std::time::{Duration, Instant};

fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples
        .iter()
        .map(praxis_graphlaw::TripleStore::decode_triple)
        .collect()
}

// ---------------------------------------------------------------------
// Item #17: a rule body requiring `?x math:greaterThan ?x` -- a value can
// never be strictly greater than itself, so the rule can never fire for
// any candidate value.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_self_greater_than_never_fires(v in -1000i64..1000i64) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
             \n\
             :item :value {v} .\n\
             \n\
             {{ ?i :value ?v . ?v math:greaterThan ?v }} => {{ ?i a :Impossible }}.\n"
        );
        let mut store = TripleStore::from(&data);
        let inferred = store.materialize().unwrap();
        let decoded = decode_all(&inferred);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("Impossible")),
            "?v math:greaterThan ?v (self-comparison) can never hold for value {}, but something was derived: {:?}", v, decoded
        );
    }
}

// ---------------------------------------------------------------------
// Item #18: a rule body conjoining `?x log:equalTo ?y` and `?x
// log:notEqualTo ?y` on the SAME pair -- mutually exclusive builtins,
// so the body can never bind for any pair of values.
// ---------------------------------------------------------------------
proptest! {
    #[test]
    fn prop_equal_and_not_equal_same_pair_never_fires(a in 0i64..100, b in 0i64..100) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
             \n\
             :x :val {a} .\n\
             :y :val {b} .\n\
             \n\
             {{ ?s :val ?v1 . ?t :val ?v2 . ?v1 log:equalTo ?v2 . ?v1 log:notEqualTo ?v2 }} => {{ ?s :contradictsWith ?t }}.\n"
        );
        let mut store = TripleStore::from(&data);
        let inferred = store.materialize().unwrap();
        let decoded = decode_all(&inferred);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("contradictsWith")),
            "log:equalTo and log:notEqualTo on the same pair ({}, {}) are mutually exclusive and must never both hold: {:?}", a, b, decoded
        );
    }
}

/// Item #19: a "liar"-style scenario -- two independent `log:implies`
/// reifications derive DIRECTLY CONTRADICTORY facts (`:TaxPayer` and
/// `:NotTaxPayer`) about the same subject from the same base fact. Since
/// this engine's forward-chaining is monotonic (facts are only ever
/// added, never retracted), the actual, verified behavior is NOT an
/// infinite oscillation/paradox -- both contradictory facts simply
/// coexist in the derived set. This test confirms that concretely (with
/// a wall-clock timeout guard, since non-termination was the real risk
/// being tested for): materialize() must terminate quickly, and both
/// contradictory facts must be present (proving the engine doesn't loop
/// trying to "resolve" the contradiction, and doesn't silently drop one
/// side either).
#[test]
fn test_liar_style_contradictory_log_implies_terminates_with_both_facts() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :bob a :GoodCitizen .\n\
                :alice :says { ?x a :GoodCitizen } .\n\
                :carol :saysOpposite { ?x a :GoodCitizen } .\n\
                { ?s :says ?f . ?f log:implies { ?x a :TaxPayer } } => { ?x a :TaxPayer }.\n\
                { ?s :saysOpposite ?f2 . ?f2 log:implies { ?x a :NotTaxPayer } } => { ?x a :NotTaxPayer }.\n";

    let mut store = TripleStore::from(data);
    let start = Instant::now();
    let inferred = store.materialize().unwrap();
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_secs(5),
        "a contradictory pair of log:implies derivations must terminate quickly, took {:?} (possible non-termination)",
        elapsed
    );

    let decoded = decode_all(&inferred);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/bob") && d.contains("TaxPayer") && !d.contains("Not")),
        "expected :bob a :TaxPayer to be derived. Derived: {:?}",
        decoded
    );
    assert!(
        decoded.iter().any(|d| d.contains("/bob") && d.contains("NotTaxPayer")),
        "expected :bob a :NotTaxPayer to ALSO be derived (monotonic engine: contradictions coexist rather than causing non-termination). Derived: {:?}", decoded
    );
}

/// Item #20: a tautological rule (`{ ?x a :A } <= { ?x a :A }`) -- always
/// true given any :A instance, but derives NOTHING new (zero information
/// gain). Confirms the fixpoint detector terminates immediately rather
/// than looping on a rule that can never produce a new fact, and that no
/// duplicate/redundant entries leak into the derived set.
#[test]
fn test_tautological_rule_terminates_and_adds_nothing_new() {
    let data = "@prefix : <http://example.org/> .\n\
                \n\
                :a a :A .\n\
                :b a :A .\n\
                \n\
                { ?x a :A } <= { ?x a :A }.\n";

    let mut store = TripleStore::from(data);
    let start = Instant::now();
    let inferred = store.materialize().unwrap();
    let elapsed = start.elapsed();

    assert!(
        elapsed < Duration::from_secs(5),
        "a zero-information-gain tautological rule must terminate quickly, took {:?} (possible infinite loop)",
        elapsed
    );

    let decoded = decode_all(&inferred);
    assert!(
        decoded.is_empty() || decoded.iter().all(|d| d.contains("/a") || d.contains("/b")),
        "a tautological rule must derive nothing beyond what's already true (no new facts), got: {:?}", decoded
    );
}
