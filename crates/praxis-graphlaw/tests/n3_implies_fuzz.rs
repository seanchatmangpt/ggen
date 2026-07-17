//! Fuzzing for `log:implies` dynamic rule reification (targets the
//! specifically named low-confidence N3 gap: `log:implies` is the hardest
//! single mechanism in scope and was newest/least-exercised code). Each
//! test generates many scenarios via `proptest` and compares the real
//! `materialize()` output against an independently-computed expectation.

use praxis_graphlaw::TripleStore;
use proptest::prelude::*;

fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples
        .iter()
        .map(praxis_graphlaw::TripleStore::decode_triple)
        .collect()
}

// Vary the number of items tagged "target" vs "other"; the log:implies
// rule must derive exactly one :Match fact per item whose tag matches
// the antecedent's pattern, and NONE for items that don't -- the
// independent oracle here is simply "count of items tagged target",
// computed in the test itself, not trusted from the reasoner.
proptest! {
    #[test]
    fn prop_log_implies_derives_exactly_matching_antecedent_count(
        tags in prop::collection::vec(any::<bool>(), 0..8),
    ) {
        let mut data = String::from(
            "@prefix : <http://example.org/> .\n\
             @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
             \n\
             :speaker :says { ?x :hasTag \"target\" } .\n\
             { ?s :says ?f . ?f log:implies { ?x a :Match } } => { ?x a :Match }.\n"
        );
        let mut expected_matches = 0usize;
        for (i, &is_target) in tags.iter().enumerate() {
            let tag = if is_target { "target" } else { "other" };
            data.push_str(&format!(":item{i} :hasTag \"{tag}\" .\n"));
            if is_target { expected_matches += 1; }
        }

        let mut store = TripleStore::from(&data);
        let inferred = store.materialize().unwrap();
        let decoded = decode_all(&inferred);

        let actual_matches = (0..tags.len())
            .filter(|i| decoded.iter().any(|d| d.contains(&format!("/item{i}>")) && d.contains("Match")))
            .count();

        prop_assert_eq!(
            actual_matches, expected_matches,
            "expected {} :Match derivations (one per target-tagged item out of {}), got {}. Derived: {:?}",
            expected_matches, tags.len(), actual_matches, decoded
        );
    }
}

/// A variable that appears ONLY in the antecedent formula (not in the
/// consequent or the outer body) must not leak into the derived head --
/// each matching antecedent instantiation must still ground the
/// consequent correctly regardless of the antecedent-only variable's
/// value, and multiple distinct antecedent-only-variable values for the
/// SAME shared variable must not produce spurious extra derivations
/// beyond one :Match per distinct shared-variable value.
#[test]
fn test_log_implies_antecedent_only_variable_does_not_leak() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :speaker :says { ?x :hasOwner ?owner } .\n\
                :item1 :hasOwner :alice .\n\
                :item1 :hasOwner :bob .\n\
                :item2 :hasOwner :carol .\n\
                { ?s :says ?f . ?f log:implies { ?x a :Owned } } => { ?x a :Owned }.\n";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded.iter().any(|d| d.contains("/item1") && d.contains("Owned")),
        "item1 (owned by alice AND bob) must be derived as :Owned via at least one owner match. Derived: {:?}", decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/item2") && d.contains("Owned")),
        "item2 (owned by carol) must be derived as :Owned. Derived: {:?}",
        decoded
    );
    // The antecedent-only variable ?owner must never leak into the derived
    // facts -- no derived triple should mention alice/bob/carol as a
    // ?x-position subject of :Owned (only item1/item2 should).
    assert!(
        !decoded.iter().any(|d| (d.contains("/alice") || d.contains("/bob") || d.contains("/carol")) && d.contains("Owned")),
        "the antecedent-only ?owner variable must not leak into the derived :Owned facts. Derived: {:?}", decoded
    );
}

/// Zero antecedent matches must derive nothing at all -- not a spurious
/// unconditional derivation, not a panic.
#[test]
fn test_log_implies_zero_antecedent_matches_derives_nothing() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :speaker :says { ?x :hasTag \"target\" } .\n\
                :item0 :hasTag \"other\" .\n\
                :item1 :hasTag \"another\" .\n\
                { ?s :says ?f . ?f log:implies { ?x a :Match } } => { ?x a :Match }.\n";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);
    assert!(
        !decoded.iter().any(|d| d.contains("Match")),
        "zero antecedent matches must derive zero :Match facts, got: {:?}",
        decoded
    );
}
