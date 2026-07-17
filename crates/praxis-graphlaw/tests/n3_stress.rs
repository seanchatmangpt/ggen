//! Stress and counterfactual tests for the N3 forward-chaining reasoner.
//!
//! `lib/tests/n3_conformance/vendored/deep_taxonomy.n3` deliberately truncates
//! EYE's own `deep-taxonomy` scalability benchmark (a 10000-level,
//! ~30000-triple `rdfs:subClassOf` chain) down to a 3-level chain, for a fast
//! conformance check. This file restores the *scale* dimension: it builds a
//! deep chain programmatically (so the fixture itself doesn't bloat the repo)
//! and asserts both correctness (the type propagates all the way to the top
//! of the chain) and that the fixpoint terminates in a practical amount of
//! time, using the exact same backward-chaining `<=` rule as the vendored
//! conformance case.

use praxis_graphlaw::TripleStore;
use std::time::{Duration, Instant};

/// Build an N3 document encoding a linear `rdfs:subClassOf` chain of the
/// given depth (`:N0 rdfs:subClassOf :N1 . :N1 rdfs:subClassOf :N2 . ...`)
/// with a single fact at the bottom (`:TestVariable a :N0`), plus the same
/// backward-chaining transitivity rule EYE's `deep-taxonomy` benchmark uses.
fn build_deep_taxonomy_chain(depth: usize) -> String {
    let mut doc = String::from(
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
         @prefix : <http://example.org/deep#>.\n\n\
         :TestVariable rdf:type :N0.\n\n",
    );
    for i in 0..depth {
        doc.push_str(&format!(":N{} rdfs:subClassOf :N{}.\n", i, i + 1));
    }
    doc.push_str("\n{?X a ?D} <= {?C rdfs:subClassOf ?D. ?X a ?C}.\n");
    doc
}

/// Stress test: a 200-level `rdfs:subClassOf` chain (~65x deeper than the
/// truncated conformance fixture) must still correctly derive that
/// `:TestVariable` is a member of the topmost class, and must do so within a
/// generous but bounded wall-clock budget (this is a correctness+termination
/// check, not a tight performance benchmark -- see `lib/benches/` for that).
///
/// Measured characteristic (release build, `timing_test2` probe used during
/// development, not checked in): this reasoner's fixpoint is naive/non-semi-naive
/// -- each iteration re-matches every rule against the *entire* current
/// `TripleIndex` rather than only against facts newly derived in the prior
/// iteration -- so a single linear chain of depth N costs roughly O(N)
/// iterations x O(N) rescan cost each, i.e. cubic-ish overall (measured:
/// depth 100 -> 14ms, 300 -> 298ms, 500 -> 1.3s in release; roughly a 21x
/// slowdown for a 5x depth increase, consistent with O(N^3)). 200 is chosen
/// here specifically because a debug build (what `cargo test` runs by
/// default) is meaningfully slower than release, and 500 in debug mode
/// exceeded 30 seconds during development -- 200 keeps this a fast,
/// reliable correctness check without either silently weakening the test or
/// making the default test run flaky/slow on CI. This cubic-ish scaling is a
/// known, real characteristic of the current implementation (not a bug to
/// silently paper over) -- semi-naive/incremental evaluation, which only
/// re-matches rules against the delta of newly-derived facts each round,
/// would bring this down to roughly linear and is the natural next
/// optimization if deeper chains are needed in practice.
#[test]
fn test_deep_taxonomy_chain_200_levels() {
    const DEPTH: usize = 200;
    let doc = build_deep_taxonomy_chain(DEPTH);

    let start = Instant::now();
    let mut store = TripleStore::from(&doc);
    let inferred = store.materialize().unwrap();
    let elapsed = start.elapsed();

    let decoded: Vec<String> = inferred.iter().map(TripleStore::decode_triple).collect();
    let top_class = format!("N{}", DEPTH);
    assert!(
        decoded.iter().any(|d| d.contains("TestVariable") && d.contains(&format!("#{}>", top_class))),
        "expected :TestVariable to be typed all the way up to :{} via {}-level transitivity, but it wasn't derived (derived {} triples total)",
        top_class, DEPTH, decoded.len()
    );

    // Every intermediate class along the chain must also have been derived
    // (this is what actually stresses the fixpoint -- a shallow/incorrect
    // implementation might only catch the last hop rather than the full
    // transitive chain).
    for i in 1..=DEPTH {
        let class = format!("N{}", i);
        assert!(
            decoded
                .iter()
                .any(|d| d.contains("TestVariable") && d.contains(&format!("#{}>", class))),
            ":TestVariable should have been derived as a member of :{} (hop {} of {})",
            class,
            i,
            DEPTH
        );
    }

    assert!(
        elapsed < Duration::from_secs(20),
        "{}-level transitive closure took {:?}, expected well under 20s (debug build) -- possible fixpoint performance regression",
        DEPTH, elapsed
    );
}

/// Counterfactual: a *broken* chain (one missing link partway up) must NOT
/// propagate the type past the break. This guards against an
/// over-permissive implementation that derives more than it should (e.g. by
/// conflating "is a subclass of something in the chain" with "is a subclass
/// of everything downstream regardless of an actual link").
#[test]
fn test_deep_taxonomy_chain_broken_link_stops_propagation() {
    const DEPTH: usize = 50;
    const BREAK_AT: usize = 20; // omit the :N20 rdfs:subClassOf :N21 triple

    let mut doc = String::from(
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
         @prefix : <http://example.org/deep#>.\n\n\
         :TestVariable rdf:type :N0.\n\n",
    );
    for i in 0..DEPTH {
        if i == BREAK_AT {
            continue; // deliberately break the chain here
        }
        doc.push_str(&format!(":N{} rdfs:subClassOf :N{}.\n", i, i + 1));
    }
    doc.push_str("\n{?X a ?D} <= {?C rdfs:subClassOf ?D. ?X a ?C}.\n");

    let mut store = TripleStore::from(&doc);
    let inferred = store.materialize().unwrap();
    let decoded: Vec<String> = inferred.iter().map(TripleStore::decode_triple).collect();

    // Everything up to and including the break point should still be derived...
    for i in 1..=BREAK_AT {
        let class = format!("N{}", i);
        assert!(
            decoded
                .iter()
                .any(|d| d.contains("TestVariable") && d.contains(&format!("#{}>", class))),
            ":TestVariable should still be derived as :{} (before the break)",
            class
        );
    }
    // ...but nothing past the break should be, since the link is missing.
    for i in (BREAK_AT + 1)..=DEPTH {
        let class = format!("N{}", i);
        assert!(
            !decoded.iter().any(|d| d.contains("TestVariable") && d.contains(&format!("#{}>", class))),
            ":TestVariable must NOT be derived as :{} -- the chain is broken at N{}->N{}, so this would be a false positive",
            class, BREAK_AT, BREAK_AT + 1
        );
    }
}
