//! Stress tests for recursive stratification and negation cycles in the Datalog
//! engine (`praxis_graphlaw::datalog`). Tests deep recursion chains and cycle
//! detection at scale, ensuring the stratification algorithm remains performant
//! and exhaustive even with long chains of negation-dependencies.

use praxis_graphlaw::datalog::validate_rules;
use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;
use std::collections::HashMap;
use std::time::{Duration, Instant};

fn pred(name: &str) -> String {
    format!("http://example.org/{}", name)
}

/// Stress: a 20-layer negation-stratification chain (P0 negates nothing;
/// P1 negates P0; P2 negates P1; ...; P19 negates P18) must be accepted and
/// assigned strictly increasing strata all the way down the chain, within a
/// practical time budget. This is 6-7x deeper than the conformance suite's
/// `test_three_layer_stratification_chain`, specifically to catch an O(n^2)
/// or worse stratification algorithm before it becomes a real bottleneck.
#[test]
fn test_deep_stratification_chain_20_layers() {
    const LAYERS: usize = 20;
    let mut rules = Vec::new();

    // P0(x) :- Base(x).  (stratum 0, no negation)
    rules.push(Rule {
        head: Triple::from(
            "?x".to_string(),
            pred("P0"),
            "http://example.org/true".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                pred("Base"),
                "http://example.org/true".to_string(),
            ),
        }],
    });

    // P{i}(x) :- Base(x), not P{i-1}(x).  for i in 1..LAYERS
    for i in 1..LAYERS {
        rules.push(Rule {
            head: Triple::from(
                "?x".to_string(),
                pred(&format!("P{}", i)),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred(&format!("P{}", i - 1)),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        });
    }

    let start = Instant::now();
    let result = validate_rules(&rules, &HashMap::new());
    let elapsed = start.elapsed();

    let strata = result.expect("a valid, acyclic 20-layer negation chain must be accepted");
    assert_eq!(
        strata.len(),
        LAYERS,
        "expected one stratum assignment per rule"
    );

    // Strata must be strictly increasing along the chain: stratum(P_i) > stratum(P_{i-1}).
    for i in 1..LAYERS {
        assert!(
            strata[i] > strata[i - 1],
            "stratum(P{})={} must be strictly greater than stratum(P{})={}",
            i,
            strata[i],
            i - 1,
            strata[i - 1]
        );
    }

    assert!(
        elapsed < Duration::from_secs(5),
        "20-layer stratification took {:?}, expected well under 5s",
        elapsed
    );

    // End-to-end: materialize and check the alternating true/false pattern
    // propagates correctly all the way to the top of the chain.
    let mut store = TripleStore::new();
    store.add(Triple::from(
        "http://example.org/item".to_string(),
        pred("Base"),
        "http://example.org/true".to_string(),
    ));
    store
        .add_rules(rules)
        .expect("rules already validated above");
    let derived = store.materialize().unwrap();
    let decoded: Vec<String> = derived.iter().map(TripleStore::decode_triple).collect();

    // P0 holds (Base holds). P1 = Base & not P0 = false. P2 = Base & not P1 = true. ...
    // So P_i holds iff i is even.
    for i in 0..LAYERS {
        let should_hold = i % 2 == 0;
        let holds = decoded.iter().any(|d| d.contains(&format!("/P{}>", i)));
        assert_eq!(
            holds,
            should_hold,
            "P{} should {}hold (alternating negation chain), but derived facts were: {:?}",
            i,
            if should_hold { "" } else { "NOT " },
            decoded
        );
    }
}

/// Counterfactual: the SAME 20-layer chain, but with one extra rule closing
/// it into a cycle (P0 additionally negates P19) must be REJECTED, not
/// silently accepted just because the chain is long. Guards against a
/// stratification algorithm that only checks local (adjacent-layer) cycles
/// and misses one that closes across the whole chain.
#[test]
fn test_deep_stratification_chain_with_far_cycle_rejected() {
    const LAYERS: usize = 20;
    let mut rules = Vec::new();

    rules.push(Rule {
        head: Triple::from(
            "?x".to_string(),
            pred("P0"),
            "http://example.org/true".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    pred("Base"),
                    "http://example.org/true".to_string(),
                ),
            },
            // The far cycle-closing edge: P0 negates P{LAYERS-1}.
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    pred(&format!("P{}", LAYERS - 1)),
                    "http://example.org/true".to_string(),
                ),
            },
        ],
    });
    for i in 1..LAYERS {
        rules.push(Rule {
            head: Triple::from(
                "?x".to_string(),
                pred(&format!("P{}", i)),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred(&format!("P{}", i - 1)),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        });
    }

    let result = validate_rules(&rules, &HashMap::new());
    assert!(
        result.is_err(),
        "a 20-layer chain closed into a cycle by one far edge (P0 negates P19) must be rejected as unstratifiable, not silently accepted"
    );
}

/// Counterfactual: a diamond-shaped dependency must be rejected when BOTH
/// converging paths pass through negation back to a common ancestor, closing
/// a cycle through reconvergence rather than a simple linear chain. Shape:
///   P(x) :- Base(x), not Q(x).
///   Q(x) :- Base(x), not R(x).
///   R(x) :- Base(x), not S(x).
///   S(x) :- Base(x), not P(x).      <- closes the cycle: P -> Q -> R -> S -> P
/// This isn't a diamond in the classic DAG sense (two paths reconverging on a
/// single downstream node) but a cycle *discovered through* multiple
/// negation-mediated hops -- guards against a stratification checker that
/// only tracks the most recently visited predicate rather than the full
/// transitive negation-dependency graph.
#[test]
fn test_diamond_reconvergence_rejected() {
    let rules = vec![
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("P"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Q"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("Q"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("R"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("R"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("S"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("S"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("P"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
    ];

    let result = validate_rules(&rules, &HashMap::new());
    assert!(
        result.is_err(),
        "a 4-predicate cycle closed entirely through negation edges (P->Q->R->S->P) must be rejected as unstratifiable"
    );
}

/// Counterfactual: a ruleset containing TWO independent, disjoint
/// unstratifiable cycles (predicates A/B and X/Y, with no edges connecting
/// the two groups) must still be rejected. Guards against a stratification
/// checker that stops relaxing after finding the first cycle and never
/// notices -- or never even reaches, depending on iteration order -- a
/// second, separate one. Also confirms a well-behaved, correctly
/// stratifiable rule (Safe, unrelated to either cycle) coexisting in the
/// same ruleset doesn't mask the rejection.
#[test]
fn test_multiple_disjoint_cycles_rejected() {
    let rules = vec![
        // Cycle 1: A <-> B via negation.
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("A"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("B"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("B"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("A"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
        // Cycle 2: X <-> Y via negation -- entirely disjoint from A/B.
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("X"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Y"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("Y"),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("X"),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        },
        // An unrelated, perfectly safe rule that must not mask the rejection.
        Rule {
            head: Triple::from(
                "?x".to_string(),
                pred("Safe"),
                "http://example.org/true".to_string(),
            ),
            body: vec![BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    pred("Base"),
                    "http://example.org/true".to_string(),
                ),
            }],
        },
    ];

    let result = validate_rules(&rules, &HashMap::new());
    assert!(
        result.is_err(),
        "a ruleset with two independent unstratifiable cycles (A<->B and X<->Y) must be rejected even though it also contains a valid, safe rule"
    );
}
