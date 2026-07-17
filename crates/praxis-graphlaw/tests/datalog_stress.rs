//! Stress and counterfactual tests for the Datalog stratification/safety/
//! aggregation engine (`praxis_graphlaw::datalog`), going beyond the conformance
//! suite's small hand-picked cases to check behavior *at scale* and that
//! rejection paths don't silently start passing once inputs get large.

use praxis_graphlaw::datalog::validate_rules;
use praxis_graphlaw::encoding::Encoder;
use praxis_graphlaw::triples::{
    Aggregate, AggregateFunction, BodyLiteral, Rule, Triple, VarOrTerm,
};
use praxis_graphlaw::TripleStore;
use std::collections::HashMap;
use std::time::{Duration, Instant};

fn decode_all(triples: &[Triple]) -> Vec<String> {
    triples.iter().map(TripleStore::decode_triple).collect()
}

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

/// Stress: aggregation (COUNT grouped by department) over 1000 facts across
/// 50 groups must produce exactly the right per-group counts and complete
/// quickly. This is ~300x more facts than the conformance suite's
/// hand-picked 3-employee example.
#[test]
fn test_large_scale_grouped_aggregation() {
    const NUM_DEPTS: usize = 50;
    const EMPLOYEES_PER_DEPT: usize = 20; // 1000 facts total

    let mut store = TripleStore::new();
    for d in 0..NUM_DEPTS {
        for e in 0..EMPLOYEES_PER_DEPT {
            store.add(Triple::from(
                format!("http://example.org/dept{}", d),
                pred("hasEmployee"),
                format!("http://example.org/emp{}_{}", d, e),
            ));
        }
    }

    let rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("employeeCount"),
            "?count".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from("?d".to_string(), pred("hasEmployee"), "?e".to_string()),
        }],
    };
    let agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    let start = Instant::now();
    store
        .add_rule_with_aggregate(rule, agg)
        .expect("valid aggregate rule over 1000 facts must be accepted");
    let derived = store.materialize().unwrap();
    let elapsed = start.elapsed();

    let decoded: Vec<String> = derived.iter().map(TripleStore::decode_triple).collect();
    let count_facts: Vec<&String> = decoded
        .iter()
        .filter(|d| d.contains("employeeCount"))
        .collect();

    assert_eq!(
        count_facts.len(),
        NUM_DEPTS,
        "expected exactly {} employeeCount facts (one per department), got {}: {:?}",
        NUM_DEPTS,
        count_facts.len(),
        count_facts
    );
    for d in 0..NUM_DEPTS {
        assert!(
            count_facts
                .iter()
                .any(|f| f.contains(&format!("/dept{}>", d))
                    && f.contains(&EMPLOYEES_PER_DEPT.to_string())),
            "dept{} should have employeeCount={}, got: {:?}",
            d,
            EMPLOYEES_PER_DEPT,
            count_facts
        );
    }

    assert!(
        elapsed < Duration::from_secs(10),
        "1000-fact grouped aggregation took {:?}, expected well under 10s",
        elapsed
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

/// Composition: a rule negates on a predicate that is itself derived from a
/// value produced by an EARLIER-stratum aggregate (COUNT), rather than
/// negation-depth and aggregation being tested only in isolation as the
/// other stress tests above do. Shape:
///   employeeCount(d, c)   :- hasEmployee(d, e)        [COUNT aggregate, stratum 0]
///   HighStaff(d)          :- employeeCount(d, "5")     [ordinary join on the
///                                                        aggregate's own output, stratum 0/1]
///   LowStaffAlert(d)      :- isDept(d), not HighStaff(d) [negates on a
///                                                          rule that consumes
///                                                          the aggregate, stratum 2]
/// dept1 has 3 employees (not HighStaff -> alerted), dept2 has 5 (HighStaff
/// -> not alerted). This is the realistic real-world composition pattern
/// (aggregate -> derived boolean -> negated), which neither
/// `test_large_scale_grouped_aggregation` (no negation) nor the
/// stratification-chain tests above (no aggregation) actually exercise.
#[test]
fn test_stratified_negation_over_aggregate_derived_predicate() {
    let mut store = TripleStore::new();
    for e in 0..3 {
        store.add(Triple::from(
            "http://example.org/dept1".to_string(),
            pred("hasEmployee"),
            format!("http://example.org/emp1_{}", e),
        ));
    }
    for e in 0..5 {
        store.add(Triple::from(
            "http://example.org/dept2".to_string(),
            pred("hasEmployee"),
            format!("http://example.org/emp2_{}", e),
        ));
    }
    store.add(Triple::from(
        "http://example.org/dept1".to_string(),
        pred("isDept"),
        "http://example.org/true".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/dept2".to_string(),
        pred("isDept"),
        "http://example.org/true".to_string(),
    ));

    let count_rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("employeeCount"),
            "?count".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from("?d".to_string(), pred("hasEmployee"), "?e".to_string()),
        }],
    };
    let agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };
    store
        .add_rule_with_aggregate(count_rule, agg)
        .expect("aggregate rule must be accepted");

    // The COUNT aggregate encodes its numeric result via a raw,
    // bracket-less `Encoder::add(count.to_string())` (see aggregation.rs),
    // NOT through `Triple::from`'s normal convention of wrapping bare
    // strings in `<...>` before interning -- so matching it here requires
    // building the object term from the exact same raw-string encoding
    // rather than going through `Triple::from("...", "...", "5")`, which
    // would intern the differently-shaped string "<5>" and never match.
    let five_id = Encoder::add("5".to_string());
    let high_staff_rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("HighStaff"),
            "http://example.org/true".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple {
                s: VarOrTerm::convert("?d".to_string()),
                p: VarOrTerm::convert(pred("employeeCount")),
                o: VarOrTerm::new_encoded_term(five_id),
                g: None,
            },
        }],
    };
    let low_staff_alert_rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("LowStaffAlert"),
            "http://example.org/true".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    pred("isDept"),
                    "http://example.org/true".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?d".to_string(),
                    pred("HighStaff"),
                    "http://example.org/true".to_string(),
                ),
            },
        ],
    };
    store
        .add_rules(vec![high_staff_rule, low_staff_alert_rule])
        .expect(
            "negation over an aggregate-derived predicate must be accepted as safely stratifiable",
        );

    let derived = store.materialize().unwrap();
    let decoded = decode_all(&derived);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/dept2") && d.contains("HighStaff")),
        "dept2 (5 employees) must be classified HighStaff, got: {:?}",
        decoded
    );
    assert!(
        !decoded
            .iter()
            .any(|d| d.contains("/dept1") && d.contains("HighStaff")),
        "dept1 (3 employees) must NOT be classified HighStaff, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/dept1") && d.contains("LowStaffAlert")),
        "dept1 (not HighStaff) must be flagged LowStaffAlert, got: {:?}",
        decoded
    );
    assert!(
        !decoded
            .iter()
            .any(|d| d.contains("/dept2") && d.contains("LowStaffAlert")),
        "dept2 (HighStaff) must NOT be flagged LowStaffAlert, got: {:?}",
        decoded
    );
}
