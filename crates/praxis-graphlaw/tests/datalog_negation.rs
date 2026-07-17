use praxis_graphlaw::datalog::validate_rules;
use praxis_graphlaw::triples::{Aggregate, BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;
use std::collections::HashMap;

/// TICKET-004: Test basic stratified negation.
/// Negation-as-failure should be correctly evaluated across stratum boundaries.
///
/// Rules under test:
/// { ?x <http://example.org/type> <http://example.org/Parent> . not { ?x <http://example.org/hasChild> ?y } } => { ?x <http://example.org/type> <http://example.org/Childless> }
///
/// Facts:
/// :a <http://example.org/type> <http://example.org/Parent> .
/// :b <http://example.org/type> <http://example.org/Parent> .
/// :a <http://example.org/hasChild> :b .
///
/// Expected outcome:
/// :b is derived as Childless, but :a is not.
#[test]
fn test_stratified_negation_basic() {
    let mut store = TripleStore::new();

    // Add facts
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Parent".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/b".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Parent".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/hasChild".to_string(),
        "http://example.org/b".to_string(),
    ));

    // Define rule with negated body literal
    let rule = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/Childless".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/Parent".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/hasChild".to_string(),
                    "?y".to_string(),
                ),
            },
        ],
    };

    store.add_rules(vec![rule]).expect("rules must load");

    // Materialize derivations
    let derived = store.materialize().unwrap();

    // Verify results
    let childless_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let decoded_p = TripleStore::decode_triple(t);
            decoded_p.contains("Childless")
        })
        .map(TripleStore::decode_triple)
        .collect();

    // Expecting exactly one Childless derivation: :b
    assert_eq!(
        childless_triples.len(),
        1,
        "There should be exactly one childless parent derived"
    );
    assert!(
        childless_triples[0].contains("http://example.org/b"),
        "http://example.org/b should be Childless"
    );
    assert!(
        !childless_triples[0].contains("http://example.org/a"),
        "http://example.org/a has a child, so it should not be Childless"
    );
}

/// TICKET-004: Test that unstratifiable rulesets are rejected rather than leading to hang or incorrect results.
///
/// Rules under test (self-negation cycle):
/// { ?x <http://example.org/type> <http://example.org/A> . not { ?x <http://example.org/type> <http://example.org/B> } } => { ?x <http://example.org/type> <http://example.org/B> }
#[test]
fn test_unstratifiable_rules_rejected() {
    let mut store = TripleStore::new();

    let rule = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/B".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/A".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/B".to_string(),
                ),
            },
        ],
    };

    let res = store.add_rules(vec![rule]);
    assert!(res.is_err());
}

/// TICKET-004: Test that the rule safety check rejects rules with unbound variables in negated literals.
///
/// Rules under test (unsafe):
/// { not { ?x <http://example.org/type> <http://example.org/A> } } => { ?x <http://example.org/type> <http://example.org/B> }
#[test]
fn test_rule_safety_check_rejects_unbound_negated_var() {
    let mut store = TripleStore::new();

    let unsafe_rule = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/B".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: true,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/type".to_string(),
                "http://example.org/A".to_string(),
            ),
        }],
    };

    // Rule safety check must reject this because ?x is only in a negated body literal.
    let res = store.add_rules(vec![unsafe_rule]);
    assert!(res.is_err());
}

/// TICKET-004: Test that evaluation terminates when recursive rulesets are processed.
///
/// Rules under test:
/// { ?x <http://example.org/type> <http://example.org/Node> } => { ?x <http://example.org/reachable> ?x }
/// { ?x <http://example.org/edge> ?y . ?y <http://example.org/reachable> ?z } => { ?x <http://example.org/reachable> ?z }
/// { ?x <http://example.org/type> <http://example.org/Node> . not { ?x <http://example.org/reachable> <http://example.org/c> } } => { ?x <http://example.org/type> <http://example.org/Disconnected> }
///
/// Facts:
/// :a <http://example.org/type> <http://example.org/Node> .
/// :b <http://example.org/type> <http://example.org/Node> .
/// :c <http://example.org/type> <http://example.org/Node> .
/// :a <http://example.org/edge> :b .
/// :b <http://example.org/edge> :c .
///
/// Expected outcome:
/// - Reachability computes: :a -> :a, :b -> :b, :c -> :c, :a -> :b, :b -> :c, :a -> :c
/// - Disconnectedness computes: Only :c is disconnected from :c? No, :c reaches :c, so nobody is disconnected from :c except if we have another node :d.
///   Let's add :d <http://example.org/type> <http://example.org/Node> (isolated).
/// - :d should be derived as Disconnected from :c.
#[test]
fn test_fixpoint_terminates_on_recursive_ruleset() {
    let mut store = TripleStore::new();

    // Facts
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Node".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/b".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Node".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/c".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Node".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/d".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Node".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/edge".to_string(),
        "http://example.org/b".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/b".to_string(),
        "http://example.org/edge".to_string(),
        "http://example.org/c".to_string(),
    ));

    // Rules
    let r1 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/reachable".to_string(),
            "?x".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/type".to_string(),
                "http://example.org/Node".to_string(),
            ),
        }],
    };

    let r2 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/reachable".to_string(),
            "?z".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/edge".to_string(),
                    "?y".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?y".to_string(),
                    "http://example.org/reachable".to_string(),
                    "?z".to_string(),
                ),
            },
        ],
    };

    let r3 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/Disconnected".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/Node".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/reachable".to_string(),
                    "http://example.org/c".to_string(),
                ),
            },
        ],
    };

    store.add_rules(vec![r1, r2, r3]).expect("rules must load");

    let derived = store.materialize().unwrap();

    let disconnected: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("Disconnected")
        })
        .map(TripleStore::decode_triple)
        .collect();

    // Verify that ONLY :d is disconnected from :c
    assert_eq!(
        disconnected.len(),
        1,
        "There should be exactly one disconnected node"
    );
    assert!(
        disconnected[0].contains("http://example.org/d"),
        "Node d should be disconnected"
    );
}

#[test]
fn test_negation_empty_relations() {
    let mut store = TripleStore::new();
    let rule = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/C".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/A".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/B".to_string(),
                ),
            },
        ],
    };
    assert!(store.add_rules(vec![rule]).is_ok());
    let derived = store.materialize().unwrap();
    assert!(
        derived.is_empty(),
        "Derived set should be empty on empty relations"
    );
}

#[test]
fn test_negation_unbound_vars_rejected() {
    let mut store = TripleStore::new();
    let rule = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/C".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/A".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?y".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/B".to_string(),
                ),
            },
        ],
    };
    // ?y is unbound by any positive body literal, should be rejected
    assert!(store.add_rules(vec![rule]).is_err());
}

#[test]
fn test_empty_body_rule() {
    let mut store = TripleStore::new();
    let rule = Rule {
        head: Triple::from(
            "http://example.org/a".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/C".to_string(),
        ),
        body: vec![],
    };
    assert!(store.add_rules(vec![rule]).is_ok());
    let derived = store.materialize().unwrap();
    assert_eq!(derived.len(), 1);
    let s = TripleStore::decode_triple(&derived[0]);
    assert!(s.contains("http://example.org/a") && s.contains("C"));
}

#[test]
fn test_long_unstratifiable_cycle_rejected() {
    let mut store = TripleStore::new();
    // Rule 1: A and not B => C
    let r1 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/C".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/A".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/B".to_string(),
                ),
            },
        ],
    };
    // Rule 2: C => B
    let r2 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/B".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/type".to_string(),
                "http://example.org/C".to_string(),
            ),
        }],
    };
    // Rule 3: B => A
    let r3 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/A".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/type".to_string(),
                "http://example.org/B".to_string(),
            ),
        }],
    };
    // This forms a cycle B -> A -> C -> B, where A -> C is negative.
    // So C depends negatively on B, and B depends positively on C.
    // Thus it is unstratifiable.
    assert!(store.add_rules(vec![r1, r2, r3]).is_err());
}

/// TICKET-004: Test a 3+ layer stratification chain.
///
/// Predicate :C depends via negation on :B, which itself depends via negation
/// on :A, which depends positively on the EDB predicate :hasFoo. This forms a
/// strict chain of three strata (not a cycle), so `validate_rules` must
/// accept it and assign each rule's head predicate a distinct, strictly
/// increasing stratum: stratum(A) < stratum(B) < stratum(C).
///
/// Rules under test:
/// { ?x :hasFoo ?y } => { ?x rdf:type :A }
/// { ?x rdf:type :Base . not { ?x rdf:type :A } } => { ?x rdf:type :B }
/// { ?x rdf:type :Base . not { ?x rdf:type :B } } => { ?x rdf:type :C }
///
/// Facts:
/// :a rdf:type :Base . :b rdf:type :Base . :a :hasFoo :x1 .
/// (only :a has :hasFoo, so only :a gets :A)
///
/// Expected derivations:
/// :A = { :a }                      (from :hasFoo)
/// :B = { :b }                      (:b has Base but not A)
/// :C = { :a }                      (:a has Base but not B; :b has B so is excluded)
#[test]
fn test_three_layer_stratification_chain() {
    let mut store = TripleStore::new();

    // Facts
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Base".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/b".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Base".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/hasFoo".to_string(),
        "http://example.org/x1".to_string(),
    ));

    // Rule 1 (defines :A): { ?x :hasFoo ?y } => { ?x rdf:type :A }
    let r1 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/A".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/hasFoo".to_string(),
                "?y".to_string(),
            ),
        }],
    };

    // Rule 2 (defines :B, negates :A): { ?x rdf:type :Base . not { ?x rdf:type :A } } => { ?x rdf:type :B }
    let r2 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/B".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/Base".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/A".to_string(),
                ),
            },
        ],
    };

    // Rule 3 (defines :C, negates :B): { ?x rdf:type :Base . not { ?x rdf:type :B } } => { ?x rdf:type :C }
    let r3 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/C".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/Base".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/B".to_string(),
                ),
            },
        ],
    };

    let rules = vec![r1.clone(), r2.clone(), r3.clone()];
    let aggregates: HashMap<Rule, Aggregate> = HashMap::new();

    // Directly exercise validate_rules to confirm correct, distinct, strictly
    // increasing stratification across all three layers.
    let strata = validate_rules(&rules, &aggregates)
        .expect("a strict negation chain A -> B -> C (no cycle) must be stratifiable");

    assert_eq!(strata.len(), 3);
    let (stratum_a, stratum_b, stratum_c) = (strata[0], strata[1], strata[2]);
    assert!(
        stratum_a < stratum_b,
        "stratum(A)={} must be strictly less than stratum(B)={} (negation edge A -> B)",
        stratum_a,
        stratum_b
    );
    assert!(
        stratum_b < stratum_c,
        "stratum(B)={} must be strictly less than stratum(C)={} (negation edge B -> C)",
        stratum_b,
        stratum_c
    );

    // Also verify materialization derives the expected facts per layer.
    let res = store.add_rules(rules);
    assert!(res.is_ok(), "the rule set must be accepted end-to-end");

    let derived = store.materialize().unwrap();
    let decoded: Vec<String> = derived.iter().map(TripleStore::decode_triple).collect();

    let a_facts: Vec<&String> = decoded
        .iter()
        .filter(|s| s.contains("http://example.org/A"))
        .collect();
    let b_facts: Vec<&String> = decoded
        .iter()
        .filter(|s| s.contains("http://example.org/B"))
        .collect();
    let c_facts: Vec<&String> = decoded
        .iter()
        .filter(|s| s.contains("http://example.org/C"))
        .collect();

    assert_eq!(
        a_facts.len(),
        1,
        "only :a should get :A, got: {:?}",
        a_facts
    );
    assert!(a_facts[0].contains("http://example.org/a"));

    assert_eq!(
        b_facts.len(),
        1,
        "only :b should get :B, got: {:?}",
        b_facts
    );
    assert!(b_facts[0].contains("http://example.org/b"));

    assert_eq!(
        c_facts.len(),
        1,
        "only :a should get :C, got: {:?}",
        c_facts
    );
    assert!(c_facts[0].contains("http://example.org/a"));
}

/// Validation cross-check against https://github.com/fogfish/datalog (an Erlang
/// Datalog query engine). Its hardest documented semantics -- `union_2`/`union_3`
/// (multiple distinct rules deriving the *same* head predicate act as a logical
/// OR over their derivations) and `recursion_1`/`recursion_2`/`recursion_3`
/// (transitive-closure fixpoint over a recursive rule referencing its own head)
/// -- are both already exercised together, incidentally, inside
/// `test_fixpoint_terminates_on_recursive_ruleset` above (its r1/r2 pair is a
/// base-case + recursive-case rule pair sharing the single `reachable` head).
/// This test isolates that same union+recursion combination as its own
/// independently-named, minimal case, so it's auditable on its own rather than
/// only incidentally covered inside a larger scenario.
///
/// Note: fogfish/datalog does not implement or test stratified negation,
/// negation-cycle rejection, or rule safety checking at all -- those are
/// roxi-only hard cases (see `test_unstratifiable_rules_rejected`,
/// `test_rule_safety_check_rejects_unbound_negated_var`,
/// `test_three_layer_stratification_chain`) with no fogfish counterpart to
/// benchmark against. fogfish's guard-predicate (`x>2`), native cross-module
/// join, and n-ary (beyond-triple) relation features are architecturally
/// inapplicable to roxi's triple-based `BodyLiteral`/`Triple` model and are
/// not portable.
#[test]
fn test_union_semantics_multiple_rules_same_head() {
    let mut store = TripleStore::new();

    // Facts: a chain graph a -> b -> c, plus an isolated node d.
    for (s, o) in [("a", "b"), ("b", "c")] {
        store.add(Triple::from(
            format!("http://example.org/{s}"),
            "http://example.org/edge".to_string(),
            format!("http://example.org/{o}"),
        ));
    }

    // Two distinct rules, DIFFERENT bodies, but the SAME head predicate
    // (`reaches`) -- this is exactly fogfish's `union_2` pattern: the head
    // relation is the union of whatever each rule independently derives.
    //
    //   reaches(x,y) :- edge(x,y).                       // base case (r1)
    //   reaches(x,y) :- edge(x,z), reaches(z,y).          // recursive case (r2)
    let r1 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/reaches".to_string(),
            "?y".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/edge".to_string(),
                "?y".to_string(),
            ),
        }],
    };
    let r2 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/reaches".to_string(),
            "?y".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/edge".to_string(),
                    "?z".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?z".to_string(),
                    "http://example.org/reaches".to_string(),
                    "?y".to_string(),
                ),
            },
        ],
    };

    let res = store.add_rules(vec![r1, r2]);
    assert!(
        res.is_ok(),
        "union-by-shared-head rule pair must be accepted"
    );

    let derived = store.materialize().unwrap();
    let decoded: Vec<String> = derived.iter().map(TripleStore::decode_triple).collect();
    let reaches: Vec<&String> = decoded
        .iter()
        .filter(|s| s.contains("http://example.org/reaches"))
        .collect();

    // Transitive closure of a->b->c is exactly {a-b, b-c, a-c} -- 3 pairs,
    // with a-c ONLY derivable via r2 (recursion), proving both rules'
    // contributions are unioned into one relation, and that the recursive
    // rule actually reaches its fixpoint (doesn't stop after one iteration).
    assert_eq!(
        reaches.len(),
        3,
        "expected exactly 3 reaches pairs (a-b, b-c, a-c), got: {:?}",
        reaches
    );
    assert!(
        decoded.iter().any(|s| s.contains("http://example.org/a")
            && s.contains("http://example.org/reaches")
            && s.contains("http://example.org/c")),
        "a-c must be derived transitively via the recursive rule r2, not just directly by r1"
    );
}
