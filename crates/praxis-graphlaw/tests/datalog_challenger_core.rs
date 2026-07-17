use praxis_graphlaw::triples::{Aggregate, AggregateFunction, BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

/// Test empty relations in negation.
#[test]
fn test_empty_relations_negation() {
    let mut store = TripleStore::new();

    // Fact: :a is a Parent. But there are no hasChild relations at all.
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/Parent".to_string(),
    ));

    // Rule: if ?x is a Parent and ?x has no child ?y, ?x is Childless.
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

    let res = store.add_rules(vec![rule]);
    assert!(res.is_ok());

    let derived = store.materialize().unwrap();
    let childless: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("Childless")
        })
        .map(TripleStore::decode_triple)
        .collect();

    assert_eq!(childless.len(), 1);
    assert!(childless[0].contains("http://example.org/a"));
}

/// Test empty relations in aggregations.
#[test]
fn test_empty_relations_aggregation() {
    let mut store = TripleStore::new();

    // No facts at all in the store.
    let rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/employeeCount".to_string(),
            "?count".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?d".to_string(),
                "http://example.org/hasEmployee".to_string(),
                "?e".to_string(),
            ),
        }],
    };

    let agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    let res = store.add_rule_with_aggregate(rule, agg);
    assert!(res.is_ok());

    let derived = store.materialize().unwrap();
    assert_eq!(derived.len(), 0);
}

/// Test unbound aggregate source variable.
#[test]
fn test_unbound_aggregate_source_var() {
    let mut store = TripleStore::new();

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e1".to_string(),
    ));

    // Rule where ?unbound is NOT bound in the body.
    let rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/employeeCount".to_string(),
            "?count".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?d".to_string(),
                "http://example.org/hasEmployee".to_string(),
                "?e".to_string(),
            ),
        }],
    };

    let agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?unbound".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    let res = store.add_rule_with_aggregate(rule, agg);
    if res.is_ok() {
        let derived = store.materialize().unwrap();
        // Since ?unbound is not in the bindings, it shouldn't produce any count fact.
        assert_eq!(derived.len(), 0);
    }
}
