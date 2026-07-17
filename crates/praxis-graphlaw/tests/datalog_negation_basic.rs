use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

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
