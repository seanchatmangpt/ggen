use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

/// CONFORM-001: Rule Safety - Positive binding rule (safe) is accepted.
#[test]
fn test_safe_rule_accepted() {
    let mut store = TripleStore::new();

    // { ?x <http://example.org/type> <http://example.org/Parent> } => { ?x <http://example.org/type> <http://example.org/HasBeenParent> }
    let safe_rule = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/HasBeenParent".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/type".to_string(),
                "http://example.org/Parent".to_string(),
            ),
        }],
    };

    // The safety checker should accept this rule
    let res = store.add_rules(vec![safe_rule]);
    assert!(res.is_ok());
}

/// CONFORM-002: Rule Safety - Unbound variable in the head is rejected.
#[test]
fn test_unsafe_unbound_head_var_rejected() {
    let mut store = TripleStore::new();

    // { ?x <http://example.org/type> <http://example.org/Parent> } => { ?y <http://example.org/type> <http://example.org/Orphan> }
    // Note that ?y does not appear in any positive literal in the body.
    let unsafe_rule = Rule {
        head: Triple::from(
            "?y".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/Orphan".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/type".to_string(),
                "http://example.org/Parent".to_string(),
            ),
        }],
    };

    // Rule safety check must reject this.
    let res = store.add_rules(vec![unsafe_rule]);
    assert!(res.is_err());
}

/// CONFORM-003: Rule Safety - Unbound variable in a negated literal is rejected.
#[test]
fn test_unsafe_unbound_negated_var_rejected() {
    let mut store = TripleStore::new();

    // { ?x <http://example.org/type> <http://example.org/Parent> . not { ?y <http://example.org/hasChild> ?z } } => { ?x <http://example.org/type> <http://example.org/Childless> }
    // Here, ?y and ?z are in a negated literal and do not appear in any positive body literal.
    let unsafe_rule = Rule {
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
                    "?y".to_string(),
                    "http://example.org/hasChild".to_string(),
                    "?z".to_string(),
                ),
            },
        ],
    };

    // Rule safety check must reject this.
    let res = store.add_rules(vec![unsafe_rule]);
    assert!(res.is_err());
}
