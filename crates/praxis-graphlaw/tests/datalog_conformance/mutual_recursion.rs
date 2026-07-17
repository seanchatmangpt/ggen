use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

/// CONFORM-004: Mutual Recursion - Two predicates each depending on the other.
/// The fixpoint evaluation must terminate correctly.
///
/// Rules:
/// 1) { ?x <http://example.org/type> <http://example.org/A> } => { ?x <http://example.org/type> <http://example.org/B> }
/// 2) { ?x <http://example.org/type> <http://example.org/B> } => { ?x <http://example.org/type> <http://example.org/A> }
///
/// Facts:
/// :a <http://example.org/type> <http://example.org/A> .
///
/// Expected outcome:
/// - Both :a :type :A and :a :type :B are in the store.
/// - Fixpoint iteration terminates.
#[test]
fn test_mutual_recursion_fixpoint() {
    let mut store = TripleStore::new();

    // Facts
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/A".to_string(),
    ));

    // Rule 1: A => B
    let r1 = Rule {
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
                "http://example.org/A".to_string(),
            ),
        }],
    };

    // Rule 2: B => A
    let r2 = Rule {
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

    store.add_rules(vec![r1, r2]).expect("rules must load");

    // Evaluation
    let derived = store.materialize().unwrap();

    // Verify
    let mut has_b = false;
    for t in derived.iter() {
        let s = TripleStore::decode_triple(t);
        if s.contains("http://example.org/a") && s.contains("type") && s.contains("B") {
            has_b = true;
        }
    }

    assert!(has_b, "Should contain B");
    assert_eq!(2, store.len(), "Store should contain both A and B");
}
