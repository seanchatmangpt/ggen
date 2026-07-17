use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

/// CONFORM-005: Stratified Negation - Negation across a single stratum boundary.
/// Evaluated in strata order.
///
/// Rules:
/// Stratum 0:
/// 1) { ?x <http://example.org/type> <http://example.org/A> } => { ?x <http://example.org/type> <http://example.org/B> }
///
/// Stratum 1:
/// 2) { ?x <http://example.org/type> <http://example.org/B> . not { ?x <http://example.org/type> <http://example.org/C> } } => { ?x <http://example.org/type> <http://example.org/D> }
///
/// Facts:
/// :a <http://example.org/type> <http://example.org/A> .
/// :b <http://example.org/type> <http://example.org/A> .
/// :b <http://example.org/type> <http://example.org/C> .
///
/// Expected outcome:
/// - :a is derived as B and D.
/// - :b is derived as B and C, but NOT D.
#[test]
fn test_negation_across_stratum_boundary() {
    let mut store = TripleStore::new();

    // Facts
    store.add(Triple::from(
        "http://example.org/a".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/A".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/b".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/A".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/b".to_string(),
        "http://example.org/type".to_string(),
        "http://example.org/C".to_string(),
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

    // Rule 2: B and not C => D
    let r2 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/type".to_string(),
            "http://example.org/D".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/B".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/type".to_string(),
                    "http://example.org/C".to_string(),
                ),
            },
        ],
    };

    store.add_rules(vec![r1, r2]).expect("rules must load");

    let derived = store.materialize().unwrap();

    let mut a_has_d = false;
    let mut b_has_d = false;

    for t in derived.iter() {
        let s = TripleStore::decode_triple(t);
        if s.contains("D") {
            if s.contains("http://example.org/a") {
                a_has_d = true;
            }
            if s.contains("http://example.org/b") {
                b_has_d = true;
            }
        }
    }

    assert!(a_has_d, "a should be derived as D");
    assert!(!b_has_d, "b should not be derived as D because it has C");
}
