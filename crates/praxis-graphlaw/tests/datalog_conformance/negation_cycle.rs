use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

/// CONFORM-006: Stratified Negation - Negation through a cycle is rejected.
/// The ruleset is unstratifiable (A depends negatively on itself via B, or vice-versa).
///
/// Rules:
/// 1) { ?x <http://example.org/type> <http://example.org/A> . not { ?x <http://example.org/type> <http://example.org/B> } } => { ?x <http://example.org/type> <http://example.org/B> }
/// 2) { ?x <http://example.org/type> <http://example.org/B> } => { ?x <http://example.org/type> <http://example.org/A> }
///
/// Expected outcome:
/// - Rejected at load-time (should return an error).
#[test]
fn test_negation_cycle_rejected() {
    let mut store = TripleStore::new();

    let r1 = Rule {
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

    // The load_rules or add_rules API should detect the cycle and return Err.
    let res = store.add_rules(vec![r1, r2]);
    assert!(res.is_err());
}
