//! Catalog items #21-22 (docs/jira/26.7.4 impossible/illogical constructs plan):
//! Datalog rules that are structurally impossible to safely/consistently
//! evaluate. These are deterministic (the impossibility is a property of
//! rule *structure*, not of any particular data value), run directly against
//! the real `praxis_graphlaw::datalog::validate_rules` engine.

use praxis_graphlaw::datalog::validate_rules;
use praxis_graphlaw::triples::{BodyLiteral, Rule, Triple};
use std::collections::HashMap;

fn pred(name: &str) -> String {
    format!("http://example.org/{}", name)
}

/// Item #21: classic unsafe negation. `?x` in the head IS bound by a
/// positive body literal (so the head-safety check passes), but the negated
/// literal's variable `?y` has NO positive occurrence anywhere in the same
/// rule body -- there is no way to enumerate which `?y` to check "not" for,
/// making the rule unevaluable. Must be rejected, not silently accepted
/// (e.g. by treating the unbound negation as vacuously true/false).
#[test]
fn test_unsafe_negation_wholly_unbound_variable_rejected() {
    let rule = Rule {
        head: Triple::from(
            "?x".to_string(),
            pred("Alert"),
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
                    "?y".to_string(),
                    pred("Unbound"),
                    "http://example.org/true".to_string(),
                ),
            },
        ],
    };

    let result = validate_rules(std::slice::from_ref(&rule), &HashMap::new());
    assert!(
        result.is_err(),
        "a negated literal whose variable (?y) has no positive occurrence anywhere in the rule body must be rejected as unsafe"
    );
    let err = result.unwrap_err();
    assert!(
        err.contains("unsafe"),
        "expected the error to explain the unsafe-negation reason, got: {}",
        err
    );
}

/// Item #22: the shortest possible self-contradictory stratification case --
/// a single rule whose body negates its OWN head predicate: `P(x) :- Base(x),
/// not P(x).` This is safe (per the safety check: ?x IS shared between the
/// positive Base(x) and the negated P(x) literal), but it is a 1-cycle
/// through negation -- the minimal Russell's-paradox-shaped unstratifiable
/// program (P is true iff P is false). Must be rejected by stratification,
/// not accepted as if the cycle didn't exist.
#[test]
fn test_shortest_self_negation_cycle_rejected() {
    let rule = Rule {
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
                    pred("P"),
                    "http://example.org/true".to_string(),
                ),
            },
        ],
    };

    let result = validate_rules(std::slice::from_ref(&rule), &HashMap::new());
    assert!(
        result.is_err(),
        "P(x) :- Base(x), not P(x) is a direct self-negation cycle (the minimal unstratifiable program) and must be rejected"
    );
}
