//! Regression for a real grammar gap: `n3.pest`'s `Subject` production had no
//! `Literal` alternative (`Object` did), so any bare literal used as a
//! subject -- e.g. a numeric base-case fact, or the real EYE `ackermann`/
//! `ill-formed-literals`/`proof-by-induction` idiom `true log:callWithCut
//! true.` -- was a hard parse error (`expected TP`), not a semantic gap. Now
//! fixed by adding `Literal` to `Subject`; `term_from_pair`/`parse_subject`
//! already handled `Rule::Literal` generically (used by `Object`), so no
//! parser-logic change was needed beyond the grammar itself.

use praxis_graphlaw::TripleStore;

#[test]
fn bare_literal_as_subject_parses_and_is_stored() {
    let n3 = r#"
@prefix : <http://example.org/#>.
@prefix math: <http://www.w3.org/2000/10/swap/math#>.
5 math:notEqualTo 3.
"#;
    let store = TripleStore::from(n3);
    assert_eq!(store.rules.len(), 0, "this is a fact, not a rule");
    assert_eq!(
        store.triple_index.len(),
        1,
        "the literal-subject triple should be stored"
    );
}

#[test]
fn literal_subject_inside_a_rule_body_parses() {
    // Mirrors the real EYE `ackermann.n3` cut-guard idiom: a bare `true`
    // literal used as the subject of an ordinary triple pattern inside a
    // rule's antecedent.
    let n3 = r#"
@prefix : <http://example.org/#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
{ :a :cutGuard "fired". } <= { true log:notEqualTo false. }.
"#;
    let store = TripleStore::from(n3);
    assert_eq!(store.rules.len(), 1);
}
