use praxis_graphlaw::TripleStore;
use std::panic;

#[test]
fn exotic_builtin_predicate_panics_instead_of_silently_failing() {
    let n3 = r#"
@prefix : <http://example.org/#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
{ :a :cutGuardFired "true". } <= { true log:callWithCut true. }.
"#;
    let store = TripleStore::from(n3);
    let goal_src = "@prefix : <http://example.org/#>.\n:a :cutGuardFired \"true\".";
    let goal = TripleStore::from(goal_src)
        .triple_index
        .triples
        .first()
        .unwrap()
        .clone();

    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| store.prove(&goal)));
    assert!(
        result.is_err(),
        "prove() over an unregistered log:callWithCut should panic, not silently return false"
    );
}

#[test]
fn ordinary_unmatched_predicate_does_not_panic() {
    let n3 = r#"
@prefix : <http://example.org/#>.
{ :a :derived "true". } <= { :x :hasOwner :y. }.
"#;
    let store = TripleStore::from(n3);
    let goal_src = "@prefix : <http://example.org/#>.\n:a :derived \"true\".";
    let goal = TripleStore::from(goal_src)
        .triple_index
        .triples
        .first()
        .unwrap()
        .clone();

    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| store.prove(&goal)));
    assert!(
        result.is_ok(),
        "an ordinary unmatched user predicate must NOT panic"
    );
    assert!(
        !result.unwrap(),
        "the rule body genuinely doesn't hold, so prove() should just return false"
    );
}
