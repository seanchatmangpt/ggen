//! Regression tests for builtins added to close real, catalogued EYE-corpus
//! gaps (see `lib/tests/n3_conformance/SKIPPED.md`): `math:notEqualTo`
//! (`kaprekar`/`goldbach`/`gcd-bezout-identity`), `list:notMember` and
//! `list:firstRest` (`path-discovery`'s graph-search algorithm).

use praxis_graphlaw::TripleStore;

fn goal_from(src: &str) -> praxis_graphlaw::triples::Triple {
    TripleStore::from(src)
        .triple_index
        .triples
        .first()
        .unwrap()
        .clone()
}

#[test]
fn math_not_equal_to() {
    let n3 = r#"
@prefix : <http://example.org/#>.
@prefix math: <http://www.w3.org/2000/10/swap/math#>.
{ :a :notEqual "true". } <= { 3 math:notEqualTo 5. }.
{ :b :notEqual "true". } <= { 3 math:notEqualTo 3. }.
"#;
    let store = TripleStore::from(n3);
    assert!(
        store.prove(&goal_from(
            "@prefix : <http://example.org/#>.\n:a :notEqual \"true\"."
        )),
        "3 != 5 should be provable"
    );
    assert!(
        !store.prove(&goal_from(
            "@prefix : <http://example.org/#>.\n:b :notEqual \"true\"."
        )),
        "3 != 3 should NOT be provable"
    );
}

#[test]
fn list_not_member() {
    let n3 = r#"
@prefix : <http://example.org/#>.
@prefix list: <http://www.w3.org/2000/10/swap/list#>.
{ :c :notMember "true". } <= { (1 2 3) list:notMember 9. }.
{ :d :notMember "true". } <= { (1 2 3) list:notMember 2. }.
"#;
    let store = TripleStore::from(n3);
    assert!(
        store.prove(&goal_from(
            "@prefix : <http://example.org/#>.\n:c :notMember \"true\"."
        )),
        "9 is not in (1 2 3), should be provable"
    );
    assert!(
        !store.prove(&goal_from(
            "@prefix : <http://example.org/#>.\n:d :notMember \"true\"."
        )),
        "2 IS in (1 2 3), notMember should be false"
    );
}

#[test]
fn list_first_rest_cons() {
    let n3 = r#"
@prefix : <http://example.org/#>.
@prefix list: <http://www.w3.org/2000/10/swap/list#>.
{ ?NewList list:firstRest (:a (:b :c)). } <= true.
"#;
    let store = TripleStore::from(n3);
    let expected_list_store = TripleStore::from(
        "@prefix : <http://example.org/#>.\n{?Dummy :dummy (:a :b :c).} <= true.",
    );
    let expected_list = expected_list_store.rules_index.rules[0].head.o.clone();

    let goal = store.rules_index.rules[0].head.clone();
    let rows = store.solve(&goal);
    assert_eq!(
        rows.len(),
        1,
        "expected exactly one solution, got {:?}",
        rows
    );
    let bound = rows[0].get(&goal.s.to_encoded()).expect("?NewList bound");
    assert_eq!(
        bound[0],
        expected_list.to_encoded(),
        "expected (:a :b :c) cons result"
    );
}
