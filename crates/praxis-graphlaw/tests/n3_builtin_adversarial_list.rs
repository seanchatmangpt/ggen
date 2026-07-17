//! Adversarial edge-case testing for list builtins (empty list, single-element
//! list, deeply nested lists, out-of-range memberAt).

use praxis_graphlaw::TripleStore;
use proptest::prelude::*;

fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples
        .iter()
        .map(praxis_graphlaw::TripleStore::decode_triple)
        .collect()
}

fn materialize(data: &str) -> Vec<String> {
    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    decode_all(&inferred)
}

// ===========================================================================
// list: empty list, single-element, deeply nested, out-of-range memberAt.
// ===========================================================================

#[test]
fn list_length_of_empty_list_is_zero() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( ) .\n\
                { ?s :l ?l . ?l list:length ?n } => { ?s :result ?n }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains("\"0\"")),
        "empty list must have length 0, got: {:?}",
        decoded
    );
}

#[test]
fn list_first_and_last_of_empty_list_do_not_derive() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( ) .\n\
                { ?s :l ?l . ?l list:first ?x } => { ?s :firstResult ?x }.\n\
                { ?s :l ?l . ?l list:last ?x } => { ?s :lastResult ?x }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded
            .iter()
            .any(|d| d.contains("/firstResult") || d.contains("/lastResult")),
        "list:first/list:last on an empty list must not derive anything, got: {:?}",
        decoded
    );
}

#[test]
fn list_first_and_last_of_single_element_list_are_the_same_element() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( 42 ) .\n\
                { ?s :l ?l . ?l list:first ?x } => { ?s :firstResult ?x }.\n\
                { ?s :l ?l . ?l list:last ?x } => { ?s :lastResult ?x }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/firstResult") && d.contains("42")),
        "single-element list: list:first must be 42, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/lastResult") && d.contains("42")),
        "single-element list: list:last must be 42, got: {:?}",
        decoded
    );
}

#[test]
fn list_rest_of_single_element_list_is_empty() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( 42 ) .\n\
                { ?s :l ?l . ?l list:rest ?r . ?r list:length ?n } => { ?s :result ?n }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains("\"0\"")),
        "list:rest of a single-element list must be an empty list (length 0), got: {:?}",
        decoded
    );
}

// Deeply nested lists (3+ levels): oracle is the true structural member
// count at each level, cross-checked with list:length and list:first
// unwrapping one level at a time.
proptest! {
    #[test]
    fn prop_list_nested_three_levels_length_and_first(
        a in 0i64..50, b in 0i64..50, c in 0i64..50,
    ) {
        // :l = ( ( ( a b ) c ) ) -- 3 levels of nesting, 1 outer member.
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( ( ( {a} {b} ) {c} ) ) .\n\
             {{ ?s :l ?l . ?l list:length ?n }} => {{ ?s :outerLen ?n }}.\n\
             {{ ?s :l ?l . ?l list:first ?f . ?f list:length ?n2 }} => {{ ?s :midLen ?n2 }}.\n\
             {{ ?s :l ?l . ?l list:first ?f . ?f list:first ?f2 . ?f2 list:length ?n3 }} => {{ ?s :innerLen ?n3 }}.\n\
             {{ ?s :l ?l . ?l list:first ?f . ?f list:first ?f2 . ?f2 list:first ?ff }} => {{ ?s :innermost ?ff }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(decoded.iter().any(|d| d.contains("/outerLen") && d.contains("\"1\"")),
            "outer list has 1 member, got: {:?}", decoded);
        prop_assert!(decoded.iter().any(|d| d.contains("/midLen") && d.contains("\"2\"")),
            "middle list ((a b) c) has 2 members, got: {:?}", decoded);
        prop_assert!(decoded.iter().any(|d| d.contains("/innerLen") && d.contains("\"2\"")),
            "innermost list (a b) has 2 members, got: {:?}", decoded);
        prop_assert!(decoded.iter().any(|d| d.contains("/innermost") && d.contains(&format!("\"{a}\""))),
            "deepest list:first must unwrap all the way to {}, got: {:?}", a, decoded);
    }
}

// list:memberAt out-of-range index must not derive (no crash, no wraparound,
// no silent zero).
proptest! {
    #[test]
    fn prop_list_member_at_out_of_range_never_derives(
        len in 0usize..6,
        idx in 6i64..20,
    ) {
        let members: Vec<i64> = (0..len as i64).collect();
        let list_str = members.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( {list_str} ) .\n\
             :s :i {idx} .\n\
             {{ ?s :l ?l . ?s :i ?i . ( ?l ?i ) list:memberAt ?x }} => {{ ?s :result ?x }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("/result")),
            "memberAt index {} is out of range for a list of length {}, must not derive, got: {:?}",
            idx, len, decoded
        );
    }

    #[test]
    fn prop_list_member_at_negative_index_never_derives(
        neg_idx in -20i64..0,
    ) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( 1 2 3 ) .\n\
             :s :i {neg_idx} .\n\
             {{ ?s :l ?l . ?s :i ?i . ( ?l ?i ) list:memberAt ?x }} => {{ ?s :result ?x }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("/result")),
            "negative memberAt index {} must not derive, got: {:?}", neg_idx, decoded
        );
    }

    #[test]
    fn prop_list_member_at_in_range_exact_value(
        members in prop::collection::vec(0i64..100, 1..8),
    ) {
        let idx = members.len() / 2; // always in range
        let list_str = members.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( {list_str} ) .\n\
             :s :i {idx} .\n\
             {{ ?s :l ?l . ?s :i ?i . ( ?l ?i ) list:memberAt ?x }} => {{ ?s :result ?x }}.\n"
        );
        let decoded = materialize(&data);
        let expected = members[idx];
        prop_assert!(
            decoded.iter().any(|d| d.contains("/result") && d.contains(&format!("\"{expected}\""))),
            "memberAt {} of {:?} must be {}, got: {:?}", idx, members, expected, decoded
        );
    }
}
