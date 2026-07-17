//! Combinatorial builtin-dispatch fuzzing (targets a specifically named
//! low-confidence N3 gap: existing `n3_builtins.rs` tests exercise exactly
//! ONE builtin per rule body -- combining builtins from different
//! namespaces (`math:`, `string:`, `list:`, `log:`) in a single rule body
//! is untested and is exactly where a builtin-dispatch bug that only
//! manifests when multiple builtins share row-context/bindings would hide).

use praxis_graphlaw::TripleStore;
use proptest::prelude::*;

fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples
        .iter()
        .map(praxis_graphlaw::TripleStore::decode_triple)
        .collect()
}

// Combine `math:sum` + `math:greaterThan` + `string:concat` in a single
// rule body: compute a total price, gate on it exceeding a threshold, and
// build a combined label -- all three builtins must correctly share the
// same row's bindings simultaneously.
proptest! {
    #[test]
    fn prop_three_builtins_combined_in_one_rule(
        item_price in 0i64..500,
        tax in 0i64..100,
    ) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
             @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
             \n\
             :order :itemPrice {item_price} .\n\
             :order :taxPrice {tax} .\n\
             :order :label \"Order\" .\n\
             \n\
             {{ ?o :itemPrice ?p1 . ?o :taxPrice ?p2 . ?o :label ?lbl . \
                ( ?p1 ?p2 ) math:sum ?total . ?total math:greaterThan 100 . \
                ( ?lbl \": \" ?total ) string:concat ?full }} => {{ ?o :bigOrderLabel ?full }}.\n"
        );

        let mut store = TripleStore::from(&data);
        let inferred = store.materialize().unwrap();
        let decoded = decode_all(&inferred);

        let total = item_price + tax;
        let should_derive = total > 100;

        if should_derive {
            let expected_label = format!("Order: {total}");
            prop_assert!(
                decoded.iter().any(|d| d.contains("bigOrderLabel") && d.contains(&expected_label)),
                "total={} (>100) must derive bigOrderLabel {:?}, got: {:?}", total, expected_label, decoded
            );
        } else {
            prop_assert!(
                !decoded.iter().any(|d| d.contains("bigOrderLabel")),
                "total={} (<=100) must NOT derive any bigOrderLabel, got: {:?}", total, decoded
            );
        }
    }
}

// Combine `list:in` + `math:greaterThan` in one rule body: a value must
// simultaneously be a member of a list AND exceed a numeric threshold.
proptest! {
    #[test]
    fn prop_list_in_and_math_greater_than_combined(
        values in prop::collection::vec(0i64..20, 1..6),
    ) {
        let mut data = String::from(
            "@prefix : <http://example.org/> .\n\
             @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n"
        );
        data.push_str(&format!(
            ":s :values ( {} ) .\n",
            values.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ")
        ));
        data.push_str(
            "{ ?s :values ?list . ?item list:in ?list . ?item math:greaterThan 10 } => { ?item a :BigValue }.\n"
        );

        let mut store = TripleStore::from(&data);
        let inferred = store.materialize().unwrap();
        let decoded = decode_all(&inferred);

        let expected: std::collections::HashSet<i64> = values.iter().copied().filter(|&v| v > 10).collect();
        let actual: std::collections::HashSet<i64> = values.iter()
            .copied()
            .filter(|v| decoded.iter().any(|d| d.contains("BigValue") && d.contains(&format!("\"{v}\""))))
            .collect();

        prop_assert_eq!(
            &actual, &expected,
            "values {:?}: expected BigValue for {:?}, derived for {:?}. Full derived set: {:?}",
            values, expected, actual, decoded
        );
    }
}
