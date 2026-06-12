#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
use ggen_graph::dialect::{check_datalog, check_n3, check_shacl, check_shex, check_sparql};

#[test]
fn test_dialect_sabotage_checks() {
    // 1. Sabotage SPARQL: invalid query string
    let res = check_sparql("SELECT ?s WHERE {");
    assert!(res.is_err());

    // 2. Sabotage SHACL: invalid turtle syntax
    let res = check_shacl("PREFIX sh: <http://www.w3.org/ns/shacl#> \n <Shape> a sh:NodeShape");
    assert!(res.is_err());

    // 3. Sabotage N3: unbalanced brackets
    let res = check_n3("{ ?x a <http://example.org/T> .");
    assert!(res.is_err());

    // 4. Sabotage Datalog: missing rule termination
    let res = check_datalog("ancestor(X, Y) :- parent(X, Y)");
    assert!(res.is_err());

    // 5. Sabotage ShEx: missing brace
    let res = check_shex("<Shape> {");
    assert!(res.is_err());
}
