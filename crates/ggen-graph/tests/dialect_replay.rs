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
use ggen_graph::dialect::{check_n3, check_sparql};

#[test]
fn test_dialect_replay() {
    // Replay SPARQL
    let query = "ASK { ?s ?p ?o }";
    let res1 = check_sparql(query).unwrap();
    let res2 = check_sparql(query).unwrap();
    assert_eq!(res1.conforms, res2.conforms);
    assert_eq!(res1.message, res2.message);
    assert_eq!(res1.supported, res2.supported);

    // Replay N3
    let n3 = "{ ?x a <http://www.w3.org/ns/prov#Entity> } => { ?x a <http://www.w3.org/ns/prov#Item> } .";
    let res_n3_1 = check_n3(n3).unwrap();
    let res_n3_2 = check_n3(n3).unwrap();
    assert_eq!(res_n3_1.conforms, res_n3_2.conforms);
    assert_eq!(res_n3_1.message, res_n3_2.message);
    assert_eq!(res_n3_1.supported, res_n3_2.supported);
}
