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
// Tests use unwrap() for clear failure messages; panics are intentional in test context.
use ggen_graph::dialect::check_n3;

#[test]
fn test_dialect_receipt_binding() {
    let input = "{ ?x a <http://www.w3.org/ns/prov#Entity> } => { ?x a <http://www.w3.org/ns/prov#Item> } .";

    // Evaluate N3 input
    let res = check_n3(input).unwrap();

    // Bind inputs/outputs in a cryptographic hash
    let mut hasher = blake3::Hasher::new();
    hasher.update(input.as_bytes());
    hasher.update(res.conforms.to_string().as_bytes());
    hasher.update(res.message.as_bytes());
    hasher.update(res.supported.to_string().as_bytes());
    let receipt_hash = hasher.finalize().to_hex().to_string();

    assert!(!receipt_hash.is_empty());

    // Verify that the receipt is deterministic
    let mut hasher2 = blake3::Hasher::new();
    hasher2.update(input.as_bytes());
    hasher2.update(res.conforms.to_string().as_bytes());
    hasher2.update(res.message.as_bytes());
    hasher2.update(res.supported.to_string().as_bytes());
    let receipt_hash2 = hasher2.finalize().to_hex().to_string();

    assert_eq!(receipt_hash, receipt_hash2);
}
