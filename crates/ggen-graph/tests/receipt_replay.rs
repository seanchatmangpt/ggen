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
use ggen_graph::{DeterministicGraph, RdfDelta};
use std::error::Error;

#[test]
fn test_receipt_replay_success() -> Result<(), Box<dyn Error>> {
    let q1 = "<http://example.org/user/1> <http://example.org/status> \"active\" .";
    let q2 = "<http://example.org/user/1> <http://example.org/status> \"inactive\" .";

    let quad1 = DeterministicGraph::parse_nquad(q1)?;
    let quad2 = DeterministicGraph::parse_nquad(q2)?;

    // Initial state
    let graph = DeterministicGraph::new()?;
    graph.insert_quad(&quad1)?;
    let initial_hash = graph.state_hash()?;

    // Create target state
    let target = DeterministicGraph::new()?;
    target.insert_quad(&quad2)?;
    let final_hash = target.state_hash()?;

    // Compute and apply delta
    let delta = RdfDelta::compute(&graph, &target)?;
    let receipt = graph.apply_delta(&delta, &[])?;

    // Verify receipt properties
    receipt.verify()?;
    assert_eq!(receipt.pre_state_hash, initial_hash);
    assert_eq!(receipt.post_state_hash, final_hash);
    assert_eq!(receipt.delta_hash, delta.hash());

    // Replay: Start another graph at initial_hash, apply the same delta
    let replay_graph = DeterministicGraph::new()?;
    replay_graph.insert_quad(&quad1)?;
    assert_eq!(replay_graph.state_hash()?, initial_hash);

    let replay_receipt = replay_graph.apply_delta(&delta, &[])?;
    replay_receipt.verify()?;

    // Verify the state matches the receipt
    assert_eq!(replay_graph.state_hash()?, final_hash);
    assert_eq!(replay_graph.state_hash()?, receipt.post_state_hash);

    Ok(())
}

#[test]
fn test_receipt_tamper_detection() -> Result<(), Box<dyn Error>> {
    let q1 = "<http://example.org/item/1> <http://example.org/price> \"100\" .";
    let q2 = "<http://example.org/item/1> <http://example.org/price> \"120\" .";

    let graph = DeterministicGraph::new()?;
    graph.insert_quad(&DeterministicGraph::parse_nquad(q1)?)?;

    let target = DeterministicGraph::new()?;
    target.insert_quad(&DeterministicGraph::parse_nquad(q2)?)?;

    let delta = RdfDelta::compute(&graph, &target)?;
    let receipt = graph.apply_delta(&delta, &[])?;

    // Verifying intact receipt passes
    receipt.verify()?;

    // Try modifying timestamp
    let mut tampered_receipt = receipt.clone();
    tampered_receipt.timestamp += chrono::Duration::seconds(1);
    assert!(tampered_receipt.verify().is_err());

    // Try modifying pre_state_hash
    let mut tampered_receipt2 = receipt.clone();
    tampered_receipt2.pre_state_hash[0] ^= 1;
    assert!(tampered_receipt2.verify().is_err());

    // Try modifying post_state_hash
    let mut tampered_receipt3 = receipt.clone();
    tampered_receipt3.post_state_hash[0] ^= 1;
    assert!(tampered_receipt3.verify().is_err());

    // Try modifying delta_hash
    let mut tampered_receipt4 = receipt.clone();
    tampered_receipt4.delta_hash[0] ^= 1;
    assert!(tampered_receipt4.verify().is_err());

    Ok(())
}
