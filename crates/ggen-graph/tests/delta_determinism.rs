use ggen_graph::{DeterministicGraph, RdfDelta};
use std::error::Error;

#[test]
fn test_delta_determinism_insertion_order() -> Result<(), Box<dyn Error>> {
    let q1 = "<http://example.org/s1> <http://example.org/p1> \"o1\" .";
    let q2 = "<http://example.org/s2> <http://example.org/p2> \"o2\" .";
    let q3 = "<http://example.org/s3> <http://example.org/p3> \"o3\" .";

    // Baseline graph
    let baseline = DeterministicGraph::new()?;
    let quad1 = DeterministicGraph::parse_nquad(q1)?;
    baseline.insert_quad(&quad1)?;

    // Target 1: insert q2 then q3
    let target1 = DeterministicGraph::new()?;
    target1.insert_quad(&quad1)?;
    target1.insert_quad(&DeterministicGraph::parse_nquad(q2)?)?;
    target1.insert_quad(&DeterministicGraph::parse_nquad(q3)?)?;

    // Target 2: insert q3 then q2
    let target2 = DeterministicGraph::new()?;
    target2.insert_quad(&quad1)?;
    target2.insert_quad(&DeterministicGraph::parse_nquad(q3)?)?;
    target2.insert_quad(&DeterministicGraph::parse_nquad(q2)?)?;

    // Compute deltas from baseline to targets
    let delta1 = RdfDelta::compute(&baseline, &target1)?;
    let delta2 = RdfDelta::compute(&baseline, &target2)?;

    // The deltas must be identical
    assert_eq!(delta1, delta2);
    assert_eq!(delta1.hash(), delta2.hash());

    // Apply delta to baseline and verify it transitions to target state
    let receipt = baseline.apply_delta(&delta1, &[])?;
    receipt.verify()?;

    assert_eq!(baseline.state_hash()?, target1.state_hash()?);
    assert_eq!(baseline.state_hash()?, target2.state_hash()?);

    Ok(())
}

#[test]
fn test_delta_determinism_empty_transitions() -> Result<(), Box<dyn Error>> {
    let graph = DeterministicGraph::new()?;
    let delta = RdfDelta::compute(&graph, &graph)?;

    assert!(delta.additions.is_empty());
    assert!(delta.deletions.is_empty());

    let pre_hash = graph.state_hash()?;
    let receipt = graph.apply_delta(&delta, &[])?;
    receipt.verify()?;

    assert_eq!(graph.state_hash()?, pre_hash);
    Ok(())
}
