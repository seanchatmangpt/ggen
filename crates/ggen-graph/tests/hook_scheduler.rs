use ggen_graph::{DeterministicGraph, KnowledgeHook, RdfDelta};
use std::error::Error;

#[test]
fn test_hook_scheduler_all_pass() -> Result<(), Box<dyn Error>> {
    let graph = DeterministicGraph::new()?;
    let q1 = "<http://example.org/a> <http://example.org/status> \"pending\" .";
    graph.insert_quad(&DeterministicGraph::parse_nquad(q1)?)?;

    let q2 = "<http://example.org/a> <http://example.org/status> \"approved\" .";
    let target = DeterministicGraph::new()?;
    target.insert_quad(&DeterministicGraph::parse_nquad(q2)?)?;

    let delta = RdfDelta::compute(&graph, &target)?;

    // Scheduled hooks
    let hook1 = KnowledgeHook::new(
        "has_status".to_string(),
        "ASK WHERE { ?s <http://example.org/status> ?status }".to_string(),
    );
    let hook2 = KnowledgeHook::new(
        "not_pending".to_string(),
        "ASK WHERE { FILTER NOT EXISTS { ?s <http://example.org/status> \"pending\" } }"
            .to_string(),
    );

    let receipt = graph.apply_delta(&delta, &[hook1, hook2])?;
    receipt.verify()?;

    // The changes should have been applied successfully
    assert!(graph.contains_quad(&DeterministicGraph::parse_nquad(q2)?)?);
    assert!(!graph.contains_quad(&DeterministicGraph::parse_nquad(q1)?)?);

    Ok(())
}

#[test]
fn test_hook_scheduler_aborts_on_failure() -> Result<(), Box<dyn Error>> {
    let graph = DeterministicGraph::new()?;
    let q1 = "<http://example.org/a> <http://example.org/status> \"pending\" .";
    graph.insert_quad(&DeterministicGraph::parse_nquad(q1)?)?;

    let q2 = "<http://example.org/a> <http://example.org/status> \"rejected\" .";
    let target = DeterministicGraph::new()?;
    target.insert_quad(&DeterministicGraph::parse_nquad(q2)?)?;

    let delta = RdfDelta::compute(&graph, &target)?;

    // Hook 1: Checks status presence (would pass)
    let hook1 = KnowledgeHook::new(
        "has_status".to_string(),
        "ASK WHERE { ?s <http://example.org/status> ?status }".to_string(),
    );
    // Hook 2: Explicitly forbids "rejected" (will fail)
    let hook2 = KnowledgeHook::new(
        "no_rejected".to_string(),
        "ASK WHERE { FILTER NOT EXISTS { ?s <http://example.org/status> \"rejected\" } }"
            .to_string(),
    );

    let res = graph.apply_delta(&delta, &[hook1, hook2]);
    assert!(res.is_err());

    // State should be rolled back to pending
    assert!(graph.contains_quad(&DeterministicGraph::parse_nquad(q1)?)?);
    assert!(!graph.contains_quad(&DeterministicGraph::parse_nquad(q2)?)?);

    Ok(())
}
