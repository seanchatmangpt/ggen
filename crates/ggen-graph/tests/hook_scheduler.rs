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
use ggen_graph::{DeterministicGraph, GraphError, KnowledgeHook, RdfDelta};
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

/// Regression test for the `apply_delta` rollback gap: a hook whose SPARQL query
/// is syntactically valid but raises a runtime evaluation error (as opposed to
/// cleanly evaluating to `false`) must still trigger the documented rollback of
/// already-applied deletions/insertions. Before the fix, `hook.execute(self)?`
/// propagated the error via `?` and unwound past the rollback block entirely,
/// leaving the store partially mutated.
///
/// A `SERVICE <iri> { ... }` clause is syntactically valid SPARQL 1.1 that
/// reliably raises `QueryEvaluationError::UnsupportedService` at evaluation
/// time in this workspace (oxigraph's `http-client` feature -- the only thing
/// that would register a default SERVICE handler -- is not enabled), so no
/// network access is required to exercise this path deterministically.
#[test]
fn test_hook_scheduler_rolls_back_on_hook_evaluation_error() -> Result<(), Box<dyn Error>> {
    let graph = DeterministicGraph::new()?;
    let q1 = "<http://example.org/a> <http://example.org/status> \"pending\" .";
    graph.insert_quad(&DeterministicGraph::parse_nquad(q1)?)?;

    let q2 = "<http://example.org/a> <http://example.org/status> \"approved\" .";
    let target = DeterministicGraph::new()?;
    target.insert_quad(&DeterministicGraph::parse_nquad(q2)?)?;

    let delta = RdfDelta::compute(&graph, &target)?;

    // This hook is syntactically valid SPARQL but errors out at evaluation
    // time (no SERVICE handler is registered for this IRI), rather than
    // cleanly evaluating to `false`.
    let erroring_hook = KnowledgeHook::new(
        "unreachable_service_check".to_string(),
        "ASK { SERVICE <http://ggen-test.invalid/service> { ?s ?p ?o } }".to_string(),
    );

    // Sanity check: confirm this query really does return `Err`, not `Ok(false)`,
    // so the test actually exercises the error path the fix targets.
    let direct_result = erroring_hook.execute(&graph);
    assert!(
        matches!(direct_result, Err(GraphError::Sparql(_))),
        "expected a GraphError::Sparql evaluation error, got: {direct_result:?}"
    );

    let res = graph.apply_delta(&delta, &[erroring_hook]);
    assert!(res.is_err());

    // State must be rolled back to pre-delta contents despite the hook
    // failing via a hard error rather than a clean `false` evaluation.
    assert!(graph.contains_quad(&DeterministicGraph::parse_nquad(q1)?)?);
    assert!(!graph.contains_quad(&DeterministicGraph::parse_nquad(q2)?)?);

    Ok(())
}
