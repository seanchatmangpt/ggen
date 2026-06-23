//! Deterministic RDF graph module for `ggen`.
//!
//! Provides a wrapper around Oxigraph with deterministic hashing, state change detection
//! (deltas), validation hooks, and cryptographic transition receipts.

#![deny(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::todo,
    clippy::unimplemented
)]

pub mod coherence;
pub mod delta;
pub mod diagnostics;
pub mod dialect;
pub mod doctor;
pub mod graph;
pub mod interchangeable;
pub mod ocel;
pub mod prelude;
pub mod receipt;
pub mod shacl;
pub mod sparql;
pub mod vocab;

pub use coherence::{
    CoherenceChecker, CoherenceDrift, CoherenceReport, DriftKind, Pole, PoleState,
};
pub use graph::quad::parse_nquad;
pub use graph::{
    extract_prefixes, iri_terms, parse_nquads_located, parse_ntriples_located,
    parse_turtle_located, DeterministicGraph, IriTerms, KnowledgeHook, LocatedParse,
    ParseDiagnostic, RdfDelta, TransitionReceipt,
};
pub use interchangeable::{AdapterLayer, GenesisCore, OuterMembrane, ProjectionLayer};
pub use ocel::{
    check_guard, check_lifecycle_order, ConformanceDrift, ConformanceReport, discover_dfg, DfgEdge,
    OcelConformanceChecker, OcelProcessDiscovery, ProcessModel,
};
pub use shacl::{validate_shacl, ShaclSeverity, ShaclViolation};
pub use sparql::{check_sparql_syntax, sparql_kind, SparqlKind};

/// Error type for deterministic graph operations.
#[derive(Debug, thiserror::Error)]
pub enum GraphError {
    /// Errors originating from the underlying Oxigraph storage.
    #[error("Oxigraph storage error: {0}")]
    Oxigraph(#[from] oxigraph::store::StorageError),

    /// Errors originating from SPARQL query evaluation.
    #[error("SPARQL evaluation error: {0}")]
    Sparql(#[from] oxigraph::sparql::QueryEvaluationError),

    /// Errors related to RDF serialization or parsing.
    #[error("Serialization or parsing error: {0}")]
    Serialization(String),

    /// Verification failures for transition receipts.
    #[error("Receipt verification failed: {0}")]
    VerificationFailed(String),

    /// Failures in hook validation execution.
    #[error("Knowledge hook validation failed: {0}")]
    HookFailed(String),

    /// Other failures or errors.
    #[error("Other error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_graph_and_receipt_flow() -> Result<(), Box<dyn std::error::Error>> {
        let graph = DeterministicGraph::new()?;
        let q1_str = "<http://example.org/alice> <http://example.org/name> \"Alice\" <http://example.org/graph> .";
        let q2_str = "<http://example.org/bob> <http://example.org/name> \"Bob\" <http://example.org/graph> .";
        let q1 = parse_nquad(q1_str)?;
        let q2 = parse_nquad(q2_str)?;

        // Test insertion & contains
        graph.insert_quad(&q1)?;
        assert!(graph.contains_quad(&q1)?);
        assert!(!graph.contains_quad(&q2)?);

        // Test state hash
        let hash1 = graph.state_hash()?;

        // Test delta computation
        let target = DeterministicGraph::new()?;
        target.insert_quad(&q1)?;
        target.insert_quad(&q2)?;
        let hash2 = target.state_hash()?;

        let delta = RdfDelta::compute(&graph, &target)?;
        assert_eq!(delta.additions.len(), 1);
        assert_eq!(delta.deletions.len(), 0);
        assert_eq!(delta.additions[0], q2.to_string());

        // Test transition receipt
        let receipt = graph.apply_delta(&delta, &[])?;
        assert_eq!(receipt.pre_state_hash, hash1);
        assert_eq!(receipt.post_state_hash, hash2);
        assert_eq!(receipt.delta_hash, delta.hash());
        receipt.verify()?;

        // Verify cryptographic falsifiability
        let mut tampered_receipt = receipt.clone();
        tampered_receipt.signature_or_hash[0] ^= 1;
        assert!(tampered_receipt.verify().is_err());

        // Test validation hook success
        let hook_bob = KnowledgeHook::new(
            "has_bob".to_string(),
            "ASK WHERE { GRAPH ?g { ?s <http://example.org/name> \"Bob\" } }".to_string(),
        );
        assert!(hook_bob.execute(&graph)?);

        // Test validation hook failure (no Charlie should exist)
        let hook_no_charlie = KnowledgeHook::new(
            "no_charlie".to_string(),
            "ASK WHERE { FILTER NOT EXISTS { GRAPH ?g { ?s <http://example.org/name> \"Charlie\" } } }".to_string(),
        );
        assert!(hook_no_charlie.execute(&graph)?);

        // Try applying a delta that adds Charlie but violates the hook
        let target_charlie = DeterministicGraph::new()?;
        target_charlie.insert_quad(&q1)?;
        target_charlie.insert_quad(&q2)?;
        let q3_str = "<http://example.org/charlie> <http://example.org/name> \"Charlie\" <http://example.org/graph> .";
        let q3 = parse_nquad(q3_str)?;
        target_charlie.insert_quad(&q3)?;

        let delta_charlie = RdfDelta::compute(&graph, &target_charlie)?;
        let apply_res = graph.apply_delta(&delta_charlie, &[hook_no_charlie]);

        // Should fail due to the hook
        assert!(apply_res.is_err());

        // State should be rolled back (Charlie not present, Bob and Alice still present)
        assert!(graph.contains_quad(&q1)?);
        assert!(graph.contains_quad(&q2)?);
        assert!(!graph.contains_quad(&q3)?);
        assert_eq!(graph.state_hash()?, hash2);

        Ok(())
    }
}
