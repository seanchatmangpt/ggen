//! State change representation and difference computation for RDF graphs.

use crate::graph::hash::hash_delta;
use crate::graph::DeterministicGraph;
use crate::GraphError;
use serde::{Deserialize, Serialize};

/// Represents a set of modifications (additions and deletions) to an RDF graph.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RdfDelta {
    /// List of N-Quads to be added.
    pub additions: Vec<String>,
    /// List of N-Quads to be deleted.
    pub deletions: Vec<String>,
}

impl RdfDelta {
    /// Create a new `RdfDelta`.
    pub fn new(additions: Vec<String>, deletions: Vec<String>) -> Self {
        Self {
            additions,
            deletions,
        }
    }

    /// Computes the difference between a baseline graph and a target graph.
    ///
    /// # Errors
    ///
    /// Returns `GraphError` if reading quads fails.
    pub fn compute(
        baseline: &DeterministicGraph, target: &DeterministicGraph,
    ) -> Result<Self, GraphError> {
        let baseline_quads = baseline.all_quads()?;
        let target_quads = target.all_quads()?;

        let mut additions = Vec::new();
        let mut deletions = Vec::new();

        let baseline_set: std::collections::HashSet<String> =
            baseline_quads.iter().map(ToString::to_string).collect();
        let target_set: std::collections::HashSet<String> =
            target_quads.iter().map(ToString::to_string).collect();

        for q_str in &target_set {
            if !baseline_set.contains(q_str) {
                additions.push(q_str.clone());
            }
        }

        for q_str in &baseline_set {
            if !target_set.contains(q_str) {
                deletions.push(q_str.clone());
            }
        }

        additions.sort();
        deletions.sort();

        Ok(Self {
            additions,
            deletions,
        })
    }

    /// Applies the delta directly to the given `DeterministicGraph`.
    ///
    /// # Errors
    ///
    /// Returns `GraphError` if parsing or applying quads fails.
    pub fn apply(&self, graph: &DeterministicGraph) -> Result<(), GraphError> {
        for del in &self.deletions {
            let quad = DeterministicGraph::parse_nquad(del)?;
            graph.remove_quad(&quad)?;
        }
        for add in &self.additions {
            let quad = DeterministicGraph::parse_nquad(add)?;
            graph.insert_quad(&quad)?;
        }
        Ok(())
    }

    /// Computes the difference (diff) between two deltas.
    /// Returns a new `RdfDelta` representing additions and deletions that are in `other` but not in `self`.
    pub fn diff(&self, other: &Self) -> Self {
        let mut additions = Vec::new();
        let self_adds: std::collections::HashSet<_> = self.additions.iter().collect();
        for add in &other.additions {
            if !self_adds.contains(add) {
                additions.push(add.clone());
            }
        }

        let mut deletions = Vec::new();
        let self_dels: std::collections::HashSet<_> = self.deletions.iter().collect();
        for del in &other.deletions {
            if !self_dels.contains(del) {
                deletions.push(del.clone());
            }
        }

        Self {
            additions,
            deletions,
        }
    }

    /// Computes the blake3 hash of the delta.
    pub fn hash(&self) -> [u8; 32] {
        hash_delta(&self.additions, &self.deletions)
    }
}
