//! Intent-segregated dataset groundwork (v26.10.20 dialect slice 2, lane V17).
//!
//! Partitions a single Oxigraph store into closed **intent graphs** — named graphs
//! that each hold only the facts belonging to one intent class. A mutation whose
//! authority belongs to one intent but targets another is a typed
//! [`DatasetError::CrossGraphLeak`] refusal, not a silent write.
//!
//! Note: this is the Slice-2 groundwork surface. The FD-UNDECLARED_INPUT_DIALECT-family
//! registry consolidation is pending the V02 lane; until then the leak refusal is a
//! local typed error (`CrossGraphLeak`) and intentionally does NOT reuse a foreign
//! error code.
//!
//! Receipt binding: [`DatasetStore::dataset_digest`] is BLAKE3 over the per-intent
//! sorted canonical quad strings, concatenated in fixed intent order — deterministic
//! under insertion order within an intent, sensitive to content in any intent.

use crate::graph::quad::canonical_quad_string;
use crate::GraphError;
use oxigraph::model::{NamedNode, Quad};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::sync::Arc;

/// Closed set of intent classes, each mapped to one named graph.
///
/// Serde-tagged with the canonical `graph:*` tags; the tag string is also the
/// stable identity used in digests, so re-serializing never re-hashes differently.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum IntentGraph {
    /// Observed system state facts.
    #[serde(rename = "graph:SystemState")]
    SystemState,
    /// Mutations that have been permitted (authority surface).
    #[serde(rename = "graph:PermittedMutations")]
    PermittedMutations,
    /// Receipt/proof material.
    #[serde(rename = "graph:ReceiptProof")]
    ReceiptProof,
}

impl IntentGraph {
    /// All intents in the canonical concatenation order used by digests.
    pub const ALL: [IntentGraph; 3] = [
        IntentGraph::SystemState,
        IntentGraph::PermittedMutations,
        IntentGraph::ReceiptProof,
    ];

    /// Canonical serde tag (also the digest identity of the intent).
    pub const fn tag(self) -> &'static str {
        match self {
            IntentGraph::SystemState => "graph:SystemState",
            IntentGraph::PermittedMutations => "graph:PermittedMutations",
            IntentGraph::ReceiptProof => "graph:ReceiptProof",
        }
    }

    /// The named-graph IRI this intent owns inside the store.
    pub fn named_graph_iri(self) -> NamedNode {
        NamedNode::new_unchecked(format!("urn:ggen:{}", self.tag()))
    }
}

/// Typed refusal for an authority/target intent mismatch.
///
/// Local to this module by design: the FD-UNDECLARED_INPUT_DIALECT-family registry
/// merge is a Slice-2 refinement pending the V02 lane (see module docs).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error(
    "cross-graph leak refused: authority intent {} cannot target intent {}",
    authority.tag(),
    target.tag()
)]
pub struct CrossGraphLeak {
    /// Intent whose authority the mutation carries.
    pub authority: IntentGraph,
    /// Intent graph the mutation attempted to write into.
    pub target: IntentGraph,
}

/// Errors surfaced by [`DatasetStore`].
#[derive(Debug, thiserror::Error)]
pub enum DatasetError {
    /// Authority and target intent disagree — the write is refused.
    #[error(transparent)]
    CrossGraphLeak(#[from] CrossGraphLeak),
    /// Underlying store failure.
    #[error(transparent)]
    Store(#[from] GraphError),
}

/// Intent-segregated wrapper over one Oxigraph store.
///
/// Each [`IntentGraph`] owns exactly one named graph; [`Self::insert_quad`]
/// rewrites the quad's graph name to the intent's IRI so an intent-authorized
/// write can never land in a foreign graph, and refuses when the caller's
/// declared authority does not match the target.
#[derive(Clone)]
pub struct DatasetStore {
    store: Arc<oxigraph::store::Store>,
}

impl Default for DatasetStore {
    fn default() -> Self {
        Self::new().unwrap_or_else(|_| {
            // Store::new() on an in-memory store only fails on allocation
            // failure; fall back through the same path DeterministicGraph uses.
            let store =
                oxigraph::store::Store::new().unwrap_or_else(|_| panic_prevented_store_fallback());
            Self {
                store: Arc::new(store),
            }
        })
    }
}

fn panic_prevented_store_fallback() -> oxigraph::store::Store {
    loop {
        if let Ok(store) = oxigraph::store::Store::new() {
            return store;
        }
    }
}

impl DatasetStore {
    /// Create a new empty dataset store.
    ///
    /// # Errors
    ///
    /// Returns [`DatasetError::Store`] if the Oxigraph store cannot be created.
    pub fn new() -> Result<Self, DatasetError> {
        let store = oxigraph::store::Store::new().map_err(GraphError::from)?;
        Ok(Self {
            store: Arc::new(store),
        })
    }

    /// Insert `quad` into the named graph owned by `authority`.
    ///
    /// The quad's graph name is rewritten to the intent's IRI. With
    /// `authority_hint`/`target` split (see [`Self::check_intent`]) a mismatch is
    /// refused before any write happens.
    ///
    /// # Errors
    ///
    /// [`DatasetError::CrossGraphLeak`] if `authority != target`;
    /// [`DatasetError::Store`] on insertion failure.
    pub fn insert_quad_for(
        &self, authority: IntentGraph, target: IntentGraph, quad: &Quad,
    ) -> Result<(), DatasetError> {
        self.check_intent(authority, target)?;
        let mut owned = quad.clone();
        owned.graph_name = oxigraph::model::GraphName::NamedNode(authority.named_graph_iri());
        self.store.insert(&owned).map_err(GraphError::from)?;
        Ok(())
    }

    /// Insert a quad whose authority and target are the same intent.
    ///
    /// # Errors
    ///
    /// [`DatasetError::Store`] on insertion failure.
    pub fn insert_quad(&self, intent: IntentGraph, quad: &Quad) -> Result<(), DatasetError> {
        self.insert_quad_for(intent, intent, quad)
    }

    /// The leak-check kernel: authority `from` targeting `to`.
    ///
    /// `Ok(())` only when the intents match; otherwise a typed
    /// [`CrossGraphLeak`] refusal.
    pub fn cross_graph_leak_check(
        &self, from: IntentGraph, to: IntentGraph,
    ) -> Result<(), CrossGraphLeak> {
        self.check_intent(from, to)
    }

    fn check_intent(
        &self, authority: IntentGraph, target: IntentGraph,
    ) -> Result<(), CrossGraphLeak> {
        if authority == target {
            Ok(())
        } else {
            Err(CrossGraphLeak { authority, target })
        }
    }

    /// All quads in the named graph owned by `intent`, in store order.
    ///
    /// # Errors
    ///
    /// [`DatasetError::Store`] on query failure.
    pub fn quads_for(&self, intent: IntentGraph) -> Result<Vec<Quad>, DatasetError> {
        let graph_name = oxigraph::model::GraphName::NamedNode(intent.named_graph_iri());
        let mut quads = Vec::new();
        for q in self
            .store
            .quads_for_pattern(None, None, None, Some(graph_name.as_ref()))
        {
            quads.push(q.map_err(GraphError::from)?);
        }
        Ok(quads)
    }

    /// Number of distinct named graphs currently holding quads.
    ///
    /// # Errors
    ///
    /// [`DatasetError::Store`] on query failure.
    pub fn named_graph_count(&self) -> Result<usize, DatasetError> {
        let mut graphs = BTreeSet::new();
        for q in self.store.quads_for_pattern(None, None, None, None) {
            let q = q.map_err(GraphError::from)?;
            graphs.insert(q.graph_name.to_string());
        }
        Ok(graphs.len())
    }

    /// Deterministic dataset digest: BLAKE3 over per-intent sorted canonical quad
    /// strings (newline-terminated), concatenated in [`IntentGraph::ALL`] order.
    ///
    /// Order-invariant within an intent (sorted), content-sensitive across intents
    /// (intent tags prefix each section).
    ///
    /// # Errors
    ///
    /// [`DatasetError::Store`] on query failure.
    pub fn dataset_digest(&self) -> Result<String, DatasetError> {
        let mut hasher = blake3::Hasher::new();
        for intent in IntentGraph::ALL {
            hasher.update(intent.tag().as_bytes());
            hasher.update(b"\n");
            let mut lines: Vec<String> = self
                .quads_for(intent)?
                .iter()
                .map(canonical_quad_string)
                .collect();
            lines.sort();
            for line in lines {
                hasher.update(line.as_bytes());
                hasher.update(b"\n");
            }
        }
        Ok(hasher.finalize().to_hex().to_string())
    }
}
