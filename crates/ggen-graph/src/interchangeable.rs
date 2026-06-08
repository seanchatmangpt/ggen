//! Interchangeable parts architecture for ggen-graph.
//!
//! Provides the core abstractions:
//! 1. `GenesisCore`: The embedded RDF graph storage core.
//! 2. `OuterMembrane`: Admission control, validation, and sanitization boundary.
//! 3. `AdapterLayer`: Protocol translation layer (e.g. JSON-RPC to internal deltas).
//! 4. `ProjectionLayer`: Outward-facing state projectors (OCEL, PROV, receipts).

use crate::delta::RdfDelta;
use crate::graph::dataset::DeterministicGraph;
use crate::ocel::{EvidenceProjector, OCEL};
use crate::receipt::GraphReceipt;
use crate::GraphError;
use oxigraph::model::Quad;
use serde_json::Value;

/// Genesis acts as the embedded core of the interchangeable part architecture.
#[derive(Clone)]
pub struct GenesisCore {
    graph: DeterministicGraph,
}

impl GenesisCore {
    /// Create a new instance of the Genesis core.
    pub fn new() -> Result<Self, GraphError> {
        Ok(Self {
            graph: DeterministicGraph::new()?,
        })
    }

    /// Access the underlying deterministic graph.
    pub fn graph(&self) -> &DeterministicGraph {
        &self.graph
    }

    /// Execute a transaction with the core using an RdfDelta.
    pub fn execute_transaction(&self, delta: &RdfDelta) -> Result<GraphReceipt, GraphError> {
        self.graph.apply_delta(delta, &[])
    }
}

/// The Outer Membrane forms the boundary around the Genesis core, intercepting,
/// validating, and sanitizing all inputs and operations.
#[derive(Default)]
pub struct OuterMembrane;

impl OuterMembrane {
    /// Create a new instance of the Outer Membrane.
    pub fn new() -> Self {
        Self
    }

    /// Admits and sanitizes an incoming SPARQL query or N-Quad string.
    ///
    /// # Errors
    ///
    /// Returns `GraphError` if the query is empty or contains injection attempts.
    pub fn admit_input(&self, query: &str) -> Result<(), GraphError> {
        let trimmed = query.trim();
        if trimmed.is_empty() {
            return Err(GraphError::Other(
                "Empty input rejected by outer membrane".to_string(),
            ));
        }

        // Active injection/sabotage scanner
        let upper = trimmed.to_uppercase();
        if upper.contains("DROP GRAPH") || upper.contains("DELETE WHERE") {
            return Err(GraphError::Other(
                "Security violation: injection pattern blocked by outer membrane".to_string(),
            ));
        }

        Ok(())
    }

    /// Validates a list of quads before admitting them to the core.
    ///
    /// # Errors
    ///
    /// Returns `GraphError` if any quad fails validation checks.
    pub fn validate_quads(&self, quads: &[Quad]) -> Result<(), GraphError> {
        for q in quads {
            let predicate_uri = q.predicate.as_str();
            if predicate_uri.is_empty() {
                return Err(GraphError::Other(
                    "Invalid predicate rejected by outer membrane".to_string(),
                ));
            }
            if q.subject.to_string().is_empty() {
                return Err(GraphError::Other(
                    "Invalid subject rejected by outer membrane".to_string(),
                ));
            }
        }
        Ok(())
    }
}

/// The Adapter Layer bridges external protocols (like JSON-RPC) into core-native types.
pub struct AdapterLayer;

impl AdapterLayer {
    /// Adapts a JSON-RPC-like request map to an internal `RdfDelta`.
    ///
    /// # Errors
    ///
    /// Returns `GraphError` if the request is invalid or missing required fields.
    pub fn adapt_json_rpc(request: &Value) -> Result<RdfDelta, GraphError> {
        let method = request
            .get("method")
            .and_then(|m| m.as_str())
            .ok_or_else(|| GraphError::Other("Missing JSON-RPC method".to_string()))?;

        if method != "apply_delta" {
            return Err(GraphError::Other(format!("Unsupported method: {}", method)));
        }

        let params = request
            .get("params")
            .ok_or_else(|| GraphError::Other("Missing JSON-RPC params".to_string()))?;

        let additions_arr = params
            .get("additions")
            .and_then(|a| a.as_array())
            .ok_or_else(|| {
                GraphError::Other("Missing or invalid additions in params".to_string())
            })?;

        let deletions_arr = params
            .get("deletions")
            .and_then(|d| d.as_array())
            .ok_or_else(|| {
                GraphError::Other("Missing or invalid deletions in params".to_string())
            })?;

        let mut additions = Vec::new();
        for val in additions_arr {
            let s = val
                .as_str()
                .ok_or_else(|| GraphError::Other("Non-string addition quad".to_string()))?;
            additions.push(s.to_string());
        }

        let mut deletions = Vec::new();
        for val in deletions_arr {
            let s = val
                .as_str()
                .ok_or_else(|| GraphError::Other("Non-string deletion quad".to_string()))?;
            deletions.push(s.to_string());
        }

        Ok(RdfDelta::new(additions, deletions))
    }
}

/// The Projection Layer projects the internal core state or transitions into external formats.
pub struct ProjectionLayer;

impl ProjectionLayer {
    /// Projects the core state into an `OCEL` format.
    pub fn project_state_to_ocel(core: &GenesisCore) -> Result<OCEL, GraphError> {
        EvidenceProjector::extract_ocel(core.graph())
    }

    /// Projects an `OCEL` back into the core state.
    pub fn project_ocel_to_state(core: &GenesisCore, log: &OCEL) -> Result<(), GraphError> {
        EvidenceProjector::project_ocel(core.graph(), log)
    }
}
