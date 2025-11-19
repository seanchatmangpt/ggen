//! Relations between concepts in alien ontologies.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Represents a relation between concepts in an alien ontology.
///
/// Relations may have very different semantics across alien cognitive architectures.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Relation {
    /// Unique identifier
    pub id: Uuid,

    /// Relation type
    pub relation_type: RelationType,

    /// Source concept
    pub source: Uuid,

    /// Target concept
    pub target: Uuid,

    /// Relation strength or weight (0.0 to 1.0)
    pub strength: f64,

    /// Directionality
    pub directed: bool,

    /// Human-readable label
    pub label: Option<String>,

    /// Additional properties
    pub properties: HashMap<String, serde_json::Value>,

    /// Temporal validity (for temporal ontologies)
    pub temporal_validity: Option<TemporalValidity>,
}

impl Relation {
    /// Create a new relation
    #[must_use]
    pub fn new(
        relation_type: RelationType,
        source: Uuid,
        target: Uuid,
        strength: f64,
        directed: bool,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            relation_type,
            source,
            target,
            strength: strength.clamp(0.0, 1.0),
            directed,
            label: None,
            properties: HashMap::new(),
            temporal_validity: None,
        }
    }

    /// Set label
    pub fn with_label(mut self, label: String) -> Self {
        self.label = Some(label);
        self
    }

    /// Add property
    pub fn with_property(mut self, key: String, value: serde_json::Value) -> Self {
        self.properties.insert(key, value);
        self
    }
}

/// Types of relations in alien ontologies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RelationType {
    /// Subsumption (is-a)
    Subsumption,
    /// Part-of relationship
    PartOf,
    /// Similarity or analogy
    Similarity,
    /// Causal relationship
    Causation,
    /// Temporal precedence
    Precedence,
    /// Quantum entanglement
    Entanglement,
    /// Resonance (for crystalline systems)
    Resonance,
    /// Collective binding (for hive minds)
    CollectiveBinding,
    /// Hyperspatial adjacency
    HyperspatialAdjacency,
    /// Custom relation
    Custom(String),
}

/// Temporal validity for relations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TemporalValidity {
    /// Start time coordinates
    pub start: Vec<f64>,
    /// End time coordinates (None for infinite)
    pub end: Option<Vec<f64>>,
    /// Probability of validity at any given time
    pub probability: Option<f64>,
}
