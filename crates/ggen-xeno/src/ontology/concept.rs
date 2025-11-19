//! Concept representation for alien knowledge systems.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Represents a concept in an alien ontology.
///
/// Concepts are the fundamental units of knowledge that may have
/// very different representations across alien cognitive architectures.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Concept {
    /// Unique identifier
    pub id: Uuid,

    /// Concept identifier (may be non-human readable)
    pub identifier: String,

    /// Human-readable label (if translatable)
    pub label: Option<String>,

    /// Description in human terms (if translatable)
    pub description: Option<String>,

    /// Native representation (architecture-specific)
    pub native_form: Vec<u8>,

    /// Concept type
    pub concept_type: ConceptType,

    /// Additional properties
    pub properties: HashMap<String, serde_json::Value>,

    /// Temporal markers (for temporal ontologies)
    pub temporal_markers: Vec<TemporalMarker>,

    /// Quantum state (for quantum ontologies)
    pub quantum_state: Option<QuantumState>,
}

impl Concept {
    /// Create a new concept
    #[must_use]
    pub fn new(identifier: String, native_form: Vec<u8>, concept_type: ConceptType) -> Self {
        Self {
            id: Uuid::new_v4(),
            identifier,
            label: None,
            description: None,
            native_form,
            concept_type,
            properties: HashMap::new(),
            temporal_markers: Vec::new(),
            quantum_state: None,
        }
    }

    /// Set human-readable label
    pub fn with_label(mut self, label: String) -> Self {
        self.label = Some(label);
        self
    }

    /// Set description
    pub fn with_description(mut self, description: String) -> Self {
        self.description = Some(description);
        self
    }

    /// Add a property
    pub fn with_property(mut self, key: String, value: serde_json::Value) -> Self {
        self.properties.insert(key, value);
        self
    }
}

/// Types of concepts in alien ontologies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ConceptType {
    /// Entity (individual, object, being)
    Entity,
    /// Process or action
    Process,
    /// Property or attribute
    Property,
    /// Relation
    Relation,
    /// Abstract concept
    Abstract,
    /// Temporal event
    TemporalEvent,
    /// Quantum superposition
    QuantumSuperposition,
    /// Collective consciousness node
    CollectiveNode,
    /// Crystalline resonance pattern
    CrystallinePattern,
    /// Unknown type
    Unknown,
}

/// Temporal marker for non-linear time concepts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TemporalMarker {
    /// Time coordinate (may be multidimensional)
    pub coordinates: Vec<f64>,
    /// Causality vector
    pub causality: Option<Vec<f64>>,
    /// Temporal uncertainty
    pub uncertainty: Option<f64>,
}

/// Quantum state representation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct QuantumState {
    /// Superposition states with amplitudes
    pub states: Vec<(String, f64)>,
    /// Entanglement references
    pub entangled_with: Vec<Uuid>,
    /// Measurement basis
    pub basis: Option<String>,
}
