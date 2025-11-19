//! Core ontology abstractions for xenobiological knowledge systems.
//!
//! This module defines the fundamental traits and structures for representing
//! alien knowledge systems in a way that can be translated to human ontologies.

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

pub mod metadata;
pub mod concept;
pub mod relation;

pub use metadata::OntologyMetadata;
pub use concept::Concept;
pub use relation::Relation;

/// Represents different types of alien cognitive architectures
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CognitiveArchitecture {
    /// Silicon-based, non-temporal logic systems
    Crystalline,
    /// Quantum superposition-based knowledge representation
    Quantum,
    /// Distributed hive-mind collective intelligence
    Collective,
    /// Non-linear temporal reasoning
    Temporal,
    /// Multidimensional hyperspatial logic
    Hyperspatial,
    /// Probabilistic uncertainty-based reasoning
    Probabilistic,
    /// Unknown or hybrid architecture
    Unknown,
}

/// Core trait for xenobiological ontology systems.
///
/// This trait defines the interface that all alien knowledge representation
/// systems must implement to be compatible with the universal translator.
#[async_trait]
pub trait XenoOntology: Send + Sync {
    /// Get the unique identifier for this ontology
    fn id(&self) -> Uuid;

    /// Get metadata about this ontology
    fn metadata(&self) -> &OntologyMetadata;

    /// Get the cognitive architecture type
    fn architecture(&self) -> CognitiveArchitecture;

    /// Query concepts by pattern or identifier
    async fn query_concepts(&self, query: &str) -> Result<Vec<Concept>, OntologyError>;

    /// Get all relations in the ontology
    async fn get_relations(&self) -> Result<Vec<Relation>, OntologyError>;

    /// Add a new concept to the ontology
    async fn add_concept(&mut self, concept: Concept) -> Result<(), OntologyError>;

    /// Add a new relation between concepts
    async fn add_relation(&mut self, relation: Relation) -> Result<(), OntologyError>;

    /// Serialize the ontology to a portable format
    async fn serialize(&self) -> Result<Vec<u8>, OntologyError>;

    /// Deserialize from a portable format
    async fn deserialize(data: &[u8]) -> Result<Self, OntologyError>
    where
        Self: Sized;

    /// Validate the internal consistency of the ontology
    async fn validate(&self) -> Result<ValidationReport, OntologyError>;

    /// Get compatibility score with another ontology (0.0 to 1.0)
    async fn compatibility_score(&self, other: &dyn XenoOntology) -> Result<f64, OntologyError>;
}

/// Error types for ontology operations
#[derive(Debug, thiserror::Error)]
pub enum OntologyError {
    /// Concept not found
    #[error("Concept not found: {0}")]
    ConceptNotFound(String),

    /// Invalid relation
    #[error("Invalid relation: {0}")]
    InvalidRelation(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(String),

    /// Deserialization error
    #[error("Deserialization error: {0}")]
    DeserializationError(String),

    /// Validation error
    #[error("Validation error: {0}")]
    ValidationError(String),

    /// Incompatible architectures
    #[error("Incompatible cognitive architectures: {0} and {1}")]
    IncompatibleArchitectures(String, String),

    /// Translation error
    #[error("Translation error: {0}")]
    TranslationError(String),

    /// Unknown error
    #[error("Unknown error: {0}")]
    Unknown(String),
}

/// Report from ontology validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    /// Whether the ontology is valid
    pub valid: bool,
    /// List of warnings
    pub warnings: Vec<String>,
    /// List of errors
    pub errors: Vec<String>,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl ValidationReport {
    /// Create a new valid report
    #[must_use]
    pub fn valid() -> Self {
        Self {
            valid: true,
            warnings: Vec::new(),
            errors: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Create a new invalid report with errors
    #[must_use]
    pub fn invalid(errors: Vec<String>) -> Self {
        Self {
            valid: false,
            warnings: Vec::new(),
            errors,
            metadata: HashMap::new(),
        }
    }

    /// Add a warning to the report
    pub fn add_warning(&mut self, warning: String) {
        self.warnings.push(warning);
    }

    /// Add an error to the report
    pub fn add_error(&mut self, error: String) {
        self.errors.push(error);
        self.valid = false;
    }
}
