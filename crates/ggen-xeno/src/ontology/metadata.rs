//! Metadata for xenobiological ontologies.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Metadata about an alien ontology.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OntologyMetadata {
    /// Ontology name or identifier
    pub name: String,

    /// Version
    pub version: String,

    /// Origin civilization or species
    pub origin: OriginInfo,

    /// Discovery or first contact information
    pub discovery: DiscoveryInfo,

    /// Language or communication protocol
    pub protocol: ProtocolInfo,

    /// Computational requirements
    pub computational_requirements: ComputationalRequirements,

    /// Additional metadata
    pub custom_fields: HashMap<String, serde_json::Value>,
}

impl OntologyMetadata {
    /// Create new metadata
    #[must_use]
    pub fn new(name: String, origin: OriginInfo) -> Self {
        Self {
            name,
            version: "0.1.0".to_string(),
            origin,
            discovery: DiscoveryInfo::default(),
            protocol: ProtocolInfo::default(),
            computational_requirements: ComputationalRequirements::default(),
            custom_fields: HashMap::new(),
        }
    }
}

/// Information about the origin civilization
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OriginInfo {
    /// Civilization name or designation
    pub civilization: String,

    /// Star system or location
    pub location: Option<String>,

    /// Estimated age (in Earth years)
    pub age_years: Option<u64>,

    /// Biological/physical basis
    pub biological_basis: BiologicalBasis,

    /// Technology level (Kardashev scale approximation)
    pub kardashev_level: Option<f64>,
}

/// Biological or physical basis of the alien intelligence
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BiologicalBasis {
    /// Carbon-based life
    Carbon,
    /// Silicon-based life
    Silicon,
    /// Quantum coherent systems
    Quantum,
    /// Plasma-based entities
    Plasma,
    /// Electromagnetic patterns
    Electromagnetic,
    /// Collective organism
    Collective,
    /// Post-biological AI
    PostBiological,
    /// Unknown
    Unknown,
}

/// Discovery information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DiscoveryInfo {
    /// Discovery date
    pub date: Option<DateTime<Utc>>,

    /// Mission or expedition name
    pub mission: Option<String>,

    /// Discovery method
    pub method: DiscoveryMethod,

    /// Location of discovery
    pub location: Option<String>,

    /// Notes
    pub notes: Option<String>,
}

impl Default for DiscoveryInfo {
    fn default() -> Self {
        Self {
            date: Some(Utc::now()),
            mission: None,
            method: DiscoveryMethod::Speculative,
            location: None,
            notes: None,
        }
    }
}

/// Method of discovery
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiscoveryMethod {
    /// Radio signal
    RadioSignal,
    /// Artifact analysis
    Artifact,
    /// Direct contact
    DirectContact,
    /// Archaeological excavation
    Archaeological,
    /// Speculative/theoretical
    Speculative,
}

/// Communication protocol information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProtocolInfo {
    /// Protocol name
    pub name: String,

    /// Version
    pub version: String,

    /// Encoding method
    pub encoding: String,

    /// Error correction available
    pub error_correction: bool,

    /// Requires quantum channel
    pub requires_quantum: bool,
}

impl Default for ProtocolInfo {
    fn default() -> Self {
        Self {
            name: "Unknown".to_string(),
            version: "0.0.0".to_string(),
            encoding: "Binary".to_string(),
            error_correction: false,
            requires_quantum: false,
        }
    }
}

/// Computational requirements for processing the ontology
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComputationalRequirements {
    /// Minimum RAM (in GB)
    pub min_ram_gb: Option<f64>,

    /// Requires quantum computing
    pub requires_quantum_computing: bool,

    /// Requires parallel processing
    pub requires_parallel: bool,

    /// Estimated processing complexity (Big-O notation)
    pub complexity: Option<String>,
}

impl Default for ComputationalRequirements {
    fn default() -> Self {
        Self {
            min_ram_gb: Some(1.0),
            requires_quantum_computing: false,
            requires_parallel: false,
            complexity: Some("O(n)".to_string()),
        }
    }
}
