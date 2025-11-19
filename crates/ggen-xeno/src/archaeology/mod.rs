//! Interstellar software archaeology tools.
//!
//! Tools for analyzing, decoding, and understanding ancient alien software
//! and computational artifacts discovered during space exploration missions.

use crate::ontology::{CognitiveArchitecture, OntologyError};
use crate::paradigms::ComputationalParadigm;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

pub mod decoder;
pub mod analyzer;
pub mod reconstructor;

/// An alien software artifact discovered during archaeology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Artifact {
    /// Unique identifier
    pub id: Uuid,

    /// Discovery information
    pub discovery: DiscoveryInfo,

    /// Raw data
    pub raw_data: Vec<u8>,

    /// Detected computational paradigm (if any)
    pub paradigm: Option<ComputationalParadigm>,

    /// Detected cognitive architecture
    pub architecture: Option<CognitiveArchitecture>,

    /// Estimated age (in years)
    pub estimated_age: Option<u64>,

    /// Decoding progress (0.0 to 1.0)
    pub decoding_progress: f64,

    /// Extracted metadata
    pub metadata: HashMap<String, String>,

    /// Analysis notes
    pub notes: Vec<String>,
}

/// Information about artifact discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoveryInfo {
    /// Location of discovery
    pub location: String,

    /// Mission or expedition
    pub mission: Option<String>,

    /// Discovery date
    pub date: DateTime<Utc>,

    /// Discoverer(s)
    pub discoverers: Vec<String>,

    /// Context notes
    pub context: Option<String>,
}

impl Artifact {
    /// Create a new artifact
    #[must_use]
    pub fn new(raw_data: Vec<u8>, location: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            discovery: DiscoveryInfo {
                location,
                mission: None,
                date: Utc::now(),
                discoverers: Vec::new(),
                context: None,
            },
            raw_data,
            paradigm: None,
            architecture: None,
            estimated_age: None,
            decoding_progress: 0.0,
            metadata: HashMap::new(),
            notes: Vec::new(),
        }
    }

    /// Add a note to the artifact
    pub fn add_note(&mut self, note: String) {
        self.notes.push(note);
    }

    /// Update decoding progress
    pub fn update_progress(&mut self, progress: f64) {
        self.decoding_progress = progress.clamp(0.0, 1.0);
    }

    /// Get size in bytes
    #[must_use]
    pub fn size(&self) -> usize {
        self.raw_data.len()
    }
}

/// Software archaeology mission
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArchaeologyMission {
    /// Mission identifier
    pub id: Uuid,

    /// Mission name
    pub name: String,

    /// Target location
    pub target: String,

    /// Start date
    pub start_date: DateTime<Utc>,

    /// Status
    pub status: MissionStatus,

    /// Artifacts discovered
    pub artifacts: Vec<Uuid>,

    /// Mission objectives
    pub objectives: Vec<String>,

    /// Findings and conclusions
    pub findings: Vec<String>,
}

/// Mission status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MissionStatus {
    /// Mission planning phase
    Planning,
    /// Mission in progress
    InProgress,
    /// Mission completed successfully
    Completed,
    /// Mission failed or aborted
    Failed,
    /// Mission on hold
    OnHold,
}

impl ArchaeologyMission {
    /// Create a new mission
    #[must_use]
    pub fn new(name: String, target: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            name,
            target,
            start_date: Utc::now(),
            status: MissionStatus::Planning,
            artifacts: Vec::new(),
            objectives: Vec::new(),
            findings: Vec::new(),
        }
    }

    /// Add an objective
    pub fn add_objective(&mut self, objective: String) {
        self.objectives.push(objective);
    }

    /// Record an artifact discovery
    pub fn record_artifact(&mut self, artifact_id: Uuid) {
        self.artifacts.push(artifact_id);
    }

    /// Add a finding
    pub fn add_finding(&mut self, finding: String) {
        self.findings.push(finding);
    }

    /// Update mission status
    pub fn update_status(&mut self, status: MissionStatus) {
        self.status = status;
    }
}

/// Pattern recognition results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternAnalysis {
    /// Detected patterns
    pub patterns: Vec<Pattern>,

    /// Confidence score (0.0 to 1.0)
    pub confidence: f64,

    /// Analysis method used
    pub method: String,
}

/// A detected pattern in alien software
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pattern {
    /// Pattern type
    pub pattern_type: PatternType,

    /// Location in artifact (byte offset)
    pub offset: usize,

    /// Length in bytes
    pub length: usize,

    /// Pattern description
    pub description: String,

    /// Confidence (0.0 to 1.0)
    pub confidence: f64,
}

/// Types of patterns that can be detected
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PatternType {
    /// Repeating sequence
    Repetition,
    /// Header or metadata section
    Header,
    /// Data structure
    DataStructure,
    /// Instruction sequence
    Instructions,
    /// Compression signature
    Compression,
    /// Encryption signature
    Encryption,
    /// Checksum or hash
    Checksum,
    /// Unknown pattern
    Unknown,
}

/// Artifact decoder
pub struct Decoder {
    /// Known encoding schemes
    schemes: HashMap<String, Box<dyn DecodingScheme>>,
}

impl Decoder {
    /// Create a new decoder
    #[must_use]
    pub fn new() -> Self {
        Self {
            schemes: HashMap::new(),
        }
    }

    /// Register a decoding scheme
    pub fn register_scheme(&mut self, name: String, scheme: Box<dyn DecodingScheme>) {
        self.schemes.insert(name, scheme);
    }

    /// Attempt to decode an artifact
    ///
    /// # Errors
    ///
    /// Returns an error if decoding fails
    pub fn decode(&self, artifact: &Artifact) -> Result<DecodedArtifact, OntologyError> {
        // Try each scheme
        for (name, scheme) in &self.schemes {
            if let Ok(decoded) = scheme.decode(&artifact.raw_data) {
                return Ok(DecodedArtifact {
                    original: artifact.clone(),
                    scheme_used: name.clone(),
                    decoded_data: decoded,
                });
            }
        }

        Err(OntologyError::Unknown("No suitable decoding scheme found".to_string()))
    }
}

impl Default for Decoder {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait for decoding schemes
pub trait DecodingScheme: Send + Sync {
    /// Attempt to decode data
    ///
    /// # Errors
    ///
    /// Returns an error if this scheme cannot decode the data
    fn decode(&self, data: &[u8]) -> Result<Vec<u8>, OntologyError>;

    /// Check if this scheme can decode the data
    fn can_decode(&self, data: &[u8]) -> bool;
}

/// Result of successful decoding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecodedArtifact {
    /// Original artifact
    pub original: Artifact,

    /// Decoding scheme used
    pub scheme_used: String,

    /// Decoded data
    pub decoded_data: Vec<u8>,
}
