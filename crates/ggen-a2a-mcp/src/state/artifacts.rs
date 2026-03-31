//! Artifact Registry - Storage and retrieval of TOGAF artifacts.
//!
//! Artifacts are the deliverables produced at each turn of the protocol.
//! They are indexed by ID, turn number, and phase for efficient retrieval.

use std::collections::{HashMap, HashSet};

use chrono::Utc;
use serde::{Deserialize, Serialize};

use super::togaf_state::TogafPhase;

// ---------------------------------------------------------------------------
// ArtifactType
// ---------------------------------------------------------------------------

/// Classification of an artifact within the TOGAF ADM.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArtifactType {
    /// Stakeholder map and analysis.
    StakeholderMap,
    /// Architecture vision statement.
    ArchitectureVision,
    /// Business capability map.
    BusinessCapabilityMap,
    /// Data entity catalog.
    DataEntityCatalog,
    /// Technology catalog.
    TechnologyCatalog,
    /// Implementation strategy document.
    ImplementationStrategy,
    /// Migration plan.
    MigrationPlan,
    /// FIBO semantic validation result.
    FiboValidation,
    /// ARB approval record.
    ArbApproval,
    /// Phase handoff package.
    HandoffPackage,
    /// Any other artifact type.
    Other(String),
}

impl std::fmt::Display for ArtifactType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArtifactType::StakeholderMap => write!(f, "StakeholderMap"),
            ArtifactType::ArchitectureVision => write!(f, "ArchitectureVision"),
            ArtifactType::BusinessCapabilityMap => write!(f, "BusinessCapabilityMap"),
            ArtifactType::DataEntityCatalog => write!(f, "DataEntityCatalog"),
            ArtifactType::TechnologyCatalog => write!(f, "TechnologyCatalog"),
            ArtifactType::ImplementationStrategy => write!(f, "ImplementationStrategy"),
            ArtifactType::MigrationPlan => write!(f, "MigrationPlan"),
            ArtifactType::FiboValidation => write!(f, "FiboValidation"),
            ArtifactType::ArbApproval => write!(f, "ArbApproval"),
            ArtifactType::HandoffPackage => write!(f, "HandoffPackage"),
            ArtifactType::Other(s) => write!(f, "Other({})", s),
        }
    }
}

// ---------------------------------------------------------------------------
// Artifact
// ---------------------------------------------------------------------------

/// A single artifact produced during the protocol.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Artifact {
    /// Unique identifier for this artifact.
    pub id: String,
    /// The turn number at which this artifact was produced.
    pub turn: usize,
    /// The phase this artifact belongs to.
    pub phase: TogafPhase,
    /// Classification of the artifact.
    pub artifact_type: ArtifactType,
    /// Human-readable name.
    pub name: String,
    /// Arbitrary content (JSON value).
    pub content: serde_json::Value,
    /// FIBO concepts referenced by this artifact.
    pub fibo_concepts: Vec<String>,
    /// Creation timestamp.
    pub created_at: chrono::DateTime<chrono::Utc>,
}

impl Artifact {
    /// Create a new artifact with the given parameters.
    pub fn new(
        id: impl Into<String>,
        turn: usize,
        phase: TogafPhase,
        artifact_type: ArtifactType,
        name: impl Into<String>,
        content: serde_json::Value,
    ) -> Self {
        Self {
            id: id.into(),
            turn,
            phase,
            artifact_type,
            name: name.into(),
            content,
            fibo_concepts: Vec::new(),
            created_at: Utc::now(),
        }
    }

    /// Add FIBO concepts to the artifact (builder-style).
    pub fn with_fibo_concepts(mut self, concepts: Vec<String>) -> Self {
        self.fibo_concepts = concepts;
        self
    }

    /// Convenience constructor for tests: creates an artifact with sensible
    /// defaults and a JSON-null body.
    #[cfg(test)]
    pub fn new_test(turn: usize, phase: TogafPhase, name: &str) -> Self {
        Self {
            id: format!("artifact-{}-{}", turn, name.to_lowercase().replace(' ', "-")),
            turn,
            phase,
            artifact_type: ArtifactType::Other(name.to_string()),
            name: name.to_string(),
            content: serde_json::Value::Null,
            fibo_concepts: Vec::new(),
            created_at: Utc::now(),
        }
    }
}

// ---------------------------------------------------------------------------
// ArtifactRegistry
// ---------------------------------------------------------------------------

/// In-memory registry for artifacts with multi-key indexing.
///
/// Artifacts are indexed by:
/// - Primary key: artifact ID
/// - Secondary index: turn number
/// - Secondary index: phase
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ArtifactRegistry {
    /// Primary storage keyed by artifact ID.
    artifacts: HashMap<String, Artifact>,
    /// Secondary index: turn number -> list of artifact IDs.
    by_turn: HashMap<usize, Vec<String>>,
    /// Secondary index: phase -> list of artifact IDs.
    by_phase: HashMap<TogafPhase, Vec<String>>,
}

impl ArtifactRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert an artifact into the registry.
    ///
    /// If an artifact with the same ID already exists, it is replaced.
    pub fn insert(&mut self, artifact: Artifact) {
        let id = artifact.id.clone();
        let turn = artifact.turn;
        let phase = artifact.phase;

        // Remove old entry if replacing.
        if let Some(old) = self.artifacts.remove(&id) {
            self.remove_from_indices(&old);
        }

        self.by_turn.entry(turn).or_default().push(id.clone());
        self.by_phase.entry(phase).or_default().push(id.clone());
        self.artifacts.insert(id, artifact);
    }

    /// Remove an artifact by ID.
    ///
    /// Returns the removed artifact, if it existed.
    pub fn remove(&mut self, id: &str) -> Option<Artifact> {
        let artifact = self.artifacts.remove(id)?;
        self.remove_from_indices(&artifact);
        Some(artifact)
    }

    /// Get a reference to an artifact by ID.
    pub fn get(&self, id: &str) -> Option<&Artifact> {
        self.artifacts.get(id)
    }

    /// Get all artifacts for a specific turn.
    pub fn get_for_turn(&self, turn: usize) -> Vec<&Artifact> {
        self.by_turn
            .get(&turn)
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| self.artifacts.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all artifacts for a specific phase.
    pub fn get_for_phase(&self, phase: TogafPhase) -> Vec<&Artifact> {
        self.by_phase
            .get(&phase)
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| self.artifacts.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all artifacts as a flat vector.
    pub fn all(&self) -> Vec<&Artifact> {
        self.artifacts.values().collect()
    }

    /// Number of artifacts stored.
    pub fn count(&self) -> usize {
        self.artifacts.len()
    }

    /// Check if an artifact with the given ID exists.
    pub fn contains(&self, id: &str) -> bool {
        self.artifacts.contains_key(id)
    }

    /// Get all unique FIBO concepts referenced across all artifacts.
    pub fn all_fibo_concepts(&self) -> HashSet<String> {
        let mut concepts = HashSet::new();
        for artifact in self.artifacts.values() {
            for concept in &artifact.fibo_concepts {
                concepts.insert(concept.clone());
            }
        }
        concepts
    }

    /// Get all artifacts of a specific type.
    pub fn get_by_type(&self, artifact_type: &ArtifactType) -> Vec<&Artifact> {
        self.artifacts
            .values()
            .filter(|a| &a.artifact_type == artifact_type)
            .collect()
    }

    // -- Private helpers --

    fn remove_from_indices(&mut self, artifact: &Artifact) {
        if let Some(ids) = self.by_turn.get_mut(&artifact.turn) {
            ids.retain(|id| id != &artifact.id);
            if ids.is_empty() {
                self.by_turn.remove(&artifact.turn);
            }
        }
        if let Some(ids) = self.by_phase.get_mut(&artifact.phase) {
            ids.retain(|id| id != &artifact.id);
            if ids.is_empty() {
                self.by_phase.remove(&artifact.phase);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_artifact(id: &str, turn: usize, phase: TogafPhase, name: &str) -> Artifact {
        Artifact::new(
            id,
            turn,
            phase,
            ArtifactType::Other(name.to_string()),
            name,
            serde_json::Value::Null,
        )
    }

    #[test]
    fn registry_insert_and_get() {
        let mut reg = ArtifactRegistry::new();
        let artifact = make_artifact("a1", 1, TogafPhase::A, "StakeholderMap");
        reg.insert(artifact);

        assert_eq!(reg.count(), 1);
        assert!(reg.contains("a1"));
        assert!(reg.get("a1").is_some());
        assert!(reg.get("nonexistent").is_none());
    }

    #[test]
    fn registry_index_by_turn() {
        let mut reg = ArtifactRegistry::new();
        reg.insert(make_artifact("a1", 1, TogafPhase::A, "Artifact1"));
        reg.insert(make_artifact("a2", 1, TogafPhase::A, "Artifact2"));
        reg.insert(make_artifact("a3", 2, TogafPhase::A, "Artifact3"));

        let turn1 = reg.get_for_turn(1);
        assert_eq!(turn1.len(), 2);

        let turn2 = reg.get_for_turn(2);
        assert_eq!(turn2.len(), 1);

        let turn3 = reg.get_for_turn(3);
        assert!(turn3.is_empty());
    }

    #[test]
    fn registry_index_by_phase() {
        let mut reg = ArtifactRegistry::new();
        reg.insert(make_artifact("a1", 1, TogafPhase::A, "Artifact1"));
        reg.insert(make_artifact("a2", 2, TogafPhase::A, "Artifact2"));
        reg.insert(make_artifact("a3", 9, TogafPhase::B, "Artifact3"));

        let phase_a = reg.get_for_phase(TogafPhase::A);
        assert_eq!(phase_a.len(), 2);

        let phase_b = reg.get_for_phase(TogafPhase::B);
        assert_eq!(phase_b.len(), 1);

        let phase_c = reg.get_for_phase(TogafPhase::C);
        assert!(phase_c.is_empty());
    }

    #[test]
    fn registry_replace_artifact() {
        let mut reg = ArtifactRegistry::new();
        reg.insert(make_artifact("a1", 1, TogafPhase::A, "V1"));

        // Replace with new version.
        let mut v2 = make_artifact("a1", 1, TogafPhase::A, "V2");
        v2.content = serde_json::json!({"version": 2});
        reg.insert(v2);

        assert_eq!(reg.count(), 1);
        let retrieved = reg.get("a1").unwrap();
        assert_eq!(retrieved.name, "V2");
    }

    #[test]
    fn registry_remove() {
        let mut reg = ArtifactRegistry::new();
        reg.insert(make_artifact("a1", 1, TogafPhase::A, "Artifact1"));
        reg.insert(make_artifact("a2", 2, TogafPhase::B, "Artifact2"));

        let removed = reg.remove("a1");
        assert!(removed.is_some());
        assert_eq!(reg.count(), 1);
        assert!(!reg.contains("a1"));
        assert!(reg.get_for_turn(1).is_empty());
    }

    #[test]
    fn registry_all_and_fibo_concepts() {
        let mut reg = ArtifactRegistry::new();
        let mut a1 = make_artifact("a1", 1, TogafPhase::A, "A1");
        a1.fibo_concepts = vec!["fibo-fnd:LegalPerson".to_string()];
        let mut a2 = make_artifact("a2", 2, TogafPhase::B, "A2");
        a2.fibo_concepts = vec![
            "fibo-fnd:LegalPerson".to_string(),
            "fibo-lcc:LoanContract".to_string(),
        ];

        reg.insert(a1);
        reg.insert(a2);

        assert_eq!(reg.all().len(), 2);
        let concepts = reg.all_fibo_concepts();
        assert_eq!(concepts.len(), 2);
        assert!(concepts.contains("fibo-fnd:LegalPerson"));
        assert!(concepts.contains("fibo-lcc:LoanContract"));
    }

    #[test]
    fn artifact_type_display() {
        assert_eq!(ArtifactType::StakeholderMap.to_string(), "StakeholderMap");
        assert_eq!(
            ArtifactType::Other("CustomType".to_string()).to_string(),
            "Other(CustomType)"
        );
    }
}
