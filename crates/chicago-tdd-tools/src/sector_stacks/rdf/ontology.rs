//! RDF Ontology Data Structures
//!
//! Defines core data structures for workflow ontologies, guards, and hooks.
//! This module is dependency-free; ontology loading is handled in the playground project
//! using the oxigraph ecosystem.

use std::collections::HashMap;

/// Represents a workflow stage in the ontology
#[derive(Debug, Clone)]
pub struct WorkflowStage {
    /// Stage identifier (URI)
    pub id: String,
    /// Stage name
    pub name: String,
    /// Stage number in workflow
    pub stage_number: u32,
    /// Whether stage is deterministic
    pub is_deterministic: bool,
    /// Maximum latency in seconds
    pub max_latency_seconds: u32,
}

/// Represents a guard constraint
#[derive(Debug, Clone)]
pub struct GuardConstraint {
    /// Guard identifier
    pub id: String,
    /// Guard type (Legality, Budget, Chronology, Causality, Recursion)
    pub guard_type: String,
    /// List of constraints
    pub constraints: Vec<String>,
}

/// Represents a knowledge hook
#[derive(Debug, Clone)]
pub struct KnowledgeHook {
    /// Hook identifier
    pub id: String,
    /// Hook name
    pub name: String,
    /// Hook description
    pub description: String,
    /// Input type
    pub input_type: String,
    /// Output type
    pub output_type: String,
}

/// Sector-specific ontology loaded from RDF
#[derive(Debug, Clone)]
pub struct SectorOntology {
    /// Sector name (Academic, Claims, etc.)
    pub sector: String,
    /// Workflow stages
    pub stages: HashMap<String, WorkflowStage>,
    /// Guard constraints
    pub guards: HashMap<String, GuardConstraint>,
    /// Knowledge hooks
    pub hooks: HashMap<String, KnowledgeHook>,
    /// Raw RDF triples (for advanced querying)
    pub triples: Vec<(String, String, String)>,
}

impl SectorOntology {
    /// Create a new empty ontology
    #[must_use]
    pub fn new(sector: String) -> Self {
        Self {
            sector,
            stages: HashMap::new(),
            guards: HashMap::new(),
            hooks: HashMap::new(),
            triples: Vec::new(),
        }
    }

    /// Add a workflow stage
    pub fn add_stage(&mut self, stage: WorkflowStage) {
        self.stages.insert(stage.id.clone(), stage);
    }

    /// Add a guard constraint
    pub fn add_guard(&mut self, guard: GuardConstraint) {
        self.guards.insert(guard.id.clone(), guard);
    }

    /// Add a knowledge hook
    pub fn add_hook(&mut self, hook: KnowledgeHook) {
        self.hooks.insert(hook.id.clone(), hook);
    }

    /// Get stage by ID
    #[must_use]
    pub fn get_stage(&self, id: &str) -> Option<&WorkflowStage> {
        self.stages.get(id)
    }

    /// Get all deterministic stages
    #[must_use]
    pub fn deterministic_stages(&self) -> Vec<&WorkflowStage> {
        self.stages.values().filter(|s| s.is_deterministic).collect()
    }

    /// Count total stages
    #[must_use]
    pub fn stage_count(&self) -> usize {
        self.stages.len()
    }

    /// Count total guards
    #[must_use]
    pub fn guard_count(&self) -> usize {
        self.guards.len()
    }

    /// Count total hooks
    #[must_use]
    pub fn hook_count(&self) -> usize {
        self.hooks.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ontology_creation() {
        let ontology = SectorOntology::new("Academic".to_string());
        assert_eq!(ontology.sector, "Academic");
        assert_eq!(ontology.stage_count(), 0);
    }

    #[test]
    fn test_add_stage() {
        let mut ontology = SectorOntology::new("Claims".to_string());

        let stage = WorkflowStage {
            id: "validation".to_string(),
            name: "Validation".to_string(),
            stage_number: 1,
            is_deterministic: true,
            max_latency_seconds: 30,
        };

        ontology.add_stage(stage.clone());
        assert_eq!(ontology.stage_count(), 1);
        assert_eq!(ontology.get_stage("validation").unwrap().name, "Validation");
    }

    #[test]
    fn test_deterministic_stages() {
        let mut ontology = SectorOntology::new("Academic".to_string());

        ontology.add_stage(WorkflowStage {
            id: "stage1".to_string(),
            name: "Stage 1".to_string(),
            stage_number: 1,
            is_deterministic: true,
            max_latency_seconds: 60,
        });

        ontology.add_stage(WorkflowStage {
            id: "stage2".to_string(),
            name: "Stage 2".to_string(),
            stage_number: 2,
            is_deterministic: false,
            max_latency_seconds: 120,
        });

        let deterministic = ontology.deterministic_stages();
        assert_eq!(deterministic.len(), 1);
    }

    #[test]
    fn test_add_guard() {
        let mut ontology = SectorOntology::new("Claims".to_string());

        let guard = GuardConstraint {
            id: "budget".to_string(),
            guard_type: "Budget".to_string(),
            constraints: vec!["settlement <= policy_limit".to_string()],
        };

        ontology.add_guard(guard);
        assert_eq!(ontology.guard_count(), 1);
    }

    #[test]
    fn test_add_hook() {
        let mut ontology = SectorOntology::new("Academic".to_string());

        let hook = KnowledgeHook {
            id: "desk_review".to_string(),
            name: "Desk Review".to_string(),
            description: "Initial desk review of paper".to_string(),
            input_type: "PaperSubmission".to_string(),
            output_type: "ReviewResult".to_string(),
        };

        ontology.add_hook(hook);
        assert_eq!(ontology.hook_count(), 1);
    }
}
