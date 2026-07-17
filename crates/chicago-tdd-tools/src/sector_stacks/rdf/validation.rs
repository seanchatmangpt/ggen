//! RDF-Driven Operation Validation
//!
//! This module closes the loop between RDF ontology definitions and Rust runtime operations
//! by validating sector operations against their ontology specifications.

use super::ontology::{GuardConstraint, SectorOntology};

/// Result of RDF validation
pub type RdfValidationResult = Result<(), RdfValidationError>;

/// Errors that can occur during RDF validation
#[derive(Debug, Clone)]
pub enum RdfValidationError {
    /// Operation not found in ontology
    OperationNotDefined {
        /// Operation name that was not found
        operation: String,
        /// Sector where operation was expected
        sector: String,
    },
    /// Operation violates guard constraint
    GuardViolation {
        /// Guard that was violated
        guard: String,
        /// Operation that violated it
        operation: String,
    },
    /// Invalid stage transition
    InvalidStageTransition {
        /// Source stage number
        from: u32,
        /// Target stage number
        to: u32,
    },
    /// Latency budget exceeded
    LatencyBudgetExceeded {
        /// Stage name
        stage: String,
        /// Actual latency in ms
        actual: u32,
        /// Budgeted latency in ms
        budgeted: u32,
    },
    /// Ontology not loaded
    OntologyNotLoaded,
}

impl std::fmt::Display for RdfValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OperationNotDefined { operation, sector } => {
                write!(f, "Operation '{operation}' not defined in {sector} ontology")
            }
            Self::GuardViolation { guard, operation } => {
                write!(f, "Guard '{guard}' violated by operation '{operation}'")
            }
            Self::InvalidStageTransition { from, to } => {
                write!(f, "Invalid stage transition from {from} to {to}")
            }
            Self::LatencyBudgetExceeded { stage, actual, budgeted } => {
                write!(f, "Stage '{stage}' latency {actual} exceeds budget {budgeted}")
            }
            Self::OntologyNotLoaded => {
                write!(f, "Ontology not loaded")
            }
        }
    }
}

impl std::error::Error for RdfValidationError {}

/// Validates sector operations against RDF ontology definitions
pub struct RdfOperationValidator {
    ontology: Option<SectorOntology>,
}

impl RdfOperationValidator {
    /// Create a new validator
    #[must_use]
    pub const fn new() -> Self {
        Self { ontology: None }
    }

    /// Set the ontology for validation
    #[must_use]
    pub fn with_ontology(mut self, ontology: SectorOntology) -> Self {
        self.ontology = Some(ontology);
        self
    }

    /// Validate that an operation is defined in the ontology
    ///
    /// # Errors
    ///
    /// Returns an error if the ontology is not loaded or the operation is not defined.
    pub fn validate_operation_defined(&self, operation: &str) -> RdfValidationResult {
        let ontology = self.ontology.as_ref().ok_or(RdfValidationError::OntologyNotLoaded)?;

        // Check if operation exists in hooks
        if ontology.hooks.contains_key(operation) {
            Ok(())
        } else {
            Err(RdfValidationError::OperationNotDefined {
                operation: operation.to_string(),
                sector: ontology.sector.clone(),
            })
        }
    }

    /// Validate a stage transition
    ///
    /// # Errors
    ///
    /// Returns an error if the ontology is not loaded, stages are not found, or transition is invalid.
    pub fn validate_stage_transition(
        &self,
        from_stage: &str,
        to_stage: &str,
    ) -> RdfValidationResult {
        let ontology = self.ontology.as_ref().ok_or(RdfValidationError::OntologyNotLoaded)?;

        let from = ontology.get_stage(from_stage).ok_or_else(|| {
            RdfValidationError::OperationNotDefined {
                operation: from_stage.to_string(),
                sector: ontology.sector.clone(),
            }
        })?;

        let to = ontology.get_stage(to_stage).ok_or_else(|| {
            RdfValidationError::OperationNotDefined {
                operation: to_stage.to_string(),
                sector: ontology.sector.clone(),
            }
        })?;

        // Validate stage progression (must be forward or same)
        if to.stage_number >= from.stage_number {
            Ok(())
        } else {
            Err(RdfValidationError::InvalidStageTransition {
                from: from.stage_number,
                to: to.stage_number,
            })
        }
    }

    /// Validate operation latency against budget
    ///
    /// # Errors
    ///
    /// Returns an error if the ontology is not loaded, stage is not found, or latency exceeds budget.
    pub fn validate_latency_budget(&self, stage: &str, latency_ms: u32) -> RdfValidationResult {
        let ontology = self.ontology.as_ref().ok_or(RdfValidationError::OntologyNotLoaded)?;

        let stage_def =
            ontology
                .get_stage(stage)
                .ok_or_else(|| RdfValidationError::OperationNotDefined {
                    operation: stage.to_string(),
                    sector: ontology.sector.clone(),
                })?;

        if latency_ms <= stage_def.max_latency_seconds * 1000 {
            Ok(())
        } else {
            Err(RdfValidationError::LatencyBudgetExceeded {
                stage: stage.to_string(),
                actual: latency_ms,
                budgeted: stage_def.max_latency_seconds * 1000,
            })
        }
    }

    /// Get all guard constraints for validation
    ///
    /// # Errors
    ///
    /// Returns an error if the ontology is not loaded.
    pub fn get_guards(&self) -> Result<Vec<GuardConstraint>, RdfValidationError> {
        let ontology = self.ontology.as_ref().ok_or(RdfValidationError::OntologyNotLoaded)?;

        Ok(ontology.guards.values().cloned().collect())
    }

    /// Check if all stages are deterministic
    ///
    /// # Errors
    ///
    /// Returns an error if the ontology is not loaded.
    pub fn all_stages_deterministic(&self) -> Result<bool, RdfValidationError> {
        let ontology = self.ontology.as_ref().ok_or(RdfValidationError::OntologyNotLoaded)?;

        Ok(ontology.deterministic_stages().len() == ontology.stage_count())
    }
}

impl Default for RdfOperationValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::super::ontology::WorkflowStage;
    use super::*;

    #[test]
    fn test_validator_creation() {
        let validator = RdfOperationValidator::new();
        assert!(validator.ontology.is_none());
    }

    #[test]
    fn test_ontology_not_loaded() {
        let validator = RdfOperationValidator::new();
        let result = validator.validate_operation_defined("test");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_operation_with_ontology() {
        let ontology = SectorOntology::new("Test".to_string());
        let validator = RdfOperationValidator::new().with_ontology(ontology.clone());

        // Should fail because no hooks defined
        let result = validator.validate_operation_defined("test");
        assert!(result.is_err());
    }

    #[test]
    fn test_stage_transition_validation() {
        let ontology = {
            let mut ont = SectorOntology::new("Test".to_string());
            ont.add_stage(WorkflowStage {
                id: "stage1".to_string(),
                name: "Stage 1".to_string(),
                stage_number: 1,
                is_deterministic: true,
                max_latency_seconds: 30,
            });
            ont.add_stage(WorkflowStage {
                id: "stage2".to_string(),
                name: "Stage 2".to_string(),
                stage_number: 2,
                is_deterministic: true,
                max_latency_seconds: 30,
            });
            ont
        };

        let validator = RdfOperationValidator::new().with_ontology(ontology);

        // Forward transition should succeed
        assert!(validator.validate_stage_transition("stage1", "stage2").is_ok());

        // Backward transition should fail
        assert!(validator.validate_stage_transition("stage2", "stage1").is_err());
    }

    #[test]
    fn test_latency_validation() {
        let ontology = {
            let mut ont = SectorOntology::new("Test".to_string());
            ont.add_stage(WorkflowStage {
                id: "fast".to_string(),
                name: "Fast Stage".to_string(),
                stage_number: 1,
                is_deterministic: true,
                max_latency_seconds: 1,
            });
            ont
        };

        let validator = RdfOperationValidator::new().with_ontology(ontology);

        // Within budget: 500ms < 1000ms (1 second)
        assert!(validator.validate_latency_budget("fast", 500).is_ok());

        // Exceeds budget: 2000ms > 1000ms (1 second)
        assert!(validator.validate_latency_budget("fast", 2000).is_err());
    }

    #[test]
    fn test_guard_retrieval() {
        let ontology = {
            let mut ont = SectorOntology::new("Test".to_string());
            ont.add_guard(super::super::ontology::GuardConstraint {
                id: "g1".to_string(),
                guard_type: "Budget".to_string(),
                constraints: vec!["x <= 100".to_string()],
            });
            ont
        };

        let validator = RdfOperationValidator::new().with_ontology(ontology);
        let guards = validator.get_guards().unwrap();
        assert_eq!(guards.len(), 1);
    }

    #[test]
    fn test_determinism_check() {
        let ontology = {
            let mut ont = SectorOntology::new("Test".to_string());
            ont.add_stage(WorkflowStage {
                id: "s1".to_string(),
                name: "Stage".to_string(),
                stage_number: 1,
                is_deterministic: true,
                max_latency_seconds: 30,
            });
            ont
        };

        let validator = RdfOperationValidator::new().with_ontology(ontology);
        assert!(validator.all_stages_deterministic().unwrap());
    }
}
