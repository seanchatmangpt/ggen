//! Lifecycle validation module
//!
//! This module provides validation functionality for lifecycle operations.
//! Validates production readiness for lifecycle operations.

use super::state::LifecycleState;
use super::state_validation::ValidatedLifecycleState;
use serde::{Deserialize, Serialize};

/// Validation severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValidationSeverity {
    Error,
    Warning,
    Info,
}

/// A single validation issue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationIssue {
    pub severity: ValidationSeverity,
    pub message: String,
    pub field: Option<String>,
}

/// Validation result containing multiple issues
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub issues: Vec<ValidationIssue>,
    pub is_valid: bool,
}

impl ValidationResult {
    pub fn new() -> Self {
        Self {
            issues: Vec::new(),
            is_valid: true,
        }
    }

    pub fn add_issue(&mut self, severity: ValidationSeverity, message: String) {
        self.issues.push(ValidationIssue {
            severity,
            message,
            field: None,
        });
        if severity == ValidationSeverity::Error {
            self.is_valid = false;
        }
    }

    pub fn add_issue_with_field(
        &mut self, severity: ValidationSeverity, message: String, field: String,
    ) {
        self.issues.push(ValidationIssue {
            severity,
            message,
            field: Some(field),
        });
        if severity == ValidationSeverity::Error {
            self.is_valid = false;
        }
    }
}

impl Default for ValidationResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Validator for production readiness
pub struct ReadinessValidator;

impl ReadinessValidator {
    pub fn new() -> Self {
        Self
    }

    /// Validate lifecycle state for production readiness
    ///
    /// Performs basic validation checks:
    /// - State structure validity (using ValidatedLifecycleState)
    /// - Critical phase prerequisites (deploy requires test)
    /// - Cache key validity
    pub fn validate(&self, state: &LifecycleState) -> ValidationResult {
        let mut result = ValidationResult::new();

        // Validate state structure using ValidatedLifecycleState
        match ValidatedLifecycleState::new(state.clone()) {
            Ok(_) => {
                // State structure is valid
            }
            Err(e) => {
                result.add_issue(
                    ValidationSeverity::Error,
                    format!("Lifecycle state validation failed: {}", e),
                );
                return result;
            }
        }

        // Check critical production readiness requirements
        let completed_phases: std::collections::HashSet<&str> = state
            .phase_history
            .iter()
            .filter(|r| r.success)
            .map(|r| r.phase.as_str())
            .collect();

        // Critical: deploy phase requires test phase to have completed
        if completed_phases.contains("deploy") && !completed_phases.contains("test") {
            result.add_issue(
                ValidationSeverity::Error,
                "Deploy phase executed without test phase completion".to_string(),
            );
        }

        // Warning: check for phases that should typically run before deploy
        if completed_phases.contains("deploy") {
            let recommended_phases = ["build", "test"];
            for phase in recommended_phases {
                if !completed_phases.contains(phase) {
                    result.add_issue(
                        ValidationSeverity::Warning,
                        format!("Deploy phase executed without {} phase completion", phase),
                    );
                }
            }
        }

        // Validate cache keys reference valid phases
        for cache_key in &state.cache_keys {
            if !completed_phases.contains(cache_key.phase.as_str()) {
                result.add_issue_with_field(
                    ValidationSeverity::Error,
                    format!(
                        "Cache key references non-existent phase: {}",
                        cache_key.phase
                    ),
                    cache_key.phase.clone(),
                );
            }
        }

        result
    }
}

impl Default for ReadinessValidator {
    fn default() -> Self {
        Self::new()
    }
}
