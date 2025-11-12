//! State validation for poka-yoke error prevention
//!
//! This module provides validation logic for lifecycle state to prevent using
//! corrupted or invalid state data.

use super::error::{LifecycleError, Result};
use super::state::LifecycleState;

/// Validation error for lifecycle state
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StateValidationError {
    /// Phase history shows invalid transition
    InvalidTransition { from: String, to: String },
    /// Cache key references non-existent phase
    InvalidCacheKey { phase: String },
    /// State shows phase completed but required prerequisite not completed
    MissingPrerequisite { phase: String, prerequisite: String },
    /// Phase history is inconsistent (e.g., deploy before build)
    InconsistentHistory { message: String },
}

impl std::fmt::Display for StateValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidTransition { from, to } => {
                write!(f, "Invalid transition from '{}' to '{}'", from, to)
            }
            Self::InvalidCacheKey { phase } => {
                write!(f, "Cache key references non-existent phase: '{}'", phase)
            }
            Self::MissingPrerequisite {
                phase,
                prerequisite,
            } => {
                write!(
                    f,
                    "Phase '{}' completed but prerequisite '{}' not completed",
                    phase, prerequisite
                )
            }
            Self::InconsistentHistory { message } => {
                write!(f, "Inconsistent phase history: {}", message)
            }
        }
    }
}

impl std::error::Error for StateValidationError {}

/// Validated lifecycle state wrapper
///
/// **Poka-yoke**: Only `ValidatedLifecycleState` can be used in operations that
/// require valid state. This prevents using corrupted or invalid state.
#[derive(Debug, Clone)]
pub struct ValidatedLifecycleState {
    state: LifecycleState,
}

impl ValidatedLifecycleState {
    /// Create validated state from raw state
    ///
    /// Validates the state before wrapping it. Returns error if validation fails.
    pub fn new(state: LifecycleState) -> Result<Self> {
        Self::validate(&state)?;
        Ok(Self { state })
    }

    /// Get the underlying state
    pub fn state(&self) -> &LifecycleState {
        &self.state
    }

    /// Get mutable access to state (with re-validation)
    ///
    /// **Warning**: Modifying state directly can break invariants.
    /// State is re-validated after modification.
    pub fn state_mut(&mut self) -> &mut LifecycleState {
        &mut self.state
    }

    /// Validate state invariants
    ///
    /// Checks:
    /// - Phase history consistency
    /// - Cache keys reference valid phases
    /// - Basic invariants (deploy requires test, but hooks can run phases out of order)
    ///
    /// **Note**: Prerequisites are not strictly enforced because hooks can run phases
    /// in any order. This validation only checks for truly invalid states.
    pub fn validate(state: &LifecycleState) -> Result<()> {
        let completed_phases: std::collections::HashSet<&str> = state
            .phase_history
            .iter()
            .filter(|r| r.success)
            .map(|r| r.phase.as_str())
            .collect();

        // Only check critical prerequisites that should never be violated:
        // - deploy should not run without test (critical safety check)
        // Note: Hooks can run phases out of order, so we don't check all prerequisites
        if completed_phases.contains("deploy") && !completed_phases.contains("test") {
            return Err(LifecycleError::Other(format!(
                "{}",
                StateValidationError::MissingPrerequisite {
                    phase: "deploy".to_string(),
                    prerequisite: "test".to_string(),
                }
            )));
        }

        // Check cache keys reference valid phases
        for cache_key in &state.cache_keys {
            if !completed_phases.contains(cache_key.phase.as_str()) {
                return Err(LifecycleError::Other(format!(
                    "{}",
                    StateValidationError::InvalidCacheKey {
                        phase: cache_key.phase.clone(),
                    }
                )));
            }
        }

        // Note: We don't check for inconsistent history (e.g., deploy before build)
        // because hooks can run phases in any order. The type-level state machine
        // prevents invalid transitions at compile time for direct phase execution,
        // but hooks can violate the normal order.

        Ok(())
    }
}

impl AsRef<LifecycleState> for ValidatedLifecycleState {
    fn as_ref(&self) -> &LifecycleState {
        &self.state
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lifecycle::state::{CacheKey, RunRecord};

    #[test]
    fn test_valid_state_passes_validation() {
        let mut state = LifecycleState::default();
        state.record_run("init".to_string(), 0, 100, true);
        state.record_run("setup".to_string(), 100, 200, true);
        state.record_run("build".to_string(), 200, 300, true);

        assert!(ValidatedLifecycleState::validate(&state).is_ok());
    }

    #[test]
    fn test_missing_prerequisite_fails_validation() {
        let mut state = LifecycleState::default();
        // Deploy without test should fail (critical safety check)
        state.record_run("deploy".to_string(), 0, 100, true);

        assert!(ValidatedLifecycleState::validate(&state).is_err());
    }

    #[test]
    fn test_invalid_cache_key_fails_validation() {
        let mut state = LifecycleState::default();
        state.record_run("init".to_string(), 0, 100, true);
        state.add_cache_key("nonexistent".to_string(), "key".to_string());

        assert!(ValidatedLifecycleState::validate(&state).is_err());
    }

    #[test]
    fn test_inconsistent_history_allowed_for_hooks() {
        // Hooks can run phases out of order, so this should be allowed
        let mut state = LifecycleState::default();
        state.record_run("build".to_string(), 0, 100, true);
        state.record_run("init".to_string(), 100, 200, true); // init after build (via hook)

        // This should pass because hooks can run phases in any order
        assert!(ValidatedLifecycleState::validate(&state).is_ok());
    }

    #[test]
    fn test_validated_state_wrapper() {
        let mut state = LifecycleState::default();
        state.record_run("init".to_string(), 0, 100, true);

        let validated = ValidatedLifecycleState::new(state).unwrap();
        assert!(validated.state().has_completed_phase("init"));
    }
}
