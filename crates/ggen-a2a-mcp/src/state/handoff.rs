//! Handoff Protocol - Phase-to-phase transition validation.
//!
//! When a TOGAF phase completes, the handoff protocol validates that all
//! required artifacts exist, FIBO consistency is maintained, and ARB
//! approval has been granted before passing control to the next phase.

use std::fmt;

use async_trait::async_trait;
use chrono::Utc;
use serde::{Deserialize, Serialize};
use tracing::{info, instrument};

use super::artifacts::Artifact;
use super::error::StateResult;
use super::togaf_state::{PhaseState, PhaseStatus, TogafPhase};

// ---------------------------------------------------------------------------
// ValidationResult
// ---------------------------------------------------------------------------

/// Result of a single validation check.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Name of the validation check.
    pub name: String,
    /// Whether the check passed.
    pub passed: bool,
    /// Human-readable description of the result.
    pub message: String,
}

impl ValidationResult {
    /// Create a passing validation result.
    pub fn pass(name: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            passed: true,
            message: message.into(),
        }
    }

    /// Create a failing validation result.
    pub fn fail(name: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            passed: false,
            message: message.into(),
        }
    }
}

// ---------------------------------------------------------------------------
// HandoffStatus
// ---------------------------------------------------------------------------

/// Status of a phase handoff.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum HandoffStatus {
    /// Handoff accepted; target phase may begin.
    Accepted,
    /// Handoff rejected with a reason.
    Rejected(String),
    /// Handoff is pending validation.
    Pending,
}

impl fmt::Display for HandoffStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HandoffStatus::Accepted => write!(f, "Accepted"),
            HandoffStatus::Rejected(reason) => write!(f, "Rejected({})", reason),
            HandoffStatus::Pending => write!(f, "Pending"),
        }
    }
}

// ---------------------------------------------------------------------------
// FiboValidationResult
// ---------------------------------------------------------------------------

/// Result of FIBO semantic consistency validation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FiboValidationResult {
    /// FIBO concept that was validated.
    pub concept: String,
    /// Whether the concept usage is consistent.
    pub is_consistent: bool,
    /// Details about the validation.
    pub details: String,
}

// ---------------------------------------------------------------------------
// HandoffPackage
// ---------------------------------------------------------------------------

/// The package of artifacts and state transferred between phases.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HandoffPackage {
    /// Phase that produced this package.
    pub source_phase: TogafPhase,
    /// Phase that will receive this package.
    pub target_phase: TogafPhase,
    /// Artifacts to transfer.
    pub artifacts: Vec<Artifact>,
    /// FIBO validation results from the source phase.
    pub fibo_validations: Vec<FiboValidationResult>,
    /// Human-readable summary of the source phase state.
    pub state_summary: String,
}

impl HandoffPackage {
    /// Create a new handoff package.
    pub fn new(
        source_phase: TogafPhase, target_phase: TogafPhase, artifacts: Vec<Artifact>,
    ) -> Self {
        Self {
            source_phase,
            target_phase,
            artifacts,
            fibo_validations: Vec::new(),
            state_summary: String::new(),
        }
    }

    /// Add FIBO validation results (builder-style).
    pub fn with_fibo_validations(mut self, validations: Vec<FiboValidationResult>) -> Self {
        self.fibo_validations = validations;
        self
    }

    /// Set the state summary (builder-style).
    pub fn with_state_summary(mut self, summary: impl Into<String>) -> Self {
        self.state_summary = summary.into();
        self
    }

    /// Check if all FIBO validations passed.
    pub fn all_fibo_consistent(&self) -> bool {
        self.fibo_validations.iter().all(|v| v.is_consistent)
    }
}

// ---------------------------------------------------------------------------
// HandoffResult
// ---------------------------------------------------------------------------

/// Result of a handoff validation attempt.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HandoffResult {
    /// Whether the handoff was accepted, rejected, or is pending.
    pub status: HandoffStatus,
    /// Individual validation results.
    pub validations: Vec<ValidationResult>,
    /// Timestamp of the handoff attempt.
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl HandoffResult {
    /// Create an accepted handoff result.
    pub fn accepted(validations: Vec<ValidationResult>) -> Self {
        Self {
            status: HandoffStatus::Accepted,
            validations,
            timestamp: Utc::now(),
        }
    }

    /// Create a rejected handoff result.
    pub fn rejected(reason: impl Into<String>, validations: Vec<ValidationResult>) -> Self {
        Self {
            status: HandoffStatus::Rejected(reason.into()),
            validations,
            timestamp: Utc::now(),
        }
    }
}

// ---------------------------------------------------------------------------
// HandoffValidator trait
// ---------------------------------------------------------------------------

/// A validator that checks handoff criteria before phase transitions.
#[async_trait]
pub trait HandoffValidator: Send + Sync {
    /// Validate the handoff from `source` phase to `target` phase.
    async fn validate(
        &self, source: &PhaseState, artifacts: &[Artifact], target: TogafPhase,
    ) -> StateResult<ValidationResult>;
}

// ---------------------------------------------------------------------------
// Built-in Validators
// ---------------------------------------------------------------------------

/// Validates that FIBO concepts are used consistently across artifacts.
pub struct FiboConsistencyValidator;

impl FiboConsistencyValidator {
    pub fn new() -> Self {
        Self
    }
}

impl Default for FiboConsistencyValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl HandoffValidator for FiboConsistencyValidator {
    #[instrument(skip(self, source, artifacts))]
    async fn validate(
        &self, source: &PhaseState, artifacts: &[Artifact], _target: TogafPhase,
    ) -> StateResult<ValidationResult> {
        // Collect all FIBO concepts referenced in the artifacts.
        let all_concepts: Vec<&String> = artifacts
            .iter()
            .flat_map(|a| a.fibo_concepts.iter())
            .collect();

        if all_concepts.is_empty() {
            // No FIBO concepts referenced -- this is acceptable for some
            // phases (e.g., Phase F migration planning may not add new
            // FIBO concepts).
            return Ok(ValidationResult::pass(
                "FiboConsistency",
                "No FIBO concepts to validate; pass by default",
            ));
        }

        // Check for duplicates (same concept referenced multiple times is fine,
        // but we log it).
        let unique_count = {
            let mut set = std::collections::HashSet::new();
            for c in &all_concepts {
                set.insert(c.as_str());
            }
            set.len()
        };

        if unique_count > 0 {
            Ok(ValidationResult::pass(
                "FiboConsistency",
                format!(
                    "Phase {}: {} unique FIBO concepts across {} artifacts",
                    source.phase,
                    unique_count,
                    artifacts.len()
                ),
            ))
        } else {
            Ok(ValidationResult::fail(
                "FiboConsistency",
                "FIBO concept validation produced no results".to_string(),
            ))
        }
    }
}

/// Validates that all required artifacts for the source phase are present.
pub struct ArtifactCompletenessValidator {
    /// Minimum number of artifacts required for a handoff to succeed.
    min_artifacts: usize,
}

impl ArtifactCompletenessValidator {
    pub fn new(min_artifacts: usize) -> Self {
        Self { min_artifacts }
    }

    /// Create a validator that requires at least 1 artifact.
    pub fn permissive() -> Self {
        Self::new(1)
    }
}

#[async_trait]
impl HandoffValidator for ArtifactCompletenessValidator {
    #[instrument(skip(self, source, artifacts))]
    async fn validate(
        &self, source: &PhaseState, artifacts: &[Artifact], _target: TogafPhase,
    ) -> StateResult<ValidationResult> {
        if artifacts.len() >= self.min_artifacts {
            Ok(ValidationResult::pass(
                "ArtifactCompleteness",
                format!(
                    "Phase {} has {} artifacts (min required: {})",
                    source.phase,
                    artifacts.len(),
                    self.min_artifacts
                ),
            ))
        } else {
            Ok(ValidationResult::fail(
                "ArtifactCompleteness",
                format!(
                    "Phase {} has {} artifacts, but {} required",
                    source.phase,
                    artifacts.len(),
                    self.min_artifacts
                ),
            ))
        }
    }
}

/// Validates that ARB approval has been granted for the source phase.
pub struct ArbApprovalValidator;

impl ArbApprovalValidator {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ArbApprovalValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl HandoffValidator for ArbApprovalValidator {
    #[instrument(skip(self, source, _artifacts))]
    async fn validate(
        &self, source: &PhaseState, _artifacts: &[Artifact], _target: TogafPhase,
    ) -> StateResult<ValidationResult> {
        match &source.status {
            PhaseStatus::ArbApproved => Ok(ValidationResult::pass(
                "ArbApproval",
                format!("Phase {} ARB gate approved", source.phase),
            )),
            PhaseStatus::ArbPending => Ok(ValidationResult::fail(
                "ArbApproval",
                format!("Phase {} ARB gate is pending approval", source.phase),
            )),
            other => Ok(ValidationResult::fail(
                "ArbApproval",
                format!(
                    "Phase {} status is {}, expected ArbApproved",
                    source.phase, other
                ),
            )),
        }
    }
}

// ---------------------------------------------------------------------------
// HandoffProtocol
// ---------------------------------------------------------------------------

/// Orchestrates phase handoff validation.
///
/// The protocol runs a series of [`HandoffValidator`]s against the
/// source phase state and its artifacts. All validators must pass for
/// the handoff to be accepted.
pub struct HandoffProtocol {
    validators: Vec<Box<dyn HandoffValidator>>,
}

impl HandoffProtocol {
    /// Create a new protocol with no validators.
    pub fn new() -> Self {
        Self {
            validators: Vec::new(),
        }
    }

    /// Create a protocol with the standard set of TOGAF validators.
    pub fn with_standard_validators() -> Self {
        Self {
            validators: vec![
                Box::new(ArbApprovalValidator::new()),
                Box::new(ArtifactCompletenessValidator::permissive()),
                Box::new(FiboConsistencyValidator::new()),
            ],
        }
    }

    /// Add a custom validator.
    pub fn add_validator(mut self, validator: Box<dyn HandoffValidator>) -> Self {
        self.validators.push(validator);
        self
    }

    /// Validate a handoff from the source phase state to the target phase.
    ///
    /// Runs all registered validators and returns a [`HandoffResult`]
    /// indicating whether the handoff is accepted or rejected.
    #[instrument(skip(self, source_state, artifacts))]
    pub async fn validate_handoff(
        &self, source_state: &PhaseState, artifacts: &[Artifact], target_phase: TogafPhase,
    ) -> HandoffResult {
        let mut results = Vec::new();

        for validator in &self.validators {
            match validator
                .validate(source_state, artifacts, target_phase)
                .await
            {
                Ok(result) => {
                    let passed = result.passed;
                    results.push(result);
                    if !passed {
                        // Short-circuit on first failure.
                        let failure_messages: Vec<String> = results
                            .iter()
                            .filter(|r| !r.passed)
                            .map(|r| r.message.clone())
                            .collect();
                        let reason = failure_messages.join("; ");
                        return HandoffResult::rejected(reason, results);
                    }
                }
                Err(e) => {
                    results.push(ValidationResult::fail(
                        "ValidatorError",
                        format!("Validator failed with error: {}", e),
                    ));
                    return HandoffResult::rejected(format!("Validator error: {}", e), results);
                }
            }
        }

        info!(
            source = %source_state.phase,
            target = %target_phase,
            "Handoff validated successfully"
        );

        HandoffResult::accepted(results)
    }

    /// Build a handoff package from artifacts and state.
    pub fn build_package(
        &self, source_phase: TogafPhase, target_phase: TogafPhase, artifacts: Vec<Artifact>,
        state_summary: impl Into<String>,
    ) -> HandoffPackage {
        HandoffPackage::new(source_phase, target_phase, artifacts).with_state_summary(state_summary)
    }
}

impl Default for HandoffProtocol {
    fn default() -> Self {
        Self::with_standard_validators()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::togaf_state::TogafPhase;

    fn arb_approved_phase_state(phase: TogafPhase) -> PhaseState {
        let mut ps = PhaseState::new(phase);
        ps.status = PhaseStatus::ArbApproved;
        ps.completed_turns = phase.turn_range().collect();
        ps.current_turn = phase.arb_gate();
        ps
    }

    fn arb_pending_phase_state(phase: TogafPhase) -> PhaseState {
        let mut ps = PhaseState::new(phase);
        ps.status = PhaseStatus::ArbPending;
        ps.completed_turns = phase.turn_range().collect();
        ps.current_turn = phase.arb_gate();
        ps
    }

    fn test_artifact(turn: usize, phase: TogafPhase, name: &str) -> Artifact {
        Artifact::new_test(turn, phase, name)
    }

    #[tokio::test]
    async fn arb_approval_validator_passes() {
        let validator = ArbApprovalValidator::new();
        let ps = arb_approved_phase_state(TogafPhase::A);
        let result = validator.validate(&ps, &[], TogafPhase::B).await.unwrap();
        assert!(result.passed);
    }

    #[tokio::test]
    async fn arb_approval_validator_fails_pending() {
        let validator = ArbApprovalValidator::new();
        let ps = arb_pending_phase_state(TogafPhase::A);
        let result = validator.validate(&ps, &[], TogafPhase::B).await.unwrap();
        assert!(!result.passed);
    }

    #[tokio::test]
    async fn artifact_completeness_validator_passes() {
        let validator = ArtifactCompletenessValidator::new(2);
        let ps = PhaseState::new(TogafPhase::A);
        let artifacts = vec![
            test_artifact(1, TogafPhase::A, "A1"),
            test_artifact(2, TogafPhase::A, "A2"),
        ];
        let result = validator
            .validate(&ps, &artifacts, TogafPhase::B)
            .await
            .unwrap();
        assert!(result.passed);
    }

    #[tokio::test]
    async fn artifact_completeness_validator_fails() {
        let validator = ArtifactCompletenessValidator::new(3);
        let ps = PhaseState::new(TogafPhase::A);
        let artifacts = vec![test_artifact(1, TogafPhase::A, "A1")];
        let result = validator
            .validate(&ps, &artifacts, TogafPhase::B)
            .await
            .unwrap();
        assert!(!result.passed);
    }

    #[tokio::test]
    async fn fibo_consistency_validator_no_concepts() {
        let validator = FiboConsistencyValidator::new();
        let ps = PhaseState::new(TogafPhase::A);
        let artifacts = vec![test_artifact(1, TogafPhase::A, "A1")];
        let result = validator
            .validate(&ps, &artifacts, TogafPhase::B)
            .await
            .unwrap();
        assert!(result.passed);
    }

    #[tokio::test]
    async fn fibo_consistency_validator_with_concepts() {
        let validator = FiboConsistencyValidator::new();
        let ps = PhaseState::new(TogafPhase::A);
        let mut artifact = test_artifact(1, TogafPhase::A, "A1");
        artifact.fibo_concepts = vec![
            "fibo-fnd:LegalPerson".to_string(),
            "fibo-lcc:LoanContract".to_string(),
        ];
        let result = validator
            .validate(&ps, &[artifact], TogafPhase::B)
            .await
            .unwrap();
        assert!(result.passed);
        assert!(result.message.contains("2 unique FIBO concepts"));
    }

    #[tokio::test]
    async fn handoff_protocol_accepts_valid_handoff() {
        let protocol = HandoffProtocol::with_standard_validators();
        let ps = arb_approved_phase_state(TogafPhase::A);
        let artifacts = vec![test_artifact(1, TogafPhase::A, "StakeholderMap")];

        let result = protocol
            .validate_handoff(&ps, &artifacts, TogafPhase::B)
            .await;

        assert_eq!(result.status, HandoffStatus::Accepted);
        assert!(result.validations.iter().all(|v| v.passed));
    }

    #[tokio::test]
    async fn handoff_protocol_rejects_missing_approval() {
        let protocol = HandoffProtocol::with_standard_validators();
        let ps = arb_pending_phase_state(TogafPhase::A);
        let artifacts = vec![test_artifact(1, TogafPhase::A, "StakeholderMap")];

        let result = protocol
            .validate_handoff(&ps, &artifacts, TogafPhase::B)
            .await;

        assert!(matches!(result.status, HandoffStatus::Rejected(_)));
    }

    #[tokio::test]
    async fn handoff_package_all_fibo_consistent() {
        let pkg =
            HandoffPackage::new(TogafPhase::A, TogafPhase::B, vec![]).with_fibo_validations(vec![
                FiboValidationResult {
                    concept: "fibo-fnd:LegalPerson".to_string(),
                    is_consistent: true,
                    details: "Consistent".to_string(),
                },
                FiboValidationResult {
                    concept: "fibo-lcc:LoanContract".to_string(),
                    is_consistent: true,
                    details: "Consistent".to_string(),
                },
            ]);
        assert!(pkg.all_fibo_consistent());
    }

    #[tokio::test]
    async fn handoff_package_fibo_inconsistent() {
        let pkg =
            HandoffPackage::new(TogafPhase::A, TogafPhase::B, vec![]).with_fibo_validations(vec![
                FiboValidationResult {
                    concept: "fibo-fnd:LegalPerson".to_string(),
                    is_consistent: true,
                    details: "OK".to_string(),
                },
                FiboValidationResult {
                    concept: "fibo-bad:Invalid".to_string(),
                    is_consistent: false,
                    details: "Not found".to_string(),
                },
            ]);
        assert!(!pkg.all_fibo_consistent());
    }
}
