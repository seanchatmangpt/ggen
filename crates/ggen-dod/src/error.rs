//! Error types for the DoD system

use thiserror::Error;

/// Result type for DoD operations
pub type DoDResult<T> = Result<T, DoDError>;

/// Comprehensive error type for all DoD subsystems
#[derive(Debug, Error)]
pub enum DoDError {
    #[error("observation error: {0}")]
    Observation(String),

    #[error("observation validation failed: {0}")]
    ObservationValidation(String),

    #[error("contract error: {0}")]
    Contract(String),

    #[error("invariant violation: {0}")]
    InvariantViolation(String),

    #[error("kernel decision error: {0}")]
    KernelDecision(String),

    #[error("kernel timing constraint violated (expected ≤ {expected}ms, took {actual}ms)")]
    TimingViolation { expected: u64, actual: u64 },

    #[error("receipt error: {0}")]
    Receipt(String),

    #[error("receipt verification failed: invalid signature")]
    ReceiptVerificationFailed,

    #[error("decision error: {0}")]
    Decision(String),

    #[error("decision closure violated: {0}")]
    DecisionClosureViolated(String),

    #[error("autonomic loop error: {0}")]
    AutonomicLoop(String),

    #[error("mape-k phase error: {0}")]
    MAPEKPhase(String),

    #[error("doctrine violation: {0}")]
    DoctrineViolation(String),

    #[error("tenant isolation error: {0}")]
    TenantIsolation(String),

    #[error("schema evolution (ΔΣ) rejected: {reason}")]
    SchemaEvolutionRejected { reason: String },

    #[error("proof requirements not met: {0}")]
    ProofRequirementsNotMet(String),

    #[error("idempotence violation: operation produced different results on replay")]
    IdempotenceViolation,

    #[error("determinism violation: same input produced different outputs")]
    DeterminismViolation,

    #[error("closed-world assumption violated: decision depends on external mutable state")]
    ClosedWorldViolation,

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    #[error("serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    #[error("internal error: {0}")]
    Internal(String),
}

/// Errors that can occur during observation schema validation
#[derive(Debug, Error)]
pub enum ObservationValidationError {
    #[error("schema mismatch: {0}")]
    SchemaMismatch(String),

    #[error("missing required field: {0}")]
    MissingRequiredField(String),

    #[error("type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    #[error("constraint violation: {0}")]
    ConstraintViolation(String),

    #[error("size violation: observation exceeds maximum size")]
    SizeViolation,
}

/// Errors that can occur during contract validation
#[derive(Debug, Error)]
pub enum ContractValidationError {
    #[error("schema version mismatch: {0}")]
    VersionMismatch(String),

    #[error("ontology not found: {0}")]
    OntologyNotFound(String),

    #[error("invalid ontology: {0}")]
    InvalidOntology(String),

    #[error("constraint validation failed: {0}")]
    ConstraintValidationFailed(String),
}

/// Errors during timing verification
#[derive(Debug, Error)]
pub enum TimingError {
    #[error("timing constraint exceeded: {0}")]
    ConstraintExceeded(String),

    #[error("timing measurement error: {0}")]
    MeasurementError(String),

    #[error("clock skew detected: {0}")]
    ClockSkew(String),
}

impl DoDError {
    /// Returns true if this error is fatal and should stop execution
    pub fn is_fatal(&self) -> bool {
        matches!(
            self,
            DoDError::DecisionClosureViolated(_)
                | DoDError::DoctrineViolation(_)
                | DoDError::ClosedWorldViolation
                | DoDError::InvariantViolation(_)
                | DoDError::DeterminismViolation
        )
    }

    /// Returns true if this error can be recovered from
    pub fn is_recoverable(&self) -> bool {
        !self.is_fatal()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fatal_errors() {
        let fatal = DoDError::ClosedWorldViolation;
        assert!(fatal.is_fatal());
    }

    #[test]
    fn test_recoverable_errors() {
        let recoverable = DoDError::Observation("test".to_string());
        assert!(recoverable.is_recoverable());
    }
}
