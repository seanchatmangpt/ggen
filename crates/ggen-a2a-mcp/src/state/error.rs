//! Error types for TOGAF state management.

use thiserror::Error;

/// Errors that can occur during TOGAF state management operations.
#[derive(Error, Debug)]
pub enum StateError {
    /// Turn number is out of the valid range.
    #[error("Invalid turn {0}: out of range 1-{1}")]
    InvalidTurn(usize, usize),

    /// Attempted to advance past the final turn.
    #[error("Cannot advance past turn {0}")]
    TurnExhausted(usize),

    /// Phase is in a status that does not permit the requested operation.
    #[error("Phase {0} is {1}, cannot advance")]
    PhaseNotInProgress(String, String),

    /// ARB gate has not been approved for the given turn.
    #[error("ARB gate at turn {0} not approved")]
    ArbGateNotApproved(usize),

    /// Phase handoff validation failed.
    #[error("Handoff from {0} to {1} failed: {2}")]
    HandoffFailed(String, String, String),

    /// Requested artifact not found.
    #[error("Artifact {0} not found")]
    ArtifactNotFound(String),

    /// Lock was poisoned (a writer panicked while holding the lock).
    #[error("Lock poisoned: {0}")]
    LockPoisoned(String),

    /// Approval response from an unknown or unexpected stakeholder.
    #[error("Unexpected reviewer for gate at turn {0}: {1}")]
    UnexpectedReviewer(usize, String),

    /// Duplicate approval response from the same stakeholder.
    #[error("Duplicate response from {0} for gate at turn {1}")]
    DuplicateResponse(String, usize),

    /// Gate not found at the specified turn.
    #[error("No ARB gate defined at turn {0}")]
    GateNotFound(usize),
}

/// Result type alias for state management operations.
pub type StateResult<T> = std::result::Result<T, StateError>;
