//! Error types for cleanroom testing framework.

use thiserror::Error;

/// Result type alias using CleanroomError.
pub type Result<T> = std::result::Result<T, CleanroomError>;

/// Top-level error type for the cleanroom crate.
#[derive(Debug, Error)]
pub enum CleanroomError {
    /// Backend-related errors (engine detection, execution).
    #[error("backend error: {0}")]
    Backend(#[from] BackendError),

    /// Policy violations or constraint failures.
    #[error("policy violation: {0}")]
    Policy(#[from] PolicyError),

    /// Coverage collection or merge errors.
    #[error("coverage error: {0}")]
    Coverage(#[from] CoverageError),

    /// Scenario execution errors.
    #[error("scenario error: {0}")]
    Scenario(#[from] ScenarioError),

    /// Service fixture errors.
    #[error("service error: {0}")]
    Service(#[from] ServiceError),

    /// Standard I/O errors.
    #[error("io: {0}")]
    Io(#[from] std::io::Error),

    /// System time errors.
    #[error("time: {0}")]
    Time(#[from] std::time::SystemTimeError),
}

/// Errors related to backend execution (Docker, Podman, local).
#[derive(Debug, Error)]
pub enum BackendError {
    /// Container engine (Docker/Podman) not found on host.
    #[error("engine not found: {engine}")]
    EngineNotFound {
        /// The engine name that was not found
        engine: String,
    },

    /// Command exited with non-zero status.
    #[error("nonzero exit ({code}): {msg}")]
    NonZero {
        /// The exit code
        code: i32,
        /// The error message
        msg: String,
    },

    /// Image pull or validation failed.
    #[error("image error: {0}")]
    ImageError(String),

    /// Backend detection failed.
    #[error("backend detection failed: {0}")]
    DetectionFailed(String),

    /// Runtime execution error.
    #[error("runtime error: {0}")]
    Runtime(String),
}

/// Errors related to policy enforcement.
#[derive(Debug, Error)]
pub enum PolicyError {
    /// Generic policy violation.
    #[error("{0}")]
    Violation(String),

    /// Network policy violation.
    #[error("network policy violation: {0}")]
    NetworkViolation(String),

    /// Security constraint violation.
    #[error("security violation: {0}")]
    SecurityViolation(String),
}

impl PolicyError {
    /// Create a security violation error
    pub fn security(msg: impl Into<String>) -> Self {
        Self::SecurityViolation(msg.into())
    }
}

/// Errors related to coverage collection and merging.
#[derive(Debug, Error)]
pub enum CoverageError {
    /// Generic coverage error.
    #[error("{0}")]
    Msg(String),

    /// Path remapping failed.
    #[error("path remap failed: {0}")]
    RemapFailed(String),

    /// Coverage merge failed.
    #[error("merge failed: {0}")]
    MergeFailed(String),
}

/// Errors related to scenario execution.
#[derive(Debug, Error)]
pub enum ScenarioError {
    /// Generic scenario error.
    #[error("{0}")]
    Msg(String),

    /// Assertion failed.
    #[error("assertion failed: {0}")]
    AssertionFailed(String),

    /// Step execution failed.
    #[error("step failed: {0}")]
    StepFailed(String),
}

/// Errors related to service fixtures.
#[derive(Debug, Error)]
pub enum ServiceError {
    /// Generic service error.
    #[error("{0}")]
    Msg(String),

    /// Service health check failed.
    #[error("health check failed: {0}")]
    HealthCheckFailed(String),

    /// Service startup timeout.
    #[error("startup timeout: {0}")]
    StartupTimeout(String),
}
