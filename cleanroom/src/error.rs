//! Error types for the cleanroom crate

use thiserror::Error;

/// Cleanroom error type
#[derive(Error, Debug)]
pub enum CleanroomError {
    /// Backend-related errors
    #[error("Backend error: {0}")]
    Backend(#[from] BackendError),

    /// Policy violation errors
    #[error("Policy error: {0}")]
    Policy(#[from] PolicyError),

    /// Coverage-related errors
    #[error("Coverage error: {0}")]
    Coverage(#[from] CoverageError),

    /// Scenario execution errors
    #[error("Scenario error: {0}")]
    Scenario(#[from] ScenarioError),

    /// Service fixture errors
    #[error("Service error: {0}")]
    Service(#[from] ServiceError),

    /// IO errors
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Result type alias for cleanroom operations
pub type Result<T> = std::result::Result<T, CleanroomError>;

/// Backend execution errors
#[derive(Error, Debug)]
pub enum BackendError {
    #[error("Runtime error: {0}")]
    Runtime(String),

    #[error("Command not found: {0}")]
    CommandNotFound(String),

    #[error("Image pull failed: {0}")]
    ImagePullFailed(String),

    #[error("Container spawn failed: {0}")]
    SpawnFailed(String),
}

/// Policy enforcement errors
#[derive(Error, Debug)]
pub enum PolicyError {
    #[error("Security violation: {0}")]
    SecurityViolation(String),

    #[error("Resource limit exceeded: {0}")]
    ResourceLimitExceeded(String),

    #[error("Policy configuration error: {0}")]
    ConfigurationError(String),
}

/// Coverage collection errors
#[derive(Error, Debug)]
pub enum CoverageError {
    #[error("Coverage collection failed: {0}")]
    CollectionFailed(String),

    #[error("Coverage merge failed: {0}")]
    MergeFailed(String),

    #[error("Coverage format error: {0}")]
    FormatError(String),
}

/// Scenario execution errors
#[derive(Error, Debug)]
pub enum ScenarioError {
    #[error("Step execution failed: {0}")]
    StepFailed(String),

    #[error("Assertion failed: {0}")]
    AssertionFailed(String),

    #[error("Timeout exceeded: {0}")]
    Timeout(String),
}

/// Service fixture errors
#[derive(Error, Debug)]
pub enum ServiceError {
    #[error("Service startup failed: {0}")]
    StartupFailed(String),

    #[error("Service health check failed: {0}")]
    HealthCheckFailed(String),

    #[error("Service teardown failed: {0}")]
    TeardownFailed(String),

    #[error("Service error: {0}")]
    Msg(String),

    #[error("Service startup timeout: {0}")]
    StartupTimeout(String),
}