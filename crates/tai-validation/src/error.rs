//! Error types for validation operations

use std::fmt;

/// Validation error types
#[derive(Debug, Clone)]
pub enum ValidationError {
    /// SLO threshold violation
    SloViolation {
        /// Metric name
        metric: String,
        /// Actual value
        actual: f64,
        /// Threshold
        threshold: f64,
    },

    /// Compliance control failure
    ComplianceFailure {
        /// Framework type
        framework: String,
        /// Control ID
        control_id: String,
        /// Violation description
        description: String,
    },

    /// Security vulnerability detected
    SecurityVulnerability {
        /// Vulnerability type (SAST, DAST, etc.)
        vuln_type: String,
        /// Severity (Critical, High, Medium, Low)
        severity: String,
        /// Description
        description: String,
    },

    /// SHACL shape violation
    ShapeViolation {
        /// Shape URI
        shape_uri: String,
        /// Violated property
        property: String,
        /// Violation message
        message: String,
    },

    /// Pre-flight check failure
    PreFlightFailure {
        /// Check name
        check_name: String,
        /// Failure reason
        reason: String,
    },

    /// Evidence collection error
    EvidenceError(String),

    /// Coverage analysis error
    CoverageError(String),

    /// Acceptance criteria error
    AcceptanceCriteriaError(String),

    /// Test execution error
    ExecutionError(String),

    /// Configuration error
    ConfigError(String),

    /// IO error
    IoError(String),

    /// RDF/SPARQL processing error
    RdfError(String),

    /// Generic validation error
    Other(String),
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValidationError::SloViolation {
                metric,
                actual,
                threshold,
            } => write!(
                f,
                "SLO violation: {} (actual: {}, threshold: {})",
                metric, actual, threshold
            ),
            ValidationError::ComplianceFailure {
                framework,
                control_id,
                description,
            } => write!(
                f,
                "Compliance failure [{}:{}]: {}",
                framework, control_id, description
            ),
            ValidationError::SecurityVulnerability {
                vuln_type,
                severity,
                description,
            } => write!(
                f,
                "Security vulnerability [{}:{}]: {}",
                vuln_type, severity, description
            ),
            ValidationError::ShapeViolation {
                shape_uri,
                property,
                message,
            } => write!(
                f,
                "SHACL violation [{}:{}]: {}",
                shape_uri, property, message
            ),
            ValidationError::PreFlightFailure { check_name, reason } => {
                write!(f, "Pre-flight check [{}] failed: {}", check_name, reason)
            }
            ValidationError::EvidenceError(e) => write!(f, "Evidence error: {}", e),
            ValidationError::CoverageError(e) => write!(f, "Coverage error: {}", e),
            ValidationError::AcceptanceCriteriaError(e) => {
                write!(f, "Acceptance criteria error: {}", e)
            }
            ValidationError::ExecutionError(e) => write!(f, "Execution error: {}", e),
            ValidationError::ConfigError(e) => write!(f, "Configuration error: {}", e),
            ValidationError::IoError(e) => write!(f, "IO error: {}", e),
            ValidationError::RdfError(e) => write!(f, "RDF error: {}", e),
            ValidationError::Other(e) => write!(f, "Validation error: {}", e),
        }
    }
}

impl std::error::Error for ValidationError {}

impl From<std::io::Error> for ValidationError {
    fn from(err: std::io::Error) -> Self {
        ValidationError::IoError(err.to_string())
    }
}

impl From<serde_json::Error> for ValidationError {
    fn from(err: serde_json::Error) -> Self {
        ValidationError::Other(err.to_string())
    }
}

impl From<String> for ValidationError {
    fn from(err: String) -> Self {
        ValidationError::Other(err)
    }
}

impl From<&str> for ValidationError {
    fn from(err: &str) -> Self {
        ValidationError::Other(err.to_string())
    }
}

/// Result type for validation operations
pub type Result<T> = std::result::Result<T, ValidationError>;
