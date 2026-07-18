//! Error handling with helpful suggestions and recovery paths

use std::fmt;
use colored::Colorize;

/// Validation errors with helpful context
#[derive(Debug, Clone)]
pub enum ValidationError {
    /// Input validation failed
    InputValidation {
        /// The invalid input
        input: String,
        /// Why it's invalid
        reason: String,
        /// Suggested fix
        suggestion: String,
    },

    /// Validation approach failed
    ApproachFailed {
        /// Which approach failed
        approach: String,
        /// Error details
        details: String,
        /// Suggested fix
        suggestion: String,
    },

    /// Timeout during validation
    Timeout {
        /// Which operation timed out
        operation: String,
        /// Timeout duration
        duration_secs: u64,
        /// Suggested fix
        suggestion: String,
    },

    /// Resource exhaustion
    ResourceExhausted {
        /// Which resource
        resource: String,
        /// Suggested fix
        suggestion: String,
    },

    /// Multiple validation failures
    MultipleFailures {
        /// All failures that occurred
        failures: Vec<ValidationError>,
    },
}

// Make ValidationError thread-safe
unsafe impl Send for ValidationError {}
unsafe impl Sync for ValidationError {}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InputValidation { input, reason, .. } => {
                write!(f, "Invalid input: '{}'. Reason: {}", input, reason)
            }
            Self::ApproachFailed { approach, details, .. } => {
                write!(f, "Validation approach '{}' failed: {}", approach, details)
            }
            Self::Timeout { operation, duration_secs, .. } => {
                write!(
                    f,
                    "Operation '{}' timed out after {}s",
                    operation, duration_secs
                )
            }
            Self::ResourceExhausted { resource, .. } => {
                write!(f, "Resource exhausted: {}", resource)
            }
            Self::MultipleFailures { failures } => {
                write!(
                    f,
                    "Multiple failures occurred ({} total)",
                    failures.len()
                )
            }
        }
    }
}

impl std::error::Error for ValidationError {}

impl ValidationError {
    /// Get suggestion for fixing the error
    pub fn suggestion(&self) -> String {
        match self {
            Self::InputValidation { suggestion, .. } => suggestion.clone(),
            Self::ApproachFailed { suggestion, .. } => suggestion.clone(),
            Self::Timeout { suggestion, .. } => suggestion.clone(),
            Self::ResourceExhausted { suggestion, .. } => suggestion.clone(),
            Self::MultipleFailures { .. } => {
                "Fix the individual failures shown above".to_string()
            }
        }
    }

    /// Get severity level
    pub fn severity(&self) -> ErrorSeverity {
        match self {
            Self::InputValidation { .. } => ErrorSeverity::Warning,
            Self::ApproachFailed { .. } => ErrorSeverity::Error,
            Self::Timeout { .. } => ErrorSeverity::Warning,
            Self::ResourceExhausted { .. } => ErrorSeverity::Error,
            Self::MultipleFailures { .. } => ErrorSeverity::Error,
        }
    }

    /// Check if error is recoverable
    pub fn is_recoverable(&self) -> bool {
        !matches!(self, Self::ResourceExhausted { .. })
    }
}

/// Error severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

impl ErrorSeverity {
    /// Get color for terminal output
    pub fn color(&self) -> colored::Color {
        match self {
            ErrorSeverity::Info => colored::Color::Blue,
            ErrorSeverity::Warning => colored::Color::Yellow,
            ErrorSeverity::Error => colored::Color::Red,
            ErrorSeverity::Critical => colored::Color::Magenta,
        }
    }
}

/// Result of validation
#[derive(Debug, Clone, serde::Serialize)]
pub struct ValidationResult {
    /// Which approach was used
    pub approach: String,

    /// Whether validation passed
    pub passed: bool,

    /// Validation score (0.0 - 1.0)
    pub score: f64,

    /// Detailed results
    pub details: serde_json::Value,

    /// Recommendations for improvement
    pub recommendations: Vec<String>,

    /// Time taken
    pub duration_ms: u64,
}

// Make ValidationResult thread-safe
unsafe impl Send for ValidationResult {}
unsafe impl Sync for ValidationResult {}

impl ValidationResult {
    /// Create a new validation result
    pub fn new(
        approach: &str,
        passed: bool,
        score: f64,
        details: serde_json::Value,
        recommendations: Vec<String>,
        duration_ms: u64,
    ) -> Self {
        Self {
            approach: approach.to_string(),
            passed,
            score,
            details,
            recommendations,
            duration_ms,
        }
    }

    /// Get assessment message
    pub fn assessment(&self) -> &str {
        if self.passed {
            if self.score >= 0.9 {
                "Excellent"
            } else if self.score >= 0.7 {
                "Good"
            } else {
                "Acceptable"
            }
        } else {
            if self.score >= 0.5 {
                "Fair"
            } else if self.score >= 0.3 {
                "Poor"
            } else {
                "Critical"
            }
        }
    }

    /// Get colored assessment
    pub fn colored_assessment(&self) -> String {
        let assessment = self.assessment();
        let color = if self.passed {
            colored::Color::Green
        } else {
            colored::Color::Red
        };
        assessment.color(color).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_suggestion() {
        let error = ValidationError::InputValidation {
            input: "bad".to_string(),
            reason: "Too short".to_string(),
            suggestion: "Make it longer".to_string(),
        };

        assert_eq!(error.suggestion(), "Make it longer");
    }

    #[    test]
    fn test_validation_assessment() {
        let result = ValidationResult::new(
            "test",
            true,
            0.95,
            serde_json::json!({}),
            vec![],
            1000,
        );

        assert_eq!(result.assessment(), "Excellent");
        assert!(result.passed);
    }
}
