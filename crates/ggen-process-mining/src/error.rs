//! Error types for ggen-process-mining.
//!
//! This module defines comprehensive error types for all process mining operations,
//! following the Result<T, E> pattern with no unwrap/expect in production code.

use std::path::PathBuf;
use thiserror::Error;

/// Result type alias for ggen-process-mining operations.
///
/// All fallible operations return `Result<T, Error>` to ensure proper error
/// handling without unwrap/expect.
pub type Result<T> = std::result::Result<T, Error>;

/// Comprehensive error type for process mining operations.
///
/// This error type covers all possible failure modes in process mining:
/// - Event log parsing errors (XES/OCEL)
/// - Model conversion errors (YAWL↔PetriNet)
/// - Discovery algorithm errors
/// - Conformance checking errors
#[derive(Error, Debug)]
pub enum Error {
    /// Error parsing XES event log format.
    #[error("XES parsing error at line {line}: {message}")]
    XesParse {
        /// Line number where the error occurred.
        line: usize,
        /// Error message.
        message: String,
    },

    /// Error parsing OCEL event log format.
    #[error("OCEL parsing error: {0}")]
    OcelParse(String),

    /// Error reading event log file.
    #[error("Failed to read event log file '{path}': {source}")]
    EventLogRead {
        /// Path to the file that failed to read.
        path: PathBuf,
        /// Underlying IO error.
        source: std::io::Error,
    },

    /// Error writing event log file.
    #[error("Failed to write event log file '{path}': {source}")]
    EventLogWrite {
        /// Path to the file that failed to write.
        path: PathBuf,
        /// Underlying IO error.
        source: std::io::Error,
    },

    /// Error converting YAWL to Petri net.
    #[error("YAWL to PetriNet conversion failed: {0}")]
    YawlToPetriNetConversion(String),

    /// Error converting Petri net to YAWL.
    #[error("PetriNet to YAWL conversion failed: {0}")]
    PetriNetToYawlConversion(String),

    /// Invalid YAWL workflow structure.
    #[error("Invalid YAWL workflow: {0}")]
    InvalidYawl(String),

    /// Invalid Petri net structure.
    #[error("Invalid Petri net: {0}")]
    InvalidPetriNet(String),

    /// Error during Alpha++ process discovery.
    #[error("Alpha++ discovery failed: {0}")]
    AlphaPlusPlusDiscovery(String),

    /// Insufficient event log for discovery.
    #[error("Insufficient event log for discovery: need at least {min} traces, got {actual}")]
    InsufficientLog {
        /// Minimum required traces.
        min: usize,
        /// Actual trace count.
        actual: usize,
    },

    /// Error during conformance checking.
    #[error("Conformance checking failed: {0}")]
    ConformanceChecking(String),

    /// Invalid trace in event log.
    #[error("Invalid trace '{trace}': {reason}")]
    InvalidTrace {
        /// Trace identifier.
        trace: String,
        /// Reason for invalidity.
        reason: String,
    },

    /// Missing event attribute.
    #[error("Missing required attribute '{attribute}' in event")]
    MissingAttribute {
        /// Name of the missing attribute.
        attribute: String,
    },

    /// Invalid timestamp format.
    #[error("Invalid timestamp format: '{value}' is not valid ISO 8601")]
    InvalidTimestamp {
        /// The invalid timestamp value.
        value: String,
    },

    /// RDF/SPARQL query error.
    #[error("SPARQL query error: {0}")]
    SparqlQuery(String),

    /// Template rendering error.
    #[error("Template rendering error: {0}")]
    TemplateRendering(String),

    /// Generic configuration error.
    #[error("Configuration error: {0}")]
    Configuration(String),

    /// Timeout during operation.
    #[error("Operation timeout after {duration_ms}ms")]
    Timeout {
        /// Duration in milliseconds.
        duration_ms: u64,
    },

    /// Generic IO error.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

impl Error {
    /// Create an XES parsing error.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process_mining::Error;
    ///
    /// let err = Error::xes_parse(42, "unexpected token");
    /// ```
    #[must_use]
    pub const fn xes_parse(line: usize, message: String) -> Self {
        Self::XesParse { line, message }
    }

    /// Create an OCEL parsing error.
    #[must_use]
    pub const fn ocel_parse(message: String) -> Self {
        Self::OcelParse(message)
    }

    /// Create an insufficient log error.
    #[must_use]
    pub const fn insufficient_log(min: usize, actual: usize) -> Self {
        Self::InsufficientLog { min, actual }
    }

    /// Create an invalid trace error.
    #[must_use]
    pub const fn invalid_trace(trace: String, reason: String) -> Self {
        Self::InvalidTrace { trace, reason }
    }

    /// Create a missing attribute error.
    #[must_use]
    pub const fn missing_attribute(attribute: String) -> Self {
        Self::MissingAttribute { attribute }
    }

    /// Create an invalid timestamp error.
    #[must_use]
    pub const fn invalid_timestamp(value: String) -> Self {
        Self::InvalidTimestamp { value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = Error::xes_parse(10, "unexpected token".to_string());
        assert_eq!(
            err.to_string(),
            "XES parsing error at line 10: unexpected token"
        );
    }

    #[test]
    fn test_insufficient_log_error() {
        let err = Error::insufficient_log(5, 2);
        assert!(matches!(err, Error::InsufficientLog { min: 5, actual: 2 }));
    }

    #[test]
    fn test_invalid_trace_error() {
        let err = Error::invalid_trace("trace-1".to_string(), "empty trace".to_string());
        assert_eq!(
            err.to_string(),
            "Invalid trace 'trace-1': empty trace"
        );
    }

    #[test]
    fn test_missing_attribute_error() {
        let err = Error::missing_attribute("timestamp".to_string());
        assert_eq!(
            err.to_string(),
            "Missing required attribute 'timestamp' in event"
        );
    }

    #[test]
    fn test_invalid_timestamp_error() {
        let err = Error::invalid_timestamp("not-a-timestamp".to_string());
        assert_eq!(
            err.to_string(),
            "Invalid timestamp format: 'not-a-timestamp' is not valid ISO 8601"
        );
    }

    #[test]
    fn test_result_type_exists() {
        // Verify Result type alias works
        let result: Result<()> = Ok(());
        assert!(result.is_ok());
    }
}
