//! Error types for ggen-yawl.
//!
//! This module defines the error types used throughout the ggen-yawl crate.
//! All operations return [`Result<T, Error>`] where [`Error`] represents
//! the various failure modes that can occur during YAWL workflow generation.
//!
//! # Example
//!
//! ```rust
//! use ggen_yawl::Error;
//!
//! fn handle_result(result: Result<String, Error>) {
//!     match result {
//!         Ok(xml) => println!("Generated: {}", xml),
//!         Err(Error::OntologyLoad(msg)) => eprintln!("Load error: {}", msg),
//!         Err(Error::Sparql(msg)) => eprintln!("Query error: {}", msg),
//!         Err(e) => eprintln!("Other error: {}", e),
//!     }
//! }
//! ```

use ggen_utils::error::Error as GgenError;
use thiserror::Error;

/// Result type for ggen-yawl operations.
///
/// This type alias is used throughout the crate for operations that can fail.
/// It wraps a successful value `T` or an [`Error`].
///
/// # Type Parameters
///
/// * `T` - The success type
pub type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur during YAWL workflow generation.
///
/// This enum represents all possible error conditions that can occur
/// during the five-stage transformation pipeline from ontology to YAWL.
///
/// # Variants
///
/// * [`OntologyLoad`] - Errors loading or parsing ontology files
/// * [`Sparql`] - SPARQL query execution errors
/// * [`Template`] - Template rendering errors
/// * [`Validation`] - YAWL validation errors
/// * [`Io`] - File I/O errors
/// * [`Serde`] - JSON serialization/deserialization errors
/// * [`Ggen`] - Errors from underlying ggen-utils
/// * [`Oxigraph`] - RDF store errors from Oxigraph
/// * [`Other`] - Generic errors with context
#[derive(Debug, Error)]
pub enum Error {
    /// Error loading or parsing ontology.
    ///
    /// This variant is used when the ontology file cannot be read,
    /// does not exist, or contains invalid RDF syntax.
    #[error("Ontology load error: {0}")]
    OntologyLoad(String),

    /// RDF/SPARQL operation error.
    ///
    /// This variant is used when SPARQL queries fail to execute,
    /// return unexpected results, or have syntax errors.
    #[error("SPARQL error: {0}")]
    Sparql(String),

    /// Template rendering error.
    ///
    /// This variant is used when Tera template rendering fails
    /// due to missing variables, syntax errors, or I/O issues.
    #[error("Template error: {0}")]
    Template(String),

    /// Validation error for generated YAWL.
    ///
    /// This variant is used when the generated YAWL XML
    /// does not conform to the YAWL schema or has structural issues.
    #[error("YAWL validation failed: {0}")]
    Validation(String),

    /// IO error.
    ///
    /// This variant wraps standard I/O errors from file operations.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Serde error.
    ///
    /// This variant wraps JSON serialization/deserialization errors.
    #[error("Serialization error: {0}")]
    Serde(#[from] serde_json::Error),

    /// Underlying ggen-utils error.
    ///
    /// This variant wraps errors from the ggen-utils crate.
    #[error("Ggen error: {0}")]
    Ggen(#[from] GgenError),

    /// Oxigraph error.
    ///
    /// This variant is used for RDF store-specific errors from Oxigraph.
    #[error("RDF store error: {0}")]
    Oxigraph(String),

    /// Generic error with context.
    ///
    /// This variant is used for errors that don't fit into other categories
    /// but still need to be reported with context.
    #[error("{0}")]
    Other(String),
}

impl Error {
    /// Create a new SPARQL error with context.
    ///
    /// # Arguments
    ///
    /// * `msg` - Error message describing the SPARQL failure
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::Error;
    ///
    /// let err = Error::sparql("Query execution timeout");
    /// assert_eq!(err.to_string(), "SPARQL error: Query execution timeout");
    /// ```
    pub fn sparql(msg: impl Into<String>) -> Self {
        Self::Sparql(msg.into())
    }

    /// Create a new template error with context.
    ///
    /// # Arguments
    ///
    /// * `msg` - Error message describing the template failure
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::Error;
    ///
    /// let err = Error::template("Missing variable: workflow_name");
    /// assert_eq!(err.to_string(), "Template error: Missing variable: workflow_name");
    /// ```
    pub fn template(msg: impl Into<String>) -> Self {
        Self::Template(msg.into())
    }

    /// Create a new validation error with context.
    ///
    /// # Arguments
    ///
    /// * `msg` - Error message describing the validation failure
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::Error;
    ///
    /// let err = Error::validation("Missing <decomposition> element");
    /// assert_eq!(err.to_string(), "YAWL validation failed: Missing <decomposition> element");
    /// ```
    pub fn validation(msg: impl Into<String>) -> Self {
        Self::Validation(msg.into())
    }
}

// Convert oxigraph::sparql::QueryEvaluationError to our Error
impl From<oxigraph::sparql::QueryEvaluationError> for Error {
    fn from(err: oxigraph::sparql::QueryEvaluationError) -> Self {
        Self::Sparql(err.to_string())
    }
}

// Convert oxigraph::store::StorageError to our Error
impl From<oxigraph::store::StorageError> for Error {
    fn from(err: oxigraph::store::StorageError) -> Self {
        Self::Oxigraph(err.to_string())
    }
}

// Convert tera::Error to our Error
impl From<tera::Error> for Error {
    fn from(err: tera::Error) -> Self {
        Self::Template(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = Error::OntologyLoad("file not found".to_string());
        assert!(err.to_string().contains("Ontology load error"));
    }
}
