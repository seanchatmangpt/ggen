//! Error types for ontology extraction and processing

use std::fmt;

/// Ontology processing errors
#[derive(Debug, Clone)]
pub enum OntologyError {
    /// SPARQL query execution failed
    SparqlError { query: String, reason: String },
    /// Ontology namespace not found
    NamespaceNotFound { namespace: String },
    /// Class not found in ontology
    ClassNotFound { uri: String },
    /// Property not found in ontology
    PropertyNotFound { uri: String },
    /// Invalid ontology format
    InvalidFormat { reason: String },
    /// Cardinality constraint violation
    CardinalityViolation {
        property: String,
        constraint: String,
    },
    /// Type mismatch in property value
    TypeMismatch {
        property: String,
        expected: String,
        found: String,
    },
    /// Cyclic dependency detected
    CyclicDependency { classes: Vec<String> },
    /// Parse error in ontology
    ParseError { line: usize, reason: String },
    /// General ontology error
    Other { message: String },
}

impl fmt::Display for OntologyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SparqlError { query, reason } => {
                write!(f, "SPARQL error: {} (query: {})", reason, query)
            }
            Self::NamespaceNotFound { namespace } => {
                write!(f, "Namespace not found: {}", namespace)
            }
            Self::ClassNotFound { uri } => {
                write!(f, "Class not found: {}", uri)
            }
            Self::PropertyNotFound { uri } => {
                write!(f, "Property not found: {}", uri)
            }
            Self::InvalidFormat { reason } => {
                write!(f, "Invalid ontology format: {}", reason)
            }
            Self::CardinalityViolation {
                property,
                constraint,
            } => {
                write!(f, "Cardinality violation on {}: {}", property, constraint)
            }
            Self::TypeMismatch {
                property,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Type mismatch on property {}: expected {}, found {}",
                    property, expected, found
                )
            }
            Self::CyclicDependency { classes } => {
                write!(f, "Cyclic dependency detected in classes: {:?}", classes)
            }
            Self::ParseError { line, reason } => {
                write!(f, "Parse error at line {}: {}", line, reason)
            }
            Self::Other { message } => {
                write!(f, "Ontology error: {}", message)
            }
        }
    }
}

impl std::error::Error for OntologyError {}

impl From<String> for OntologyError {
    fn from(msg: String) -> Self {
        Self::Other { message: msg }
    }
}

impl From<&str> for OntologyError {
    fn from(msg: &str) -> Self {
        Self::Other {
            message: msg.to_string(),
        }
    }
}

pub type OntologyResult<T> = Result<T, OntologyError>;
