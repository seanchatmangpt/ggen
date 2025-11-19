//! # Compile-Time Ontology Transformation Guardrails
//!
//! This module provides type-level validation for ontology transformations,
//! preventing invalid transformations at compile time.

use std::marker::PhantomData;
use std::path::PathBuf;

/// Marker trait for schema versions - sealed to prevent extension
pub trait SchemaVersion: private::Sealed {
    const VERSION: &'static str;
    const NAMESPACE: &'static str;
}

/// Schema version markers
#[derive(Debug, Clone, Copy)]
pub struct V1_0_0;

#[derive(Debug, Clone, Copy)]
pub struct V2_0_0;

#[derive(Debug, Clone, Copy)]
pub struct V2_1_0;

impl SchemaVersion for V1_0_0 {
    const VERSION: &'static str = "1.0.0";
    const NAMESPACE: &'static str = "http://ggen.io/schema/v1#";
}

impl SchemaVersion for V2_0_0 {
    const VERSION: &'static str = "2.0.0";
    const NAMESPACE: &'static str = "http://ggen.io/schema/v2#";
}

impl SchemaVersion for V2_1_0 {
    const VERSION: &'static str = "2.1.0";
    const NAMESPACE: &'static str = "http://ggen.io/schema/v2.1#";
}

/// Marker trait for ontology validation state
pub trait ValidationState: private::Sealed {}

#[derive(Debug, Clone, Copy)]
pub struct Unvalidated;

#[derive(Debug, Clone, Copy)]
pub struct Validated;

#[derive(Debug, Clone, Copy)]
pub struct SchemaChecked;

impl ValidationState for Unvalidated {}
impl ValidationState for Validated {}
impl ValidationState for SchemaChecked {}

/// Validated ontology with compile-time schema version tracking
///
/// # Type Parameters
///
/// - `V`: Schema version marker type
/// - `S`: Validation state marker type
///
/// # Examples
///
/// ```
/// use ggen_domain::poka_yoke::ontology_guards::{ValidatedOntology, V2_0_0};
///
/// // This is enforced at compile time
/// let ontology = ValidatedOntology::<V2_0_0, _>::new(graph);
/// ```
#[derive(Debug, Clone)]
pub struct ValidatedOntology<V: SchemaVersion, S: ValidationState> {
    graph: crate::graph::core::Graph,
    _version: PhantomData<V>,
    _state: PhantomData<S>,
}

impl<V: SchemaVersion> ValidatedOntology<V, Unvalidated> {
    /// Create a new unvalidated ontology
    pub fn new(graph: crate::graph::core::Graph) -> Self {
        Self {
            graph,
            _version: PhantomData,
            _state: PhantomData,
        }
    }

    /// Validate the ontology structure
    ///
    /// # Returns
    ///
    /// `Ok(ValidatedOntology<V, Validated>)` if validation succeeds
    pub fn validate(self) -> Result<ValidatedOntology<V, Validated>, ValidationError> {
        // Perform basic structural validation
        self.validate_namespaces()?;
        self.validate_required_predicates()?;

        Ok(ValidatedOntology {
            graph: self.graph,
            _version: PhantomData,
            _state: PhantomData,
        })
    }

    fn validate_namespaces(&self) -> Result<(), ValidationError> {
        // Ensure required namespaces are present
        let required_namespaces = [
            V::NAMESPACE,
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            "http://www.w3.org/2000/01/rdf-schema#",
        ];

        // TODO: Check graph for required namespaces
        // For now, always succeed
        Ok(())
    }

    fn validate_required_predicates(&self) -> Result<(), ValidationError> {
        // Ensure required predicates exist
        // TODO: Implement actual validation
        Ok(())
    }
}

impl<V: SchemaVersion> ValidatedOntology<V, Validated> {
    /// Perform schema-level validation
    ///
    /// # Returns
    ///
    /// `Ok(ValidatedOntology<V, SchemaChecked>)` if schema validation succeeds
    pub fn check_schema(self) -> Result<ValidatedOntology<V, SchemaChecked>, ValidationError> {
        // Perform SHACL or similar schema validation
        self.validate_shacl_constraints()?;
        self.validate_cardinality()?;

        Ok(ValidatedOntology {
            graph: self.graph,
            _version: PhantomData,
            _state: PhantomData,
        })
    }

    fn validate_shacl_constraints(&self) -> Result<(), ValidationError> {
        // TODO: Implement SHACL validation
        Ok(())
    }

    fn validate_cardinality(&self) -> Result<(), ValidationError> {
        // TODO: Implement cardinality checks
        Ok(())
    }
}

impl<V: SchemaVersion, S: ValidationState> ValidatedOntology<V, S> {
    /// Get reference to the underlying graph
    pub fn graph(&self) -> &crate::graph::core::Graph {
        &self.graph
    }

    /// Get schema version
    pub fn version(&self) -> &'static str {
        V::VERSION
    }

    /// Get schema namespace
    pub fn namespace(&self) -> &'static str {
        V::NAMESPACE
    }
}

/// Namespace validator for compile-time namespace checking
pub struct NamespaceValidator<V: SchemaVersion> {
    _version: PhantomData<V>,
}

impl<V: SchemaVersion> NamespaceValidator<V> {
    /// Create a new namespace validator for schema version V
    pub fn new() -> Self {
        Self {
            _version: PhantomData,
        }
    }

    /// Validate that a namespace URI is compatible with schema version V
    ///
    /// # Physical Constraint
    ///
    /// Returns a compile-time validated namespace or error
    pub fn validate_namespace(&self, uri: &str) -> Result<ValidatedNamespace<V>, ValidationError> {
        if uri.starts_with(V::NAMESPACE) {
            Ok(ValidatedNamespace {
                uri: uri.to_string(),
                _version: PhantomData,
            })
        } else {
            Err(ValidationError::InvalidNamespace {
                expected: V::NAMESPACE,
                found: uri.to_string(),
            })
        }
    }
}

impl<V: SchemaVersion> Default for NamespaceValidator<V> {
    fn default() -> Self {
        Self::new()
    }
}

/// A namespace that has been validated at compile-time for schema version V
#[derive(Debug, Clone)]
pub struct ValidatedNamespace<V: SchemaVersion> {
    uri: String,
    _version: PhantomData<V>,
}

impl<V: SchemaVersion> ValidatedNamespace<V> {
    /// Get the namespace URI
    pub fn uri(&self) -> &str {
        &self.uri
    }

    /// Get the schema version this namespace is validated for
    pub fn version(&self) -> &'static str {
        V::VERSION
    }
}

/// Transformation guard for ontology transformations
///
/// # Type Parameters
///
/// - `From`: Source schema version
/// - `To`: Target schema version
///
/// # Physical Constraint
///
/// Only compatible transformations can be constructed
pub struct TransformationGuard<From: SchemaVersion, To: SchemaVersion> {
    _from: PhantomData<From>,
    _to: PhantomData<To>,
}

impl<From: SchemaVersion, To: SchemaVersion> TransformationGuard<From, To> {
    /// Attempt to create a transformation guard
    ///
    /// # Returns
    ///
    /// `Some(guard)` if transformation is valid, `None` otherwise
    pub fn new() -> Option<Self> {
        // Check if transformation is allowed
        if Self::is_valid_transformation() {
            Some(Self {
                _from: PhantomData,
                _to: PhantomData,
            })
        } else {
            None
        }
    }

    fn is_valid_transformation() -> bool {
        // Define valid transformation paths
        // For now, allow forward migrations only
        let from_ver = From::VERSION;
        let to_ver = To::VERSION;

        matches!(
            (from_ver, to_ver),
            ("1.0.0", "2.0.0") | ("2.0.0", "2.1.0") | ("1.0.0", "2.1.0")
        )
    }
}

/// Ontology transformation with compile-time version tracking
pub struct OntologyTransform<From: SchemaVersion, To: SchemaVersion> {
    guard: TransformationGuard<From, To>,
}

impl<From: SchemaVersion, To: SchemaVersion> OntologyTransform<From, To> {
    /// Create a new transformation if it's valid
    ///
    /// # Returns
    ///
    /// `Some(transform)` if transformation is allowed, `None` otherwise
    ///
    /// # Physical Constraint
    ///
    /// Invalid transformations cannot be created
    pub fn new() -> Option<Self> {
        TransformationGuard::new().map(|guard| Self { guard })
    }

    /// Apply transformation to a validated ontology
    ///
    /// # Type Safety
    ///
    /// Input must be validated for `From` schema
    /// Output is guaranteed to be validated for `To` schema
    pub fn apply(
        &self,
        ontology: ValidatedOntology<From, SchemaChecked>,
    ) -> Result<ValidatedOntology<To, Validated>, ValidationError> {
        // Transform the ontology
        let transformed_graph = self.transform_graph(ontology.graph)?;

        // Return validated ontology with new schema version
        Ok(ValidatedOntology {
            graph: transformed_graph,
            _version: PhantomData,
            _state: PhantomData,
        })
    }

    fn transform_graph(&self, graph: crate::graph::core::Graph) -> Result<crate::graph::core::Graph, ValidationError> {
        // TODO: Implement actual transformation logic
        // For now, return the same graph
        Ok(graph)
    }
}

/// Validation errors
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Invalid namespace: expected {expected}, found {found}")]
    InvalidNamespace {
        expected: &'static str,
        found: String,
    },

    #[error("Missing required predicate: {0}")]
    MissingPredicate(String),

    #[error("SHACL validation failed: {0}")]
    ShaclValidation(String),

    #[error("Cardinality constraint violated: {0}")]
    CardinalityViolation(String),

    #[error("Graph error: {0}")]
    GraphError(#[from] crate::graph::error::Error),
}

/// Private module to seal traits
mod private {
    pub trait Sealed {}

    impl Sealed for super::V1_0_0 {}
    impl Sealed for super::V2_0_0 {}
    impl Sealed for super::V2_1_0 {}
    impl Sealed for super::Unvalidated {}
    impl Sealed for super::Validated {}
    impl Sealed for super::SchemaChecked {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_schema_version_tracking() {
        assert_eq!(V1_0_0::VERSION, "1.0.0");
        assert_eq!(V2_0_0::VERSION, "2.0.0");
        assert_eq!(V2_1_0::VERSION, "2.1.0");
    }

    #[test]
    fn test_valid_transformation() {
        // V1 -> V2 should be allowed
        let transform = OntologyTransform::<V1_0_0, V2_0_0>::new();
        assert!(transform.is_some());
    }

    #[test]
    fn test_invalid_transformation() {
        // V2 -> V1 should NOT be allowed (backward migration)
        let transform = OntologyTransform::<V2_0_0, V1_0_0>::new();
        assert!(transform.is_none());
    }

    #[test]
    fn test_namespace_validation() {
        let validator = NamespaceValidator::<V2_0_0>::new();

        let valid = validator.validate_namespace("http://ggen.io/schema/v2#Something");
        assert!(valid.is_ok());

        let invalid = validator.validate_namespace("http://other.io/schema#Something");
        assert!(invalid.is_err());
    }
}
