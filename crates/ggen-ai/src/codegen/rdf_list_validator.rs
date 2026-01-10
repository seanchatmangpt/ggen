//! RDF List Chain Validation
//!
//! This module provides validation for RDF list chains to detect malformed rdf:rest* sequences.
//!
//! RDF lists are represented as linked chains of nodes with rdf:first and rdf:rest properties,
//! terminated by rdf:nil. This validator ensures:
//! - Chains terminate at rdf:nil
//! - No circular references exist
//! - Chains don't exceed maximum depth
//! - All nodes are properly formed
//!
//! ## Example
//!
//! ```text
//! # Valid list: (1 2 3)
//! ex:list1 rdf:first 1 ;
//!          rdf:rest ex:list2 .
//! ex:list2 rdf:first 2 ;
//!          rdf:rest ex:list3 .
//! ex:list3 rdf:first 3 ;
//!          rdf:rest rdf:nil .
//!
//! # Invalid: missing nil terminator
//! ex:badList1 rdf:first "a" ;
//!             rdf:rest ex:badList2 .
//! ex:badList2 rdf:first "b" .  # No rdf:rest!
//!
//! # Invalid: circular reference
//! ex:cycleA rdf:first "x" ;
//!           rdf:rest ex:cycleB .
//! ex:cycleB rdf:first "y" ;
//!           rdf:rest ex:cycleA .  # Back to start!
//! ```

use crate::error::GgenAiError;
use oxigraph::model::Term;
use oxigraph::store::Store;
use std::collections::HashSet;

/// Validation error types for RDF lists
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationError {
    /// Circular reference detected in list chain
    CircularReference {
        /// Node where cycle begins
        start: String,
        /// Node that closes the cycle
        cycle_node: String,
    },
    /// List does not terminate at rdf:nil
    MissingNilTerminator {
        /// Final node reached before max depth
        tail: String,
    },
    /// Invalid rdf:rest property
    InvalidRestProperty {
        /// Node with invalid property
        node: String,
        /// Reason for invalidity
        reason: String,
    },
    /// Maximum depth exceeded
    MaxDepthExceeded {
        /// Maximum allowed depth
        max: usize,
        /// Actual depth reached
        actual: usize,
    },
    /// Missing rdf:first property
    MissingFirstProperty {
        /// Node missing rdf:first
        node: String,
    },
    /// Invalid list head
    InvalidListHead {
        /// The problematic head node
        node: String,
        /// Reason for invalidity
        reason: String,
    },
    /// RDF store query error
    QueryError {
        /// Error message from query execution
        message: String,
    },
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationError::CircularReference { start, cycle_node } => {
                write!(
                    f,
                    "Circular reference in RDF list: {} -> ... -> {} (back to {})",
                    start, cycle_node, start
                )
            }
            ValidationError::MissingNilTerminator { tail } => {
                write!(
                    f,
                    "RDF list does not terminate at rdf:nil. Last node: {}",
                    tail
                )
            }
            ValidationError::InvalidRestProperty { node, reason } => {
                write!(f, "Invalid rdf:rest property at {}: {}", node, reason)
            }
            ValidationError::MaxDepthExceeded { max, actual } => {
                write!(
                    f,
                    "RDF list exceeds maximum depth: {} (limit: {})",
                    actual, max
                )
            }
            ValidationError::MissingFirstProperty { node } => {
                write!(f, "RDF list node {} missing rdf:first property", node)
            }
            ValidationError::InvalidListHead { node, reason } => {
                write!(f, "Invalid list head {}: {}", node, reason)
            }
            ValidationError::QueryError { message } => {
                write!(f, "RDF query error: {}", message)
            }
        }
    }
}

impl std::error::Error for ValidationError {}

/// RDF List Validator
///
/// Validates RDF list chains for well-formedness and integrity.
pub struct RdfListValidator {
    /// Maximum allowed depth for list chains
    max_depth: usize,
}

impl RdfListValidator {
    /// Create a new RDF list validator with default max depth
    pub fn new() -> Self {
        Self {
            max_depth: 10000,
        }
    }

    /// Create a new RDF list validator with custom max depth
    pub fn with_max_depth(max_depth: usize) -> Self {
        Self { max_depth }
    }

    /// Validate an RDF list chain starting from a head node
    ///
    /// # Arguments
    ///
    /// * `store` - The RDF store containing the list data
    /// * `head_uri` - URI of the list head node (or rdf:nil for empty list)
    ///
    /// # Returns
    ///
    /// Ordered vector of list member URIs/literals, or a ValidationError
    pub fn validate_list(
        &self,
        store: &Store,
        head_uri: &str,
    ) -> std::result::Result<Vec<String>, ValidationError> {
        // Handle empty list case
        if self.is_nil_node(head_uri) {
            return Ok(vec![]);
        }

        let mut members = Vec::new();
        let mut visited = HashSet::new();
        let mut current = head_uri.to_string();

        for _depth in 0..self.max_depth {
            // Check for circular reference
            if visited.contains(&current) {
                return Err(ValidationError::CircularReference {
                    start: head_uri.to_string(),
                    cycle_node: current,
                });
            }

            visited.insert(current.clone());

            // Get rdf:first value
            let first_value = self.get_first_value(store, &current)?;
            members.push(first_value);

            // Get rdf:rest value
            let rest_value = self.get_rest_value(store, &current)?;

            // Check if we've reached the end
            if self.is_nil_node(&rest_value) {
                return Ok(members);
            }

            current = rest_value;
        }

        Err(ValidationError::MaxDepthExceeded {
            max: self.max_depth,
            actual: self.max_depth,
        })
    }

    /// Get the rdf:first value from a list node
    fn get_first_value(&self, store: &Store, node_uri: &str) -> std::result::Result<String, ValidationError> {
        let query = format!(
            r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT ?value
            WHERE {{
                <{node_uri}> rdf:first ?value .
            }}
            "#
        );

        #[allow(deprecated)]
        let results = store.query(&query).map_err(|e| ValidationError::QueryError {
            message: format!("Failed to query rdf:first: {}", e),
        })?;

        use oxigraph::sparql::QueryResults;
        match results {
            QueryResults::Solutions(mut solutions) => {
                if let Some(solution) = solutions.next() {
                    let solution = solution.map_err(|e| ValidationError::QueryError {
                        message: format!("SPARQL solution error: {}", e),
                    })?;

                    if let Some(term) = solution.get("value") {
                        return Ok(self.term_to_string(term));
                    }
                }
            }
            _ => {}
        }

        Err(ValidationError::MissingFirstProperty {
            node: node_uri.to_string(),
        })
    }

    /// Get the rdf:rest value from a list node
    fn get_rest_value(&self, store: &Store, node_uri: &str) -> std::result::Result<String, ValidationError> {
        let query = format!(
            r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT ?rest
            WHERE {{
                <{node_uri}> rdf:rest ?rest .
            }}
            "#
        );

        #[allow(deprecated)]
        let results = store.query(&query).map_err(|e| ValidationError::QueryError {
            message: format!("Failed to query rdf:rest: {}", e),
        })?;

        use oxigraph::sparql::QueryResults;
        match results {
            QueryResults::Solutions(mut solutions) => {
                if let Some(solution) = solutions.next() {
                    let solution = solution.map_err(|e| ValidationError::QueryError {
                        message: format!("SPARQL solution error: {}", e),
                    })?;

                    if let Some(term) = solution.get("rest") {
                        return Ok(self.term_to_string(term));
                    }
                }
            }
            _ => {}
        }

        Err(ValidationError::MissingNilTerminator {
            tail: node_uri.to_string(),
        })
    }

    /// Check if a node URI is rdf:nil
    fn is_nil_node(&self, uri: &str) -> bool {
        uri == "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"
            || uri == "rdf:nil"
    }

    /// Convert an RDF term to a string representation
    fn term_to_string(&self, term: &Term) -> String {
        match term {
            Term::NamedNode(node) => node.as_str().to_string(),
            Term::Literal(literal) => literal.value().to_string(),
            Term::BlankNode(bn) => format!("_:{}", bn.as_str()),
        }
    }
}

impl Default for RdfListValidator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert ValidationError to GgenAiError
impl From<ValidationError> for GgenAiError {
    fn from(err: ValidationError) -> Self {
        GgenAiError::Validation(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxigraph::model::{Literal, NamedNode};
    use oxigraph::store::Store;

    // Helper function to add a triple to the store
    fn add_triple(store: &Store, subject: &str, predicate: &str, object: &str) {
        use oxigraph::model::GraphName;
        let s = NamedNode::new(subject).unwrap();
        let p = NamedNode::new(predicate).unwrap();
        let o = NamedNode::new(object).unwrap();
        let quad = oxigraph::model::Quad::new(s, p, o, GraphName::DefaultGraph);
        store.insert(&quad).expect("Failed to insert triple");
    }

    // Helper function to add a triple with literal object
    fn add_triple_with_literal(store: &Store, subject: &str, predicate: &str, object: &str) {
        use oxigraph::model::GraphName;
        let s = NamedNode::new(subject).unwrap();
        let p = NamedNode::new(predicate).unwrap();
        let o = Literal::new_simple_literal(object);
        let quad = oxigraph::model::Quad::new(s, p, o, GraphName::DefaultGraph);
        store.insert(&quad).expect("Failed to insert triple");
    }

    const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
    const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
    const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";

    #[test]
    fn test_validator_creation() {
        let validator = RdfListValidator::new();
        assert_eq!(validator.max_depth, 10000);
        // Verify validator is properly initialized
        assert!(!validator.is_nil_node("http://example.com/node"));
    }

    #[test]
    fn test_validator_custom_max_depth() {
        let validator = RdfListValidator::with_max_depth(100);
        assert_eq!(validator.max_depth, 100);
    }

    #[test]
    fn test_validator_default() {
        let validator = RdfListValidator::default();
        assert_eq!(validator.max_depth, 10000);
    }

    #[test]
    fn test_empty_list() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        let result = validator.validate_list(&store, RDF_NIL).expect("Validation failed");
        assert!(result.is_empty());
    }

    #[test]
    fn test_single_element_list() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a single-element list: (42)
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "42");
        add_triple(&store, "http://example.com/list1", RDF_REST, RDF_NIL);

        let result = validator
            .validate_list(&store, "http://example.com/list1")
            .expect("Validation failed");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], "42");
    }

    #[test]
    fn test_three_element_list() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a three-element list: (1 2 3)
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "1");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "2");
        add_triple(&store, "http://example.com/list2", RDF_REST, "http://example.com/list3");

        add_triple_with_literal(&store, "http://example.com/list3", RDF_FIRST, "3");
        add_triple(&store, "http://example.com/list3", RDF_REST, RDF_NIL);

        let result = validator
            .validate_list(&store, "http://example.com/list1")
            .expect("Validation failed");
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], "1");
        assert_eq!(result[1], "2");
        assert_eq!(result[2], "3");
    }

    #[test]
    fn test_missing_nil_terminator() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a malformed list without nil terminator
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "a");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "b");
        // No rdf:rest property!

        let result = validator.validate_list(&store, "http://example.com/list1");
        assert!(result.is_err());

        match result {
            Err(ValidationError::MissingNilTerminator { tail }) => {
                assert_eq!(tail, "http://example.com/list2");
            }
            _ => panic!("Expected MissingNilTerminator error"),
        }
    }

    #[test]
    fn test_circular_reference_two_nodes() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a circular reference: list1 -> list2 -> list1
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "a");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "b");
        add_triple(&store, "http://example.com/list2", RDF_REST, "http://example.com/list1");

        let result = validator.validate_list(&store, "http://example.com/list1");
        assert!(result.is_err());

        match result {
            Err(ValidationError::CircularReference { start, cycle_node }) => {
                assert_eq!(start, "http://example.com/list1");
                assert_eq!(cycle_node, "http://example.com/list1");
            }
            _ => panic!("Expected CircularReference error"),
        }
    }

    #[test]
    fn test_circular_reference_self_loop() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a self-loop: list1 -> list1
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "x");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list1");

        let result = validator.validate_list(&store, "http://example.com/list1");
        assert!(result.is_err());

        match result {
            Err(ValidationError::CircularReference { .. }) => {
                // Expected
            }
            _ => panic!("Expected CircularReference error"),
        }
    }

    #[test]
    fn test_circular_reference_three_node_cycle() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a cycle: list1 -> list2 -> list3 -> list1
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "a");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "b");
        add_triple(&store, "http://example.com/list2", RDF_REST, "http://example.com/list3");

        add_triple_with_literal(&store, "http://example.com/list3", RDF_FIRST, "c");
        add_triple(&store, "http://example.com/list3", RDF_REST, "http://example.com/list1");

        let result = validator.validate_list(&store, "http://example.com/list1");
        assert!(result.is_err());

        match result {
            Err(ValidationError::CircularReference { .. }) => {
                // Expected
            }
            _ => panic!("Expected CircularReference error"),
        }
    }

    #[test]
    fn test_max_depth_exceeded() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::with_max_depth(5);

        // Create a chain longer than max_depth
        for i in 0..10 {
            let subject = format!("http://example.com/list{}", i);
            let value = format!("{}", i);
            add_triple_with_literal(&store, &subject, RDF_FIRST, &value);

            if i < 9 {
                let rest = format!("http://example.com/list{}", i + 1);
                add_triple(&store, &subject, RDF_REST, &rest);
            } else {
                add_triple(&store, &subject, RDF_REST, RDF_NIL);
            }
        }

        let result = validator.validate_list(&store, "http://example.com/list0");
        assert!(result.is_err());

        match result {
            Err(ValidationError::MaxDepthExceeded { max, actual }) => {
                assert_eq!(max, 5);
                assert_eq!(actual, 5);
            }
            _ => panic!("Expected MaxDepthExceeded error"),
        }
    }

    #[test]
    fn test_missing_first_property() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a malformed list node without rdf:first
        add_triple(&store, "http://example.com/list1", RDF_REST, RDF_NIL);

        let result = validator.validate_list(&store, "http://example.com/list1");
        assert!(result.is_err());

        match result {
            Err(ValidationError::MissingFirstProperty { node }) => {
                assert_eq!(node, "http://example.com/list1");
            }
            _ => panic!("Expected MissingFirstProperty error"),
        }
    }

    #[test]
    fn test_string_list_values() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a list of strings
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "apple");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "banana");
        add_triple(&store, "http://example.com/list2", RDF_REST, "http://example.com/list3");

        add_triple_with_literal(&store, "http://example.com/list3", RDF_FIRST, "cherry");
        add_triple(&store, "http://example.com/list3", RDF_REST, RDF_NIL);

        let result = validator
            .validate_list(&store, "http://example.com/list1")
            .expect("Validation failed");
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], "apple");
        assert_eq!(result[1], "banana");
        assert_eq!(result[2], "cherry");
    }

    #[test]
    fn test_named_node_list_values() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a list of named nodes (URIs)
        add_triple(&store, "http://example.com/list1", RDF_FIRST, "http://example.com/node1");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple(&store, "http://example.com/list2", RDF_FIRST, "http://example.com/node2");
        add_triple(&store, "http://example.com/list2", RDF_REST, RDF_NIL);

        let result = validator
            .validate_list(&store, "http://example.com/list1")
            .expect("Validation failed");
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], "http://example.com/node1");
        assert_eq!(result[1], "http://example.com/node2");
    }

    #[test]
    fn test_unicode_values() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a list with unicode strings
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "café");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "日本語");
        add_triple(&store, "http://example.com/list2", RDF_REST, "http://example.com/list3");

        add_triple_with_literal(&store, "http://example.com/list3", RDF_FIRST, "🚀");
        add_triple(&store, "http://example.com/list3", RDF_REST, RDF_NIL);

        let result = validator
            .validate_list(&store, "http://example.com/list1")
            .expect("Validation failed");
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], "café");
        assert_eq!(result[1], "日本語");
        assert_eq!(result[2], "🚀");
    }

    #[test]
    fn test_nil_node_recognition() {
        let validator = RdfListValidator::new();

        // Test both forms of nil
        assert!(validator.is_nil_node("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"));
        assert!(validator.is_nil_node("rdf:nil"));
        assert!(!validator.is_nil_node("http://example.com/something"));
    }

    #[test]
    fn test_validation_error_display_circular() {
        let err = ValidationError::CircularReference {
            start: "http://example.com/list1".to_string(),
            cycle_node: "http://example.com/list2".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("Circular reference"));
        assert!(msg.contains("list1"));
    }

    #[test]
    fn test_validation_error_display_missing_nil() {
        let err = ValidationError::MissingNilTerminator {
            tail: "http://example.com/tail".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("does not terminate at rdf:nil"));
    }

    #[test]
    fn test_validation_error_display_invalid_rest() {
        let err = ValidationError::InvalidRestProperty {
            node: "http://example.com/node".to_string(),
            reason: "multiple rdf:rest properties found".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("Invalid rdf:rest property"));
    }

    #[test]
    fn test_validation_error_display_max_depth() {
        let err = ValidationError::MaxDepthExceeded {
            max: 100,
            actual: 150,
        };
        let msg = err.to_string();
        assert!(msg.contains("exceeds maximum depth"));
    }

    #[test]
    fn test_validation_error_display_missing_first() {
        let err = ValidationError::MissingFirstProperty {
            node: "http://example.com/node".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("missing rdf:first"));
    }

    #[test]
    fn test_validation_error_is_error() {
        let err = ValidationError::CircularReference {
            start: "a".to_string(),
            cycle_node: "b".to_string(),
        };
        let _: &dyn std::error::Error = &err;
    }

    #[test]
    fn test_validation_error_to_ggen_ai_error() {
        let err = ValidationError::CircularReference {
            start: "http://example.com/list1".to_string(),
            cycle_node: "http://example.com/list2".to_string(),
        };
        let ggen_err: GgenAiError = err.into();
        let msg = ggen_err.to_string();
        assert!(msg.contains("Circular reference"));
    }

    #[test]
    fn test_ten_element_list() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a 10-element list
        for i in 0..10 {
            let subject = format!("http://example.com/list{}", i);
            let value = format!("{}", i);
            add_triple_with_literal(&store, &subject, RDF_FIRST, &value);

            if i < 9 {
                let rest = format!("http://example.com/list{}", i + 1);
                add_triple(&store, &subject, RDF_REST, &rest);
            } else {
                add_triple(&store, &subject, RDF_REST, RDF_NIL);
            }
        }

        let result = validator
            .validate_list(&store, "http://example.com/list0")
            .expect("Validation failed");
        assert_eq!(result.len(), 10);
        for i in 0..10 {
            assert_eq!(result[i], format!("{}", i));
        }
    }

    #[test]
    fn test_list_with_duplicate_values() {
        let store = Store::new().expect("Failed to create store");
        let validator = RdfListValidator::new();

        // Create a list with duplicate values (allowed - it's the chain structure that matters)
        add_triple_with_literal(&store, "http://example.com/list1", RDF_FIRST, "same");
        add_triple(&store, "http://example.com/list1", RDF_REST, "http://example.com/list2");

        add_triple_with_literal(&store, "http://example.com/list2", RDF_FIRST, "same");
        add_triple(&store, "http://example.com/list2", RDF_REST, "http://example.com/list3");

        add_triple_with_literal(&store, "http://example.com/list3", RDF_FIRST, "same");
        add_triple(&store, "http://example.com/list3", RDF_REST, RDF_NIL);

        let result = validator
            .validate_list(&store, "http://example.com/list1")
            .expect("Validation failed");
        assert_eq!(result.len(), 3);
        assert!(result.iter().all(|v| v == "same"));
    }
}
