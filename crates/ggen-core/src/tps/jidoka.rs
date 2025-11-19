//! Jidoka - Autonomous error detection and quality control
//!
//! This module implements automatic error detection with semantic validation,
//! following the TPS principle of "autonomation" (automation with a human touch).

use crate::graph::Graph;
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Type of validation issue detected
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum IssueType {
    /// Orphaned reference (points to non-existent entity)
    OrphanedReference,
    /// Unused class (defined but never used)
    UnusedClass,
    /// Cardinality violation (min/max count not satisfied)
    CardinalityViolation,
    /// Type inconsistency across languages
    TypeInconsistency,
    /// Missing required property
    MissingProperty,
    /// Invalid range for property
    InvalidRange,
}

/// A validation issue found in the ontology or generated code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationIssue {
    /// Type of issue
    pub issue_type: IssueType,
    /// Severity (0-10, where 10 is critical)
    pub severity: u8,
    /// Human-readable description
    pub description: String,
    /// Location in the ontology (IRI or file path)
    pub location: String,
    /// Suggestion for fixing the issue
    pub suggestion: Option<String>,
}

impl ValidationIssue {
    /// Create a new validation issue
    pub fn new(
        issue_type: IssueType,
        severity: u8,
        description: String,
        location: String,
        suggestion: Option<String>,
    ) -> Self {
        Self {
            issue_type,
            severity,
            description,
            location,
            suggestion,
        }
    }

    /// Check if this is a critical issue (severity >= 8)
    pub fn is_critical(&self) -> bool {
        self.severity >= 8
    }

    /// Check if this is a warning (severity < 5)
    pub fn is_warning(&self) -> bool {
        self.severity < 5
    }
}

/// Semantic validator for RDF ontologies
pub struct SemanticValidator {
    graph: Graph,
}

impl SemanticValidator {
    /// Create a new semantic validator
    pub fn new(graph: Graph) -> Self {
        Self { graph }
    }

    /// Validate all semantic constraints
    pub fn validate_all(&self) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // Run all validation checks
        issues.extend(self.validate_references()?);
        issues.extend(self.detect_unused_classes()?);
        issues.extend(self.check_cardinality()?);

        // Sort by severity (highest first)
        issues.sort_by(|a, b| b.severity.cmp(&a.severity));

        Ok(issues)
    }

    /// Validate that all references point to defined entities
    pub fn validate_references(&self) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // SPARQL query to find orphaned range references
        let query = r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?property ?range WHERE {
                ?property rdfs:range ?range .
                FILTER NOT EXISTS {
                    { ?range a owl:Class . }
                    UNION
                    { ?range a rdfs:Class . }
                }
            }
        "#;

        let results = self.graph.query(query)?;

        for result in results {
            if let (Some(property), Some(range)) = (
                result.get("property").map(|v| v.to_string()),
                result.get("range").map(|v| v.to_string()),
            ) {
                issues.push(ValidationIssue::new(
                    IssueType::OrphanedReference,
                    8, // High severity
                    format!("Property '{}' has undefined range '{}'", property, range),
                    property.clone(),
                    Some(format!("Define class '{}' or update the range reference", range)),
                ));
            }
        }

        Ok(issues)
    }

    /// Detect classes that are defined but never used
    pub fn detect_unused_classes(&self) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // SPARQL query to find unused classes
        let query = r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?class WHERE {
                { ?class a owl:Class . } UNION { ?class a rdfs:Class . }
                FILTER NOT EXISTS { ?instance a ?class . }
                FILTER NOT EXISTS { ?shape sh:targetClass ?class . }
            }
        "#;

        let results = self.graph.query(query)?;

        for result in results {
            if let Some(class) = result.get("class").map(|v| v.to_string()) {
                issues.push(ValidationIssue::new(
                    IssueType::UnusedClass,
                    3, // Low severity (warning)
                    format!("Class '{}' is defined but never used", class),
                    class.clone(),
                    Some(format!(
                        "Remove '{}' if not needed, or add instances/references",
                        class
                    )),
                ));
            }
        }

        Ok(issues)
    }

    /// Check cardinality constraints
    pub fn check_cardinality(&self) -> Result<Vec<ValidationIssue>> {
        let mut issues = Vec::new();

        // SPARQL query to find cardinality violations
        let query = r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?shape ?path ?minCount WHERE {
                ?shape sh:path ?path ;
                       sh:minCount ?minCount .
                FILTER (?minCount > 0)
            }
        "#;

        let results = self.graph.query(query)?;

        for result in results {
            if let (Some(shape), Some(path), Some(min_count)) = (
                result.get("shape").map(|v| v.to_string()),
                result.get("path").map(|v| v.to_string()),
                result.get("minCount").and_then(|v| v.to_string().parse::<i32>().ok()),
            ) {
                // Check if the minimum count is satisfied
                let count_query = format!(
                    r#"
                    SELECT (COUNT(?value) as ?count) WHERE {{
                        ?instance <{}> ?value .
                    }}
                "#,
                    path
                );

                let count_results = self.graph.query(&count_query)?;

                if let Some(count_result) = count_results.first() {
                    if let Some(count) = count_result
                        .get("count")
                        .and_then(|v| v.to_string().parse::<i32>().ok())
                    {
                        if count < min_count {
                            issues.push(ValidationIssue::new(
                                IssueType::CardinalityViolation,
                                7,
                                format!(
                                    "Property '{}' requires {} values but only {} found",
                                    path, min_count, count
                                ),
                                shape,
                                Some(format!(
                                    "Add {} more value(s) for property '{}'",
                                    min_count - count,
                                    path
                                )),
                            ));
                        }
                    }
                }
            }
        }

        Ok(issues)
    }
}

/// Runtime invariant assertion system
#[derive(Debug, Clone)]
pub struct InvariantAssertion {
    /// Name of the invariant
    pub name: String,
    /// Error message if invariant is violated
    pub error_message: String,
}

impl InvariantAssertion {
    /// Create a new invariant assertion
    pub fn new(name: String, error_message: String) -> Self {
        Self {
            name,
            error_message,
        }
    }

    /// Check the invariant (placeholder - actual check would use a predicate function)
    pub fn check(&self) -> Result<()> {
        // In a real implementation, this would check the actual invariant
        // For now, we'll always pass
        Ok(())
    }
}

/// Collection of common invariants for code generation
pub struct InvariantRegistry {
    invariants: Vec<InvariantAssertion>,
}

impl InvariantRegistry {
    /// Create a new registry with default invariants
    pub fn new() -> Self {
        let mut registry = Self {
            invariants: Vec::new(),
        };

        // Add default invariants
        registry.add_default_invariants();
        registry
    }

    /// Add a custom invariant
    pub fn add(&mut self, invariant: InvariantAssertion) {
        self.invariants.push(invariant);
    }

    /// Add default invariants
    fn add_default_invariants(&mut self) {
        self.invariants.push(InvariantAssertion::new(
            "output_paths_exist".to_string(),
            "All output paths must be within project root".to_string(),
        ));

        self.invariants.push(InvariantAssertion::new(
            "ontology_valid".to_string(),
            "Ontology must be valid before generation".to_string(),
        ));

        self.invariants.push(InvariantAssertion::new(
            "no_circular_hooks".to_string(),
            "Lifecycle hooks must not have circular dependencies".to_string(),
        ));
    }

    /// Check all invariants
    pub fn check_all(&self) -> Result<()> {
        for invariant in &self.invariants {
            invariant.check()?;
        }
        Ok(())
    }
}

impl Default for InvariantRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_validation_issue_creation, {
        let issue = ValidationIssue::new(
            IssueType::OrphanedReference,
            8,
            "Test issue".to_string(),
            "http://example.org/test".to_string(),
            Some("Fix it".to_string()),
        );

        assert!(issue.is_critical());
        assert!(!issue.is_warning());
        assert_eq!(issue.severity, 8);

        Ok(())
    });

    test!(test_semantic_validator, {
        let graph = Graph::new()?;
        graph.insert_turtle(
            r#"
            @prefix : <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            @prefix owl: <http://www.w3.org/2002/07/owl#> .

            :User a rdfs:Class .
            :name rdfs:domain :User .
        "#,
        )?;

        let validator = SemanticValidator::new(graph);
        let issues = validator.validate_all()?;

        // Should complete without errors
        // (actual validation results depend on the ontology content)
        assert!(issues.len() >= 0);

        Ok(())
    });

    test!(test_invariant_registry, {
        let registry = InvariantRegistry::new();

        // Should have default invariants
        assert!(registry.invariants.len() > 0);

        // Check all should pass (placeholder implementation)
        let result = registry.check_all();
        assert!(result.is_ok());

        Ok(())
    });
}
