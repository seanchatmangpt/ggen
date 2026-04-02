//! SPARQL-based SHACL validator
//!
//! This module implements SHACL validation by translating SHACL shapes into SPARQL queries.
//! It follows Constitution Principle IX (Lean Six Sigma - poka-yoke design).

use crate::graph::{CachedResult, Graph};
use crate::validation::error::Result;
use crate::validation::shacl::{ShaclShape, ShapeLoader};
use crate::validation::violation::{ConstraintType, ValidationResult, Violation};
use std::time::Instant;

/// SPARQL-based SHACL validator
///
/// Validates RDF graphs against SHACL shapes by translating constraints into SPARQL queries.
#[derive(Debug, Clone)]
pub struct SparqlValidator {
    /// Maximum validation duration in milliseconds (default: 30000ms = 30s)
    timeout_ms: u64,
}

impl SparqlValidator {
    /// Create a new SPARQL validator with default timeout (30s)
    pub fn new() -> Self {
        Self { timeout_ms: 30000 }
    }

    /// Set the validation timeout in milliseconds
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = timeout_ms;
        self
    }

    /// Validate an RDF graph against SHACL shapes.
    ///
    /// Loads shapes from the `shapes` graph, then for each shape:
    /// 1. Find all instances of the target class
    /// 2. Check each property constraint via SPARQL
    /// 3. Collect violations
    pub fn validate(&self, ontology: &Graph, shapes: &Graph) -> Result<ValidationResult> {
        let start = Instant::now();
        let mut violations = Vec::new();

        // Load shapes from the shapes graph
        let loader = ShapeLoader::new();
        let shape_set = loader.load(shapes)?;

        for (_iri, shape) in &shape_set.shapes {
            // Check timeout
            if start.elapsed().as_millis() as u64 > self.timeout_ms {
                return Err(crate::validation::error::ValidationError::timeout_error(
                    start.elapsed().as_millis() as u64,
                    self.timeout_ms,
                ));
            }

            self.validate_shape(ontology, shape, &mut violations);
        }

        let duration_ms = start.elapsed().as_millis() as u64;

        if violations.is_empty() {
            Ok(ValidationResult::pass(duration_ms))
        } else {
            Ok(ValidationResult::fail(violations, duration_ms))
        }
    }

    /// Validate a single shape against the ontology graph
    fn validate_shape(&self, graph: &Graph, shape: &ShaclShape, violations: &mut Vec<Violation>) {
        for (path, constraint) in &shape.properties {
            let effective_severity =
                constraint.severity;

            // sh:minCount — required property check
            if let Some(min_count) = constraint.min_count {
                if min_count > 0 {
                    let nodes = self.find_missing_property_nodes(graph, &shape.target_class, path);
                    for node in &nodes {
                        let msg = constraint
                            .message
                            .clone()
                            .unwrap_or_else(|| format!("Missing required property {path} on {node}"));
                        violations.push(
                            Violation::new(node, ConstraintType::Cardinality, msg)
                                .with_result_path(path)
                                .with_source_shape(&shape.iri)
                                .with_severity(effective_severity),
                        );
                    }
                }
            }

            // sh:maxCount — too many values check
            if let Some(max_count) = constraint.max_count {
                let nodes = self.find_over_cardinality_nodes(
                    graph,
                    &shape.target_class,
                    path,
                    max_count,
                );
                for (node, actual) in &nodes {
                    let msg = constraint.message.clone().unwrap_or_else(|| {
                        format!(
                            "Property {path} on {node} has {actual} values (max {max_count})"
                        )
                    });
                    violations.push(
                        Violation::new(node, ConstraintType::Cardinality, msg)
                            .with_result_path(path)
                            .with_source_shape(&shape.iri)
                            .with_severity(effective_severity)
                            .with_values(max_count.to_string(), actual.to_string()),
                    );
                }
            }

            // sh:datatype — type validation
            if let Some(ref datatype) = constraint.datatype {
                let nodes =
                    self.find_wrong_datatype_nodes(graph, &shape.target_class, path, datatype);
                for (node, actual_type) in &nodes {
                    let msg = constraint.message.clone().unwrap_or_else(|| {
                        format!(
                            "Property {path} on {node} has wrong datatype: expected {datatype}, got {actual_type}"
                        )
                    });
                    violations.push(
                        Violation::new(node, ConstraintType::Datatype, msg)
                            .with_result_path(path)
                            .with_source_shape(&shape.iri)
                            .with_severity(effective_severity)
                            .with_values(datatype.clone(), actual_type.clone()),
                    );
                }
            }

            // sh:in — enumeration check
            if let Some(ref allowed) = constraint.allowed_values {
                let nodes = self.find_invalid_enumeration_nodes(
                    graph,
                    &shape.target_class,
                    path,
                    allowed,
                );
                for (node, actual) in &nodes {
                    let allowed_str = allowed.join(", ");
                    let msg = constraint.message.clone().unwrap_or_else(|| {
                        format!(
                            "Property {path} on {node} has invalid value \"{actual}\" (allowed: [{allowed_str}])"
                        )
                    });
                    violations.push(
                        Violation::new(node, ConstraintType::Enumeration, msg)
                            .with_result_path(path)
                            .with_source_shape(&shape.iri)
                            .with_severity(effective_severity)
                            .with_values(format!("[{allowed_str}]"), actual.clone()),
                    );
                }
            }

            // sh:pattern — regex validation
            if let Some(ref pattern) = constraint.pattern {
                let nodes =
                    self.find_pattern_violations(graph, &shape.target_class, path, pattern);
                for (node, actual) in &nodes {
                    let msg = constraint.message.clone().unwrap_or_else(|| {
                        format!(
                            "Property {path} on {node} does not match pattern \"{pattern}\": \"{actual}\""
                        )
                    });
                    violations.push(
                        Violation::new(node, ConstraintType::Pattern, msg)
                            .with_result_path(path)
                            .with_source_shape(&shape.iri)
                            .with_severity(effective_severity)
                            .with_values(format!("~/{pattern}/"), actual.clone()),
                    );
                }
            }

            // sh:minLength — minimum string length
            if let Some(min_len) = constraint.min_length {
                let nodes = self.find_too_short_nodes(
                    graph,
                    &shape.target_class,
                    path,
                    min_len,
                );
                for (node, actual) in &nodes {
                    let msg = constraint.message.clone().unwrap_or_else(|| {
                        format!(
                            "Property {path} on {node} is too short: {actual} chars (min {min_len})"
                        )
                    });
                    violations.push(
                        Violation::new(node, ConstraintType::StringLength, msg)
                            .with_result_path(path)
                            .with_source_shape(&shape.iri)
                            .with_severity(effective_severity)
                            .with_values(min_len.to_string(), actual.to_string()),
                    );
                }
            }

            // sh:maxLength — maximum string length
            if let Some(max_len) = constraint.max_length {
                let nodes = self.find_too_long_nodes(
                    graph,
                    &shape.target_class,
                    path,
                    max_len,
                );
                for (node, actual) in &nodes {
                    let msg = constraint.message.clone().unwrap_or_else(|| {
                        format!(
                            "Property {path} on {node} is too long: {actual} chars (max {max_len})"
                        )
                    });
                    violations.push(
                        Violation::new(node, ConstraintType::StringLength, msg)
                            .with_result_path(path)
                            .with_source_shape(&shape.iri)
                            .with_severity(effective_severity)
                            .with_values(max_len.to_string(), actual.to_string()),
                    );
                }
            }
        }
    }

    /// Find nodes that are missing a required property (sh:minCount)
    fn find_missing_property_nodes(
        &self, graph: &Graph, target_class: &str, path: &str,
    ) -> Vec<String> {
        let query = format!(
            r#"
                SELECT ?node WHERE {{
                    ?node a <{target_class}> .
                    FILTER NOT EXISTS {{ ?node <{path}> ?value }}
                }}
            "#
        );

        match graph.query_cached(&query) {
            Ok(CachedResult::Solutions(rows)) => rows
                .iter()
                .filter_map(|row| row.get("node").cloned())
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Find nodes where a property exceeds max cardinality (sh:maxCount)
    fn find_over_cardinality_nodes(
        &self, graph: &Graph, target_class: &str, path: &str, max_count: u32,
    ) -> Vec<(String, String)> {
        let query = format!(
            r#"
                SELECT ?node (COUNT(?value) AS ?count) WHERE {{
                    ?node a <{target_class}> .
                    ?node <{path}> ?value .
                }}
                GROUP BY ?node
                HAVING (COUNT(?value) > {max_count})
            "#
        );

        match graph.query_cached(&query) {
            Ok(CachedResult::Solutions(rows)) => rows
                .iter()
                .filter_map(|row| {
                    let node = row.get("node").cloned()?;
                    let count = row.get("count").cloned()?;
                    Some((node, count))
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Find nodes where a property has the wrong datatype (sh:datatype)
    fn find_wrong_datatype_nodes(
        &self, graph: &Graph, target_class: &str, path: &str, expected_datatype: &str,
    ) -> Vec<(String, String)> {
        let query = format!(
            r#"
                SELECT ?node ?actualType WHERE {{
                    ?node a <{target_class}> .
                    ?node <{path}> ?value .
                    BIND (datatype(?value) AS ?actualType)
                    FILTER (?actualType != <{expected_datatype}> && ?actualType != <http://www.w3.org/1999/02/22-rdf-syntax-ns#langString>)
                }}
            "#
        );

        match graph.query_cached(&query) {
            Ok(CachedResult::Solutions(rows)) => rows
                .iter()
                .filter_map(|row| {
                    let node = row.get("node").cloned()?;
                    let actual = row.get("actualType").cloned()?;
                    Some((node, actual))
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Find nodes where a property value is not in the allowed enumeration (sh:in)
    fn find_invalid_enumeration_nodes(
        &self, graph: &Graph, target_class: &str, path: &str, allowed: &[String],
    ) -> Vec<(String, String)> {
        let values_list = allowed
            .iter()
            .map(|v| format!("\"{}\"", v.replace('"', "\\\"")))
            .collect::<Vec<_>>()
            .join(", ");

        let query = format!(
            r#"
                SELECT ?node ?value WHERE {{
                    ?node a <{target_class}> .
                    ?node <{path}> ?value .
                    FILTER (?value NOT IN ({values_list}))
                }}
            "#
        );

        match graph.query_cached(&query) {
            Ok(CachedResult::Solutions(rows)) => rows
                .iter()
                .filter_map(|row| {
                    let node = row.get("node").cloned()?;
                    let value = row.get("value").cloned()?;
                    Some((node, value))
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Find nodes where a property value doesn't match a regex pattern (sh:pattern)
    fn find_pattern_violations(
        &self, graph: &Graph, target_class: &str, path: &str, pattern: &str,
    ) -> Vec<(String, String)> {
        let escaped_pattern = pattern.replace('\\', "\\\\").replace("'", "\\'");
        let query = format!(
            r#"
                SELECT ?node ?value WHERE {{
                    ?node a <{target_class}> .
                    ?node <{path}> ?value .
                    FILTER (!REGEX(STR(?value), '{escaped_pattern}'))
                }}
            "#
        );

        match graph.query_cached(&query) {
            Ok(CachedResult::Solutions(rows)) => rows
                .iter()
                .filter_map(|row| {
                    let node = row.get("node").cloned()?;
                    let value = row.get("value").cloned()?;
                    Some((node, value))
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Find nodes where a property value is too short (sh:minLength)
    fn find_too_short_nodes(
        &self, graph: &Graph, target_class: &str, path: &str, min_length: u32,
    ) -> Vec<(String, String)> {
        let query = format!(
            r#"
                SELECT ?node ?length WHERE {{
                    ?node a <{target_class}> .
                    ?node <{path}> ?value .
                    BIND (STRLEN(STR(?value)) AS ?length)
                    FILTER (?length < {min_length})
                }}
            "#
        );

        match graph.query_cached(&query) {
            Ok(CachedResult::Solutions(rows)) => rows
                .iter()
                .filter_map(|row| {
                    let node = row.get("node").cloned()?;
                    let length = row.get("length").cloned()?;
                    Some((node, length))
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Find nodes where a property value is too long (sh:maxLength)
    fn find_too_long_nodes(
        &self, graph: &Graph, target_class: &str, path: &str, max_length: u32,
    ) -> Vec<(String, String)> {
        let query = format!(
            r#"
                SELECT ?node ?length WHERE {{
                    ?node a <{target_class}> .
                    ?node <{path}> ?value .
                    BIND (STRLEN(STR(?value)) AS ?length)
                    FILTER (?length > {max_length})
                }}
            "#
        );

        match graph.query_cached(&query) {
            Ok(CachedResult::Solutions(rows)) => rows
                .iter()
                .filter_map(|row| {
                    let node = row.get("node").cloned()?;
                    let length = row.get("length").cloned()?;
                    Some((node, length))
                })
                .collect(),
            _ => Vec::new(),
        }
    }
}

impl Default for SparqlValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::validation::violation::Severity;

    /// Helper: load a TTL string into a Graph
    fn graph_from_ttl(ttl: &str) -> Graph {
        Graph::load_from_string(ttl).expect("load graph from TTL")
    }

    #[test]
    fn test_validator_passes_valid_data() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:name ;
                    sh:minCount 1 ;
                    sh:datatype xsd:string ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:alice a ex:Person ;
                ex:name "Alice" .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(result.passed);
        assert_eq!(result.violation_count, 0);
    }

    #[test]
    fn test_validator_detects_missing_required_property() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:name ;
                    sh:minCount 1 ;
                    sh:message "Every person must have a name" ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:bob a ex:Person .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 1);
        assert_eq!(result.violations[0].constraint_type, ConstraintType::Cardinality);
        assert_eq!(result.violations[0].message, "Every person must have a name");
    }

    #[test]
    fn test_validator_detects_wrong_datatype() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:age ;
                    sh:datatype xsd:integer ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:alice a ex:Person ;
                ex:age "not-a-number" .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 1);
        assert_eq!(result.violations[0].constraint_type, ConstraintType::Datatype);
    }

    #[test]
    fn test_validator_detects_invalid_enumeration() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:TaskShape a sh:NodeShape ;
                sh:targetClass ex:Task ;
                sh:property [
                    sh:path ex:priority ;
                    sh:in ( "P1" "P2" "P3" ) ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:t1 a ex:Task ;
                ex:priority "P1" .

            ex:t2 a ex:Task ;
                ex:priority "INVALID" .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 1);
        assert_eq!(result.violations[0].constraint_type, ConstraintType::Enumeration);
        assert_eq!(result.violations[0].actual_value.as_deref(), Some("INVALID"));
    }

    #[test]
    fn test_validator_detects_pattern_violation() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:CodeShape a sh:NodeShape ;
                sh:targetClass ex:Code ;
                sh:property [
                    sh:path ex:id ;
                    sh:pattern "^[A-Z]{3}-[0-9]+$" ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:c1 a ex:Code ;
                ex:id "ABC-123" .

            ex:c2 a ex:Code ;
                ex:id "invalid" .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 1);
        assert_eq!(result.violations[0].constraint_type, ConstraintType::Pattern);
    }

    #[test]
    fn test_validator_detects_min_length_violation() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:name ;
                    sh:minLength 5 ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:p1 a ex:Person ;
                ex:name "Alice" .

            ex:p2 a ex:Person ;
                ex:name "Al" .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 1);
        assert_eq!(result.violations[0].constraint_type, ConstraintType::StringLength);
    }

    #[test]
    fn test_validator_detects_max_length_violation() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:bio ;
                    sh:maxLength 10 ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:p1 a ex:Person ;
                ex:bio "Short" .

            ex:p2 a ex:Person ;
                ex:bio "This bio is way too long" .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 1);
        assert_eq!(result.violations[0].constraint_type, ConstraintType::StringLength);
    }

    #[test]
    fn test_validator_no_instances_no_violations() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:name ;
                    sh:minCount 1 ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .
            ex:something a ex:OtherThing .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(result.passed);
    }

    #[test]
    fn test_validator_multiple_violations_multiple_nodes() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:name ;
                    sh:minCount 1 ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .

            ex:p1 a ex:Person .
            ex:p2 a ex:Person .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 2);
    }

    #[test]
    fn test_validator_with_timeout() {
        let validator = SparqlValidator::new().with_timeout(5000);
        assert_eq!(validator.timeout_ms, 5000);
    }

    #[test]
    fn test_validator_empty_shapes_graph() {
        let data_ttl = r#"
            @prefix ex: <http://example.org#> .
            ex:alice a ex:Person ; ex:name "Alice" .
        "#;

        let shapes = graph_from_ttl("");
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(result.passed);
    }

    #[test]
    fn test_validator_severity_propagation() {
        let shapes_ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:name ;
                    sh:minCount 1 ;
                    sh:severity sh:Warning ;
                ] .
        "#;

        let data_ttl = r#"
            @prefix ex: <http://example.org#> .
            ex:bob a ex:Person .
        "#;

        let shapes = graph_from_ttl(shapes_ttl);
        let data = graph_from_ttl(data_ttl);
        let validator = SparqlValidator::new();
        let result = validator.validate(&data, &shapes).expect("validate");

        assert!(!result.passed);
        assert_eq!(result.violation_count, 1);
        assert_eq!(result.violations[0].severity, Severity::Warning);
        // Warning violations are not blocking
        assert!(!result.has_blocking_violations());
    }
}
