//! SHACL shape definitions and loading
//!
//! This module defines types for representing SHACL shapes and provides
//! a ShapeLoader that uses SPARQL to parse SHACL TTL files.
//!
//! ## Architecture
//!
//! ```text
//! shapes.ttl (SHACL) → ShapeLoader (SPARQL queries) → ShaclShapeSet
//!                                                           ↓
//!                                           Vec<ShaclShape> with PropertyConstraints
//! ```
//!
//! ## Constitution Compliance
//!
//! - ✓ Principle V: Type-First Thinking (strong typing for SHACL concepts)
//! - ✓ Principle VII: Result<T,E> error handling
//! - ✓ Principle IX: Lean Six Sigma (poka-yoke design)

use crate::graph::Graph;
use crate::validation::error::Result;
use crate::validation::violation::Severity;
use std::collections::BTreeMap;

/// A property constraint from a SHACL shape
///
/// Represents a single `sh:property` block with all its constraints.
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyConstraint {
    /// Property path (IRI)
    pub path: String,
    /// Minimum cardinality (sh:minCount)
    pub min_count: Option<u32>,
    /// Maximum cardinality (sh:maxCount)
    pub max_count: Option<u32>,
    /// Expected datatype IRI (sh:datatype)
    pub datatype: Option<String>,
    /// Allowed values for enumeration (sh:in)
    pub allowed_values: Option<Vec<String>>,
    /// Regex pattern (sh:pattern)
    pub pattern: Option<String>,
    /// Minimum string length (sh:minLength)
    pub min_length: Option<u32>,
    /// Maximum string length (sh:maxLength)
    pub max_length: Option<u32>,
    /// Validation severity
    pub severity: Severity,
    /// Custom error message (sh:message)
    pub message: Option<String>,
}

impl PropertyConstraint {
    pub fn new(path: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            min_count: None,
            max_count: None,
            datatype: None,
            allowed_values: None,
            pattern: None,
            min_length: None,
            max_length: None,
            severity: Severity::Violation,
            message: None,
        }
    }
}

/// A SHACL NodeShape with its property constraints
#[derive(Debug, Clone, PartialEq)]
pub struct ShaclShape {
    /// Shape IRI
    pub iri: String,
    /// Target class IRI (sh:targetClass)
    pub target_class: String,
    /// Property constraints indexed by property path
    pub properties: BTreeMap<String, PropertyConstraint>,
    /// Shape-level severity
    pub severity: Option<Severity>,
}

impl ShaclShape {
    pub fn new(iri: impl Into<String>, target_class: impl Into<String>) -> Self {
        Self {
            iri: iri.into(),
            target_class: target_class.into(),
            properties: BTreeMap::new(),
            severity: None,
        }
    }
}

/// Collection of SHACL shapes
#[derive(Debug, Clone)]
pub struct ShaclShapeSet {
    /// Shapes indexed by shape IRI
    pub shapes: BTreeMap<String, ShaclShape>,
}

impl ShaclShapeSet {
    pub fn new() -> Self {
        Self {
            shapes: BTreeMap::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.shapes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.shapes.is_empty()
    }
}

impl Default for ShaclShapeSet {
    fn default() -> Self {
        Self::new()
    }
}

/// Loads SHACL shapes from RDF graphs using SPARQL queries
pub struct ShapeLoader;

impl ShapeLoader {
    pub fn new() -> Self {
        Self
    }

    /// Load all SHACL shapes from a graph using SPARQL queries.
    ///
    /// ## Algorithm
    ///
    /// 1. Find all `sh:NodeShape`s with `sh:targetClass`
    /// 2. For each shape, find all `sh:property` constraints
    /// 3. Load constraint fields (minCount, maxCount, datatype, pattern, etc.)
    /// 4. Parse allowed values for `sh:in` enumerations
    /// 5. Parse shape-level severity
    pub fn load(&self, graph: &Graph) -> Result<ShaclShapeSet> {
        let mut shape_set = ShaclShapeSet::new();

        // Step 1: Find all NodeShapes with targetClass
        let find_shapes_query = r#"
            PREFIX sh: <http://www.w3.org/ns/shacl#>
            SELECT ?shape ?targetClass WHERE {
                ?shape a sh:NodeShape .
                ?shape sh:targetClass ?targetClass .
            }
        "#;

        let shape_rows = match graph.query_cached(find_shapes_query) {
            Ok(crate::graph::CachedResult::Solutions(rows)) => rows,
            Ok(_) => return Ok(shape_set),
            Err(_) => return Ok(shape_set),
        };

        for row in &shape_rows {
            let shape_iri = strip_iri_brackets(row.get("shape").map(|s| s.as_str()).unwrap_or("")).to_string();
            let target_class = strip_iri_brackets(row.get("targetClass").map(|s| s.as_str()).unwrap_or("")).to_string();

            if shape_iri.is_empty() || target_class.is_empty() {
                continue;
            }

            let mut shape = ShaclShape::new(&shape_iri, &target_class);

            // Step 2: Find all property constraints for this shape
            let find_properties_query = format!(
                r#"
                    PREFIX sh: <http://www.w3.org/ns/shacl#>
                    SELECT ?property ?path WHERE {{
                        <{shape_iri}> sh:property ?property .
                        ?property sh:path ?path .
                    }}
                "#
            );

            let prop_rows = match graph.query_cached(&find_properties_query) {
                Ok(crate::graph::CachedResult::Solutions(rows)) => rows,
                Ok(_) => continue,
                Err(_) => continue,
            };

            for prop_row in &prop_rows {
                let property_iri = strip_iri_brackets(prop_row.get("property").map(|s| s.as_str()).unwrap_or("")).to_string();
                let path = strip_iri_brackets(prop_row.get("path").map(|s| s.as_str()).unwrap_or("")).to_string();

                if path.is_empty() {
                    continue;
                }

                let mut constraint = PropertyConstraint::new(&path);

                // Step 3: Load constraint fields for this property
                self.load_constraint_fields(graph, &property_iri, &mut constraint);

                // Step 4: Load allowed values for sh:in
                self.load_allowed_values(graph, &property_iri, &mut constraint);

                shape.properties.insert(path.clone(), constraint);
            }

            // Step 5: Parse shape-level severity
            shape.severity = self.load_shape_severity(graph, &shape_iri);

            shape_set.shapes.insert(shape_iri, shape);
        }

        Ok(shape_set)
    }

    /// Load individual constraint fields (minCount, maxCount, datatype, pattern, etc.)
    fn load_constraint_fields(
        &self, graph: &Graph, property_iri: &str, constraint: &mut PropertyConstraint,
    ) {
        let fields_query = format!(
            r#"
                PREFIX sh: <http://www.w3.org/ns/shacl#>
                SELECT ?field ?value WHERE {{
                    <{property_iri}> ?field ?value .
                    FILTER (?field IN (sh:minCount, sh:maxCount, sh:datatype, sh:pattern, sh:minLength, sh:maxLength, sh:message, sh:severity))
                }}
            "#
        );

        let rows = match graph.query_cached(&fields_query) {
            Ok(crate::graph::CachedResult::Solutions(rows)) => rows,
            _ => return,
        };

        for row in &rows {
            let field = strip_iri_brackets(row.get("field").map(|s| s.as_str()).unwrap_or(""));
            let value = row.get("value").cloned().unwrap_or_default();

            match field {
                "http://www.w3.org/ns/shacl#minCount" => {
                    if let Ok(n) = strip_literal_quotes(&value).parse::<u32>() {
                        constraint.min_count = Some(n);
                    }
                }
                "http://www.w3.org/ns/shacl#maxCount" => {
                    if let Ok(n) = strip_literal_quotes(&value).parse::<u32>() {
                        constraint.max_count = Some(n);
                    }
                }
                "http://www.w3.org/ns/shacl#datatype" => {
                    constraint.datatype = Some(strip_iri_brackets(&value).to_string());
                }
                "http://www.w3.org/ns/shacl#pattern" => {
                    constraint.pattern = Some(strip_literal_quotes(&value).to_string());
                }
                "http://www.w3.org/ns/shacl#minLength" => {
                    if let Ok(n) = strip_literal_quotes(&value).parse::<u32>() {
                        constraint.min_length = Some(n);
                    }
                }
                "http://www.w3.org/ns/shacl#maxLength" => {
                    if let Ok(n) = strip_literal_quotes(&value).parse::<u32>() {
                        constraint.max_length = Some(n);
                    }
                }
                "http://www.w3.org/ns/shacl#message" => {
                    constraint.message = Some(strip_literal_quotes(&value).to_string());
                }
                "http://www.w3.org/ns/shacl#severity" => {
                    constraint.severity = parse_severity(strip_iri_brackets(&value));
                }
                _ => {}
            }
        }
    }

    /// Load allowed values for sh:in enumeration constraint
    fn load_allowed_values(
        &self, graph: &Graph, property_iri: &str, constraint: &mut PropertyConstraint,
    ) {
        let in_query = format!(
            r#"
                PREFIX sh: <http://www.w3.org/ns/shacl#>
                SELECT ?value WHERE {{
                    <{property_iri}> sh:in ?list .
                    ?list rdf:rest*/rdf:first ?value .
                }}
            "#
        );

        let rows = match graph.query_cached(&in_query) {
            Ok(crate::graph::CachedResult::Solutions(rows)) => rows,
            _ => return,
        };

        let values: Vec<String> = rows
            .iter()
            .filter_map(|row| row.get("value").map(|v| strip_literal_quotes(v).to_string()))
            .collect();

        if !values.is_empty() {
            constraint.allowed_values = Some(values);
        }
    }

    /// Load shape-level severity from sh:severity
    fn load_shape_severity(&self, graph: &Graph, shape_iri: &str) -> Option<Severity> {
        let severity_query = format!(
            r#"
                PREFIX sh: <http://www.w3.org/ns/shacl#>
                SELECT ?severity WHERE {{
                    <{shape_iri}> sh:severity ?severity .
                }}
            "#
        );

        match graph.query_cached(&severity_query) {
            Ok(crate::graph::CachedResult::Solutions(rows)) => rows
                .first()
                .and_then(|row| row.get("severity"))
                .map(|s| parse_severity(s)),
            _ => None,
        }
    }
}

impl Default for ShapeLoader {
    fn default() -> Self {
        Self::new()
    }
}

/// Strip angle brackets from an IRI string returned by SPARQL.
///
/// Oxigraph's `Term::to_string()` wraps IRIs in `<...>`, e.g. `<http://example.org#Person>`.
/// This helper removes those brackets for cleaner comparison.
fn strip_iri_brackets(s: &str) -> &str {
    s.strip_prefix('<').and_then(|s| s.strip_suffix('>')).unwrap_or(s)
}

/// Strip surrounding quotes from a literal string returned by SPARQL.
///
/// Oxigraph's `Term::to_string()` wraps literals in quotes, e.g. `"Alice"`.
/// This helper removes the outermost quotes.
fn strip_literal_quotes(s: &str) -> &str {
    s.strip_prefix('"').and_then(|s| s.strip_suffix('"')).unwrap_or(s)
}

/// Parse a SHACL severity IRI into a Severity enum
fn parse_severity(iri: &str) -> Severity {
    match iri {
        "http://www.w3.org/ns/shacl#Warning" => Severity::Warning,
        "http://www.w3.org/ns/shacl#Info" => Severity::Info,
        _ => Severity::Violation,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_property_constraint_new() {
        let c = PropertyConstraint::new("http://example.org#name");
        assert_eq!(c.path, "http://example.org#name");
        assert_eq!(c.min_count, None);
        assert_eq!(c.severity, Severity::Violation);
    }

    #[test]
    fn test_shacl_shape_new() {
        let s = ShaclShape::new("http://example.org#PersonShape", "http://example.org#Person");
        assert_eq!(s.iri, "http://example.org#PersonShape");
        assert_eq!(s.target_class, "http://example.org#Person");
        assert!(s.properties.is_empty());
        assert!(s.severity.is_none());
    }

    #[test]
    fn test_shacl_shape_set_default() {
        let set = ShaclShapeSet::default();
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
    }

    #[test]
    fn test_parse_severity_violation() {
        assert_eq!(
            parse_severity("http://www.w3.org/ns/shacl#Violation"),
            Severity::Violation
        );
    }

    #[test]
    fn test_parse_severity_warning() {
        assert_eq!(
            parse_severity("http://www.w3.org/ns/shacl#Warning"),
            Severity::Warning
        );
    }

    #[test]
    fn test_parse_severity_info() {
        assert_eq!(
            parse_severity("http://www.w3.org/ns/shacl#Info"),
            Severity::Info
        );
    }

    #[test]
    fn test_parse_severity_unknown() {
        assert_eq!(parse_severity("http://unknown.org#Something"), Severity::Violation);
    }

    #[test]
    fn test_shape_loader_loads_from_graph() {
        let ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [
                    sh:path ex:name ;
                    sh:minCount 1 ;
                    sh:datatype xsd:string ;
                    sh:minLength 1 ;
                    sh:severity sh:Violation ;
                    sh:message "Name is required" ;
                ] ;
                sh:property [
                    sh:path ex:age ;
                    sh:datatype xsd:integer ;
                    sh:maxCount 1 ;
                ] .
        "#;

        let graph = Graph::load_from_string(ttl).expect("load graph");
        let loader = ShapeLoader::new();
        let set = loader.load(&graph).expect("load shapes");

        assert!(!set.is_empty());
        assert!(set.shapes.contains_key("http://example.org#PersonShape"));

        let shape = &set.shapes["http://example.org#PersonShape"];
        assert_eq!(shape.target_class, "http://example.org#Person");
        assert_eq!(shape.properties.len(), 2);

        // Check name constraint
        let name_constraint = &shape.properties["http://example.org#name"];
        assert_eq!(name_constraint.min_count, Some(1));
        assert_eq!(
            name_constraint.datatype.as_deref(),
            Some("http://www.w3.org/2001/XMLSchema#string")
        );
        assert_eq!(name_constraint.min_length, Some(1));
        assert_eq!(name_constraint.message.as_deref(), Some("Name is required"));

        // Check age constraint
        let age_constraint = &shape.properties["http://example.org#age"];
        assert_eq!(age_constraint.max_count, Some(1));
        assert_eq!(
            age_constraint.datatype.as_deref(),
            Some("http://www.w3.org/2001/XMLSchema#integer")
        );
    }

    #[test]
    fn test_shape_loader_loads_enumeration() {
        let ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

            ex:TaskShape a sh:NodeShape ;
                sh:targetClass ex:Task ;
                sh:property [
                    sh:path ex:priority ;
                    sh:in ( "P1" "P2" "P3" ) ;
                ] .
        "#;

        let graph = Graph::load_from_string(ttl).expect("load graph");
        let loader = ShapeLoader::new();
        let set = loader.load(&graph).expect("load shapes");

        let shape = &set.shapes["http://example.org#TaskShape"];
        let priority = &shape.properties["http://example.org#priority"];
        let values = priority.allowed_values.as_ref().expect("should have allowed values");
        assert_eq!(values.len(), 3);
        assert!(values.contains(&"P1".to_string()));
        assert!(values.contains(&"P2".to_string()));
        assert!(values.contains(&"P3".to_string()));
    }

    #[test]
    fn test_shape_loader_loads_shape_severity() {
        let ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:LenientShape a sh:NodeShape ;
                sh:targetClass ex:Item ;
                sh:severity sh:Warning ;
                sh:property [
                    sh:path ex:description ;
                    sh:minCount 1 ;
                ] .
        "#;

        let graph = Graph::load_from_string(ttl).expect("load graph");
        let loader = ShapeLoader::new();
        let set = loader.load(&graph).expect("load shapes");

        let shape = &set.shapes["http://example.org#LenientShape"];
        assert_eq!(shape.severity, Some(Severity::Warning));
    }

    #[test]
    fn test_shape_loader_empty_graph() {
        let graph = Graph::new().expect("new graph");
        let loader = ShapeLoader::new();
        let set = loader.load(&graph).expect("load shapes");
        assert!(set.is_empty());
    }

    #[test]
    fn test_shape_loader_pattern_constraint() {
        let ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:CodeShape a sh:NodeShape ;
                sh:targetClass ex:Code ;
                sh:property [
                    sh:path ex:id ;
                    sh:pattern "^[A-Z]{3}-[0-9]+$" ;
                    sh:maxLength 20 ;
                ] .
        "#;

        let graph = Graph::load_from_string(ttl).expect("load graph");
        let loader = ShapeLoader::new();
        let set = loader.load(&graph).expect("load shapes");

        let shape = &set.shapes["http://example.org#CodeShape"];
        let id_constraint = &shape.properties["http://example.org#id"];
        assert_eq!(
            id_constraint.pattern.as_deref(),
            Some("^[A-Z]{3}-[0-9]+$")
        );
        assert_eq!(id_constraint.max_length, Some(20));
    }

    #[test]
    fn test_shape_loader_multiple_shapes() {
        let ttl = r#"
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org#> .

            ex:PersonShape a sh:NodeShape ;
                sh:targetClass ex:Person ;
                sh:property [ sh:path ex:name ; sh:minCount 1 ] .

            ex:OrgShape a sh:NodeShape ;
                sh:targetClass ex:Organization ;
                sh:property [ sh:path ex:title ; sh:minCount 1 ] .
        "#;

        let graph = Graph::load_from_string(ttl).expect("load graph");
        let loader = ShapeLoader::new();
        let set = loader.load(&graph).expect("load shapes");

        assert_eq!(set.len(), 2);
        assert!(set.shapes.contains_key("http://example.org#PersonShape"));
        assert!(set.shapes.contains_key("http://example.org#OrgShape"));
    }
}
