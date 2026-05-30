//! SHACL shape definitions and loading
//!
//! This module defines types for representing SHACL shapes and provides
//! a ShapeLoader that uses SPARQL to parse SHACL TTL files.
//!
//! ## Architecture
//!
//! \`\`\`text
//! shapes.ttl (SHACL) → ShapeLoader (SPARQL queries) → ShaclShapeSet
//!                                                           ↓
//!                                           Vec<ShaclShape> with PropertyConstraints
//! \`\`\`
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
/// Represents a single \`sh:property\` block with all its constraints.
/// PartialEq without Eq: All fields (u32, String, Option, Vec, Severity) implement Eq
#[allow(clippy::derive_partial_eq_without_eq)]
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
/// PartialEq without Eq: All fields (String, BTreeMap, Option, Severity) implement Eq
#[allow(clippy::derive_partial_eq_without_eq)]
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

    pub fn is_empty(&self) -> bool {
        self.shapes.is_empty()
    }

    pub fn len(&self) -> usize {
        self.shapes.len()
    }
}

/// Loads SHACL shapes from RDF graphs using SPARQL queries
pub struct ShapeLoader;

impl ShapeLoader {
    pub fn new() -> Self {
        Self
    }

    /// Load all SHACL shapes from a graph using SPARQL queries.
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
            _ => return Ok(shape_set),
        };

        for row in &shape_rows {
            let shape_iri =
                strip_iri_brackets(row.get("shape").map(|s| s.as_str()).unwrap_or("")).to_string();
            let target_class =
                strip_iri_brackets(row.get("targetClass").map(|s| s.as_str()).unwrap_or(""))
                    .to_string();

            if shape_iri.is_empty() || target_class.is_empty() {
                continue;
            }

            let mut shape = ShaclShape::new(&shape_iri, &target_class);

            // Step 2: Find all property constraints for this shape and their fields in a
            // SINGLE query. The `sh:property` objects are blank nodes; a blank-node label
            // from one query result is NOT a stable reference usable as a subject in a
            // later query (it becomes a fresh existential variable, matching ANY node).
            // Keeping `?property` as a join variable here binds each property's path to
            // its own fields, avoiding cross-contamination between property shapes.
            let find_properties_query = format!(
                r#"
                    PREFIX sh: <http://www.w3.org/ns/shacl#>
                    SELECT ?path ?field ?value WHERE {{
                        {} sh:property ?property .
                        ?property sh:path ?path .
                        ?property ?field ?value .
                        FILTER (?field IN (sh:minCount, sh:maxCount, sh:datatype, sh:pattern, sh:minLength, sh:maxLength, sh:message, sh:severity))
                    }}
                "#,
                format_term_for_sparql(&shape_iri)
            );

            let prop_rows = match graph.query_cached(&find_properties_query) {
                Ok(crate::graph::CachedResult::Solutions(rows)) => rows,
                _ => continue,
            };

            for prop_row in &prop_rows {
                let path =
                    strip_iri_brackets(prop_row.get("path").map(|s| s.as_str()).unwrap_or(""))
                        .to_string();

                if path.is_empty() {
                    continue;
                }

                let field =
                    strip_iri_brackets(prop_row.get("field").map(|s| s.as_str()).unwrap_or(""))
                        .to_string();
                let value = prop_row.get("value").cloned().unwrap_or_default();

                let constraint = shape
                    .properties
                    .entry(path.clone())
                    .or_insert_with(|| PropertyConstraint::new(&path));
                apply_constraint_field(constraint, &field, &value);
            }

            // Step 3: Load sh:in allowed values per property (path → values), keeping
            // `?property` as a join variable so blank-node property shapes resolve.
            let in_query = format!(
                r#"
                    PREFIX sh: <http://www.w3.org/ns/shacl#>
                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                    SELECT ?path ?value WHERE {{
                        {} sh:property ?property .
                        ?property sh:path ?path .
                        ?property sh:in/rdf:rest*/rdf:first ?value .
                    }}
                "#,
                format_term_for_sparql(&shape_iri)
            );
            if let Ok(crate::graph::CachedResult::Solutions(in_rows)) =
                graph.query_cached(&in_query)
            {
                for in_row in &in_rows {
                    let path =
                        strip_iri_brackets(in_row.get("path").map(|s| s.as_str()).unwrap_or(""))
                            .to_string();
                    if path.is_empty() {
                        continue;
                    }
                    if let Some(value) = in_row.get("value") {
                        let constraint = shape
                            .properties
                            .entry(path.clone())
                            .or_insert_with(|| PropertyConstraint::new(&path));
                        constraint
                            .allowed_values
                            .get_or_insert_with(Vec::new)
                            .push(strip_literal_quotes(value).to_string());
                    }
                }
            }

            // Step 5: Parse shape-level severity
            shape.severity = self.load_shape_severity(graph, &shape_iri);

            shape_set.shapes.insert(shape_iri, shape);
        }

        Ok(shape_set)
    }

    /// Load shape-level severity
    fn load_shape_severity(&self, graph: &Graph, shape_iri: &str) -> Option<Severity> {
        let severity_query = format!(
            r#"
                PREFIX sh: <http://www.w3.org/ns/shacl#>
                SELECT ?severity WHERE {{
                    {} sh:severity ?severity .
                }}
            "#,
            format_term_for_sparql(shape_iri)
        );

        match graph.query_cached(&severity_query) {
            Ok(crate::graph::CachedResult::Solutions(rows)) => rows
                .first()
                .and_then(|row| row.get("severity"))
                .map(|v| parse_severity(strip_iri_brackets(v))),
            _ => None,
        }
    }
}

/// Apply a single `sh:*` constraint field/value pair to a `PropertyConstraint`.
fn apply_constraint_field(constraint: &mut PropertyConstraint, field: &str, value: &str) {
    match field {
        "http://www.w3.org/ns/shacl#minCount" => {
            if let Ok(n) = strip_literal_quotes(value).parse::<u32>() {
                constraint.min_count = Some(n);
            }
        }
        "http://www.w3.org/ns/shacl#maxCount" => {
            if let Ok(n) = strip_literal_quotes(value).parse::<u32>() {
                constraint.max_count = Some(n);
            }
        }
        "http://www.w3.org/ns/shacl#datatype" => {
            constraint.datatype = Some(strip_iri_brackets(value).to_string());
        }
        "http://www.w3.org/ns/shacl#pattern" => {
            constraint.pattern = Some(strip_literal_quotes(value).to_string());
        }
        "http://www.w3.org/ns/shacl#minLength" => {
            if let Ok(n) = strip_literal_quotes(value).parse::<u32>() {
                constraint.min_length = Some(n);
            }
        }
        "http://www.w3.org/ns/shacl#maxLength" => {
            if let Ok(n) = strip_literal_quotes(value).parse::<u32>() {
                constraint.max_length = Some(n);
            }
        }
        "http://www.w3.org/ns/shacl#message" => {
            constraint.message = Some(strip_literal_quotes(value).to_string());
        }
        "http://www.w3.org/ns/shacl#severity" => {
            constraint.severity = parse_severity(strip_iri_brackets(value));
        }
        _ => {}
    }
}

/// Strip surrounding angle brackets from an IRI returned by SPARQL.
fn strip_iri_brackets(s: &str) -> &str {
    s.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s)
}

/// Strip surrounding quotes from a literal string returned by SPARQL.
/// Handles datatypes and language tags (e.g. "Alice"@en -> Alice).
fn strip_literal_quotes(s: &str) -> &str {
    if let Some(inner) = s.strip_prefix('"') {
        if let Some(end_quote_idx) = inner.rfind('"') {
            return &inner[..end_quote_idx];
        }
    }
    s
}

/// Format a term for use in a SPARQL query.
/// Wraps IRIs in <> and leaves blank nodes (starting with _:) as-is.
fn format_term_for_sparql(s: &str) -> String {
    if s.starts_with("_:") {
        s.to_string()
    } else if s.starts_with('<') && s.ends_with('>') {
        s.to_string()
    } else {
        format!("<{}>", s)
    }
}

/// Parse a SHACL severity IRI into a Severity enum
fn parse_severity(iri: &str) -> Severity {
    match iri {
        "http://www.w3.org/ns/shacl#Warning" => Severity::Warning,
        "http://www.w3.org/ns/shacl#Info" => Severity::Info,
        _ => Severity::Violation,
    }
}
