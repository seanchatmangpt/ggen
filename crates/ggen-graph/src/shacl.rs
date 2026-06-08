//! Structured SHACL validation for LSP diagnostics.
//!
//! Unlike [`crate::dialect::check_shacl`] (which folds everything into one
//! pass/fail [`crate::dialect::DialectResult`]), this returns one
//! [`ShaclViolation`] per failing focus node so a language server can render a
//! diagnostic per violation. It evaluates `sh:minCount` cardinality constraints
//! over `sh:targetClass` instances — the subset ggen enforces at sync time.

use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Term;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;

use crate::GraphError;

/// Severity of a SHACL result, mirroring `sh:severity`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShaclSeverity {
    /// `sh:Violation` (default).
    Violation,
    /// `sh:Warning`.
    Warning,
    /// `sh:Info`.
    Info,
}

/// A single SHACL constraint violation against a focus node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShaclViolation {
    /// The instance IRI that violated the shape.
    pub focus: String,
    /// The shape IRI that was violated.
    pub shape: String,
    /// The property path (IRI) the constraint applies to.
    pub path: String,
    /// Human-readable explanation.
    pub message: String,
    /// Result severity.
    pub severity: ShaclSeverity,
}

fn load(store: &Store, turtle: &str) -> Result<(), GraphError> {
    store
        .load_from_reader(RdfParser::from_format(RdfFormat::Turtle), turtle.as_bytes())
        .map_err(|e| GraphError::Serialization(e.to_string()))
}

fn solutions(
    store: &Store, query: &str,
) -> Result<Vec<oxigraph::sparql::QuerySolution>, GraphError> {
    let results = SparqlEvaluator::new()
        .parse_query(query)
        .map_err(|e| GraphError::Serialization(e.to_string()))?
        .on_store(store)
        .execute()
        .map_err(|e| GraphError::Serialization(e.to_string()))?;
    match results {
        QueryResults::Solutions(sols) => {
            let mut out = Vec::new();
            for sol in sols {
                out.push(sol.map_err(|e| GraphError::Serialization(e.to_string()))?);
            }
            Ok(out)
        }
        _ => Ok(Vec::new()),
    }
}

/// Validate `data` (Turtle) against one or more SHACL `shape_graphs` (Turtle).
///
/// Returns the list of violations; an empty vec means the data conforms. Parse
/// failures in the data or shapes surface as [`GraphError`] (those are reported
/// separately by the located parsers).
pub fn validate_shacl(
    data: &str, shape_graphs: &[&str],
) -> Result<Vec<ShaclViolation>, GraphError> {
    let store = Store::new().map_err(GraphError::Oxigraph)?;
    load(&store, data)?;
    for shapes in shape_graphs {
        load(&store, shapes)?;
    }

    let shape_rows = solutions(
        &store,
        "PREFIX sh: <http://www.w3.org/ns/shacl#>
         SELECT ?shape WHERE { ?shape a sh:NodeShape }",
    )?;

    let mut violations = Vec::new();

    for row in shape_rows {
        let shape_iri = match row.get("shape") {
            Some(Term::NamedNode(n)) => n.as_str().to_string(),
            _ => continue,
        };

        // Resolve sh:targetClass (fall back to the shape IRI itself).
        let target_rows = solutions(
            &store,
            &format!(
                "PREFIX sh: <http://www.w3.org/ns/shacl#>
                 SELECT ?target WHERE {{ <{shape_iri}> sh:targetClass ?target }}"
            ),
        )?;
        let mut targets: Vec<String> = target_rows
            .iter()
            .filter_map(|s| match s.get("target") {
                Some(Term::NamedNode(n)) => Some(n.as_str().to_string()),
                _ => None,
            })
            .collect();
        if targets.is_empty() {
            targets.push(shape_iri.clone());
        }

        // Collect required properties (sh:minCount >= 1) for this shape.
        let prop_rows = solutions(
            &store,
            &format!(
                "PREFIX sh: <http://www.w3.org/ns/shacl#>
                 SELECT ?path ?min WHERE {{ <{shape_iri}> sh:property [ sh:path ?path ; sh:minCount ?min ] }}"
            ),
        )?;
        let mut required: Vec<(String, i64)> = Vec::new();
        for s in &prop_rows {
            let path = match s.get("path") {
                Some(Term::NamedNode(n)) => n.as_str().to_string(),
                _ => continue,
            };
            let min = match s.get("min") {
                Some(Term::Literal(l)) => l.value().parse::<i64>().unwrap_or(0),
                _ => 0,
            };
            if min > 0 {
                required.push((path, min));
            }
        }
        if required.is_empty() {
            continue;
        }

        // Find instances of each target class and check cardinality.
        for target in &targets {
            let inst_rows = solutions(
                &store,
                &format!("SELECT ?inst WHERE {{ ?inst a <{target}> }}"),
            )?;
            for inst_sol in &inst_rows {
                let inst = match inst_sol.get("inst") {
                    Some(Term::NamedNode(n)) => n.as_str().to_string(),
                    _ => continue,
                };
                for (path, min) in &required {
                    let count_rows = solutions(
                        &store,
                        &format!("SELECT (COUNT(?v) AS ?c) WHERE {{ <{inst}> <{path}> ?v }}"),
                    )?;
                    let count = count_rows
                        .first()
                        .and_then(|s| match s.get("c") {
                            Some(Term::Literal(l)) => l.value().parse::<i64>().ok(),
                            _ => None,
                        })
                        .unwrap_or(0);
                    if count < *min {
                        violations.push(ShaclViolation {
                            focus: inst.clone(),
                            shape: shape_iri.clone(),
                            path: path.clone(),
                            message: format!(
                                "property <{path}> requires minCount {min} but found {count}"
                            ),
                            severity: ShaclSeverity::Violation,
                        });
                    }
                }
            }
        }
    }

    Ok(violations)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SHAPES: &str = r"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [ sh:path ex:name ; sh:minCount 1 ] .
    ";

    #[test]
    fn conforming_data_has_no_violations() -> Result<(), GraphError> {
        let data = r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person ; ex:name "Alice" .
        "#;
        let violations = validate_shacl(data, &[SHAPES])?;
        assert!(
            violations.is_empty(),
            "expected conformance, got {violations:?}"
        );
        Ok(())
    }

    #[test]
    fn missing_required_property_is_a_violation() -> Result<(), GraphError> {
        let data = r"
            @prefix ex: <http://example.org/> .
            ex:bob a ex:Person .
        ";
        let violations = validate_shacl(data, &[SHAPES])?;
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].focus, "http://example.org/bob");
        assert_eq!(violations[0].path, "http://example.org/name");
        assert_eq!(violations[0].severity, ShaclSeverity::Violation);
        Ok(())
    }
}
