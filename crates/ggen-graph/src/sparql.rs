//! SPARQL syntax checking and query-form detection for LSP use.
//!
//! Wraps the oxigraph [`SparqlEvaluator`](oxigraph::sparql::SparqlEvaluator)
//! parser so a language server can validate `.rq`/`.sparql` documents and learn
//! the query form (which ggen's law constrains per rule role).

use oxigraph::sparql::SparqlEvaluator;

/// The SPARQL query form.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SparqlKind {
    /// `SELECT` — required for generation rules.
    Select,
    /// `CONSTRUCT` — used for inference rules.
    Construct,
    /// `ASK` — required for `WHEN` conditions.
    Ask,
    /// `DESCRIBE`.
    Describe,
    /// Could not be determined.
    Unknown,
}

/// Detect the query form by scanning past `PREFIX`/`BASE` headers and comments.
#[must_use]
pub fn sparql_kind(query: &str) -> SparqlKind {
    for raw in query.lines() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let upper = line.to_ascii_uppercase();
        if upper.starts_with("PREFIX") || upper.starts_with("BASE") {
            continue;
        }
        // First meaningful keyword token determines the form.
        if upper.starts_with("SELECT") {
            return SparqlKind::Select;
        } else if upper.starts_with("CONSTRUCT") {
            return SparqlKind::Construct;
        } else if upper.starts_with("ASK") {
            return SparqlKind::Ask;
        } else if upper.starts_with("DESCRIBE") {
            return SparqlKind::Describe;
        }
        // A non-keyword first line means we cannot classify from the prefix scan;
        // fall through to a whole-string contains check below.
        break;
    }
    let upper = query.to_ascii_uppercase();
    if upper.contains("SELECT") {
        SparqlKind::Select
    } else if upper.contains("CONSTRUCT") {
        SparqlKind::Construct
    } else if upper.contains("ASK") {
        SparqlKind::Ask
    } else if upper.contains("DESCRIBE") {
        SparqlKind::Describe
    } else {
        SparqlKind::Unknown
    }
}

/// Validate SPARQL query syntax. Returns the parser error message on failure.
///
/// # Errors
/// Returns the parser's message if `query` is not a syntactically valid SPARQL query.
pub fn check_sparql_syntax(query: &str) -> Result<(), String> {
    SparqlEvaluator::new()
        .parse_query(query)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_query_forms() {
        assert_eq!(
            sparql_kind("PREFIX ex: <http://e/>\nSELECT ?s WHERE { ?s ?p ?o }"),
            SparqlKind::Select
        );
        assert_eq!(
            sparql_kind("ASK { ?s ?p ?o }"),
            SparqlKind::Ask
        );
        assert_eq!(
            sparql_kind("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"),
            SparqlKind::Construct
        );
    }

    #[test]
    fn valid_query_passes_syntax_check() {
        assert!(check_sparql_syntax("SELECT ?s WHERE { ?s ?p ?o }").is_ok());
    }

    #[test]
    fn invalid_query_fails_syntax_check() {
        assert!(check_sparql_syntax("SELECT WHERE {{{").is_err());
    }
}
