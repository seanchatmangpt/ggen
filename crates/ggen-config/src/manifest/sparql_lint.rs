//! Shared pure SPARQL determinism lint: the single source of truth for
//! `E0010` / `E0011` / `E0013`.
//!
//! Before this module the same three laws were implemented twice -- once at
//! author time (`ggen-lsp`'s `SparqlAnalyzer`) and once at sync time
//! (`manifest::validation`). Both now call [`lint_query`]; a differential test
//! in `ggen-lsp/tests/sparql_lint_differential_test.rs` pins that they agree.
//!
//! Severity (strict => error, non-strict => warning) is deliberately NOT
//! decided here: it is the caller's policy. This function reports *what* is
//! violated, never *how hard* to refuse.

/// The role a query plays, which selects which ORDER BY law applies.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryForm {
    /// SELECT-shaped (generation-rule queries): E0013 applies.
    Select,
    /// CONSTRUCT-shaped (inference rules): E0011 applies.
    Construct,
    /// ASK / DESCRIBE / unclassified: no ORDER BY law applies.
    Other,
}

/// A violated SPARQL law, identified by its diagnostic code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SparqlLawCode {
    /// VALUES in an external `.rq` file.
    E0010,
    /// CONSTRUCT lacks ORDER BY.
    E0011,
    /// SELECT lacks ORDER BY.
    E0013,
}

impl SparqlLawCode {
    /// The stable string code (`"E0010"` ...).
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::E0010 => "E0010",
            Self::E0011 => "E0011",
            Self::E0013 => "E0013",
        }
    }
}

/// Returns true if the SPARQL query string contains `ORDER BY` (case-insensitive).
#[must_use]
pub fn query_has_order_by(sparql: &str) -> bool {
    sparql.to_uppercase().contains("ORDER BY")
}

/// Returns true if the query contains a VALUES clause. `#` line comments are
/// stripped first so commented-out VALUES blocks do not trigger the guard.
#[must_use]
pub fn query_contains_values(query: &str) -> bool {
    query
        .lines()
        .filter(|line| !line.trim_start().starts_with('#'))
        .any(|line| {
            line.split_whitespace()
                .any(|tok| tok.eq_ignore_ascii_case("VALUES"))
        })
}

/// Lint `query` for the three shared laws. `external_file` is true when the
/// text comes from an external `.rq` file (E0010 only applies there; inline
/// queries in `ggen.toml` may legitimately carry VALUES).
///
/// Output is sorted by code and deterministic.
#[must_use]
pub fn lint_query(query: &str, form: QueryForm, external_file: bool) -> Vec<SparqlLawCode> {
    let mut out = Vec::new();
    if external_file && query_contains_values(query) {
        out.push(SparqlLawCode::E0010);
    }
    if !query_has_order_by(query) {
        match form {
            QueryForm::Construct => out.push(SparqlLawCode::E0011),
            QueryForm::Select => out.push(SparqlLawCode::E0013),
            QueryForm::Other => {}
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn select_without_order_by_is_e0013() {
        assert_eq!(
            lint_query("SELECT ?s WHERE { ?s ?p ?o }", QueryForm::Select, true),
            vec![SparqlLawCode::E0013]
        );
    }

    #[test]
    fn construct_without_order_by_is_e0011_and_ordered_is_clean() {
        assert_eq!(
            lint_query("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }", QueryForm::Construct, false),
            vec![SparqlLawCode::E0011]
        );
        assert!(lint_query(
            "SELECT ?s WHERE { ?s ?p ?o } order by ?s",
            QueryForm::Select,
            true
        )
        .is_empty());
    }

    #[test]
    fn values_only_flagged_for_external_and_not_in_comments() {
        let q = "SELECT ?s WHERE { VALUES ?s { 1 } } ORDER BY ?s";
        assert_eq!(lint_query(q, QueryForm::Select, true), vec![SparqlLawCode::E0010]);
        assert!(lint_query(q, QueryForm::Select, false).is_empty());
        let c = "# VALUES here\nSELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s";
        assert!(lint_query(c, QueryForm::Select, true).is_empty());
    }
}
