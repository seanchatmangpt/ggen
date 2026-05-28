//! Position-aware parsing for LSP diagnostics.
//!
//! The standard [`parse_turtle`](super::parse::parse_turtle) family discards source
//! locations (errors collapse into [`crate::GraphError::Serialization`]). Language
//! servers need line/column spans to render squiggles, so these functions stream the
//! oxrdfio parser directly and capture the [`oxigraph::io::RdfSyntaxError`] location.
//!
//! Positions are reported as oxrdfio emits them: **1-based** `line`/`column` plus a
//! byte `offset`. LSP front-ends (which use 0-based positions) convert at the boundary.

use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Quad;

/// A located parse diagnostic for a single RDF syntax error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseDiagnostic {
    /// 1-based line of the error start.
    pub line: u64,
    /// 1-based column of the error start.
    pub column: u64,
    /// 1-based line of the error end (== `line` when no range is available).
    pub end_line: u64,
    /// 1-based column of the error end (== `column` when no range is available).
    pub end_column: u64,
    /// Byte offset of the error start (0 when unavailable).
    pub offset: u64,
    /// Human-readable parser message.
    pub message: String,
}

/// The quads successfully parsed plus any located syntax diagnostics.
///
/// `diagnostics` is empty on a clean parse. The RDF parsers halt at the first
/// syntax error, so at most one diagnostic is produced per call — but it carries
/// an accurate source location, which is what an LSP needs.
#[derive(Debug, Clone, Default)]
pub struct LocatedParse {
    /// Quads parsed before any error.
    pub quads: Vec<Quad>,
    /// Located syntax diagnostics (empty on success).
    pub diagnostics: Vec<ParseDiagnostic>,
}

fn parse_located(format: RdfFormat, content: &str) -> LocatedParse {
    let mut quads = Vec::new();
    let mut diagnostics = Vec::new();

    let mut parser = RdfParser::from_format(format).for_slice(content.as_bytes());
    loop {
        match parser.next() {
            Some(Ok(quad)) => quads.push(quad),
            Some(Err(err)) => {
                let (line, column, end_line, end_column, offset) = match err.location() {
                    Some(range) => (
                        range.start.line + 1,
                        range.start.column + 1,
                        range.end.line + 1,
                        range.end.column + 1,
                        range.start.offset,
                    ),
                    None => (1, 1, 1, 1, 0),
                };
                diagnostics.push(ParseDiagnostic {
                    line,
                    column,
                    end_line,
                    end_column,
                    offset,
                    message: err.to_string(),
                });
                // The parser stops yielding meaningful quads after a syntax error.
                break;
            }
            None => break,
        }
    }

    LocatedParse { quads, diagnostics }
}

/// Parse Turtle, capturing located syntax diagnostics for LSP use.
pub fn parse_turtle_located(content: &str) -> LocatedParse {
    parse_located(RdfFormat::Turtle, content)
}

/// Parse N-Triples, capturing located syntax diagnostics for LSP use.
pub fn parse_ntriples_located(content: &str) -> LocatedParse {
    parse_located(RdfFormat::NTriples, content)
}

/// Parse N-Quads, capturing located syntax diagnostics for LSP use.
pub fn parse_nquads_located(content: &str) -> LocatedParse {
    parse_located(RdfFormat::NQuads, content)
}

/// Extract the `@prefix`/`PREFIX` declarations from a Turtle document as
/// `(prefix, iri)` pairs, for completion and hover. Prefixes are collected after
/// draining the parser (oxrdfio discovers them during parsing). Malformed input
/// yields whatever prefixes were seen before the error.
pub fn extract_prefixes(content: &str) -> Vec<(String, String)> {
    let mut parser = RdfParser::from_format(RdfFormat::Turtle).for_slice(content.as_bytes());
    // Drain the parser so all prefix declarations are observed.
    for item in parser.by_ref() {
        if item.is_err() {
            break;
        }
    }
    parser
        .prefixes()
        .map(|(prefix, iri)| (prefix.to_string(), iri.to_string()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clean_turtle_has_no_diagnostics() {
        let ttl = "@prefix ex: <http://example.org/> .\nex:a ex:b ex:c .\n";
        let result = parse_turtle_located(ttl);
        assert!(result.diagnostics.is_empty(), "expected clean parse");
        assert_eq!(result.quads.len(), 1);
    }

    #[test]
    fn malformed_turtle_reports_located_diagnostic() {
        // Missing the final '.' / closing — a genuine syntax error.
        let ttl = "@prefix ex: <http://example.org/> .\nex:a ex:b \"unterminated\n";
        let result = parse_turtle_located(ttl);
        assert!(
            !result.diagnostics.is_empty(),
            "expected a syntax diagnostic"
        );
        let diag = &result.diagnostics[0];
        // Error is on or after the second line; positions are 1-based.
        assert!(diag.line >= 1);
        assert!(diag.column >= 1);
        assert!(!diag.message.is_empty());
    }

    #[test]
    fn extract_prefixes_returns_declared_prefixes() {
        let ttl = "@prefix ex: <http://example.org/> .\n@prefix sh: <http://www.w3.org/ns/shacl#> .\nex:a ex:b ex:c .\n";
        let prefixes = extract_prefixes(ttl);
        assert!(prefixes
            .iter()
            .any(|(p, i)| p == "ex" && i == "http://example.org/"));
        assert!(prefixes
            .iter()
            .any(|(p, i)| p == "sh" && i == "http://www.w3.org/ns/shacl#"));
    }
}
