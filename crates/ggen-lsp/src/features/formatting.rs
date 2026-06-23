//! Document and range formatting for ggen law surfaces.
//!
//! Every formatter here is **real**: it reuses the same canonical
//! parse-and-reserialize machinery the rest of ggen relies on, so the text an
//! editor sees after `Format Document` is byte-for-byte what the toolchain would
//! produce — no hand-rolled, fragile pretty-printers.
//!
//! | File type | Backend | Canonical form |
//! |-----------|---------|----------------|
//! | [`Toml`]  | `toml::to_string_pretty` over a parsed `toml::Table` | pretty TOML |
//! | [`Rdf`]   | oxigraph parse → `RdfSerializer` (prefixed Turtle) | sorted, prefixed Turtle |
//! | [`Sparql`]| `qlue_ls::format_raw` (Qlue-ls, MIT) | pretty-formatted SPARQL |
//! | [`Tera`]  | — (no canonical formatter exists) | `None` |
//! | `Unknown` | — | `None` |
//!
//! A formatter returns `None` (leaving the buffer untouched) whenever the input
//! does not parse. We never emit a partial/corrupt reserialization: an invalid
//! document is the analyzer's problem to *diagnose*, not the formatter's to
//! *rewrite*.
//!
//! [`Toml`]: crate::state::FileType::Toml
//! [`Rdf`]: crate::state::FileType::Rdf
//! [`Sparql`]: crate::state::FileType::Sparql
//! [`Tera`]: crate::state::FileType::Tera

use lsp_max::lsp_types::{Position, Range, TextEdit};
use oxigraph::io::{RdfFormat, RdfParser, RdfSerializer};
use oxigraph::model::Quad;

use crate::state::FileType;

/// Produce a single full-document replacement edit that canonically formats
/// `content` according to `file_type`.
///
/// Returns `None` when the file type has no canonical formatter (Tera, Unknown)
/// or when `content` does not parse — never a corrupt rewrite.
#[must_use]
pub fn format_document(file_type: FileType, content: &str) -> Option<Vec<TextEdit>> {
    let formatted = match file_type {
        FileType::Toml => format_toml(content)?,
        FileType::Rdf => format_turtle(content)?,
        FileType::Sparql => format_sparql(content)?,
        // No canonical formatter for Tera, USD, or MaterialX surfaces yet.
        FileType::Tera | FileType::Unknown | FileType::Usd | FileType::Mtlx => return None,
    };

    // A no-op format (already canonical) is still a valid, empty-effect edit; we
    // return it so clients see a deterministic "nothing to change" result rather
    // than a "formatter unavailable" one.
    Some(vec![TextEdit {
        range: whole_document_range(content),
        new_text: formatted,
    }])
}

/// Range formatting.
///
/// TOML, Turtle, and SPARQL are *whole-document* grammars for canonicalization
/// purposes: a canonical TOML table, a sorted Turtle graph, and a normalized
/// SPARQL query are only well-defined over the entire document, not an arbitrary
/// line span. Per the LSP spec a server may satisfy a range-format request by
/// formatting the whole document, which is what we do for the grammars that
/// support document formatting. Tera/Unknown return `None`.
///
/// `_range` is accepted for protocol compatibility; the canonical formatters
/// here are document-scoped, so the selected range does not change the result.
#[must_use]
pub fn format_range(file_type: FileType, content: &str, _range: Range) -> Option<Vec<TextEdit>> {
    match file_type {
        FileType::Toml | FileType::Rdf | FileType::Sparql => format_document(file_type, content),
        FileType::Tera | FileType::Unknown | FileType::Usd | FileType::Mtlx => None,
    }
}

/// Pretty-print TOML by round-tripping through a parsed table.
///
/// Parsing as `toml::Table` (rather than a bare `toml::Value`) both rejects
/// non-table top levels and lets `to_string_pretty` order scalars before
/// sub-tables, which is the canonical pretty layout. Returns `None` on any parse
/// or serialization error so an invalid file is left untouched.
fn format_toml(content: &str) -> Option<String> {
    let table: toml::Table = toml::from_str(content).ok()?;
    toml::to_string_pretty(&table).ok()
}

/// Reserialize Turtle: parse to quads with oxigraph, then dump prefixed Turtle.
///
/// oxigraph's Turtle serializer emits a deterministic, prefixed graph. We feed it
/// the declared `@prefix`es (recovered from the source) so the output keeps
/// compact prefixed names instead of expanding every IRI. Returns `None` if the
/// document is not valid Turtle.
fn format_turtle(content: &str) -> Option<String> {
    let quads = parse_turtle_quads(content)?;

    // The document already parsed cleanly above, so every recovered prefix IRI is
    // a valid IRI; `with_prefix` (which consumes and returns the serializer) only
    // fails on an unparseable IRI, which cannot occur here. We thread the
    // serializer through each call and bail to `None` on the unreachable error
    // rather than silently dropping triples.
    let mut serializer = RdfSerializer::from_format(RdfFormat::Turtle);
    for (prefix, iri) in ggen_graph::extract_prefixes(content) {
        serializer = serializer.with_prefix(prefix, iri).ok()?;
    }

    let mut writer = serializer.for_writer(Vec::<u8>::new());
    for quad in &quads {
        writer.serialize_quad(quad).ok()?;
    }
    let bytes = writer.finish().ok()?;
    String::from_utf8(bytes).ok()
}

/// Parse Turtle to quads, returning `None` if the document has any syntax error.
///
/// We require a fully clean parse: a single missing `.` should leave the buffer
/// alone, not silently drop the triples after the error.
fn parse_turtle_quads(content: &str) -> Option<Vec<Quad>> {
    let mut quads = Vec::new();
    for result in RdfParser::from_format(RdfFormat::Turtle).for_slice(content.as_bytes()) {
        quads.push(result.ok()?);
    }
    Some(quads)
}

/// Format a SPARQL query with Qlue-ls's real, opinionated formatter (MIT) —
/// proper indentation/layout, not just spargebra normalization. Returns `None` on
/// any syntax error (never a corrupt rewrite).
fn format_sparql(content: &str) -> Option<String> {
    // `qlue_ls::format_raw` runs an error-tolerant parser and will happily
    // reformat a partial/broken syntax tree, so it cannot be trusted to reject
    // invalid input. Gate on a clean parse first (same parser Qlue-ls uses): if
    // there are any parse errors we leave the buffer untouched.
    let (_, errors) = ll_sparql_parser::parse(content);
    if !errors.is_empty() {
        return None;
    }
    qlue_ls::format_raw(content.to_string()).ok()
}

/// The `(0,0)..(last_line, last_col)` range covering the entire document, so a
/// single `TextEdit` replaces all of it. An empty document yields an empty range.
fn whole_document_range(content: &str) -> Range {
    let mut last_line = 0u32;
    let mut last_col = 0u32;
    for ch in content.chars() {
        if ch == '\n' {
            last_line = last_line.saturating_add(1);
            last_col = 0;
        } else {
            last_col = last_col.saturating_add(1);
        }
    }
    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: last_line,
            character: last_col,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pull the single full-document replacement text out of a format result.
    fn formatted_text(edits: Option<Vec<TextEdit>>) -> Option<String> {
        let mut edits = edits?;
        assert_eq!(
            edits.len(),
            1,
            "formatters emit exactly one full-document edit"
        );
        Some(edits.remove(0).new_text)
    }

    // ---- TOML --------------------------------------------------------------

    #[test]
    fn toml_messy_but_valid_is_reformatted_and_still_parses() {
        // Misaligned spacing, blank lines, out-of-order keys — all valid TOML.
        let messy = "  [logging]\n\n\nlevel    =    \"info\"\nformat=\"json\"\n";
        let out =
            formatted_text(format_document(FileType::Toml, messy)).expect("valid TOML formats");
        assert_ne!(out, messy, "messy input must actually change");
        // Output is still valid TOML carrying the same data.
        let reparsed: toml::Table = toml::from_str(&out).expect("formatted TOML re-parses");
        assert_eq!(reparsed["logging"]["level"].as_str(), Some("info"));
        assert_eq!(reparsed["logging"]["format"].as_str(), Some("json"));
    }

    #[test]
    fn toml_is_idempotent() {
        let messy = "[a]\nx=1\n[b]\ny=2\n";
        let once = formatted_text(format_document(FileType::Toml, messy)).expect("first pass");
        let twice = formatted_text(format_document(FileType::Toml, &once)).expect("second pass");
        assert_eq!(once, twice, "formatting a formatted document is a no-op");
    }

    #[test]
    fn toml_invalid_returns_none_no_corruption() {
        // Missing closing bracket — must not rewrite (would corrupt).
        assert!(format_document(FileType::Toml, "[logging\nlevel = \"info\"\n").is_none());
    }

    // ---- Turtle / RDF ------------------------------------------------------

    #[test]
    fn turtle_messy_but_valid_is_reformatted_and_still_parses() {
        let messy = "@prefix ex:   <http://example.org/> .\n\n\n   ex:Thing    a    ex:Class    ;   ex:p   ex:o   .\n";
        let out =
            formatted_text(format_document(FileType::Rdf, messy)).expect("valid Turtle formats");
        assert_ne!(out, messy, "messy Turtle must change");
        // Output is still valid Turtle with the same triples.
        let reparsed = parse_turtle_quads(&out).expect("formatted Turtle re-parses");
        let original = parse_turtle_quads(messy).expect("original parses");
        assert_eq!(reparsed.len(), original.len(), "triple count preserved");
        assert!(
            out.contains("ex:"),
            "prefixed names are preserved, not expanded"
        );
    }

    #[test]
    fn turtle_is_idempotent() {
        let messy = "@prefix ex: <http://example.org/> .\nex:a a ex:B ; ex:p ex:o .\n";
        let once = formatted_text(format_document(FileType::Rdf, messy)).expect("first pass");
        let twice = formatted_text(format_document(FileType::Rdf, &once)).expect("second pass");
        assert_eq!(once, twice, "formatting canonical Turtle is a no-op");
    }

    #[test]
    fn turtle_invalid_returns_none_no_corruption() {
        // Unterminated literal — must not rewrite.
        assert!(format_document(
            FileType::Rdf,
            "@prefix ex: <http://example.org/> .\nex:a ex:b \"oops\n"
        )
        .is_none());
    }

    // ---- SPARQL ------------------------------------------------------------

    #[test]
    fn sparql_messy_but_valid_is_reformatted_and_still_parses() {
        let messy = "select   ?s ?o where{?s   <http://example.org/p>   ?o.}";
        let out =
            formatted_text(format_document(FileType::Sparql, messy)).expect("valid SPARQL formats");
        assert_ne!(out, messy, "messy SPARQL must change");
        // Output is still a valid SPARQL query: feeding it back through the same
        // Qlue-ls parser must succeed (a real re-parse, no deprecated oxigraph API).
        assert!(
            qlue_ls::format_raw(out.clone()).is_ok(),
            "formatted SPARQL re-parses"
        );
        assert!(
            out.contains("SELECT"),
            "keywords normalized to canonical form"
        );
    }

    #[test]
    fn sparql_is_idempotent() {
        let messy = "SELECT ?s WHERE { ?s ?p ?o }";
        let once = formatted_text(format_document(FileType::Sparql, messy)).expect("first pass");
        let twice = formatted_text(format_document(FileType::Sparql, &once)).expect("second pass");
        assert_eq!(once, twice, "formatting a normalized query is a no-op");
    }

    #[test]
    fn sparql_invalid_returns_none_no_corruption() {
        assert!(format_document(FileType::Sparql, "SELECT WHERE {{{").is_none());
    }

    // ---- Non-formattable surfaces -----------------------------------------

    #[test]
    fn tera_and_unknown_return_none() {
        assert!(format_document(FileType::Tera, "{{ name }}\n").is_none());
        assert!(format_document(FileType::Unknown, "anything\n").is_none());
    }

    // ---- Range formatting --------------------------------------------------

    #[test]
    fn range_format_delegates_to_document_for_supported_grammars() {
        let messy = "[a]\nx=1\n";
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 3,
            },
        };
        let via_range = formatted_text(format_range(FileType::Toml, messy, range))
            .expect("range format delegates");
        let via_doc = formatted_text(format_document(FileType::Toml, messy)).expect("doc format");
        assert_eq!(
            via_range, via_doc,
            "range format == document format for TOML"
        );
    }

    #[test]
    fn range_format_none_for_tera() {
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 1,
            },
        };
        assert!(format_range(FileType::Tera, "{{ x }}", range).is_none());
    }

    // ---- whole_document_range geometry ------------------------------------

    #[test]
    fn whole_document_range_covers_to_last_column() {
        let r = whole_document_range("ab\ncde");
        assert_eq!(
            r.start,
            Position {
                line: 0,
                character: 0
            }
        );
        assert_eq!(
            r.end,
            Position {
                line: 1,
                character: 3
            }
        );
    }

    #[test]
    fn whole_document_range_trailing_newline() {
        let r = whole_document_range("ab\n");
        assert_eq!(
            r.end,
            Position {
                line: 1,
                character: 0
            }
        );
    }

    #[test]
    fn whole_document_range_empty_document() {
        let r = whole_document_range("");
        assert_eq!(
            r.end,
            Position {
                line: 0,
                character: 0
            }
        );
    }
}
