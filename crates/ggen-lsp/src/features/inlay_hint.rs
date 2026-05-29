//! Real LSP inlay hints for ggen law surfaces.
//!
//! Inlay hints are the "show me what this resolves to, inline" surface. They
//! make the implicit explicit at author time, without changing the document:
//!
//! - **RDF** (`.ttl`/`.nt`/`.nq`): after a prefixed name like `ex:Foo`, show the
//!   resolved IRI (`: <http://example.org/Foo>`) using the file's `@prefix`
//!   declarations. The agent sees exactly what `ex:Foo` *is* without leaving the
//!   line.
//! - **TOML** (`ggen.toml`): after an enum key's value (e.g. `level = "info"`),
//!   show the allowed value set (` (debug|info|warn|error)`). The same enum law
//!   surfaced by the toml analyzer's diagnostics is surfaced here as guidance.
//! - **SPARQL** (`.rq`/`.sparql`): after a prefixed name, show the resolved IRI
//!   using `PREFIX` declarations.
//!
//! Every hint carries a real `position` derived from the source text, a string
//! label, and a `kind`. Only hints whose position falls within the requested
//! `range` are emitted, per the LSP contract.

use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

use crate::state::FileType;

/// Enum laws drawn from ggen's config schema, mirroring
/// `analyzers::toml_analyzer::ENUMS`. Keyed by the TOML key name; the value is
/// the admitted set of values. Surfacing these inline turns the schema into
/// author-time guidance.
const TOML_ENUMS: &[(&str, &[&str])] = &[
    ("level", &["debug", "info", "warn", "error"]),
    ("format", &["json", "text"]),
    (
        "default_format",
        &["turtle", "rdfxml", "ntriples", "nquads", "trig"],
    ),
];

/// Compute inlay hints for a document within `range`.
///
/// Returns `None` when there are no hints (rather than an empty vector) so the
/// LSP layer can treat "no hints" uniformly. Hints whose anchor position is
/// outside `range` are filtered out.
#[must_use]
pub fn inlay_hints(
    file_type: FileType,
    content: &str,
    range: Range,
) -> Option<Vec<InlayHint>> {
    let hints = match file_type {
        FileType::Rdf => rdf_hints(content),
        FileType::Toml => toml_hints(content),
        FileType::Sparql => sparql_hints(content),
        FileType::Tera | FileType::Unknown => Vec::new(),
    };

    let in_range: Vec<InlayHint> = hints
        .into_iter()
        .filter(|h| position_in_range(h.position, range))
        .collect();

    if in_range.is_empty() {
        None
    } else {
        Some(in_range)
    }
}

// --- RDF -------------------------------------------------------------------

/// Build inlay hints for an RDF (Turtle-family) document: after each prefixed
/// name `pfx:local`, append the resolved IRI using the document's `@prefix`
/// declarations.
fn rdf_hints(content: &str) -> Vec<InlayHint> {
    let prefixes = parse_turtle_prefixes(content);
    if prefixes.is_empty() {
        return Vec::new();
    }
    let mut hints = Vec::new();
    for (line_idx, line) in content.lines().enumerate() {
        // Skip the prefix declarations themselves — resolving `ex:` on an
        // `@prefix ex: <...>` line is noise.
        if line.trim_start().starts_with("@prefix")
            || line.trim_start().to_ascii_uppercase().starts_with("PREFIX")
        {
            continue;
        }
        for tok in prefixed_names(line) {
            if let Some(iri) = resolve(&tok.text, &prefixes) {
                hints.push(iri_hint(line_idx, tok.end_col, &iri));
            }
        }
    }
    hints
}

// --- SPARQL ----------------------------------------------------------------

/// Build inlay hints for a SPARQL query: after each prefixed name, append the
/// resolved IRI using `PREFIX pfx: <iri>` declarations.
fn sparql_hints(content: &str) -> Vec<InlayHint> {
    let prefixes = parse_sparql_prefixes(content);
    if prefixes.is_empty() {
        return Vec::new();
    }
    let mut hints = Vec::new();
    for (line_idx, line) in content.lines().enumerate() {
        if line.trim_start().to_ascii_uppercase().starts_with("PREFIX") {
            continue;
        }
        for tok in prefixed_names(line) {
            if let Some(iri) = resolve(&tok.text, &prefixes) {
                hints.push(iri_hint(line_idx, tok.end_col, &iri));
            }
        }
    }
    hints
}

// --- TOML ------------------------------------------------------------------

/// Build inlay hints for a `ggen.toml`: after the value of a known enum key,
/// append the admitted set, e.g. `level = "info"  (debug|info|warn|error)`.
fn toml_hints(content: &str) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    for (line_idx, line) in content.lines().enumerate() {
        let Some((raw_key, raw_val)) = line.split_once('=') else {
            continue;
        };
        let key = raw_key.trim();
        let Some((_, allowed)) = TOML_ENUMS.iter().find(|(k, _)| *k == key) else {
            continue;
        };
        // The value text may be empty (e.g. `level = `). Anchor the hint right
        // after whatever value (or whitespace) exists on the line so it reads as
        // a trailing annotation.
        let value_present = !raw_val.trim().is_empty();
        if !value_present {
            // Still useful: hint the allowed set right after `=`.
        }
        let end_col = u32::try_from(line.chars().count()).unwrap_or(u32::MAX);
        let label = format!(" ({})", allowed.join("|"));
        hints.push(InlayHint {
            position: Position {
                line: u32::try_from(line_idx).unwrap_or(0),
                character: end_col,
            },
            label: InlayHintLabel::String(label),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: Some(true),
            padding_right: None,
            data: None,
        });
    }
    hints
}

// --- shared helpers --------------------------------------------------------

/// A prefixed name found on a line, with the column (0-based, char count) where
/// it ends — the anchor point for the trailing IRI hint.
struct PrefixedToken {
    text: String,
    end_col: u32,
}

/// Scan a line for prefixed names of the form `pfx:local` (excluding full IRIs
/// in `<...>` and blank-node `_:` labels). Returns each token with its end
/// column. Char-based columns match LSP's UTF-16-agnostic expectations for the
/// ASCII identifiers used here.
fn prefixed_names(line: &str) -> Vec<PrefixedToken> {
    let chars: Vec<char> = line.chars().collect();
    let is_pname_char =
        |c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '.' | ':' | '#');
    let mut out = Vec::new();
    let mut i = 0usize;
    let mut in_iri = false; // inside <...>
    let mut in_str = false; // inside "..."
    while i < chars.len() {
        let c = chars[i];
        if in_str {
            if c == '"' {
                in_str = false;
            }
            i += 1;
            continue;
        }
        if in_iri {
            if c == '>' {
                in_iri = false;
            }
            i += 1;
            continue;
        }
        match c {
            '<' => {
                in_iri = true;
                i += 1;
                continue;
            }
            '"' => {
                in_str = true;
                i += 1;
                continue;
            }
            '#' => break, // rest of line is a comment
            _ => {}
        }
        if is_pname_char(c) {
            let start = i;
            while i < chars.len() && is_pname_char(chars[i]) {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            // A prefixed name has exactly one ':' with a non-empty prefix part,
            // is not a blank node (`_:`), and is not a bare IRI scheme (`http:`
            // immediately followed by `//`).
            if let Some((pfx, local)) = token.split_once(':') {
                let is_blank = pfx == "_";
                let is_scheme = local.starts_with("//");
                let single_colon = !local.contains(':');
                if !is_blank && !is_scheme && single_colon {
                    out.push(PrefixedToken {
                        text: token,
                        end_col: u32::try_from(i).unwrap_or(u32::MAX),
                    });
                }
            }
        } else {
            i += 1;
        }
    }
    out
}

/// Resolve `pfx:local` against a prefix map to a full IRI string.
fn resolve(token: &str, prefixes: &[(String, String)]) -> Option<String> {
    let (pfx, local) = token.split_once(':')?;
    for (p, iri) in prefixes {
        if p == pfx {
            return Some(format!("{iri}{local}"));
        }
    }
    None
}

/// Build a trailing IRI hint `: <iri>` anchored at `end_col` on `line_idx`.
fn iri_hint(line_idx: usize, end_col: u32, iri: &str) -> InlayHint {
    InlayHint {
        position: Position {
            line: u32::try_from(line_idx).unwrap_or(0),
            character: end_col,
        },
        label: InlayHintLabel::String(format!(": <{iri}>")),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: None,
        data: None,
    }
}

/// Parse Turtle `@prefix pfx: <iri> .` declarations into `(prefix, iri)` pairs.
/// The empty prefix (`@prefix : <iri> .`) is captured with an empty key.
fn parse_turtle_prefixes(content: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        let rest = if let Some(r) = trimmed.strip_prefix("@prefix") {
            r
        } else if let Some(r) = trimmed
            .strip_prefix("PREFIX")
            .or_else(|| trimmed.strip_prefix("prefix"))
        {
            r
        } else {
            continue;
        };
        if let Some((pfx, iri)) = parse_prefix_body(rest) {
            out.push((pfx, iri));
        }
    }
    out
}

/// Parse SPARQL `PREFIX pfx: <iri>` declarations.
fn parse_sparql_prefixes(content: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        let upper = trimmed.to_ascii_uppercase();
        if !upper.starts_with("PREFIX") {
            continue;
        }
        let rest = &trimmed[6..]; // strip the 6-char "PREFIX"
        if let Some((pfx, iri)) = parse_prefix_body(rest) {
            out.push((pfx, iri));
        }
    }
    out
}

/// Parse the body of a prefix declaration: `<pfx>: <iri>` possibly trailed by a
/// `.`. Works for both Turtle and SPARQL forms. The prefix part keeps its name
/// without the trailing colon; an empty prefix yields `""`.
fn parse_prefix_body(rest: &str) -> Option<(String, String)> {
    let rest = rest.trim();
    // Find the colon that terminates the prefix label. The prefix label is the
    // run up to the first ':'.
    let colon = rest.find(':')?;
    let pfx = rest[..colon].trim().to_string();
    let after = rest[colon + 1..].trim();
    let lt = after.find('<')?;
    let gt = after[lt + 1..].find('>')?;
    let iri = after[lt + 1..lt + 1 + gt].to_string();
    Some((pfx, iri))
}

/// Is `pos` within `range` (inclusive on both ends, line/character ordered)?
fn position_in_range(pos: Position, range: Range) -> bool {
    let after_start = (pos.line, pos.character) >= (range.start.line, range.start.character);
    let before_end = (pos.line, pos.character) <= (range.end.line, range.end.character);
    after_start && before_end
}

#[cfg(test)]
mod tests {
    use super::*;

    fn full_range() -> Range {
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: u32::MAX,
                character: u32::MAX,
            },
        }
    }

    #[test]
    fn rdf_resolves_prefixed_name_to_iri_at_correct_position() {
        // Line 0: @prefix (skipped). Line 1: usage of ex:Foo.
        let ttl = "@prefix ex: <http://example.org/> .\nex:Foo a ex:Class .\n";
        let hints = inlay_hints(FileType::Rdf, ttl, full_range()).expect("hints");

        // ex:Foo resolves to http://example.org/Foo, ex:Class to .../Class.
        let foo = hints
            .iter()
            .find(|h| match &h.label {
                InlayHintLabel::String(s) => s.contains("http://example.org/Foo"),
                _ => false,
            })
            .expect("a resolved IRI hint for ex:Foo");

        // "ex:Foo" starts at column 0 on line 1 and is 6 chars long → ends at col 6.
        assert_eq!(foo.position.line, 1);
        assert_eq!(foo.position.character, 6);
        match &foo.label {
            InlayHintLabel::String(s) => assert_eq!(s, ": <http://example.org/Foo>"),
            other => panic!("expected string label, got {other:?}"),
        }
        assert_eq!(foo.kind, Some(InlayHintKind::TYPE));

        // ex:Class is also resolved.
        assert!(hints.iter().any(|h| matches!(
            &h.label,
            InlayHintLabel::String(s) if s.contains("http://example.org/Class")
        )));
    }

    #[test]
    fn rdf_does_not_hint_unknown_prefix_or_prefix_decl_line() {
        let ttl = "@prefix ex: <http://example.org/> .\nfoo:Bar a ex:Class .\n";
        let hints = inlay_hints(FileType::Rdf, ttl, full_range()).expect("hints");
        // foo: is unknown → no hint for foo:Bar.
        assert!(hints.iter().all(|h| match &h.label {
            InlayHintLabel::String(s) => !s.contains("Bar"),
            _ => true,
        }));
        // No hint anchored on the @prefix line (line 0).
        assert!(hints.iter().all(|h| h.position.line != 0));
    }

    #[test]
    fn toml_enum_value_gets_allowed_set_hint() {
        let toml = "[logging]\nlevel = \"info\"\n";
        let hints = inlay_hints(FileType::Toml, toml, full_range()).expect("hints");
        let hint = hints.first().expect("one hint for level");
        match &hint.label {
            InlayHintLabel::String(s) => assert_eq!(s, " (debug|info|warn|error)"),
            other => panic!("expected string label, got {other:?}"),
        }
        // Anchored on line 1 (the `level = ...` line), at end of line.
        assert_eq!(hint.position.line, 1);
        let line_len = u32::try_from("level = \"info\"".chars().count()).unwrap();
        assert_eq!(hint.position.character, line_len);
        assert_eq!(hint.kind, Some(InlayHintKind::TYPE));
    }

    #[test]
    fn toml_format_and_default_format_enums_are_hinted() {
        let toml = "[logging]\nformat = \"json\"\n[rdf]\ndefault_format = \"turtle\"\n";
        let hints = inlay_hints(FileType::Toml, toml, full_range()).expect("hints");
        assert!(hints.iter().any(|h| matches!(
            &h.label,
            InlayHintLabel::String(s) if s == " (json|text)"
        )));
        assert!(hints.iter().any(|h| matches!(
            &h.label,
            InlayHintLabel::String(s) if s == " (turtle|rdfxml|ntriples|nquads|trig)"
        )));
    }

    #[test]
    fn toml_non_enum_key_gets_no_hint() {
        let toml = "[project]\nname = \"acme\"\n";
        assert!(inlay_hints(FileType::Toml, toml, full_range()).is_none());
    }

    #[test]
    fn sparql_resolves_prefixed_name() {
        let rq = "PREFIX ex: <http://example.org/>\nSELECT ?s WHERE { ?s a ex:Thing }\n";
        let hints = inlay_hints(FileType::Sparql, rq, full_range()).expect("hints");
        assert!(hints.iter().any(|h| matches!(
            &h.label,
            InlayHintLabel::String(s) if s == ": <http://example.org/Thing>"
        )));
    }

    #[test]
    fn range_filter_excludes_out_of_range_hints() {
        let ttl = "@prefix ex: <http://example.org/> .\nex:Foo a ex:Class .\nex:Bar a ex:Class .\n";
        // Range covering only line 1.
        let range = Range {
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: u32::MAX,
            },
        };
        let hints = inlay_hints(FileType::Rdf, ttl, range).expect("hints");
        assert!(hints.iter().all(|h| h.position.line == 1));
        // ex:Bar on line 2 must be excluded.
        assert!(hints.iter().all(|h| match &h.label {
            InlayHintLabel::String(s) => !s.contains("/Bar"),
            _ => true,
        }));
    }

    #[test]
    fn empty_prefix_resolves() {
        let ttl = "@prefix : <http://example.org/base#> .\n:Root a :Node .\n";
        let hints = inlay_hints(FileType::Rdf, ttl, full_range()).expect("hints");
        assert!(hints.iter().any(|h| matches!(
            &h.label,
            InlayHintLabel::String(s) if s == ": <http://example.org/base#Root>"
        )));
    }

    #[test]
    fn unknown_and_tera_file_types_yield_no_hints() {
        assert!(inlay_hints(FileType::Unknown, "anything", full_range()).is_none());
        assert!(inlay_hints(FileType::Tera, "{{ x }}", full_range()).is_none());
    }
}
