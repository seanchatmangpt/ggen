//! E0014 author-time analyzer: a `[[generation.rules]]` `query`/`template` that
//! references a pack not declared in `[[packs]]`.
//!
//! The decision itself is NOT re-implemented here: it is
//! `ggen_config::manifest::validation::undeclared_pack_refs`, the same pure
//! function the sync-time `E0014` hard error calls, and the message is the same
//! `e0014_message`. This module only adds author-time position information.

use ggen_config::manifest::validation::{e0014_message, undeclared_pack_refs};
use ggen_config::manifest::ManifestParser;
use ggen_config::ConfigSchemaClassification;
use lsp_max::lsp_types_max::DiagnosticSeverity;
use lsp_max_protocol::{LawAxis, MaxDiagnostic};

use super::diag::{self, Span};

/// Diagnostic code, identical to the sync-time hard error's.
pub const E0014: &str = "E0014";

/// Pure E0014 detector over raw `ggen.toml` text.
///
/// Returns nothing for text that is not a declarative-rules manifest or does not
/// deserialize: load failures are surfaced by `check.rs::fold_manifest_load_errors`,
/// not absorbed here. Reads no files.
#[must_use]
pub fn undeclared_pack_diagnostics(manifest_text: &str) -> Vec<MaxDiagnostic> {
    if !matches!(
        ggen_config::classify_ggen_toml(manifest_text),
        ConfigSchemaClassification::DeclarativeRules
    ) {
        return Vec::new();
    }
    let Ok(manifest) = ManifestParser::parse_str(manifest_text) else {
        return Vec::new();
    };
    let blocks = rule_blocks(manifest_text);
    let lines: Vec<&str> = manifest_text.lines().collect();
    let mut out = Vec::new();
    for (idx, rule) in manifest.generation.rules.iter().enumerate() {
        for (field, pack) in undeclared_pack_refs(&manifest.packs, rule) {
            let (line, start_col, end_col) = locate(&lines, blocks.get(idx), field, &pack);
            out.push(diag::max(
                Span {
                    start_line: line,
                    start_col,
                    end_line: line,
                    end_col,
                },
                DiagnosticSeverity::ERROR,
                Some(E0014),
                e0014_message(&pack, &rule.name),
                LawAxis::Domain,
            ));
        }
    }
    out
}

/// `(header_line, end_line_exclusive)` of each `[[generation.rules]]` block, in order.
fn rule_blocks(text: &str) -> Vec<(usize, usize)> {
    let lines: Vec<&str> = text.lines().collect();
    let mut starts = Vec::new();
    for (i, l) in lines.iter().enumerate() {
        if l.trim() == "[[generation.rules]]" {
            starts.push(i);
        }
    }
    let mut out = Vec::new();
    for (k, &s) in starts.iter().enumerate() {
        let mut end = starts.get(k + 1).copied().unwrap_or(lines.len());
        for (j, l) in lines.iter().enumerate().take(end).skip(s + 1) {
            if l.trim_start().starts_with('[') {
                end = j;
                break;
            }
        }
        out.push((s, end));
    }
    out
}

/// Line + column span of the pack name inside the `field = {...}` line of the
/// block; falls back to the block header line (whole line) when not found.
fn locate(
    lines: &[&str], block: Option<&(usize, usize)>, field: &str, pack: &str,
) -> (u32, u32, u32) {
    let to_u32 = |v: usize| u32::try_from(v).unwrap_or(u32::MAX);
    let Some(&(start, end)) = block else {
        return (0, 0, u32::MAX);
    };
    for (i, l) in lines.iter().enumerate().take(end).skip(start + 1) {
        let t = l.trim_start();
        let is_field = t
            .strip_prefix(field)
            .is_some_and(|rest| rest.trim_start().starts_with('='));
        if !is_field {
            continue;
        }
        if let Some(byte) = l.find(&format!("\"{pack}\"")) {
            let col = l[..byte].chars().count() + 1;
            let width = pack.chars().count();
            return (to_u32(i), to_u32(col), to_u32(col + width));
        }
        return (to_u32(i), 0, u32::MAX);
    }
    (to_u32(start), 0, u32::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;

    const HEAD: &str = "[project]\nname = \"t\"\nversion = \"1.0.0\"\n\n[ontology]\nsource = \"o.ttl\"\n\n";

    #[test]
    fn undeclared_query_pack_yields_e0014_with_position() {
        let text = format!(
            "{HEAD}[[generation.rules]]\nname = \"r\"\noutput_file = \"out.rs\"\nquery = {{ pack = \"ghost\", output = \"queries\", file = \"q.rq\" }}\ntemplate = {{ inline = \"x\" }}\n"
        );
        let diags = undeclared_pack_diagnostics(&text);
        assert_eq!(diags.len(), 1, "{diags:?}");
        let d = &diags[0];
        assert_eq!(d.law_id, "E0014");
        assert_eq!(d.lsp.severity, Some(DiagnosticSeverity::ERROR));
        assert!(d.lsp.message.contains("'ghost'") && d.lsp.message.contains("'r'"));
        // `query = ...` is line index 11 (0-based); pack name starts after `{ pack = "`.
        assert_eq!(d.lsp.range.start.line, 11);
        assert_eq!(d.lsp.range.start.character, 17);
        assert_eq!(d.lsp.range.end.character, 22);
    }

    #[test]
    fn declared_pack_is_clean() {
        let text = format!(
            "{HEAD}[[packs]]\nname = \"good\"\nregistry = \"local\"\npath = \"./good\"\n\n[[generation.rules]]\nname = \"r\"\noutput_file = \"out.rs\"\nquery = {{ pack = \"good\", output = \"queries\", file = \"q.rq\" }}\ntemplate = {{ pack = \"good\", output = \"templates\", file = \"t.tera\" }}\n"
        );
        assert!(undeclared_pack_diagnostics(&text).is_empty());
    }

    #[test]
    fn undeclared_template_pack_flagged_even_when_query_pack_declared() {
        let text = format!(
            "{HEAD}[[packs]]\nname = \"a\"\nregistry = \"local\"\npath = \"./a\"\n\n[[generation.rules]]\nname = \"r\"\noutput_file = \"out.rs\"\nquery = {{ pack = \"a\", output = \"queries\", file = \"q.rq\" }}\ntemplate = {{ pack = \"b\", output = \"templates\", file = \"t.tera\" }}\n"
        );
        let diags = undeclared_pack_diagnostics(&text);
        assert_eq!(diags.len(), 1, "{diags:?}");
        let line = text.lines().nth(diags[0].lsp.range.start.line as usize).unwrap();
        assert!(line.starts_with("template"), "anchored on: {line}");
    }

    #[test]
    fn non_manifest_text_yields_nothing() {
        assert!(undeclared_pack_diagnostics("not [ toml").is_empty());
    }
}
