//! SPARQL law-surface analyzer (`.rq`, `.sparql`).
//!
//! Surfaces the SPARQL laws ggen enforces at sync time, at author time:
//! - **E0010**: external `.rq` files must not contain `VALUES` (data must be inline in `ggen.toml`).
//! - **E0011**/**E0013**: `CONSTRUCT`/`SELECT` queries should carry `ORDER BY`.
//! - syntax errors from the oxigraph SPARQL parser.
//!
//! Plus keyword/variable completion and a variable outline.
//!
//! # E0011/E0013 severity mirrors the schema default (`strict_mode = true`)
//!
//! This analyzer has no manifest/project context (`new_from_content` takes only
//! `content: &str`; `crate::analyzers::build_analyzer`'s six call sites never
//! thread a resolved `ggen.toml` in), so it cannot read a project's actual
//! `[validation] strict_mode` override. `ggen_config::manifest::types::
//! default_strict_mode` returns `true` -- a project with no explicit
//! `[validation]` override (confirmed by `ggen-config`'s own
//! `missing_order_by_refuses_by_default_when_validation_section_is_absent`
//! test) gets a hard `ConfigError::Validation` refusal from `ggen sync run`
//! for a missing `ORDER BY`, via the shared `query_has_order_by` predicate
//! imported below. E0011/E0013 are therefore emitted at `ERROR` severity here
//! too, matching that default, so a headless `ggen lsp check` exit code is not
//! a false "this will sync cleanly" signal for the common (no-override) case.
//! A project that explicitly opts out with `strict_mode = false` still only
//! gets a warning from `ggen sync run` for the same file -- this analyzer has
//! no way to see that override and will still report ERROR there. That
//! remaining divergence is narrower (fails closed, not open) and documented,
//! not silently absorbed.

use lsp_max::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionResponse, DiagnosticSeverity, Hover, Location,
    Position, Range, SymbolKind, TextEdit, WorkspaceEdit,
};
use std::collections::BTreeSet;

use ggen_config::manifest::validation::{query_contains_values, query_has_order_by};
use ggen_graph::{check_sparql_syntax, sparql_kind, SparqlKind};

use crate::analyzers::diag;

const KEYWORDS: &[&str] = &[
    "SELECT",
    "CONSTRUCT",
    "ASK",
    "DESCRIBE",
    "WHERE",
    "PREFIX",
    "BASE",
    "ORDER BY",
    "GROUP BY",
    "LIMIT",
    "OFFSET",
    "FILTER",
    "OPTIONAL",
    "UNION",
    "DISTINCT",
    "BIND",
    "VALUES",
    "MINUS",
    "HAVING",
    "SERVICE",
];

#[derive(Clone)]
pub struct SparqlAnalyzer {
    source: String,
}

impl SparqlAnalyzer {
    pub fn new_from_content(content: &str) -> Result<Self, crate::error::LspError> {
        Ok(Self {
            source: content.to_string(),
        })
    }

    /// The SPARQL laws, surfaced as diagnostics carrying ggen's real E00XX codes.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<lsp_max_protocol::MaxDiagnostic> {
        let mut diags = Vec::new();

        // Syntax (oxigraph parser). spargebra does not expose a position, so we
        // anchor the message to the first line.
        if let Err(message) = check_sparql_syntax(&self.source) {
            diags.push(diag::max_whole_line(
                0,
                DiagnosticSeverity::ERROR,
                None,
                format!("SPARQL syntax error: {message}"),
                lsp_max_protocol::LawAxis::Autopoiesis,
            ));
        }

        // E0010 — VALUES forbidden in external .rq files.
        if query_contains_values(&self.source) {
            if let Some(line) = find_line(&self.source, "VALUES") {
                diags.push(diag::max_whole_line(
                    line,
                    DiagnosticSeverity::ERROR,
                    Some("E0010"),
                    "VALUES data must be inline in ggen.toml — External .rq files are for \
                     queries against real RDF triples only",
                    lsp_max_protocol::LawAxis::Domain,
                ));
            }
        }

        // E0011 — CONSTRUCT should carry ORDER BY. ERROR: matches
        // `default_strict_mode() -> true` (see module doc) -- `ggen sync run`
        // hard-refuses this under the schema default, so this analyzer must
        // not report a lower severity than the pipeline it is meant to preview.
        if sparql_kind(&self.source) == SparqlKind::Construct && !query_has_order_by(&self.source) {
            diags.push(diag::max_whole_line(
                0,
                DiagnosticSeverity::ERROR,
                Some("E0011"),
                "CONSTRUCT query lacks ORDER BY — refused under strict_mode (ggen's schema \
                 default: strict_mode = true; set `strict_mode = false` under [validation] in \
                 ggen.toml to downgrade this to a warning at sync time)",
                lsp_max_protocol::LawAxis::Domain,
            ));
        }

        // E0013 — SELECT should carry ORDER BY. ERROR: see E0011 above.
        if sparql_kind(&self.source) == SparqlKind::Select && !query_has_order_by(&self.source) {
            diags.push(diag::max_whole_line(
                0,
                DiagnosticSeverity::ERROR,
                Some("E0013"),
                "SELECT query lacks ORDER BY — refused under strict_mode (ggen's schema \
                 default: strict_mode = true; set `strict_mode = false` under [validation] in \
                 ggen.toml to downgrade this to a warning at sync time)",
                lsp_max_protocol::LawAxis::Domain,
            ));
        }

        // E0015 — Identity CONSTRUCT (no-op mapping).
        if sparql_kind(&self.source) == SparqlKind::Construct && is_identity_construct(&self.source)
        {
            diags.push(diag::max_whole_line(
                0,
                DiagnosticSeverity::WARNING,
                Some("E0015"),
                "Identity CONSTRUCT query detected — this is a no-op mapping that just \
                 copies triples. Ensure this is intended.",
                lsp_max_protocol::LawAxis::Domain,
            ));
        }

        diags
    }

    pub fn completion_at(&self, _line: u32, _character: u32) -> Option<CompletionResponse> {
        let mut items: Vec<CompletionItem> = KEYWORDS
            .iter()
            .map(|kw| CompletionItem {
                label: (*kw).to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            })
            .collect();
        for var in self.variables() {
            items.push(CompletionItem {
                label: var.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                ..Default::default()
            });
        }
        Some(CompletionResponse::Array(items))
    }

    pub fn hover_at(&self, _line: u32, _character: u32) -> Option<Hover> {
        None
    }

    pub fn definition_at(&self, _line: u32, _character: u32) -> Option<Location> {
        None
    }

    pub fn references_at(&self, _line: u32, _character: u32) -> Option<Vec<Location>> {
        None
    }

    pub fn semantic_tokens(&self) -> Option<lsp_max::lsp_types::SemanticTokens> {
        None
    }

    pub fn document_symbols(
        &self, _range: Option<Range>,
    ) -> Vec<lsp_max::lsp_types::DocumentSymbol> {
        self.variables()
            .into_iter()
            .map(|var| make_symbol(&var))
            .collect()
    }

    pub fn code_lenses(&self) -> Option<Vec<lsp_max::lsp_types::CodeLens>> {
        None
    }

    pub fn folding_ranges(&self) -> Option<Vec<lsp_max::lsp_types::FoldingRange>> {
        None
    }

    pub fn format_document(&self) -> Option<Vec<TextEdit>> {
        None
    }

    pub fn inlay_hints(&self, _range: Option<Range>) -> Vec<lsp_max::lsp_types::InlayHint> {
        Vec::new()
    }

    pub fn rename_symbol(&self, _position: Position, _new_name: &str) -> Option<WorkspaceEdit> {
        None
    }

    pub fn call_hierarchy_items(
        &self, _position: Position,
    ) -> Option<Vec<lsp_max::lsp_types::CallHierarchyItem>> {
        None
    }

    /// Collect `?var` / `$var` names used in the query (sorted, unique).
    fn variables(&self) -> Vec<String> {
        let mut set = BTreeSet::new();
        for token in self
            .source
            .split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '?' || c == '$'))
        {
            if (token.starts_with('?') || token.starts_with('$')) && token.len() > 1 {
                set.insert(token.to_string());
            }
        }
        set.into_iter().collect()
    }
}

fn find_line(source: &str, needle: &str) -> Option<u32> {
    source.lines().enumerate().find_map(|(idx, text)| {
        if text
            .split_whitespace()
            .any(|t| t.eq_ignore_ascii_case(needle))
        {
            u32::try_from(idx).ok()
        } else {
            None
        }
    })
}

fn make_symbol(name: &str) -> lsp_max::lsp_types::DocumentSymbol {
    let range = Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: 0,
            character: u32::MAX,
        },
    };
    #[allow(deprecated)]
    lsp_max::lsp_types::DocumentSymbol {
        name: name.to_string(),
        detail: None,
        kind: SymbolKind::VARIABLE,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    }
}

/// Returns true if `query` is an identity CONSTRUCT (no-op mapping).
fn is_identity_construct(query: &str) -> bool {
    let stripped: String = query
        .lines()
        .filter(|l| !l.trim_start().starts_with('#'))
        .collect::<Vec<_>>()
        .join(" ");
    let upper = stripped.to_uppercase();

    if !upper.contains("CONSTRUCT") {
        return false;
    }

    // 1) CONSTRUCT WHERE { ... }
    if let Some(pos) = upper.find("CONSTRUCT") {
        let after = upper[pos + 9..].trim();
        if after.starts_with("WHERE") {
            return true;
        }
    }

    // 2) CONSTRUCT { PATTERN } WHERE { PATTERN }
    if let (Some(s1), Some(e1)) = (upper.find('{'), upper.find('}')) {
        let block1 = upper[s1 + 1..e1].trim();
        if let Some(after_block1) = upper[e1 + 1..].find("WHERE") {
            let remainder = &upper[e1 + 1 + after_block1 + 5..];
            if let (Some(s2), Some(e2)) = (remainder.find('{'), remainder.find('}')) {
                let block2 = remainder[s2 + 1..e2].trim();
                if !block1.is_empty() && block1 == block2 {
                    return true;
                }
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn values_in_rq_triggers_e0010() {
        let q = "SELECT ?s WHERE { VALUES ?s { <http://x> } }";
        let analyzer = SparqlAnalyzer::new_from_content(q).expect("analyzer");
        let diags = analyzer.diagnostics();
        assert!(diags.iter().any(
            |d| d.lsp.code == Some(lsp_max::lsp_types::NumberOrString::String("E0010".into()))
        ));
    }

    #[test]
    fn construct_without_order_by_triggers_e0011() {
        let q = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
        let analyzer = SparqlAnalyzer::new_from_content(q).expect("analyzer");
        let diags = analyzer.diagnostics();
        assert!(diags.iter().any(
            |d| d.lsp.code == Some(lsp_max::lsp_types::NumberOrString::String("E0011".into()))
        ));
    }

    #[test]
    fn select_without_order_by_triggers_e0013() {
        let q = "SELECT ?s WHERE { ?s ?p ?o }";
        let analyzer = SparqlAnalyzer::new_from_content(q).expect("analyzer");
        let diags = analyzer.diagnostics();
        assert!(diags.iter().any(
            |d| d.lsp.code == Some(lsp_max::lsp_types::NumberOrString::String("E0013".into()))
        ));
    }

    #[test]
    fn identity_construct_triggers_e0015() {
        let q = "CONSTRUCT WHERE { ?s ?p ?o }";
        let analyzer = SparqlAnalyzer::new_from_content(q).expect("analyzer");
        let diags = analyzer.diagnostics();
        assert!(diags.iter().any(
            |d| d.lsp.code == Some(lsp_max::lsp_types::NumberOrString::String("E0015".into()))
        ));

        let q2 = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
        let analyzer2 = SparqlAnalyzer::new_from_content(q2).expect("analyzer");
        let diags2 = analyzer2.diagnostics();
        assert!(diags2.iter().any(
            |d| d.lsp.code == Some(lsp_max::lsp_types::NumberOrString::String("E0015".into()))
        ));
    }

    #[test]
    fn clean_select_has_no_law_violations() {
        let q = "SELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s";
        let analyzer = SparqlAnalyzer::new_from_content(q).expect("analyzer");
        assert!(analyzer.diagnostics().is_empty());
    }

    #[test]
    fn syntax_error_is_reported() {
        let q = "SELECT WHERE {{{";
        let analyzer = SparqlAnalyzer::new_from_content(q).expect("analyzer");
        assert!(!analyzer.diagnostics().is_empty());
    }
}
