//! Tera template law-surface analyzer (`.tera`).
//!
//! Surfaces template syntax errors (ggen's **E0024**) at author time, plus
//! filter/keyword completion, an outline of `{% set %}`/`{% block %}`/`{% include %}`
//! directives, and folding over `{% for %}`/`{% if %}`/`{% block %}` blocks.

use std::collections::HashSet;
use tower_lsp::lsp_types::{
    CallHierarchyItem, CodeLens, CompletionItem, CompletionItemKind, CompletionResponse, Diagnostic,
    DiagnosticSeverity, DocumentSymbol, FoldingRange, FoldingRangeKind, Hover, InlayHint, Location,
    Position, Range, SymbolKind, TextEdit, WorkspaceEdit,
};

use crate::analyzers::diag;

const FILTERS: &[&str] = &[
    "upper", "lower", "capitalize", "trim", "truncate", "length", "reverse", "first", "last",
    "join", "default", "replace", "title", "wordcount", "slugify", "json_encode", "abs", "round",
];
const KEYWORDS: &[&str] = &[
    "if", "elif", "else", "endif", "for", "endfor", "block", "endblock", "set", "include",
    "extends", "macro", "endmacro", "filter", "endfilter", "raw", "endraw", "in", "and", "or",
    "not",
];

#[derive(Clone)]
pub struct TeraAnalyzer {
    source: String,
    #[allow(dead_code)]
    available_vars: HashSet<String>,
}

impl TeraAnalyzer {
    pub fn new_from_content(
        content: &str,
        sparql_bindings: &str,
    ) -> Result<Self, crate::error::LspError> {
        Ok(Self {
            source: content.to_string(),
            available_vars: Self::extract_sparql_vars(sparql_bindings),
        })
    }

    fn extract_sparql_vars(sparql: &str) -> HashSet<String> {
        let mut vars = HashSet::new();
        if let Some(select_idx) = sparql.find("SELECT") {
            let after = &sparql[select_idx + 6..];
            if let Some(where_idx) = after.find("WHERE") {
                for word in after[..where_idx].split_whitespace() {
                    if let Some(name) = word.strip_prefix('?') {
                        vars.insert(name.to_string());
                    }
                }
            }
        }
        vars
    }

    /// Template syntax diagnostics, carrying ggen's E0024 code.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut tera = tera::Tera::default();
        match tera.add_raw_template("__ggen_lsp_doc__", &self.source) {
            Ok(()) => Vec::new(),
            Err(err) => {
                let mut message = err.to_string();
                let mut source = std::error::Error::source(&err);
                while let Some(cause) = source {
                    message.push_str(&format!(": {cause}"));
                    source = cause.source();
                }
                vec![diag::whole_line(
                    0,
                    DiagnosticSeverity::ERROR,
                    Some("E0024"),
                    format!("Tera template error: {message}"),
                )]
            }
        }
    }

    pub fn completion_at(&self, _line: u32, _character: u32) -> Option<CompletionResponse> {
        let mut items: Vec<CompletionItem> = FILTERS
            .iter()
            .map(|f| CompletionItem {
                label: (*f).to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("Tera filter".to_string()),
                ..Default::default()
            })
            .collect();
        for kw in KEYWORDS {
            items.push(CompletionItem {
                label: (*kw).to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
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

    pub fn semantic_tokens(&self) -> Option<tower_lsp::lsp_types::SemanticTokens> {
        None
    }

    pub fn document_symbols(&self) -> Option<Vec<DocumentSymbol>> {
        let mut symbols = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let line = u32::try_from(idx).unwrap_or(0);
            for (marker, kind) in [
                ("{% set ", SymbolKind::VARIABLE),
                ("{% block ", SymbolKind::NAMESPACE),
                ("{% macro ", SymbolKind::FUNCTION),
                ("{% include ", SymbolKind::FILE),
            ] {
                if let Some(pos) = text.find(marker) {
                    let rest = &text[pos + marker.len()..];
                    let name: String = rest
                        .chars()
                        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '"' || *c == '.')
                        .collect();
                    if !name.trim().is_empty() {
                        symbols.push(make_symbol(name.trim_matches('"'), kind, line));
                    }
                }
            }
        }
        if symbols.is_empty() {
            None
        } else {
            Some(symbols)
        }
    }

    pub fn code_lenses(&self) -> Option<Vec<CodeLens>> {
        None
    }

    pub fn folding_ranges(&self) -> Option<Vec<FoldingRange>> {
        let mut ranges = Vec::new();
        let mut stack: Vec<u32> = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let line = u32::try_from(idx).unwrap_or(0);
            if ["{% for", "{% if", "{% block", "{% macro"]
                .iter()
                .any(|m| text.contains(m))
            {
                stack.push(line);
            }
            if ["{% endfor", "{% endif", "{% endblock", "{% endmacro"]
                .iter()
                .any(|m| text.contains(m))
            {
                if let Some(start) = stack.pop() {
                    if line > start {
                        ranges.push(FoldingRange {
                            start_line: start,
                            end_line: line,
                            start_character: None,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Region),
                            collapsed_text: None,
                        });
                    }
                }
            }
        }
        if ranges.is_empty() {
            None
        } else {
            Some(ranges)
        }
    }

    pub fn format_document(&self) -> Option<Vec<TextEdit>> {
        None
    }

    pub fn inlay_hints(&self) -> Option<Vec<InlayHint>> {
        None
    }

    pub fn rename_symbol(&self, _position: Position, _new_name: &str) -> Option<WorkspaceEdit> {
        None
    }

    pub fn call_hierarchy_items(&self, _position: Position) -> Option<Vec<CallHierarchyItem>> {
        None
    }
}

fn make_symbol(name: &str, kind: SymbolKind, line: u32) -> DocumentSymbol {
    let range = Range {
        start: Position { line, character: 0 },
        end: Position {
            line,
            character: u32::MAX,
        },
    };
    #[allow(deprecated)]
    DocumentSymbol {
        name: name.to_string(),
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_template_has_no_diagnostics() {
        let t = "Hello {{ name }}\n{% for x in items %}{{ x }}{% endfor %}\n";
        let analyzer = TeraAnalyzer::new_from_content(t, "").expect("analyzer");
        assert!(analyzer.diagnostics().is_empty());
    }

    #[test]
    fn unclosed_block_reports_e0024() {
        let t = "{% for x in items %}{{ x }}\n"; // missing endfor
        let analyzer = TeraAnalyzer::new_from_content(t, "").expect("analyzer");
        let diags = analyzer.diagnostics();
        assert!(!diags.is_empty());
        assert_eq!(
            diags[0].code,
            Some(tower_lsp::lsp_types::NumberOrString::String("E0024".into()))
        );
    }
}
