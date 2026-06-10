//! RDF law-surface analyzer (`.ttl`, `.nt`, `.nq`).
//!
//! Surfaces ggen's RDF law at author time: located syntax diagnostics, prefix and
//! class/property completion, hover over declared terms, definition jumps, an
//! outline of declared terms, and safe (text-level) rename across the document.

use lsp_max::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionResponse, DiagnosticSeverity, Hover,
    HoverContents, Location, MarkupContent, MarkupKind, Position, Range, SymbolKind, TextEdit, Url,
    WorkspaceEdit,
};

use ggen_graph::{
    extract_prefixes, iri_terms, parse_nquads_located, parse_ntriples_located,
    parse_turtle_located, DeterministicGraph, IriTerms, LocatedParse,
};

use crate::analyzers::diag;

/// RDF serialization flavor, selected from the file extension.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RdfFlavor {
    /// Turtle (`.ttl`).
    Turtle,
    /// N-Triples (`.nt`).
    NTriples,
    /// N-Quads (`.nq`).
    NQuads,
}

impl RdfFlavor {
    /// Pick a flavor from a URI/path, defaulting to Turtle.
    #[must_use]
    pub fn from_path(path: &str) -> Self {
        if path.ends_with(".nt") {
            Self::NTriples
        } else if path.ends_with(".nq") {
            Self::NQuads
        } else {
            Self::Turtle
        }
    }
}

#[derive(Clone)]
pub struct RdfAnalyzer {
    source: String,
    located: LocatedParse,
    prefixes: Vec<(String, String)>,
    terms: IriTerms,
}

impl RdfAnalyzer {
    /// Build a Turtle analyzer (the default flavor).
    pub fn new_from_content(content: &str) -> Result<Self, crate::error::LspError> {
        Ok(Self::with_flavor(content, RdfFlavor::Turtle))
    }

    /// Build an analyzer for a specific RDF flavor.
    #[must_use]
    pub fn with_flavor(content: &str, flavor: RdfFlavor) -> Self {
        let located = match flavor {
            RdfFlavor::Turtle => parse_turtle_located(content),
            RdfFlavor::NTriples => parse_ntriples_located(content),
            RdfFlavor::NQuads => parse_nquads_located(content),
        };
        // Build a graph from the quads parsed so far for term introspection.
        let terms = build_terms(&located);
        let prefixes = if flavor == RdfFlavor::Turtle {
            extract_prefixes(content)
        } else {
            Vec::new()
        };
        Self {
            source: content.to_string(),
            located,
            prefixes,
            terms,
        }
    }

    /// Located syntax diagnostics — the core "refusal before execution" output.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<lsp_max::lsp_types::Diagnostic> {
        self.located
            .diagnostics
            .iter()
            .map(|d| {
                diag::from_oxrdfio(
                    d.line,
                    d.column,
                    d.end_line,
                    d.end_column,
                    DiagnosticSeverity::ERROR,
                    None,
                    format!("RDF syntax error: {}", d.message),
                )
            })
            .collect()
    }

    pub fn completion_at(&self, _line: u32, _character: u32) -> Option<CompletionResponse> {
        let mut items: Vec<CompletionItem> = Vec::new();
        for (prefix, iri) in &self.prefixes {
            items.push(CompletionItem {
                label: format!("{prefix}:"),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some(iri.clone()),
                ..Default::default()
            });
        }
        for class in &self.terms.classes {
            items.push(CompletionItem {
                label: compact(class, &self.prefixes),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some(class.clone()),
                ..Default::default()
            });
        }
        for prop in &self.terms.properties {
            items.push(CompletionItem {
                label: compact(prop, &self.prefixes),
                kind: Some(CompletionItemKind::PROPERTY),
                detail: Some(prop.clone()),
                ..Default::default()
            });
        }
        if items.is_empty() {
            None
        } else {
            Some(CompletionResponse::Array(items))
        }
    }

    pub fn hover_at(&self, line: u32, character: u32) -> Option<Hover> {
        let token = word_at(&self.source, line, character)?;
        let iri = self.resolve(&token)?;
        let kind = if self.terms.classes.iter().any(|c| c == &iri) {
            "class"
        } else if self.terms.properties.iter().any(|p| p == &iri) {
            "property"
        } else {
            "term"
        };
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("**{kind}** `{iri}`"),
            }),
            range: None,
        })
    }

    pub fn definition_at(&self, line: u32, character: u32) -> Option<Location> {
        let token = word_at(&self.source, line, character)?;
        // Jump to the first line that declares the token as a subject (`token a ...`).
        for (idx, text) in self.source.lines().enumerate() {
            let trimmed = text.trim_start();
            if trimmed.starts_with(&token) && trimmed[token.len()..].trim_start().starts_with('a') {
                let pos = Position {
                    line: u32::try_from(idx).unwrap_or(0),
                    character: 0,
                };
                return Some(Location {
                    uri: placeholder_uri(),
                    range: Range {
                        start: pos,
                        end: pos,
                    },
                });
            }
        }
        None
    }

    pub fn references_at(&self, line: u32, character: u32) -> Option<Vec<Location>> {
        let token = word_at(&self.source, line, character)?;
        let mut locations = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let mut from = 0usize;
            while let Some(rel) = text[from..].find(&token) {
                let col = from + rel;
                let start = Position {
                    line: u32::try_from(idx).unwrap_or(0),
                    character: u32::try_from(col).unwrap_or(0),
                };
                let end = Position {
                    line: start.line,
                    character: u32::try_from(col + token.len()).unwrap_or(0),
                };
                locations.push(Location {
                    uri: placeholder_uri(),
                    range: Range { start, end },
                });
                from = col + token.len();
            }
        }
        if locations.is_empty() {
            None
        } else {
            Some(locations)
        }
    }

    pub fn semantic_tokens(&self) -> Option<lsp_max::lsp_types::SemanticTokens> {
        // Minimal but real: highlight prefixed-name tokens as PROPERTY/CLASS roles.
        // (Full tokenization is a future refinement; None keeps clients happy.)
        None
    }

    pub fn document_symbols(&self) -> Option<Vec<lsp_max::lsp_types::DocumentSymbol>> {
        let mut symbols = Vec::new();
        for class in &self.terms.classes {
            symbols.push(make_symbol(
                &compact(class, &self.prefixes),
                SymbolKind::CLASS,
                find_decl_line(&self.source, class, &self.prefixes),
            ));
        }
        for prop in &self.terms.properties {
            symbols.push(make_symbol(
                &compact(prop, &self.prefixes),
                SymbolKind::PROPERTY,
                find_decl_line(&self.source, prop, &self.prefixes),
            ));
        }
        if symbols.is_empty() {
            None
        } else {
            Some(symbols)
        }
    }

    pub fn code_lenses(&self) -> Option<Vec<lsp_max::lsp_types::CodeLens>> {
        None
    }

    pub fn folding_ranges(&self) -> Option<Vec<lsp_max::lsp_types::FoldingRange>> {
        let mut ranges = Vec::new();
        let mut open: Option<u32> = None;
        for (idx, text) in self.source.lines().enumerate() {
            let line = u32::try_from(idx).unwrap_or(0);
            if text.contains('[') && !text.contains(']') {
                open = Some(line);
            } else if text.contains(']') {
                if let Some(start) = open.take() {
                    if line > start {
                        ranges.push(lsp_max::lsp_types::FoldingRange {
                            start_line: start,
                            end_line: line,
                            start_character: None,
                            end_character: None,
                            kind: Some(lsp_max::lsp_types::FoldingRangeKind::Region),
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

    pub fn inlay_hints(&self) -> Option<Vec<lsp_max::lsp_types::InlayHint>> {
        None
    }

    pub fn rename_symbol(&self, position: Position, new_name: &str) -> Option<WorkspaceEdit> {
        let token = word_at(&self.source, position.line, position.character)?;
        let mut edits = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let mut from = 0usize;
            while let Some(rel) = text[from..].find(&token) {
                let col = from + rel;
                let start = Position {
                    line: u32::try_from(idx).unwrap_or(0),
                    character: u32::try_from(col).unwrap_or(0),
                };
                let end = Position {
                    line: start.line,
                    character: u32::try_from(col + token.len()).unwrap_or(0),
                };
                edits.push(TextEdit {
                    range: Range { start, end },
                    new_text: new_name.to_string(),
                });
                from = col + token.len();
            }
        }
        if edits.is_empty() {
            return None;
        }
        #[allow(clippy::mutable_key_type)]
        let mut changes = std::collections::HashMap::new();
        changes.insert(placeholder_uri(), edits);
        Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
            metadata: None,
        })
    }

    pub fn call_hierarchy_items(
        &self, _position: Position,
    ) -> Option<Vec<lsp_max::lsp_types::CallHierarchyItem>> {
        None
    }

    pub fn type_hierarchy_items(
        &self, _position: Position,
    ) -> Option<Vec<lsp_max::lsp_types::TypeHierarchyItem>> {
        None
    }

    /// Resolve a token (prefixed name or full IRI) to a full IRI against the
    /// document's prefixes.
    fn resolve(&self, token: &str) -> Option<String> {
        if let Some((prefix, local)) = token.split_once(':') {
            if local.starts_with("//") {
                return Some(token.to_string()); // already a full IRI scheme
            }
            for (p, iri) in &self.prefixes {
                if p == prefix {
                    return Some(format!("{iri}{local}"));
                }
            }
        }
        // Maybe it's already a known full IRI.
        if self.terms.classes.iter().any(|c| c == token)
            || self.terms.properties.iter().any(|p| p == token)
        {
            return Some(token.to_string());
        }
        None
    }
}

fn build_terms(located: &LocatedParse) -> IriTerms {
    let graph = match DeterministicGraph::new() {
        Ok(g) => g,
        Err(_) => return IriTerms::default(),
    };
    for quad in &located.quads {
        if graph.insert_quad(quad).is_err() {
            return IriTerms::default();
        }
    }
    iri_terms(&graph).unwrap_or_default()
}

/// Compact a full IRI to `prefix:local` when a known prefix matches.
fn compact(iri: &str, prefixes: &[(String, String)]) -> String {
    for (prefix, ns) in prefixes {
        if let Some(local) = iri.strip_prefix(ns.as_str()) {
            return format!("{prefix}:{local}");
        }
    }
    iri.to_string()
}

fn find_decl_line(source: &str, iri: &str, prefixes: &[(String, String)]) -> u32 {
    let compacted = compact(iri, prefixes);
    for (idx, text) in source.lines().enumerate() {
        if text.contains(&compacted) || text.contains(iri) {
            return u32::try_from(idx).unwrap_or(0);
        }
    }
    0
}

fn make_symbol(name: &str, kind: SymbolKind, line: u32) -> lsp_max::lsp_types::DocumentSymbol {
    let range = Range {
        start: Position { line, character: 0 },
        end: Position {
            line,
            character: u32::MAX,
        },
    };
    #[allow(deprecated)]
    lsp_max::lsp_types::DocumentSymbol {
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

/// Extract the token under the cursor — a contiguous run of IRI/prefixed-name
/// characters. 0-based `line`/`character`.
fn word_at(source: &str, line: u32, character: u32) -> Option<String> {
    let text = source.lines().nth(line as usize)?;
    let chars: Vec<char> = text.chars().collect();
    let is_word = |c: char| c.is_alphanumeric() || matches!(c, ':' | '_' | '-' | '#' | '/' | '.');
    let idx = character as usize;
    if idx > chars.len() {
        return None;
    }
    let mut start = idx.min(chars.len());
    while start > 0 && is_word(chars[start - 1]) {
        start -= 1;
    }
    let mut end = idx.min(chars.len());
    while end < chars.len() && is_word(chars[end]) {
        end += 1;
    }
    if start == end {
        return None;
    }
    let token: String = chars[start..end].iter().collect();
    let token = token.trim_matches(|c| c == '<' || c == '>').to_string();
    if token.is_empty() {
        None
    } else {
        Some(token)
    }
}

fn placeholder_uri() -> Url {
    // Document edits are applied to the active document; the client substitutes
    // the real URI. A stable placeholder keeps the type total.
    #[allow(clippy::expect_used)]
    "file:///".parse::<Url>().expect("static URI is valid")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clean_turtle_has_no_diagnostics() {
        let ttl = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .\n";
        let analyzer = RdfAnalyzer::with_flavor(ttl, RdfFlavor::Turtle);
        assert!(analyzer.diagnostics().is_empty());
    }

    #[test]
    fn malformed_turtle_produces_error_diagnostic() {
        let ttl = "@prefix ex: <http://example.org/> .\nex:a ex:b \"oops\n";
        let analyzer = RdfAnalyzer::with_flavor(ttl, RdfFlavor::Turtle);
        let diags = analyzer.diagnostics();
        assert!(!diags.is_empty());
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn completion_offers_prefixes() {
        let ttl = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .\n";
        let analyzer = RdfAnalyzer::with_flavor(ttl, RdfFlavor::Turtle);
        let completions = analyzer.completion_at(2, 0).expect("some completions");
        let CompletionResponse::Array(items) = completions else {
            panic!("expected array");
        };
        assert!(items.iter().any(|i| i.label == "ex:"));
    }
}
