//! `ggen.toml` law-surface analyzer.
//!
//! Surfaces TOML syntax errors (located via byte-span → line/col) and a set of
//! enum/key laws drawn from ggen's config schema: `ai.provider`, `logging.level`,
//! `logging.format`, and `rdf.default_format`. Offers completion for the known
//! sections and enum values so agents can only author admitted configuration.

use tower_lsp::lsp_types::{
    CodeLens, CompletionItem, CompletionItemKind, CompletionResponse, Diagnostic,
    DiagnosticSeverity, DocumentSymbol, FoldingRange, Hover, HoverContents, InlayHint, MarkupContent,
    MarkupKind, Position, Range, SymbolKind, TextEdit, WorkspaceEdit,
};

use crate::analyzers::diag;

/// (key path, allowed values) enum laws from ggen's config schema.
const ENUMS: &[(&str, &[&str])] = &[
    ("level", &["debug", "info", "warn", "error"]),
    ("format", &["json", "text"]),
    ("default_format", &["turtle", "rdfxml", "ntriples", "nquads", "trig"]),
];

const SECTIONS: &[&str] = &["project", "ontology", "validation", "logging", "rdf", "generation"];

#[derive(Clone)]
pub struct TomlAnalyzer {
    source: String,
}

impl TomlAnalyzer {
    pub fn new_from_content(content: &str) -> Result<Self, crate::error::LspError> {
        Ok(Self {
            source: content.to_string(),
        })
    }

    /// TOML syntax + enum-law diagnostics (E0001 for parse, E0023 for enum law).
    #[must_use]
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        // Syntax first: a parse failure means nothing else can be trusted.
        if let Err(err) = toml::from_str::<toml::Value>(&self.source) {
            let (line, col) = err
                .span()
                .map(|s| byte_to_line_col(&self.source, s.start))
                .unwrap_or((0, 0));
            return vec![diag::at(
                line,
                col,
                line,
                u32::MAX,
                DiagnosticSeverity::ERROR,
                Some("E0001"),
                format!("ggen.toml parse error: {}", err.message()),
            )];
        }

        let mut diags = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let line = u32::try_from(idx).unwrap_or(0);

            // Enum laws: `key = "value"` where key is a known enum and value is not allowed.
            let Some((raw_key, raw_val)) = text.split_once('=') else {
                continue;
            };
            let key = raw_key.trim();
            let value = raw_val.trim().trim_matches('"').trim();
            for (enum_key, allowed) in ENUMS {
                if key == *enum_key && !value.is_empty() && !allowed.contains(&value) {
                    diags.push(diag::at(
                        line,
                        0,
                        line,
                        u32::MAX,
                        DiagnosticSeverity::ERROR,
                        Some("E0023"),
                        format!(
                            "invalid value \"{value}\" for `{key}` — expected one of: {}",
                            allowed.join(", ")
                        ),
                    ));
                }
            }
        }
        diags
    }

    pub fn completion_at(&self, line: u32, _character: u32) -> Option<CompletionResponse> {
        let current = self.source.lines().nth(line as usize).unwrap_or("");
        let trimmed = current.trim_start();

        // Inside a section header `[` → suggest section names.
        if trimmed.starts_with('[') {
            let items = SECTIONS
                .iter()
                .map(|s| CompletionItem {
                    label: (*s).to_string(),
                    kind: Some(CompletionItemKind::MODULE),
                    ..Default::default()
                })
                .collect();
            return Some(CompletionResponse::Array(items));
        }

        // `key = ` where key is an enum → suggest allowed values.
        if let Some((raw_key, _)) = current.split_once('=') {
            let key = raw_key.trim();
            for (enum_key, allowed) in ENUMS {
                if key == *enum_key {
                    let items = allowed
                        .iter()
                        .map(|v| CompletionItem {
                            label: format!("\"{v}\""),
                            kind: Some(CompletionItemKind::ENUM_MEMBER),
                            ..Default::default()
                        })
                        .collect();
                    return Some(CompletionResponse::Array(items));
                }
            }
        }
        None
    }

    pub fn hover_at(&self, line: u32, _character: u32) -> Option<Hover> {
        let current = self.source.lines().nth(line as usize)?;
        let (raw_key, _) = current.split_once('=')?;
        let key = raw_key.trim();
        let (_, allowed) = ENUMS.iter().find(|(k, _)| *k == key)?;
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("`{key}` — allowed values: {}", allowed.join(", ")),
            }),
            range: None,
        })
    }

    pub fn semantic_tokens(&self) -> Option<tower_lsp::lsp_types::SemanticTokens> {
        None
    }

    pub fn document_symbols(&self) -> Option<Vec<DocumentSymbol>> {
        let mut symbols = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let trimmed = text.trim();
            if trimmed.starts_with('[') && trimmed.ends_with(']') {
                let name = trimmed.trim_matches(|c| c == '[' || c == ']');
                symbols.push(make_symbol(name, u32::try_from(idx).unwrap_or(0)));
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
        // Fold each [section] until the next section header.
        let mut ranges = Vec::new();
        let mut current: Option<u32> = None;
        let lines: Vec<&str> = self.source.lines().collect();
        for (idx, text) in lines.iter().enumerate() {
            let line = u32::try_from(idx).unwrap_or(0);
            if text.trim_start().starts_with('[') {
                if let Some(start) = current.take() {
                    if line > start + 1 {
                        ranges.push(section_fold(start, line - 1));
                    }
                }
                current = Some(line);
            }
        }
        if let Some(start) = current {
            let last = u32::try_from(lines.len().saturating_sub(1)).unwrap_or(start);
            if last > start + 1 {
                ranges.push(section_fold(start, last));
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
}

fn byte_to_line_col(source: &str, byte: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= byte {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

fn section_fold(start: u32, end: u32) -> FoldingRange {
    FoldingRange {
        start_line: start,
        end_line: end,
        start_character: None,
        end_character: None,
        kind: Some(tower_lsp::lsp_types::FoldingRangeKind::Region),
        collapsed_text: None,
    }
}

fn make_symbol(name: &str, line: u32) -> DocumentSymbol {
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
        kind: SymbolKind::NAMESPACE,
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
    fn invalid_log_level_enum_is_rejected() {
        let toml = "[logging]\nlevel = \"verbose\"\n";
        let analyzer = TomlAnalyzer::new_from_content(toml).expect("analyzer");
        let diags = analyzer.diagnostics();
        assert!(diags
            .iter()
            .any(|d| d.code == Some(tower_lsp::lsp_types::NumberOrString::String("E0023".into()))));
    }

    #[test]
    fn valid_log_level_enum_passes() {
        let toml = "[logging]\nlevel = \"info\"\n";
        let analyzer = TomlAnalyzer::new_from_content(toml).expect("analyzer");
        assert!(analyzer.diagnostics().is_empty());
    }

    #[test]
    fn ggen_does_not_police_llm_or_unknown_sections() {
        // ggen is not in the LLM business: an [ai] section is just an unknown
        // section, NOT a ggen-flagged diagnostic. The LSP validates structure +
        // known config enums only.
        let toml = "[ai]\nprovider = \"openai\"\n";
        let analyzer = TomlAnalyzer::new_from_content(toml).expect("analyzer");
        assert!(
            analyzer.diagnostics().is_empty(),
            "ggen must not emit diagnostics about LLM/unknown sections"
        );
    }

    #[test]
    fn syntax_error_reports_e0001_with_location() {
        let toml = "[logging\nlevel = \"info\"\n"; // missing closing ]
        let analyzer = TomlAnalyzer::new_from_content(toml).expect("analyzer");
        let diags = analyzer.diagnostics();
        assert_eq!(
            diags[0].code,
            Some(tower_lsp::lsp_types::NumberOrString::String("E0001".into()))
        );
    }

    #[test]
    fn enum_completion_offers_log_levels() {
        let toml = "[logging]\nlevel = \n";
        let analyzer = TomlAnalyzer::new_from_content(toml).expect("analyzer");
        let completions = analyzer.completion_at(1, 8).expect("completions");
        let CompletionResponse::Array(items) = completions else {
            panic!("array expected");
        };
        assert!(items.iter().any(|i| i.label == "\"info\""));
    }
}
