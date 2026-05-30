pub mod diag;
pub mod harness_analyzer;
pub mod rdf_analyzer;
pub mod sparql_analyzer;
pub mod tera_analyzer;
pub mod toml_analyzer;

use std::fmt;
use tower_lsp::lsp_types::{
    CallHierarchyItem, CodeLens, CompletionResponse, Diagnostic, DocumentSymbol, FoldingRange,
    Hover, InlayHint, Location, Position, SemanticTokens, TextEdit, TypeHierarchyItem,
    WorkspaceEdit,
};

pub use harness_analyzer::{harness_mismatch_diagnostics, DeclaredTarget, GGEN_HARNESS_001};
pub use rdf_analyzer::{RdfAnalyzer, RdfFlavor};
pub use sparql_analyzer::SparqlAnalyzer;
pub use tera_analyzer::{unbound_projection_diagnostics, TeraAnalyzer, GGEN_TPL_001};
pub use toml_analyzer::TomlAnalyzer;

use crate::state::FileType;

/// Cross-surface GGEN-HARNESS-001 detection over a harness index.
///
/// Runs the pure [`harness_mismatch_diagnostics`] detector against the index's
/// declared targets and on-disk proof files. Groups every resulting diagnostic
/// onto the crate's `Cargo.toml` — the declaration/squiggle surface. Mirror of
/// [`detect_tpl_001`]'s `Vec<(PathBuf, Vec<Diagnostic>)>` shape. Reads no files
/// (the [`crate::harness_index::HarnessIndex`] already did the I/O).
#[must_use]
pub fn detect_harness_001(
    index: &crate::harness_index::HarnessIndex,
) -> Vec<(std::path::PathBuf, Vec<Diagnostic>)> {
    let diags = harness_mismatch_diagnostics(&index.targets, &index.existing_files);
    if diags.is_empty() {
        return Vec::new();
    }
    // Every mismatch in this index anchors on the same crate Cargo.toml.
    vec![(index.root.join("Cargo.toml"), diags)]
}

/// Cross-surface GGEN-TPL-001 detection over a whole project index.
///
/// For each rule whose template content was resolved, run the pure
/// [`unbound_projection_diagnostics`] detector against the rule's SPARQL
/// `SELECT` variables. Rules with no resolved template content are skipped —
/// a missing template is an index-level issue (Agent 1's `issues`), not
/// GGEN-TPL-001. Reads/writes no files: the index already did the I/O.
///
/// The returned route is always source-law repair (handled by the route
/// engine); diagnostics never target emitted output.
#[must_use]
pub fn detect_tpl_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<Diagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let Some(template) = entry.template_content.as_deref() else {
            continue;
        };
        let diags = unbound_projection_diagnostics(template, &entry.selected_vars);
        if !diags.is_empty() {
            out.push((entry.template_path.clone().unwrap_or_default(), diags));
        }
    }
    out
}

/// Build the appropriate analyzer for a document path + content, or `None` if the
/// path is not a recognized ggen law surface.
///
/// Shared by the interactive server
/// and the headless `ggen lsp check` gate so both enforce identical law.
#[must_use]
pub fn build_analyzer(path: &str, content: &str) -> Option<DocumentAnalyzer> {
    match FileType::from_path(path) {
        FileType::Rdf => Some(DocumentAnalyzer::Rdf(RdfAnalyzer::with_flavor(
            content,
            RdfFlavor::from_path(path),
        ))),
        FileType::Sparql => SparqlAnalyzer::new_from_content(content)
            .ok()
            .map(DocumentAnalyzer::Sparql),
        FileType::Tera => TeraAnalyzer::new_from_content(content, "")
            .ok()
            .map(DocumentAnalyzer::Tera),
        FileType::Toml => TomlAnalyzer::new_from_content(content)
            .ok()
            .map(DocumentAnalyzer::Toml),
        FileType::Unknown => None,
    }
}

/// A document analyzer dispatched on file type. Each variant is a pure function
/// over document text, so the same analyzer drives both the interactive server
/// and the headless `ggen lsp check` gate.
#[derive(Clone)]
pub enum DocumentAnalyzer {
    Rdf(RdfAnalyzer),
    Sparql(SparqlAnalyzer),
    Tera(TeraAnalyzer),
    Toml(TomlAnalyzer),
}

impl fmt::Debug for DocumentAnalyzer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Rdf(_) => f.write_str("DocumentAnalyzer::Rdf"),
            Self::Sparql(_) => f.write_str("DocumentAnalyzer::Sparql"),
            Self::Tera(_) => f.write_str("DocumentAnalyzer::Tera"),
            Self::Toml(_) => f.write_str("DocumentAnalyzer::Toml"),
        }
    }
}

impl DocumentAnalyzer {
    /// The law-surface diagnostics for this document — the core of the LSP-as-type-system.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        match self {
            Self::Rdf(a) => a.diagnostics(),
            Self::Sparql(a) => a.diagnostics(),
            Self::Tera(a) => a.diagnostics(),
            Self::Toml(a) => a.diagnostics(),
        }
    }

    pub fn completion_at(&self, line: u32, character: u32) -> Option<CompletionResponse> {
        match self {
            Self::Rdf(a) => a.completion_at(line, character),
            Self::Sparql(a) => a.completion_at(line, character),
            Self::Tera(a) => a.completion_at(line, character),
            Self::Toml(a) => a.completion_at(line, character),
        }
    }

    pub fn hover_at(&self, line: u32, character: u32) -> Option<Hover> {
        match self {
            Self::Rdf(a) => a.hover_at(line, character),
            Self::Sparql(a) => a.hover_at(line, character),
            Self::Tera(a) => a.hover_at(line, character),
            Self::Toml(a) => a.hover_at(line, character),
        }
    }

    pub fn definition_at(&self, line: u32, character: u32) -> Option<Location> {
        match self {
            Self::Rdf(a) => a.definition_at(line, character),
            Self::Sparql(a) => a.definition_at(line, character),
            Self::Tera(a) => a.definition_at(line, character),
            Self::Toml(_) => None,
        }
    }

    pub fn references_at(&self, line: u32, character: u32) -> Option<Vec<Location>> {
        match self {
            Self::Rdf(a) => a.references_at(line, character),
            Self::Sparql(a) => a.references_at(line, character),
            Self::Tera(a) => a.references_at(line, character),
            Self::Toml(_) => None,
        }
    }

    pub fn semantic_tokens(&self) -> Option<SemanticTokens> {
        match self {
            Self::Rdf(a) => a.semantic_tokens(),
            Self::Sparql(a) => a.semantic_tokens(),
            Self::Tera(a) => a.semantic_tokens(),
            Self::Toml(a) => a.semantic_tokens(),
        }
    }

    pub fn document_symbols(&self) -> Option<Vec<DocumentSymbol>> {
        match self {
            Self::Rdf(a) => a.document_symbols(),
            Self::Sparql(a) => a.document_symbols(),
            Self::Tera(a) => a.document_symbols(),
            Self::Toml(a) => a.document_symbols(),
        }
    }

    pub fn code_lenses(&self) -> Option<Vec<CodeLens>> {
        match self {
            Self::Rdf(a) => a.code_lenses(),
            Self::Sparql(a) => a.code_lenses(),
            Self::Tera(a) => a.code_lenses(),
            Self::Toml(a) => a.code_lenses(),
        }
    }

    pub fn folding_ranges(&self) -> Option<Vec<FoldingRange>> {
        match self {
            Self::Rdf(a) => a.folding_ranges(),
            Self::Sparql(a) => a.folding_ranges(),
            Self::Tera(a) => a.folding_ranges(),
            Self::Toml(a) => a.folding_ranges(),
        }
    }

    pub fn format_document(&self) -> Option<Vec<TextEdit>> {
        match self {
            Self::Rdf(a) => a.format_document(),
            Self::Sparql(a) => a.format_document(),
            Self::Tera(a) => a.format_document(),
            Self::Toml(a) => a.format_document(),
        }
    }

    pub fn inlay_hints(&self) -> Option<Vec<InlayHint>> {
        match self {
            Self::Rdf(a) => a.inlay_hints(),
            Self::Sparql(a) => a.inlay_hints(),
            Self::Tera(a) => a.inlay_hints(),
            Self::Toml(a) => a.inlay_hints(),
        }
    }

    pub fn rename_symbol(&self, position: Position, new_name: &str) -> Option<WorkspaceEdit> {
        match self {
            Self::Rdf(a) => a.rename_symbol(position, new_name),
            Self::Sparql(a) => a.rename_symbol(position, new_name),
            Self::Tera(a) => a.rename_symbol(position, new_name),
            Self::Toml(a) => a.rename_symbol(position, new_name),
        }
    }

    pub fn call_hierarchy_items(&self, position: Position) -> Option<Vec<CallHierarchyItem>> {
        match self {
            Self::Rdf(a) => a.call_hierarchy_items(position),
            Self::Sparql(a) => a.call_hierarchy_items(position),
            Self::Tera(a) => a.call_hierarchy_items(position),
            Self::Toml(_) => None,
        }
    }

    pub fn type_hierarchy_items(&self, position: Position) -> Option<Vec<TypeHierarchyItem>> {
        match self {
            Self::Rdf(a) => a.type_hierarchy_items(position),
            Self::Sparql(_) | Self::Tera(_) | Self::Toml(_) => None,
        }
    }
}
