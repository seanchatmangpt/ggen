pub mod diag;
pub mod harness_analyzer;
pub mod rdf_analyzer;
pub mod source_law_analyzer;
pub mod sparql_analyzer;
pub mod tera_analyzer;
pub mod toml_analyzer;

use std::collections::BTreeSet;
use std::fmt;
use tower_lsp::lsp_types::{
    CallHierarchyItem, CodeLens, CompletionResponse, Diagnostic, DocumentSymbol, FoldingRange,
    Hover, InlayHint, Location, Position, SemanticTokens, TextEdit, TypeHierarchyItem,
    WorkspaceEdit,
};

pub use harness_analyzer::{harness_mismatch_diagnostics, DeclaredTarget, GGEN_HARNESS_001};
pub use rdf_analyzer::{RdfAnalyzer, RdfFlavor};
pub use source_law_analyzer::{do_not_edit_diagnostics, GGEN_SRC_002, GGEN_SRC_003};
pub use sparql_analyzer::SparqlAnalyzer;
pub use tera_analyzer::{
    unbound_output_path_diagnostics, unbound_projection_diagnostics, unbound_rule_file_diagnostics,
    TeraAnalyzer, GGEN_OUT_001, GGEN_RULE_001, GGEN_TPL_001,
};
pub use toml_analyzer::{source_caste_path_violation, TomlAnalyzer, GGEN_SRC_001};

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

/// Cross-surface GGEN-OUT-001 detection over a whole project index.
///
/// The dual of [`detect_tpl_001`]: for each rule, run the pure
/// [`unbound_output_path_diagnostics`] detector against the rule's `output_file`
/// pattern and its SPARQL `SELECT` variables. Diagnostics anchor on the rule's
/// `ggen.toml` manifest (the declaration surface where `output_file` is written),
/// NOT on the template file (that is GGEN-TPL-001's anchor) and NEVER on an
/// emitted output file.
///
/// Rules whose `selected_vars` is empty (`SELECT *` / a missing query file) are
/// SKIPPED: there is no introspectable producer to compare the output-path
/// references against, so firing would be noise rather than law. Static
/// `output_file` paths (no `{{`) are silent by construction inside the pure
/// detector. Reads/writes no files: the index already did the I/O.
///
/// The returned route is always source-law repair (handled by the route engine);
/// diagnostics never target emitted output.
#[must_use]
pub fn detect_out_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<Diagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        if entry.selected_vars.is_empty() {
            continue; // SELECT * / missing query → no lawful comparison
        }
        let diags = unbound_output_path_diagnostics(&entry.output_file, &entry.selected_vars);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-RULE-001 detection over a whole project index.
///
/// For each rule, run the pure [`unbound_rule_file_diagnostics`] detector against
/// the rule's index-level `issues`, surfacing any MISSING query/template
/// `{file=...}` as a live GGEN-RULE-001 error. This is the FOUNDATIONAL
/// binding-integrity check the GGEN-TPL-001 / GGEN-OUT-001 detectors PRESUPPOSE:
/// they skip a rule whose bound file is missing (`template_content` `None` /
/// empty `selected_vars`), so the previously-silent
/// [`crate::rule_index::RuleIndexEntry::issues`] channel gains exactly one lawful
/// publishing consumer.
///
/// Diagnostics anchor on the rule's `ggen.toml` manifest (the declaration surface
/// where the dangling `{file=...}` is written), NEVER an emitted output. Reads /
/// writes no files: the index already did its overlay-aware I/O.
#[must_use]
pub fn detect_rule_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<Diagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let diags = unbound_rule_file_diagnostics(&entry.issues);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-SRC-001 detection over a whole project index.
///
/// For each generation rule, check that `output_file` does not target a
/// source-caste directory (`generated/`, `output/`, `outputs/`, `gen/`, etc.).
/// RenderedSource is source — caste paths are a first-class source law violation.
/// Diagnostics anchor on the rule's `ggen.toml` manifest.
#[must_use]
pub fn detect_src_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<Diagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let output = &entry.output_file;
        if let Some(violated) = source_caste_path_violation(output) {
            // Anchor the diagnostic on the manifest where output_file is declared.
            let diags = vec![diag::at(
                0,
                0,
                0,
                u32::MAX,
                tower_lsp::lsp_types::DiagnosticSeverity::ERROR,
                Some(GGEN_SRC_001),
                format!(
                    "GGEN-SRC-001 SECOND_CLASS_PATH: rule `{}` output_file `{output}` \
                     targets a source-caste directory (`{violated}/`). \
                     RenderedSource is source — move to a first-class path.",
                    entry.rule_id
                ),
            )];
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-SRC-002/003 detection over a project's emitted Rust sources.
///
/// Scans every `.rs` file in the project root's `src/` tree for DO NOT EDIT
/// banners and source-caste comments. Returns diagnostics anchored on the
/// offending source file.
///
/// This is a file-system scan, not an index lookup: it runs over whatever `.rs`
/// files actually exist, so it catches violations whether or not ggen knows about them.
#[must_use]
pub fn detect_src_002_003_in_dir(
    src_dir: &std::path::Path,
) -> Vec<(std::path::PathBuf, Vec<Diagnostic>)> {
    let mut out = Vec::new();
    let Ok(entries) = std::fs::read_dir(src_dir) else {
        return out;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        let diags = do_not_edit_diagnostics(&content);
        if !diags.is_empty() {
            out.push((path, diags));
        }
    }
    out
}

/// Canonical SPARQL `SELECT`-projection variable extractor (no leading `?`).
///
/// Single source of truth shared by [`crate::rule_index`] (`selected_vars`,
/// the law-of-record) and [`tera_analyzer`] (`available_vars`, single-file
/// isolation). Both call sites previously carried divergent copies — a
/// case-sensitive, paren-blind one in the analyzer and the robust one here —
/// which could disagree on the same query. Unifying removes that drift.
///
/// `issues` (when `Some`) receives a non-fatal note for `SELECT *`.
///
/// Algorithm:
/// 1. Find `SELECT` (case-insensitive).
/// 2. Take the text after `SELECT` up to the next `WHERE` (case-insensitive);
///    if no `WHERE`, use the rest of the string.
/// 3. Collect whitespace/paren-delimited tokens starting with `?`, strip the
///    `?`, keep only the leading SPARQL-var-char run, and insert into a
///    [`BTreeSet`].
/// 4. `SELECT DISTINCT` is handled naturally (`DISTINCT` is not a `?`-token).
/// 5. `SELECT *` yields an empty set and (if a sink is provided) pushes an info
///    issue.
#[must_use]
pub(crate) fn select_projection_vars(
    query: &str, issues: Option<&mut Vec<String>>,
) -> BTreeSet<String> {
    let mut vars = BTreeSet::new();

    let lower = query.to_ascii_lowercase();
    let select_pos = match lower.find("select") {
        Some(pos) => pos,
        None => return vars,
    };

    // Slice starting right after the literal "select".
    let after_select = &query[select_pos + "select".len()..];
    let after_select_lower = &lower[select_pos + "select".len()..];

    // Bound the projection by the first WHERE (if present).
    let projection = match after_select_lower.find("where") {
        Some(where_pos) => &after_select[..where_pos],
        None => after_select,
    };

    let mut saw_star = false;
    for raw in projection.split(|c: char| c.is_whitespace() || c == '(' || c == ')') {
        let token = raw.trim();
        if token.is_empty() {
            continue;
        }
        if token == "*" {
            saw_star = true;
            continue;
        }
        if let Some(name) = token.strip_prefix('?') {
            // Guard against bindings like `?x` followed by punctuation; keep the
            // leading identifier-ish run only (SPARQL var chars).
            let cleaned: String = name
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            if !cleaned.is_empty() {
                vars.insert(cleaned);
            }
        }
    }

    if vars.is_empty() && saw_star {
        if let Some(issues) = issues {
            issues.push(
                "SELECT * is not introspectable: no explicit projection variables".to_string(),
            );
        }
    }

    vars
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
