pub mod diag;
pub mod harness_analyzer;
pub mod rdf_analyzer;
pub mod source_law_analyzer;
pub mod sparql_analyzer;
pub mod tera_analyzer;
pub mod toml_analyzer;

use lsp_max::lsp_types_max::{
    CallHierarchyItem, CodeLens, CompletionResponse, DocumentSymbol, FoldingRange, Hover,
    InlayHint, Location, Position, Range, SemanticTokens, TextEdit, TypeHierarchyItem,
    WorkspaceEdit,
};
use lsp_max_protocol::MaxDiagnostic;
use std::collections::BTreeSet;
use std::fmt;

pub use harness_analyzer::{harness_mismatch_diagnostics, DeclaredTarget, GGEN_HARNESS_001};
pub use rdf_analyzer::{RdfAnalyzer, RdfFlavor};
pub use source_law_analyzer::{do_not_edit_diagnostics, GGEN_SRC_002, GGEN_SRC_003};
pub use sparql_analyzer::SparqlAnalyzer;
pub use tera_analyzer::{
    is_select_star, lexical_clean, pack_001_diagnostics, select_star_diagnostics, strip_tera_vars,
    unbound_output_path_diagnostics, unbound_projection_diagnostics, unbound_rule_file_diagnostics,
    yield_001_diagnostics, yield_003_diagnostics, yield_004_diagnostics, yield_005_diagnostics,
    TeraAnalyzer, GGEN_OUT_001, GGEN_PACK_001, GGEN_QUERY_002, GGEN_RULE_001, GGEN_TPL_001,
    GGEN_YIELD_001, GGEN_YIELD_003, GGEN_YIELD_004, GGEN_YIELD_005,
};
pub use toml_analyzer::{source_caste_path_violation, TomlAnalyzer, GGEN_SRC_001};

use crate::state::FileType;

/// Cross-surface GGEN-HARNESS-001 detection over a harness index.
#[must_use]
pub fn detect_harness_001(
    index: &crate::harness_index::HarnessIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let diags = harness_mismatch_diagnostics(&index.targets, &index.existing_files);
    if diags.is_empty() {
        return Vec::new();
    }
    let max_diags = diags
        .into_iter()
        .map(|d| to_max_diagnostic(d, lsp_max_protocol::LawAxis::Fixture))
        .collect();
    vec![(index.root.join("Cargo.toml"), max_diags)]
}

/// Cross-surface GGEN-TPL-001 detection over a whole project index.
#[must_use]
pub fn detect_tpl_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
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
#[must_use]
pub fn detect_out_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        if entry.selected_vars.is_empty() {
            continue;
        }
        let diags = unbound_output_path_diagnostics(&entry.output_file, &entry.selected_vars);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-RULE-001 detection over a whole project index.
#[must_use]
pub fn detect_rule_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
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
#[must_use]
pub fn detect_src_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let output = &entry.output_file;
        if let Some(violated) = source_caste_path_violation(output) {
            let msg = format!(
                "GGEN-SRC-001 SECOND_CLASS_PATH: rule `{}` output_file `{output}` \
                 targets a source-caste directory (`{violated}/`). \
                 RenderedSource is source — move to a first-class path.",
                entry.rule_id
            );
            let diags = vec![diag::max_whole_line(
                0,
                lsp_max::lsp_types::DiagnosticSeverity::ERROR,
                Some(GGEN_SRC_001),
                msg,
                lsp_max_protocol::LawAxis::Domain,
            )];
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-SRC-002/003 detection over a project's emitted Rust sources.
#[must_use]
pub fn detect_src_002_003_in_dir(
    src_dir: &std::path::Path,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
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
            let max_diags = diags
                .into_iter()
                .map(|d| to_max_diagnostic(d, lsp_max_protocol::LawAxis::Autopoiesis))
                .collect();
            out.push((path, max_diags));
        }
    }
    out
}

/// Cross-surface GGEN-YIELD-001 detection: output_file escapes the project root.
///
/// For each rule whose `output_file` pattern (after stripping Tera `{{ }}`
/// interpolations) resolves outside `project_root`, emits an ERROR diagnostic
/// anchored on the `ggen.toml` declaration surface. Best-effort: a missing or
/// unparseable manifest yields no diagnostics.
#[must_use]
pub fn detect_yield_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let diags = yield_001_diagnostics(&entry.output_file, &entry.manifest_path, &project.root);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-YIELD-004 detection: multiple rules target the same output file.
#[must_use]
pub fn detect_yield_004(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    let mut output_to_rules: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();

    for entry in &project.rule_entries {
        output_to_rules
            .entry(entry.output_file.clone())
            .or_default()
            .push(entry.rule_id.clone());
    }

    for entry in &project.rule_entries {
        if let Some(rules) = output_to_rules.get(&entry.output_file) {
            if rules.len() > 1 {
                let competing: Vec<String> = rules
                    .iter()
                    .filter(|&r| r != &entry.rule_id)
                    .cloned()
                    .collect();
                let diags = yield_004_diagnostics(&entry.rule_id, &entry.output_file, &competing);
                if !diags.is_empty() {
                    out.push((entry.manifest_path.clone(), diags));
                }
            }
        }
    }
    out
}

/// Cross-surface GGEN-YIELD-003 detection: output_file lacks a static filename base.
#[must_use]
pub fn detect_yield_003(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let diags = yield_003_diagnostics(&entry.output_file);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-YIELD-005 detection: output_file is a remote URL.
#[must_use]
pub fn detect_yield_005(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let diags = yield_005_diagnostics(&entry.output_file);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-QUERY-002 detection: SELECT * disables TPL-001/OUT-001.
///
/// For each rule that uses `SELECT *`, emits a WARNING diagnostic anchored on
/// the `ggen.toml` declaration surface. Pack-sourced queries (resolved at
/// generation time) are skipped. Best-effort: a missing manifest yields no
/// diagnostics.
#[must_use]
pub fn detect_query_002(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        // Skip pack-sourced queries (empty query_content with no inline flag).
        if entry.query_content.is_empty() && !entry.query_inline {
            continue;
        }
        let diags = select_star_diagnostics(&entry.rule_id, &entry.query_content);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

/// Cross-surface GGEN-PACK-001 detection: pack-sourced query/template disables
/// author-time provision checks (GGEN-TPL-001 / GGEN-OUT-001).
///
/// For each rule whose query or template resolves via `QuerySource::Pack` /
/// `TemplateSource::Pack`, emits a WARNING diagnostic on the `ggen.toml` surface
/// so the author knows author-time checks are inactive for that rule. Best-effort:
/// a missing manifest yields no diagnostics.
#[must_use]
pub fn detect_pack_001(
    project: &crate::project_index::ProjectIndex,
) -> Vec<(std::path::PathBuf, Vec<MaxDiagnostic>)> {
    let mut out = Vec::new();
    for entry in &project.rule_entries {
        let diags = pack_001_diagnostics(&entry.issues);
        if !diags.is_empty() {
            out.push((entry.manifest_path.clone(), diags));
        }
    }
    out
}

fn to_max_diagnostic(
    d: lsp_max::lsp_types::Diagnostic, axis: lsp_max_protocol::LawAxis,
) -> lsp_max_protocol::MaxDiagnostic {
    let code_str = match &d.code {
        Some(lsp_max::lsp_types::NumberOrString::String(s)) => s.clone(),
        Some(lsp_max::lsp_types::NumberOrString::Number(n)) => n.to_string(),
        None => "GGEN-UNKNOWN".to_string(),
    };
    let mut h = blake3::Hasher::new();
    h.update(d.message.as_bytes());
    let id = format!("{}-{:.8}", code_str, h.finalize().to_hex());

    let lsp_max_diag: lsp_max::lsp_types_max::Diagnostic =
        serde_json::from_value(serde_json::to_value(d).unwrap()).unwrap();

    lsp_max_protocol::MaxDiagnostic {
        lsp: lsp_max_diag.clone(),
        diagnostic_id: id,
        law_id: code_str,
        law_axis: axis,
        violated_invariant: lsp_max_diag.message.clone(),
        ..lsp_max_protocol::MaxDiagnostic::default()
    }
}

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
    let after_select = &query[select_pos + "select".len()..];
    let after_select_lower = &lower[select_pos + "select".len()..];
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

pub trait Analyzer {
    fn diagnostics(&self) -> Vec<MaxDiagnostic>;
    fn completion_at(&self, line: u32, character: u32) -> Option<CompletionResponse>;
    fn hover_at(&self, line: u32, character: u32) -> Option<Hover>;
    fn definition_at(&self, line: u32, character: u32) -> Option<Location>;
    fn references_at(&self, line: u32, character: u32) -> Option<Vec<Location>>;
    fn semantic_tokens(&self) -> Option<SemanticTokens>;
    fn document_symbols(&self, range: Option<Range>) -> Vec<DocumentSymbol>;
    fn code_lenses(&self) -> Option<Vec<CodeLens>>;
    fn folding_ranges(&self) -> Option<Vec<FoldingRange>>;
    fn format_document(&self) -> Option<Vec<TextEdit>>;
    fn inlay_hints(&self, range: Option<Range>) -> Vec<InlayHint>;
    fn prepare_rename(&self, position: Position) -> Option<Range>;
    fn rename_symbol(&self, position: Position, new_name: &str) -> Option<WorkspaceEdit>;
    fn call_hierarchy_items(&self, position: Position) -> Option<Vec<CallHierarchyItem>>;
    fn type_hierarchy_items(&self, position: Position) -> Option<Vec<TypeHierarchyItem>>;
}

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
    #[must_use]
    pub fn diagnostics(&self) -> Vec<MaxDiagnostic> {
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

    pub fn document_symbols(&self, range: Option<Range>) -> Vec<DocumentSymbol> {
        match self {
            Self::Rdf(a) => a
                .document_symbols(range)
                .into_iter()
                .map(|s| serde_json::from_value(serde_json::to_value(s).unwrap()).unwrap())
                .collect(),
            Self::Sparql(a) => a
                .document_symbols(range)
                .into_iter()
                .map(|s| serde_json::from_value(serde_json::to_value(s).unwrap()).unwrap())
                .collect(),
            Self::Tera(a) => a.document_symbols(range),
            Self::Toml(a) => a.document_symbols(range),
        }
    }

    pub fn code_lenses(&self) -> Option<Vec<CodeLens>> {
        match self {
            Self::Rdf(a) => a.code_lenses().map(|v| {
                v.into_iter()
                    .map(|l| serde_json::from_value(serde_json::to_value(l).unwrap()).unwrap())
                    .collect()
            }),
            Self::Sparql(a) => a.code_lenses().map(|v| {
                v.into_iter()
                    .map(|l| serde_json::from_value(serde_json::to_value(l).unwrap()).unwrap())
                    .collect()
            }),
            Self::Tera(a) => a.code_lenses(),
            Self::Toml(a) => a.code_lenses(),
        }
    }

    pub fn folding_ranges(&self) -> Option<Vec<FoldingRange>> {
        match self {
            Self::Rdf(a) => a.folding_ranges().map(|v| {
                v.into_iter()
                    .map(|r| serde_json::from_value(serde_json::to_value(r).unwrap()).unwrap())
                    .collect()
            }),
            Self::Sparql(a) => a.folding_ranges().map(|v| {
                v.into_iter()
                    .map(|r| serde_json::from_value(serde_json::to_value(r).unwrap()).unwrap())
                    .collect()
            }),
            Self::Tera(a) => a.folding_ranges(),
            Self::Toml(a) => a.folding_ranges(),
        }
    }

    pub fn format_document(&self) -> Option<Vec<TextEdit>> {
        match self {
            Self::Rdf(a) => a.format_document().map(|v| {
                v.into_iter()
                    .map(|e| serde_json::from_value(serde_json::to_value(e).unwrap()).unwrap())
                    .collect()
            }),
            Self::Sparql(a) => a.format_document().map(|v| {
                v.into_iter()
                    .map(|e| serde_json::from_value(serde_json::to_value(e).unwrap()).unwrap())
                    .collect()
            }),
            Self::Tera(a) => a.format_document(),
            Self::Toml(a) => a.format_document(),
        }
    }

    pub fn inlay_hints(&self, range: Option<Range>) -> Vec<InlayHint> {
        match self {
            Self::Rdf(a) => a
                .inlay_hints(range)
                .into_iter()
                .map(|h| serde_json::from_value(serde_json::to_value(h).unwrap()).unwrap())
                .collect(),
            Self::Sparql(a) => a
                .inlay_hints(range)
                .into_iter()
                .map(|h| serde_json::from_value(serde_json::to_value(h).unwrap()).unwrap())
                .collect(),
            Self::Tera(a) => a.inlay_hints(range),
            Self::Toml(a) => a.inlay_hints(range),
        }
    }

    pub fn prepare_rename(&self, position: Position) -> Option<Range> {
        match self {
            Self::Rdf(a) => a.prepare_rename(position),
            _ => None,
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
