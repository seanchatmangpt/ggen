//! MaterialX (`.mtlx`) law-surface analyzer.
//!
//! Parses `.mtlx` files (MaterialX XML format) line-by-line and emits:
//!
//! - `GGEN-MTLX-001` (WARNING) — Unbound material input: an `<input>` element
//!   that has no `nodegraph`, `output`, or `value` attribute — no upstream connection
//!   and no default value.

use lsp_max::lsp_types::{
    CallHierarchyItem, CodeLens, CompletionResponse, DiagnosticSeverity, DocumentSymbol,
    FoldingRange, Hover, InlayHint, Location, Position, Range, SemanticTokens, TextEdit,
    TypeHierarchyItem, WorkspaceEdit,
};
use lsp_max_protocol::{LawAxis, MaxDiagnostic};

use crate::analyzers::diag;

/// GGEN-MTLX-001: Unbound material input — no connection and no default value.
pub const GGEN_MTLX_001: &str = "GGEN-MTLX-001";

#[derive(Clone)]
pub struct MtlxAnalyzer {
    source: String,
}

impl MtlxAnalyzer {
    /// Construct a `MtlxAnalyzer` over the given `.mtlx` source text.
    #[must_use]
    pub fn new(content: &str) -> Self {
        Self {
            source: content.to_string(),
        }
    }

    /// Parse the source and return all `GGEN-MTLX-*` diagnostics.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<MaxDiagnostic> {
        let mut diags: Vec<MaxDiagnostic> = Vec::new();

        for (idx, raw_line) in self.source.lines().enumerate() {
            let line_no = u32::try_from(idx).unwrap_or(0);
            let trimmed = raw_line.trim();

            // Only inspect lines that open an `<input` element.
            if !trimmed.starts_with("<input ") && !trimmed.starts_with("<input\t") {
                continue;
            }

            // An input is bound if it has nodegraph=, output=, or value= attributes.
            let has_binding = trimmed.contains("nodegraph=")
                || trimmed.contains("output=")
                || trimmed.contains("value=");

            if !has_binding {
                // Extract the input name for a clearer message.
                let input_name = extract_xml_attr(trimmed, "name").unwrap_or_default();
                let msg = if input_name.is_empty() {
                    "GGEN-MTLX-001: `<input>` element has no upstream connection \
                     (`nodegraph`, `output`) and no default `value`."
                        .to_string()
                } else {
                    format!(
                        "GGEN-MTLX-001: input \"{input_name}\" has no upstream connection \
                         (`nodegraph`, `output`) and no default `value`."
                    )
                };
                diags.push(diag::max_whole_line(
                    line_no,
                    DiagnosticSeverity::WARNING,
                    Some(GGEN_MTLX_001),
                    msg,
                    LawAxis::Domain,
                ));
            }
        }

        diags
    }

    pub fn completion_at(&self, _line: u32, _character: u32) -> Option<CompletionResponse> {
        None
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

    pub fn semantic_tokens(&self) -> Option<SemanticTokens> {
        None
    }

    pub fn document_symbols(&self, _range: Option<Range>) -> Vec<DocumentSymbol> {
        Vec::new()
    }

    pub fn code_lenses(&self) -> Option<Vec<CodeLens>> {
        None
    }

    pub fn folding_ranges(&self) -> Option<Vec<FoldingRange>> {
        None
    }

    pub fn format_document(&self) -> Option<Vec<TextEdit>> {
        None
    }

    pub fn inlay_hints(&self, _range: Option<Range>) -> Vec<InlayHint> {
        Vec::new()
    }

    pub fn prepare_rename(&self, _position: Position) -> Option<Range> {
        None
    }

    pub fn rename_symbol(&self, _position: Position, _new_name: &str) -> Option<WorkspaceEdit> {
        None
    }

    pub fn call_hierarchy_items(&self, _position: Position) -> Option<Vec<CallHierarchyItem>> {
        None
    }

    pub fn type_hierarchy_items(&self, _position: Position) -> Option<Vec<TypeHierarchyItem>> {
        None
    }
}

/// Extract the value of a named XML attribute from a single-line element string.
///
/// Looks for `name="value"` or `name='value'` patterns. Returns `None` if not found.
fn extract_xml_attr<'a>(line: &'a str, attr: &str) -> Option<&'a str> {
    let search = format!("{attr}=\"");
    if let Some(start) = line.find(&search) {
        let rest = &line[start + search.len()..];
        let end = rest.find('"')?;
        return Some(&rest[..end]);
    }
    let search_single = format!("{attr}='");
    if let Some(start) = line.find(&search_single) {
        let rest = &line[start + search_single.len()..];
        let end = rest.find('\'')?;
        return Some(&rest[..end]);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unbound_input_detection() {
        let content = r#"<?xml version="1.0"?>
<materialx version="1.38">
  <nodegraph name="NG_surface">
    <input name="base_color" type="color3" />
  </nodegraph>
</materialx>"#;
        let analyzer = MtlxAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            diags.iter().any(|d| d.law_id == GGEN_MTLX_001),
            "expected GGEN-MTLX-001 but got: {:?}",
            diags.iter().map(|d| &d.law_id).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_bound_input_with_value_no_diagnostic() {
        let content = r#"<?xml version="1.0"?>
<materialx version="1.38">
  <nodegraph name="NG_surface">
    <input name="base_color" type="color3" value="0.8, 0.8, 0.8" />
  </nodegraph>
</materialx>"#;
        let analyzer = MtlxAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            !diags.iter().any(|d| d.law_id == GGEN_MTLX_001),
            "should NOT emit GGEN-MTLX-001 when value= is present"
        );
    }

    #[test]
    fn test_bound_input_with_nodegraph_no_diagnostic() {
        let content = r#"<?xml version="1.0"?>
<materialx version="1.38">
  <material name="M_surface">
    <input name="surfaceshader" type="surfaceshader" nodegraph="NG_surface" output="out" />
  </material>
</materialx>"#;
        let analyzer = MtlxAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            !diags.iter().any(|d| d.law_id == GGEN_MTLX_001),
            "should NOT emit GGEN-MTLX-001 when nodegraph= is present"
        );
    }

    #[test]
    fn test_multiple_unbound_inputs() {
        let content = r#"<?xml version="1.0"?>
<materialx version="1.38">
  <nodegraph name="NG_surface">
    <input name="base_color" type="color3" />
    <input name="roughness" type="float" />
  </nodegraph>
</materialx>"#;
        let analyzer = MtlxAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert_eq!(
            diags.iter().filter(|d| d.law_id == GGEN_MTLX_001).count(),
            2,
            "expected exactly 2 GGEN-MTLX-001 diagnostics"
        );
    }

    #[test]
    fn test_empty_file_no_diagnostics() {
        let content = r#"<?xml version="1.0"?>
<materialx version="1.38">
</materialx>"#;
        let analyzer = MtlxAnalyzer::new(content);
        assert!(analyzer.diagnostics().is_empty());
    }
}
