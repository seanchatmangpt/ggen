//! OpenUSD ASCII (`.usda`) law-surface analyzer.
//!
//! Parses `.usda` files line-by-line with a prim-depth stack and emits:
//!
//! - `GGEN-USD-001` (ERROR) — Foreign geometry detected: a prim belonging to one part
//!   contains a nested prim whose name suggests it belongs to a different part.
//! - `GGEN-USD-002` (ERROR) — Missing `owner_part_id` attribute on a `def Mesh` or
//!   `def Xform` prim.
//! - `GGEN-USD-003` (WARNING) — Socket prim containing a `def Mesh` block.

use lsp_max::lsp_types::{
    CallHierarchyItem, CodeLens, CompletionResponse, DiagnosticSeverity, DocumentSymbol,
    FoldingRange, Hover, InlayHint, Location, Position, Range, SemanticTokens, TextEdit,
    TypeHierarchyItem, WorkspaceEdit,
};
use lsp_max_protocol::{LawAxis, MaxDiagnostic};

use crate::analyzers::diag;

/// GGEN-USD-001: Foreign geometry detected in a part prim.
pub const GGEN_USD_001: &str = "GGEN-USD-001";

/// GGEN-USD-002: Missing `owner_part_id` attribute on a `def Mesh` / `def Xform`.
pub const GGEN_USD_002: &str = "GGEN-USD-002";

/// GGEN-USD-003: Socket prim with a mesh payload.
pub const GGEN_USD_003: &str = "GGEN-USD-003";

/// The canonical set of part names checked for foreign-geometry detection.
const PART_NAMES: &[&str] = &[
    "Head", "Torso", "Arm", "Leg", "Wing", "Blade", "Backpack", "Loadout",
];

/// Returns the first part name found within `prim_name`, or `None`.
fn part_in(prim_name: &str) -> Option<&'static str> {
    PART_NAMES.iter().find(|&&p| prim_name.contains(p)).copied()
}

/// A lightweight stack frame that records one `def` prim opening.
#[derive(Debug, Clone)]
struct PrimFrame {
    /// The raw prim name (e.g. `SM_Torso`).
    name: String,
    /// The part name detected in the prim name, if any.
    part: Option<&'static str>,
    /// Whether this frame is a `def Mesh` prim.
    is_mesh: bool,
    /// Whether this frame is a `def Xform` prim.
    is_xform: bool,
    /// Whether this frame is a socket prim (name contains "Socket" or "socket").
    is_socket: bool,
    /// Whether an `owner_part_id` attribute was found inside this prim's body.
    has_owner_part_id: bool,
    /// 0-based line where the `def` keyword was found (for diagnostic anchoring).
    line: u32,
}

#[derive(Clone)]
pub struct UsdAnalyzer {
    source: String,
}

impl UsdAnalyzer {
    /// Construct a `UsdAnalyzer` over the given `.usda` source text.
    #[must_use]
    pub fn new(content: &str) -> Self {
        Self {
            source: content.to_string(),
        }
    }

    /// Parse the source and return all `GGEN-USD-*` diagnostics.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<MaxDiagnostic> {
        let mut diags: Vec<MaxDiagnostic> = Vec::new();
        // Stack of open prim frames; top is the innermost open prim.
        let mut stack: Vec<PrimFrame> = Vec::new();

        for (idx, raw_line) in self.source.lines().enumerate() {
            let line_no = u32::try_from(idx).unwrap_or(0);
            let trimmed = raw_line.trim();

            // ── Closing brace ────────────────────────────────────────────────
            if trimmed == "}" {
                if let Some(frame) = stack.pop() {
                    // Check for missing owner_part_id on Mesh / Xform prims.
                    if (frame.is_mesh || frame.is_xform) && !frame.has_owner_part_id {
                        diags.push(diag::max_whole_line(
                            frame.line,
                            DiagnosticSeverity::ERROR,
                            Some(GGEN_USD_002),
                            format!(
                                "GGEN-USD-002: prim \"{}\" is missing a \
                                 `custom string owner_part_id = \"...\"` attribute.",
                                frame.name
                            ),
                            LawAxis::Domain,
                        ));
                    }
                }
                continue;
            }

            // ── `def` prim opening ───────────────────────────────────────────
            // Matches: `def Mesh "Name" {`, `def Xform "Name" {`, `def "Name" {`
            if trimmed.starts_with("def ") || trimmed == "def" {
                let prim_name = extract_prim_name(trimmed);
                let is_mesh = trimmed.starts_with("def Mesh ");
                let is_xform = trimmed.starts_with("def Xform ");
                let is_socket = prim_name.contains("Socket") || prim_name.contains("socket");
                let part = part_in(&prim_name);

                // Foreign geometry check: if there is a parent prim with a part name,
                // and THIS prim has a DIFFERENT part name, emit GGEN-USD-001.
                if let Some(parent_part) = stack.iter().rev().find_map(|f| f.part) {
                    if let Some(child_part) = part {
                        if child_part != parent_part {
                            diags.push(diag::max_whole_line(
                                line_no,
                                DiagnosticSeverity::ERROR,
                                Some(GGEN_USD_001),
                                format!(
                                    "GGEN-USD-001: foreign geometry — prim \"{prim_name}\" \
                                     (part: {child_part}) is nested inside a prim belonging \
                                     to part \"{parent_part}\".",
                                ),
                                LawAxis::Domain,
                            ));
                        }
                    }
                }

                // Socket-with-mesh check: if there is an open socket ancestor and this is a Mesh.
                if is_mesh {
                    if let Some(socket_frame) = stack.iter().rev().find(|f| f.is_socket) {
                        diags.push(diag::max_whole_line(
                            socket_frame.line,
                            DiagnosticSeverity::WARNING,
                            Some(GGEN_USD_003),
                            format!(
                                "GGEN-USD-003: socket prim \"{}\" contains a \
                                 `def Mesh` payload — sockets must not carry geometry.",
                                socket_frame.name
                            ),
                            LawAxis::Domain,
                        ));
                    }
                }

                // Push the new frame onto the stack; body scanning continues.
                stack.push(PrimFrame {
                    name: prim_name,
                    part,
                    is_mesh,
                    is_xform,
                    is_socket,
                    has_owner_part_id: false,
                    line: line_no,
                });
                continue;
            }

            // ── Attribute scan inside prim body ──────────────────────────────
            if trimmed.contains("owner_part_id") {
                if let Some(frame) = stack.last_mut() {
                    frame.has_owner_part_id = true;
                }
            }
        }

        // Any frames still open at EOF (malformed file) — check owner_part_id.
        while let Some(frame) = stack.pop() {
            if (frame.is_mesh || frame.is_xform) && !frame.has_owner_part_id {
                diags.push(diag::max_whole_line(
                    frame.line,
                    DiagnosticSeverity::ERROR,
                    Some(GGEN_USD_002),
                    format!(
                        "GGEN-USD-002: prim \"{}\" is missing a \
                         `custom string owner_part_id = \"...\"` attribute.",
                        frame.name
                    ),
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

/// Extract the prim name from a `def [Type] "Name" {` line.
///
/// Looks for the first quoted string. Returns an empty string if none found.
fn extract_prim_name(trimmed: &str) -> String {
    let mut in_quote = false;
    let mut name = String::new();
    for ch in trimmed.chars() {
        if ch == '"' {
            if in_quote {
                break; // closing quote
            }
            in_quote = true;
        } else if in_quote {
            name.push(ch);
        }
    }
    name
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_foreign_geometry_detection() {
        let content = r#"#usda 1.0
def Xform "SM_Torso" {
    def Mesh "SM_Head_Geo" {}
}
"#;
        let analyzer = UsdAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            diags.iter().any(|d| d.law_id == GGEN_USD_001),
            "expected GGEN-USD-001 but got: {:?}",
            diags.iter().map(|d| &d.law_id).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_missing_owner_part_id() {
        let content = r#"#usda 1.0
def Mesh "Torso_Main" {
    float3[] extent = [(-1, -1, -1), (1, 1, 1)]
}
"#;
        let analyzer = UsdAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            diags.iter().any(|d| d.law_id == GGEN_USD_002),
            "expected GGEN-USD-002 but got: {:?}",
            diags.iter().map(|d| &d.law_id).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_owner_part_id_present_no_usd002() {
        let content = r#"#usda 1.0
def Mesh "Torso_Main" {
    custom string owner_part_id = "torso"
}
"#;
        let analyzer = UsdAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            !diags.iter().any(|d| d.law_id == GGEN_USD_002),
            "should NOT emit GGEN-USD-002 when owner_part_id is present"
        );
    }

    #[test]
    fn test_socket_with_mesh_payload() {
        let content = r#"#usda 1.0
def Xform "AttachSocket_Shoulder" {
    def Mesh "Geo_Payload" {
        custom string owner_part_id = "arm"
    }
}
"#;
        let analyzer = UsdAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            diags.iter().any(|d| d.law_id == GGEN_USD_003),
            "expected GGEN-USD-003 but got: {:?}",
            diags.iter().map(|d| &d.law_id).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_same_part_no_foreign_geometry() {
        // A Torso prim containing another Torso sub-prim is not foreign.
        let content = r#"#usda 1.0
def Xform "SM_Torso" {
    def Mesh "SM_Torso_Geo" {
        custom string owner_part_id = "torso"
    }
}
"#;
        let analyzer = UsdAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            !diags.iter().any(|d| d.law_id == GGEN_USD_001),
            "should NOT emit GGEN-USD-001 for same-part nesting"
        );
    }

    #[test]
    fn test_clean_file_no_diagnostics() {
        let content = r#"#usda 1.0
def Xform "SM_Torso" {
    custom string owner_part_id = "torso"
    def Mesh "SM_Torso_Main" {
        custom string owner_part_id = "torso"
    }
}
"#;
        let analyzer = UsdAnalyzer::new(content);
        let diags = analyzer.diagnostics();
        assert!(
            diags.is_empty(),
            "clean file should have no diagnostics, got: {:?}",
            diags.iter().map(|d| &d.law_id).collect::<Vec<_>>()
        );
    }
}
