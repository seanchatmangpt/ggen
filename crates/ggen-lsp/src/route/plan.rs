//! Route carry + projection types.
//!
//! `RoutePlanRef` is the minimal handle serialized into `Diagnostic.data` at
//! diagnostic-build time: the route id/family + the run-specific bindings, so
//! `code_action` reconstructs the transition without re-analysis. `RoutePlan` is
//! the editor-agnostic, topologically-ordered projection returned to non-editor
//! agents (MCP / headless), shared byte-identically across all channels.

use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Range, TextEdit};

use super::model::{RepairFamily, Provenance, RouteBindings, RouteId};

/// Serialized into `Diagnostic.data`. The handle from a failed transition back
/// into the process model (a specific route) plus the bindings to instantiate it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutePlanRef {
    /// Schema version; older diagnostics fail closed rather than mis-deserialize.
    pub v: u16,
    /// Which route in the registry repairs this transition.
    pub route_id: RouteId,
    /// Family (redundant with the route, lets clients group without the table).
    pub family: RepairFamily,
    /// Bindings extracted from the diagnostic site.
    pub bindings: RouteBindings,
}

impl RoutePlanRef {
    /// Current carry schema version.
    pub const VERSION: u16 = 1;

    /// Build a ref for a route id + family + bindings at the current version.
    #[must_use]
    pub fn new(route_id: RouteId, family: RepairFamily, bindings: RouteBindings) -> Self {
        Self {
            v: Self::VERSION,
            route_id,
            family,
            bindings,
        }
    }
}

/// The diagnostic a plan repairs, normalized for cross-channel transport.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticRef {
    /// Diagnostic code as a string (LSP code is NumberOrString; normalized here).
    pub code: String,
    /// Human message.
    pub message: String,
    /// 0-based range.
    pub range: Range,
    /// "error" | "warning" | "information" | "hint".
    pub severity: String,
}

impl DiagnosticRef {
    /// Normalize an LSP `Diagnostic` for cross-channel transport.
    #[must_use]
    pub fn from_diagnostic(d: &Diagnostic) -> Self {
        let code = match &d.code {
            Some(NumberOrString::String(s)) => s.clone(),
            Some(NumberOrString::Number(n)) => n.to_string(),
            None => String::new(),
        };
        let severity = match d.severity {
            Some(DiagnosticSeverity::ERROR) => "error",
            Some(DiagnosticSeverity::WARNING) => "warning",
            Some(DiagnosticSeverity::INFORMATION) => "information",
            Some(DiagnosticSeverity::HINT) => "hint",
            _ => "error",
        };
        Self {
            code,
            message: d.message.clone(),
            range: d.range,
            severity: severity.to_string(),
        }
    }
}

/// A single ordered step in a [`RoutePlan`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutePlanStep {
    /// Human-readable title.
    pub title: String,
    /// Concrete edit this step would make, if computable.
    pub edit: Option<TextEdit>,
}

/// Editor-agnostic projection of a route applied to a concrete diagnostic site.
/// Identical serde shape across CodeAction `data`, the MCP tool, and `--with-routes`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutePlan {
    /// Route identifier.
    pub route_id: RouteId,
    /// Family repaired.
    pub family: RepairFamily,
    /// Human title.
    pub title: String,
    /// Provenance (seeded vs mined).
    pub provenance: Provenance,
    /// Steps in a lawful (topologically sorted) order.
    pub ordered_steps: Vec<RoutePlanStep>,
    /// The diagnostic this plan repairs.
    pub target: DiagnosticRef,
}
