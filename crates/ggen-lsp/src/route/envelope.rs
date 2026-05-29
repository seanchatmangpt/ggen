//! `RouteEnvelope` — the ONE canonical route projection shared by every channel.
//!
//! LSP CodeAction `data`, headless `--with_routes`, the MCP tool, and the A2A
//! bridge all emit this byte-identical shape for a given diagnostic. Channel
//! differences are transport only, never logic: one route engine, one envelope,
//! one refusal shape. The envelope COMPOSES the existing [`RoutePlan`] and
//! [`CompactTraceView`] — it does not replace them.

use serde::{Deserialize, Serialize};

use super::compact::CompactTraceView;
use super::plan::{DiagnosticRef, RoutePlan, RoutePlanStep};

/// The canonical cross-channel route projection for one diagnostic site.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteEnvelope {
    /// Cross-channel diagnostic-site identity (see [`route_case_id`]). Distinct
    /// from the per-run OCEL episode id `blake3(file|code|run_id)`, which is the
    /// replay key — this id is stable across channels at projection time.
    pub case_id: String,
    /// Selected route id.
    pub route_id: String,
    /// `"seed"` | `"mined"`.
    pub route_source: String,
    /// Diagnostic code (E00XX, normalized).
    pub diagnostic_code: String,
    /// Law-surface path.
    pub file: String,
    /// Diagnostic span `"sl:sc-el:ec"`.
    pub span: String,
    /// Repair family (Debug name).
    pub family: String,
    /// Human title.
    pub title: String,
    /// Lawfully-ordered steps.
    pub ordered_steps: Vec<RoutePlanStep>,
    /// Receipt head when an OCEL slice is available (None at pure projection time).
    pub receipt_head: Option<String>,
    /// The compact OCEL-TON card (display projection).
    pub compact_trace: CompactTraceView,
    /// The diagnostic this envelope repairs.
    pub target: DiagnosticRef,
}

impl RouteEnvelope {
    /// Project a selected [`RoutePlan`] at `file` into the canonical envelope.
    #[must_use]
    pub fn from_plan(plan: &RoutePlan, file: &str) -> Self {
        let span = crate::check::span_str(plan.target.range);
        let compact = CompactTraceView::from_route_plan(plan, file);
        Self {
            case_id: route_case_id(file, &plan.target.code, &span),
            route_id: plan.route_id.0.clone(),
            route_source: crate::check::route_source(&plan.provenance).to_string(),
            diagnostic_code: plan.target.code.clone(),
            file: file.to_string(),
            span,
            family: format!("{:?}", plan.family),
            title: plan.title.clone(),
            ordered_steps: plan.ordered_steps.clone(),
            receipt_head: compact.receipt_head.clone(),
            compact_trace: compact,
            target: plan.target.clone(),
        }
    }
}

/// The cross-channel diagnostic-site identity: `blake3(file|code|span)`. Distinct
/// from the per-run OCEL episode id (`blake3(file|code|run_id)`) — the episode id
/// is the replay key; this is the projection-time route identity all channels share.
#[must_use]
pub fn route_case_id(file: &str, code: &str, span: &str) -> String {
    let h = blake3::hash(format!("{file}|{code}|{span}").as_bytes());
    format!("c:{}", &h.to_hex()[..8])
}

/// One refusal shape for "no admissible route", shared across channels — so a
/// missing route reads identically whether the agent arrived via LSP, MCP, or A2A.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteRefusal {
    pub diagnostic_code: String,
    pub span: String,
    pub reason: String,
}

impl RouteRefusal {
    /// Build the refusal for a diagnostic that has no admissible route.
    #[must_use]
    pub fn from_target(target: &DiagnosticRef) -> Self {
        Self {
            diagnostic_code: target.code.clone(),
            span: crate::check::span_str(target.range),
            reason: "no admissible route for this diagnostic".to_string(),
        }
    }
}
