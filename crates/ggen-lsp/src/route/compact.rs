//! `CompactTraceView` — the author-time receipt preview (OCEL-TON).
//!
//! A dense, process-bearing card the LSP shows instead of raw JSON: the active
//! episode's events, top DFG edges, the POWL route, and the receipt head. One
//! struct → three projections (Markdown hover, `Diagnostic.data`, MCP/headless
//! JSON) so every surface shows the identical view. Only the *active slice* is
//! shown; the full trace stays in `.ggen/ocel/` `.ggen/receipts/` `.ggen/replay/`.

use serde::{Deserialize, Serialize};

use super::model::Provenance;
use super::plan::RoutePlan;

/// One compact event row: `(index, activity, dt_seconds_from_start, object_ids)`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompactEventRow {
    pub index: u32,
    pub activity: String,
    /// Seconds since the episode's first event.
    pub dt: i64,
    pub objects: Vec<String>,
}

/// Compact POWL view: ordered step titles (the route geometry, flattened to a
/// lawful order for display).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompactPowlView {
    pub steps: Vec<String>,
}

/// The compact, LSP-visible process-state card for one diagnostic episode.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompactTraceView {
    pub case_id: String,
    pub family: String,
    pub diagnostic_code: String,
    pub file: String,
    pub route_id: Option<String>,
    pub provenance: Option<String>,
    pub status: String,
    pub events: Vec<CompactEventRow>,
    pub dfg: Vec<(String, String, u64)>,
    pub powl: Option<CompactPowlView>,
    pub receipt_head: Option<String>,
}

impl CompactTraceView {
    /// Build a minimal view from a selected [`RoutePlan`] (the editor/MCP path,
    /// where the active-episode event slice may be absent). Event/DFG/receipt
    /// slices are filled by the caller when an OCEL log is available.
    #[must_use]
    pub fn from_route_plan(plan: &RoutePlan, file: &str) -> Self {
        let provenance = Some(match &plan.provenance {
            Provenance::Seeded => "seeded".to_string(),
            Provenance::Mined { .. } => "mined".to_string(),
        });
        let powl = Some(CompactPowlView {
            steps: plan.ordered_steps.iter().map(|s| s.title.clone()).collect(),
        });
        // Canonical cross-channel site id (same helper the RouteEnvelope uses), so
        // the hover card and the envelope agree on case_id by construction.
        let span = crate::check::span_str(plan.target.range);
        Self {
            case_id: super::envelope::route_case_id(file, &plan.target.code, &span),
            family: format!("{:?}", plan.family),
            diagnostic_code: plan.target.code.clone(),
            file: file.to_string(),
            route_id: Some(plan.route_id.0.clone()),
            provenance,
            status: plan.target.severity.clone(),
            events: Vec::new(),
            dfg: Vec::new(),
            powl,
            receipt_head: None,
        }
    }

    /// Render the OCEL-TON text card (TOON-like, process-specific). This is what
    /// appears in hover / the `Copy OCEL-TON` command.
    #[must_use]
    pub fn to_toon(&self) -> String {
        let mut s = String::new();
        s.push_str(&format!("case: {}\n", self.case_id));
        s.push_str(&format!("family: {}\n", self.family));
        s.push_str(&format!("diag: {}\n", self.diagnostic_code));
        s.push_str(&format!("file: {}\n", self.file));
        if let Some(r) = &self.route_id {
            let prov = self.provenance.as_deref().unwrap_or("?");
            s.push_str(&format!("route: {r} ({prov})\n"));
        }
        s.push_str(&format!("status: {}\n", self.status));

        if !self.events.is_empty() {
            s.push_str("\nE[e,a,dt,o]\n");
            for e in &self.events {
                s.push_str(&format!(
                    "{},{},{},[{}]\n",
                    e.index,
                    e.activity,
                    e.dt,
                    e.objects.join(",")
                ));
            }
        }
        if !self.dfg.is_empty() {
            s.push_str("\nDFG[from,to,n]\n");
            for (f, t, n) in &self.dfg {
                s.push_str(&format!("{f},{t},{n}\n"));
            }
        }
        if let Some(powl) = &self.powl {
            if !powl.steps.is_empty() {
                s.push_str("\nPOWL\n");
                s.push_str(&powl.steps.join(" < "));
                s.push('\n');
            }
        }
        if let Some(r) = &self.receipt_head {
            s.push_str(&format!("\nR=blake3:{r}\n"));
        }
        s
    }

    /// Render the Markdown hover block (fenced OCEL-TON card).
    #[must_use]
    pub fn to_hover_markdown(&self) -> String {
        format!(
            "**{}** — `{}`\n\n```ocel-ton\n{}```",
            self.family, self.diagnostic_code, self.to_toon()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::route::model::{PartialOrder, Provenance, RepairFamily, RepairRoute, RouteId, StepId, RepairStep, EditTemplate, RouteBindings};
    use crate::route::{plan::DiagnosticRef, edit::route_plan};
    use tower_lsp::lsp_types::{Position, Range};

    fn sample_plan() -> RoutePlan {
        let route = RepairRoute {
            id: RouteId("mined.template-failure".into()),
            family: RepairFamily::TemplateFailure,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "Add ORDER BY".into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "mined".into(),
            provenance: Provenance::Mined {
                confidence: 1.0, support: 5, success_rate: 1.0,
                first_seen: "t0".into(), last_seen: "t1".into(), source_report_hash: "h".into(),
            },
            priority: 1,
        };
        let target = DiagnosticRef {
            code: "E0011".into(), message: "CONSTRUCT lacks ORDER BY".into(),
            range: Range { start: Position{line:0,character:0}, end: Position{line:0,character:1} },
            severity: "warning".into(),
        };
        route_plan(&route, &RouteBindings::default(), "rule.rq", target)
    }

    #[test]
    fn toon_card_carries_route_and_powl() {
        let view = CompactTraceView::from_route_plan(&sample_plan(), "rule.rq");
        let toon = view.to_toon();
        assert!(toon.contains("route: mined.template-failure (mined)"), "{toon}");
        assert!(toon.contains("POWL"));
        assert!(toon.contains("Add ORDER BY"));
        assert!(toon.contains("diag: E0011"));
    }

    #[test]
    fn hover_markdown_is_fenced() {
        let view = CompactTraceView::from_route_plan(&sample_plan(), "rule.rq");
        let md = view.to_hover_markdown();
        assert!(md.contains("```ocel-ton"));
        assert!(md.contains("TemplateFailure"));
    }
}
