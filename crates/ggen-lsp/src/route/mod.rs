//! POWL-native repair-route engine: separable routes, a fast registry, the
//! `Diagnostic.data` carry, and edit/projection rendering.
//!
//! A `Diagnostic` is the observable trace of a failed process transition; a
//! `CodeAction` is the transition that repairs it. The registry is the process
//! model; the LSP surface projects it. Routes are precomputed (seeded or mined),
//! so hot-path selection is two hashmap probes — never mining at edit time.

pub mod compact;
pub mod diagnostic_species;
pub mod edit;
pub mod envelope;
pub mod model;
pub mod plan;
pub mod promoted;
pub mod registry;

pub use compact::{CompactEventRow, CompactPowlView, CompactTraceView};
pub use diagnostic_species::{species_for, species_registry, DiagnosticSpecies};
pub use edit::{render_edit, route_plan, workspace_edit_from_route};
pub use envelope::{route_case_id, RouteEnvelope, RouteRefusal};
pub use model::{
    Anchor, EditTemplate, PartialOrder, Provenance, RepairFamily, RepairRoute, RepairStep,
    RouteBindings, RouteId, StepId,
};
pub use plan::{DiagnosticRef, RoutePlan, RoutePlanRef, RoutePlanStep};
pub use promoted::{
    default_pack_routes_path, is_promotable, load_promoted, promotion, write_promoted,
    PromotedRoutes,
};
pub use registry::{family_of_code, family_of_diagnostic, RouteRegistry};
// `action_route_for` and `route_plan_for_diagnostic` are defined in this module.

use tower_lsp::lsp_types::Diagnostic;

/// The single route-selection entry point used by the editor `code_action` path.
/// Headless and MCP go through [`route_plan_for_diagnostic`], which calls the same
/// `select_for_diagnostic` underneath — so all three channels agree on route id
/// by construction. This named fn makes the editor path testable for parity.
#[must_use]
pub fn action_route_for<'a>(
    registry: &'a RouteRegistry, diag: &Diagnostic,
) -> Option<&'a RepairRoute> {
    registry.select_for_diagnostic(diag)
}

/// Produce the `RoutePlan` for a diagnostic, if a route exists. The single
/// entry point shared by the editor CodeAction path, the MCP tool, and the
/// headless `--with-routes` gate — so every channel yields an identical plan.
///
/// Bindings are derived from the diagnostic alone (its range is the edit site),
/// which is sufficient for the seeded delete/advisory routes; analyzer-enriched
/// bindings (prefix/iri/symbol) are a future refinement.
#[must_use]
pub fn route_plan_for_diagnostic(
    registry: &RouteRegistry, diag: &Diagnostic, content: &str,
) -> Option<RoutePlan> {
    let route = registry.select_for_diagnostic(diag)?;
    let bindings = RouteBindings {
        site: Some(diag.range),
        ..Default::default()
    };
    Some(route_plan(
        route,
        &bindings,
        content,
        DiagnosticRef::from_diagnostic(diag),
    ))
}

/// Project a diagnostic into the canonical [`RouteEnvelope`] — the single entry
/// point every channel (LSP CodeAction `data`, headless, MCP, A2A) uses, so all
/// emit a byte-equivalent envelope for the same diagnostic. `file` is the
/// law-surface path (the envelope's stable site identity needs it).
#[must_use]
pub fn envelope_for_diagnostic(
    registry: &RouteRegistry, diag: &Diagnostic, content: &str, file: &str,
) -> Option<RouteEnvelope> {
    route_plan_for_diagnostic(registry, diag, content)
        .map(|plan| RouteEnvelope::from_plan(&plan, file))
}
