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

use lsp_max::lsp_types::Diagnostic;

/// The single route-selection entry point used by the editor `code_action` path.
///
/// Headless and MCP go through [`route_plan_for_diagnostic`], which calls the same
/// `select_for_diagnostic` underneath — so all three channels agree on route id
/// by construction. This named fn makes the editor path testable for parity.
#[must_use]
pub fn action_route_for<'a>(
    registry: &'a RouteRegistry, diag: &Diagnostic,
) -> Option<&'a RepairRoute> {
    registry.select_for_diagnostic(diag)
}

/// Produce the `RoutePlan` for a diagnostic, if a route exists.
///
/// The single
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

/// Project a diagnostic into the canonical [`RouteEnvelope`].
///
/// This is the single entry
/// point every channel (LSP CodeAction `data`, headless, MCP, A2A) uses, so all
/// emit a byte-equivalent envelope for the same diagnostic.
///
/// `file` is the
/// law-surface path (the envelope's stable site identity needs it).
#[must_use]
pub fn envelope_for_diagnostic(
    registry: &RouteRegistry, diag: &Diagnostic, content: &str, file: &str,
) -> Option<RouteEnvelope> {
    route_plan_for_diagnostic(registry, diag, content)
        .map(|plan| RouteEnvelope::from_plan(&plan, file))
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_max::lsp_types::{DiagnosticSeverity, NumberOrString, Position, Range};

    /// Red-team finding F2 (decorative-completion, `route/edit.rs:15`): the only
    /// non-advisory seeded route (`parse.declare-prefix`) renders
    /// `EditTemplate::InsertLine`'s `{prefix}`/`{iri}` placeholders via
    /// `render_tmpl`, but `route_plan_for_diagnostic` (this fn) -- the entry
    /// point backing the MCP tool `ggen.lsp.repair_route`, `check.rs`'s
    /// `--with-routes` gate, and `hover.rs` -- only ever sets `RouteBindings.site`,
    /// never `prefix`/`iri`. Unlike the editor's `handle_code_action`, which
    /// filters unfillable routes via a `has_real_edit` check before surfacing a
    /// `CodeAction`, this headless/MCP/hover entry point had no equivalent
    /// filter: an agent calling `ggen.lsp.repair_route` on a `ParseFailure`
    /// diagnostic got back a well-typed `TextEdit` whose `new_text` was the
    /// literal, un-substituted template string `"@prefix {prefix}: <{iri}> .\n"`
    /// -- not valid Turtle, and not a real repair.
    #[test]
    fn parse_failure_route_never_carries_an_unfilled_placeholder_edit() {
        let registry = RouteRegistry::seeded();
        // A code-less, located RDF parse diagnostic -- exactly what the real
        // Turtle/SPARQL analyzer emits for an unknown-prefix parse failure
        // (see `family_of_diagnostic`'s "RDF syntax error" message match).
        let diag = Diagnostic {
            range: Range {
                start: Position {
                    line: 3,
                    character: 0,
                },
                end: Position {
                    line: 3,
                    character: 5,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("E0001".into())),
            code_description: None,
            source: None,
            message: "RDF syntax error: unknown prefix `sh`".into(),
            related_information: None,
            tags: None,
            data: None,
        };
        let content = "ex:a ex:b ex:c .\n";

        // Production call site, byte-for-byte what `mcp::build_repair_routes_in`
        // (crates/ggen-lsp/src/mcp/mod.rs:318), `check.rs`'s `--with-routes` gate,
        // and `hover.rs` all invoke.
        let plan = route_plan_for_diagnostic(&registry, &diag, content)
            .expect("ParseFailure diagnostic must select the seeded parse.declare-prefix route");
        assert_eq!(plan.route_id.0, "parse.declare-prefix");

        for step in &plan.ordered_steps {
            if let Some(edit) = &step.edit {
                assert!(
                    !edit.new_text.contains("{prefix}") && !edit.new_text.contains("{iri}"),
                    "route_plan_for_diagnostic must never surface a TextEdit with an \
                     unfilled `{{prefix}}`/`{{iri}}` placeholder (F2); got new_text = {:?}",
                    edit.new_text
                );
            }
        }

        // Same contract through the envelope projection the MCP tool actually
        // serializes back to the calling agent.
        let envelope = envelope_for_diagnostic(&registry, &diag, content, "spec.ttl")
            .expect("envelope must be produced for a routed ParseFailure diagnostic");
        for step in &envelope.ordered_steps {
            if let Some(edit) = &step.edit {
                assert!(
                    !edit.new_text.contains("{prefix}") && !edit.new_text.contains("{iri}"),
                    "envelope_for_diagnostic must never surface a TextEdit with an \
                     unfilled `{{prefix}}`/`{{iri}}` placeholder (F2); got new_text = {:?}",
                    edit.new_text
                );
            }
        }
    }
}
