//! Repair-route registry: family → routes, with cold-start seeds and O(1) lookup.
//!
//! The registry is built once (seeded built-ins, optionally merged with mined
//! routes from a pack `powl/` dir) and read on the hot path via two hashmap
//! probes — no mining, no I/O, sub-100ms.

use std::collections::HashMap;
use std::path::Path;
use tower_lsp::lsp_types::{Diagnostic, NumberOrString};

use super::model::{
    Anchor, EditTemplate, PartialOrder, Provenance, RepairFamily, RepairRoute, RepairStep, RouteId,
    StepId,
};
use super::promoted::{is_promotable, load_promoted};

/// Family → routes (priority-sorted) + id → route, for fast lookup.
#[derive(Debug, Clone, Default)]
pub struct RouteRegistry {
    by_family: HashMap<RepairFamily, Vec<RepairRoute>>,
    by_id: HashMap<RouteId, RepairRoute>,
}

impl RouteRegistry {
    /// Cold-start registry: built-in seeded routes only. No I/O.
    #[must_use]
    pub fn seeded() -> Self {
        let mut reg = Self::default();
        for route in seed_routes() {
            reg.insert(route);
        }
        reg
    }

    /// Merge mined routes from a promoted-route artifact over the seeds
    /// (best-effort; a missing/invalid/old-version file leaves seeds intact).
    /// Mined routes never *remove* seeds — precedence is decided per-lookup by
    /// the conformance gate in [`Self::select_for_diagnostic`].
    #[must_use]
    pub fn with_pack_routes(mut self, path: &Path) -> Self {
        if let Some(promoted) = load_promoted(path) {
            for route in promoted.routes {
                self.insert(route);
            }
        }
        self
    }

    fn insert(&mut self, route: RepairRoute) {
        self.by_id.insert(route.id.clone(), route.clone());
        let v = self.by_family.entry(route.family).or_default();
        v.push(route);
        // Highest priority first.
        v.sort_by_key(|r| std::cmp::Reverse(r.priority));
    }

    /// Number of registered routes.
    #[must_use]
    pub fn len(&self) -> usize {
        self.by_id.len()
    }

    /// True if no routes are registered.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.by_id.is_empty()
    }

    /// Select the route for a diagnostic under **conformance-gated precedence**
    /// (van der Aalst: trust only what the log proves). A mined route is
    /// preferred over the family's seed only when it clears the support +
    /// measured-success thresholds ([`is_promotable`]); otherwise the seed
    /// holds. Pure + O(routes-in-family) — no I/O, no mining on the hot path.
    #[must_use]
    pub fn select_for_diagnostic(&self, diag: &Diagnostic) -> Option<&RepairRoute> {
        let family = family_of_diagnostic(diag)?;
        let routes = self.by_family.get(&family)?;
        // 1. A promotable (evidence-proven) mined route wins.
        routes
            .iter()
            .find(|r| is_promotable(&r.provenance))
            // 2. else the seed (doctrine is the safe default).
            .or_else(|| {
                routes
                    .iter()
                    .find(|r| matches!(r.provenance, Provenance::Seeded))
            })
            // 3. else whatever exists (e.g. a sub-threshold mined route).
            .or_else(|| routes.first())
    }

    /// Fetch a route by id (used to reconstruct from a carried `RoutePlanRef`).
    #[must_use]
    pub fn get(&self, id: &RouteId) -> Option<&RepairRoute> {
        self.by_id.get(id)
    }
}

/// Map a diagnostic to its repair family — the single reconciliation point
/// between E-codes/sources and families.
#[must_use]
pub fn family_of_diagnostic(diag: &Diagnostic) -> Option<RepairFamily> {
    if let Some(NumberOrString::String(code)) = &diag.code {
        return family_of_code(code);
    }
    // RDF located-parse diagnostics carry no code; disambiguate by message.
    if diag.message.starts_with("RDF syntax error") {
        return Some(RepairFamily::ParseFailure);
    }
    None
}

/// Map a diagnostic code string to its repair family. Used by the offline miner
/// (which has only the captured code string, not a full `Diagnostic`). The
/// capture sentinel `"RDF"` (a code-less located-parse diagnostic) maps to
/// `ParseFailure`.
#[must_use]
pub fn family_of_code(code: &str) -> Option<RepairFamily> {
    match code {
        "E0010" | "E0011" | "E0024" => Some(RepairFamily::TemplateFailure),
        "E0023" => Some(RepairFamily::ConfigValue),
        "E0001" => Some(RepairFamily::ParseFailure),
        "RDF" => Some(RepairFamily::ParseFailure),
        _ => None,
    }
}

/// Built-in cold-start routes. Kept minimal and high-confidence; mined routes
/// extend these per family.
fn seed_routes() -> Vec<RepairRoute> {
    vec![
        // Invalid enum value → ADVISORY only. We cannot safely choose the
        // replacement (the diagnostic lists the allowed set but not the intent),
        // so no destructive auto-edit is offered — just guidance.
        RepairRoute {
            id: RouteId("config.fix-enum-value".into()),
            family: RepairFamily::ConfigValue,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "Replace with one of the admitted values listed in the diagnostic"
                        .into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Use an admitted enum value".into(),
            provenance: Provenance::Seeded,
            priority: 5,
        },
        // ParseFailure (UnknownPrefix): declare the missing prefix.
        RepairRoute {
            id: RouteId("parse.declare-prefix".into()),
            family: RepairFamily::ParseFailure,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("declare".into()),
                    title: "Declare the missing @prefix".into(),
                    edit: EditTemplate::InsertLine {
                        anchor: Anchor::AfterLastPrefix,
                        text: "@prefix {prefix}: <{iri}> .".into(),
                    },
                }],
                edges: vec![],
            },
            description: "Declare the undeclared prefix".into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // TemplateFailure (E0010): advisory — VALUES must move inline to ggen.toml.
        RepairRoute {
            id: RouteId("template.values-inline".into()),
            family: RepairFamily::TemplateFailure,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "Move VALUES data inline into ggen.toml".into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Move VALUES out of the external .rq into ggen.toml".into(),
            provenance: Provenance::Seeded,
            priority: 5,
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::{DiagnosticSeverity, Position, Range};

    fn diag(code: &str, msg: &str) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 1 },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: if code.is_empty() {
                None
            } else {
                Some(NumberOrString::String(code.to_string()))
            },
            code_description: None,
            source: Some("ggen-lsp".to_string()),
            message: msg.to_string(),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    #[test]
    fn seeded_registry_is_nonempty_and_sound() {
        let reg = RouteRegistry::seeded();
        assert!(!reg.is_empty());
        for route in seed_routes() {
            assert!(route.steps.is_sound(), "seed route {:?} must be sound", route.id);
        }
    }

    #[test]
    fn enum_violation_is_advisory_not_destructive() {
        // The critical regression guard: an invalid enum value must NOT get a
        // delete/replace edit — only advisory guidance.
        let reg = RouteRegistry::seeded();
        let d = diag("E0023", "invalid value \"verbose\" for `level` — expected one of: ...");
        let route = reg.select_for_diagnostic(&d).expect("advisory route");
        assert_eq!(route.family, RepairFamily::ConfigValue);
        assert!(
            matches!(route.steps.nodes[0].edit, EditTemplate::NoOp),
            "enum-violation repair must be advisory (NoOp), never a destructive edit"
        );
    }

    #[test]
    fn rdf_syntax_message_maps_to_parse_failure() {
        let d = diag("", "RDF syntax error: unexpected token");
        assert_eq!(family_of_diagnostic(&d), Some(RepairFamily::ParseFailure));
    }

    #[test]
    fn unknown_code_has_no_route() {
        let reg = RouteRegistry::seeded();
        assert!(reg.select_for_diagnostic(&diag("E9999", "x")).is_none());
    }

    // ---- conformance-gated precedence (the vdA gate) ----

    fn mined_route(id: &str, support: u32, success: f32) -> RepairRoute {
        RepairRoute {
            id: RouteId(id.into()),
            family: RepairFamily::TemplateFailure, // same family as seed `template.values-inline`
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "mined advisory".into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "mined route".into(),
            provenance: Provenance::Mined {
                confidence: 0.9,
                support,
                success_rate: success,
                first_seen: "2026-05-28T00:00:00+00:00".into(),
                last_seen: "2026-05-28T01:00:00+00:00".into(),
                source_report_hash: "h".into(),
            },
            priority: 1, // LOWER than the seed's priority — proves precedence is
                         // by conformance, not by priority number.
        }
    }

    fn registry_with(mined: RepairRoute) -> RouteRegistry {
        use crate::route::promoted::{default_pack_routes_path, write_promoted, PromotedRoutes};
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = default_pack_routes_path(dir.path());
        write_promoted(
            &path,
            &PromotedRoutes {
                version: PromotedRoutes::VERSION,
                source_log_hash: "x".into(),
                routes: vec![mined],
            },
        )
        .expect("write");
        let reg = RouteRegistry::seeded().with_pack_routes(&path);
        // keep dir alive until after load
        drop(dir);
        reg
    }

    #[test]
    fn proven_mined_route_beats_seed() {
        // support>=3 AND success>=0.6 → evidence supersedes doctrine, even though
        // the mined route has LOWER priority than the seed.
        let reg = registry_with(mined_route("mined.proven", 5, 0.8));
        let route = reg
            .select_for_diagnostic(&diag("E0010", "VALUES…"))
            .expect("route");
        assert_eq!(route.id.0, "mined.proven", "proven mined route must win");
    }

    #[test]
    fn unproven_mined_route_loses_to_seed() {
        // Below threshold (low support) → seed (doctrine) holds.
        let reg = registry_with(mined_route("mined.weak", 1, 0.9));
        let route = reg
            .select_for_diagnostic(&diag("E0010", "VALUES…"))
            .expect("route");
        assert_eq!(
            route.id.0, "template.values-inline",
            "sub-threshold mined route must NOT override the seed"
        );
    }
}
