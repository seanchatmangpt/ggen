//! Repair-route model: separable POWL partial orders with a cheap soundness check.
//!
//! A `RepairRoute` is a POWL model — a partial order of repair steps. Independent
//! fixes carry no ordering edge (auto-concurrency); real dependencies are edges.
//! The soundness check is the paper's structural test (separable WF-net): a route
//! is sound iff its step graph is acyclic with a single entry and reachable exit.

use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::Range;

/// Stable identifier for a route within its family (e.g. "unknown-prefix.declare").
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RouteId(pub String);

/// Stable identifier for a step within a route.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StepId(pub String);

/// The diagnostic family a route repairs — the join key between diagnostics and
/// routes. A family aggregates the E-codes that denote the same failed transition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RepairFamily {
    /// UnknownPrefix → RDF parse failure.
    ParseFailure,
    /// PrivatePredicate → public-vocab policy violation.
    PublicVocabViolation,
    /// MissingTargetClass → SHACL failure.
    ShaclFailure,
    /// UnboundSPARQLVar / CircularInclude / template syntax → template failure.
    TemplateFailure,
    /// BrokenRename → dangling reference.
    DanglingReference,
    /// MissingReceipt → admission failure.
    AdmissionFailure,
    /// InvalidNQuadGraph → load failure.
    LoadFailure,
    /// An invalid config enum value (e.g. `level = "verbose"`). Advisory only —
    /// the correct value is not knowable from the diagnostic alone, so we never
    /// offer a destructive auto-edit.
    ConfigValue,
}

/// Where a route came from. Seeded routes are always available (cold start);
/// mined routes carry the miner's evidence.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Provenance {
    /// Built-in, hand-authored cold-start route.
    Seeded,
    /// Promoted from a mined dominant edge. Carries the evidence the
    /// conformance-gated precedence decision is based on (van der Aalst: trust
    /// only what the log proves).
    Mined {
        /// 0.0–1.0 confidence the route resolves the diagnostic.
        confidence: f32,
        /// Number of episodes supporting this route (the noise-filter input).
        support: u32,
        /// Fraction of supporting episodes that lawfully closed
        /// (`DiagnosticRaised ≺ RepairApplied ≺ GatePassed`), measured via SPARQL.
        success_rate: f32,
        /// RFC-3339 timestamp of the earliest supporting episode.
        first_seen: String,
        /// RFC-3339 timestamp of the latest supporting episode.
        last_seen: String,
        /// BLAKE3 of the mining report this route was promoted from.
        source_report_hash: String,
    },
}

/// How a step's text edit is produced from the diagnostic bindings + document.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EditTemplate {
    /// Insert a whole line at a computed anchor.
    InsertLine {
        /// Where to insert.
        anchor: Anchor,
        /// Template text with `{prefix}`/`{iri}`/`{symbol}` placeholders.
        text: String,
    },
    /// Replace the binding's `site` range with rendered text.
    ReplaceSite {
        /// Template text.
        text: String,
    },
    /// No textual change — an advisory/guard-only step.
    NoOp,
}

/// Anchor for an [`EditTemplate::InsertLine`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Anchor {
    /// Insert at the very top of the document.
    TopOfFile,
    /// Insert on the line after the last `@prefix` declaration (or top of file).
    AfterLastPrefix,
}

/// A single repair step — a node in the route's partial order.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepairStep {
    /// Unique within the route.
    pub id: StepId,
    /// Human-readable title (shown in the CodeAction menu / agent display).
    pub title: String,
    /// How this step's edit is computed.
    pub edit: EditTemplate,
}

/// A POWL partial order over steps: nodes + "must precede" edges.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PartialOrder {
    /// The repair steps.
    pub nodes: Vec<RepairStep>,
    /// `(a, b)` means step a must be applied before step b. Acyclic invariant.
    pub edges: Vec<(StepId, StepId)>,
}

/// A repair route: a separable POWL model that repairs one diagnostic family.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepairRoute {
    /// Stable route id.
    pub id: RouteId,
    /// Diagnostic family this route repairs.
    pub family: RepairFamily,
    /// The partial order of repair steps.
    pub steps: PartialOrder,
    /// Human description.
    pub description: String,
    /// Route origin.
    pub provenance: Provenance,
    /// Higher fires first when multiple routes match a family.
    pub priority: i32,
}

/// Run-specific bindings extracted from a diagnostic site, used to instantiate a
/// route's edit templates into concrete edits.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RouteBindings {
    /// The offending token's range (replace/insert anchor).
    pub site: Option<Range>,
    /// Undeclared/private prefix, if relevant.
    pub prefix: Option<String>,
    /// Full IRI to bind a prefix to, or public replacement IRI.
    pub iri: Option<String>,
    /// SPARQL var / SHACL shape / receipt id, family-dependent.
    pub symbol: Option<String>,
}

impl PartialOrder {
    /// Cheap structural soundness check (separable WF-net criterion):
    /// acyclic, every edge references existing nodes, ≥1 node, and (for >1 node)
    /// at least one entry (in-degree 0) and one exit (out-degree 0).
    #[must_use]
    pub fn is_sound(&self) -> bool {
        if self.nodes.is_empty() {
            return false;
        }
        // All edge endpoints must exist.
        let ids: std::collections::HashSet<&StepId> = self.nodes.iter().map(|n| &n.id).collect();
        for (a, b) in &self.edges {
            if !ids.contains(a) || !ids.contains(b) {
                return false;
            }
        }
        if !self.is_acyclic() {
            return false;
        }
        if self.nodes.len() == 1 {
            return true; // single step (edges must be empty/self — caught by acyclic)
        }
        let has_entry = self
            .nodes
            .iter()
            .any(|n| !self.edges.iter().any(|(_, t)| *t == n.id));
        let has_exit = self
            .nodes
            .iter()
            .any(|n| !self.edges.iter().any(|(s, _)| *s == n.id));
        has_entry && has_exit
    }

    /// Topological sort of step ids respecting the partial order (ties broken by
    /// declaration order for determinism). Returns `None` if cyclic.
    #[must_use]
    pub fn topo_order(&self) -> Option<Vec<StepId>> {
        let mut in_deg: std::collections::HashMap<&StepId, usize> =
            self.nodes.iter().map(|n| (&n.id, 0usize)).collect();
        for (_, t) in &self.edges {
            if let Some(d) = in_deg.get_mut(t) {
                *d += 1;
            }
        }
        // Process nodes in declaration order to keep ties deterministic.
        let mut order = Vec::new();
        let mut ready: Vec<&StepId> = self
            .nodes
            .iter()
            .filter(|n| in_deg.get(&n.id) == Some(&0))
            .map(|n| &n.id)
            .collect();
        let mut remaining: std::collections::HashMap<&StepId, usize> = in_deg.clone();
        while let Some(id) = ready.first().copied() {
            ready.remove(0);
            order.push(id.clone());
            for (s, t) in &self.edges {
                if s == id {
                    if let Some(d) = remaining.get_mut(t) {
                        *d -= 1;
                        if *d == 0 {
                            // insert preserving declaration order
                            ready.push(t);
                            ready.sort_by_key(|x| {
                                self.nodes.iter().position(|n| &n.id == *x).unwrap_or(usize::MAX)
                            });
                        }
                    }
                }
            }
        }
        if order.len() == self.nodes.len() {
            Some(order)
        } else {
            None
        }
    }

    fn is_acyclic(&self) -> bool {
        self.topo_order().is_some()
    }

    /// Two steps are concurrent iff neither precedes the other (transitively).
    #[must_use]
    pub fn are_concurrent(&self, a: &StepId, b: &StepId) -> bool {
        a != b && !self.precedes(a, b) && !self.precedes(b, a)
    }

    fn precedes(&self, a: &StepId, b: &StepId) -> bool {
        // BFS over edges from a, looking for b.
        let mut stack = vec![a];
        let mut seen = std::collections::HashSet::new();
        while let Some(cur) = stack.pop() {
            for (s, t) in &self.edges {
                if s == cur {
                    if t == b {
                        return true;
                    }
                    if seen.insert(t) {
                        stack.push(t);
                    }
                }
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn step(id: &str) -> RepairStep {
        RepairStep {
            id: StepId(id.to_string()),
            title: id.to_string(),
            edit: EditTemplate::NoOp,
        }
    }

    #[test]
    fn linear_route_is_sound_and_ordered() {
        let po = PartialOrder {
            nodes: vec![step("a"), step("b"), step("c")],
            edges: vec![
                (StepId("a".into()), StepId("b".into())),
                (StepId("b".into()), StepId("c".into())),
            ],
        };
        assert!(po.is_sound());
        assert_eq!(
            po.topo_order(),
            Some(vec![
                StepId("a".into()),
                StepId("b".into()),
                StepId("c".into())
            ])
        );
    }

    #[test]
    fn cyclic_route_is_unsound() {
        let po = PartialOrder {
            nodes: vec![step("a"), step("b")],
            edges: vec![
                (StepId("a".into()), StepId("b".into())),
                (StepId("b".into()), StepId("a".into())),
            ],
        };
        assert!(!po.is_sound());
        assert_eq!(po.topo_order(), None);
    }

    #[test]
    fn independent_steps_are_concurrent() {
        // a precedes both b and c; b and c are independent (concurrent).
        let po = PartialOrder {
            nodes: vec![step("a"), step("b"), step("c")],
            edges: vec![
                (StepId("a".into()), StepId("b".into())),
                (StepId("a".into()), StepId("c".into())),
            ],
        };
        assert!(po.is_sound());
        assert!(po.are_concurrent(&StepId("b".into()), &StepId("c".into())));
        assert!(!po.are_concurrent(&StepId("a".into()), &StepId("b".into())));
    }

    #[test]
    fn single_step_route_is_sound() {
        let po = PartialOrder {
            nodes: vec![step("only")],
            edges: vec![],
        };
        assert!(po.is_sound());
    }
}
