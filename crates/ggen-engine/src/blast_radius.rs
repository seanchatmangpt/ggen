//! Blast-radius gate (reverse-80/20 §3): compute the transitive impact set of a
//! planned write and refuse when it intersects protected critical surfaces.
//!
//! GATE ONLY — wiring into the write path (`write.rs`/`sync.rs`) is a follow-up
//! work order; this module is never called from the write path in this diff.
//!
//! SCIP integration: [`ScipGraph`] adapts the `ggen-scip` index (`Query`
//! handle) to [`SymbolGraph`]. Callers come from SCIP reference occurrences
//! attributed to the nearest preceding definition in the same document (the
//! emitter's ranges are name-only, so no enclosing-scope span exists —
//! documented heuristic, deterministic). Subtype relations are NOT encoded in
//! the ggen-scip JSON index (no inheritance extraction): [`ScipGraph::
//! subtypes_of`] is always empty — an honest encoding gap, not a silent one.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashSet};

/// Default protected critical-system surfaces (configurable at call sites).
pub const DEFAULT_PROTECTED_PATHS: &[&str] =
    &["ggen-engine/src/sync.rs", "ggen-abb-sbb/src/lib.rs"];

/// Default closure depth cap.
pub const DEFAULT_MAX_DEPTH: usize = 16;

/// A planned write in an admission plan.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PlannedWrite {
    /// File the write lands in.
    pub path: String,
    /// Symbols the write touches, resolved by the planner (diff hunk or
    /// template projection).
    pub symbols: Vec<String>,
}

/// A planned-write admission plan — input to [`admit_write`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WritePlan {
    /// Writes to admit as one unit.
    pub writes: Vec<PlannedWrite>,
    /// Overrides [`DEFAULT_MAX_DEPTH`] when set.
    pub max_depth: Option<usize>,
}

impl WritePlan {
    /// Plan with the default depth cap.
    pub fn new(writes: Vec<PlannedWrite>) -> Self {
        Self { writes, max_depth: None }
    }
}

/// How a symbol entered the radius.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RadiusOrigin {
    /// Directly written in the plan.
    Written,
    /// Reached through callers* only.
    CallersStar,
    /// Reached through subtypes* (or subtype-then-callers) from a written symbol.
    CallersStarPlusSubtypesStar,
}

/// The transitive impact set of a write plan.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BlastRadius {
    /// Symbol ids in the radius and how each was reached.
    pub symbols: BTreeMap<String, RadiusOrigin>,
    /// `caller -> callee` edges traversed, for the receipt slice.
    pub edges: BTreeSet<String>,
    /// Depth cap that was applied.
    pub depth_cap: usize,
    /// True if the cap truncated the closure (radius is a lower bound).
    pub truncated: bool,
}

/// Definition site of a symbol, for protected-path intersection.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SymbolLoc {
    /// File the symbol is defined in.
    pub path: String,
    /// Symbol id.
    pub symbol: String,
}

/// One written symbol's refusal detail.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RefusedWrite {
    /// File the planned write lands in.
    pub path: String,
    /// The written symbol whose radius touches a protected surface.
    pub symbol: String,
    /// Radius symbol that lands on a protected path.
    pub radius_symbol: String,
    /// The protected path it lands on.
    pub protected_path: String,
    /// Closure depth at which the protected symbol was reached.
    pub depth: usize,
}

/// The symbol graph. The R80-3 SCIP index implements this trait; until that
/// lands (disclosed follow-up), [`VecGraph`] is the in-repo fixture impl.
pub trait SymbolGraph {
    /// Symbols that reference `symbol` (callers* direction, one hop).
    fn callers_of(&self, symbol: &str) -> Vec<String>;
    /// Direct subtypes of `symbol` (implements/extends relations).
    fn subtypes_of(&self, symbol: &str) -> Vec<String>;
    /// Definition site of a symbol, for protected-path intersection.
    fn def_site(&self, symbol: &str) -> Option<SymbolLoc>;
}

/// Vec-based fixture implementation of [`SymbolGraph`].
#[derive(Debug, Clone, Default, PartialEq)]
pub struct VecGraph {
    /// `(caller, callee)` pairs: caller references callee.
    pub refs: Vec<(String, String)>,
    /// `(super, sub)` pairs.
    pub subtypes: Vec<(String, String)>,
    /// Definition sites.
    pub defs: Vec<SymbolLoc>,
}

impl SymbolGraph for VecGraph {
    fn callers_of(&self, symbol: &str) -> Vec<String> {
        self.refs
            .iter()
            .filter(|(_, callee)| callee == symbol)
            .map(|(caller, _)| caller.clone())
            .collect()
    }

    fn subtypes_of(&self, symbol: &str) -> Vec<String> {
        self.subtypes
            .iter()
            .filter(|(sup, _)| sup == symbol)
            .map(|(_, sub)| sub.clone())
            .collect()
    }

    fn def_site(&self, target: &str) -> Option<SymbolLoc> {
        self.defs.iter().find(|d| d.symbol == target).cloned()
    }
}

/// SCIP-backed [`SymbolGraph`] adapter over a `ggen-scip` [`ggen_scip::
/// query::Query`] handle. Built eagerly: reference occurrences are attributed
/// to enclosing definitions once, so `callers_of` is O(log n) map lookup.
///
/// Encoding gap (disclosed): the ggen-scip JSON index carries only
/// definition/reference occurrences — no subtype/implements relations — so
/// [`SymbolGraph::subtypes_of`] always returns `[]` and the radius is
/// callers*-only over SCIP. The fixture [`VecGraph`] remains the only graph
/// that exercises the subtype arm.
#[derive(Debug, Clone, Default, PartialEq)]
#[must_use]
pub struct ScipGraph {
    /// wire symbol -> set of caller wire symbols (inverted SCIP references).
    callers: BTreeMap<String, BTreeSet<String>>,
    /// wire symbol -> document path of its definition occurrence.
    def_sites: BTreeMap<String, String>,
}

impl ScipGraph {
    /// Build the adapter from a loaded SCIP query handle.
    #[must_use]
    pub fn from_query(query: &ggen_scip::query::Query) -> Self {
        let mut callers: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        let mut def_sites: BTreeMap<String, String> = BTreeMap::new();

        for doc in &query.index().documents {
            // Definition anchors in this document, in document order (the
            // emitter sorts occurrences by (range, symbol)).
            let mut def_anchors: Vec<(u32, u32, &str)> = doc
                .occurrences
                .iter()
                .filter(|o| o.roles & ggen_scip::ROLE_DEFINITION != 0)
                .map(|o| (o.range[0], o.range[1], o.symbol.as_str()))
                .collect();
            def_anchors.sort_unstable();

            for occ in &doc.occurrences {
                let is_def = occ.roles & ggen_scip::ROLE_DEFINITION != 0;
                let is_ref = occ.roles & ggen_scip::ROLE_REFERENCE != 0;
                if !is_ref {
                    continue;
                }
                // Nearest preceding definition anchor = the enclosing item
                // (heuristic — see module docs). Definition-only occurrences
                // (merged def+ref at the name node) attribute to themselves;
                // those are skipped because `is_def` occurrence is the anchor.
                if is_def {
                    continue;
                }
                let caller = def_anchors
                    .iter()
                    .rev()
                    .find(|(r, c, _)| (*r, *c) <= (occ.range[0], occ.range[1]))
                    .map(|(_, _, s)| s.to_string());
                if let Some(caller) = caller {
                    callers
                        .entry(occ.symbol.clone())
                        .or_default()
                        .insert(caller);
                }
            }

            for o in doc
                .occurrences
                .iter()
                .filter(|o| o.roles & ggen_scip::ROLE_DEFINITION != 0)
            {
                def_sites.insert(o.symbol.clone(), doc.path.clone());
            }
        }

        Self { callers, def_sites }
    }
}

impl SymbolGraph for ScipGraph {
    fn callers_of(&self, symbol: &str) -> Vec<String> {
        self.callers
            .get(symbol)
            .map(|s| s.iter().cloned().collect())
            .unwrap_or_default()
    }

    fn subtypes_of(&self, _symbol: &str) -> Vec<String> {
        // Encoding gap: SCIP JSON index carries no subtype relations.
        Vec::new()
    }

    fn def_site(&self, symbol: &str) -> Option<SymbolLoc> {
        self.def_sites
            .get(symbol)
            .map(|path| SymbolLoc { path: path.clone(), symbol: symbol.to_string() })
    }
}

/// Verdict of [`admit_write`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Admission {
    /// No protected surface is in any written symbol's radius.
    Alive {
        /// blake3 of the canonical plan.
        plan_digest: String,
        /// Radius slice payload.
        radius: BlastRadius,
        /// Chained admission record.
        receipt: BlastRadiusReceipt,
    },
    /// A written symbol's radius intersects a protected path.
    Refused {
        /// blake3 of the canonical plan.
        plan_digest: String,
        /// Per-written-symbol violation details.
        violations: Vec<RefusedWrite>,
        /// Chained admission record.
        receipt: BlastRadiusReceipt,
    },
    /// Closure was depth-cap truncated, so the radius is a lower bound, not a
    /// full proof: a refusal still stands, but a clean verdict degrades to
    /// PARTIAL (the gate never certifies ALIVE on an unproven radius).
    Partial {
        /// blake3 of the canonical plan.
        plan_digest: String,
        /// Why the verdict is degraded.
        reason: String,
        /// Radius slice payload (lower bound).
        radius: BlastRadius,
        /// Chained admission record.
        receipt: BlastRadiusReceipt,
    },
}

/// abb-sbb-shaped admission record, blake3 chained.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BlastRadiusReceipt {
    /// blake3 hex of the canonical (serialized) plan.
    pub plan_digest: String,
    /// blake3 hex over the radius slice.
    pub radius_digest: String,
    /// Chain head: `blake3(plan_digest ++ radius_digest ++ verdict)`.
    pub receipt_digest: String,
    /// "ALIVE" | "REFUSED" | "PARTIAL".
    pub verdict: String,
    /// Protected paths the gate was configured with.
    pub protected_paths: Vec<String>,
    /// Depth cap the closure ran under.
    pub depth_cap: usize,
}

fn b3(parts: &[&str]) -> String {
    let mut h = blake3::Hasher::new();
    for p in parts {
        h.update(p.as_bytes());
    }
    h.finalize().to_hex().to_string()
}

fn plan_digest(plan: &WritePlan) -> String {
    let canonical = serde_json::to_string(plan).expect("plan serializes");
    b3(&[&canonical])
}

/// Transitive closure over callers* (SCIP reference/def relations) and
/// subtypes*, depth-capped and cycle-guarded.
pub fn compute(target: &str, graph: &dyn SymbolGraph, max_depth: usize) -> BlastRadius {
    let mut symbols: BTreeMap<String, RadiusOrigin> = BTreeMap::new();
    let mut edges: BTreeSet<String> = BTreeSet::new();
    let mut truncated = false;
    // (symbol, depth, origin) frontier — cycle-guarded by `seen`.
    let mut frontier: Vec<(String, usize, RadiusOrigin)> =
        vec![(target.to_string(), 0, RadiusOrigin::Written)];
    let mut seen: HashSet<String> = HashSet::new();
    seen.insert(target.to_string());

    while let Some((sym, depth, origin)) = frontier.pop() {
        if depth >= max_depth {
            truncated = true;
            continue;
        }
        if depth > 0 {
            symbols.entry(sym.clone()).or_insert(origin.clone());
        }
        let mut next: Vec<(String, RadiusOrigin)> = graph
            .callers_of(&sym)
            .into_iter()
            .map(|c| (c, RadiusOrigin::CallersStar))
            .collect();
        for sub in graph.subtypes_of(&sym) {
            // A subtype inherits the callers-direction traversal: its callers
            // are also impacted, hence the compound origin.
            next.push((sub.clone(), RadiusOrigin::CallersStarPlusSubtypesStar));
            for c in graph.callers_of(&sub) {
                next.push((c, RadiusOrigin::CallersStarPlusSubtypesStar));
            }
        }
        for (n, n_origin) in next {
            edges.insert(format!("{sym} -> {n}"));
            if seen.insert(n.clone()) {
                frontier.push((n, depth + 1, n_origin));
            }
        }
    }

    BlastRadius { symbols, edges, depth_cap: max_depth, truncated }
}

/// Admission gate: REFUSED if any written path's symbols' radius intersects
/// `protected_paths`; ALIVE (or PARTIAL under depth truncation) with slice
/// payload otherwise.
pub fn admit_write(
    plan: &WritePlan,
    graph: &dyn SymbolGraph,
    protected_paths: &[String],
) -> Admission {
    let pd = plan_digest(plan);
    let cap = plan.max_depth.unwrap_or(DEFAULT_MAX_DEPTH);

    // Compute one merged radius over all written symbols (cycle-guarded by
    // construction), tracking per-symbol reach depth for violation reports.
    let mut radius_symbols: BTreeMap<String, (RadiusOrigin, usize)> = BTreeMap::new();
    let mut edges: BTreeSet<String> = BTreeSet::new();
    let mut truncated = false;
    let mut frontier: Vec<(String, usize)> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    for w in &plan.writes {
        for s in &w.symbols {
            if seen.insert(s.clone()) {
                radius_symbols.insert(s.clone(), (RadiusOrigin::Written, 0));
                frontier.push((s.clone(), 0));
            }
        }
    }

    while let Some((sym, depth)) = frontier.pop() {
        if depth >= cap {
            truncated = true;
            continue;
        }
        let mut next: Vec<(String, RadiusOrigin)> = graph
            .callers_of(&sym)
            .into_iter()
            .map(|c| (c, RadiusOrigin::CallersStar))
            .collect();
        for sub in graph.subtypes_of(&sym) {
            next.push((sub.clone(), RadiusOrigin::CallersStarPlusSubtypesStar));
            for c in graph.callers_of(&sub) {
                next.push((c, RadiusOrigin::CallersStarPlusSubtypesStar));
            }
        }
        for (n, origin) in next {
            edges.insert(format!("{sym} -> {n}"));
            if seen.insert(n.clone()) {
                radius_symbols.insert(n.clone(), (origin, depth + 1));
                frontier.push((n, depth + 1));
            }
        }
    }

    let radius = BlastRadius {
        symbols: radius_symbols
            .iter()
            .map(|(k, (o, _))| (k.clone(), o.clone()))
            .collect(),
        edges,
        depth_cap: cap,
        truncated,
    };

    // Protected-surface intersection: a radius symbol defined on a protected
    // path, or a written path itself protected, is a violation.
    let mut violations: Vec<RefusedWrite> = Vec::new();
    for w in &plan.writes {
        if protected_paths.iter().any(|p| p == &w.path) {
            for s in &w.symbols {
                violations.push(RefusedWrite {
                    path: w.path.clone(),
                    symbol: s.clone(),
                    radius_symbol: s.clone(),
                    protected_path: w.path.clone(),
                    depth: 0,
                });
            }
        }
        for s in &w.symbols {
            for (rs, (_, depth)) in &radius_symbols {
                if rs == s && depth == &0 {
                    continue; // the written symbol itself; path handled above
                }
                if let Some(loc) = graph.def_site(rs) {
                    if protected_paths.iter().any(|p| p == &loc.path) {
                        violations.push(RefusedWrite {
                            path: w.path.clone(),
                            symbol: s.clone(),
                            radius_symbol: rs.clone(),
                            protected_path: loc.path.clone(),
                            depth: *depth,
                        });
                    }
                }
            }
        }
    }
    violations.sort_by(|a, b| (&a.path, &a.symbol).cmp(&(&b.path, &b.symbol)));
    violations.dedup();

    let radius_digest = b3(&[
        &serde_json::to_string(&radius).expect("radius serializes"),
    ]);
    let verdict = if violations.is_empty() { "ALIVE" } else { "REFUSED" };
    let receipt = BlastRadiusReceipt {
        plan_digest: pd.clone(),
        radius_digest: radius_digest.clone(),
        receipt_digest: b3(&[&pd, &radius_digest, verdict]),
        verdict: verdict.to_string(),
        protected_paths: protected_paths.to_vec(),
        depth_cap: cap,
    };

    if !violations.is_empty() {
        return Admission::Refused { plan_digest: pd, violations, receipt };
    }
    if truncated {
        return Admission::Partial {
            plan_digest: pd,
            reason: format!(
                "radius depth-capped at {cap}; radius is a lower bound, ALIVE not certified"
            ),
            radius,
            receipt,
        };
    }
    Admission::Alive { plan_digest: pd, radius, receipt }
}
