//! Stage-1 decomposition: `ConvertNetToPOWL` (Algorithm 3) with its two
//! partitioners `PartitionMG` (Algorithm 1, conflict-hiding → partial order)
//! and `PartitionSM` (Algorithm 2, concurrency-hiding → choice graph).
//!
//! # Separability is the admission predicate
//!
//! Algorithm 3's fall-through branch — *neither a base case, nor a
//! conflict-hiding partition, nor a concurrency-hiding partition exists* — is
//! the paper's completeness boundary: the algorithm is complete exactly on
//! **separable** WF-nets (Def 3.13). We do not approximate the fall-through;
//! we **refuse** it, emitting a [`Refusal`] carrying a machine reason and a
//! BLAKE3 receipt over the offending (sub-)net.
//!
//! This is a Rice-style boundary for process models: "is this WF-net
//! expressible in POWL 2.0?" is answered constructively (a decomposition) or
//! refused with evidence — never silently approximated. Non-free-choice nets
//! are refused up front (every separable net is free-choice, Def 3.13
//! corollary), and irreducible free-choice fragments are refused at the
//! recursion level where both partitioners fail.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::net::WfNet;
use crate::powl::{ChoiceGraph, GNode, Powl, END, START};

/// Machine-readable reason a WF-net (or sub-net) was refused as non-separable.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RefusalReason {
    /// The net is not free-choice (Def 3.4). Every separable WF-net is
    /// free-choice, so this is a sufficient non-separability witness.
    NonFreeChoice {
        /// The two transitions with overlapping-but-unequal pre-sets.
        transitions: (String, String),
    },
    /// At some recursion level, no base case and no valid partition was
    /// found: an irreducible fragment outside POWL 2.0's expressive scope.
    IrreducibleFragment {
        /// depth in the recursion at which the fall-through fired.
        depth: usize,
    },
    /// The bounded-lane recursion budget was exhausted before termination.
    BudgetExhausted {
        /// the depth budget that was hit.
        budget: usize,
    },
}

impl std::fmt::Display for RefusalReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefusalReason::NonFreeChoice { transitions: (a, b) } => write!(
                f,
                "non-free-choice: transitions {a} and {b} share input places but have unequal pre-sets; \
                 every separable WF-net is free-choice"
            ),
            RefusalReason::IrreducibleFragment { depth } => write!(
                f,
                "irreducible fragment at depth {depth}: neither a conflict-hiding nor a \
                 concurrency-hiding partition exists (fall-through of Algorithm 3)"
            ),
            RefusalReason::BudgetExhausted { budget } => {
                write!(f, "bounded-lane recursion budget {budget} exhausted")
            }
        }
    }
}

/// A receipted refusal: the doctrine payoff. Carries the reason and a BLAKE3
/// content address of the (sub-)net that could not be decomposed.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Refusal {
    /// classified reason.
    pub reason: RefusalReason,
    /// BLAKE3 hex hash of the offending (sub-)net.
    pub net_hash: String,
    /// the POWL 2.0 admission-predicate verdict — always `false` here.
    pub separable: bool,
}

impl std::fmt::Display for Refusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "REFUSED (non-separable, net blake3:{}): {}",
            self.net_hash, self.reason
        )
    }
}

/// The maximum recursion depth (the "bounded lanes"). Separable nets in
/// practice nest far shallower; exceeding it is itself a refusal.
pub const DEFAULT_DEPTH_BUDGET: usize = 64;

/// Convert a safe & sound WF-net into an equivalent POWL 2.0 model
/// (Algorithm 3), or refuse it as non-separable.
///
/// # Errors
/// Returns [`Refusal`] when the net falls outside the separable class.
pub fn convert(net: &WfNet) -> Result<Powl, Refusal> {
    convert_with_budget(net, DEFAULT_DEPTH_BUDGET)
}

/// [`convert`] with an explicit bounded-lane depth budget.
///
/// # Errors
/// Returns [`Refusal`] when the net is non-separable or the budget is hit.
pub fn convert_with_budget(net: &WfNet, budget: usize) -> Result<Powl, Refusal> {
    // Cheap up-front witness: non-free-choice ⇒ non-separable.
    if let Some((a, b)) = non_free_choice_witness(net) {
        return Err(Refusal {
            reason: RefusalReason::NonFreeChoice {
                transitions: (a, b),
            },
            net_hash: net.content_hash(),
            separable: false,
        });
    }
    convert_rec(net, 0, budget)
}

fn non_free_choice_witness(net: &WfNet) -> Option<(String, String)> {
    let ts: Vec<&String> = net.transitions().keys().collect();
    for i in 0..ts.len() {
        let pi = net.pre_trans(ts[i]);
        for &tj in ts.iter().skip(i + 1) {
            let pj = net.pre_trans(tj);
            if !pi.is_disjoint(&pj) && pi != pj {
                return Some((ts[i].clone(), tj.clone()));
            }
        }
    }
    None
}

fn convert_rec(net: &WfNet, depth: usize, budget: usize) -> Result<Powl, Refusal> {
    if depth > budget {
        return Err(Refusal {
            reason: RefusalReason::BudgetExhausted { budget },
            net_hash: net.content_hash(),
            separable: false,
        });
    }

    // (1) Base case: a single transition between exactly source and sink.
    if let Some(leaf) = base_case(net) {
        return Ok(leaf);
    }

    // (2) Attempt a marked-graph (partial order) decomposition.
    let mg = partition_mg(net);
    if mg.len() > 1 && is_conflict_hiding(net, &mg) && mg_makes_progress(net, &mg) {
        let mut children = Vec::with_capacity(mg.len());
        for part in &mg {
            children.push(convert_child(net, part, project_mg, depth, budget)?);
        }
        let order = execution_order(net, &mg);
        return Ok(Powl::PartialOrder { children, order });
    }

    // (3) Attempt a state-machine (choice graph) decomposition.
    let sm = partition_sm(net);
    if sm.len() > 1 && is_concurrency_hiding(net, &sm) && sm_makes_progress(net, &sm) {
        let mut children = Vec::with_capacity(sm.len());
        for part in &sm {
            children.push(convert_child(net, part, project_sm, depth, budget)?);
        }
        let graph = execution_flow(net, &sm);
        return Ok(Powl::Choice { children, graph });
    }

    // (4) Fall-through: refuse (the completeness boundary).
    Err(Refusal {
        reason: RefusalReason::IrreducibleFragment { depth },
        net_hash: net.content_hash(),
        separable: false,
    })
}

/// Convert one child part. A **singleton** part `{t}` is a single transition
/// whose POWL representation is always `Leaf(t)` — the surrounding
/// choice/concurrency is captured at the parent node (including a parent
/// self-edge `i→i` for a self-looping transition). Shortcutting it here both
/// matches the semantics and terminates the recursion on self-loops (whose
/// projection would otherwise reproduce a bare-loop sub-net indefinitely).
/// Larger parts are projected and recursed per Algorithm 3.
fn convert_child(
    net: &WfNet, part: &BTreeSet<String>, project: fn(&WfNet, &BTreeSet<String>) -> WfNet,
    depth: usize, budget: usize,
) -> Result<Powl, Refusal> {
    if part.len() == 1 {
        let t = part.iter().next().expect("singleton");
        return Ok(Powl::Leaf(net.label(t)));
    }
    let sub = project(net, part);
    convert_rec(&sub, depth + 1, budget)
}

// ── (1) base case ────────────────────────────────────────────────────────────

fn base_case(net: &WfNet) -> Option<Powl> {
    if net.transitions().len() != 1 || net.places().len() != 2 {
        return None;
    }
    let t = net.transitions().keys().next()?;
    let pre = net.pre_trans(t);
    let post = net.post_trans(t);
    if pre.len() == 1 && post.len() == 1 && pre.contains(net.source()) && post.contains(net.sink())
    {
        return Some(Powl::Leaf(net.label(t)));
    }
    None
}

// ── (2) PartitionMG — Algorithm 1 (conflict-hiding) ────────────────────────────

/// Union-find grouping of transition ids into a partition.
struct Groups {
    parent: BTreeMap<String, String>,
}

impl Groups {
    fn singletons<'a>(ts: impl IntoIterator<Item = &'a String>) -> Self {
        Groups {
            parent: ts.into_iter().map(|t| (t.clone(), t.clone())).collect(),
        }
    }
    fn find(&mut self, x: &str) -> String {
        let p = self.parent[x].clone();
        if p == x {
            return p;
        }
        let root = self.find(&p);
        self.parent.insert(x.to_string(), root.clone());
        root
    }
    fn union_all(&mut self, members: &BTreeSet<String>) {
        let mut iter = members.iter();
        let Some(first) = iter.next() else { return };
        let root = self.find(first);
        for m in iter {
            let r = self.find(m);
            self.parent.insert(r, root.clone());
        }
    }
    fn parts(&mut self) -> Vec<BTreeSet<String>> {
        let keys: Vec<String> = self.parent.keys().cloned().collect();
        let mut by_root: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for k in keys {
            let r = self.find(&k);
            by_root.entry(r).or_default().insert(k);
        }
        by_root.into_values().collect()
    }
}

/// Algorithm 1: group transitions so all exclusive choices are hidden inside
/// parts (yielding a top-level marked graph).
fn partition_mg(net: &WfNet) -> Vec<BTreeSet<String>> {
    let mut g = Groups::singletons(net.transitions().keys());
    // Precompute reachability once.
    let reach: BTreeMap<String, BTreeSet<String>> = net
        .transitions()
        .keys()
        .map(|t| (t.clone(), net.reaches(t)))
        .collect();

    // Forward analysis: XOR-splits (places with >1 outgoing transition).
    for p in net.places() {
        let branches = net.post_place(p);
        if branches.len() <= 1 {
            continue;
        }
        let mut group = BTreeSet::new();
        for t in net.transitions().keys() {
            let reached_by = branches.iter().filter(|b| reach[*b].contains(t)).count();
            if reached_by > 0 && reached_by < branches.len() {
                group.insert(t.clone());
            }
        }
        if group.len() > 1 {
            g.union_all(&group);
        }
    }

    // Backward analysis: XOR-joins (places with >1 incoming transition).
    for p in net.places() {
        let branches = net.pre_place(p);
        if branches.len() <= 1 {
            continue;
        }
        let mut group = BTreeSet::new();
        for t in net.transitions().keys() {
            let reaches_branch = branches.iter().filter(|b| reach[t].contains(*b)).count();
            if reaches_branch > 0 && reaches_branch < branches.len() {
                group.insert(t.clone());
            }
        }
        if group.len() > 1 {
            g.union_all(&group);
        }
    }

    g.parts()
}

/// Def 4.1: a partition is conflict-hiding iff no place is a top-level
/// XOR-split/join and every part has a single (equivalence-class) entry and
/// exit interface.
fn is_conflict_hiding(net: &WfNet, parts: &[BTreeSet<String>]) -> bool {
    // Conditions 1 & 2: each place in ≤1 part's entry set and ≤1 part's exit.
    for p in net.places() {
        let in_entry = parts
            .iter()
            .filter(|part| net.entry_places(part).contains(p))
            .count();
        if in_entry > 1 {
            return false;
        }
        let in_exit = parts
            .iter()
            .filter(|part| net.exit_places(part).contains(p))
            .count();
        if in_exit > 1 {
            return false;
        }
    }
    // Conditions 3 & 4: single-entry / single-exit fragments (≈ equivalence).
    for part in parts {
        let entry: Vec<String> = net.entry_places(part).into_iter().collect();
        for i in 0..entry.len() {
            for j in (i + 1)..entry.len() {
                if !net.equiv_wrt(&entry[i], &entry[j], part) {
                    return false;
                }
            }
        }
        let exit: Vec<String> = net.exit_places(part).into_iter().collect();
        for i in 0..exit.len() {
            for j in (i + 1)..exit.len() {
                if !net.equiv_wrt(&exit[i], &exit[j], part) {
                    return false;
                }
            }
        }
    }
    true
}

/// Def 4.3 (+ transitive closure): `i ≺ j` iff `Tᵢ▷ ∩ ▷Tⱼ ≠ ∅`.
fn execution_order(net: &WfNet, parts: &[BTreeSet<String>]) -> BTreeSet<(usize, usize)> {
    let n = parts.len();
    let exits: Vec<BTreeSet<String>> = parts.iter().map(|p| net.exit_places(p)).collect();
    let entries: Vec<BTreeSet<String>> = parts.iter().map(|p| net.entry_places(p)).collect();
    let mut rel = BTreeSet::new();
    for i in 0..n {
        for j in 0..n {
            if i != j && !exits[i].is_disjoint(&entries[j]) {
                rel.insert((i, j));
            }
        }
    }
    transitive_closure(&rel, n)
}

fn transitive_closure(rel: &BTreeSet<(usize, usize)>, n: usize) -> BTreeSet<(usize, usize)> {
    let mut m = vec![vec![false; n]; n];
    for &(i, j) in rel {
        m[i][j] = true;
    }
    for k in 0..n {
        for i in 0..n {
            for j in 0..n {
                if m[i][k] && m[k][j] {
                    m[i][j] = true;
                }
            }
        }
    }
    let mut out = BTreeSet::new();
    for i in 0..n {
        for j in 0..n {
            if m[i][j] && i != j {
                out.insert((i, j));
            }
        }
    }
    out
}

// ── (3) PartitionSM — Algorithm 2 (concurrency-hiding) ─────────────────────────

/// Algorithm 2: group transitions so all parallel splits/joins are hidden
/// inside parts (yielding a top-level state machine).
fn partition_sm(net: &WfNet) -> Vec<BTreeSet<String>> {
    let mut g = Groups::singletons(net.transitions().keys());

    // Forward analysis: AND-splits (transitions with >1 output place).
    for tsplit in net.transitions().keys() {
        let branches = net.post_trans(tsplit);
        if branches.len() <= 1 {
            continue;
        }
        let fr: BTreeMap<String, BTreeSet<String>> = branches
            .iter()
            .map(|p| (p.clone(), net.fwd_restricted(p, tsplit)))
            .collect();
        let mut threads = BTreeSet::new();
        for t in net.transitions().keys() {
            let hit = branches.iter().filter(|p| fr[*p].contains(t)).count();
            if hit > 0 && hit < branches.len() {
                threads.insert(t.clone());
            }
        }
        let mut group = threads;
        group.insert(tsplit.clone());
        if group.len() > 1 {
            g.union_all(&group);
        }
    }

    // Backward analysis: AND-joins (transitions with >1 input place).
    for tjoin in net.transitions().keys() {
        let branches = net.pre_trans(tjoin);
        if branches.len() <= 1 {
            continue;
        }
        let br: BTreeMap<String, BTreeSet<String>> = branches
            .iter()
            .map(|p| (p.clone(), net.bwd_restricted(p, tjoin)))
            .collect();
        let mut threads = BTreeSet::new();
        for t in net.transitions().keys() {
            let hit = branches.iter().filter(|p| br[*p].contains(t)).count();
            if hit > 0 && hit < branches.len() {
                threads.insert(t.clone());
            }
        }
        let mut group = threads;
        group.insert(tjoin.clone());
        if group.len() > 1 {
            g.union_all(&group);
        }
    }

    g.parts()
}

/// Def 4.4: concurrency-hiding iff every part has exactly one entry and one
/// exit place.
fn is_concurrency_hiding(net: &WfNet, parts: &[BTreeSet<String>]) -> bool {
    parts
        .iter()
        .all(|part| net.entry_places(part).len() == 1 && net.exit_places(part).len() == 1)
}

/// Def 4.8: the top-level choice graph `flow(N,G)`.
fn execution_flow(net: &WfNet, parts: &[BTreeSet<String>]) -> ChoiceGraph {
    let n = parts.len();
    let exits: Vec<BTreeSet<String>> = parts.iter().map(|p| net.exit_places(p)).collect();
    let entries: Vec<BTreeSet<String>> = parts.iter().map(|p| net.entry_places(p)).collect();
    let mut edges = BTreeSet::new();
    for i in 0..n {
        for j in 0..n {
            if !exits[i].is_disjoint(&entries[j]) {
                edges.insert((GNode::Child(i), GNode::Child(j)));
            }
        }
        if entries[i].contains(net.source()) {
            edges.insert((START, GNode::Child(i)));
        }
        if exits[i].contains(net.sink()) {
            edges.insert((GNode::Child(i), END));
        }
    }
    ChoiceGraph { n, edges }
}

// ── projections + normalization ────────────────────────────────────────────────

/// `P|T'` — places touching part `T'`.
fn places_touching(net: &WfNet, part: &BTreeSet<String>) -> BTreeSet<String> {
    net.places()
        .iter()
        .filter(|p| !net.pre_place(p).is_disjoint(part) || !net.post_place(p).is_disjoint(part))
        .cloned()
        .collect()
}

/// Def 4.2: partial-order projection `ProjectMG(N,T')` with fresh `ps`/`pe`
/// unifying the (possibly multiple, but ≈-equivalent) entry/exit places.
fn project_mg(net: &WfNet, part: &BTreeSet<String>) -> WfNet {
    let entry = net.entry_places(part);
    let exit = net.exit_places(part);
    let touching = places_touching(net, part);

    let ps = fresh(net, "ps");
    let pe = fresh(net, "pe");

    let mut places: BTreeSet<String> = touching
        .iter()
        .filter(|p| !entry.contains(*p) && !exit.contains(*p))
        .cloned()
        .collect();
    places.insert(ps.clone());
    places.insert(pe.clone());

    let mut pt: BTreeSet<(String, String)> = BTreeSet::new();
    let mut tp: BTreeSet<(String, String)> = BTreeSet::new();

    // Internal arcs among kept places and T'.
    for p in &places {
        for t in net.post_place(p) {
            if part.contains(&t) {
                pt.insert((p.clone(), t.clone()));
            }
        }
        for t in net.pre_place(p) {
            if part.contains(&t) {
                tp.insert((t.clone(), p.clone()));
            }
        }
    }
    // Redirect entry-place arcs (into/out of T') onto ps.
    for p in &entry {
        for t in net.post_place(p) {
            if part.contains(&t) {
                pt.insert((ps.clone(), t.clone()));
            }
        }
        for t in net.pre_place(p) {
            if part.contains(&t) {
                tp.insert((t.clone(), ps.clone()));
            }
        }
    }
    // Redirect exit-place arcs onto pe.
    for p in &exit {
        for t in net.pre_place(p) {
            if part.contains(&t) {
                tp.insert((t.clone(), pe.clone()));
            }
        }
        for t in net.post_place(p) {
            if part.contains(&t) {
                pt.insert((pe.clone(), t.clone()));
            }
        }
    }

    let transitions: BTreeMap<String, crate::net::Label> =
        part.iter().map(|t| (t.clone(), net.label(t))).collect();

    normalize(places, transitions, pt, tp, ps, pe)
}

/// Def 4.7: choice-graph projection `ProjectSM(N,T')`; the unique entry/exit
/// places are kept as-is.
fn project_sm(net: &WfNet, part: &BTreeSet<String>) -> WfNet {
    let entry = net.entry_places(part);
    let exit = net.exit_places(part);
    let places = places_touching(net, part);
    let ps = entry
        .iter()
        .next()
        .cloned()
        .unwrap_or_else(|| net.source().to_string());
    let pe = exit
        .iter()
        .next()
        .cloned()
        .unwrap_or_else(|| net.sink().to_string());

    let mut pt = BTreeSet::new();
    let mut tp = BTreeSet::new();
    for p in &places {
        for t in net.post_place(p) {
            if part.contains(&t) {
                pt.insert((p.clone(), t.clone()));
            }
        }
        for t in net.pre_place(p) {
            if part.contains(&t) {
                tp.insert((t.clone(), p.clone()));
            }
        }
    }
    let transitions: BTreeMap<String, crate::net::Label> =
        part.iter().map(|t| (t.clone(), net.label(t))).collect();

    normalize(places, transitions, pt, tp, ps, pe)
}

/// Normalization: add a fresh source (via a silent transition) if `ps` has
/// incoming arcs, and a fresh sink if `pe` has outgoing arcs, so the result
/// is a valid WF-net (Def 3.3).
fn normalize(
    mut places: BTreeSet<String>, mut transitions: BTreeMap<String, crate::net::Label>,
    mut pt: BTreeSet<(String, String)>, mut tp: BTreeSet<(String, String)>, ps: String, pe: String,
) -> WfNet {
    let ps_has_in = tp.iter().any(|(_, p)| p == &ps);
    let pe_has_out = pt.iter().any(|(p, _)| p == &pe);

    let source = if ps_has_in {
        let new_src = uniq(&places, &transitions, "src");
        let tau = uniq_trans(&transitions, "tau_in");
        places.insert(new_src.clone());
        transitions.insert(tau.clone(), None);
        pt.insert((new_src.clone(), tau.clone()));
        tp.insert((tau, ps.clone()));
        new_src
    } else {
        ps.clone()
    };

    let sink = if pe_has_out {
        let new_sink = uniq(&places, &transitions, "snk");
        let tau = uniq_trans(&transitions, "tau_out");
        places.insert(new_sink.clone());
        transitions.insert(tau.clone(), None);
        tp.insert((tau.clone(), new_sink.clone()));
        pt.insert((pe.clone(), tau));
        new_sink
    } else {
        pe.clone()
    };

    WfNet::new(places, transitions, pt, tp, source, sink)
        .expect("projection + normalization yields a valid WF-net")
}

fn fresh(net: &WfNet, stem: &str) -> String {
    let mut i = 0;
    loop {
        let cand = format!("__{stem}{i}");
        if !net.places().contains(&cand) && !net.transitions().contains_key(&cand) {
            return cand;
        }
        i += 1;
    }
}

fn uniq(
    places: &BTreeSet<String>, transitions: &BTreeMap<String, crate::net::Label>, stem: &str,
) -> String {
    let mut i = 0;
    loop {
        let cand = format!("__{stem}{i}");
        if !places.contains(&cand) && !transitions.contains_key(&cand) {
            return cand;
        }
        i += 1;
    }
}

fn uniq_trans(transitions: &BTreeMap<String, crate::net::Label>, stem: &str) -> String {
    let mut i = 0;
    loop {
        let cand = format!("__{stem}{i}");
        if !transitions.contains_key(&cand) {
            return cand;
        }
        i += 1;
    }
}

// ── structural-progress guards (Algorithm 3, lines 7 & 17) ─────────────────────

/// Line 7: ensure no part's projection reproduces the whole net (which would
/// stall the recursion). We compare canonical structural signatures.
fn mg_makes_progress(net: &WfNet, parts: &[BTreeSet<String>]) -> bool {
    let sig = net.signature();
    parts
        .iter()
        .all(|part| project_mg(net, part).signature() != sig)
}

fn sm_makes_progress(net: &WfNet, parts: &[BTreeSet<String>]) -> bool {
    let sig = net.signature();
    parts
        .iter()
        .all(|part| project_sm(net, part).signature() != sig)
}
