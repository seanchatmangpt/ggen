//! POWL 2.0 model (Kourani et al. Defs 3.6–3.9): a hierarchy of leaves,
//! partial orders (concurrency), and choice graphs (generalized decision +
//! cyclic logic), plus a *bounded* language interpreter.
//!
//! The full language of a model with a cyclic choice graph is infinite, so
//! [`Powl::language_upto`] enumerates only label-sequences of length `≤ k`.
//! That is exactly what the differential round-trip test compares against the
//! WF-net's bounded token-game language — three independent computations of
//! the same bounded language must agree.
//!
//! ## A separate Lean formalization exists
//!
//! `~/mfact/procint/ProcInt/Models/ChoiceGraph.lean` and
//! `~/mfact/procint/ProcInt/Models/Powl.lean` are a kernel-checked Lean 4
//! formalization (`lake build`, zero unauthorized axioms) of definitions from
//! the same Kourani/Park/van der Aalst paper lineage this module implements.
//! Neither codebase references the other — mfact's own doc comments say those
//! two files port a *different, separate* Rust crate (`wasm4pm-compat`, not
//! `powl2-decompose`). See the doc comments on [`ChoiceGraph`], [`GNode`],
//! and [`Powl`] below for which specific definitions correspond and where
//! the two sides diverge. This is a citation for discoverability only; the
//! two formalizations have not been cross-verified against each other.

use std::collections::BTreeSet;

/// A sequence of activity labels (silent `τ` steps are already elided).
pub type Trace = Vec<String>;
/// A (bounded) language: a set of traces.
pub type Language = BTreeSet<Trace>;

/// A directed choice graph over child indices `0..n` plus the artificial
/// start `▷` and end `□` nodes (Def 3.6). Edges are index pairs; [`START`]
/// and [`END`] are the sentinels for `▷`/`□`.
///
/// `ChoiceGraph`/`ChoiceGraph.Valid` in
/// `~/mfact/procint/ProcInt/Models/ChoiceGraph.lean` independently formalize
/// Definition 1 of Kourani, Park, van der Aalst, "Unlocking Non-Block-
/// Structured Decisions: Inductive Mining with Choice Graphs" (arXiv:
/// 2505.07052) — a directed graph over start/end/activity/submodel nodes
/// with a unique reachable-from-start, reaches-end node set. This type here
/// implements the same shape of definition under Def 3.6 of arXiv:2602.15739
/// (Kourani, Park & van der Aalst, "Hierarchical Decomposition of Separable
/// Workflow-Nets"), a different, later paper by an overlapping author set;
/// whether Def 1 (2505.07052) and Def 3.6 (2602.15739) are the identical
/// definition restated has not been verified from this codebase alone.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChoiceGraph {
    /// number of child submodels (graph nodes, excluding ▷/□).
    pub n: usize,
    /// directed edges over `{START, 0..n, END}`.
    pub edges: BTreeSet<(GNode, GNode)>,
}

/// A node of a [`ChoiceGraph`]: the start `▷`, the end `□`, or a child index.
///
/// Compare `ChoiceGraphNode` in
/// `~/mfact/procint/ProcInt/Models/ChoiceGraph.lean`: Lean's `start`/
/// `finish` correspond to [`Start`](GNode::Start)/[`End`](GNode::End) here,
/// and Lean's `subModel (i : ℕ)` corresponds to [`Child`](GNode::Child).
/// Lean additionally carries an inline `activity (a : α)` node kind that
/// this Rust type does not have as a separate `GNode` variant — here an
/// activity is always reached indirectly, as a `Child` index into a
/// [`Powl::Leaf`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum GNode {
    /// The artificial start node `▷`.
    Start,
    /// The artificial end node `□`.
    End,
    /// Child submodel index.
    Child(usize),
}

/// The `▷` start sentinel.
pub const START: GNode = GNode::Start;
/// The `□` end sentinel.
pub const END: GNode = GNode::End;

/// A POWL 2.0 model over transition labels (Def 3.7).
///
/// `~/mfact/procint/ProcInt/Models/Powl.lean` also defines a Lean `Powl`
/// inductive type, but by its own doc comment it formalizes a *different*
/// source: Kourani and van Zelst, BPM 2023, Definitions 1–2 — the original
/// tree-structured POWL (`atom` / `silent` / `xor` / `loop` / `po`), not the
/// choice-graph-based POWL 2.0 this enum implements. The two are not
/// structural analogs: this enum's [`Choice`](Powl::Choice) variant routes
/// through a [`ChoiceGraph`] (exclusive paths *and* cycles), whereas Lean's
/// exclusive choice is an n-ary tree node (`xor`, arity ≥ 2) and its
/// iteration is a dedicated `loop (doP redoP : Powl α)` constructor rather
/// than a graph cycle. See the [`ChoiceGraph`] doc comment above for the
/// definition that Lean's `ChoiceGraph.lean` (not `Powl.lean`) does
/// correspond to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Powl {
    /// A leaf transition: `Some(activity)` or silent `τ` (`None`).
    Leaf(Option<String>),
    /// A partial order `≺(ψ₁,…,ψₙ)` over its children. `order` holds the
    /// (transitively closed) strict partial order as index pairs `(i,j)`
    /// meaning `ψᵢ ≺ ψⱼ` (all of `ψᵢ` before all of `ψⱼ`).
    PartialOrder {
        /// child submodels.
        children: Vec<Powl>,
        /// `(i,j) ∈ order` iff `ψᵢ ≺ ψⱼ`.
        order: BTreeSet<(usize, usize)>,
    },
    /// A choice graph `γ(ψ₁,…,ψₙ)` (Def 3.6/3.9): exclusive paths and cycles.
    Choice {
        /// child submodels.
        children: Vec<Powl>,
        /// the routing graph over child indices.
        graph: ChoiceGraph,
    },
    /// An external execution cut identifying a POWL region whose execution boundary
    /// leaves the current process cell.
    ExternalCut {
        /// The admitted POWL region (W).
        region: Box<Powl>,
        /// The declared SPARQL projection (Q).
        projection: String,
        /// The declared Tera renderer (T).
        renderer: String,
    },
}

/// Structural address of a node within a [`Powl`] model: the sequence of
/// child indices along the path from the model's root to this node (the
/// empty path addresses the root itself).
///
/// PRD v26.7.11 §7.3 requires "every POWL activity SHALL be addressable as
/// a potential workflow socket." [`SocketPath`] is that addressing scheme:
/// every node in a [`Powl`] tree, leaf or composite, has exactly one path,
/// and no two nodes in the same tree share a path, because each level of
/// recursion appends a distinct ordinal child index (see
/// [`Powl::collect_sockets`]). `PartialOrder`/`Choice` children are indexed
/// by their position in `children`; `ExternalCut` has exactly one child —
/// its `region` — always addressed as index `0`.
///
/// # Determinism
/// Construction is pure index bookkeeping: no wall clock, no randomness, no
/// hashing. `Ord` is lexicographic over the index sequence (a strict prefix
/// sorts before any path it prefixes), so any collection of paths sorts and
/// dedupes deterministically without a canonicalization pass.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SocketPath(Vec<usize>);

impl SocketPath {
    /// The root path (addresses the model's top-level node).
    #[must_use]
    pub fn root() -> Self {
        SocketPath(Vec::new())
    }

    /// Extends this path one level down to child `index`.
    #[must_use]
    pub fn child(&self, index: usize) -> Self {
        let mut v = self.0.clone();
        v.push(index);
        SocketPath(v)
    }

    /// Depth of this path (`0` for the root).
    #[must_use]
    pub fn depth(&self) -> usize {
        self.0.len()
    }

    /// The immediate parent path, or `None` if this is the root.
    #[must_use]
    pub fn parent(&self) -> Option<SocketPath> {
        if self.0.is_empty() {
            None
        } else {
            Some(SocketPath(self.0[..self.0.len() - 1].to_vec()))
        }
    }

    /// The raw child-index sequence (read-only view), root-to-node order.
    #[must_use]
    pub fn segments(&self) -> &[usize] {
        &self.0
    }
}

impl std::fmt::Display for SocketPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            write!(f, "/")
        } else {
            for seg in &self.0 {
                write!(f, "/{seg}")?;
            }
            Ok(())
        }
    }
}

/// The [`Powl`] variant found at a [`WorkflowSocketId`]'s path, recorded so
/// callers can dispatch on socket kind without re-walking the tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SocketKind {
    /// [`Powl::Leaf`].
    Leaf,
    /// [`Powl::PartialOrder`].
    PartialOrder,
    /// [`Powl::Choice`].
    Choice,
    /// [`Powl::ExternalCut`].
    ExternalCut,
}

impl SocketKind {
    fn of(model: &Powl) -> Self {
        match model {
            Powl::Leaf(_) => SocketKind::Leaf,
            Powl::PartialOrder { .. } => SocketKind::PartialOrder,
            Powl::Choice { .. } => SocketKind::Choice,
            Powl::ExternalCut { .. } => SocketKind::ExternalCut,
        }
    }
}

/// A stable identifier for a potential **workflow socket**: any addressable
/// point in an admitted POWL v2 model (PRD v26.7.11 §7.3, "recursive
/// workflow sockets" + "every POWL activity SHALL be addressable as a
/// potential workflow socket").
///
/// Every node is addressable, not only leaf activities: a composite
/// (`PartialOrder`/`Choice`/`ExternalCut`) region can itself be the target
/// of a *recursive* workflow socket — e.g. an external cut projects a whole
/// subtree, not a single activity — so restricting addressability to leaves
/// would under-cover the PRD's "recursive" qualifier.
///
/// Combines the structural [`SocketPath`] with the [`SocketKind`] observed
/// at that path so a socket id captures both *where* a node lives and
/// *what* it is; two ids are equal only if both agree, which catches an
/// address computed against a stale/mismatched model at construction
/// rather than silently returning the wrong node at first use.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WorkflowSocketId {
    /// Structural address within the model.
    pub path: SocketPath,
    /// The kind of node found at `path`.
    pub kind: SocketKind,
}

impl std::fmt::Display for WorkflowSocketId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "socket:{}#{:?}", self.path, self.kind)
    }
}

impl Powl {
    /// The bounded language of the model: all traces of length `≤ max_len`
    /// (Def 3.9, truncated). For acyclic models with `max_len` at least the
    /// longest trace this is the *exact* language.
    #[must_use]
    pub fn language_upto(&self, max_len: usize) -> Language {
        match self {
            Powl::Leaf(None) => {
                let mut l = Language::new();
                l.insert(vec![]);
                l
            }
            Powl::Leaf(Some(a)) => {
                let mut l = Language::new();
                if max_len >= 1 {
                    l.insert(vec![a.clone()]);
                }
                l
            }
            Powl::PartialOrder { children, order } => {
                let child_langs: Vec<Language> =
                    children.iter().map(|c| c.language_upto(max_len)).collect();
                shuffle_language(&child_langs, order, max_len)
            }
            Powl::Choice { children, graph } => {
                let child_langs: Vec<Language> =
                    children.iter().map(|c| c.language_upto(max_len)).collect();
                choice_language(&child_langs, graph, max_len)
            }
            Powl::ExternalCut { region, .. } => region.language_upto(max_len),
        }
    }

    /// Every node in this model, addressed as a [`WorkflowSocketId`] (PRD
    /// v26.7.11 §7.3: "every POWL activity SHALL be addressable as a
    /// potential workflow socket"). Includes leaves and composite
    /// (`PartialOrder`/`Choice`/`ExternalCut`) nodes alike.
    ///
    /// # Complexity
    /// O(n log n) where n is the number of nodes in the tree: a single DFS
    /// with one `BTreeSet` insert (O(log n)) per node.
    #[must_use]
    pub fn sockets(&self) -> BTreeSet<WorkflowSocketId> {
        let mut out = BTreeSet::new();
        self.collect_sockets(&SocketPath::root(), &mut out);
        out
    }

    /// DFS helper for [`Self::sockets`]; see that method for complexity.
    fn collect_sockets(&self, path: &SocketPath, out: &mut BTreeSet<WorkflowSocketId>) {
        out.insert(WorkflowSocketId {
            path: path.clone(),
            kind: SocketKind::of(self),
        });
        match self {
            Powl::Leaf(_) => {}
            Powl::PartialOrder { children, .. } | Powl::Choice { children, .. } => {
                for (idx, child) in children.iter().enumerate() {
                    child.collect_sockets(&path.child(idx), out);
                }
            }
            Powl::ExternalCut { region, .. } => {
                region.collect_sockets(&path.child(0), out);
            }
        }
    }

    /// Resolves a [`SocketPath`] to the node at that address, or `None` if
    /// the path does not exist in this model (an out-of-range child index,
    /// or a path that descends past a leaf or past `ExternalCut`'s single
    /// `region` child).
    ///
    /// # Complexity
    /// O(depth) — one step per path segment, no allocation.
    #[must_use]
    pub fn socket_at(&self, path: &SocketPath) -> Option<&Powl> {
        let mut node = self;
        for &seg in path.segments() {
            node = match node {
                Powl::Leaf(_) => return None,
                Powl::PartialOrder { children, .. } | Powl::Choice { children, .. } => {
                    children.get(seg)?
                }
                Powl::ExternalCut { region, .. } => {
                    if seg == 0 {
                        region.as_ref()
                    } else {
                        return None;
                    }
                }
            };
        }
        Some(node)
    }

    /// Computes the explicit parent-child closure over this model (PRD
    /// v26.7.11 §7.3: "parent-child closure SHALL be representable"): the
    /// set of direct structural edges, addressed by [`WorkflowSocketId`],
    /// from which transitive ancestor/descendant queries are answered
    /// without re-walking the [`Powl`] tree — see [`ParentChildClosure`].
    ///
    /// # Complexity
    /// O(n log n) — see [`ParentChildClosure::from_model`].
    #[must_use]
    pub fn parent_child_closure(&self) -> ParentChildClosure {
        ParentChildClosure::from_model(self)
    }
}

/// A direct (depth-1) parent→child structural edge, addressed by
/// [`WorkflowSocketId`] on both ends.
///
/// `Ord` compares `parent` first, then `child` (derived, field
/// declaration order), so a [`BTreeSet<ParentChildEdge>`] sorts as a stable,
/// auditable edge list grouped by parent — the canonical form a receipt
/// would hash.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParentChildEdge {
    /// The composite node's socket id.
    pub parent: WorkflowSocketId,
    /// The immediate child's socket id.
    pub child: WorkflowSocketId,
}

/// The explicit parent-child closure of a [`Powl`] model (PRD v26.7.11
/// §7.3). Carries the canonical direct-edge set plus two indices derived
/// from it (child-list-per-parent, parent-of-child) so repeated queries
/// don't re-walk the [`Powl`] tree or rescan the flat edge set.
///
/// # Determinism
/// Built by a single deterministic DFS over the tree (child order = index
/// order in `children`/`region`); no hashing, randomness, or wall clock.
/// The two indices are derived solely from the edges built in the same
/// pass, so they cannot diverge from `edges()`.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ParentChildClosure {
    edges: BTreeSet<ParentChildEdge>,
    children_index: std::collections::BTreeMap<WorkflowSocketId, BTreeSet<WorkflowSocketId>>,
    parent_index: std::collections::BTreeMap<WorkflowSocketId, WorkflowSocketId>,
}

impl ParentChildClosure {
    /// Builds the direct-edge closure of `model` (the root is included as
    /// a socket via [`Powl::sockets`]-compatible addressing, but has no
    /// incoming edge — see [`Self::parent_of`]).
    ///
    /// # Complexity
    /// O(n log n) — one DFS over the tree (n nodes), with one `BTreeSet`/
    /// `BTreeMap` insert (O(log n)) per edge.
    #[must_use]
    pub fn from_model(model: &Powl) -> Self {
        let mut edges = BTreeSet::new();
        let mut children_index = std::collections::BTreeMap::new();
        let mut parent_index = std::collections::BTreeMap::new();
        Self::walk(
            model,
            &SocketPath::root(),
            &mut edges,
            &mut children_index,
            &mut parent_index,
        );
        ParentChildClosure {
            edges,
            children_index,
            parent_index,
        }
    }

    /// DFS helper for [`Self::from_model`]; see that method for complexity.
    fn walk(
        node: &Powl, path: &SocketPath, edges: &mut BTreeSet<ParentChildEdge>,
        children_index: &mut std::collections::BTreeMap<
            WorkflowSocketId,
            BTreeSet<WorkflowSocketId>,
        >,
        parent_index: &mut std::collections::BTreeMap<WorkflowSocketId, WorkflowSocketId>,
    ) {
        let parent_id = WorkflowSocketId {
            path: path.clone(),
            kind: SocketKind::of(node),
        };
        let direct_children: Vec<(usize, &Powl)> = match node {
            Powl::Leaf(_) => Vec::new(),
            Powl::PartialOrder { children, .. } | Powl::Choice { children, .. } => {
                children.iter().enumerate().collect()
            }
            Powl::ExternalCut { region, .. } => vec![(0, region.as_ref())],
        };
        for (idx, child) in direct_children {
            let child_path = path.child(idx);
            let child_id = WorkflowSocketId {
                path: child_path.clone(),
                kind: SocketKind::of(child),
            };
            edges.insert(ParentChildEdge {
                parent: parent_id.clone(),
                child: child_id.clone(),
            });
            children_index
                .entry(parent_id.clone())
                .or_default()
                .insert(child_id.clone());
            parent_index.insert(child_id.clone(), parent_id.clone());
            Self::walk(child, &child_path, edges, children_index, parent_index);
        }
    }

    /// All direct parent→child edges, sorted by `(parent, child)`.
    #[must_use]
    pub fn edges(&self) -> &BTreeSet<ParentChildEdge> {
        &self.edges
    }

    /// The direct children of `parent` (empty if `parent` is a leaf or not
    /// present in this closure).
    ///
    /// # Complexity
    /// O(log n) — a single `BTreeMap` lookup plus a clone of the (typically
    /// small) child set.
    #[must_use]
    pub fn children_of(&self, parent: &WorkflowSocketId) -> BTreeSet<WorkflowSocketId> {
        self.children_index.get(parent).cloned().unwrap_or_default()
    }

    /// The direct parent of `child`, or `None` if `child` is the model root
    /// (or not present in this closure).
    ///
    /// # Complexity
    /// O(log n) — a single `BTreeMap` lookup.
    #[must_use]
    pub fn parent_of(&self, child: &WorkflowSocketId) -> Option<WorkflowSocketId> {
        self.parent_index.get(child).cloned()
    }

    /// All descendants of `of` (transitive closure over child edges).
    ///
    /// # Complexity
    /// O(n log n) worst case — bounded BFS visiting each node at most once,
    /// each step an O(log n) index lookup.
    #[must_use]
    pub fn descendants(&self, of: &WorkflowSocketId) -> BTreeSet<WorkflowSocketId> {
        let mut out = BTreeSet::new();
        let mut frontier = vec![of.clone()];
        while let Some(node) = frontier.pop() {
            for child in self.children_of(&node) {
                if out.insert(child.clone()) {
                    frontier.push(child);
                }
            }
        }
        out
    }

    /// All ancestors of `of` (transitive closure over parent edges), up to
    /// and including the root's direct children (the root itself has no
    /// parent, so it is never included).
    ///
    /// # Complexity
    /// O(depth log n) — one O(log n) index lookup per level of the tree.
    #[must_use]
    pub fn ancestors(&self, of: &WorkflowSocketId) -> BTreeSet<WorkflowSocketId> {
        let mut out = BTreeSet::new();
        let mut current = of.clone();
        while let Some(parent) = self.parent_of(&current) {
            // Defensive cycle guard: `parent_index` is built exclusively by
            // `walk`'s tree DFS, which never revisits a node, so a repeat
            // here can only mean a caller-constructed closure violated that
            // invariant — refuse to loop forever rather than panic.
            if !out.insert(parent.clone()) {
                break;
            }
            current = parent;
        }
        out
    }
}

impl ChoiceGraph {
    /// Successors of `node` in the graph (deterministic order).
    #[must_use]
    pub fn successors(&self, node: GNode) -> Vec<GNode> {
        self.edges
            .iter()
            .filter(|(u, _)| *u == node)
            .map(|(_, v)| *v)
            .collect()
    }
}

/// `L(γ(ψ₁,…))` bounded: union over all `▷→…→□` paths of the concatenation of
/// the visited children's languages, keeping traces of length `≤ max_len`.
/// Cyclic graphs terminate because a path visiting more than `max_len`
/// *labelled* children can only produce over-length traces (silent children
/// are capped separately to keep enumeration finite).
fn choice_language(child_langs: &[Language], graph: &ChoiceGraph, max_len: usize) -> Language {
    let mut out = Language::new();
    // (current node, accumulated language, path-length budget)
    let mut stack: Vec<(GNode, Vec<Trace>, usize)> = vec![(START, vec![vec![]], 0)];
    // Cap total nodes visited on a path to bound cyclic enumeration.
    let node_budget = max_len + 2;
    while let Some((node, acc, steps)) = stack.pop() {
        if steps > node_budget {
            continue;
        }
        for next in graph.successors(node) {
            match next {
                GNode::End => {
                    for t in &acc {
                        if t.len() <= max_len {
                            out.insert(t.clone());
                        }
                    }
                }
                GNode::Child(i) => {
                    let mut extended = Vec::new();
                    for prefix in &acc {
                        for suffix in &child_langs[i] {
                            let mut cat = prefix.clone();
                            cat.extend(suffix.iter().cloned());
                            if cat.len() <= max_len {
                                extended.push(cat);
                            }
                        }
                    }
                    if !extended.is_empty() {
                        stack.push((next, extended, steps + 1));
                    }
                }
                GNode::Start => {}
            }
        }
    }
    out
}

/// `L(≺(ψ₁,…))` bounded: for every choice of one trace per child, all
/// order-preserving interleavings (Def 3.8), keeping length `≤ max_len`.
fn shuffle_language(
    child_langs: &[Language], order: &BTreeSet<(usize, usize)>, max_len: usize,
) -> Language {
    let mut out = Language::new();
    let choices: Vec<Vec<Trace>> = child_langs
        .iter()
        .map(|l| l.iter().cloned().collect())
        .collect();
    let mut selection: Vec<Trace> = vec![vec![]; choices.len()];
    cartesian(&choices, 0, &mut selection, &mut |sel| {
        interleave(sel, order, max_len, &mut out);
    });
    out
}

/// Enumerate the cartesian product of one trace per child, invoking `f` on
/// each full selection.
fn cartesian(
    choices: &[Vec<Trace>], idx: usize, selection: &mut Vec<Trace>, f: &mut impl FnMut(&[Trace]),
) {
    if idx == choices.len() {
        f(selection);
        return;
    }
    for candidate in &choices[idx] {
        selection[idx] = candidate.clone();
        cartesian(choices, idx + 1, selection, f);
    }
}

/// Order-preserving shuffle (Def 3.8): child `i`'s next element may be emitted
/// only when every predecessor `j ≺ i` has been fully emitted.
fn interleave(
    seqs: &[Trace], order: &BTreeSet<(usize, usize)>, max_len: usize, out: &mut Language,
) {
    let n = seqs.len();
    let mut pos = vec![0usize; n];
    let mut acc = Vec::new();
    interleave_rec(seqs, order, &mut pos, &mut acc, max_len, out);
}

fn interleave_rec(
    seqs: &[Trace], order: &BTreeSet<(usize, usize)>, pos: &mut [usize], acc: &mut Trace,
    max_len: usize, out: &mut Language,
) {
    if acc.len() > max_len {
        return;
    }
    let n = seqs.len();
    if (0..n).all(|i| pos[i] == seqs[i].len()) {
        out.insert(acc.clone());
        return;
    }
    for i in 0..n {
        if pos[i] >= seqs[i].len() {
            continue;
        }
        // predecessors j ≺ i must be fully emitted
        let ready = (0..n).all(|j| !order.contains(&(j, i)) || pos[j] == seqs[j].len());
        if !ready {
            continue;
        }
        acc.push(seqs[i][pos[i]].clone());
        pos[i] += 1;
        interleave_rec(seqs, order, pos, acc, max_len, out);
        pos[i] -= 1;
        acc.pop();
    }
}

#[cfg(test)]
mod socket_tests {
    use super::*;

    /// `≺(a, b)` under a fresh partial order, `a` and `b` leaves.
    fn two_leaf_partial_order() -> Powl {
        Powl::PartialOrder {
            children: vec![
                Powl::Leaf(Some("a".to_string())),
                Powl::Leaf(Some("b".to_string())),
            ],
            order: BTreeSet::from([(0, 1)]),
        }
    }

    /// `≺(≺(a, b), γ(c, d))`: nested partial order over a leaf pair and a
    /// choice graph over another leaf pair — exercises every non-leaf
    /// `Powl` variant except `ExternalCut` at two depths.
    fn nested_model() -> Powl {
        let inner_po = two_leaf_partial_order();
        let choice = Powl::Choice {
            children: vec![
                Powl::Leaf(Some("c".to_string())),
                Powl::Leaf(Some("d".to_string())),
            ],
            graph: ChoiceGraph {
                n: 2,
                edges: BTreeSet::from([
                    (START, GNode::Child(0)),
                    (GNode::Child(0), END),
                    (START, GNode::Child(1)),
                    (GNode::Child(1), END),
                ]),
            },
        };
        Powl::PartialOrder {
            children: vec![inner_po, choice],
            order: BTreeSet::from([(0, 1)]),
        }
    }

    #[test]
    fn socket_path_root_has_depth_zero_and_no_parent() {
        let root = SocketPath::root();
        assert_eq!(root.depth(), 0);
        assert_eq!(root.parent(), None);
        assert_eq!(root.segments(), &[] as &[usize]);
        assert_eq!(root.to_string(), "/");
    }

    #[test]
    fn socket_path_child_extends_and_parent_reverses() {
        let p = SocketPath::root().child(2).child(0);
        assert_eq!(p.depth(), 2);
        assert_eq!(p.segments(), &[2, 0]);
        assert_eq!(p.to_string(), "/2/0");
        assert_eq!(p.parent(), Some(SocketPath::root().child(2)));
    }

    #[test]
    fn socket_path_ord_is_prefix_before_extension() {
        // A path is Less than any path it is a strict prefix of — this is
        // what makes `SocketPath::root()` a safe global lower bound.
        let root = SocketPath::root();
        let child = root.child(0);
        assert!(root < child);
    }

    #[test]
    fn sockets_covers_every_node_leaf_and_composite() {
        let model = nested_model();
        let sockets = model.sockets();
        // root + inner PartialOrder + 2 leaves + Choice + 2 leaves = 7.
        assert_eq!(sockets.len(), 7);
        let kinds: Vec<SocketKind> = sockets.iter().map(|s| s.kind).collect();
        assert_eq!(kinds.iter().filter(|k| **k == SocketKind::Leaf).count(), 4);
        assert_eq!(
            kinds
                .iter()
                .filter(|k| **k == SocketKind::PartialOrder)
                .count(),
            2
        );
        assert_eq!(
            kinds.iter().filter(|k| **k == SocketKind::Choice).count(),
            1
        );
    }

    #[test]
    fn sockets_is_deterministic_across_calls() {
        let model = nested_model();
        let first: Vec<WorkflowSocketId> = model.sockets().into_iter().collect();
        let second: Vec<WorkflowSocketId> = model.sockets().into_iter().collect();
        assert_eq!(first, second);
    }

    #[test]
    fn external_cut_region_addressed_at_child_zero() {
        let cut = Powl::ExternalCut {
            region: Box::new(Powl::Leaf(Some("out-of-cell".to_string()))),
            projection: "SELECT * { ?s ?p ?o }".to_string(),
            renderer: "template".to_string(),
        };
        let sockets = cut.sockets();
        assert_eq!(sockets.len(), 2); // the cut node itself + its region leaf
        let region_path = SocketPath::root().child(0);
        assert!(sockets
            .iter()
            .any(|s| s.path == region_path && s.kind == SocketKind::Leaf));
    }

    #[test]
    fn socket_at_round_trips_every_socket() {
        let model = nested_model();
        for socket in model.sockets() {
            let node = model
                .socket_at(&socket.path)
                .unwrap_or_else(|| panic!("socket_at failed to resolve {}", socket.path));
            assert_eq!(SocketKind::of(node), socket.kind);
        }
    }

    #[test]
    fn socket_at_rejects_out_of_range_index() {
        let model = two_leaf_partial_order();
        assert!(model.socket_at(&SocketPath::root().child(5)).is_none());
    }

    #[test]
    fn socket_at_rejects_descent_past_a_leaf() {
        let model = two_leaf_partial_order();
        let leaf_path = SocketPath::root().child(0);
        assert!(model.socket_at(&leaf_path.child(0)).is_none());
    }

    /// `≺(≺(γ(a, b), c), d)`: three levels of nesting below the root
    /// (root PartialOrder → child PartialOrder → Choice → leaf), the case
    /// PROJ-750's adversarial review flagged as untested depth for
    /// `ParentChildClosure` determinism specifically (existing fixtures in
    /// this module top out at 2 levels of nesting).
    fn three_level_nested_model() -> Powl {
        let choice = Powl::Choice {
            children: vec![
                Powl::Leaf(Some("a".to_string())),
                Powl::Leaf(Some("b".to_string())),
            ],
            graph: ChoiceGraph {
                n: 2,
                edges: BTreeSet::from([
                    (START, GNode::Child(0)),
                    (GNode::Child(0), END),
                    (START, GNode::Child(1)),
                    (GNode::Child(1), END),
                ]),
            },
        };
        let level1 = Powl::PartialOrder {
            children: vec![choice, Powl::Leaf(Some("c".to_string()))],
            order: BTreeSet::from([(0, 1)]),
        };
        Powl::PartialOrder {
            children: vec![level1, Powl::Leaf(Some("d".to_string()))],
            order: BTreeSet::from([(0, 1)]),
        }
    }

    #[test]
    fn parent_child_closure_is_deterministic_across_calls_three_level_nesting() {
        let model = three_level_nested_model();
        // Leaf "a" sits at /0/0/0: root PartialOrder -> level1 PartialOrder
        // -> Choice -> Leaf, i.e. 3 levels of nesting below the root.
        let leaf_a_path = SocketPath::root().child(0).child(0).child(0);
        assert_eq!(leaf_a_path.depth(), 3);
        assert_eq!(
            model.socket_at(&leaf_a_path),
            Some(&Powl::Leaf(Some("a".to_string())))
        );

        let first = model.parent_child_closure();
        let second = model.parent_child_closure();
        assert_eq!(first, second);
    }

    #[test]
    fn parent_child_closure_direct_edges_match_tree_shape() {
        let model = nested_model();
        let closure = model.parent_child_closure();
        let root = WorkflowSocketId {
            path: SocketPath::root(),
            kind: SocketKind::PartialOrder,
        };
        let root_children = closure.children_of(&root);
        assert_eq!(root_children.len(), 2);
        // Root has no parent.
        assert_eq!(closure.parent_of(&root), None);
    }

    #[test]
    fn parent_child_closure_children_of_leaf_is_empty() {
        let model = nested_model();
        let closure = model.parent_child_closure();
        let leaf_a = WorkflowSocketId {
            path: SocketPath::root().child(0).child(0),
            kind: SocketKind::Leaf,
        };
        assert!(closure.children_of(&leaf_a).is_empty());
    }

    #[test]
    fn parent_child_closure_descendants_is_transitive() {
        let model = nested_model();
        let closure = model.parent_child_closure();
        let root = WorkflowSocketId {
            path: SocketPath::root(),
            kind: SocketKind::PartialOrder,
        };
        let descendants = closure.descendants(&root);
        // Every other socket in the tree is a descendant of the root.
        assert_eq!(descendants.len(), model.sockets().len() - 1);
    }

    #[test]
    fn parent_child_closure_ancestors_reverses_descendants() {
        let model = nested_model();
        let closure = model.parent_child_closure();
        let leaf_c = WorkflowSocketId {
            path: SocketPath::root().child(1).child(0),
            kind: SocketKind::Leaf,
        };
        let ancestors = closure.ancestors(&leaf_c);
        // choice node (depth 1) + root (depth 0).
        assert_eq!(ancestors.len(), 2);
        let root = WorkflowSocketId {
            path: SocketPath::root(),
            kind: SocketKind::PartialOrder,
        };
        assert!(ancestors.contains(&root));
        // And the root's descendant set must contain leaf_c back.
        assert!(closure.descendants(&root).contains(&leaf_c));
    }

    #[test]
    fn parent_child_closure_edge_count_equals_node_count_minus_one() {
        let model = nested_model();
        let closure = model.parent_child_closure();
        // A tree with n nodes has exactly n-1 edges.
        assert_eq!(closure.edges().len(), model.sockets().len() - 1);
    }

    #[test]
    fn parent_child_closure_unknown_socket_has_no_children_or_parent() {
        let model = two_leaf_partial_order();
        let closure = model.parent_child_closure();
        let phantom = WorkflowSocketId {
            path: SocketPath::root().child(99),
            kind: SocketKind::Leaf,
        };
        assert!(closure.children_of(&phantom).is_empty());
        assert_eq!(closure.parent_of(&phantom), None);
        assert!(closure.ancestors(&phantom).is_empty());
        assert!(closure.descendants(&phantom).is_empty());
    }

    /// Recursively counts every `Powl` tree node (leaves + composites),
    /// walking only `children`/`region` — the same structural surface
    /// [`Powl::collect_sockets`] and [`ParentChildClosure::walk`] use.
    /// Deliberately never consults `Choice::graph`, so it is an independent
    /// ground truth for the node count that cannot be inflated or deflated
    /// by a routing cycle.
    fn count_tree_nodes(model: &Powl) -> usize {
        1 + match model {
            Powl::Leaf(_) => 0,
            Powl::PartialOrder { children, .. } | Powl::Choice { children, .. } => {
                children.iter().map(count_tree_nodes).sum()
            }
            Powl::ExternalCut { region, .. } => count_tree_nodes(region),
        }
    }

    /// Plain DFS cycle detection (recursion-stack method) over a
    /// [`ChoiceGraph`]'s directed edges. Used only to confirm a test
    /// fixture is genuinely cyclic before trusting it as the cyclic case —
    /// not part of the crate's production surface.
    fn graph_has_cycle(graph: &ChoiceGraph) -> bool {
        fn visit(
            node: GNode, graph: &ChoiceGraph, visiting: &mut BTreeSet<GNode>,
            done: &mut BTreeSet<GNode>,
        ) -> bool {
            if done.contains(&node) {
                return false;
            }
            if !visiting.insert(node) {
                return true; // back-edge: `node` is already on the current path
            }
            for next in graph.successors(node) {
                if visit(next, graph, visiting, done) {
                    return true;
                }
            }
            visiting.remove(&node);
            done.insert(node);
            false
        }
        let mut visiting = BTreeSet::new();
        let mut done = BTreeSet::new();
        visit(START, graph, &mut visiting, &mut done)
    }

    /// PROJ-750's Fortune-5 done bar names two coverage bars: "test depth
    /// extended to 3+-level nesting AND at least one cyclic ChoiceGraph
    /// case". [`parent_child_closure_is_deterministic_across_calls_three_level_nesting`]
    /// covers the first bar; this test covers the second, reusing the exact
    /// WF-net shape of `decompose_tests.rs::loop_decomposes_to_cyclic_choice_graph`
    /// (a POWL loop: `a` forks into a redo branch `c` that feeds back into the
    /// same choice point, or an exit branch `b`) so the routing graph under
    /// test is a *real* decomposition output, not a hand-approximated cycle.
    ///
    /// [`Powl::sockets`] and [`Powl::parent_child_closure`] only ever recurse
    /// over `children: Vec<Powl>` (a `Choice` node's tree-structural
    /// children) — never over `ChoiceGraph::edges` (the routing graph that
    /// carries the cycle). This test proves that by checking the node counts
    /// both methods return against [`count_tree_nodes`], an independent
    /// count that is structurally incapable of seeing `graph.edges` — and
    /// documents it as the invariant that makes a routing cycle inert to
    /// both methods: neither can loop forever, double-count, or diverge run
    /// to run because of a cycle that exists only in `graph.edges`.
    #[test]
    fn sockets_and_closure_are_inert_to_a_cyclic_choice_graph() {
        let n = crate::net::WfNet::new(
            ["source", "p1", "sink"].map(str::to_string),
            [
                ("a".to_string(), Some("a".to_string())),
                ("c".to_string(), Some("c".to_string())),
                ("b".to_string(), Some("b".to_string())),
            ],
            [
                ("source".to_string(), "a".to_string()),
                ("p1".to_string(), "c".to_string()),
                ("p1".to_string(), "b".to_string()),
            ],
            [
                ("a".to_string(), "p1".to_string()),
                ("c".to_string(), "p1".to_string()),
                ("b".to_string(), "sink".to_string()),
            ],
            "source",
            "sink",
        )
        .unwrap_or_else(|e| panic!("valid WF-net (mirrors decompose_tests::loop_net): {e}"));

        let model = crate::decompose::convert(&n).unwrap_or_else(|e| {
            panic!("loop net is separable (mirrors decompose_tests::loop_net): {e:?}")
        });
        let Powl::Choice { graph, .. } = &model else {
            panic!(
                "a loop decomposes to Powl::Choice, per \
                 decompose_tests::loop_decomposes_to_cyclic_choice_graph; got {model:?}"
            );
        };

        // Confirm the routing graph is genuinely cyclic — otherwise this
        // would not be a meaningful test of cyclic-graph inertness.
        assert!(
            graph_has_cycle(graph),
            "loop_net's ChoiceGraph must contain a routing cycle for this \
             to be a meaningful test of cyclic-graph inertness; graph = {graph:?}"
        );

        // Node counts from sockets()/parent_child_closure() must equal the
        // independent tree-only count — proof neither consults graph.edges.
        let expected_nodes = count_tree_nodes(&model);
        let sockets = model.sockets();
        assert_eq!(sockets.len(), expected_nodes);

        let closure = model.parent_child_closure();
        // A tree with n nodes has exactly n-1 direct edges (same invariant
        // `parent_child_closure_edge_count_equals_node_count_minus_one`
        // checks acyclically), confirmed here across a real routing cycle.
        assert_eq!(closure.edges().len(), expected_nodes - 1);

        // Determinism holds across a cyclic graph too, not only acyclic
        // ones: re-running both methods must reproduce identical output.
        assert_eq!(model.sockets(), sockets);
        assert_eq!(model.parent_child_closure(), closure);
    }
}
