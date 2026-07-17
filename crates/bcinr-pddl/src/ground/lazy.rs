//! Lazy grounding-as-join over dictionary-encoded facts.
//!
//! The naive grounder ([`crate::GroundProblem::build`]) materializes the
//! full Cartesian product `∏ᵢ |objects_of_type(paramᵢ)|` for every action
//! schema, then lets forward search discard the ones that never fire. For a
//! domain with sparse static structure (a road map, a supply graph) that is
//! quadratically — or worse — wasteful: almost every ground action is dead on
//! arrival because one of its precondition atoms can never hold.
//!
//! This grounder treats grounding as a **relational join**, the way a query
//! engine (qlever, a datalog evaluator) would:
//!
//! 1. **Relaxed reachability.** From the initial facts, repeatedly join each
//!    schema's preconditions against the current fact set and add its
//!    (delete-relaxed) effects, to a fixpoint. The result `R` is every atom
//!    that could *ever* be true. An action that can ever fire has all its
//!    preconditions in `R`; anything else is dead.
//! 2. **Join-driven materialization.** Ground each schema by joining its
//!    preconditions against `R`: an atom that introduces new variables is
//!    resolved by an ordered scan of the sorted, encoded fact store
//!    ([`super::facts`]); an atom that is already fully determined is settled by
//!    a single [`XorFilter`](super::xorf::XorFilter)-gated membership probe.
//!    Only surviving bindings are materialized, type-filtered exactly as the
//!    naive grounder's per-parameter candidate lists would be, and emitted in
//!    the naive grounder's odometer order.
//!
//! The produced action list is therefore *the naive list with the never-firing
//! entries removed*. Because a removed action is never applicable in any
//! reachable state, BFS over the pruned list explores the identical search tree
//! and returns the identical plan — correctness is not approximated, only
//! wasted materialization is.

use super::dict::{Dict, SymId};
use super::facts::FactStore;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use wasm4pm_compat::pddl::{
    Pddl8ActionSchema, Pddl8Atom, Pddl8Domain, Pddl8GroundAction, Pddl8GroundAtom, Pddl8Problem,
    Pddl8Tape, PDDL8_MAX_GROUND, PDDL8_MAX_PLAN_DEPTH,
};

/// Errors from indexed grounding / plan finding. Mirrors the meaningful subset
/// of `crate::Pddl8Error` this path can produce, so the caller's
/// "infeasibility is `Ok`" classification is unchanged whichever grounder ran.
use bcinr_mfw_ir::{
    BoundHit, BoundKind, Digest, ExhaustionWitness, PlannerOutcome, SearchProfileId,
};

/// `SearchProfileId` for this crate's indexed/lazy grounder's BFS — distinct
/// from `super::LEGACY_WHOLE_RUN_SEARCH_PROFILE` (the naive grounder's BFS)
/// so an `ExhaustionWitness` can, in principle, be traced back to which
/// grounder produced it. Both are sentinels predating the portfolio/rail
/// profile registry in `crate::search`.
const INDEXED_BFS_SEARCH_PROFILE: SearchProfileId = SearchProfileId(1);

/// Counters describing how much the indexed grounder saved over naive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GroundStats {
    /// Ground actions the naive grounder would materialize:
    /// `Σ_schemas ∏ᵢ |candidates(paramᵢ)|`.
    pub candidate_groundings: usize,
    /// Ground actions this grounder actually materialized (reachable subset).
    pub materialized_groundings: usize,
    /// Size of the relaxed-reachable atom set `R`.
    pub reachable_atoms: usize,
}

impl GroundStats {
    /// Fraction `materialized / candidate` in `[0, 1]`; `1.0` when there were no
    /// candidates. Lower is better.
    #[must_use]
    pub fn materialization_ratio(&self) -> f64 {
        if self.candidate_groundings == 0 {
            1.0
        } else {
            self.materialized_groundings as f64 / self.candidate_groundings as f64
        }
    }
}

/// Type lattice restricting parameter bindings to type-compatible objects — the
/// same rule the naive grounder applies via its per-parameter candidate lists.
struct TypeIndex {
    object_type: HashMap<String, String>,
    parent: HashMap<String, String>,
}

impl TypeIndex {
    fn build(domain: &Pddl8Domain, problem: &Pddl8Problem) -> Self {
        let object_type = problem.object_types.iter().cloned().collect();
        let parent = domain
            .types
            .iter()
            .filter_map(|t| t.parent.clone().map(|p| (t.name.clone(), p)))
            .collect();
        Self {
            object_type,
            parent,
        }
    }

    fn satisfies(&self, obj: &str, required: &str) -> bool {
        if required == "object" {
            return true;
        }
        let mut cur: &str = self
            .object_type
            .get(obj)
            .map(String::as_str)
            .unwrap_or("object");
        loop {
            if cur == required {
                return true;
            }
            match self.parent.get(cur) {
                Some(p) => cur = p.as_str(),
                None => return false,
            }
        }
    }
}

/// Per-schema, per-parameter candidate object IDs (type-filtered), in
/// `problem.objects` order — the naive grounder's candidate lists, encoded.
struct SchemaPlan<'a> {
    schema: &'a Pddl8ActionSchema,
    /// param index → ordered candidate object IDs.
    cand_ids: Vec<Vec<u32>>,
    /// param name → param index.
    param_index: HashMap<String, usize>,
}

impl<'a> SchemaPlan<'a> {
    fn build(
        schema: &'a Pddl8ActionSchema, objects: &[String], type_index: &TypeIndex, dict: &mut Dict,
    ) -> Self {
        let typed: HashMap<&str, &str> = schema
            .typed_params
            .iter()
            .map(|(p, t)| (p.as_str(), t.as_str()))
            .collect();
        let cand_ids: Vec<Vec<u32>> = schema
            .params
            .iter()
            .map(|p| {
                let required = typed.get(p.as_str()).copied().unwrap_or("object");
                objects
                    .iter()
                    .filter(|o| type_index.satisfies(o, required))
                    .map(|o| dict.intern(o).0)
                    .collect()
            })
            .collect();
        let param_index = schema
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.clone(), i))
            .collect();
        Self {
            schema,
            cand_ids,
            param_index,
        }
    }

    /// Ground actions naive would materialize for this schema.
    fn candidate_count(&self) -> usize {
        self.cand_ids.iter().map(Vec::len).product()
    }

    fn is_candidate(&self, p: usize, obj_id: u32) -> bool {
        self.cand_ids[p].contains(&obj_id)
    }

    /// Odometer digit: position of `obj_id` in parameter `p`'s candidate list.
    fn position_of(&self, p: usize, obj_id: u32) -> Option<usize> {
        self.cand_ids[p].iter().position(|&id| id == obj_id)
    }
}

/// A dictionary-encoded, XOR-pruned, lazily-grounded PDDL8 problem.
///
/// `initial_state`/`goal`/`actions` are `pub` to mirror
/// `crate::ground::GroundProblem`'s field visibility exactly — callers
/// that alias `IndexedGroundProblem` in place of `GroundProblem` (e.g.
/// `use pddl_index::ground::IndexedGroundProblem as GroundProblem;`) need no
/// call-site changes beyond the import.
pub struct IndexedGroundProblem {
    /// The problem's initial-state ground atoms.
    pub initial_state: BTreeSet<Pddl8GroundAtom>,
    /// The problem's goal condition as a conjunction of ground atoms.
    pub goal: Vec<Pddl8GroundAtom>,
    /// All materialized (reachable) ground actions, indexed by position.
    pub actions: Vec<Pddl8GroundAction>,
    action_index: HashMap<Pddl8GroundAtom, Vec<usize>>,
    always_applicable: Vec<usize>,
    stats: GroundStats,
}

/// A partial parameter binding: `param index → object ID` (or unbound).
type Binding = Vec<Option<u32>>;

impl IndexedGroundProblem {
    /// Build the indexed grounding of `problem` under `domain`.
    ///
    /// `max_ground` bounds the number of *materialized* (reachable) ground
    /// actions; `None` uses [`PDDL8_MAX_GROUND`]. Note this bounds the pruned
    /// set, so a domain whose naive product blows past the ceiling can still be
    /// grounded here as long as its reachable subset fits.
    pub fn build(
        domain: &Pddl8Domain, problem: &Pddl8Problem, max_ground: Option<usize>,
    ) -> Result<Self, crate::error::Pddl8Error> {
        let limit = max_ground.unwrap_or(PDDL8_MAX_GROUND);
        let mut dict = Dict::new();

        // Pre-intern every symbol the fixpoint/join will reference, so `dict`
        // is immutable (only `get`) for the rest of the build.
        for o in &problem.objects {
            dict.intern(o);
        }
        for atom in problem.init.iter().chain(problem.goal.iter()) {
            intern_atom(&mut dict, atom);
        }
        for schema in &domain.actions {
            for atom in schema
                .preconditions
                .iter()
                .chain(schema.add_effects.iter())
                .chain(schema.del_effects.iter())
            {
                intern_atom(&mut dict, atom);
            }
        }

        let type_index = TypeIndex::build(domain, problem);
        let plans: Vec<SchemaPlan> = domain
            .actions
            .iter()
            .map(|s| SchemaPlan::build(s, &problem.objects, &type_index, &mut dict))
            .collect();

        // Seed R with the initial facts.
        let mut reachable = FactStore::new();
        for atom in &problem.init {
            insert_ground(&mut reachable, &dict, &atom.pred, &atom.args);
        }

        // Relaxed-reachability fixpoint (delete-relaxed: only add effects).
        // The fixpoint join resolves open atoms by index scan and closed atoms
        // by exact `contains`; it does not need the XOR filter (which only
        // accelerates closed probes and is rebuilt once, after the fixpoint,
        // for the materialization pass). Freezing per iteration would rebuild
        // the filter O(iterations) times for no benefit.
        loop {
            let mut new_facts: Vec<(SymId, Vec<SymId>)> = Vec::new();
            for plan in &plans {
                for binding in join_bindings(plan, &dict, &reachable) {
                    for eff in &plan.schema.add_effects {
                        if let Some(fact) = ground_atom_ids(eff, &binding, plan, &dict) {
                            new_facts.push(fact);
                        }
                    }
                }
            }
            let mut changed = false;
            for (pred, args) in new_facts {
                if reachable.insert(pred, &args) {
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        reachable.freeze();

        // Final materialization pass: emit reachable ground actions per schema,
        // in naive odometer order.
        let candidate_groundings: usize = plans.iter().map(SchemaPlan::candidate_count).sum();
        let mut actions: Vec<Pddl8GroundAction> = Vec::new();
        for plan in &plans {
            let mut seen: HashSet<Vec<u32>> = HashSet::new();
            let mut keyed: Vec<(Vec<usize>, Pddl8GroundAction)> = Vec::new();
            for binding in join_bindings(plan, &dict, &reachable) {
                let ids: Vec<u32> = binding.clone();
                if !seen.insert(ids.clone()) {
                    continue; // guard against a degenerate double-match
                }
                let key: Option<Vec<usize>> = plan
                    .schema
                    .params
                    .iter()
                    .enumerate()
                    .map(|(p, _)| plan.position_of(p, ids[p]))
                    .collect();
                let Some(key) = key else { continue };
                keyed.push((key, instantiate(plan, &binding, &dict)));
            }
            keyed.sort_by(|a, b| a.0.cmp(&b.0));
            for (_, ga) in keyed {
                actions.push(ga);
                if actions.len() > limit {
                    return Err(crate::error::Pddl8Error::BoundExceeded {
                        what: "ground actions",
                        limit,
                        got: actions.len(),
                    });
                }
            }
        }

        if actions.is_empty() {
            return Err(crate::error::Pddl8Error::EmptyGrounding);
        }

        let mut action_index: HashMap<Pddl8GroundAtom, Vec<usize>> = HashMap::new();
        let mut always_applicable: Vec<usize> = Vec::new();
        for (i, action) in actions.iter().enumerate() {
            if action.preconditions.is_empty() {
                always_applicable.push(i);
            }
            for p in &action.preconditions {
                action_index.entry(p.clone()).or_default().push(i);
            }
        }

        let initial_state: BTreeSet<Pddl8GroundAtom> = problem
            .init
            .iter()
            .map(|a| Pddl8GroundAtom {
                pred: a.pred.clone(),
                args: a.args.clone(),
            })
            .collect();
        let goal: Vec<Pddl8GroundAtom> = problem
            .goal
            .iter()
            .map(|a| Pddl8GroundAtom {
                pred: a.pred.clone(),
                args: a.args.clone(),
            })
            .collect();

        let stats = GroundStats {
            candidate_groundings,
            materialized_groundings: actions.len(),
            reachable_atoms: reachable.len(),
        };

        Ok(Self {
            initial_state,
            goal,
            actions,
            action_index,
            always_applicable,
            stats,
        })
    }

    /// BFS forward search over the pruned action set.
    ///
    /// # Bounded vs. Exhausted
    ///
    /// Same "bounded != exhausted" obligation as
    /// `crate::GroundProblem::find_plan` (see that function's doc comment):
    /// `PDDL8_MAX_PLAN_DEPTH` caps how deep any single BFS branch may be
    /// expanded, and this search tracks (`depth_bound_hit`) whether any
    /// branch was cut off *solely* for exceeding that cap rather than the
    /// frontier genuinely running dry. If so, an empty queue does not prove
    /// unreachability — this returns `Bounded`, not `Exhausted`.
    ///
    /// (This doc comment used to claim "identical strategy to
    /// `crate::GroundProblem::find_plan`" — true of the join/prune
    /// grounding strategy, but that sibling's `find_plan` was patched in
    /// commit f736dca5 to make exactly this Bounded/Exhausted distinction
    /// and this function was not; both now agree, closing that gap rather
    /// than continuing to assert an equivalence this function did not
    /// actually have.)
    pub fn find_plan(&self) -> PlannerOutcome<Pddl8Tape> {
        let goal_set: BTreeSet<Pddl8GroundAtom> = self.goal.iter().cloned().collect();
        let mut queue: VecDeque<(BTreeSet<Pddl8GroundAtom>, Vec<usize>)> = VecDeque::new();
        let mut visited: HashSet<Vec<Pddl8GroundAtom>> = HashSet::new();

        let init_sorted: Vec<Pddl8GroundAtom> = self.initial_state.iter().cloned().collect();
        visited.insert(init_sorted);
        queue.push_back((self.initial_state.clone(), vec![]));

        // Set the moment any branch is discarded purely for exceeding
        // `PDDL8_MAX_PLAN_DEPTH` — see the doc comment above.
        let mut depth_bound_hit = false;
        let mut max_depth_observed: u64 = 0;

        while let Some((state, path)) = queue.pop_front() {
            max_depth_observed = max_depth_observed.max(path.len() as u64);
            if path.len() > PDDL8_MAX_PLAN_DEPTH {
                depth_bound_hit = true;
                continue;
            }
            if goal_set.iter().all(|g| state.contains(g)) {
                let plan: Vec<Pddl8GroundAction> =
                    path.into_iter().map(|i| self.actions[i].clone()).collect();
                return PlannerOutcome::Found(Pddl8Tape::from_plan(plan));
            }
            let mut candidates: BTreeSet<usize> = self.always_applicable.iter().copied().collect();
            for atom in state.iter() {
                if let Some(idxs) = self.action_index.get(atom) {
                    candidates.extend(idxs.iter().copied());
                }
            }
            for i in candidates {
                let action = &self.actions[i];
                if action.preconditions.iter().all(|p| state.contains(p)) {
                    let mut next = state.clone();
                    for d in &action.del_effects {
                        next.remove(d);
                    }
                    for a in &action.add_effects {
                        next.insert(a.clone());
                    }
                    let sorted: Vec<Pddl8GroundAtom> = next.iter().cloned().collect();
                    if !visited.contains(&sorted) {
                        visited.insert(sorted);
                        let mut p2 = path.clone();
                        p2.push(i);
                        queue.push_back((next, p2));
                    }
                }
            }
        }

        if depth_bound_hit {
            // At least one branch was cut off solely by the depth cap, not
            // by the frontier genuinely running dry — a structural bound
            // was hit, not a proof of unreachability. See the doc comment
            // on this function.
            return PlannerOutcome::Bounded(BoundHit {
                kind: BoundKind::PlanDepth,
                limit: PDDL8_MAX_PLAN_DEPTH as u64,
                observed: max_depth_observed,
            });
        }

        let mut goal_digest_buf = Vec::new();
        for g in &self.goal {
            goal_digest_buf.extend_from_slice(g.label().as_bytes());
            goal_digest_buf.push(0);
        }
        goal_digest_buf.extend_from_slice(&(self.actions.len() as u64).to_le_bytes());
        PlannerOutcome::Exhausted(ExhaustionWitness {
            search_profile: INDEXED_BFS_SEARCH_PROFILE,
            explored_states: visited.len() as u64,
            frontier_empty: true,
            digest: Digest::hash(&goal_digest_buf),
        })
    }

    /// Reachability statistics for this grounding.
    #[must_use]
    pub fn stats(&self) -> GroundStats {
        self.stats
    }

    /// The materialized ground actions (reachable subset, in naive order).
    #[must_use]
    pub fn actions(&self) -> &[Pddl8GroundAction] {
        &self.actions
    }
}

// ── join engine ────────────────────────────────────────────────────────────

/// Enumerate the full parameter bindings for `plan` whose every precondition
/// atom holds in `reachable`, type-filtered to candidate objects.
///
/// Preconditions are processed in order: an atom introducing at least one
/// unbound variable is resolved by scanning that predicate's sorted fact list
/// (the join's index scan); a fully-determined atom is settled by a single
/// XOR-filter-gated membership probe (the join's semijoin prune). Parameters
/// that never appear in a precondition are expanded over their candidate lists
/// at the end — matching the naive grounder's treatment of effect-only params.
fn join_bindings(plan: &SchemaPlan, dict: &Dict, reachable: &FactStore) -> Vec<Vec<u32>> {
    let n = plan.schema.params.len();
    let mut partials: Vec<Binding> = vec![vec![None; n]];

    for atom in &plan.schema.preconditions {
        if partials.is_empty() {
            break;
        }
        let Some(pred_id) = dict.get(&atom.pred) else {
            return Vec::new(); // predicate never appears anywhere ⇒ no facts.
        };
        let mut next: Vec<Binding> = Vec::new();
        for b in &partials {
            if atom_is_closed(atom, b, &plan.param_index) {
                // Semijoin prune: fully determined ⇒ one filter-gated probe.
                if let Some(ids) = close_atom(atom, b, &plan.param_index, dict) {
                    let id_syms: Vec<SymId> = ids.iter().map(|&i| SymId(i)).collect();
                    if reachable.contains(pred_id, &id_syms) {
                        next.push(b.clone());
                    }
                }
            } else {
                // Index scan: extend the binding over matching facts.
                for tuple in reachable.tuples_for(pred_id) {
                    if tuple.len() != atom.args.len() {
                        continue;
                    }
                    if let Some(nb) = try_extend(b, atom, tuple, plan, dict) {
                        next.push(nb);
                    }
                }
            }
        }
        partials = next;
    }

    // Expand any parameter still unbound (effect-only params) over its
    // candidate list, then drop the `Option`.
    let mut full: Vec<Vec<u32>> = Vec::new();
    for b in partials {
        expand_unbound(&b, plan, &mut full);
    }
    full
}

/// Whether `atom` has no unbound variables under partial binding `b`.
fn atom_is_closed(atom: &Pddl8Atom, b: &Binding, param_index: &HashMap<String, usize>) -> bool {
    atom.args.iter().all(|arg| {
        if Pddl8Atom::is_variable(arg) {
            param_index.get(arg).is_some_and(|&pi| b[pi].is_some())
        } else {
            true
        }
    })
}

/// Resolve a closed atom's argument IDs, or `None` if a constant is unknown.
fn close_atom(
    atom: &Pddl8Atom, b: &Binding, param_index: &HashMap<String, usize>, dict: &Dict,
) -> Option<Vec<u32>> {
    atom.args
        .iter()
        .map(|arg| {
            if Pddl8Atom::is_variable(arg) {
                param_index.get(arg).and_then(|&pi| b[pi])
            } else {
                dict.get(arg).map(|s| s.0)
            }
        })
        .collect()
}

/// Extend `b` so `atom` matches `tuple`, or `None` on any conflict / off-type
/// binding.
fn try_extend(
    b: &Binding, atom: &Pddl8Atom, tuple: &[u32], plan: &SchemaPlan, dict: &Dict,
) -> Option<Binding> {
    let mut nb = b.clone();
    for (j, arg) in atom.args.iter().enumerate() {
        let fid = tuple[j];
        if Pddl8Atom::is_variable(arg) {
            let pi = *plan.param_index.get(arg)?;
            match nb[pi] {
                Some(x) if x != fid => return None,
                Some(_) => {}
                None => {
                    if !plan.is_candidate(pi, fid) {
                        return None; // type-incompatible: naive never emits it.
                    }
                    nb[pi] = Some(fid);
                }
            }
        } else if dict.get(arg).map(|s| s.0) != Some(fid) {
            return None; // constant mismatch.
        }
    }
    Some(nb)
}

/// Fill every still-`None` parameter of `b` over its candidate list, pushing
/// each complete binding (as IDs) to `out`.
fn expand_unbound(b: &Binding, plan: &SchemaPlan, out: &mut Vec<Vec<u32>>) {
    match b.iter().position(Option::is_none) {
        None => out.push(b.iter().map(|o| o.unwrap()).collect()),
        Some(pi) => {
            for &cand in &plan.cand_ids[pi] {
                let mut nb = b.clone();
                nb[pi] = Some(cand);
                expand_unbound(&nb, plan, out);
            }
        }
    }
}

// ── instantiation ────────────────────────────────────────────────────────────

/// Build a ground atom's `(pred_id, arg_ids)` from a complete `binding`, or
/// `None` if a constant is unknown.
fn ground_atom_ids(
    atom: &Pddl8Atom, binding: &[u32], plan: &SchemaPlan, dict: &Dict,
) -> Option<(SymId, Vec<SymId>)> {
    let pred = dict.get(&atom.pred)?;
    let args: Option<Vec<SymId>> = atom
        .args
        .iter()
        .map(|arg| {
            if Pddl8Atom::is_variable(arg) {
                plan.param_index.get(arg).map(|&pi| SymId(binding[pi]))
            } else {
                dict.get(arg)
            }
        })
        .collect();
    args.map(|a| (pred, a))
}

/// Materialize a `Pddl8GroundAction` from a complete `binding` — byte-identical
/// in shape and label to `crate`'s `instantiate`.
fn instantiate(plan: &SchemaPlan, binding: &[u32], dict: &Dict) -> Pddl8GroundAction {
    let ground = |atom: &Pddl8Atom| -> Pddl8GroundAtom {
        let args: Vec<String> = atom
            .args
            .iter()
            .map(|arg| {
                if Pddl8Atom::is_variable(arg) {
                    let pi = plan.param_index[arg];
                    dict.resolve(SymId(binding[pi])).to_owned()
                } else {
                    arg.clone()
                }
            })
            .collect();
        Pddl8GroundAtom {
            pred: atom.pred.clone(),
            args,
        }
    };

    let bound_args: Vec<String> = binding
        .iter()
        .map(|&id| dict.resolve(SymId(id)).to_owned())
        .collect();
    let label = if bound_args.is_empty() {
        plan.schema.name.clone()
    } else {
        format!("{}({})", plan.schema.name, bound_args.join(","))
    };
    Pddl8GroundAction {
        schema_name: plan.schema.name.clone(),
        label,
        preconditions: plan.schema.preconditions.iter().map(&ground).collect(),
        add_effects: plan.schema.add_effects.iter().map(&ground).collect(),
        del_effects: plan.schema.del_effects.iter().map(&ground).collect(),
    }
}

// ── interning helpers ──────────────────────────────────────────────────────

fn intern_atom(dict: &mut Dict, atom: &Pddl8Atom) {
    dict.intern(&atom.pred);
    for arg in &atom.args {
        if !Pddl8Atom::is_variable(arg) {
            dict.intern(arg);
        }
    }
}

/// Insert a ground atom given by string predicate + string args (init facts).
fn insert_ground(store: &mut FactStore, dict: &Dict, pred: &str, args: &[String]) {
    let Some(pred_id) = dict.get(pred) else {
        return;
    };
    let arg_ids: Option<Vec<SymId>> = args.iter().map(|a| dict.get(a)).collect();
    if let Some(ids) = arg_ids {
        store.insert(pred_id, &ids);
    }
}
