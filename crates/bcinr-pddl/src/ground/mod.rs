mod dict;
mod facts;
pub mod lazy;
mod xorf;

// PDDL8 grounding and forward-search plan finding.

use crate::error::Pddl8Error;
use bcinr_mfw_ir::{
    BoundHit, BoundKind, Digest, ExhaustionWitness, PlannerOutcome, SearchProfileId,
};
use std::collections::{BTreeSet, HashMap};
use wasm4pm_compat::pddl::{
    CompareOp, DerivedPredicate, DurationConstraint, DurativeAction, NumericExpr,
    Pddl8ActionSchema, Pddl8Atom, Pddl8Domain, Pddl8GroundAction, Pddl8GroundAtom, Pddl8Problem,
    Pddl8Tape, PddlCondition, PddlEffect, PddlFunction, TemporalPlan, TemporalPlanStep,
    TimedLiteral, PDDL8_MAX_GROUND, PDDL8_MAX_PLAN_DEPTH,
};

/// `SearchProfileId` for the plain, un-portfolio'd whole-run BFS/greedy
/// search `GroundProblem`/`GroundTemporalProblem` run directly (as opposed
/// to a rail inside `crate::search`'s `MfwPortfolio`, which assigns its own
/// profile ids). `0` is a sentinel, not a real profile registry entry — this
/// module predates the portfolio/rail split and has exactly one search
/// strategy per problem type, so there is nothing to distinguish it from.
const LEGACY_WHOLE_RUN_SEARCH_PROFILE: SearchProfileId = SearchProfileId(0);

/// A cheap, deterministic digest summarizing what was searched — not a
/// formally meaningful proof digest, just enough to let two `ExhaustionWitness`
/// values be compared/deduplicated. Hashes the goal atom labels and the
/// ground action count.
fn search_digest(goal_labels: &[String], action_count: usize) -> Digest {
    let mut buf = Vec::new();
    for l in goal_labels {
        buf.extend_from_slice(l.as_bytes());
        buf.push(0);
    }
    buf.extend_from_slice(&(action_count as u64).to_le_bytes());
    Digest::hash(&buf)
}

pub struct GroundProblem {
    pub initial_state: BTreeSet<Pddl8GroundAtom>,
    pub goal: Vec<Pddl8GroundAtom>,
    pub actions: Vec<Pddl8GroundAction>,
    /// precondition atom -> indices of actions that require it. Lets
    /// `find_plan`'s BFS only consider actions that could possibly apply at
    /// a given state instead of linearly scanning every ground action.
    action_index: HashMap<Pddl8GroundAtom, Vec<usize>>,
    /// Indices of actions with no preconditions — always candidates.
    always_applicable: Vec<usize>,
    pub constraints: Vec<PddlCondition>,
    pub derived_predicates: Vec<GroundDerivedPredicate>,
    /// Object universe + type index quantified `Forall`/`Exists` conditions
    /// range over. See `eval_quantifier`.
    quant_domain: QuantifierDomain,
}

#[derive(Clone)]
pub struct GroundDerivedPredicate {
    pub head: Pddl8GroundAtom,
    pub condition: PddlCondition,
}

/// Object/type lookup used to restrict grounding to type-compatible bindings.
///
/// Built once per `GroundProblem`/`GroundTemporalProblem`. A parameter with no
/// entry in `typed_params`, or a required type of `"object"` (PDDL's
/// universal type), matches every object — this is what keeps untyped/legacy
/// domains behaving exactly as before.
#[derive(Clone)]
struct TypeIndex {
    /// object name -> declared type (objects absent from `object_types` are
    /// treated as type `"object"`, matching untyped-domain semantics).
    object_type: HashMap<String, String>,
    /// type name -> parent type name, for `(:types child - parent)` subtyping.
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

    /// Does `obj`'s actual (or inherited) type satisfy `required`?
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

/// The object universe + type index that a `Forall`/`Exists`-quantified
/// condition's variables range over. Built once per `GroundProblem`/
/// `GroundTemporalProblem::build` (same `TypeIndex` already used to
/// type-filter action-schema parameter candidates) and threaded through
/// every `eval_condition` call for that problem.
///
/// This is what `eval_condition` needs, and previously did not have, to
/// evaluate `Forall`/`Exists` for real instead of returning the hardcoded
/// `true`/`false` stub — see `eval_quantifier`.
#[derive(Clone)]
pub(crate) struct QuantifierDomain {
    objects: Vec<String>,
    type_index: TypeIndex,
}

impl QuantifierDomain {
    /// Candidate object names for one quantified variable's declared type.
    /// `"object"` (PDDL's universal type) matches every object, identically
    /// to `ground_schema`'s per-parameter candidate lists and
    /// `TypeIndex::satisfies`'s own `"object"` short-circuit.
    fn candidates(&self, required_type: &str) -> Vec<&String> {
        self.objects
            .iter()
            .filter(|o| self.type_index.satisfies(o, required_type))
            .collect()
    }
}

impl GroundProblem {
    pub fn build(
        domain: &Pddl8Domain, problem: &Pddl8Problem, max_ground: Option<usize>,
    ) -> Result<Self, Pddl8Error> {
        let limit = max_ground.unwrap_or(PDDL8_MAX_GROUND);

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

        let objects = &problem.objects;
        let type_index = TypeIndex::build(domain, problem);
        let mut actions = Vec::new();

        for schema in &domain.actions {
            ground_schema(schema, objects, &type_index, &mut actions)?;
            if actions.len() > limit {
                return Err(Pddl8Error::BoundExceeded {
                    what: "ground actions",
                    limit,
                    got: actions.len(),
                });
            }
        }

        if actions.is_empty() {
            return Err(Pddl8Error::EmptyGrounding);
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

        let mut constraints = Vec::new();
        for pref in &problem.preferences {
            if let wasm4pm_compat::pddl::TrajectoryConstraint::Always(c) = &pref.constraint {
                constraints.push(*c.clone());
            } else if let wasm4pm_compat::pddl::TrajectoryConstraint::And(parts) = &pref.constraint
            {
                for p in parts {
                    if let wasm4pm_compat::pddl::TrajectoryConstraint::Always(c) = p {
                        constraints.push(*c.clone());
                    }
                }
            }
        }
        let mut derived_predicates = Vec::new();
        for dp in &domain.derived {
            ground_derived_schema(
                dp,
                &problem.object_types,
                &type_index,
                &mut derived_predicates,
            )?;
        }

        let quant_domain = QuantifierDomain {
            objects: problem.objects.clone(),
            type_index,
        };

        Ok(Self {
            initial_state,
            goal,
            actions,
            action_index,
            always_applicable,
            constraints,
            derived_predicates,
            quant_domain,
        })
    }

    /// BFS forward search — returns a `Pddl8Tape` ready for execution.
    ///
    /// # Bounded vs. Exhausted
    ///
    /// `PDDL8_MAX_PLAN_DEPTH` caps how deep any single BFS branch may be
    /// expanded. If the queue empties out *without* ever discarding a
    /// branch solely for exceeding that depth cap, every reachable state
    /// really was visited and `Exhausted` is a genuine proof of
    /// unreachability. But if at least one branch was cut off only because
    /// it hit the depth cap, the state space below that cutoff was never
    /// explored — the goal could still be reachable via a longer plan, so
    /// claiming `Exhausted` would be a false proof. This mirrors the fix
    /// already applied to the sibling `find_temporal_plan_with_fn_overrides`
    /// (same file): see `crates/bcinr-pddl/tests/depth_bound_exhaustion_conflation.rs`
    /// for an adversarial regression fixture (a 70-step chain domain whose
    /// only plan exceeds the 64-step depth cap) that pins this down.
    ///
    /// # Complexity
    ///
    /// This is uninformed forward BFS with **no heuristic** — every reached
    /// state's *entire* fact set is inserted into a `HashSet` for dedup, and
    /// every candidate action from that state is expanded before the queue
    /// advances. There is no polynomial guarantee: in the worst case the
    /// reachable-state space is exponential in the grounded-action count
    /// (`self.actions.len()`), since each of up to `A` applicable actions
    /// can branch at every one of up to `PDDL8_MAX_PLAN_DEPTH` levels before
    /// the depth cap or dedup catches it — i.e. worst-case `O(A ^
    /// PDDL8_MAX_PLAN_DEPTH)` states visited, each doing `O(A * k)` work
    /// (candidate filtering + per-action apply, `k` = average
    /// precondition/effect-list size) plus a `state.len()`-sized clone for
    /// the visited-set key. The only pruning is the depth cap
    /// (`PDDL8_MAX_PLAN_DEPTH`) and the full-state-equality dedup — neither
    /// bounds the branching factor itself. This is the "exact" half of the
    /// exact/exploit rail pair `crate::search::ExactBfsRail::step` wraps
    /// verbatim (see that type's own doc comment): `FairRailScheduler`'s
    /// fairness invariant guarantees an `ExactBfsRail::step` call — and
    /// therefore this function — fires at least once every `max_gap`
    /// scheduler selections, so this cost is incurred on a real, recurring
    /// hot path, not a one-time setup cost. Contrast with the exploit rail's
    /// [`crate::search::QLensRail`]'s `step` (via `ExploitSearchRail`), which
    /// is `O(A * k)` per step by construction (bounded branching, no
    /// backtracking) — this function has
    /// no such bound. `bcinr-bench/benches/mfw_hotpath_bench.rs` benchmarks
    /// `EventSet` ops, `concurrency_admits`, and `q_lens`, but does not
    /// benchmark `find_plan`/`ExactBfsRail::step`, so this cost is
    /// documented here but not yet measured anywhere in the retrofit.
    pub fn find_plan(&self) -> PlannerOutcome<Pddl8Tape> {
        use std::collections::VecDeque;

        let goal_set: BTreeSet<Pddl8GroundAtom> = self.goal.iter().cloned().collect();
        let mut queue: VecDeque<(BTreeSet<Pddl8GroundAtom>, Vec<usize>)> = VecDeque::new();
        let mut visited: std::collections::HashSet<Vec<Pddl8GroundAtom>> = Default::default();

        let init_sorted: Vec<Pddl8GroundAtom> = self.initial_state.iter().cloned().collect();
        visited.insert(init_sorted);
        queue.push_back((self.initial_state.clone(), vec![]));

        // Set the moment any branch is discarded purely for exceeding
        // `PDDL8_MAX_PLAN_DEPTH` (never for constraint violation or
        // already-visited dedup). Once true, an empty queue no longer
        // proves unreachability — see the doc comment above.
        let mut depth_bound_hit = false;
        let mut max_depth_observed: u64 = 0;

        while let Some((mut state, path)) = queue.pop_front() {
            compute_derived_closure(
                &mut state,
                &self.derived_predicates,
                &HashMap::new(),
                &self.quant_domain,
            );
            max_depth_observed = max_depth_observed.max(path.len() as u64);
            if path.len() > PDDL8_MAX_PLAN_DEPTH {
                depth_bound_hit = true;
                continue;
            }
            if self
                .constraints
                .iter()
                .any(|c| !eval_condition(c, &state, &HashMap::new(), &self.quant_domain))
            {
                continue;
            }
            if goal_set.iter().all(|g| state.contains(g)) {
                let plan: Vec<Pddl8GroundAction> =
                    path.into_iter().map(|i| self.actions[i].clone()).collect();
                return PlannerOutcome::Found(Pddl8Tape::from_plan(plan));
            }
            // Only consider actions that could possibly apply: always-applicable
            // (no preconditions) plus those keyed by an atom currently true.
            // Full precondition check below still runs per candidate — this just
            // avoids scanning the whole action list at every BFS node.
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
            // was hit, not a proof of unreachability.
            return PlannerOutcome::Bounded(BoundHit {
                kind: BoundKind::PlanDepth,
                limit: PDDL8_MAX_PLAN_DEPTH as u64,
                observed: max_depth_observed,
            });
        }

        let goal_labels: Vec<String> = self.goal.iter().map(Pddl8GroundAtom::label).collect();
        PlannerOutcome::Exhausted(ExhaustionWitness {
            search_profile: LEGACY_WHOLE_RUN_SEARCH_PROFILE,
            explored_states: visited.len() as u64,
            frontier_empty: true,
            digest: search_digest(&goal_labels, self.actions.len()),
        })
    }
}

// ---------------------------------------------------------------------------
// PDDL 3.1 temporal grounding
// ---------------------------------------------------------------------------

/// A grounded durative action — duration bounds resolved, conditions/effects kept.
#[derive(Clone)]
pub struct GroundDurativeAction {
    pub schema_name: String,
    pub label: String,
    /// Bound object names, in schema parameter order (empty for zero-param schemas).
    pub args: Vec<String>,
    pub duration_min: f64,
    pub duration_max: f64,
    pub conditions: Vec<PddlCondition>,
    pub effects: Vec<PddlEffect>,
}

/// A grounded temporal planning problem containing classical and durative actions,
/// numeric fluents, timed initial literals, and goal conditions.
///
/// `GroundTemporalProblem` is the primary evaluation structure for solving temporal planning
/// tasks. It represents the propositional grounding of a PDDL domain and problem.
///
/// # Domain Description & Modeling
///
/// In first-order planning (PDDL), problems are described in terms of predicates, functions,
/// objects, and parameterized schemas. To plan or execute actions, they must be "grounded"—
/// i.e., parameterized schemas are expanded into concrete propositions by substituting variables
/// with objects from the domain.
///
/// A grounded temporal problem tracks:
/// - **Initial State**: A set of grounded boolean atoms (`initial_atoms`) and numeric function values (`initial_fn_values`) representing the world at time $t = 0$.
/// - **Durative Actions**: Actions that span an interval of time, with conditions checked and effects applied at the start, throughout, or at the end of execution.
/// - **Timed Initial Literals**: State changes or event boundaries pre-scheduled to occur at specific times (e.g., resource availability changes).
/// - **Goals & Constraints**: Target state conditions (`goal`) and trajectory rules (`constraints`) that must hold over the plan execution.
///
/// # Complexity Analysis
///
/// - **Grounding Time/Space**: Grounding involves substituting variables in action schemas with type-compatible objects.
///   For an action schema with $P$ parameters and a universe of $O$ objects, the number of potential grounded actions
///   is $O(O^P)$. If $A$ is the set of action schemas, the overall complexity of grounding is:
///   $$O(\sum_{a \in A} |O|^{arity(a)})$$
///   To safeguard against state-space explosion, the grounding engine enforces a hard limit of `PDDL8_MAX_GROUND`
///   (65,536) grounded actions. If exceeded, a [`Pddl8Error::BoundExceeded`] is returned.
///
/// # Examples
///
/// ```
/// use bcinr_pddl::{domain_from_pddl, problem_from_pddl, GroundTemporalProblem, PlannerOutcome};
///
/// let domain_pddl = r#"
/// (define (domain simple-temporal)
///   (:requirements :durative-actions :numeric-fluents)
///   (:functions (attention))
///   (:durative-action act
///     :parameters ()
///     :duration (= ?duration 2)
///     :condition (and (at start (>= (attention) 1)))
///     :effect (and
///       (at start (decrease (attention) 1))
///       (at end (increase (attention) 1)))))
/// "#;
///
/// let problem_pddl = r#"
/// (define (problem simple-prob)
///   (:domain simple-temporal)
///   (:init (= (attention) 1))
///   (:goal (and)))
/// "#;
///
/// let domain = domain_from_pddl(domain_pddl).unwrap();
/// let problem = problem_from_pddl(problem_pddl).unwrap();
///
/// let gtp = GroundTemporalProblem::build(&domain, &problem).unwrap();
/// let outcome = gtp.find_temporal_plan();
/// assert!(matches!(outcome, PlannerOutcome::Found(_)));
/// ```
#[derive(Clone)]
pub struct GroundTemporalProblem {
    pub initial_atoms: BTreeSet<Pddl8GroundAtom>,
    pub initial_fn_values: HashMap<String, f64>,
    pub timed_inits: Vec<TimedLiteral>,
    pub goal: PddlCondition,
    pub actions: Vec<Pddl8GroundAction>,
    pub durative_actions: Vec<GroundDurativeAction>,
    pub constraints: Vec<PddlCondition>,
    pub derived_predicates: Vec<GroundDerivedPredicate>,
    /// Object universe + type index quantified `Forall`/`Exists` conditions
    /// range over. See `eval_quantifier`.
    quant_domain: QuantifierDomain,
}

impl GroundTemporalProblem {
    /// Builds a new `GroundTemporalProblem` by grounding action schemas and durative schemas
    /// over the object universe defined in the problem.
    ///
    /// Returns a [`Pddl8Error::BoundExceeded`] if the number of grounded actions or durative actions
    /// exceeds `PDDL8_MAX_GROUND` (65,536).
    ///
    /// # Complexity
    ///
    /// Grounding complexity is $O(|A| \cdot |O|^P)$ where $|A|$ is the number of schemas, $|O|$ is the
    /// number of objects, and $P$ is the maximum schema arity.
    pub fn build(domain: &Pddl8Domain, problem: &Pddl8Problem) -> Result<Self, Pddl8Error> {
        let initial_atoms: BTreeSet<Pddl8GroundAtom> = problem
            .init
            .iter()
            .map(|a| Pddl8GroundAtom {
                pred: a.pred.clone(),
                args: a.args.clone(),
            })
            .collect();

        let initial_fn_values: HashMap<String, f64> = problem
            .fn_values
            .iter()
            .map(|(f, v)| (fn_key(f), *v))
            .collect();

        let timed_inits = problem.timed_inits.clone();

        let goal = PddlCondition::And(
            problem
                .goal
                .iter()
                .map(|a| PddlCondition::Atom(a.clone()))
                .collect(),
        );

        // Ground classical actions
        let type_index = TypeIndex::build(domain, problem);
        let mut actions = Vec::new();
        for schema in &domain.actions {
            ground_schema(schema, &problem.objects, &type_index, &mut actions)?;
            if actions.len() > PDDL8_MAX_GROUND {
                return Err(Pddl8Error::BoundExceeded {
                    what: "ground actions",
                    limit: PDDL8_MAX_GROUND,
                    got: actions.len(),
                });
            }
        }

        // Ground durative actions over objects, mirroring `ground_schema` for
        // classical actions: enumerate type-compatible bindings for each
        // schema's params and substitute the bound object names into the
        // schema's conditions/effects.
        let mut durative_actions = Vec::new();
        for da in &domain.durative_actions {
            ground_durative_schema(da, &problem.objects, &type_index, &mut durative_actions)?;
            if durative_actions.len() > PDDL8_MAX_GROUND {
                return Err(Pddl8Error::BoundExceeded {
                    what: "ground durative actions",
                    limit: PDDL8_MAX_GROUND,
                    got: durative_actions.len(),
                });
            }
        }

        let mut constraints = Vec::new();
        for pref in &problem.preferences {
            if let wasm4pm_compat::pddl::TrajectoryConstraint::Always(c) = &pref.constraint {
                constraints.push(*c.clone());
            } else if let wasm4pm_compat::pddl::TrajectoryConstraint::And(parts) = &pref.constraint
            {
                for p in parts {
                    if let wasm4pm_compat::pddl::TrajectoryConstraint::Always(c) = p {
                        constraints.push(*c.clone());
                    }
                }
            }
        }
        let mut derived_predicates = Vec::new();
        for dp in &domain.derived {
            ground_derived_schema(
                dp,
                &problem.object_types,
                &type_index,
                &mut derived_predicates,
            )?;
        }

        let quant_domain = QuantifierDomain {
            objects: problem.objects.clone(),
            type_index,
        };

        Ok(Self {
            initial_atoms,
            initial_fn_values,
            timed_inits,
            goal,
            actions,
            durative_actions,
            constraints,
            derived_predicates,
            quant_domain,
        })
    }

    /// Executes a forward-chaining temporal state-space search to find a valid `TemporalPlan`.
    ///
    /// The planner maintains a priority queue of scheduled events ordered by time.
    /// It greedily schedules actions that satisfy preconditions, applying start effects immediately
    /// and end effects upon completion.
    ///
    /// # Complexity
    ///
    /// The search is bounded by a maximum depth of `PDDL8_MAX_PLAN_DEPTH` iterations. At each step,
    /// selecting and applying actions requires evaluating preconditions over the current grounded state,
    /// resulting in a worst-case time complexity of $O(D \cdot G)$ where $D$ is the depth limit and $G$
    /// is the cost of evaluating grounded action preconditions.
    ///
    /// # Examples
    ///
    /// See the struct-level documentation for [`GroundTemporalProblem`] for an end-to-end example.
    pub fn find_temporal_plan(&self) -> PlannerOutcome<TemporalPlan> {
        self.find_temporal_plan_with_fn_overrides(&HashMap::new())
    }

    /// Same as `find_temporal_plan`, but with `overrides` merged into a
    /// cloned copy of `initial_fn_values` before planning starts — lets
    /// callers probe a perturbed numeric fluent (e.g. capacity sensitivity
    /// in `schedule_analysis::replan_with_perturbed_capacity`) without
    /// cloning the whole `GroundTemporalProblem` (grounded actions,
    /// conditions, atoms), just the small fn_values map.
    pub fn find_temporal_plan_with_fn_overrides(
        &self, overrides: &HashMap<String, f64>,
    ) -> PlannerOutcome<TemporalPlan> {
        let mut state = self.initial_atoms.clone();
        let mut fn_vals = self.initial_fn_values.clone();
        for (k, v) in overrides {
            fn_vals.insert(k.clone(), *v);
        }
        let mut steps: Vec<TemporalPlanStep> = Vec::new();
        let mut current_time = 0.0_f64;

        // Apply t=0 timed initial literals
        for til in &self.timed_inits {
            if til.time == 0.0 {
                let ga = Pddl8GroundAtom {
                    pred: til.atom.pred.clone(),
                    args: til.atom.args.clone(),
                };
                if til.negated {
                    state.remove(&ga);
                } else {
                    state.insert(ga);
                }
            }
        }

        // Pending completions: (end_time, action_idx)
        let mut pending: Vec<(f64, usize)> = Vec::new();

        // True once this planner's single greedy trajectory has died — either
        // no durative action could be scheduled and no future TIL/completion
        // event remains (`else if !scheduled` below), or a trajectory
        // constraint became false. Unlike `GroundProblem::find_plan`'s real
        // multi-path BFS frontier, this forward-chaining planner only ever
        // explores one trajectory, so either condition means that trajectory
        // is dead — that's what `ExhaustionWitness::frontier_empty` stands in
        // for below. Distinguishing this from simply running out of the
        // `PDDL8_MAX_PLAN_DEPTH` iteration budget (which returns `Bounded`,
        // not `Exhausted`) matters: the old code conflated both into the same
        // unit `Exhausted` variant, which silently claimed "search exhausted
        // its frontier" even when the loop was cut off mid-progress.
        let mut trajectory_dead = false;
        let mut iterations_run: u64 = 0;

        for _iteration in 0..PDDL8_MAX_PLAN_DEPTH {
            iterations_run += 1;
            // (TILs are now applied when time advances, not on every iteration)

            compute_derived_closure(
                &mut state,
                &self.derived_predicates,
                &fn_vals,
                &self.quant_domain,
            );
            if self
                .constraints
                .iter()
                .any(|c| !eval_condition(c, &state, &fn_vals, &self.quant_domain))
            {
                trajectory_dead = true;
                break;
            }
            // Check goal
            if eval_condition(&self.goal, &state, &fn_vals, &self.quant_domain) {
                let makespan = steps
                    .iter()
                    .map(|s| s.start_time + s.duration)
                    .fold(0.0_f64, f64::max);
                return PlannerOutcome::Found(TemporalPlan {
                    steps,
                    makespan,
                    metric_value: None,
                });
            }

            // Try to schedule every applicable durative action at this tick.
            // Re-scan after each start (bounded by durative_actions.len() passes)
            // so at-start effects (e.g. numeric capacity decrements) from one
            // start are visible when checking the next candidate's
            // preconditions in the same tick — this is what lets concurrent
            // starts correctly gate on shared resource fluents.
            let mut scheduled = false;
            let mut started_this_tick: BTreeSet<usize> = BTreeSet::new();
            for _pass in 0..self.durative_actions.len().max(1) {
                let mut started_this_pass = false;
                for (i, da) in self.durative_actions.iter().enumerate() {
                    if started_this_tick.contains(&i) {
                        continue;
                    }
                    // An action already in flight (started but not yet
                    // completed) must not be started again against itself —
                    // its own "already running" state isn't otherwise
                    // tracked for actions with no exclusive lock predicate
                    // (e.g. one that only consumes/releases a shared
                    // numeric fluent): only its *finished* effect blocks a
                    // restart, so without this guard the same grounded
                    // instance can be scheduled concurrently with itself.
                    if pending.iter().any(|(_, idx)| *idx == i) {
                        continue;
                    }
                    let applicable = da
                        .conditions
                        .iter()
                        .all(|c| eval_condition(c, &state, &fn_vals, &self.quant_domain));
                    if !applicable {
                        continue;
                    }

                    let dur = da.duration_min;
                    let end = current_time + dur;

                    // Apply at-start effects
                    for eff in &da.effects {
                        apply_effect_at_start(eff, &mut state, &mut fn_vals);
                    }

                    steps.push(TemporalPlanStep {
                        start_time: current_time,
                        duration: dur,
                        action_name: da.schema_name.clone(),
                        args: da.args.clone(),
                    });
                    pending.push((end, i));
                    scheduled = true;
                    started_this_pass = true;
                    started_this_tick.insert(i);
                }
                if !started_this_pass {
                    break;
                }
            }

            // Advance to the next event (completion or TIL)
            let next_completion = pending
                .iter()
                .enumerate()
                .min_by(|(_, a), (_, b)| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal))
                .map(|(p, (t, _))| (p, *t));

            let next_til_time = self
                .timed_inits
                .iter()
                .map(|til| til.time)
                .filter(|&t| t > current_time)
                .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

            let next_time = match (next_completion, next_til_time) {
                (Some((_, c_t)), Some(t_t)) => Some(c_t.min(t_t)),
                (Some((_, c_t)), None) => Some(c_t),
                (None, Some(t_t)) => Some(t_t),
                (None, None) => None,
            };

            if let Some(t_next) = next_time {
                let old_time = current_time;
                current_time = t_next;

                // Apply TILs that occur in (old_time, current_time]
                if current_time > old_time {
                    for til in &self.timed_inits {
                        if til.time > old_time && til.time <= current_time {
                            let ga = Pddl8GroundAtom {
                                pred: til.atom.pred.clone(),
                                args: til.atom.args.clone(),
                            };
                            if til.negated {
                                state.remove(&ga);
                            } else {
                                state.insert(ga);
                            }
                        }
                    }
                }

                // If the next event is a completion at `current_time`, process ONE completion.
                // Note: there could be multiple completions at `current_time`.
                // We pick the first one and remove it.
                if let Some(min_pos) = pending
                    .iter()
                    .enumerate()
                    .filter(|(_, (t, _))| *t == current_time)
                    .map(|(p, _)| p)
                    .next()
                {
                    let (_, idx) = pending.remove(min_pos);
                    let da = &self.durative_actions[idx];
                    for eff in &da.effects {
                        apply_effect_at_end(eff, &mut state, &mut fn_vals);
                    }
                }
            } else if !scheduled {
                trajectory_dead = true;
                break;
            }
        }

        // Final goal check after last completion
        if eval_condition(&self.goal, &state, &fn_vals, &self.quant_domain) {
            let makespan = steps
                .iter()
                .map(|s| s.start_time + s.duration)
                .fold(0.0_f64, f64::max);
            return PlannerOutcome::Found(TemporalPlan {
                steps,
                makespan,
                metric_value: None,
            });
        }

        let goal_labels = vec![format!("{:?}", self.goal)];
        if trajectory_dead {
            PlannerOutcome::Exhausted(ExhaustionWitness {
                search_profile: LEGACY_WHOLE_RUN_SEARCH_PROFILE,
                explored_states: steps.len() as u64,
                frontier_empty: true,
                digest: search_digest(
                    &goal_labels,
                    self.actions.len() + self.durative_actions.len(),
                ),
            })
        } else {
            // The loop ran through every iteration of the `PDDL8_MAX_PLAN_DEPTH`
            // budget without the trajectory dying and without reaching the
            // goal — a structural bound was hit, not a proof the trajectory
            // had nowhere left to go.
            PlannerOutcome::Bounded(BoundHit {
                kind: BoundKind::PlanDepth,
                limit: PDDL8_MAX_PLAN_DEPTH as u64,
                observed: iterations_run,
            })
        }
    }
}

/// Evaluate a `PddlCondition` against a ground state.
///
/// `quant_domain` supplies the object universe + type index that
/// `Forall`/`Exists` variables range over — see [`QuantifierDomain`] and
/// [`eval_quantifier`]. Narrowed from `pub` to `pub(crate)` in this phase:
/// nothing outside `bcinr-pddl` referenced this function (grepped), so
/// threading the new `quant_domain` parameter through is not a breaking
/// change to any real caller.
pub(crate) fn eval_condition(
    cond: &PddlCondition, state: &BTreeSet<Pddl8GroundAtom>, fn_vals: &HashMap<String, f64>,
    quant_domain: &QuantifierDomain,
) -> bool {
    match cond {
        PddlCondition::Atom(a) => state.contains(&Pddl8GroundAtom {
            pred: a.pred.clone(),
            args: a.args.clone(),
        }),
        PddlCondition::Not(inner) => !eval_condition(inner, state, fn_vals, quant_domain),
        PddlCondition::And(subs) => subs
            .iter()
            .all(|s| eval_condition(s, state, fn_vals, quant_domain)),
        PddlCondition::Or(subs) => subs
            .iter()
            .any(|s| eval_condition(s, state, fn_vals, quant_domain)),
        PddlCondition::Imply(lhs, rhs) => {
            !eval_condition(lhs, state, fn_vals, quant_domain)
                || eval_condition(rhs, state, fn_vals, quant_domain)
        }
        PddlCondition::Timed(_, inner) => eval_condition(inner, state, fn_vals, quant_domain),
        PddlCondition::Forall { vars, body } => {
            eval_quantifier(vars, body, state, fn_vals, quant_domain, true)
        }
        PddlCondition::Exists { vars, body } => {
            eval_quantifier(vars, body, state, fn_vals, quant_domain, false)
        }
        PddlCondition::Compare(lhs, op, rhs) => {
            let l = eval_numeric(lhs, fn_vals);
            let r = eval_numeric(rhs, fn_vals);
            match op {
                CompareOp::Ge => l >= r,
                CompareOp::Le => l <= r,
                CompareOp::Gt => l > r,
                CompareOp::Lt => l < r,
                CompareOp::Eq => (l - r).abs() < 1e-9,
            }
        }
    }
}

/// Evaluate a `Forall`/`Exists`-quantified condition over `quant_domain`'s
/// object universe, restricted per-variable by each `(var, type)` pair's
/// declared type — exactly like `ground_schema`'s per-parameter candidate
/// lists. `require_all = true` implements `Forall` (every binding's
/// substituted body must hold; vacuously **true** over an empty candidate
/// set, standard first-order semantics for a universally-quantified
/// statement over an empty domain); `require_all = false` implements
/// `Exists` (at least one binding's substituted body must hold; vacuously
/// **false** over an empty candidate set).
///
/// This replaces the previous hardcoded stub (`Forall => true`, `Exists =>
/// false`, unconditionally, regardless of the body) with real quantifier
/// evaluation: each quantified variable is bound, in turn, to every
/// type-compatible object via [`subst_condition`], and the substituted body
/// is recursively evaluated by [`eval_condition`]. Cost is
/// `O(∏ᵢ |candidates(varᵢ)|)` per call — the same combinatorial shape
/// grounding already pays for action-schema parameters — so this is exact,
/// not approximate, but it is re-paid on every `eval_condition` call against
/// a quantified condition (e.g. once per BFS state if a domain constraint
/// uses a quantifier), not memoized.
///
/// This function alone is not the whole capability-admission story, though:
/// `crate::capability::DefaultCapabilityProfile` marks
/// `PddlFeature::UniversalPreconditions` only `Approximate` (this evaluator
/// is genuinely correct, but the only parser-reachable path carrying a
/// `PddlCondition::Forall` into it today is a `:durative-action`'s
/// `:condition`; plain `:action` preconditions and `:goal` can't carry a
/// `Forall` through this crate's flat-`Pddl8Atom` representation at all) and
/// `PddlFeature::ExistentialPreconditions` `Unsupported` (this function's
/// `Exists` arm is just as correct — see `ground::quantifier_tests` — but no
/// parser path in this crate reaches it: `da-GD` has no `exists` production,
/// and derived-predicate bodies drop `Exists` in `ground_derived_schema`'s
/// `ground_condition` helper). See `crate::capability`'s module doc comment
/// for the full per-feature accounting.
fn eval_quantifier(
    vars: &[(String, String)], body: &PddlCondition, state: &BTreeSet<Pddl8GroundAtom>,
    fn_vals: &HashMap<String, f64>, quant_domain: &QuantifierDomain, require_all: bool,
) -> bool {
    // 8 params: threading (idx, vars, binding) for the recursive-bind state
    // plus (body, state, fn_vals, quant_domain, require_all) unchanged
    // through every level. A context struct would remove one clippy warning
    // at the cost of an extra type for a single small private helper —
    // not worth it here.
    #[allow(clippy::too_many_arguments)]
    fn recurse(
        idx: usize, vars: &[(String, String)], binding: &mut HashMap<String, String>,
        body: &PddlCondition, state: &BTreeSet<Pddl8GroundAtom>, fn_vals: &HashMap<String, f64>,
        quant_domain: &QuantifierDomain, require_all: bool,
    ) -> bool {
        if idx == vars.len() {
            let bound_body = subst_condition(body, binding);
            return eval_condition(&bound_body, state, fn_vals, quant_domain);
        }
        let (var_name, required_type) = &vars[idx];
        let candidates = quant_domain.candidates(required_type);
        if require_all {
            candidates.into_iter().all(|obj| {
                binding.insert(var_name.clone(), obj.clone());
                let r = recurse(
                    idx + 1,
                    vars,
                    binding,
                    body,
                    state,
                    fn_vals,
                    quant_domain,
                    require_all,
                );
                binding.remove(var_name);
                r
            })
        } else {
            candidates.into_iter().any(|obj| {
                binding.insert(var_name.clone(), obj.clone());
                let r = recurse(
                    idx + 1,
                    vars,
                    binding,
                    body,
                    state,
                    fn_vals,
                    quant_domain,
                    require_all,
                );
                binding.remove(var_name);
                r
            })
        }
    }
    let mut binding = HashMap::new();
    recurse(
        0,
        vars,
        &mut binding,
        body,
        state,
        fn_vals,
        quant_domain,
        require_all,
    )
}

fn apply_effect_at_start(
    eff: &PddlEffect, state: &mut BTreeSet<Pddl8GroundAtom>, fn_vals: &mut HashMap<String, f64>,
) {
    use wasm4pm_compat::pddl::TimeSpecifier;
    match eff {
        PddlEffect::Timed(TimeSpecifier::AtStart, inner) => {
            apply_effect_ground(inner, state, fn_vals)
        }
        PddlEffect::Timed(_, _) => {}
        other => apply_effect_ground(other, state, fn_vals),
    }
}

fn apply_effect_at_end(
    eff: &PddlEffect, state: &mut BTreeSet<Pddl8GroundAtom>, fn_vals: &mut HashMap<String, f64>,
) {
    use wasm4pm_compat::pddl::TimeSpecifier;
    if let PddlEffect::Timed(TimeSpecifier::AtEnd, inner) = eff {
        apply_effect_ground(inner, state, fn_vals);
    }
}

fn apply_effect_ground(
    eff: &PddlEffect, state: &mut BTreeSet<Pddl8GroundAtom>, fn_vals: &mut HashMap<String, f64>,
) {
    match eff {
        PddlEffect::Add(a) => {
            state.insert(Pddl8GroundAtom {
                pred: a.pred.clone(),
                args: a.args.clone(),
            });
        }
        PddlEffect::Del(a) => {
            state.remove(&Pddl8GroundAtom {
                pred: a.pred.clone(),
                args: a.args.clone(),
            });
        }
        PddlEffect::Numeric(ne) => apply_numeric_effect(ne, fn_vals),
        PddlEffect::When { effects, .. } => {
            for e in effects {
                apply_effect_ground(e, state, fn_vals);
            }
        }
        PddlEffect::Forall { effects, .. } => {
            for e in effects {
                apply_effect_ground(e, state, fn_vals);
            }
        }
        PddlEffect::Timed(_, inner) => apply_effect_ground(inner, state, fn_vals),
    }
}

fn apply_numeric_effect(
    ne: &wasm4pm_compat::pddl::NumericEffect, fn_vals: &mut HashMap<String, f64>,
) {
    use wasm4pm_compat::pddl::NumericEffect;
    match ne {
        NumericEffect::Assign(f, expr) => {
            let v = eval_numeric(expr, fn_vals);
            fn_vals.insert(fn_key(f), v);
        }
        NumericEffect::Increase(f, expr) => {
            let v = eval_numeric(expr, fn_vals);
            *fn_vals.entry(fn_key(f)).or_insert(0.0) += v;
        }
        NumericEffect::Decrease(f, expr) => {
            let v = eval_numeric(expr, fn_vals);
            *fn_vals.entry(fn_key(f)).or_insert(0.0) -= v;
        }
        NumericEffect::ScaleUp(f, expr) => {
            let v = eval_numeric(expr, fn_vals);
            *fn_vals.entry(fn_key(f)).or_insert(1.0) *= v;
        }
        NumericEffect::ScaleDown(f, expr) => {
            let v = eval_numeric(expr, fn_vals);
            let entry = fn_vals.entry(fn_key(f)).or_insert(1.0);
            if v != 0.0 {
                *entry /= v;
            }
        }
    }
}

fn eval_numeric(expr: &NumericExpr, fn_vals: &HashMap<String, f64>) -> f64 {
    use wasm4pm_compat::pddl::{NumericExpr, NumericOp};
    match expr {
        NumericExpr::Number(n) => *n,
        NumericExpr::FunctionTerm(name, args) => {
            let key = if args.is_empty() {
                name.clone()
            } else {
                format!("{}({})", name, args.join(","))
            };
            *fn_vals.get(&key).unwrap_or(&0.0)
        }
        NumericExpr::BinOp { op, lhs, rhs } => {
            let l = eval_numeric(lhs, fn_vals);
            let r = eval_numeric(rhs, fn_vals);
            match op {
                NumericOp::Add => l + r,
                NumericOp::Sub => l - r,
                NumericOp::Mul => l * r,
                NumericOp::Div => {
                    if r != 0.0 {
                        l / r
                    } else {
                        0.0
                    }
                }
            }
        }
        NumericExpr::Neg(inner) => -eval_numeric(inner, fn_vals),
    }
}

/// Resolve a `DurationConstraint` to (min, max) f64 bounds.
fn resolve_duration(dc: &DurationConstraint) -> (f64, f64) {
    match dc {
        DurationConstraint::Eq(expr) => {
            let v = eval_numeric(expr, &HashMap::new());
            (v, v)
        }
        DurationConstraint::Gte(expr) => {
            let v = eval_numeric(expr, &HashMap::new());
            (v, f64::INFINITY)
        }
        DurationConstraint::Lte(expr) => {
            let v = eval_numeric(expr, &HashMap::new());
            (0.0, v)
        }
        DurationConstraint::And(parts) => {
            let mut lo = 0.0_f64;
            let mut hi = f64::INFINITY;
            for p in parts {
                let (a, b) = resolve_duration(p);
                lo = lo.max(a);
                hi = hi.min(b);
            }
            (lo, hi)
        }
    }
}

/// Stable string key for a `PddlFunction`.
fn fn_key(f: &PddlFunction) -> String {
    if f.params.is_empty() {
        f.name.clone()
    } else {
        format!("{}({})", f.name, f.params.join(","))
    }
}

fn ground_schema(
    schema: &Pddl8ActionSchema, objects: &[String], type_index: &TypeIndex,
    out: &mut Vec<Pddl8GroundAction>,
) -> Result<(), Pddl8Error> {
    let n = schema.params.len();
    if n == 0 {
        if let Some(ga) = instantiate(schema, &HashMap::new()) {
            out.push(ga);
        }
        return Ok(());
    }

    // Per-parameter candidate lists, restricted to type-compatible objects
    // when the schema declares a type for that parameter — this is what
    // shrinks grounding from |objects|^n to ∏ᵢ |objects_of_type(paramᵢ)|.
    // A parameter absent from `typed_params` falls back to the full object
    // list, preserving exact behavior for untyped/legacy domains.
    let typed: HashMap<&str, &str> = schema
        .typed_params
        .iter()
        .map(|(p, t)| (p.as_str(), t.as_str()))
        .collect();
    let candidates: Vec<Vec<&String>> = schema
        .params
        .iter()
        .map(|p| match typed.get(p.as_str()) {
            Some(required) => objects
                .iter()
                .filter(|o| type_index.satisfies(o, required))
                .collect(),
            None => objects.iter().collect(),
        })
        .collect();
    if candidates.iter().any(|c| c.is_empty()) {
        return Ok(());
    }

    let mut indices = vec![0usize; n];
    loop {
        let binding: HashMap<String, String> = schema
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.clone(), candidates[i][indices[i]].clone()))
            .collect();
        if let Some(ga) = instantiate(schema, &binding) {
            out.push(ga);
        }
        // odometer increment, now bounded per-slot by candidates[i].len()
        let mut pos = n;
        loop {
            if pos == 0 {
                return Ok(());
            }
            pos -= 1;
            indices[pos] += 1;
            if indices[pos] < candidates[pos].len() {
                break;
            }
            indices[pos] = 0;
        }
    }
}

/// Ground a `DurativeAction` schema over `objects`, mirroring `ground_schema`
/// for classical actions: enumerate type-compatible bindings for `da.params`
/// and substitute the bound object names into the schema's conditions and
/// effects. A zero-param schema collapses to exactly one ground instance.
fn ground_durative_schema(
    da: &DurativeAction, objects: &[String], type_index: &TypeIndex,
    out: &mut Vec<GroundDurativeAction>,
) -> Result<(), Pddl8Error> {
    let n = da.params.len();
    let (dur_min, dur_max) = resolve_duration(&da.duration);

    if n == 0 {
        out.push(GroundDurativeAction {
            schema_name: da.name.clone(),
            label: da.name.clone(),
            args: vec![],
            duration_min: dur_min,
            duration_max: dur_max,
            conditions: da.conditions.clone(),
            effects: da.effects.clone(),
        });
        return Ok(());
    }

    // Per-parameter candidate lists, restricted to type-compatible objects —
    // same scheme as `ground_schema`'s `typed_params` lookup.
    let candidates: Vec<Vec<&String>> = da
        .params
        .iter()
        .map(|(_, required_type)| {
            objects
                .iter()
                .filter(|o| type_index.satisfies(o, required_type))
                .collect()
        })
        .collect();
    if candidates.iter().any(|c| c.is_empty()) {
        return Ok(());
    }

    let mut indices = vec![0usize; n];
    loop {
        let binding: HashMap<String, String> = da
            .params
            .iter()
            .enumerate()
            .map(|(i, (p, _))| (p.clone(), candidates[i][indices[i]].clone()))
            .collect();

        let args: Vec<String> = da
            .params
            .iter()
            .filter_map(|(p, _)| binding.get(p))
            .cloned()
            .collect();
        let label = format!("{}({})", da.name, args.join(","));

        out.push(GroundDurativeAction {
            schema_name: da.name.clone(),
            label,
            args,
            duration_min: dur_min,
            duration_max: dur_max,
            conditions: da
                .conditions
                .iter()
                .map(|c| subst_condition(c, &binding))
                .collect(),
            effects: da
                .effects
                .iter()
                .map(|e| subst_effect(e, &binding))
                .collect(),
        });

        // odometer increment, bounded per-slot by candidates[i].len()
        let mut pos = n;
        loop {
            if pos == 0 {
                return Ok(());
            }
            pos -= 1;
            indices[pos] += 1;
            if indices[pos] < candidates[pos].len() {
                break;
            }
            indices[pos] = 0;
        }
    }
}

/// Substitute schema parameter variables (e.g. `?v`) with bound object names
/// throughout a `Pddl8Atom`'s args. Non-variable args (and variables absent
/// from `binding`, e.g. universally-quantified ones) pass through unchanged.
fn subst_atom(a: &Pddl8Atom, binding: &HashMap<String, String>) -> Pddl8Atom {
    Pddl8Atom {
        pred: a.pred.clone(),
        args: a
            .args
            .iter()
            .map(|arg| {
                if Pddl8Atom::is_variable(arg) {
                    binding.get(arg).cloned().unwrap_or_else(|| arg.clone())
                } else {
                    arg.clone()
                }
            })
            .collect(),
    }
}

fn subst_numeric(expr: &NumericExpr, binding: &HashMap<String, String>) -> NumericExpr {
    match expr {
        NumericExpr::Number(n) => NumericExpr::Number(*n),
        NumericExpr::FunctionTerm(name, args) => NumericExpr::FunctionTerm(
            name.clone(),
            args.iter()
                .map(|arg| {
                    if Pddl8Atom::is_variable(arg) {
                        binding.get(arg).cloned().unwrap_or_else(|| arg.clone())
                    } else {
                        arg.clone()
                    }
                })
                .collect(),
        ),
        NumericExpr::BinOp { op, lhs, rhs } => NumericExpr::BinOp {
            op: *op,
            lhs: Box::new(subst_numeric(lhs, binding)),
            rhs: Box::new(subst_numeric(rhs, binding)),
        },
        NumericExpr::Neg(inner) => NumericExpr::Neg(Box::new(subst_numeric(inner, binding))),
    }
}

fn subst_function(f: &PddlFunction, binding: &HashMap<String, String>) -> PddlFunction {
    PddlFunction {
        name: f.name.clone(),
        params: f
            .params
            .iter()
            .map(|arg| {
                if Pddl8Atom::is_variable(arg) {
                    binding.get(arg).cloned().unwrap_or_else(|| arg.clone())
                } else {
                    arg.clone()
                }
            })
            .collect(),
    }
}

fn subst_condition(cond: &PddlCondition, binding: &HashMap<String, String>) -> PddlCondition {
    match cond {
        PddlCondition::Atom(a) => PddlCondition::Atom(subst_atom(a, binding)),
        PddlCondition::Not(inner) => PddlCondition::Not(Box::new(subst_condition(inner, binding))),
        PddlCondition::And(subs) => {
            PddlCondition::And(subs.iter().map(|s| subst_condition(s, binding)).collect())
        }
        PddlCondition::Or(subs) => {
            PddlCondition::Or(subs.iter().map(|s| subst_condition(s, binding)).collect())
        }
        PddlCondition::Forall { vars, body } => PddlCondition::Forall {
            vars: vars.clone(),
            body: Box::new(subst_condition(body, binding)),
        },
        PddlCondition::Exists { vars, body } => PddlCondition::Exists {
            vars: vars.clone(),
            body: Box::new(subst_condition(body, binding)),
        },
        PddlCondition::Imply(lhs, rhs) => PddlCondition::Imply(
            Box::new(subst_condition(lhs, binding)),
            Box::new(subst_condition(rhs, binding)),
        ),
        PddlCondition::Timed(spec, inner) => {
            PddlCondition::Timed(*spec, Box::new(subst_condition(inner, binding)))
        }
        PddlCondition::Compare(lhs, op, rhs) => PddlCondition::Compare(
            subst_numeric(lhs, binding),
            *op,
            subst_numeric(rhs, binding),
        ),
    }
}

fn subst_numeric_effect(
    ne: &wasm4pm_compat::pddl::NumericEffect, binding: &HashMap<String, String>,
) -> wasm4pm_compat::pddl::NumericEffect {
    use wasm4pm_compat::pddl::NumericEffect;
    match ne {
        NumericEffect::Assign(f, expr) => {
            NumericEffect::Assign(subst_function(f, binding), subst_numeric(expr, binding))
        }
        NumericEffect::Increase(f, expr) => {
            NumericEffect::Increase(subst_function(f, binding), subst_numeric(expr, binding))
        }
        NumericEffect::Decrease(f, expr) => {
            NumericEffect::Decrease(subst_function(f, binding), subst_numeric(expr, binding))
        }
        NumericEffect::ScaleUp(f, expr) => {
            NumericEffect::ScaleUp(subst_function(f, binding), subst_numeric(expr, binding))
        }
        NumericEffect::ScaleDown(f, expr) => {
            NumericEffect::ScaleDown(subst_function(f, binding), subst_numeric(expr, binding))
        }
    }
}

fn subst_effect(eff: &PddlEffect, binding: &HashMap<String, String>) -> PddlEffect {
    match eff {
        PddlEffect::Add(a) => PddlEffect::Add(subst_atom(a, binding)),
        PddlEffect::Del(a) => PddlEffect::Del(subst_atom(a, binding)),
        PddlEffect::Numeric(ne) => PddlEffect::Numeric(subst_numeric_effect(ne, binding)),
        PddlEffect::Timed(spec, inner) => {
            PddlEffect::Timed(*spec, Box::new(subst_effect(inner, binding)))
        }
        PddlEffect::Forall { vars, effects } => PddlEffect::Forall {
            vars: vars.clone(),
            effects: effects.iter().map(|e| subst_effect(e, binding)).collect(),
        },
        PddlEffect::When { condition, effects } => PddlEffect::When {
            condition: subst_condition(condition, binding),
            effects: effects.iter().map(|e| subst_effect(e, binding)).collect(),
        },
    }
}

fn instantiate(
    schema: &Pddl8ActionSchema, binding: &HashMap<String, String>,
) -> Option<Pddl8GroundAction> {
    fn ground_atom(a: &Pddl8Atom, binding: &HashMap<String, String>) -> Option<Pddl8GroundAtom> {
        let args: Option<Vec<String>> = a
            .args
            .iter()
            .map(|arg| {
                if Pddl8Atom::is_variable(arg) {
                    binding.get(arg).cloned()
                } else {
                    Some(arg.clone())
                }
            })
            .collect();
        args.map(|args| Pddl8GroundAtom {
            pred: a.pred.clone(),
            args,
        })
    }

    let preconditions: Option<Vec<_>> = schema
        .preconditions
        .iter()
        .map(|a| ground_atom(a, binding))
        .collect();
    let add_effects: Option<Vec<_>> = schema
        .add_effects
        .iter()
        .map(|a| ground_atom(a, binding))
        .collect();
    let del_effects: Option<Vec<_>> = schema
        .del_effects
        .iter()
        .map(|a| ground_atom(a, binding))
        .collect();

    let bound_args: Vec<String> = schema
        .params
        .iter()
        .filter_map(|p| binding.get(p))
        .cloned()
        .collect();
    let label = if bound_args.is_empty() {
        schema.name.clone()
    } else {
        format!("{}({})", schema.name, bound_args.join(","))
    };

    Some(Pddl8GroundAction {
        schema_name: schema.name.clone(),
        label,
        preconditions: preconditions?,
        add_effects: add_effects?,
        del_effects: del_effects?,
    })
}

fn ground_derived_schema(
    dp: &DerivedPredicate, objects: &[(String, String)], type_index: &TypeIndex,
    out: &mut Vec<GroundDerivedPredicate>,
) -> Result<(), crate::error::Pddl8Error> {
    let mut vars = Vec::new();
    for arg in &dp.head.args {
        if arg.starts_with('?') && !vars.iter().any(|(v, _)| v == arg) {
            vars.push((arg.clone(), "object".to_string()));
        }
    }

    fn ground_atom(a: &Pddl8Atom, binding: &HashMap<String, String>) -> Option<Pddl8GroundAtom> {
        let mut args = Vec::with_capacity(a.args.len());
        for arg in &a.args {
            if arg.starts_with('?') {
                args.push(binding.get(arg)?.clone());
            } else {
                args.push(arg.clone());
            }
        }
        Some(Pddl8GroundAtom {
            pred: a.pred.clone(),
            args,
        })
    }

    fn ground_condition(
        c: &PddlCondition, binding: &HashMap<String, String>,
    ) -> Option<PddlCondition> {
        match c {
            PddlCondition::Atom(a) => {
                let ga = ground_atom(a, binding)?;
                Some(PddlCondition::Atom(Pddl8Atom {
                    pred: ga.pred,
                    args: ga.args,
                }))
            }
            PddlCondition::And(parts) => {
                let mut ground_parts = Vec::new();
                for p in parts {
                    ground_parts.push(ground_condition(p, binding)?);
                }
                Some(PddlCondition::And(ground_parts))
            }
            PddlCondition::Not(inner) => Some(PddlCondition::Not(Box::new(ground_condition(
                inner, binding,
            )?))),
            PddlCondition::Or(parts) => {
                let mut ground_parts = Vec::new();
                for p in parts {
                    ground_parts.push(ground_condition(p, binding)?);
                }
                Some(PddlCondition::Or(ground_parts))
            }
            PddlCondition::Imply(a, b) => Some(PddlCondition::Imply(
                Box::new(ground_condition(a, binding)?),
                Box::new(ground_condition(b, binding)?),
            )),
            PddlCondition::Compare(lhs, cmp, rhs) => {
                Some(PddlCondition::Compare(lhs.clone(), *cmp, rhs.clone()))
            }
            PddlCondition::Timed(ts, c) => Some(PddlCondition::Timed(
                *ts,
                Box::new(ground_condition(c, binding)?),
            )),
            _ => None,
        }
    }

    fn recurse(
        param_idx: usize, bindings: &mut HashMap<String, String>, dp: &DerivedPredicate,
        objects: &[(String, String)], type_index: &TypeIndex,
        out: &mut Vec<GroundDerivedPredicate>, vars: &[(String, String)],
    ) -> Result<(), crate::error::Pddl8Error> {
        if param_idx == vars.len() {
            if let Some(ground_head) = ground_atom(&dp.head, bindings) {
                if let Some(ground_cond) = ground_condition(&dp.body, bindings) {
                    out.push(GroundDerivedPredicate {
                        head: ground_head,
                        condition: ground_cond,
                    });
                }
            }
            return Ok(());
        }
        let (var_name, req_type) = &vars[param_idx];
        for (obj_name, _obj_type) in objects {
            if type_index.satisfies(obj_name, req_type) {
                bindings.insert(var_name.clone(), obj_name.clone());
                recurse(param_idx + 1, bindings, dp, objects, type_index, out, vars)?;
                bindings.remove(var_name);
            }
        }
        Ok(())
    }

    recurse(0, &mut HashMap::new(), dp, objects, type_index, out, &vars)
}
/// Compute the least fixpoint of `derived` over `state` (a derived predicate
/// fires once its body holds; firing can make other derived predicates'
/// bodies hold, so this iterates to a fixpoint rather than a single pass).
///
/// Narrowed from `pub` to `pub(crate)` in this phase alongside
/// `eval_condition` (see that function's doc comment) — no external caller
/// referenced it.
pub(crate) fn compute_derived_closure(
    state: &mut BTreeSet<Pddl8GroundAtom>, derived: &[GroundDerivedPredicate],
    fn_vals: &HashMap<String, f64>, quant_domain: &QuantifierDomain,
) {
    let mut changed = true;
    while changed {
        changed = false;
        for dp in derived {
            if !state.contains(&dp.head)
                && eval_condition(&dp.condition, state, fn_vals, quant_domain)
            {
                state.insert(dp.head.clone());
                changed = true;
            }
        }
    }
}

// `check_capabilities` (a narrow, ad hoc `:object-fluents` rejection with no
// callers anywhere in this workspace) has been removed and superseded by
// `crate::capability::admit_planning_task`, which performs the same
// `:object-fluents` structural rejection (PDDL 3.1 object-valued fluents are
// not implemented anywhere in this grounder — only numeric fluents are) as
// part of a much more complete admission gate over all sixteen
// `PddlFeature`s. See `crate::capability` for the replacement.

#[cfg(test)]
mod quantifier_tests {
    //! White-box tests for `eval_quantifier`/`eval_condition`'s `Forall`/
    //! `Exists` handling — constructed directly against `QuantifierDomain`
    //! and `PddlCondition`, bypassing the PDDL parser entirely. This is
    //! deliberate: as documented in `crate::capability`, the parser has its
    //! own pre-existing, separate gaps (durative-action conditions support
    //! `Forall` but not `Exists` at the grammar level; plain `:action`
    //! preconditions and `:goal` can't carry either quantifier through
    //! `Pddl8ActionSchema.preconditions`/`Pddl8Problem.goal`'s flat
    //! `Vec<Pddl8Atom>` representation) — those are parser/grounding-pipeline
    //! limitations, not evaluator bugs. These tests prove the evaluator
    //! itself — the actual fix for the `Forall => true` / `Exists => false`
    //! stub — is correct, independent of which parser paths currently reach
    //! it. `tests/durative_quantifiers.rs` separately proves `Forall` is
    //! ALIVE end-to-end through the one parser path that does carry it
    //! (durative-action `:condition`).
    use super::*;

    fn domain(objects: &[&str], ready: &[&str]) -> (QuantifierDomain, BTreeSet<Pddl8GroundAtom>) {
        let type_index = TypeIndex {
            object_type: HashMap::new(),
            parent: HashMap::new(),
        };
        let quant_domain = QuantifierDomain {
            objects: objects.iter().map(|s| s.to_string()).collect(),
            type_index,
        };
        let state: BTreeSet<Pddl8GroundAtom> = ready
            .iter()
            .map(|o| Pddl8GroundAtom {
                pred: "ready".to_string(),
                args: vec![o.to_string()],
            })
            .collect();
        (quant_domain, state)
    }

    fn ready_body() -> PddlCondition {
        PddlCondition::Atom(Pddl8Atom {
            pred: "ready".to_string(),
            args: vec!["?i".to_string()],
        })
    }

    #[test]
    fn forall_is_true_when_every_object_satisfies_body() {
        let (qd, state) = domain(&["a", "b"], &["a", "b"]);
        let cond = PddlCondition::Forall {
            vars: vec![("?i".to_string(), "object".to_string())],
            body: Box::new(ready_body()),
        };
        assert!(eval_condition(&cond, &state, &HashMap::new(), &qd));
    }

    #[test]
    fn forall_is_false_when_one_object_fails_body() {
        let (qd, state) = domain(&["a", "b"], &["a"]);
        let cond = PddlCondition::Forall {
            vars: vec![("?i".to_string(), "object".to_string())],
            body: Box::new(ready_body()),
        };
        assert!(
            !eval_condition(&cond, &state, &HashMap::new(), &qd),
            "b is not ready, so forall must be false — the old stub always returned true here"
        );
    }

    #[test]
    fn forall_is_vacuously_true_over_an_empty_object_domain() {
        let (qd, state) = domain(&[], &[]);
        let cond = PddlCondition::Forall {
            vars: vec![("?i".to_string(), "object".to_string())],
            body: Box::new(ready_body()),
        };
        assert!(eval_condition(&cond, &state, &HashMap::new(), &qd));
    }

    #[test]
    fn exists_is_true_when_at_least_one_object_satisfies_body() {
        let (qd, state) = domain(&["a", "b"], &["b"]);
        let cond = PddlCondition::Exists {
            vars: vec![("?i".to_string(), "object".to_string())],
            body: Box::new(ready_body()),
        };
        assert!(eval_condition(&cond, &state, &HashMap::new(), &qd));
    }

    #[test]
    fn exists_is_false_when_no_object_satisfies_body() {
        let (qd, state) = domain(&["a", "b"], &[]);
        let cond = PddlCondition::Exists {
            vars: vec![("?i".to_string(), "object".to_string())],
            body: Box::new(ready_body()),
        };
        assert!(
            !eval_condition(&cond, &state, &HashMap::new(), &qd),
            "no object is ready, so exists must be false — the old stub always returned false \
             here too, so this specific case coincidentally matched the stub, but for the wrong \
             reason (constant, not evaluated)"
        );
    }

    #[test]
    fn exists_is_vacuously_false_over_an_empty_object_domain() {
        let (qd, state) = domain(&[], &[]);
        let cond = PddlCondition::Exists {
            vars: vec![("?i".to_string(), "object".to_string())],
            body: Box::new(ready_body()),
        };
        assert!(!eval_condition(&cond, &state, &HashMap::new(), &qd));
    }

    #[test]
    fn nested_forall_exists_binds_independently() {
        // forall ?i exists ?j (paired i j) — a genuinely two-variable
        // quantifier nesting, proving `eval_quantifier`'s recursive binding
        // substitutes each variable independently rather than aliasing them.
        let type_index = TypeIndex {
            object_type: HashMap::new(),
            parent: HashMap::new(),
        };
        let qd = QuantifierDomain {
            objects: vec!["a".to_string(), "b".to_string()],
            type_index,
        };
        // "a" pairs with "b" and "b" pairs with "a" (a ring), so every ?i has
        // some ?j != ?i with (paired ?i ?j) true.
        let state: BTreeSet<Pddl8GroundAtom> = [("a", "b"), ("b", "a")]
            .iter()
            .map(|(x, y)| Pddl8GroundAtom {
                pred: "paired".to_string(),
                args: vec![x.to_string(), y.to_string()],
            })
            .collect();
        let inner_body = PddlCondition::Atom(Pddl8Atom {
            pred: "paired".to_string(),
            args: vec!["?i".to_string(), "?j".to_string()],
        });
        let exists_j = PddlCondition::Exists {
            vars: vec![("?j".to_string(), "object".to_string())],
            body: Box::new(inner_body),
        };
        let forall_i_exists_j = PddlCondition::Forall {
            vars: vec![("?i".to_string(), "object".to_string())],
            body: Box::new(exists_j),
        };
        assert!(eval_condition(
            &forall_i_exists_j,
            &state,
            &HashMap::new(),
            &qd
        ));

        // Break the ring (only a->b, no b->a): now ?i=b has no matching ?j.
        let broken_state: BTreeSet<Pddl8GroundAtom> = [("a", "b")]
            .iter()
            .map(|(x, y)| Pddl8GroundAtom {
                pred: "paired".to_string(),
                args: vec![x.to_string(), y.to_string()],
            })
            .collect();
        assert!(!eval_condition(
            &forall_i_exists_j,
            &broken_state,
            &HashMap::new(),
            &qd
        ));
    }
}

#[cfg(test)]
mod bound_exceeded_tests {
    //! `Pddl8Error::BoundExceeded.limit` used to be `u8`, which silently
    //! truncated any bound above `u8::MAX` (255) — `4096usize as u8 == 0`
    //! for the default [`PDDL8_MAX_GROUND`], so a domain that genuinely
    //! exceeded it reported `limit: 0` in its refusal instead of the real
    //! bound. These tests build a domain (directly, not through the PDDL
    //! parser — grounding count is all that matters here) whose grounding
    //! exceeds a caller-supplied limit above `u8::MAX`, and assert the
    //! reported `limit` is the real value, not a wrapped-around one.
    use super::*;

    /// One untyped, single-parameter action schema grounds once per object:
    /// `n` objects in the problem produce exactly `n` ground actions.
    fn single_param_domain() -> Pddl8Domain {
        Pddl8Domain {
            name: "bound-probe".to_string(),
            predicates: vec![("p".to_string(), 1)],
            actions: vec![Pddl8ActionSchema {
                name: "act".to_string(),
                params: vec!["?x".to_string()],
                preconditions: vec![],
                add_effects: vec![Pddl8Atom {
                    pred: "p".to_string(),
                    args: vec!["?x".to_string()],
                }],
                del_effects: vec![],
                typed_params: vec![],
                condition: None,
                effects: vec![],
                numeric_effects: vec![],
            }],
            types: vec![],
            functions: vec![],
            durative_actions: vec![],
            derived: vec![],
            constraints: vec![],
            processes: vec![],
            events: vec![],
        }
    }

    fn problem_with_n_objects(n: usize) -> Pddl8Problem {
        Pddl8Problem {
            name: "bound-probe-p".to_string(),
            domain: "bound-probe".to_string(),
            objects: (0..n).map(|i| format!("o{i}")).collect(),
            init: vec![],
            goal: vec![],
            object_types: vec![],
            fn_values: vec![],
            timed_inits: vec![],
            metric: None,
            preferences: vec![],
        }
    }

    #[test]
    fn ground_action_bound_report_is_not_truncated_past_u8_max() {
        let domain = single_param_domain();
        // 301 objects -> 301 ground actions, which exceeds a 300 limit.
        // 300 > u8::MAX (255): a pre-fix `limit as u8` would have reported
        // `300usize as u8 == 44`, not the real bound of 300.
        let problem = problem_with_n_objects(301);
        match GroundProblem::build(&domain, &problem, Some(300)) {
            Err(Pddl8Error::BoundExceeded { limit, got, .. }) => {
                assert_eq!(
                    limit, 300,
                    "limit must report the real bound, not a u8-wrapped value"
                );
                assert_eq!(got, 301);
            }
            Err(other) => panic!("expected BoundExceeded, got {other:?}"),
            Ok(_) => panic!("301 ground actions must exceed the 300-action limit"),
        }
    }

    #[test]
    fn default_ground_action_bound_report_is_not_truncated() {
        // Exercise the *default* PDDL8_MAX_GROUND path (max_ground: None),
        // not just a caller-supplied limit: PDDL8_MAX_GROUND itself (4096)
        // is > u8::MAX, so this is the exact scenario the original bug
        // report cited (`4096usize as u8 == 0`).
        let domain = single_param_domain();
        let problem = problem_with_n_objects(PDDL8_MAX_GROUND + 1);
        match GroundProblem::build(&domain, &problem, None) {
            Err(Pddl8Error::BoundExceeded { limit, got, .. }) => {
                assert_eq!(
                    limit, PDDL8_MAX_GROUND,
                    "limit must report the real PDDL8_MAX_GROUND bound (4096), not \
                     4096usize as u8 == 0"
                );
                assert_eq!(got, PDDL8_MAX_GROUND + 1);
            }
            Err(other) => panic!("expected BoundExceeded, got {other:?}"),
            Ok(_) => panic!("PDDL8_MAX_GROUND + 1 ground actions must exceed the default bound"),
        }
    }
}
