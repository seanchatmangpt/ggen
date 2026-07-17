//! `MfwPlanner` — the top-level orchestrator wiring this crate's admission/
//! grounding/consequence/search/causal/concurrency machinery together with
//! `bcinr-powl`'s `PowlProjector` and `bcinr-powl-receipt`'s receipt
//! sealing into one `plan()` entry point.
//!
//! # What this module is, and isn't
//!
//! Every piece [`MfwPlanner::plan`] calls already exists and was verified
//! (ALIVE, this session) in an earlier phase — [`admit_planning_task`],
//! [`GroundProblem::build`], [`StandingConsequenceCache`], [`Residualizer`],
//! [`MassVector`]/[`q_lens`], [`MfwPortfolio`], [`PddlCausalAnalyzer`],
//! [`PddlConcurrencyAnalyzer`], `bcinr_powl::projection::PowlProjector`,
//! `bcinr_powl_receipt::projection::seal_projection_receipt`,
//! `bcinr_powl_receipt::planning::seal_planning_receipt`. This module is
//! integration glue, not a new subsystem: it does not reimplement any of
//! those, it calls them in sequence and threads real values between them.
//!
//! # Real gap #1 — a cache hit cannot produce a plan tape
//!
//! [`StandingConsequenceCache<GoalReachabilityHorizon>`]'s `Consequence`
//! type is a bare `bool` ("was the goal reachable from this state under
//! this theory") — it does not preserve the plan that proved it. So:
//!
//! - A cached `false` (goal was proven unreachable from this exact
//!   `(state, theory)` before) lets [`plan`](MfwPlanner::plan) short-circuit
//!   the entire rest of the pipeline with zero search — real, not a stub,
//!   and sound **to the extent `theory_digest` actually distinguishes
//!   theories that differ** (see
//!   [`crate::capability::domain_problem_digest`]'s doc comment for the
//!   precise, current coverage boundary: action bodies, durations, and
//!   `:init`/`:goal` are covered; `:constraints`/`:preferences`/`:metric`/
//!   PDDL+ are not, so two domains differing only in one of those would
//!   still share a `theory_digest` and could produce a false
//!   `CachedUnreachable`). `self.cache` is a persistent field reused across
//!   every `plan()` call on `self`, including calls with different
//!   `domain_text`/`problem_text` — so this short-circuit's soundness is a
//!   live property of every call, not a one-time construction-time check.
//!   ALIVE, verified this session: `plan()`'s own pipeline never actually
//!   *writes* a `false`
//!   entry (`self.cache.admit` is called exactly once, and only after
//!   `goal_reached` is already confirmed `true` — every non-`Found`
//!   portfolio outcome and the `!goal_reached` case both `return Err(...)`
//!   first), so the read-side short-circuit
//!   (`if cache_hit == Some(false) { return
//!   Err(MfwPlanError::CachedUnreachable) }`) had never actually fired in
//!   any test or real call before this session —
//!   `tests::a_hand_seeded_false_cache_entry_makes_plan_short_circuit_with_cached_unreachable`
//!   now hand-seeds a `false` entry (via the public `cache` field, at the
//!   exact `(state_digest, theory_digest)` key `plan()`'s own pipeline
//!   would derive) and confirms `plan()` genuinely returns
//!   `Err(MfwPlanError::CachedUnreachable)` without running the portfolio.
//! - A cached `true` cannot, by itself, produce the `Pddl8Tape` that
//!   `causal`/`concurrency`/POWL projection all need downstream. `plan()`
//!   documents this rather than fabricating a tape: it still has to run the
//!   portfolio to materialize *a* concrete witnessing plan, even though the
//!   cache already told it one exists. This is a genuine, structural
//!   limitation of `consequence.rs`'s existing `GoalReachabilityHorizon`
//!   design (bool-shaped consequences), not something introduced or hidden
//!   here.
//!
//! # Real gap #2 — several `ComputeEvidence` counters are not observable
//!
//! `MfwPortfolio::solve` (`crate::search`, line ~350) returns only the
//! terminal `PortfolioOutcome` — it exposes no per-rail tick counter. So
//! `ComputeEvidence::states_expanded`/`exact_rail_steps`/`exploit_rail_steps`
//! are always `0` here, documented plainly rather than estimated or
//! fabricated. Making them real would mean adding counters inside
//! `MfwPortfolio` itself — a mechanical but *separate* change, out of this
//! integration phase's scope (see the final report).
//!
//! # Feature-gated
//!
//! This whole module only compiles with the `mfw-planner` Cargo feature —
//! see `crate::mfw`'s module doc comment for why.

use std::collections::BTreeMap;

use bcinr_mfw_ir::{
    CausalAnalyzer, CausalPlan, ConcurrencyAnalyzer, ExecutableConcurrencyComplex, PlannerFailure,
    PlannerOutcome, PowlProjector as PowlProjectorTrait,
};
use wasm4pm_compat::pddl::{Pddl8GroundAction, Pddl8Tape};

use bcinr_powl_receipt::planning::{
    seal_planning_receipt, ComputeEvidence, PlannerOutcomeTag, PlanningReceipt,
};
use bcinr_powl_receipt::projection::{
    digest_powl_model, seal_projection_receipt, ProjectionReceipt,
};

use crate::capability::{
    admit_planning_task, AdmittedPlanningTask, CapabilityProfile, GroundedPlanningEpoch,
    ALL_PDDL_FEATURES,
};
use crate::causal::{CausalAnalysisError, PddlCausalAnalyzer};
use crate::concurrency::{ConcurrencyAnalysisError, PddlConcurrencyAnalyzer};
use crate::consequence::{
    ConsequenceHorizon, GoalReachabilityHorizon, PlanningResult, ResidualDecision, Residualizer,
    StandingConsequenceCache,
};
use crate::error::Pddl8Error;
use crate::ground::GroundProblem;
use crate::mfw::{q_lens, MassVector, PositiveMass, QValue};
use crate::parse::{domain31_from_pddl, domain_from_pddl, problem31_from_pddl, problem_from_pddl};
use crate::search::{ExactBfsRail, MfwPortfolio, PortfolioOutcome, QLensRail};
use bcinr_mfw_ir::{Digest, EpochBounds};

// ---------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------

/// Every terminal failure mode of [`MfwPlanner::plan`]. Every variant
/// carries the real underlying witness/error — nothing is stringified away.
#[derive(Debug)]
pub enum MfwPlanError {
    /// PDDL text failed to parse (either the `Pddl31` or `Pddl8` parse
    /// path — both are attempted, see the module doc comment on why two
    /// parses of the same text are needed).
    Parse(Pddl8Error),
    /// [`admit_planning_task`] refused the domain/problem.
    Admission(PlannerFailure),
    /// [`GroundProblem::build`] failed.
    Grounding(Pddl8Error),
    /// The standing cache already proved this exact `(state, theory_digest)`
    /// unreachable — a real, zero-search short-circuit, not an error in the
    /// usual sense, but terminal for `plan()` because there is nothing
    /// further to build a `PlannedWorkflow` from. Sound to the extent
    /// `theory_digest` actually distinguishes theories that differ — see
    /// the module doc comment's "Real gap #1" section and
    /// [`crate::capability::domain_problem_digest`] for the current
    /// coverage boundary.
    CachedUnreachable,
    /// The portfolio's exact rail proved no plan exists.
    PortfolioExhausted(bcinr_mfw_ir::ExhaustionWitness),
    /// The portfolio's exact rail hit a structural bound.
    PortfolioBounded(bcinr_mfw_ir::BoundHit),
    /// The portfolio's tick budget elapsed before a terminal outcome.
    PortfolioTickBudgetExhausted,
    /// The exact rail returned `Found(tape)`, but replaying `tape` did not
    /// actually reach the goal — a genuine inconsistency between BFS's own
    /// applicability semantics and this validator's replay, surfaced loudly
    /// rather than silently accepted.
    ValidationFailed,
    /// [`PddlCausalAnalyzer::analyze`] failed.
    CausalAnalysis(CausalAnalysisError),
    /// [`PddlConcurrencyAnalyzer::analyze`] failed.
    ConcurrencyAnalysis(ConcurrencyAnalysisError),
    /// The POWL projector's real order/concurrency preservation check
    /// failed — never swallowed.
    Projection(bcinr_powl::projection::ProjectionError),
}

impl std::fmt::Display for MfwPlanError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(e) => write!(f, "PDDL parse error: {e}"),
            Self::Admission(w) => write!(f, "admission refused: {w}"),
            Self::Grounding(e) => write!(f, "grounding failed: {e}"),
            Self::CachedUnreachable => {
                write!(
                    f,
                    "standing cache already proved this state/theory unreachable"
                )
            }
            Self::PortfolioExhausted(_) => write!(f, "portfolio exhausted: no plan exists"),
            Self::PortfolioBounded(_) => write!(f, "portfolio hit a structural bound"),
            Self::PortfolioTickBudgetExhausted => write!(f, "portfolio tick budget exhausted"),
            Self::ValidationFailed => write!(f, "found plan did not validate against the goal"),
            Self::CausalAnalysis(e) => write!(f, "causal analysis failed: {e}"),
            Self::ConcurrencyAnalysis(e) => write!(f, "concurrency analysis failed: {e}"),
            Self::Projection(e) => write!(f, "POWL projection failed: {e}"),
        }
    }
}

impl std::error::Error for MfwPlanError {}

// ---------------------------------------------------------------------
// Output types
// ---------------------------------------------------------------------

/// A plan that has been replayed against the epoch's initial state and
/// confirmed (not assumed) to reach the goal.
#[derive(Debug, Clone)]
pub struct ValidatedPlan {
    pub tape: Pddl8Tape,
    pub result: PlanningResult,
    pub goal_reached: bool,
}

/// The full output of one [`MfwPlanner::plan`] call: a validated plan, its
/// causal and concurrency analyses, its POWL projection, and the sealed
/// receipt chain (`ProjectionReceipt` -> `PlanningReceipt`) attesting to all
/// of it.
#[derive(Debug, Clone)]
pub struct PlannedWorkflow {
    pub epoch: GroundedPlanningEpoch,
    pub validated_plan: ValidatedPlan,
    pub causal_plan: CausalPlan,
    pub concurrency: ExecutableConcurrencyComplex,
    pub powl_model: bcinr_powl::model::PowlModel,
    pub projection_receipt: ProjectionReceipt,
    pub planning_receipt: PlanningReceipt,
    /// Whether the standing cache already had a (reachable) hit for this
    /// exact `(state, theory)` before this call — see the module doc
    /// comment's "Real gap #1": a hit does not skip the portfolio run, it
    /// is recorded as telemetry only.
    pub cache_hit: bool,
}

// ---------------------------------------------------------------------
// MfwPlanner
// ---------------------------------------------------------------------

/// Top-level orchestrator, generic over the consequence horizon (`H`,
/// binding the standing cache's consequence type), the causal analyzer
/// (`CA`), the concurrency analyzer (`CO`), and the POWL projector (`PJ`).
///
/// `plan()` itself is only implemented for `H = GoalReachabilityHorizon`
/// (see the `impl` block below) because [`Residualizer`] only has a real
/// `residualize` implementation for that horizon today — exactly the same
/// "one concrete instantiation is wired, the type stays generic" pattern
/// `crate::consequence`'s own `impl Residualizer<GoalReachabilityHorizon>`
/// already uses. `CA`/`CO`/`PJ` each have exactly one real implementation
/// in this workspace right now ([`PddlCausalAnalyzer`],
/// [`PddlConcurrencyAnalyzer`], `bcinr_powl::projection::PowlProjector`);
/// the generic parameters exist so a future analyzer/projector can be
/// substituted without changing `MfwPlanner`'s shape, not because multiple
/// real implementations exist today.
///
/// `crate::search`'s `MfwPortfolio<E, X>`/`crate::mfw`'s `FrontierBoxes` are
/// *not* stored as `MfwPlanner` fields: both `ExactBfsRail`/`QLensRail`
/// borrow the `GroundProblem` by reference, so a portfolio built from them
/// cannot outlive one `plan()` call — `plan()` constructs them locally,
/// scoped to the grounded problem, exercising the same real portfolio
/// machinery without forcing an artificial lifetime parameter onto
/// `MfwPlanner` itself.
pub struct MfwPlanner<H, CA, CO, PJ>
where
    H: ConsequenceHorizon,
{
    pub cache: StandingConsequenceCache<H>,
    pub causal_analyzer: CA,
    pub concurrency_analyzer: CO,
    pub projector: PJ,
    pub bounds: EpochBounds,
    pub exploit_q: QValue,
    pub max_gap: usize,
    pub max_ticks: usize,
}

/// The one real instantiation this workspace ships: `GoalReachabilityHorizon`
/// cache, `PddlCausalAnalyzer`, `PddlConcurrencyAnalyzer`,
/// `bcinr_powl::projection::PowlProjector`.
pub type DefaultMfwPlanner = MfwPlanner<
    GoalReachabilityHorizon,
    PddlCausalAnalyzer,
    PddlConcurrencyAnalyzer,
    bcinr_powl::projection::PowlProjector,
>;

impl<H, CA, CO, PJ> MfwPlanner<H, CA, CO, PJ>
where
    H: ConsequenceHorizon,
    CA: Default,
    CO: Default,
{
    /// `projector` is taken explicitly (not via a `PJ: Default` bound)
    /// because `bcinr_powl::projection::PowlProjector` — the one real
    /// implementation this workspace ships — is a bare unit struct that
    /// deliberately does not derive `Default` (see `bcinr-powl`'s
    /// `projection.rs`); constructing it as a literal (`PowlProjector`) at
    /// the call site is exactly as cheap and avoids adding a derive to
    /// already-landed Phase 2b code for this integration phase's
    /// convenience alone.
    pub fn new(
        horizon: H,
        projector: PJ,
        bounds: EpochBounds,
        exploit_q: QValue,
        max_gap: usize,
        max_ticks: usize,
    ) -> Self {
        Self {
            cache: StandingConsequenceCache::new(horizon),
            causal_analyzer: CA::default(),
            concurrency_analyzer: CO::default(),
            projector,
            bounds,
            exploit_q,
            max_gap,
            max_ticks,
        }
    }
}

impl<CA, CO, PJ> MfwPlanner<GoalReachabilityHorizon, CA, CO, PJ>
where
    CA: CausalAnalyzer<Epoch = GroundedPlanningEpoch, Error = CausalAnalysisError>,
    CO: ConcurrencyAnalyzer<Epoch = GroundedPlanningEpoch, Error = ConcurrencyAnalysisError>,
    PJ: PowlProjectorTrait<
        Model = bcinr_powl::model::PowlModel,
        Error = bcinr_powl::projection::ProjectionError,
    >,
{
    /// Run the complete admission -> grounding -> cache -> residualize ->
    /// measure -> portfolio -> validate -> causal -> concurrency -> project
    /// -> seal pipeline for one `(domain_text, problem_text)` pair.
    ///
    /// Parses `domain_text`/`problem_text` twice — once via the `Pddl31`
    /// parser (for [`admit_planning_task`]'s capability admission) and once
    /// via the `Pddl8` parser (for [`GroundProblem::build`]'s grounder) —
    /// mirroring `crate::llm_bridge::admit_candidate_domain`'s existing,
    /// already-shipped pattern for bridging the two parse paths (there is
    /// no `Pddl31Domain` -> `Pddl8Domain` conversion anywhere in this crate;
    /// grep-confirmed).
    pub fn plan(
        &mut self,
        domain_text: &str,
        problem_text: &str,
        profile: &dyn CapabilityProfile,
    ) -> Result<PlannedWorkflow, MfwPlanError> {
        // --- Parse (both paths) ---
        let domain31 = domain31_from_pddl(domain_text).map_err(MfwPlanError::Parse)?;
        let problem31 = problem31_from_pddl(problem_text).map_err(MfwPlanError::Parse)?;
        let domain8 = domain_from_pddl(domain_text).map_err(MfwPlanError::Parse)?;
        let problem8 = problem_from_pddl(problem_text).map_err(MfwPlanError::Parse)?;

        // --- Admission ---
        let admitted: AdmittedPlanningTask = admit_planning_task(&domain31, &problem31, profile)
            .into_result()
            .map_err(MfwPlanError::Admission)?;
        let capability_digest = capability_digest(profile);

        // --- Grounding ---
        let gp =
            GroundProblem::build(&domain8, &problem8, None).map_err(MfwPlanError::Grounding)?;
        let mut epoch =
            GroundedPlanningEpoch::from_ground_problem(&gp, admitted.theory_digest, self.bounds);
        // `from_ground_problem` derives `id` from `theory_digest`'s first 16
        // bytes (see `capability.rs`'s doc comment) — already deterministic,
        // no further action needed, but bind the variable name for clarity
        // at each call site below.
        let _ = &mut epoch;

        // --- Cache lookup (real gap #1, see module doc comment) ---
        let state_digest = state_digest(&gp.initial_state);
        let theory_digest = admitted.theory_digest;
        let cache_hit = self.cache.lookup(state_digest, theory_digest).copied();
        if cache_hit == Some(false) {
            return Err(MfwPlanError::CachedUnreachable);
        }
        let cache_hit_bool = cache_hit.is_some();

        // --- Residualize ---
        let residualizer = Residualizer::<GoalReachabilityHorizon>::new();
        let residual = residualizer.residualize(&gp.initial_state, &gp.goal, &gp.actions);

        // --- Measure the frontier (real for what's available; see below) ---
        let frontier_mass = measure_frontier(&residual);
        let _ = frontier_mass.map(|m| {
            // Exercise q_lens for real even in this degenerate one-box
            // case — `QLensRail::step` (crate::search) already performs
            // richer, multi-candidate q_lens ranking per hop; this is a
            // deliberately minimal, honest top-level use, not a duplicate
            // of that richer ranking.
            let dist = bcinr_pddl_positive_distribution(m);
            q_lens(self.exploit_q, &dist).ok()
        });

        // --- Portfolio solve ---
        let exact = ExactBfsRail::new(&gp);
        let exploit = vec![QLensRail::new(&gp, self.exploit_q)];
        let mut portfolio = MfwPortfolio::new(exact, exploit, self.max_gap, self.max_ticks);
        let tape = match portfolio.solve() {
            PortfolioOutcome::Found(tape) => tape,
            PortfolioOutcome::Exhausted(w, _) => return Err(MfwPlanError::PortfolioExhausted(w)),
            PortfolioOutcome::Bounded(b, _) => return Err(MfwPlanError::PortfolioBounded(b)),
            PortfolioOutcome::TickBudgetExhausted(_) => {
                return Err(MfwPlanError::PortfolioTickBudgetExhausted)
            }
        };

        // --- Validate ---
        let result = PlanningResult::from_classical_tape(&gp.initial_state, &gp.goal, &tape);
        let goal_reached = GoalReachabilityHorizon.observe(&result);
        if !goal_reached {
            return Err(MfwPlanError::ValidationFailed);
        }
        self.cache.admit(state_digest, theory_digest, &result);
        let validated_plan = ValidatedPlan {
            tape: tape.clone(),
            result,
            goal_reached,
        };

        // --- Causal analysis ---
        let occurrences = occurrences_from_tape(&tape, &epoch.actions);
        let causal_plan = self
            .causal_analyzer
            .analyze(&epoch, &occurrences)
            .map_err(MfwPlanError::CausalAnalysis)?;

        // --- Concurrency analysis ---
        let concurrency = self
            .concurrency_analyzer
            .analyze(&epoch, &causal_plan)
            .map_err(MfwPlanError::ConcurrencyAnalysis)?;

        // --- Project to POWL ---
        let (powl_model, projection_witness) = self
            .projector
            .project(&causal_plan, &concurrency)
            .map_err(MfwPlanError::Projection)?;
        let powl_model_digest = digest_powl_model(&powl_model);
        let projection_receipt = seal_projection_receipt(
            Digest::ZERO,
            &causal_plan,
            &concurrency,
            &projection_witness,
            powl_model_digest,
        );

        // --- Seal the planning receipt ---
        let cache_evidence = Digest::hash(
            &[
                state_digest.as_bytes().as_slice(),
                theory_digest.as_bytes().as_slice(),
                &[cache_hit_bool as u8],
            ]
            .concat(),
        );
        let residual_evidence = residual_digest(&residual);
        let frontier_evidence = frontier_mass
            .map(|m| Digest::hash(&m.get().to_le_bytes()))
            .unwrap_or(Digest::ZERO);
        let portfolio_evidence = Digest::hash(
            &tape
                .ops
                .iter()
                .flat_map(|op| op.label.as_bytes().to_vec())
                .collect::<Vec<u8>>(),
        );
        let compute = ComputeEvidence {
            candidate_groundings: epoch.actions.len() as u64,
            materialized_groundings: epoch.actions.len() as u64,
            // Not observable from the current search-rail/portfolio API —
            // see the module doc comment's "Real gap #2" (crate::search,
            // MfwPortfolio::solve, ~line 350). Documented zero, not
            // fabricated.
            states_expanded: 0,
            semantic_classes_visited: 1,
            cache_hits: cache_hit_bool as u64,
            residual_actions: match &residual {
                ResidualDecision::NoWork => 0,
                ResidualDecision::Residual(obligation) => {
                    obligation.candidate_supporting_actions.len() as u64
                }
            },
            exact_rail_steps: 0,
            exploit_rail_steps: 0,
        };
        let planning_receipt = seal_planning_receipt(
            projection_receipt.hash,
            epoch.id,
            self.cache.horizon().id(),
            capability_digest,
            cache_evidence,
            residual_evidence,
            frontier_evidence,
            portfolio_evidence,
            PlannerOutcomeTag::from(&PlannerOutcome::Found(())),
            causal_plan.digest,
            concurrency.digest,
            projection_witness.digest,
            compute,
        );

        Ok(PlannedWorkflow {
            epoch,
            validated_plan,
            causal_plan,
            concurrency,
            powl_model,
            projection_receipt,
            planning_receipt,
            cache_hit: cache_hit_bool,
        })
    }
}

// ---------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------

/// Deterministic digest of a ground-atom state set — same construction
/// `crate::consequence`'s own tests use (label bytes, `0`-terminated, in
/// `BTreeSet` iteration order, which is already deterministic).
fn state_digest(
    state: &std::collections::BTreeSet<wasm4pm_compat::pddl::Pddl8GroundAtom>,
) -> Digest {
    let mut buf = Vec::new();
    for atom in state {
        buf.extend_from_slice(atom.label().as_bytes());
        buf.push(0);
    }
    Digest::hash(&buf)
}

/// Real, deterministic digest of exactly which `PddlFeature`s `profile`
/// marks as supported, at what level — not a stand-in, an actual per-feature
/// accounting of the admission decision this planner run used.
fn capability_digest(profile: &dyn CapabilityProfile) -> Digest {
    let mut buf = Vec::new();
    for &feature in &ALL_PDDL_FEATURES {
        buf.extend_from_slice(format!("{feature:?}").as_bytes());
        buf.push(profile.support(feature) as u8);
    }
    Digest::hash(&buf)
}

fn residual_digest(residual: &ResidualDecision) -> Digest {
    let mut buf = Vec::new();
    match residual {
        ResidualDecision::NoWork => buf.push(0),
        ResidualDecision::Residual(obligation) => {
            buf.push(1);
            for atom in &obligation.unsatisfied_goal_atoms {
                buf.extend_from_slice(atom.label().as_bytes());
                buf.push(0);
            }
            for action in &obligation.candidate_supporting_actions {
                buf.extend_from_slice(action.label.as_bytes());
                buf.push(0);
            }
        }
    }
    Digest::hash(&buf)
}

/// Build one [`MassVector`] from real `ResidualObligation` signals.
/// `None` for [`ResidualDecision::NoWork`] — there is nothing to measure
/// (the goal already holds; a zero-everything `MassVector` would be
/// refused by `MassVector::project` anyway, and rightly so: a frontier box
/// with genuinely nothing outstanding should not receive a fabricated
/// positive score just to have a number to report).
///
/// Only two of the six dimensions have a real signal available at this
/// integration layer (`unresolved_goal_mass`, `candidate_action_mass`,
/// both straight from `ResidualObligation`); `cache_novelty_mass` is set to
/// `1.0` because this function is only ever called after a genuine cache
/// miss (a real, if binary, signal); `semantic_novelty_mass`/
/// `resource_pressure_mass`/`temporal_pressure_mass` are always `0.0` —
/// honestly zero, not measured, because `Pddl8GroundAction` (classical
/// STRIPS8 grounding) carries no numeric-fluent or temporal-constraint data
/// for this integration layer to read (the identical limitation
/// `crate::causal`'s module doc comment states for `invariants_stable`/
/// `numeric_flow`/`trajectory`).
fn measure_frontier(residual: &ResidualDecision) -> Option<PositiveMass> {
    let obligation = match residual {
        ResidualDecision::NoWork => return None,
        ResidualDecision::Residual(o) => o,
    };
    let mv = MassVector {
        unresolved_goal_mass: obligation.unsatisfied_goal_atoms.len() as f64,
        candidate_action_mass: obligation.candidate_supporting_actions.len() as f64,
        semantic_novelty_mass: 0.0,
        resource_pressure_mass: 0.0,
        temporal_pressure_mass: 0.0,
        cache_novelty_mass: 1.0,
    };
    mv.project().ok()
}

fn bcinr_pddl_positive_distribution(mass: PositiveMass) -> crate::mfw::PositiveDistribution<u8> {
    crate::mfw::PositiveDistribution::new(vec![(0u8, mass)])
        .expect("single non-empty entry is never an empty distribution")
}

/// Map a solved [`Pddl8Tape`]'s ops back into
/// [`bcinr_mfw_ir::ActionOccurrence`]s indexed into `epoch_actions` by
/// ground-action label (labels are unique per grounded instantiation —
/// `schema_name(arg0,arg1,...)` — so this lookup is exact, not
/// approximate).
fn occurrences_from_tape(
    tape: &Pddl8Tape,
    epoch_actions: &[Pddl8GroundAction],
) -> Vec<bcinr_mfw_ir::ActionOccurrence> {
    let index_by_label: BTreeMap<&str, u64> = epoch_actions
        .iter()
        .enumerate()
        .map(|(i, a)| (a.label.as_str(), i as u64))
        .collect();
    tape.ops
        .iter()
        .filter_map(|op| {
            index_by_label.get(op.action.label.as_str()).map(|&action| {
                bcinr_mfw_ir::ActionOccurrence {
                    id: bcinr_mfw_ir::ActionOccurrenceId(op.index as u32),
                    action,
                }
            })
        })
        .collect()
}

// ---------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capability::DefaultCapabilityProfile;

    const DOMAIN: &str = "(define (domain d) \
                           (:requirements :strips) \
                           (:predicates (p) (q) (r)) \
                           (:action a1 :parameters () :precondition (p) :effect (q)) \
                           (:action a2 :parameters () :precondition (q) :effect (r)))";
    const PROBLEM: &str = "(define (problem pr) (:domain d) (:init (p)) (:goal (r)))";

    fn bounds() -> EpochBounds {
        EpochBounds {
            max_ground_actions: 64,
            max_plan_depth: 64,
            max_search_steps: 1000,
            max_partition_boxes: 8,
        }
    }

    fn new_planner() -> DefaultMfwPlanner {
        MfwPlanner::new(
            GoalReachabilityHorizon,
            bcinr_powl::projection::PowlProjector,
            bounds(),
            QValue::new(1.0).unwrap(),
            2,
            20,
        )
    }

    #[test]
    fn plan_end_to_end_produces_a_planned_workflow_with_all_expected_pieces() {
        let mut planner = new_planner();
        let profile = DefaultCapabilityProfile;
        let workflow = planner.plan(DOMAIN, PROBLEM, &profile).unwrap();

        assert!(!workflow.validated_plan.tape.ops.is_empty());
        assert!(workflow.validated_plan.goal_reached);
        assert_eq!(workflow.causal_plan.occurrences.len(), 2);
        assert_eq!(workflow.concurrency.event_count, 2);
        assert!(!workflow.powl_model.nodes.is_empty());
        assert_ne!(workflow.planning_receipt.hash, Digest::ZERO);
        assert!(!workflow.cache_hit);
    }

    #[test]
    fn plan_is_deterministic_across_two_runs() {
        let profile = DefaultCapabilityProfile;
        let mut planner1 = new_planner();
        let workflow1 = planner1.plan(DOMAIN, PROBLEM, &profile).unwrap();

        let mut planner2 = new_planner();
        let workflow2 = planner2.plan(DOMAIN, PROBLEM, &profile).unwrap();

        assert_eq!(
            workflow1.planning_receipt.hash,
            workflow2.planning_receipt.hash
        );
        assert_eq!(
            workflow1.projection_receipt.hash,
            workflow2.projection_receipt.hash
        );
    }

    #[test]
    fn second_plan_call_on_the_same_planner_records_a_cache_hit() {
        let mut planner = new_planner();
        let profile = DefaultCapabilityProfile;
        let first = planner.plan(DOMAIN, PROBLEM, &profile).unwrap();
        assert!(!first.cache_hit);
        let second = planner.plan(DOMAIN, PROBLEM, &profile).unwrap();
        assert!(second.cache_hit);
        // The plan/causal/concurrency/projection content is identical
        // either way (the portfolio still ran on both calls — real gap #1)
        // — only the *planning receipt's* `cache_evidence` digest differs,
        // because it genuinely folds in whether this call was a cache hit.
        assert_eq!(first.causal_plan.digest, second.causal_plan.digest);
        assert_eq!(
            first.projection_receipt.hash,
            second.projection_receipt.hash
        );
        assert_ne!(first.planning_receipt.hash, second.planning_receipt.hash);
    }

    /// Exercises the `cache_hit == Some(false)` short-circuit at `plan()`'s
    /// line "if cache_hit == Some(false) { return
    /// Err(MfwPlanError::CachedUnreachable) }" for the first time — this
    /// module's own doc comment ("Real gap #1") called that path "a real,
    /// sound use of the cache, not a stub," but `plan()`'s own pipeline
    /// only ever calls `cache.admit` after `goal_reached` is already `true`
    /// (every non-`Found` portfolio outcome and the `!goal_reached` case
    /// return `Err` before reaching the `admit` call), so nothing in normal
    /// use of `plan()` had ever populated the cache with a `false` entry,
    /// and no test exercised this branch. Seed one by hand (via the `pub
    /// cache` field, computing the same `(state_digest, theory_digest)` key
    /// `plan()`'s own pipeline would derive for `DOMAIN`/`PROBLEM`) and
    /// confirm the short-circuit genuinely fires.
    #[test]
    fn a_hand_seeded_false_cache_entry_makes_plan_short_circuit_with_cached_unreachable() {
        let mut planner = new_planner();
        let profile = DefaultCapabilityProfile;

        // Reproduce exactly the key `plan()` itself would compute for
        // (DOMAIN, PROBLEM) — see `plan()`'s own body, lines 296-320.
        let domain31 = domain31_from_pddl(DOMAIN).unwrap();
        let problem31 = problem31_from_pddl(PROBLEM).unwrap();
        let domain8 = domain_from_pddl(DOMAIN).unwrap();
        let problem8 = problem_from_pddl(PROBLEM).unwrap();
        let admitted = admit_planning_task(&domain31, &problem31, &profile)
            .into_result()
            .unwrap();
        let gp = GroundProblem::build(&domain8, &problem8, None).unwrap();
        let key_state_digest = state_digest(&gp.initial_state);
        let key_theory_digest = admitted.theory_digest;

        // A `PlanningResult` whose final state does not contain the goal —
        // `GoalReachabilityHorizon::observe` on this is `false` by
        // construction (`result.goal.iter().all(|g|
        // result.final_state.contains(g))` over an empty final state and a
        // non-empty goal).
        let unreachable_result =
            PlanningResult::new(std::collections::BTreeSet::new(), gp.goal.clone(), 0, 0.0);
        assert!(
            !GoalReachabilityHorizon.observe(&unreachable_result),
            "fixture must genuinely observe as unreachable, or this test proves nothing"
        );
        planner
            .cache
            .admit(key_state_digest, key_theory_digest, &unreachable_result);
        assert_eq!(
            planner.cache.lookup(key_state_digest, key_theory_digest),
            Some(&false),
            "the seeded entry must be a real cache hit under the same key plan() uses"
        );

        let outcome = planner.plan(DOMAIN, PROBLEM, &profile);
        assert!(
            matches!(outcome, Err(MfwPlanError::CachedUnreachable)),
            "expected Err(CachedUnreachable) from the standing-false-hit short-circuit, got \
             {outcome:?}"
        );
    }

    /// The exact false-negative risk the module doc comment's "Real gap #1"
    /// section now names: `self.cache` is a persistent field reused across
    /// `plan()` calls with *different* `domain_text`, keyed by
    /// `(state_digest, theory_digest)`. If two structurally-different
    /// domains shared a `theory_digest` (true before
    /// `capability::domain_problem_digest` was deepened to hash action
    /// bodies, not just names) and happened to also share `state_digest`
    /// (identical `:init` text, as here), a `false` entry cached under one
    /// domain's key would incorrectly short-circuit `plan()` for the
    /// *other* domain too, returning `Err(CachedUnreachable)` for a domain
    /// that is actually solvable.
    ///
    /// `domain_a`/`domain_b` below share every name (domain name,
    /// predicate names, action name) and the same `:init` text, but have
    /// swapped precondition/effect bodies: under `domain_b`, `(goal (p))`
    /// is genuinely unreachable (the only action produces `q`, not `p`);
    /// under `domain_a`, it is trivially reachable in one step.
    #[test]
    fn a_false_cache_entry_for_one_domain_does_not_leak_into_a_same_named_different_domain() {
        const SWAP_PROBLEM: &str = "(define (problem pr) (:domain d) (:init) (:goal (p)))";
        const DOMAIN_A_REACHABLE: &str = "(define (domain d) (:predicates (p) (q)) \
             (:action a1 :parameters () :precondition () :effect (p)))";
        const DOMAIN_B_UNREACHABLE: &str = "(define (domain d) (:predicates (p) (q)) \
             (:action a1 :parameters () :precondition () :effect (q)))";

        let mut planner = new_planner();
        let profile = DefaultCapabilityProfile;

        // Hand-seed a genuine `false` entry for domain_b's real key (mirrors
        // the preceding test: `plan()` itself never writes `false`, see
        // "Real gap #1").
        let domain31_b = domain31_from_pddl(DOMAIN_B_UNREACHABLE).unwrap();
        let problem31 = problem31_from_pddl(SWAP_PROBLEM).unwrap();
        let domain8_b = domain_from_pddl(DOMAIN_B_UNREACHABLE).unwrap();
        let problem8 = problem_from_pddl(SWAP_PROBLEM).unwrap();
        let admitted_b = admit_planning_task(&domain31_b, &problem31, &profile)
            .into_result()
            .unwrap();
        let gp_b = GroundProblem::build(&domain8_b, &problem8, None).unwrap();
        let state_digest_b = state_digest(&gp_b.initial_state);
        let unreachable_result =
            PlanningResult::new(std::collections::BTreeSet::new(), gp_b.goal.clone(), 0, 0.0);
        planner.cache.admit(
            state_digest_b,
            admitted_b.theory_digest,
            &unreachable_result,
        );

        // Sanity: domain_a's real state_digest is identical (same `:init`
        // text between both domains), but its theory_digest must differ --
        // otherwise this test would not actually be exercising the
        // collision scenario it claims to.
        let domain31_a = domain31_from_pddl(DOMAIN_A_REACHABLE).unwrap();
        let domain8_a = domain_from_pddl(DOMAIN_A_REACHABLE).unwrap();
        let admitted_a = admit_planning_task(&domain31_a, &problem31, &profile)
            .into_result()
            .unwrap();
        let gp_a = GroundProblem::build(&domain8_a, &problem8, None).unwrap();
        assert_eq!(
            state_digest(&gp_a.initial_state),
            state_digest_b,
            "fixture must share state_digest across both domains, or this isn't the collision \
             scenario being tested"
        );
        assert_ne!(
            admitted_a.theory_digest, admitted_b.theory_digest,
            "domain_a and domain_b must have different theory_digest despite sharing every \
             name -- this is exactly what capability::domain_problem_digest's deepened \
             coverage is responsible for"
        );

        // The real assertion: planning domain_a (genuinely solvable) on the
        // same planner/cache must NOT be short-circuited by domain_b's
        // cached `false` entry.
        let outcome = planner.plan(DOMAIN_A_REACHABLE, SWAP_PROBLEM, &profile);
        assert!(
            outcome.is_ok(),
            "domain_a is genuinely solvable and must not be rejected via a same-state, \
             different-theory cached-unreachable entry that belongs to domain_b, got {outcome:?}"
        );
    }
}
