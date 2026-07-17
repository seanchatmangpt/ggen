//! Capability admission: what PDDL features this crate's planners *actually*
//! implement correctly, wired into a real admission gate so a caller-chosen
//! [`CapabilityProfile`] can refuse a domain that needs something the
//! profile marks unsupported instead of silently planning against it anyway
//! and producing a plan that quietly ignores part of the domain's semantics.
//!
//! # Per-feature accounting (the honest part)
//!
//! [`DefaultCapabilityProfile`] is not a guess — every rating below reflects
//! code actually read (and, where noted, actually run) this phase:
//!
//! - [`PddlFeature::Strips`], [`PddlFeature::Typing`] — `Exact`. The
//!   foundation; every passing test in this crate exercises both.
//! - [`PddlFeature::NegativePreconditions`] — `Exact`. `eval_condition`'s
//!   `Not` arm is a straightforward `!eval_condition(inner, ...)`, and it is
//!   genuinely load-bearing in a passing test:
//!   `capability_router::tests::same_file_edit_and_draft_conflict_and_sequence`
//!   only passes because `(at start (not (locked ?f)))` really gates
//!   scheduling.
//! - [`PddlFeature::Disjunction`] — `Exact`. `eval_condition`'s `Or` arm
//!   (`.any(...)`) is reachable through the same durative-condition pipeline
//!   already proven live for `Not`/`Compare`/`Timed`, and is now itself
//!   proven end-to-end by `tests/durative_disjunction.rs`: a
//!   `:durative-action`'s `(at start (or ...))` condition genuinely gates
//!   scheduling, including the adversarial case (only one of two disjuncts
//!   true still fires; neither true correctly blocks). Like
//!   `UniversalPreconditions`, this is only proven through the one
//!   parser-reachable path that exists — `:action`/`:goal` preconditions
//!   still can't carry a `PddlCondition::Or` at all in this crate.
//! - [`PddlFeature::Equality`] — `Unsupported`. Nothing in this crate
//!   special-cases the built-in `=` predicate (no equality facts are
//!   synthesized, no identity check exists anywhere in `ground/mod.rs`) — a
//!   domain declaring `:equality` gets `=` treated as an arbitrary
//!   uninterpreted predicate name, which is silently wrong, not merely
//!   incomplete.
//! - [`PddlFeature::ExistentialPreconditions`] — `Unsupported`.
//!   `ground::eval_quantifier`'s `Exists` arm is real and directly
//!   unit-tested (`ground::quantifier_tests::exists_*`), but no parser path
//!   in this crate ever constructs a `PddlCondition::Exists` that reaches
//!   `eval_condition`: the `pddl` crate's durative-action-condition grammar
//!   (`da-GD`) has no `exists` production (only `forall` — see
//!   `src/parse.rs`'s `lower_da_gd`), plain `:action` preconditions and
//!   `:goal` can't carry any `PddlCondition` at all (both are flattened to
//!   `Vec<Pddl8Atom>`), and `ground_derived_schema`'s local `ground_condition`
//!   helper drops `Exists` bodies (`_ => None`). A correct-but-unreachable
//!   evaluator is still `Unsupported` at the admission layer — this is the
//!   explicit, honest choice the mission brief allows for a feature whose
//!   evaluator works but has no way to receive real input.
//! - [`PddlFeature::UniversalPreconditions`] — `Approximate`. The mirror
//!   image: `eval_quantifier`'s `Forall` arm *is* reachable, through exactly
//!   one path — a `:durative-action`'s `:condition` — and that path is
//!   proven correct end-to-end by `tests/durative_quantifiers.rs` (including
//!   the adversarial case the pre-fix stub got wrong: one item not ready).
//!   `Approximate`, not `Exact`, because the same feature in a plain
//!   `:action` precondition or `:goal` still silently vanishes.
//! - [`PddlFeature::ConditionalEffects`] — `Unsupported`. A real bug, not a
//!   gap: `ground::apply_effect_ground`'s `PddlEffect::When { condition,
//!   effects }` arm destructures `condition` with `..` and **never
//!   evaluates it** — the effects fire unconditionally. `PddlEffect::Forall`
//!   effects have the same disease (`vars` is discarded, effects apply once
//!   without any substitution/enumeration over objects). Neither bug was
//!   introduced this phase; both are surfaced here rather than silently
//!   inherited as an `Exact`/`Approximate` rating that would overclaim.
//!   [`admit_planning_task`] refuses on this feature two ways: the
//!   declared-requirement loop (any `:requirements` entry that
//!   [`requirement_implies`] maps to `ConditionalEffects`), and a separate
//!   content scan (`effect_list_uses_conditional_or_quantified`) over every
//!   action's/durative-action's actual effect AST, so a domain that uses
//!   `when`/`forall` in an effect *without* declaring the requirement is
//!   still refused instead of silently admitted.
//! - [`PddlFeature::NumericFluents`] — `Approximate`. Numeric comparisons
//!   (`eval_numeric`/`Compare`) are genuinely evaluated and load-bearing
//!   (`capability_router`'s `(>= (attention) 1)` gates every real test in
//!   that module) — but only through the durative-action pipeline. Plain
//!   `:action` preconditions cannot carry a numeric comparison at all
//!   (`Pddl8ActionSchema.preconditions: Vec<Pddl8Atom>` has no slot for one),
//!   so `src/parse.rs`'s `collect_gd` silently drops it instead of
//!   rejecting the domain — directly demonstrated by
//!   `tests/semantic_falsifier.rs`'s `test_numeric_cost`, `#[ignore]`d with
//!   this exact citation rather than left to fail unexplained.
//! - [`PddlFeature::NumericEffects`] — `Exact`. `apply_numeric_effect`
//!   correctly implements `Assign`/`Increase`/`Decrease`/`ScaleUp`/
//!   `ScaleDown`, and it is the only numeric-effect surface `:numeric-fluents`
//!   realistically implies in this crate (paired with `:durative-actions`,
//!   the classical `Pddl8GroundAction` has no numeric-effect field at all —
//!   a structural, advertised STRIPS8 scope limit, not a silent gap).
//! - [`PddlFeature::DurativeActions`] — `Exact`. `GroundTemporalProblem` is
//!   the best-tested part of this crate (`tests/capacity.rs`,
//!   `tests/proposer_substrate.rs`, `capability_router`, the DfCM crown
//!   suite all exercise it).
//! - [`PddlFeature::TimedInitialLiterals`] — `Exact`.
//!   `tests/semantic_falsifier.rs`'s `test_til_schedule` passes and directly
//!   checks TIL-driven makespan values.
//! - [`PddlFeature::DerivedPredicates`] — `Approximate`.
//!   `compute_derived_closure`'s fixpoint iteration is real and
//!   `test_derived_predicates` passes — but `ground_derived_schema`'s
//!   `ground_condition` helper has no `Forall`/`Exists` arm, so a derived
//!   predicate whose body quantifies is silently dropped in full (not
//!   partially evaluated), never appearing in `derived_predicates` at all.
//! - [`PddlFeature::TrajectoryConstraints`] — `Unsupported`.
//!   `crate::parse::problem_from_pddl` and `problem31_from_pddl` both
//!   hardcode `preferences: vec![]` — `(:constraints ...)` is never parsed
//!   into `Pddl8Problem`/`Pddl31Problem` at all, by either function, so
//!   `GroundProblem::build`'s/`GroundTemporalProblem::build`'s
//!   `self.constraints` is always empty regardless of what a domain
//!   declares. Directly demonstrated by `test_trajectory_constraints`,
//!   `#[ignore]`d with this citation.
//! - [`PddlFeature::Preferences`] — `Unsupported`. Same root cause
//!   (`preferences: vec![]`, always), and even setting that aside, nothing
//!   in this crate computes soft-constraint violation cost against a metric.
//! - [`PddlFeature::Metrics`] — `Unsupported`. `problem.metric: Option<Metric>`
//!   is parsed but never consulted: both `GroundProblem::find_plan` and
//!   `GroundTemporalProblem::find_temporal_plan_with_fn_overrides`
//!   hardcode `metric_value: None` on every `TemporalPlan` they return, and
//!   classical `find_plan` has no metric field on `Pddl8Tape` to populate at
//!   all — no plan is ever selected or ranked by a metric.
//!
//! None of the above bugs (`ConditionalEffects`, the two `preferences: vec![]`
//! sites, `metric_value: None`) were introduced by this phase — they're
//! reported here because an honest [`CapabilityProfile`] cannot be built
//! without finding them, and finding them without saying so would be exactly
//! the silent-overclaim this module exists to prevent.

use std::collections::BTreeSet;

use bcinr_mfw_ir::{
    Digest, EpochBounds, InconsistencyWitness, PlannerOutcome, PlanningEpochId, UnsupportedFeature,
};
use wasm4pm_compat::pddl::{
    CompareOp, DurationConstraint, NumericEffect, NumericExpr, NumericOp, Pddl31Domain,
    Pddl31Problem, Pddl8Atom, Pddl8GroundAction, Pddl8GroundAtom, PddlCondition, PddlEffect,
    PddlFunction, TimeSpecifier,
};

use crate::ground::GroundProblem;

/// The sixteen PDDL-requirement-shaped capabilities this crate's planners
/// might need. Not a 1:1 mirror of the `pddl` crate's `Requirement` enum —
/// see [`requirement_implies`] for how the wider requirement vocabulary
/// (`Adl`, `Fluents`, `QuantifiedPreconditions`, `ObjectFluents`, ...) maps
/// onto these sixteen (or, for `ObjectFluents`, is rejected structurally
/// instead, since it has no corresponding `PddlFeature`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PddlFeature {
    Strips,
    Typing,
    NegativePreconditions,
    Disjunction,
    Equality,
    ExistentialPreconditions,
    UniversalPreconditions,
    ConditionalEffects,
    NumericFluents,
    NumericEffects,
    DurativeActions,
    TimedInitialLiterals,
    DerivedPredicates,
    TrajectoryConstraints,
    Preferences,
    Metrics,
}

/// All sixteen [`PddlFeature`] variants, in declaration order — used to
/// iterate the full feature set (e.g. when checking a domain's requirements
/// against a profile).
pub const ALL_PDDL_FEATURES: [PddlFeature; 16] = [
    PddlFeature::Strips,
    PddlFeature::Typing,
    PddlFeature::NegativePreconditions,
    PddlFeature::Disjunction,
    PddlFeature::Equality,
    PddlFeature::ExistentialPreconditions,
    PddlFeature::UniversalPreconditions,
    PddlFeature::ConditionalEffects,
    PddlFeature::NumericFluents,
    PddlFeature::NumericEffects,
    PddlFeature::DurativeActions,
    PddlFeature::TimedInitialLiterals,
    PddlFeature::DerivedPredicates,
    PddlFeature::TrajectoryConstraints,
    PddlFeature::Preferences,
    PddlFeature::Metrics,
];

/// How faithfully this crate's planners implement a given [`PddlFeature`].
///
/// This is a closed, four-way classification along the same "never round up
/// silently" discipline as `bcinr_mfw_ir::FormalStanding`:
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticSupport {
    /// Genuinely correct wherever it is reachable, and reachable through
    /// every path a domain declaring the corresponding requirement would
    /// realistically use.
    Exact,
    /// Genuinely correct, but only within a caller-supplied structural
    /// bound (e.g. a search/grounding limit) — not used by
    /// [`DefaultCapabilityProfile`] today (no feature in this crate is
    /// bound-limited rather than simply reachable or not), but part of the
    /// closed vocabulary for profiles built by later phases.
    BoundedExact,
    /// Genuinely correct wherever it is reachable, but reachable through
    /// only part of the surface a domain declaring the requirement might
    /// use (see the module doc comment for exactly which paths, per
    /// feature).
    Approximate,
    /// Not implemented, silently wrong if assumed, or real but practically
    /// unreachable from any parser entry point — never claim a plan
    /// respected this feature.
    Unsupported,
}

/// A policy: what level of support this planner instance claims for each
/// [`PddlFeature`]. Implementations other than [`DefaultCapabilityProfile`]
/// let a caller be *more* conservative (e.g. downgrade `Approximate` to
/// `Unsupported` for a safety-critical deployment) — [`admit_planning_task`]
/// never grants more trust than the profile it is given.
pub trait CapabilityProfile {
    fn support(&self, feature: PddlFeature) -> SemanticSupport;
}

/// The profile reflecting what this crate's planners *actually* implement
/// today, per the module doc comment's per-feature accounting.
#[derive(Debug, Clone, Copy, Default)]
pub struct DefaultCapabilityProfile;

impl CapabilityProfile for DefaultCapabilityProfile {
    fn support(&self, feature: PddlFeature) -> SemanticSupport {
        use SemanticSupport::{Approximate, Exact, Unsupported};
        match feature {
            PddlFeature::Strips => Exact,
            PddlFeature::Typing => Exact,
            PddlFeature::NegativePreconditions => Exact,
            PddlFeature::Disjunction => Exact,
            PddlFeature::Equality => Unsupported,
            PddlFeature::ExistentialPreconditions => Unsupported,
            PddlFeature::UniversalPreconditions => Approximate,
            PddlFeature::ConditionalEffects => Unsupported,
            PddlFeature::NumericFluents => Approximate,
            PddlFeature::NumericEffects => Exact,
            PddlFeature::DurativeActions => Exact,
            PddlFeature::TimedInitialLiterals => Exact,
            PddlFeature::DerivedPredicates => Approximate,
            PddlFeature::TrajectoryConstraints => Unsupported,
            PddlFeature::Preferences => Unsupported,
            PddlFeature::Metrics => Unsupported,
        }
    }
}

/// True iff `req` (a `Pddl31Domain::requirements` entry — the `Debug`
/// spelling of the `pddl` crate's `Requirement` enum, e.g.
/// `"NumericFluents"`, **not** the PDDL surface syntax's `:kebab-case`
/// spelling; see `src/parse.rs`'s `domain31_from_pddl`, which populates this
/// field via `format!("{r:?}")`) implies `feature`.
///
/// Shorthand requirements (`Adl`, `Fluents`, `QuantifiedPreconditions`) are
/// expanded to the same constituent set the `pddl` crate's own
/// `Requirement::expand()` uses. `ObjectFluents`/`DurationInequalities`/
/// `ContinuousEffects`/`ActionCosts` never imply any [`PddlFeature`] here —
/// `ObjectFluents` is rejected structurally by [`admit_planning_task`]
/// instead (see that function), `ContinuousEffects` is likewise rejected
/// structurally by [`admit_planning_task`] but via a *content* scan
/// (`effect_list_uses_continuous_effect_sentinel`) rather than this
/// declared-requirement mapping — declaring `:continuous-effects` without
/// ever using the construct is harmless, so keying the refusal on the
/// requirement string the way `ObjectFluents` does would both needlessly
/// refuse harmless domains and miss a domain that uses the construct
/// without declaring it — and the other two have no corresponding feature
/// in this crate's admission vocabulary at all (this crate does not
/// implement duration inequalities beyond the min/max bounds
/// `resolve_duration` already resolves, and `ActionCosts`'s restricted-metric
/// semantics fall under [`PddlFeature::Metrics`], which is
/// already `Unsupported`).
fn requirement_implies(req: &str, feature: PddlFeature) -> bool {
    match req {
        "Strips" => feature == PddlFeature::Strips,
        "Typing" => feature == PddlFeature::Typing,
        "NegativePreconditions" => feature == PddlFeature::NegativePreconditions,
        "DisjunctivePreconditions" => feature == PddlFeature::Disjunction,
        "Equality" => feature == PddlFeature::Equality,
        "ExistentialPreconditions" => feature == PddlFeature::ExistentialPreconditions,
        "UniversalPreconditions" => feature == PddlFeature::UniversalPreconditions,
        "QuantifiedPreconditions" => matches!(
            feature,
            PddlFeature::ExistentialPreconditions | PddlFeature::UniversalPreconditions
        ),
        "ConditionalEffects" => feature == PddlFeature::ConditionalEffects,
        "Fluents" | "NumericFluents" => {
            matches!(
                feature,
                PddlFeature::NumericFluents | PddlFeature::NumericEffects
            )
        }
        "Adl" => matches!(
            feature,
            PddlFeature::Strips
                | PddlFeature::Typing
                | PddlFeature::NegativePreconditions
                | PddlFeature::Disjunction
                | PddlFeature::Equality
                | PddlFeature::ExistentialPreconditions
                | PddlFeature::UniversalPreconditions
                | PddlFeature::ConditionalEffects
        ),
        "DurativeActions" => feature == PddlFeature::DurativeActions,
        "DerivedPredicates" => feature == PddlFeature::DerivedPredicates,
        // TimedInitialLiterals implies DurativeActions per the `pddl` crate's
        // own doc comment on `Requirement::TimedInitialLiterals`.
        "TimedInitialLiterals" => matches!(
            feature,
            PddlFeature::TimedInitialLiterals | PddlFeature::DurativeActions
        ),
        "Preferences" => feature == PddlFeature::Preferences,
        "Constraints" => feature == PddlFeature::TrajectoryConstraints,
        "ActionCosts" => feature == PddlFeature::Metrics,
        _ => false,
    }
}

/// True iff `effect` is (or contains, through a `Timed` wrapper) a
/// `PddlEffect::When` or `PddlEffect::Forall` node — i.e. the action body
/// actually *uses* a conditional or quantified effect construct, independent
/// of what the domain's `:requirements` declare.
///
/// This exists because `ground::apply_effect_ground` mishandles both
/// constructs unconditionally (see this module's doc comment on
/// [`PddlFeature::ConditionalEffects`]): its `When` arm destructures
/// `condition` with `..` and fires `effects` regardless of whether the
/// condition holds, and its `Forall` arm discards `vars` and fires `effects`
/// exactly once instead of enumerating every object binding. Gating
/// admission on the *declared* `:conditional-effects` requirement string
/// alone (see [`requirement_implies`] and the loop in
/// [`admit_planning_task`]) misses a domain that uses `when`/`forall` in an
/// effect without ever declaring the requirement — this walk inspects the
/// actual effect AST instead of trusting the domain author's requirements
/// list.
///
/// # Complexity
/// O(n) in the number of `PddlEffect` nodes reachable from `effect` — a
/// single linear walk through `Timed` wrappers, no backtracking.
fn effect_uses_conditional_or_quantified(effect: &PddlEffect) -> bool {
    match effect {
        PddlEffect::When { .. } | PddlEffect::Forall { .. } => true,
        PddlEffect::Timed(_, inner) => effect_uses_conditional_or_quantified(inner),
        PddlEffect::Add(_) | PddlEffect::Del(_) | PddlEffect::Numeric(_) => false,
    }
}

/// True iff any effect in `effects` uses a conditional/quantified construct —
/// see [`effect_uses_conditional_or_quantified`] for what counts and why
/// this check exists.
///
/// # Complexity
/// O(n) in the total number of `PddlEffect` nodes across `effects`.
fn effect_list_uses_conditional_or_quantified(effects: &[PddlEffect]) -> bool {
    effects.iter().any(effect_uses_conditional_or_quantified)
}

/// True iff `effect` is (or, through a `Timed`/`When`/`Forall` wrapper,
/// contains) the sentinel `PddlEffect::Add` atom
/// `crate::parse::CONTINUOUS_EFFECT_SENTINEL_PRED` fabricates in place of a
/// real continuous numeric effect — see that constant's doc comment on
/// [`crate::parse::lower_timed_effect`] for the fabrication this detects,
/// and [`admit_planning_task`]'s continuous-effects check for why a
/// sentinel-fingerprint scan, not a preserved AST node, is what's available
/// here: `PddlEffect` (an external `wasm4pm_compat` type) has no dedicated
/// continuous-effect variant, so — unlike `PddlEffect::When`/`Forall`, which
/// survive lowering intact — the real construct is destroyed before
/// `Pddl31Domain` is built at all, and this sentinel is the only surviving
/// signal that it was ever there.
///
/// # Complexity
/// O(n) in the number of `PddlEffect` nodes reachable from `effect`.
fn effect_uses_continuous_effect_sentinel(effect: &PddlEffect) -> bool {
    match effect {
        PddlEffect::Add(a) => a.pred == crate::parse::CONTINUOUS_EFFECT_SENTINEL_PRED,
        PddlEffect::Timed(_, inner) => effect_uses_continuous_effect_sentinel(inner),
        PddlEffect::When { effects, .. } | PddlEffect::Forall { effects, .. } => {
            effects.iter().any(effect_uses_continuous_effect_sentinel)
        }
        PddlEffect::Del(_) | PddlEffect::Numeric(_) => false,
    }
}

/// True iff any effect in `effects` carries the continuous-effect sentinel —
/// see [`effect_uses_continuous_effect_sentinel`].
///
/// # Complexity
/// O(n) in the total number of `PddlEffect` nodes across `effects`.
fn effect_list_uses_continuous_effect_sentinel(effects: &[PddlEffect]) -> bool {
    effects.iter().any(effect_uses_continuous_effect_sentinel)
}

/// True iff `effect` is (or, through a `Timed`/`When`/`Forall` wrapper,
/// contains) the sentinel `PddlEffect::Add` atom
/// `crate::parse::OBJECT_FLUENT_SENTINEL_PRED` fabricates in place of a real
/// object-fluent assignment effect — see that constant's doc comment on
/// [`crate::parse::lower_primitive_effect_full`]'s `AssignObjectFluent` arm.
/// Same rationale and shape as
/// [`effect_uses_continuous_effect_sentinel`]: `PddlEffect` has no dedicated
/// object-fluent-assignment variant, so the sentinel fingerprint is the only
/// surviving signal after lowering.
///
/// # Complexity
/// O(n) in the number of `PddlEffect` nodes reachable from `effect`.
fn effect_uses_object_fluent_sentinel(effect: &PddlEffect) -> bool {
    match effect {
        PddlEffect::Add(a) => a.pred == crate::parse::OBJECT_FLUENT_SENTINEL_PRED,
        PddlEffect::Timed(_, inner) => effect_uses_object_fluent_sentinel(inner),
        PddlEffect::When { effects, .. } | PddlEffect::Forall { effects, .. } => {
            effects.iter().any(effect_uses_object_fluent_sentinel)
        }
        PddlEffect::Del(_) | PddlEffect::Numeric(_) => false,
    }
}

/// True iff any effect in `effects` carries the object-fluent sentinel —
/// see [`effect_uses_object_fluent_sentinel`].
///
/// # Complexity
/// O(n) in the total number of `PddlEffect` nodes across `effects`.
fn effect_list_uses_object_fluent_sentinel(effects: &[PddlEffect]) -> bool {
    effects.iter().any(effect_uses_object_fluent_sentinel)
}

/// A domain + problem that passed [`admit_planning_task`]'s structural and
/// capability checks. Cheap to construct further planning stages from —
/// `theory_digest` (see [`domain_problem_digest`] for exactly which fields
/// it walks) content-addresses the domain's/problem's action bodies,
/// durations, and `:init`/`:goal` content, not just their names — it is
/// *not* the same construction as `crate::llm_bridge::compute_domain_witness`/
/// `compute_problem_witness` (those remain name/requirements-only, for a
/// human-readable LLM-facing witness string, not a semantic content digest).
/// `theory_digest` still does not cover `:constraints`/`:preferences`/
/// `:metric`/PDDL+ `:process`/`:event` — see [`domain_problem_digest`]'s doc
/// comment for the precise, current coverage boundary. Two domains/problems
/// differing only in one of those uncovered fields will still collide.
#[derive(Debug, Clone)]
pub struct AdmittedPlanningTask {
    pub domain: Pddl31Domain,
    pub problem: Pddl31Problem,
    pub theory_digest: Digest,
}

/// Structurally validate `domain`/`problem` and check every requirement
/// `domain` declares against `profile`, refusing (`PlannerOutcome::Unsupported`)
/// rather than silently proceeding for anything `profile` marks
/// `SemanticSupport::Unsupported`.
///
/// This does **not** ground or search — it is the admission gate that runs
/// before either, so a domain requiring an unsupported feature never reaches
/// `GroundProblem::build`/`GroundTemporalProblem::build` at all.
pub fn admit_planning_task(
    domain: &Pddl31Domain, problem: &Pddl31Problem, profile: &dyn CapabilityProfile,
) -> PlannerOutcome<AdmittedPlanningTask> {
    if domain.name.is_empty() {
        return PlannerOutcome::Inconsistent(InconsistencyWitness {
            kind_name: "empty-domain-name".to_string(),
            context: "admit_planning_task: domain.name must be non-empty".to_string(),
            digest: Digest::ZERO,
        });
    }
    if domain.predicates.is_empty()
        && domain.actions.is_empty()
        && domain.durative_actions.is_empty()
    {
        return PlannerOutcome::Inconsistent(InconsistencyWitness {
            kind_name: "empty-domain-structure".to_string(),
            context: "admit_planning_task: domain has no predicates and no actions".to_string(),
            digest: Digest::ZERO,
        });
    }
    if problem.domain != domain.name {
        return PlannerOutcome::Inconsistent(InconsistencyWitness {
            kind_name: "domain-problem-mismatch".to_string(),
            context: format!(
                "admit_planning_task: problem references domain '{}' but admitted domain is '{}'",
                problem.domain, domain.name
            ),
            digest: Digest::ZERO,
        });
    }

    // Structural rejection for `:object-fluents`: no `PddlFeature` variant
    // represents it (see `requirement_implies`'s doc comment) because no
    // object-fluent representation exists anywhere in this grounder —
    // supersedes the old, dead `ground::check_capabilities`, which checked
    // this same requirement but against the wrong string format (`:kebab-case`
    // against a `PascalCase`-populated field, so it could never actually
    // fire) and had no callers.
    //
    // Refuses on two independent signals, for the same reason the
    // `ConditionalEffects`/`ContinuousEffects` checks do: the declared
    // requirement string alone misses a domain that *uses*
    // `(assign (fluent) obj)`-shaped object-fluent effects without ever
    // declaring `:object-fluents` — `parse::lower_primitive_effect_full`'s
    // `AssignObjectFluent` arm fabricates a detectable
    // `parse::OBJECT_FLUENT_SENTINEL_PRED` sentinel atom in place of the
    // real assignment specifically so this scan can catch that case too
    // (see that arm's doc comment).
    //
    // This check covers object-fluent *effects* only. Object-fluent
    // *initial values* (`(= (at pkg1) loc1)`-style `:init` facts) are a
    // separate, lower-severity gap: `parse::problem31_from_pddl`'s
    // `InitElement::IsObject` arm silently drops them (no atom of any kind
    // added — a real information loss, but more honest than fabricating one)
    // regardless of whether the domain this check runs against ends up
    // admitted or refused, since it happens during *problem* parsing, not
    // here. See that arm's doc comment for the full disclosure.
    let declares_object_fluents = domain.requirements.iter().any(|r| r == "ObjectFluents");
    let uses_object_fluent_construct = domain
        .actions
        .iter()
        .any(|a| effect_list_uses_object_fluent_sentinel(&a.effect));
    if declares_object_fluents || uses_object_fluent_construct {
        return PlannerOutcome::Unsupported(UnsupportedFeature {
            feature_name: "object-fluents".to_string(),
            context: "PDDL 3.1 object-valued fluents have no representation anywhere in this \
                      grounder (only numeric fluents are modeled) — not one of the sixteen \
                      PddlFeature variants because there is nothing partial to rate; the domain \
                      is refused outright."
                .to_string(),
        });
    }

    // Structural rejection for continuous effects (`(increase fuel #t)`-shaped
    // constructs inside a `:durative-action`'s `:effect`): like
    // `ObjectFluents` above, no `PddlFeature` variant represents this
    // because this crate has no representation for a continuous, rate-based
    // numeric change at all. Unlike `ObjectFluents`, this is *not* keyed on
    // the declared `:continuous-effects` requirement string — declaring the
    // requirement without ever using the construct is harmless, so a
    // declaration-only check would both needlessly refuse harmless domains
    // and (symmetrically with the `ConditionalEffects` content scan above)
    // miss a domain that uses the construct without declaring it. Instead
    // this scans for `parse::CONTINUOUS_EFFECT_SENTINEL_PRED`, the
    // detectable fingerprint `parse::lower_timed_effect` fabricates in place
    // of every real continuous effect — see that constant's doc comment for
    // why a sentinel scan, not a preserved AST node, is what's available to
    // scan here (`PddlEffect` has no dedicated continuous-effect variant).
    for da in &domain.durative_actions {
        if effect_list_uses_continuous_effect_sentinel(&da.effects) {
            return PlannerOutcome::Unsupported(UnsupportedFeature {
                feature_name: "ContinuousEffects".to_string(),
                context: format!(
                    "durative action {:?} uses a continuous numeric effect (e.g. `(increase ... \
                     #t)`), which this crate has no representation for — \
                     parse::lower_timed_effect fabricates a meaningless sentinel fact atom in \
                     its place, so admitting this domain would let GroundTemporalProblem \
                     silently add a fake fact instead of applying the real numeric change",
                    da.name
                ),
            });
        }
    }

    for req in &domain.requirements {
        for &feature in &ALL_PDDL_FEATURES {
            if requirement_implies(req, feature)
                && profile.support(feature) == SemanticSupport::Unsupported
            {
                return PlannerOutcome::Unsupported(UnsupportedFeature {
                    feature_name: format!("{feature:?}"),
                    context: format!(
                        "domain requirement {req:?} implies PddlFeature::{feature:?}, which the \
                         given CapabilityProfile marks Unsupported"
                    ),
                });
            }
        }
    }

    // Content-based gate: the loop above only refuses a domain that
    // *declares* `:conditional-effects` in `:requirements`. That misses a
    // domain whose action bodies actually *use* `when`/`forall` in an
    // effect without ever declaring the requirement — `parse.rs`'s
    // `collect_conditional_effect` (the classical STRIPS8 lowering path)
    // silently drops a `when`'s condition and folds its effects into
    // unconditional `add_effects`/`del_effects`, and
    // `ground::apply_effect_ground` (the durative-action path) fires both
    // `When` and `Forall` effects without checking/enumerating them either.
    // Either way the result is a `PlannerOutcome::Found` plan that silently
    // misrepresents which effects actually held — a wrong answer, not a
    // refusal. Scan the real effect AST (`domain.actions`/
    // `domain.durative_actions`, both preserved faithfully by
    // `lower_action31`/`lower_durative_action`) instead of trusting the
    // declared requirements list.
    if profile.support(PddlFeature::ConditionalEffects) == SemanticSupport::Unsupported {
        for action in &domain.actions {
            if effect_list_uses_conditional_or_quantified(&action.effect) {
                return PlannerOutcome::Unsupported(UnsupportedFeature {
                    feature_name: "ConditionalEffects".to_string(),
                    context: format!(
                        "action {:?} uses a `when` or `forall` effect construct even though \
                         the domain's :requirements never declared :conditional-effects — \
                         admitting it would let ground::apply_effect_ground (or, for classical \
                         actions, parse.rs's collect_conditional_effect) silently misapply the \
                         effect instead of refusing the domain",
                        action.name
                    ),
                });
            }
        }
        for da in &domain.durative_actions {
            if effect_list_uses_conditional_or_quantified(&da.effects) {
                return PlannerOutcome::Unsupported(UnsupportedFeature {
                    feature_name: "ConditionalEffects".to_string(),
                    context: format!(
                        "durative action {:?} uses a `when` or `forall` effect construct even \
                         though the domain's :requirements never declared \
                         :conditional-effects — see the classical-action case above for why \
                         this must be refused rather than silently admitted",
                        da.name
                    ),
                });
            }
        }
    }

    let theory_digest = domain_problem_digest(domain, problem);
    PlannerOutcome::Found(AdmittedPlanningTask {
        domain: domain.clone(),
        problem: problem.clone(),
        theory_digest,
    })
}

/// Content-addressed digest of `domain`'s + `problem`'s structural identity,
/// deep enough to distinguish two domains/problems that differ only in
/// action bodies, durations, or `:init`/`:goal` content — not just in names.
///
/// This deliberately does **not** delegate to
/// `crate::llm_bridge::compute_domain_witness`/`compute_problem_witness`:
/// those two remain name/requirements-only (they exist to produce a short,
/// human-readable witness string for LLM-facing display, not a semantic
/// content digest), and this function now hashes strictly more than they
/// do — see each helper below for exactly which fields are walked.
///
/// Coverage: domain name, requirements, every predicate's full typed
/// signature, every function's full signature, every classical action's
/// params + precondition + effect tree, every durative action's params +
/// duration constraint + conditions + effects; problem name, domain
/// reference, objects, `:init` atoms, `:init` numeric-fluent values, timed
/// initial literals, and `:goal`. Not yet covered: `:constraints`,
/// `:preferences`, `:metric`, PDDL+ `:process`/`:event` blocks — a domain
/// or problem differing only in one of those would still digest-collide.
/// This is a real narrowing of (not a full close of) the "digest equality
/// != semantic equivalence" gap: the two are no longer conflatable for the
/// case this was reported against (a swapped precondition/effect on a
/// same-named action), but the digest still does not cover every field
/// `Pddl31Domain`/`Pddl31Problem` carry.
///
/// # Complexity
/// O(size of `domain`'s + `problem`'s AST) — data-dependent, not O(1):
/// every condition/effect node reachable from every action is visited
/// exactly once.
fn domain_problem_digest(domain: &Pddl31Domain, problem: &Pddl31Problem) -> Digest {
    let mut dbuf = Vec::new();
    dbuf.extend_from_slice(domain.name.as_bytes());
    for req in &domain.requirements {
        dbuf.extend_from_slice(req.as_bytes());
    }
    for (name, params) in &domain.predicates {
        dbuf.extend_from_slice(name.as_bytes());
        hash_typed_params(&mut dbuf, params);
    }
    for f in &domain.functions {
        hash_function(&mut dbuf, f);
    }
    for a in &domain.actions {
        dbuf.extend_from_slice(a.name.as_bytes());
        hash_typed_params(&mut dbuf, &a.params);
        hash_condition(&mut dbuf, &a.precondition);
        dbuf.extend_from_slice(&(a.effect.len() as u32).to_le_bytes());
        for e in &a.effect {
            hash_effect(&mut dbuf, e);
        }
    }
    for da in &domain.durative_actions {
        dbuf.extend_from_slice(da.name.as_bytes());
        hash_typed_params(&mut dbuf, &da.params);
        hash_duration_constraint(&mut dbuf, &da.duration);
        dbuf.extend_from_slice(&(da.conditions.len() as u32).to_le_bytes());
        for c in &da.conditions {
            hash_condition(&mut dbuf, c);
        }
        dbuf.extend_from_slice(&(da.effects.len() as u32).to_le_bytes());
        for e in &da.effects {
            hash_effect(&mut dbuf, e);
        }
    }
    let domain_digest = Digest::hash(&dbuf);

    let mut pbuf = Vec::new();
    pbuf.extend_from_slice(problem.name.as_bytes());
    pbuf.extend_from_slice(problem.domain.as_bytes());
    for (obj, typ) in &problem.objects {
        pbuf.extend_from_slice(obj.as_bytes());
        pbuf.extend_from_slice(typ.as_bytes());
    }
    pbuf.extend_from_slice(&(problem.init_atoms.len() as u32).to_le_bytes());
    for a in &problem.init_atoms {
        hash_atom(&mut pbuf, a);
    }
    pbuf.extend_from_slice(&(problem.init_fn_values.len() as u32).to_le_bytes());
    for (f, v) in &problem.init_fn_values {
        hash_function(&mut pbuf, f);
        pbuf.extend_from_slice(&v.to_bits().to_le_bytes());
    }
    pbuf.extend_from_slice(&(problem.timed_inits.len() as u32).to_le_bytes());
    for t in &problem.timed_inits {
        pbuf.extend_from_slice(&t.time.to_bits().to_le_bytes());
        hash_atom(&mut pbuf, &t.atom);
        pbuf.push(t.negated as u8);
    }
    hash_condition(&mut pbuf, &problem.goal);
    let problem_digest = Digest::hash(&pbuf);

    domain_digest.mix(&problem_digest)
}

/// Hashes a `(var_name, type_name)` param/typed-signature list — shared
/// shape used by predicate signatures, action/durative-action params, and
/// `forall`/`exists` binder lists.
fn hash_typed_params(buf: &mut Vec<u8>, params: &[(String, String)]) {
    buf.extend_from_slice(&(params.len() as u32).to_le_bytes());
    for (name, ty) in params {
        buf.extend_from_slice(name.as_bytes());
        buf.push(0); // separator: prevents "ab","c" colliding with "a","bc"
        buf.extend_from_slice(ty.as_bytes());
        buf.push(0xff);
    }
}

fn hash_function(buf: &mut Vec<u8>, f: &PddlFunction) {
    buf.extend_from_slice(f.name.as_bytes());
    buf.extend_from_slice(&(f.params.len() as u32).to_le_bytes());
    for p in &f.params {
        buf.extend_from_slice(p.as_bytes());
    }
}

fn hash_atom(buf: &mut Vec<u8>, atom: &Pddl8Atom) {
    buf.extend_from_slice(atom.pred.as_bytes());
    buf.extend_from_slice(&(atom.args.len() as u32).to_le_bytes());
    for a in &atom.args {
        buf.extend_from_slice(a.as_bytes());
    }
}

fn hash_numeric_op(buf: &mut Vec<u8>, op: NumericOp) {
    buf.push(match op {
        NumericOp::Add => 0,
        NumericOp::Sub => 1,
        NumericOp::Mul => 2,
        NumericOp::Div => 3,
    });
}

/// # Complexity
/// O(size of `e`'s subtree).
fn hash_numeric_expr(buf: &mut Vec<u8>, e: &NumericExpr) {
    match e {
        NumericExpr::Number(n) => {
            buf.push(0);
            buf.extend_from_slice(&n.to_bits().to_le_bytes());
        }
        NumericExpr::FunctionTerm(name, args) => {
            buf.push(1);
            buf.extend_from_slice(name.as_bytes());
            buf.extend_from_slice(&(args.len() as u32).to_le_bytes());
            for a in args {
                buf.extend_from_slice(a.as_bytes());
            }
        }
        NumericExpr::BinOp { op, lhs, rhs } => {
            buf.push(2);
            hash_numeric_op(buf, *op);
            hash_numeric_expr(buf, lhs);
            hash_numeric_expr(buf, rhs);
        }
        NumericExpr::Neg(inner) => {
            buf.push(3);
            hash_numeric_expr(buf, inner);
        }
    }
}

fn hash_compare_op(buf: &mut Vec<u8>, op: CompareOp) {
    buf.push(match op {
        CompareOp::Ge => 0,
        CompareOp::Le => 1,
        CompareOp::Gt => 2,
        CompareOp::Lt => 3,
        CompareOp::Eq => 4,
    });
}

fn hash_time_specifier(buf: &mut Vec<u8>, t: TimeSpecifier) {
    buf.push(match t {
        TimeSpecifier::AtStart => 0,
        TimeSpecifier::AtEnd => 1,
        TimeSpecifier::OverAll => 2,
    });
}

/// # Complexity
/// O(size of `c`'s subtree) — every `PddlCondition` node is visited once.
fn hash_condition(buf: &mut Vec<u8>, c: &PddlCondition) {
    match c {
        PddlCondition::Atom(a) => {
            buf.push(0);
            hash_atom(buf, a);
        }
        PddlCondition::Not(inner) => {
            buf.push(1);
            hash_condition(buf, inner);
        }
        PddlCondition::And(cs) => {
            buf.push(2);
            buf.extend_from_slice(&(cs.len() as u32).to_le_bytes());
            for c in cs {
                hash_condition(buf, c);
            }
        }
        PddlCondition::Or(cs) => {
            buf.push(3);
            buf.extend_from_slice(&(cs.len() as u32).to_le_bytes());
            for c in cs {
                hash_condition(buf, c);
            }
        }
        PddlCondition::Forall { vars, body } => {
            buf.push(4);
            hash_typed_params(buf, vars);
            hash_condition(buf, body);
        }
        PddlCondition::Exists { vars, body } => {
            buf.push(5);
            hash_typed_params(buf, vars);
            hash_condition(buf, body);
        }
        PddlCondition::Imply(a, b) => {
            buf.push(6);
            hash_condition(buf, a);
            hash_condition(buf, b);
        }
        PddlCondition::Timed(t, inner) => {
            buf.push(7);
            hash_time_specifier(buf, *t);
            hash_condition(buf, inner);
        }
        PddlCondition::Compare(lhs, op, rhs) => {
            buf.push(8);
            hash_numeric_expr(buf, lhs);
            hash_compare_op(buf, *op);
            hash_numeric_expr(buf, rhs);
        }
    }
}

fn hash_numeric_effect(buf: &mut Vec<u8>, e: &NumericEffect) {
    let (tag, f, ex) = match e {
        NumericEffect::Assign(f, ex) => (0u8, f, ex),
        NumericEffect::Increase(f, ex) => (1u8, f, ex),
        NumericEffect::Decrease(f, ex) => (2u8, f, ex),
        NumericEffect::ScaleUp(f, ex) => (3u8, f, ex),
        NumericEffect::ScaleDown(f, ex) => (4u8, f, ex),
    };
    buf.push(tag);
    hash_function(buf, f);
    hash_numeric_expr(buf, ex);
}

/// # Complexity
/// O(size of `e`'s subtree) — every `PddlEffect` node (including nested
/// `Timed`/`Forall`/`When` wrappers) is visited once.
fn hash_effect(buf: &mut Vec<u8>, e: &PddlEffect) {
    match e {
        PddlEffect::Add(a) => {
            buf.push(0);
            hash_atom(buf, a);
        }
        PddlEffect::Del(a) => {
            buf.push(1);
            hash_atom(buf, a);
        }
        PddlEffect::Numeric(ne) => {
            buf.push(2);
            hash_numeric_effect(buf, ne);
        }
        PddlEffect::Timed(t, inner) => {
            buf.push(3);
            hash_time_specifier(buf, *t);
            hash_effect(buf, inner);
        }
        PddlEffect::Forall { vars, effects } => {
            buf.push(4);
            hash_typed_params(buf, vars);
            buf.extend_from_slice(&(effects.len() as u32).to_le_bytes());
            for e in effects {
                hash_effect(buf, e);
            }
        }
        PddlEffect::When { condition, effects } => {
            buf.push(5);
            hash_condition(buf, condition);
            buf.extend_from_slice(&(effects.len() as u32).to_le_bytes());
            for e in effects {
                hash_effect(buf, e);
            }
        }
    }
}

fn hash_duration_constraint(buf: &mut Vec<u8>, d: &DurationConstraint) {
    match d {
        DurationConstraint::Eq(e) => {
            buf.push(0);
            hash_numeric_expr(buf, e);
        }
        DurationConstraint::Lte(e) => {
            buf.push(1);
            hash_numeric_expr(buf, e);
        }
        DurationConstraint::Gte(e) => {
            buf.push(2);
            hash_numeric_expr(buf, e);
        }
        DurationConstraint::And(ds) => {
            buf.push(3);
            buf.extend_from_slice(&(ds.len() as u32).to_le_bytes());
            for d in ds {
                hash_duration_constraint(buf, d);
            }
        }
    }
}

/// One bounded PDDL grounding + search run — the PDDL-shaped
/// `GroundedPlanningEpoch` `bcinr_mfw_ir::epoch`'s module doc comment
/// explicitly deferred to this crate ("`GroundedPlanningEpoch` itself...
/// is PDDL-shaped and is left to `bcinr-pddl` to define"), built from the
/// generic bound-tracking primitives [`EpochBounds`]/`bcinr_mfw_ir::DescentMeter`.
#[derive(Debug, Clone)]
pub struct GroundedPlanningEpoch {
    pub id: PlanningEpochId,
    pub theory_digest: Digest,
    pub initial_state: BTreeSet<Pddl8GroundAtom>,
    pub goal: Vec<Pddl8GroundAtom>,
    pub actions: Vec<Pddl8GroundAction>,
    pub bounds: EpochBounds,
}

impl GroundedPlanningEpoch {
    /// Build a `GroundedPlanningEpoch` from an already-grounded classical
    /// `GroundProblem` (`crate::ground::GroundProblem::build`) plus a
    /// caller-chosen `theory_digest` (typically
    /// `AdmittedPlanningTask::theory_digest`) and `bounds`.
    ///
    /// `id` is derived deterministically from `theory_digest`'s first 16
    /// bytes rather than a counter or random nonce: the same domain+problem
    /// (same `theory_digest`) always produces the same epoch id, which is
    /// exactly the property `crate::cache::StandingConsequenceCache` needs
    /// for a cache-hit to mean "the same planning epoch, not just a
    /// coincidentally-equal state."
    pub fn from_ground_problem(
        gp: &GroundProblem, theory_digest: Digest, bounds: EpochBounds,
    ) -> Self {
        let mut id_bytes = [0u8; 16];
        id_bytes.copy_from_slice(&theory_digest.as_bytes()[..16]);
        Self {
            id: PlanningEpochId(u128::from_le_bytes(id_bytes)),
            theory_digest,
            initial_state: gp.initial_state.clone(),
            goal: gp.goal.clone(),
            actions: gp.actions.clone(),
            bounds,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{domain31_from_pddl, problem31_from_pddl};

    const STRIPS_DOMAIN: &str = "(define (domain d) (:requirements :strips) (:predicates (p)) \
                                  (:action a :parameters () :precondition (p) :effect (not (p))))";
    const STRIPS_PROBLEM: &str = "(define (problem pr) (:domain d) (:init (p)) (:goal (not (p))))";

    #[test]
    fn strips_domain_is_admitted_under_default_profile() {
        let domain = domain31_from_pddl(STRIPS_DOMAIN).unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        assert!(outcome.is_found());
    }

    #[test]
    fn equality_requirement_is_refused_as_unsupported() {
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips :equality) (:predicates (p)) \
             (:action a :parameters () :precondition (p) :effect (not (p))))",
        )
        .unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        match outcome {
            PlannerOutcome::Unsupported(u) => assert_eq!(u.feature_name, "Equality"),
            other => panic!("expected Unsupported(Equality), got {other:?}"),
        }
    }

    #[test]
    fn existential_preconditions_requirement_is_refused_as_unsupported() {
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips :existential-preconditions) \
             (:predicates (p)) (:action a :parameters () :precondition (p) :effect (not (p))))",
        )
        .unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        match outcome {
            PlannerOutcome::Unsupported(u) => {
                assert_eq!(u.feature_name, "ExistentialPreconditions")
            }
            other => panic!("expected Unsupported(ExistentialPreconditions), got {other:?}"),
        }
    }

    #[test]
    fn universal_preconditions_requirement_is_admitted_as_approximate_not_refused() {
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips :universal-preconditions) \
             (:predicates (p)) (:action a :parameters () :precondition (p) :effect (not (p))))",
        )
        .unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        assert!(
            outcome.is_found(),
            "Approximate must still admit — only Unsupported refuses"
        );
    }

    #[test]
    fn object_fluents_requirement_is_refused_structurally() {
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips :object-fluents) (:predicates (p)) \
             (:action a :parameters () :precondition (p) :effect (not (p))))",
        )
        .unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        match outcome {
            PlannerOutcome::Unsupported(u) => assert_eq!(u.feature_name, "object-fluents"),
            other => panic!("expected Unsupported(object-fluents), got {other:?}"),
        }
    }

    #[test]
    fn undeclared_object_fluent_assignment_effect_is_refused_as_unsupported() {
        // Adversarial case from the gap report: a domain that *uses*
        // `(assign fluent obj)` (an object-fluent assignment effect)
        // without ever declaring `:object-fluents` bypassed the old
        // declared-requirement-only check entirely. The sentinel content
        // scan (mirroring undeclared_when_effect_is_refused_.../
        // continuous_effect_is_refused_.../ above) must catch it too.
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips) (:constants obj1) (:predicates (p)) \
             (:functions (loc)) \
             (:action a :parameters () :precondition (p) :effect (assign (loc) obj1)))",
        )
        .unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        match outcome {
            PlannerOutcome::Unsupported(u) => assert_eq!(u.feature_name, "object-fluents"),
            other => panic!(
                "expected Unsupported(object-fluents) for an undeclared object-fluent \
                 assignment effect, got {other:?}"
            ),
        }
    }

    #[test]
    fn continuous_effect_is_refused_as_unsupported_whether_or_not_declared() {
        // CONFIRMED LIVE gap (see this module's doc comment on
        // `effect_uses_continuous_effect_sentinel`): before this fix, a
        // durative action's `(increase (fuel) #t)` continuous effect was
        // silently lowered into a meaningless `_continuous_effect` sentinel
        // fact by `parse::lower_timed_effect`, and nothing in
        // `admit_planning_task` ever looked at durative-action effect
        // bodies at all, so this domain sailed through admission
        // regardless of whether `:continuous-effects` was declared.
        // Both variants below (declared and undeclared) must now be refused
        // — the content scan does not depend on the requirement string.
        let declared = domain31_from_pddl(
            "(define (domain d) \
             (:requirements :durative-actions :numeric-fluents :continuous-effects) \
             (:predicates (p)) (:functions (fuel)) \
             (:durative-action a :parameters () :duration (= ?duration 1) \
              :condition (at start (p)) :effect (increase (fuel) #t)))",
        )
        .unwrap();
        let undeclared = domain31_from_pddl(
            "(define (domain d) (:requirements :durative-actions :numeric-fluents) \
             (:predicates (p)) (:functions (fuel)) \
             (:durative-action a :parameters () :duration (= ?duration 1) \
              :condition (at start (p)) :effect (increase (fuel) #t)))",
        )
        .unwrap();
        let problem =
            problem31_from_pddl("(define (problem pr) (:domain d) (:init (p)) (:goal (p)))")
                .unwrap();

        for domain in [declared, undeclared] {
            let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
            match outcome {
                PlannerOutcome::Unsupported(u) => {
                    assert_eq!(u.feature_name, "ContinuousEffects")
                }
                other => panic!(
                    "expected Unsupported(ContinuousEffects) for a domain using a continuous \
                     effect, got {other:?}"
                ),
            }
        }
    }

    #[test]
    fn a_durative_action_with_no_continuous_effect_is_admitted_normally() {
        // The content scan must not over-trigger: an ordinary numeric
        // (non-continuous) durative effect is unaffected.
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :durative-actions :numeric-fluents) \
             (:predicates (p)) (:functions (fuel)) \
             (:durative-action a :parameters () :duration (= ?duration 1) \
              :condition (at start (p)) :effect (at end (increase (fuel) 1))))",
        )
        .unwrap();
        let problem =
            problem31_from_pddl("(define (problem pr) (:domain d) (:init (p)) (:goal (p)))")
                .unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        assert!(
            outcome.is_found(),
            "an ordinary at-end numeric effect must not be mistaken for a continuous effect"
        );
    }

    #[test]
    fn undeclared_when_effect_is_refused_as_unsupported_not_silently_admitted() {
        // Adversarial case from the gap report: the domain declares only
        // `:strips` (no `:conditional-effects`), so the declared-requirement
        // loop alone would never see anything to refuse — but action `a`'s
        // effect body actually uses `when`, which
        // `parse.rs::collect_conditional_effect` (the classical STRIPS8
        // lowering path) would otherwise silently fold into an unconditional
        // `add_effects` entry, letting `(r)` be claimed achieved even though
        // `(q)` was never true. The content scan added to
        // `admit_planning_task` must catch this regardless of the missing
        // declaration.
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips) (:predicates (p) (q) (r)) \
             (:action a :parameters () :precondition (p) :effect (when (q) (r))))",
        )
        .unwrap();
        let problem =
            problem31_from_pddl("(define (problem pr) (:domain d) (:init (p)) (:goal (r)))")
                .unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        match outcome {
            PlannerOutcome::Unsupported(u) => assert_eq!(u.feature_name, "ConditionalEffects"),
            other => panic!(
                "expected Unsupported(ConditionalEffects) for an undeclared `when` effect, got \
                 {other:?}"
            ),
        }
    }

    #[test]
    fn undeclared_forall_effect_is_refused_as_unsupported() {
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips :typing) (:types obj) \
             (:predicates (p ?x - obj)) \
             (:action a :parameters () :precondition () \
              :effect (forall (?x - obj) (p ?x))))",
        )
        .unwrap();
        let problem = problem31_from_pddl(
            "(define (problem pr) (:domain d) (:objects o1 - obj) (:init) (:goal (p o1))))",
        )
        .unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        match outcome {
            PlannerOutcome::Unsupported(u) => assert_eq!(u.feature_name, "ConditionalEffects"),
            other => panic!(
                "expected Unsupported(ConditionalEffects) for an undeclared `forall` effect, \
                 got {other:?}"
            ),
        }
    }

    #[test]
    fn empty_domain_name_is_inconsistent_not_unsupported() {
        // Structural invalidity is a different failure kind from a
        // capability refusal — Inconsistent, not Unsupported.
        let mut domain = domain31_from_pddl(STRIPS_DOMAIN).unwrap();
        domain.name = String::new();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        assert!(matches!(outcome, PlannerOutcome::Inconsistent(_)));
    }

    #[test]
    fn theory_digest_is_deterministic_for_the_same_domain_and_problem() {
        let domain = domain31_from_pddl(STRIPS_DOMAIN).unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let a = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        let b = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        let (PlannerOutcome::Found(a), PlannerOutcome::Found(b)) = (a, b) else {
            panic!("expected both admissions to succeed");
        };
        assert_eq!(a.theory_digest, b.theory_digest);
    }

    #[test]
    fn a_less_permissive_custom_profile_can_refuse_what_default_admits() {
        struct StrictProfile;
        impl CapabilityProfile for StrictProfile {
            fn support(&self, feature: PddlFeature) -> SemanticSupport {
                match feature {
                    // Downgrade Approximate -> Unsupported for everything;
                    // otherwise defer to the default.
                    _ if DefaultCapabilityProfile.support(feature)
                        == SemanticSupport::Approximate =>
                    {
                        SemanticSupport::Unsupported
                    }
                    other => DefaultCapabilityProfile.support(other),
                }
            }
        }
        let domain = domain31_from_pddl(
            "(define (domain d) (:requirements :strips :universal-preconditions) \
             (:predicates (p)) (:action a :parameters () :precondition (p) :effect (not (p))))",
        )
        .unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let default_outcome = admit_planning_task(&domain, &problem, &DefaultCapabilityProfile);
        let strict_outcome = admit_planning_task(&domain, &problem, &StrictProfile);
        assert!(default_outcome.is_found());
        assert!(matches!(strict_outcome, PlannerOutcome::Unsupported(_)));
    }

    #[test]
    fn grounded_planning_epoch_builds_from_a_ground_problem() {
        let domain = crate::parse::domain_from_pddl(STRIPS_DOMAIN).unwrap();
        let problem = crate::parse::problem_from_pddl(STRIPS_PROBLEM).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let bounds = EpochBounds {
            max_ground_actions: 64,
            max_plan_depth: 64,
            max_search_steps: 1_000,
            max_partition_boxes: 8,
        };
        let epoch =
            GroundedPlanningEpoch::from_ground_problem(&gp, Digest::hash(b"theory"), bounds);
        assert_eq!(epoch.actions.len(), gp.actions.len());
        assert_eq!(epoch.initial_state, gp.initial_state);
    }

    // -- theory_digest: digest equality != semantic equivalence coverage --

    #[test]
    fn theory_digest_distinguishes_a_swapped_precondition_and_effect() {
        // The exact adversarial scenario this gap was reported against: two
        // domains share every *name* (domain name, predicate names, action
        // name) but have opposite precondition/effect semantics on the same
        // action. Before this fix, `domain_problem_digest` only hashed
        // names, so these two domains produced a byte-identical
        // `theory_digest`. It must not, now that the digest walks the
        // actual precondition/effect trees.
        let domain_a = domain31_from_pddl(
            "(define (domain d) (:predicates (p) (q)) \
             (:action a1 :parameters () :precondition (p) :effect (q)))",
        )
        .unwrap();
        let domain_b = domain31_from_pddl(
            "(define (domain d) (:predicates (p) (q)) \
             (:action a1 :parameters () :precondition (q) :effect (p)))",
        )
        .unwrap();
        let problem =
            problem31_from_pddl("(define (problem pr) (:domain d) (:init (p)) (:goal (q)))")
                .unwrap();

        let digest_a = domain_problem_digest(&domain_a, &problem);
        let digest_b = domain_problem_digest(&domain_b, &problem);
        assert_ne!(
            digest_a, digest_b,
            "theory_digest must distinguish domains that differ only in a \
             swapped precondition/effect on a same-named action"
        );
    }

    #[test]
    fn theory_digest_distinguishes_different_init_and_goal_content() {
        let domain = domain31_from_pddl(STRIPS_DOMAIN).unwrap();
        let problem_a = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let problem_b =
            problem31_from_pddl("(define (problem pr) (:domain d) (:init) (:goal (p)))").unwrap();

        let digest_a = domain_problem_digest(&domain, &problem_a);
        let digest_b = domain_problem_digest(&domain, &problem_b);
        assert_ne!(
            digest_a, digest_b,
            "theory_digest must distinguish problems that differ only in :init/:goal content"
        );
    }

    #[test]
    fn theory_digest_is_deterministic_across_repeated_calls() {
        let domain = domain31_from_pddl(STRIPS_DOMAIN).unwrap();
        let problem = problem31_from_pddl(STRIPS_PROBLEM).unwrap();
        let d1 = domain_problem_digest(&domain, &problem);
        let d2 = domain_problem_digest(&domain, &problem);
        assert_eq!(d1, d2);
    }
}
