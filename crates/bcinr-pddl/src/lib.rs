//! bcinr-pddl — PDDL8 → POWL tape → Prolog8 admission → OCEL → BLAKE3
//!
//! # BRCE stack position
//! ```text
//! PDDL8   = candidate-future grammar  (parse + ground modules)
//! POWL    = process geometry          (wasm4pm_compat::pddl::Pddl8Tape)
//! Prolog8 = R ⊢ A gate               (execute module)
//! OCEL    = execution trace           (wasm4pm_compat::ocel::OCEL)
//! BLAKE3  = receipt / replay          (wasm4pm_compat::pddl::Pddl8ExecutionReceipt)
//! ```
//!
//! # Isolation guarantee
//! Zero *unconditional* path deps on bcinr-powl or wasm4pm-cognition — the
//! `pddl` parser crate is **only** compiled when this crate is in the
//! dependency graph. PDDL does not bleed into bcinr-powl, wasm4pm, or
//! lsp-max unless they explicitly add bcinr-pddl as a dep.
//!
//! `bcinr-powl` and `bcinr-powl-receipt` *are* optional path deps of this
//! crate (see `Cargo.toml`), enabled only by the `mfw-planner` feature (off
//! by default) for the `MfwPlanner` orchestrator in [`mfw::planner`]. A
//! default build of this crate — and every existing consumer that does not
//! opt into `mfw-planner`, including praxis's own path dependency on this
//! crate — pulls in neither. This is the one genuine, intentional exception
//! to the isolation guarantee above, and it is opt-in, not automatic.
//!
//! Canonical types live in `wasm4pm_compat::pddl` so any crate can import
//! `Pddl8Tape`, `Pddl8GroundAction`, etc. without pulling in the parser.

#![feature(once_cell_try)]

pub mod alloc_counter;
pub mod capability;
pub mod capability_router;
pub mod causal;
pub mod concurrency;
pub mod consequence;
pub mod dfcm_crown;
pub mod error;
pub mod execute;
pub mod ground;
pub mod llm_bridge;
pub mod mfw;
pub mod parse;
pub mod powl_bridge;
pub mod schedule_analysis;
pub mod search;
pub use capability::{
    admit_planning_task, AdmittedPlanningTask, CapabilityProfile, DefaultCapabilityProfile,
    GroundedPlanningEpoch, PddlFeature, SemanticSupport, ALL_PDDL_FEATURES,
};
pub use capability_router::{
    route_capability_plan, CapabilityRouteReceipt, CapabilityTask, CostVector, DesiredEffect,
};
pub use causal::{CausalAnalysisError, PddlCausalAnalyzer};
pub use concurrency::{ConcurrencyAnalysisError, PddlConcurrencyAnalyzer};
pub use consequence::{
    plan_with_standing_cache, ConsequenceHorizon, ExactStateKey, GoalReachabilityHorizon,
    MakespanObservation, MinimumMakespanHorizon, PlanningResult, ResidualDecision,
    ResidualObligation, Residualizer, StandingConsequenceCache,
};
pub use dfcm_crown::{run_dfcm_crown_suite, DfcmBenchReceipt};
pub use llm_bridge::{
    admit_candidate_domain, admit_candidate_problem, manufacture_world, AdmittedDomain,
    AdmittedProblem, WorldManufactureReceipt,
};
pub use mfw::{
    q_lens, FrontierBoxes, FrontierMeasure, MassVector, PositiveDistribution, PositiveMass,
    QLensError, QValue, WeightedDistribution,
};
pub use schedule_analysis::{
    analyze_schedule, analyze_schedule_instrumented, AnalysisSubstageNs, CapacityDelta,
    ScheduleAnalysis64,
};
pub use search::{
    ExactBfsRail, ExactSearchRail, ExactStepOutcome, ExploitSearchRail, ExploitStepOutcome,
    FairRailScheduler, MfwPortfolio, PortfolioOutcome, QLensRail, RailSelection,
};

// Re-export canonical types from wasm4pm-compat so callers only need one import.
pub use wasm4pm_compat::ocel::{OCELEvent, OCEL};
pub use wasm4pm_compat::pddl::{
    DerivedPredicate,
    DurationConstraint,
    DurativeAction,
    Metric,
    MetricDir,
    MetricExpr,
    NumericEffect,
    NumericExpr,
    NumericOp,
    Pddl31Action,
    Pddl31Domain,
    Pddl31Problem,
    Pddl8ActionSchema,
    Pddl8Atom,
    Pddl8Domain,
    Pddl8ExecutionLog,
    Pddl8ExecutionReceipt,
    Pddl8GroundAction,
    Pddl8GroundAtom,
    Pddl8Problem,
    Pddl8StepResult,
    Pddl8Tape,
    Pddl8TapeOp,
    PddlCondition,
    PddlConstraint,
    PddlEffect,
    PddlEvent,
    PddlFunction,
    PddlPreference,
    PddlProcess,
    // New PDDL 3.1 types:
    PddlType,
    TemporalExecutionReceipt,
    TemporalPlan,
    TemporalPlanStep,
    TimeSpecifier,
    TimedLiteral,
    TrajectoryConstraint,
    PDDL8_MAX_ARITY,
    PDDL8_MAX_CONJUNCTS,
    PDDL8_MAX_GROUND,
    PDDL8_MAX_PARAMS,
    PDDL8_MAX_PLAN_DEPTH,
};

pub use error::{Pddl8Error, PlannerOutcome};
pub use execute::{
    compute_plan_chain, execute_tape, execute_temporal_plan_instrumented, SubstageNs,
};
pub use ground::{GroundDurativeAction, GroundProblem, GroundTemporalProblem};
pub use parse::{domain31_from_pddl, domain_from_pddl, problem31_from_pddl, problem_from_pddl};
