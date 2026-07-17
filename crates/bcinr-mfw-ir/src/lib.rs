//! `bcinr-mfw-ir` — shared IR types and trait contracts for the
//! multifractal-workflow (MFW) planner.
//!
//! This crate contains **only** shared IR types and trait contracts: no
//! heavy algorithmic logic, no PDDL-specific or POWL-specific concepts. It
//! has **zero path-dependency on `bcinr-pddl` or `bcinr-powl`** — they
//! depend on this crate, never the reverse. Unsafe code is forbidden.
//!
//! # Modules
//!
//! - [`digest`] — `Digest`, a BLAKE3-256 newtype used everywhere a content
//!   address is needed.
//! - [`ids`] — shared semantic ID newtypes (`PlanningEpochId`,
//!   `ActionOccurrenceId`, `ConsequenceHorizonId`, four profile IDs,
//!   `PowlNodeId`).
//! - [`outcome`] — the `PlannerOutcome<T>` / witness algebra shared by
//!   every bounded search/analysis stage.
//! - [`contracts`] — `FormalLawRef` / `FormalStanding` /
//!   `SemanticOptimizationContract`, plus the seven named formal-law
//!   constants cited from `/Users/sac/mfact`.
//! - [`epoch`] — `EpochBounds` and `DescentMeter`, the generic
//!   bound-tracking primitives (PDDL-shaped `GroundedPlanningEpoch` itself
//!   lives in `bcinr-pddl`).
//! - [`event_set`] — `EventSet`, a fixed 512-slot bitset over action
//!   occurrences.
//! - [`causal`] — the independence / causal-plan IR (`ActionPair`,
//!   `IndependenceWitness`, `CausalPlan`, the `CausalAnalyzer` trait, ...).
//! - [`concurrency`] — the concurrency-complex IR (`MinimalNonFace`,
//!   `ExecutableConcurrencyComplex`, the `ConcurrencyAnalyzer` trait).
//! - [`projection`] — the POWL projection contract (witness types only;
//!   `PowlModel` itself belongs to `bcinr-powl`).
//!
//! # Formal claim ceiling
//!
//! `bcinr` itself never claims Lean/Coq verification — that lives
//! exclusively in the sibling repo `/Users/sac/mfact`. See
//! [`contracts::FormalStanding`] and the seven `LAW_*` constants in
//! [`contracts`] for exactly what is `Proven`, `Stated`, `Conjectural`, or
//! `Blocked`, per `/Users/sac/mfact`'s own Lean sources as of this crate's
//! authorship.

#![forbid(unsafe_code)]

pub mod causal;
pub mod concurrency;
pub mod contracts;
pub mod digest;
pub mod epoch;
pub mod event_set;
pub mod ids;
pub mod outcome;
pub mod projection;

pub use causal::{
    ActionOccurrence, ActionPair, AtomId, CausalAnalyzer, CausalPlan, CausalSupportEdge,
    ConstraintId, DependenceReason, DependenceWitness, EffectsCommuteWitness, FluentId,
    IndependenceRelation, IndependenceVerdict, IndependenceWitness, InvariantId,
    InvariantsStableWitness, NumericFlowWitness, PrecedenceEdge, PreconditionsStableWitness,
    StrictPartialOrder, SupportObject, TrajectoryWitness,
};
pub use concurrency::{
    ConcurrencyAnalyzer, ConcurrencyConflictWitness, ExecutableConcurrencyComplex, MinimalNonFace,
    ResourceConflictWitness,
};
pub use contracts::{
    ContractError, FormalLawRef, FormalStanding, SemanticOptimizationContract,
    LAW_CONCURRENCY_COMPLEX_DOWNWARD_CLOSED, LAW_CROWN_KERNEL_CHARACTERIZATION,
    LAW_EXECUTABLE_CONCURRENCY_INTERSECTION, LAW_MINIMAL_NONFACE_REPRESENTATION,
    LAW_OBSERVABLE_IFF_FIBER_CONSTANT, LAW_QLENS_RATIO, LAW_SPECTRUM_ESTIMATOR,
};
pub use digest::Digest;
pub use epoch::{DescentMeter, EpochBounds};
pub use event_set::{EventSet, EventSetIter, EVENT_WORDS, MAX_EPOCH_EVENTS};
pub use ids::{
    ActionOccurrenceId, ConsequenceHorizonId, MeasureProfileId, PlanningEpochId, PowlNodeId,
    SearchProfileId, SelectorProfileId, TransformationProfileId,
};
pub use outcome::{
    BoundHit, BoundKind, ExhaustionWitness, InconsistencyWitness, PlannerFailure, PlannerOutcome,
    UnsupportedFeature,
};
pub use projection::{
    ActionNodeBijection, ConcurrencyPreservationWitness, OrderPreservationWitness,
    PowlProjectionWitness, PowlProjector,
};
