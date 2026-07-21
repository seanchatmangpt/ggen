//! Outcome/witness algebra shared by every search/analysis stage in the MFW
//! pipeline.
//!
//! This supersedes `bcinr-pddl`'s existing `error::PlannerOutcome<T>`
//! (`Found(T) | Exhausted | Bounded(usize) | Unsupported(String) |
//! Inconsistent`), which collapses every non-`Found` case to either a bare
//! unit or a single `usize`/`String` — by the time it reaches
//! `into_result()`, `Exhausted`, `Unsupported`, and `Inconsistent` all
//! degrade to the same `Pddl8Error::NoAdmittedPlan`, discarding whatever
//! information the search actually had at the point of failure.
//!
//! This version keeps a witness on every non-`Found` variant, so
//! `into_result()` can hand back a typed failure (`PlannerFailure`) without
//! discarding it. **Phase 2 is done**: `bcinr-pddl/src/error.rs` no longer
//! defines its own `PlannerOutcome<T>` — it re-exports this type directly
//! (`pub use bcinr_mfw_ir::PlannerOutcome;`) and gained
//! `Pddl8Error::PlanningFailed(PlannerFailure)` so `.into_result()?` keeps
//! working without collapsing the witness onto `NoAdmittedPlan`. There is
//! exactly one `PlannerOutcome<T>` in the workspace now, not two parallel
//! types: `error.rs`, `lib.rs`, `capability_router.rs`, `ground/mod.rs`,
//! `ground/lazy.rs`, and `tests/indexed_grounding.rs` all construct and match
//! on *this* type (confirmed by grep — none references a separate local
//! shape any more).

use crate::digest::Digest;
use crate::ids::SearchProfileId;

/// Result of one search/analysis stage: either a value was found, or the
/// stage stopped for one of four distinguishable reasons, each carrying a
/// witness of what actually happened.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlannerOutcome<T> {
    /// A value was found.
    Found(T),
    /// Search exhausted its frontier without reaching the goal.
    Exhausted(ExhaustionWitness),
    /// A structural bound was hit before a value could be found.
    Bounded(BoundHit),
    /// The request needs a feature this stage does not implement.
    Unsupported(UnsupportedFeature),
    /// The stage detected an internal inconsistency (not a bound, not
    /// exhaustion — something that shouldn't be possible if inputs were
    /// well-formed).
    Inconsistent(InconsistencyWitness),
}

/// The non-`Found` half of `PlannerOutcome<T>`, as a standalone error type
/// for `Result`-based call sites. Carries exactly the same witnesses as the
/// `PlannerOutcome` variant it came from — `into_result` does not summarize
/// or discard any witness field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlannerFailure {
    Exhausted(ExhaustionWitness),
    Bounded(BoundHit),
    Unsupported(UnsupportedFeature),
    Inconsistent(InconsistencyWitness),
}

impl<T> PlannerOutcome<T> {
    /// True iff this is `Found(_)`.
    pub fn is_found(&self) -> bool {
        matches!(self, Self::Found(_))
    }

    /// Convert to a `Result`, preserving every witness field on the error
    /// side. Unlike `bcinr-pddl`'s current `into_result`, this never maps
    /// multiple distinct failure witnesses onto one shared error variant.
    pub fn into_result(self) -> Result<T, PlannerFailure> {
        match self {
            Self::Found(t) => Ok(t),
            Self::Exhausted(w) => Err(PlannerFailure::Exhausted(w)),
            Self::Bounded(b) => Err(PlannerFailure::Bounded(b)),
            Self::Unsupported(u) => Err(PlannerFailure::Unsupported(u)),
            Self::Inconsistent(i) => Err(PlannerFailure::Inconsistent(i)),
        }
    }
}

impl std::fmt::Display for PlannerFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exhausted(w) => write!(
                f,
                "search exhausted: profile={:?} explored_states={} frontier_empty={} digest={}",
                w.search_profile, w.explored_states, w.frontier_empty, w.digest
            ),
            Self::Bounded(b) => write!(
                f,
                "bound hit: kind={:?} limit={} observed={}",
                b.kind, b.limit, b.observed
            ),
            Self::Unsupported(u) => {
                write!(f, "unsupported feature: {} ({})", u.feature_name, u.context)
            }
            Self::Inconsistent(i) => {
                write!(
                    f,
                    "inconsistent: {} ({}) digest={}",
                    i.kind_name, i.context, i.digest
                )
            }
        }
    }
}

impl std::error::Error for PlannerFailure {}

/// Witness that a search stage exhausted its frontier without reaching a
/// goal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExhaustionWitness {
    pub search_profile: SearchProfileId,
    pub explored_states: u64,
    pub frontier_empty: bool,
    pub digest: Digest,
}

/// Which structural bound was exceeded.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoundKind {
    PlanDepth,
    GroundActions,
    FrontierStates,
    SearchSteps,
    RecursiveDescent,
    PartitionBoxes,
}

/// Witness that a structural bound was exceeded: `observed` crossed
/// `limit` for the given `kind`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BoundHit {
    pub kind: BoundKind,
    pub limit: u64,
    pub observed: u64,
}

/// A named, free-text feature that a stage refuses (by design) to support.
/// `feature_name` is intentionally a free string rather than an enum here:
/// pddl-specific feature enums (e.g. a `PddlFeature`) live in `bcinr-pddl`,
/// not in this crate, so producers pass their own `Debug`/`Display` string.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnsupportedFeature {
    pub feature_name: String,
    pub context: String,
}

/// Witness that a stage detected an internal inconsistency.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InconsistencyWitness {
    pub kind_name: String,
    pub context: String,
    pub digest: Digest,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_exhaustion() -> ExhaustionWitness {
        ExhaustionWitness {
            search_profile: SearchProfileId(1),
            explored_states: 42,
            frontier_empty: true,
            digest: Digest::hash(b"frontier"),
        }
    }

    #[test]
    fn found_into_result_is_ok() {
        let outcome: PlannerOutcome<u32> = PlannerOutcome::Found(7);
        assert!(outcome.is_found());
        assert_eq!(outcome.into_result(), Ok(7));
    }

    #[test]
    fn exhausted_preserves_witness_through_into_result() {
        let w = sample_exhaustion();
        let outcome: PlannerOutcome<u32> = PlannerOutcome::Exhausted(w.clone());
        assert!(!outcome.is_found());
        let err = outcome.into_result().unwrap_err();
        assert_eq!(err, PlannerFailure::Exhausted(w));
    }

    #[test]
    fn bounded_preserves_witness() {
        let hit = BoundHit {
            kind: BoundKind::RecursiveDescent,
            limit: 10,
            observed: 11,
        };
        let outcome: PlannerOutcome<u32> = PlannerOutcome::Bounded(hit);
        assert_eq!(
            outcome.into_result().unwrap_err(),
            PlannerFailure::Bounded(hit)
        );
    }

    #[test]
    fn display_does_not_panic_on_any_variant() {
        let variants: Vec<PlannerFailure> = vec![
            PlannerFailure::Exhausted(sample_exhaustion()),
            PlannerFailure::Bounded(BoundHit {
                kind: BoundKind::PlanDepth,
                limit: 1,
                observed: 2,
            }),
            PlannerFailure::Unsupported(UnsupportedFeature {
                feature_name: "existential-preconditions".into(),
                context: "ground/mod.rs eval_condition".into(),
            }),
            PlannerFailure::Inconsistent(InconsistencyWitness {
                kind_name: "goal-unreachable-but-frontier-nonempty".into(),
                context: "test".into(),
                digest: Digest::ZERO,
            }),
        ];
        for v in variants {
            let rendered = format!("{v}");
            assert!(
                !rendered.is_empty(),
                "Display for {v:?} rendered an empty string"
            );
        }
    }
}
