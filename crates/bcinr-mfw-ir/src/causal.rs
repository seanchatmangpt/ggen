//! Independence / causal-plan IR.
//!
//! `AtomId`, `FluentId`, `InvariantId`, `ConstraintId` are defined here
//! (not in `bcinr-pddl`) purely as plain `u32` newtypes so that both this
//! crate's witness types and PDDL's eventual producers of those witnesses
//! share one ID vocabulary without either crate depending on the other.
//!
//! The five independence sub-witnesses (`EffectsCommuteWitness`,
//! `PreconditionsStableWitness`, `InvariantsStableWitness`,
//! `NumericFlowWitness`, `TrajectoryWitness`) are **not** individually
//! pinned down field-by-field in the source spec beyond "commit digests,
//! bool flags, BTreeSet-of-ID members" — the concrete field lists below are
//! this phase's interpretation of that shape and should be treated as a
//! first draft, not a locked contract, by whichever phase first wires a
//! real `CausalAnalyzer` implementation against `bcinr-pddl` data.

use std::collections::{BTreeMap, BTreeSet};

use crate::digest::Digest;
use crate::ids::{ActionOccurrenceId, PlanningEpochId};

macro_rules! id_newtype {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub u32);

        impl $name {
            pub const fn get(self) -> u32 {
                self.0
            }
        }
    };
}

id_newtype!(
    /// Identifies a ground predicate atom (pddl-specific representation
    /// lives in `bcinr-pddl`; this is just the shared ID).
    AtomId
);
id_newtype!(
    /// Identifies a numeric fluent.
    FluentId
);
id_newtype!(
    /// Identifies a domain invariant.
    InvariantId
);
id_newtype!(
    /// Identifies a temporal/ordering constraint.
    ConstraintId
);

/// An unordered pair of distinct action occurrences, canonicalized so
/// `(a, b)` and `(b, a)` always produce the same `ActionPair` (`left <
/// right`) — required for it to be usable as a stable `BTreeMap` key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ActionPair {
    pub left: ActionOccurrenceId,
    pub right: ActionOccurrenceId,
}

impl ActionPair {
    /// Build a canonicalized pair from two occurrence IDs.
    ///
    /// # Panics
    /// Panics if `a == b` — a pair must relate two distinct occurrences.
    pub fn new(a: ActionOccurrenceId, b: ActionOccurrenceId) -> Self {
        assert_ne!(
            a, b,
            "ActionPair::new: an action cannot be paired with itself"
        );
        if a < b {
            ActionPair { left: a, right: b }
        } else {
            ActionPair { left: b, right: a }
        }
    }
}

/// Witness that two actions' effect sets commute (applying them in either
/// order reaches the same state).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectsCommuteWitness {
    pub commute: bool,
    pub left_effects_digest: Digest,
    pub right_effects_digest: Digest,
    pub shared_atoms: BTreeSet<AtomId>,
    pub shared_fluents: BTreeSet<FluentId>,
}

/// Witness that neither action's effects threaten the other's
/// preconditions (no delete-then-need or need-then-delete interference).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PreconditionsStableWitness {
    pub stable: bool,
    pub left_preconditions_digest: Digest,
    pub right_preconditions_digest: Digest,
    pub threatened_atoms: BTreeSet<AtomId>,
}

/// Witness that domain invariants remain satisfied regardless of which
/// interleaving of the two actions is chosen.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvariantsStableWitness {
    pub stable: bool,
    pub invariants_digest: Digest,
    pub checked_invariants: BTreeSet<InvariantId>,
}

/// Witness that numeric-fluent updates from the two actions commute (both
/// only read, or their writes are to disjoint fluents, or their combined
/// update is associative/commutative for the touched fluents).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NumericFlowWitness {
    pub commute: bool,
    pub flow_digest: Digest,
    pub touched_fluents: BTreeSet<FluentId>,
}

/// Witness that the combined trajectory of the two actions (under any
/// admissible interleaving) remains consistent with temporal/duration
/// constraints.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TrajectoryWitness {
    pub consistent: bool,
    pub trajectory_digest: Digest,
    pub checked_constraints: BTreeSet<ConstraintId>,
}

/// Bundle of the five sub-witnesses that together certify one
/// `ActionPair` as independent.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndependenceWitness {
    pub effects_commute: EffectsCommuteWitness,
    pub preconditions_stable: PreconditionsStableWitness,
    pub invariants_stable: InvariantsStableWitness,
    pub numeric_flow: NumericFlowWitness,
    pub trajectory: TrajectoryWitness,
}

/// Why an `ActionPair` was found dependent. Ordered so `BTreeSet` can hold
/// a canonical, deterministic set of reasons.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DependenceReason {
    CausalSupport,
    DeleteInterference,
    PreconditionInterference,
    InvariantConflict,
    NumericFlowConflict,
    TemporalConflict,
    TrajectoryConflict,
}

/// Witness that an `ActionPair` is dependent: the set of distinct reasons
/// found (an `ActionPair` can be dependent for more than one reason at
/// once).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependenceWitness {
    pub reasons: BTreeSet<DependenceReason>,
    /// Atoms found to be threatened by a genuine `DeleteInterference`
    /// finding (one action's delete effects intersecting the other's
    /// preconditions, either direction) — the `Dependent`-side mirror of
    /// [`PreconditionsStableWitness::threatened_atoms`], which is always
    /// empty on the `Independent` side by construction (independence
    /// requires nothing was threatened). Empty whenever `reasons` does not
    /// contain [`DependenceReason::DeleteInterference`], and also empty
    /// when an analyzer conservatively reports `DeleteInterference` as a
    /// fallback reason (e.g. effects provably don't commute but no direct
    /// atom-level conflict explains why) without having identified a
    /// specific atom — an empty set here is not itself a claim that no
    /// interference occurred, only that this field carries no atom-level
    /// provenance for it. Analyzer-populated; not required to be
    /// exhaustive across every `DependenceReason`.
    pub threatened_atoms: BTreeSet<AtomId>,
}

/// The verdict for one `ActionPair`: independent (with full witness),
/// dependent (with reasons), or the analyzer could not determine either
/// (free-text reason — analyzer-specific, no fixed vocabulary here).
///
/// `Independent` boxes its witness: `IndependenceWitness` bundles five
/// sub-witnesses and is >400 bytes, dwarfing `Dependent`'s ~24 bytes;
/// without boxing, every `IndependenceVerdict` (including every
/// `Dependent`/`Unsupported` one) would pay that size regardless of which
/// variant it holds (clippy::large_enum_variant). This is a deliberate,
/// non-semantic deviation from the literal spec shape
/// `Independent(IndependenceWitness)` — the boxed value still *is* an
/// `IndependenceWitness`, just heap-indirected.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IndependenceVerdict {
    Independent(Box<IndependenceWitness>),
    Dependent(DependenceWitness),
    Unsupported(String),
}

/// The full independence relation over a `CausalPlan`'s action occurrences:
/// every pair that was determined independent or dependent (pairs with an
/// `Unsupported` verdict are not required to appear in either map).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct IndependenceRelation {
    pub independent: BTreeMap<ActionPair, IndependenceWitness>,
    pub dependent: BTreeMap<ActionPair, DependenceWitness>,
}

/// A directed precedence edge: `before` must occur before `after`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrecedenceEdge {
    pub before: ActionOccurrenceId,
    pub after: ActionOccurrenceId,
}

/// A strict partial order over action occurrences, represented as its
/// edge set. Irreflexivity/transitivity/acyclicity are properties a
/// `CausalAnalyzer` implementation must establish before constructing one —
/// this type does not itself validate them (no analyzer logic lives in
/// this IR-only crate).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct StrictPartialOrder {
    pub edges: BTreeSet<PrecedenceEdge>,
}

/// A fact a causal-support edge is about: either a predicate atom or a
/// numeric fluent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SupportObject {
    Atom(AtomId),
    Fluent(FluentId),
}

/// An edge recording that `producer` establishes `fact`, which `consumer`
/// depends on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CausalSupportEdge {
    pub producer: ActionOccurrenceId,
    pub consumer: ActionOccurrenceId,
    pub fact: SupportObject,
}

/// One action occurrence within a causal plan. `action` is a plain `u64`
/// placeholder for the ground-action reference: `GroundActionId` is
/// pddl-specific and lives in `bcinr-pddl`, which will most likely
/// newtype-wrap this `u64` (or replace it with its own ID type via a
/// From/Into bridge) rather than this crate depending on pddl's type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ActionOccurrence {
    pub id: ActionOccurrenceId,
    pub action: u64,
}

/// The causal structure derived from one grounded planning epoch's action
/// occurrences: the occurrences themselves, their precedence order, the
/// pairwise independence relation, the causal-support edges, and a digest
/// summarizing the whole structure.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CausalPlan {
    pub epoch: PlanningEpochId,
    pub occurrences: Vec<ActionOccurrence>,
    pub precedes: StrictPartialOrder,
    pub independence: IndependenceRelation,
    pub support_edges: BTreeSet<CausalSupportEdge>,
    pub digest: Digest,
}

/// Produces a `CausalPlan` from a set of action occurrences within some
/// epoch. Generic over `Epoch` so `bcinr-pddl` can implement this trait
/// against its own `GroundedPlanningEpoch` without this crate knowing
/// pddl's types.
pub trait CausalAnalyzer {
    type Epoch;
    type Error;

    fn analyze(
        &self,
        epoch: &Self::Epoch,
        occurrences: &[ActionOccurrence],
    ) -> Result<CausalPlan, Self::Error>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn action_pair_canonicalizes_ordering() {
        let a = ActionOccurrenceId(1);
        let b = ActionOccurrenceId(2);
        assert_eq!(ActionPair::new(a, b), ActionPair::new(b, a));
        let pair = ActionPair::new(b, a);
        assert_eq!(pair.left, a);
        assert_eq!(pair.right, b);
    }

    #[test]
    #[should_panic(expected = "cannot be paired with itself")]
    fn action_pair_rejects_self_pair() {
        let a = ActionOccurrenceId(1);
        let _ = ActionPair::new(a, a);
    }

    #[test]
    fn dependence_reasons_are_ordered_for_btreeset() {
        let mut reasons = BTreeSet::new();
        reasons.insert(DependenceReason::TrajectoryConflict);
        reasons.insert(DependenceReason::CausalSupport);
        let collected: Vec<_> = reasons.into_iter().collect();
        assert_eq!(
            collected,
            vec![
                DependenceReason::CausalSupport,
                DependenceReason::TrajectoryConflict
            ]
        );
    }

    /// Mock analyzer proving the `CausalAnalyzer` trait shape is usable
    /// without any pddl-specific type — `Epoch = ()`.
    struct MockAnalyzer;

    #[derive(Debug)]
    struct MockError;

    impl CausalAnalyzer for MockAnalyzer {
        type Epoch = ();
        type Error = MockError;

        fn analyze(
            &self,
            _epoch: &(),
            occurrences: &[ActionOccurrence],
        ) -> Result<CausalPlan, MockError> {
            Ok(CausalPlan {
                epoch: PlanningEpochId(0),
                occurrences: occurrences.to_vec(),
                precedes: StrictPartialOrder::default(),
                independence: IndependenceRelation::default(),
                support_edges: BTreeSet::new(),
                digest: Digest::hash(b"mock"),
            })
        }
    }

    #[test]
    fn mock_causal_analyzer_compiles_and_runs() {
        let analyzer = MockAnalyzer;
        let occ = vec![ActionOccurrence {
            id: ActionOccurrenceId(0),
            action: 1,
        }];
        let plan = analyzer.analyze(&(), &occ).unwrap();
        assert_eq!(plan.occurrences.len(), 1);
    }
}
