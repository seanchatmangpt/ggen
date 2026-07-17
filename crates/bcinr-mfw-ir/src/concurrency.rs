//! Concurrency-complex IR: a minimal-nonfaces / Stanley-Reisner-style
//! representation of which sets of concurrently-firing action occurrences
//! are admissible.
//!
//! **Formal standing:** this representation is cited by
//! [`crate::contracts::LAW_MINIMAL_NONFACE_REPRESENTATION`], which carries
//! [`crate::contracts::FormalStanding::Blocked`] — no Lean artifact of a
//! minimal-nonfaces / Stanley-Reisner representation exists anywhere in
//! `/Users/sac/mfact`; mfact formalizes the concurrency complex `K_Pi`
//! exclusively via positive face-lists (see
//! `LAW_CONCURRENCY_COMPLEX_DOWNWARD_CLOSED`, which *is* `Proven`, but for
//! the face-list representation, not this one). Nothing in this module —
//! its doc comments, its error messages, its method names — should ever be
//! read as claiming this representation is sound or exact. The most this
//! module is entitled to say about a candidate `admits()`-approved set is
//! that it is *structurally well-formed* / *internally consistent* with
//! the minimal nonfaces recorded so far, not that it is a correct
//! executable-concurrency witness in mfact's sense.

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use crate::causal::{ActionPair, CausalPlan, FluentId, InvariantId, PrecedenceEdge};
use crate::digest::Digest;
use crate::event_set::EventSet;

/// A minimal nonface: a smallest set of events that is *not* jointly
/// admissible for concurrent execution (every proper subset of `members`
/// is admissible, but `members` itself is not).
///
/// `witness_digest` points at the full [`ConcurrencyConflictWitness`] in a
/// side table (typically [`ExecutableConcurrencyComplex::conflict_witnesses`])
/// rather than embedding it directly, keeping this type small and `Copy`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MinimalNonFace {
    pub members: EventSet,
    pub witness_digest: Digest,
}

/// Fixed-point (milli-unit, `i64`) resource-capacity conflict: `actions`
/// jointly demand more of `resource` than `capacity_milli` allows.
///
/// This is a **deliberate deviation** from the source design, which used a
/// floating-point `NumericValue` here. Fixed-point milli-units (`i64`) were
/// chosen instead specifically so this type can derive `Eq`/`Ord`/`Hash`
/// and participate in deterministic `BTreeMap`/`BTreeSet` structures and
/// `Digest` computations without `f64`'s non-total-order and
/// non-bit-reproducible-hash problems — matching this repo's determinism
/// discipline (BTreeMap/BTreeSet over HashMap/HashSet wherever ordering
/// feeds a BLAKE3 digest).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ResourceConflictWitness {
    pub actions: EventSet,
    pub resource: FluentId,
    pub capacity_milli: i64,
    pub demanded_milli: i64,
}

/// Why a set of events was found jointly inadmissible: any nonempty
/// combination of a causal ordering conflict, a temporal/invariant
/// conflict, and a resource-capacity conflict.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConcurrencyConflictWitness {
    pub causal: Option<(ActionPair, PrecedenceEdge)>,
    pub temporal: Option<(EventSet, BTreeSet<InvariantId>)>,
    pub resource: Option<ResourceConflictWitness>,
}

/// An executable-concurrency complex: the set of minimal nonfaces over
/// `event_count` events, plus the full conflict witness behind each one.
///
/// A candidate `EventSet` is admitted (see [`Self::admits`]) iff it
/// contains none of the recorded minimal nonfaces as a subset — the
/// standard minimal-nonfaces / Stanley-Reisner admission rule. See the
/// module-level doc comment for this representation's formal standing
/// (`Blocked`, not proven or Lean-formalized).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecutableConcurrencyComplex {
    pub event_count: usize,
    pub minimal_nonfaces: Vec<MinimalNonFace>,
    pub conflict_witnesses: BTreeMap<Digest, ConcurrencyConflictWitness>,
    pub digest: Digest,
}

impl ExecutableConcurrencyComplex {
    /// True iff `candidate` contains none of `self.minimal_nonfaces` as a
    /// subset — i.e. `candidate` is not known to be jointly inadmissible.
    ///
    /// As noted at the module level: a `true` result here means
    /// "structurally well-formed against the recorded minimal nonfaces,"
    /// not "proven executable-concurrent" in mfact's sense.
    ///
    /// # Complexity
    ///
    /// O(`self.minimal_nonfaces.len()`) — one [`EventSet::is_subset_of`]
    /// call per recorded nonface, each itself O(1) over `EventSet`'s fixed
    /// 8-word bitset. This is the per-candidate admission gate (called
    /// once per candidate concurrency set, e.g. once per ready-set
    /// candidate in a scheduler's inner loop), so a linear-in-nonfaces
    /// cost here is exactly the shape that matters for callers ticking it
    /// repeatedly, even though each individual call is cheap.
    pub fn admits(&self, candidate: &EventSet) -> bool {
        !self
            .minimal_nonfaces
            .iter()
            .any(|nf| nf.members.is_subset_of(candidate))
    }
}

/// Produces an `ExecutableConcurrencyComplex` from a `CausalPlan`'s
/// independence/causal structure. Generic over `Epoch` so `bcinr-pddl` can
/// implement this trait against its own `GroundedPlanningEpoch` without
/// this crate knowing pddl's types.
pub trait ConcurrencyAnalyzer {
    type Epoch;
    type Error;

    fn analyze(
        &self,
        epoch: &Self::Epoch,
        causal: &CausalPlan,
    ) -> Result<ExecutableConcurrencyComplex, Self::Error>;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The worked 3-action-capacity scenario: actions A=0, B=1, C=2 are
    /// pairwise concurrency-OK but not all three together — modeled as a
    /// complex with a single minimal nonface `{A, B, C}`. Every proper
    /// subset must be admitted; the full set must not be.
    fn worked_complex() -> ExecutableConcurrencyComplex {
        let abc = EventSet::empty().with(0).with(1).with(2);
        let witness = ConcurrencyConflictWitness {
            causal: None,
            temporal: None,
            resource: Some(ResourceConflictWitness {
                actions: abc,
                resource: FluentId(0),
                capacity_milli: 2_000,
                demanded_milli: 3_000,
            }),
        };
        let witness_digest = Digest::hash(b"abc-resource-conflict");
        let mut conflict_witnesses = BTreeMap::new();
        conflict_witnesses.insert(witness_digest, witness);

        ExecutableConcurrencyComplex {
            event_count: 3,
            minimal_nonfaces: vec![MinimalNonFace {
                members: abc,
                witness_digest,
            }],
            conflict_witnesses,
            digest: Digest::hash(b"worked-complex"),
        }
    }

    #[test]
    fn every_proper_subset_of_the_nonface_is_admitted() {
        let complex = worked_complex();
        let empty = EventSet::empty();
        let a = EventSet::empty().with(0);
        let b = EventSet::empty().with(1);
        let c = EventSet::empty().with(2);
        let ab = a.union(&b);
        let bc = b.union(&c);
        let ac = a.union(&c);

        for candidate in [empty, a, b, c, ab, bc, ac] {
            assert!(
                complex.admits(&candidate),
                "expected {candidate:?} to be admitted"
            );
        }
    }

    #[test]
    fn the_full_nonface_is_not_admitted() {
        let complex = worked_complex();
        let abc = EventSet::empty().with(0).with(1).with(2);
        assert!(!complex.admits(&abc));
    }

    #[test]
    fn a_superset_containing_the_nonface_is_also_not_admitted() {
        let complex = worked_complex();
        let abcd = EventSet::empty().with(0).with(1).with(2).with(3);
        assert!(!complex.admits(&abcd));
    }
}
