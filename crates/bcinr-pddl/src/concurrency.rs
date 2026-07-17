//! `bcinr_mfw_ir::ConcurrencyAnalyzer` implemented for [`GroundedPlanningEpoch`].
//!
//! # Formal standing — read before trusting this module's output
//!
//! [`bcinr_mfw_ir::contracts::LAW_EXECUTABLE_CONCURRENCY_INTERSECTION`] is
//! only `FormalStanding::Stated` (defined in Lean as `C_E = C_C ∩ C_T ∩ C_R`,
//! **not** proven downward-closed or a valid complex), and
//! [`bcinr_mfw_ir::contracts::LAW_MINIMAL_NONFACE_REPRESENTATION`] — the
//! representation this analyzer actually produces — is `Blocked` (no Lean
//! artifact of a minimal-nonfaces/Stanley-Reisner representation exists
//! anywhere in `/Users/sac/mfact`). Nothing in this module, its doc
//! comments, or its error messages should ever be read as claiming the
//! output is exact, complete, or proven — see
//! `bcinr_mfw_ir::concurrency`'s own module doc comment for the same
//! ceiling, which this implementation inherits.
//!
//! # What this construction actually does
//!
//! [`PddlConcurrencyAnalyzer::analyze`] builds one 2-element
//! [`MinimalNonFace`] per pair `bcinr_mfw_ir::CausalPlan::independence`
//! already marked `Dependent` — "two actions are pairwise-concurrency-
//! eligible iff `Independent`", exactly as the mission specifies. Because
//! `ExecutableConcurrencyComplex::admits` rejects any candidate `EventSet`
//! containing a recorded nonface as a subset, a 2-element nonface `{a, b}`
//! correctly blocks *every* candidate that contains both `a` and `b`,
//! regardless of what else is in the candidate — so pairwise dependence is
//! already sufficient to gate concurrency at the admission-check level for
//! this construction, without needing a separate 3-way lifting pass.
//!
//! What this construction does **not** do: detect genuine three-way (or
//! higher) conflicts that are not reducible to any single dependent pair —
//! e.g. a resource-capacity conflict where actions `A`, `B`, `C` are
//! pairwise fine but jointly exceed a shared capacity (the worked `{A,B,C}`
//! scenario in `bcinr_mfw_ir::concurrency`'s own tests). Detecting that
//! would require numeric-fluent capacity data this analyzer's input
//! (`Pddl8GroundAction`, no numeric-fluent slot — see `crate::causal`'s
//! module doc comment for the same limitation) does not carry. This is a
//! genuine, acknowledged gap (`PARTIAL`), not a bug: with the data available
//! here, there is nothing to detect that pairwise dependence didn't already
//! find. `crate::search`'s `QLensRail`/temporal pipeline (`GroundDurativeAction`,
//! which does carry numeric fluents) is where a future phase's three-way
//! capacity analysis would need to live.

use std::collections::BTreeMap;

use bcinr_mfw_ir::{
    ActionOccurrenceId, CausalPlan, ConcurrencyAnalyzer, ConcurrencyConflictWitness, Digest,
    EventSet, ExecutableConcurrencyComplex, MinimalNonFace, MAX_EPOCH_EVENTS,
};

use crate::capability::GroundedPlanningEpoch;

/// Real (not boilerplate) `ConcurrencyAnalyzer` for [`GroundedPlanningEpoch`],
/// built from a [`CausalPlan`]'s [`bcinr_mfw_ir::IndependenceRelation`]. See
/// the module doc comment for exactly what this does and does not detect.
#[derive(Debug, Clone, Copy, Default)]
pub struct PddlConcurrencyAnalyzer;

/// Errors [`PddlConcurrencyAnalyzer::analyze`] can produce.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConcurrencyAnalysisError {
    /// More occurrences than `EventSet`'s fixed 512-slot capacity — this
    /// construction cannot represent them.
    TooManyOccurrences(usize),
    /// `causal.independence.dependent` names a pair whose `ActionOccurrenceId`
    /// is not present in `causal.occurrences` — the `CausalPlan` is
    /// internally inconsistent (its independence relation and its
    /// occurrence list disagree about what occurrences exist).
    ///
    /// This is a hard refusal, not a silent skip: a recorded-`Dependent`
    /// pair that this analyzer cannot resolve to `EventSet` slots must
    /// never be treated as concurrency-admissible by omission. Before this
    /// variant existed, `analyze` `continue`d past such a pair, so no
    /// [`MinimalNonFace`] was ever emitted for it and
    /// [`ExecutableConcurrencyComplex::admits`] silently allowed it to fire
    /// concurrently — the exact "unknown defaults to no-conflict" pattern
    /// the independence relation's own default (`Dependent` unless proven
    /// `Independent`) exists to rule out. The current single production
    /// call site (`crate::mfw::planner`, which always derives `causal` and
    /// the epoch from the same `PddlCausalAnalyzer::analyze` call) never
    /// triggers this, since that path's occurrences and independence
    /// relation are always mutually consistent by construction — this
    /// guards `analyze`'s public contract against any other `CausalPlan`
    /// (a different `CausalAnalyzer` impl, or a hand-assembled one).
    UnresolvedDependentPair {
        left: ActionOccurrenceId,
        right: ActionOccurrenceId,
    },
}

impl std::fmt::Display for ConcurrencyAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooManyOccurrences(n) => write!(
                f,
                "{n} occurrences exceeds EventSet's {MAX_EPOCH_EVENTS}-slot capacity"
            ),
            Self::UnresolvedDependentPair { left, right } => write!(
                f,
                "independence.dependent names occurrence pair ({left:?}, {right:?}) not present \
                 in causal.occurrences -- refusing to silently admit an unresolvable Dependent \
                 pair as concurrency-eligible"
            ),
        }
    }
}

impl std::error::Error for ConcurrencyAnalysisError {}

impl ConcurrencyAnalyzer for PddlConcurrencyAnalyzer {
    type Epoch = GroundedPlanningEpoch;
    type Error = ConcurrencyAnalysisError;

    fn analyze(
        &self,
        _epoch: &GroundedPlanningEpoch,
        causal: &CausalPlan,
    ) -> Result<ExecutableConcurrencyComplex, ConcurrencyAnalysisError> {
        if causal.occurrences.len() > MAX_EPOCH_EVENTS {
            return Err(ConcurrencyAnalysisError::TooManyOccurrences(
                causal.occurrences.len(),
            ));
        }

        // Stable EventSet slot per occurrence: position in
        // `causal.occurrences`, not the (caller-assigned, possibly sparse)
        // `ActionOccurrenceId` itself — guarantees every slot is in-range.
        let slot_of: BTreeMap<bcinr_mfw_ir::ActionOccurrenceId, usize> = causal
            .occurrences
            .iter()
            .enumerate()
            .map(|(i, occ)| (occ.id, i))
            .collect();

        // Time complexity: this loop runs once per dependent pair
        // (`causal.independence.dependent`, up to O(n^2) pairs for n
        // occurrences), and each iteration's `.find()` below does a
        // *linear* scan of `precedes.edges` — the full O(n^2) total order
        // `PddlCausalAnalyzer::analyze` builds over all occurrence pairs
        // (see `causal.rs`), not a sparse causal-edge set. So this
        // function is O(m * n^2) where m = |dependent pairs| (itself up to
        // O(n^2)) — worst case O(n^4) in the occurrence count. This is
        // hidden behind BTreeMap/BTreeSet abstractions and easy to miss on
        // a shallow read; see also `causal.rs::PddlCausalAnalyzer::analyze`,
        // which has the equivalent note for `precedes`'s own construction.
        let mut minimal_nonfaces = Vec::new();
        let mut conflict_witnesses = BTreeMap::new();

        for (pair, dep_witness) in &causal.independence.dependent {
            let (Some(&i), Some(&j)) = (slot_of.get(&pair.left), slot_of.get(&pair.right)) else {
                // Refuse, don't silently skip: a `Dependent` pair this
                // analyzer cannot resolve to `EventSet` slots must never be
                // treated as concurrency-admissible by omission (no
                // `MinimalNonFace` emitted == `admits` allows it). See
                // `ConcurrencyAnalysisError::UnresolvedDependentPair`'s doc
                // comment.
                return Err(ConcurrencyAnalysisError::UnresolvedDependentPair {
                    left: pair.left,
                    right: pair.right,
                });
            };
            let members = EventSet::empty().with(i).with(j);

            // O(|precedes.edges|) linear scan per dependent pair — see this
            // loop's leading complexity note.
            let causal_edge = causal
                .precedes
                .edges
                .iter()
                .find(|e| {
                    (e.before == pair.left && e.after == pair.right)
                        || (e.before == pair.right && e.after == pair.left)
                })
                .copied();

            let witness = ConcurrencyConflictWitness {
                causal: causal_edge.map(|e| (*pair, e)),
                // No temporal/invariant or numeric-fluent capacity data on
                // this analyzer's input — see the module doc comment.
                temporal: None,
                resource: None,
            };

            let mut buf = Vec::new();
            buf.extend_from_slice(&(i as u64).to_le_bytes());
            buf.extend_from_slice(&(j as u64).to_le_bytes());
            for reason in &dep_witness.reasons {
                buf.push(*reason as u8);
            }
            let witness_digest = Digest::hash(&buf);

            conflict_witnesses.insert(witness_digest, witness);
            minimal_nonfaces.push(MinimalNonFace {
                members,
                witness_digest,
            });
        }

        let mut digest_buf = Vec::new();
        digest_buf.extend_from_slice(causal.digest.as_bytes());
        digest_buf.extend_from_slice(&(minimal_nonfaces.len() as u64).to_le_bytes());
        let digest = Digest::hash(&digest_buf);

        Ok(ExecutableConcurrencyComplex {
            event_count: causal.occurrences.len(),
            minimal_nonfaces,
            conflict_witnesses,
            digest,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::causal::PddlCausalAnalyzer;
    use crate::ground::GroundProblem;
    use crate::parse::{domain_from_pddl, problem_from_pddl};
    use bcinr_mfw_ir::{
        ActionOccurrence, ActionOccurrenceId, CausalAnalyzer, EpochBounds, PlanningEpochId,
    };

    fn epoch_from(domain_pddl: &str, problem_pddl: &str) -> GroundedPlanningEpoch {
        let domain = domain_from_pddl(domain_pddl).unwrap();
        let problem = problem_from_pddl(problem_pddl).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let bounds = EpochBounds {
            max_ground_actions: 64,
            max_plan_depth: 64,
            max_search_steps: 1000,
            max_partition_boxes: 8,
        };
        let mut epoch = GroundedPlanningEpoch::from_ground_problem(&gp, Digest::hash(b"t"), bounds);
        epoch.id = PlanningEpochId(1);
        epoch
    }

    fn occ(id: u32, action_index: u64) -> ActionOccurrence {
        ActionOccurrence {
            id: ActionOccurrenceId(id),
            action: action_index,
        }
    }

    #[test]
    fn dependent_pair_becomes_a_minimal_nonface_that_blocks_admission() {
        let domain = "(define (domain d) (:predicates (p) (q)) \
                       (:action a1 :parameters () :precondition () :effect (p)) \
                       (:action a2 :parameters () :precondition (p) :effect (q)))";
        let problem = "(define (problem pr) (:domain d) (:init) (:goal (and (p) (q))))";
        let epoch = epoch_from(domain, problem);
        let occurrences = vec![occ(0, 0), occ(1, 1)];
        let causal_plan = PddlCausalAnalyzer.analyze(&epoch, &occurrences).unwrap();
        assert_eq!(causal_plan.independence.dependent.len(), 1);

        let complex = PddlConcurrencyAnalyzer
            .analyze(&epoch, &causal_plan)
            .unwrap();
        assert_eq!(complex.minimal_nonfaces.len(), 1);

        let both = EventSet::empty().with(0).with(1);
        assert!(
            !complex.admits(&both),
            "the dependent pair must not be admitted as jointly concurrent"
        );
        let just_zero = EventSet::empty().with(0);
        assert!(
            complex.admits(&just_zero),
            "a single-member set can never violate a 2-element nonface"
        );
    }

    #[test]
    fn independent_pair_produces_no_nonface_and_is_admitted() {
        let domain = "(define (domain d) (:predicates (p) (q)) \
                       (:action a1 :parameters () :precondition () :effect (p)) \
                       (:action a2 :parameters () :precondition () :effect (q)))";
        let problem = "(define (problem pr) (:domain d) (:init) (:goal (and (p) (q))))";
        let epoch = epoch_from(domain, problem);
        let occurrences = vec![occ(0, 0), occ(1, 1)];
        let causal_plan = PddlCausalAnalyzer.analyze(&epoch, &occurrences).unwrap();
        assert!(causal_plan.independence.dependent.is_empty());

        let complex = PddlConcurrencyAnalyzer
            .analyze(&epoch, &causal_plan)
            .unwrap();
        assert!(complex.minimal_nonfaces.is_empty());
        let both = EventSet::empty().with(0).with(1);
        assert!(complex.admits(&both));
    }

    #[test]
    fn a_dependent_pair_unresolvable_to_occurrence_slots_is_refused_not_silently_admitted() {
        // A hand-assembled, internally-inconsistent `CausalPlan`: its
        // `independence.dependent` names an `ActionPair` whose occurrence
        // IDs are not in `occurrences` at all (e.g. produced by a
        // different `CausalAnalyzer` impl, or built by hand). Before this
        // fix, `analyze`'s `slot_of.get(..)` lookup failure was silently
        // `continue`d past: no `MinimalNonFace` was emitted for the pair,
        // so `ExecutableConcurrencyComplex::admits` treated it as
        // concurrency-admissible by omission -- a genuinely `Dependent`
        // pair silently defaulting to "no conflict". `analyze` must now
        // refuse instead.
        use bcinr_mfw_ir::{
            ActionPair, CausalPlan, DependenceReason, DependenceWitness, IndependenceRelation,
            StrictPartialOrder,
        };
        use std::collections::{BTreeMap, BTreeSet};

        let domain = "(define (domain d) (:predicates (p) (q)) \
                       (:action a1 :parameters () :precondition () :effect (p)) \
                       (:action a2 :parameters () :precondition (p) :effect (q)))";
        let problem = "(define (problem pr) (:domain d) (:init) (:goal (and (p) (q))))";
        let epoch = epoch_from(domain, problem);

        // `occurrences` only knows about id 0; the dependent pair below
        // references id 7, which has no slot.
        let unresolvable_pair = ActionPair::new(ActionOccurrenceId(0), ActionOccurrenceId(7));
        let mut dependent = BTreeMap::new();
        dependent.insert(
            unresolvable_pair,
            DependenceWitness {
                reasons: BTreeSet::from([DependenceReason::CausalSupport]),
                threatened_atoms: BTreeSet::new(),
            },
        );
        let malformed = CausalPlan {
            epoch: bcinr_mfw_ir::PlanningEpochId(1),
            occurrences: vec![occ(0, 0)],
            precedes: StrictPartialOrder::default(),
            independence: IndependenceRelation {
                independent: BTreeMap::new(),
                dependent,
            },
            support_edges: BTreeSet::new(),
            digest: Digest::hash(b"malformed"),
        };

        let result = PddlConcurrencyAnalyzer.analyze(&epoch, &malformed);
        assert_eq!(
            result,
            Err(ConcurrencyAnalysisError::UnresolvedDependentPair {
                left: ActionOccurrenceId(0),
                right: ActionOccurrenceId(7),
            }),
            "an unresolvable Dependent pair must be refused, never silently admitted"
        );
    }
}
