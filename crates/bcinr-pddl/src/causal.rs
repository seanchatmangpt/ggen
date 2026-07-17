//! `bcinr_mfw_ir::CausalAnalyzer` implemented for [`GroundedPlanningEpoch`].
//!
//! # What is genuinely checked, and what is vacuous
//!
//! Per the mission's explicit safety direction — under-approximating
//! independence is safe, over-approximating is not — [`PddlCausalAnalyzer`]
//! defaults every action pair to `Dependent` and only emits
//! [`IndependenceWitness`] when **both** of the two sub-checks this data
//! type can actually support come back clean:
//!
//! - **`effects_commute`** — genuinely simulated: `simulate_two` applies
//!   action A then B, and separately B then A, starting from the real state
//!   the plan was in immediately before the earlier of the two occurrences
//!   fired (`states_before[i]`, computed by replaying the whole occurrence
//!   sequence once). `commute` is only `true` if **both** orderings are
//!   applicable (preconditions hold) from that state **and** produce
//!   byte-identical resulting fact sets.
//! - **`preconditions_stable`** — genuinely computed: real set intersection
//!   of each action's `del_effects` against the other's `preconditions`
//!   (`threatened_atoms`), in both directions.
//!
//! The other three sub-witnesses (`invariants_stable`, `numeric_flow`,
//! `trajectory`) are set to vacuously-`true`/empty for every pair, **not**
//! because they were checked and found fine, but because
//! `wasm4pm_compat::pddl::Pddl8GroundAction` — the classical, STRIPS-only
//! ground-action representation `GroundedPlanningEpoch` carries — has no
//! invariant, numeric-fluent, or temporal-constraint slot at all to check
//! against. A vacuous truth over an empty domain is not the same claim as
//! "verified and passing" — this doc comment and every relevant field's own
//! doc comment say so explicitly, and this crate's honesty vocabulary
//! (`no-overclaiming-conversational.md`) requires exactly this disclosure.
//! `PddlFeature::NumericFluents`/`NumericEffects` are only wired up through
//! `GroundTemporalProblem`/`GroundDurativeAction` (see `crate::capability`),
//! which `GroundedPlanningEpoch`/`PddlCausalAnalyzer` do not consume — a
//! genuine scope limit of this analyzer, stated plainly here rather than
//! silently assumed away.
//!
//! `CausalSupportEdge`'s `AtomId`/`FluentId` fields are populated via
//! [`atom_id`], a deterministic (not guaranteed collision-free) hash of the
//! atom's label truncated to 32 bits — adequate for a witness field at the
//! problem sizes this crate's `PDDL8_MAX_GROUND` bound implies, not a
//! claimed bijection.

use std::collections::{BTreeMap, BTreeSet};

use bcinr_mfw_ir::{
    ActionOccurrence, ActionPair, AtomId, CausalAnalyzer, CausalPlan, CausalSupportEdge,
    DependenceReason, DependenceWitness, Digest, EffectsCommuteWitness, IndependenceRelation,
    IndependenceWitness, InvariantsStableWitness, NumericFlowWitness, PrecedenceEdge,
    PreconditionsStableWitness, StrictPartialOrder, SupportObject, TrajectoryWitness,
};
use wasm4pm_compat::pddl::{Pddl8GroundAction, Pddl8GroundAtom};

use crate::capability::GroundedPlanningEpoch;

/// Real (not boilerplate) `CausalAnalyzer` for [`GroundedPlanningEpoch`].
#[derive(Debug, Clone, Copy, Default)]
pub struct PddlCausalAnalyzer;

/// Errors [`PddlCausalAnalyzer::analyze`] can produce.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CausalAnalysisError {
    /// An `ActionOccurrence.action` index was out of range for
    /// `epoch.actions`.
    ActionIndexOutOfRange {
        occurrence: bcinr_mfw_ir::ActionOccurrenceId,
        action_index: u64,
    },
    /// Replaying `occurrences` in order from `epoch.initial_state` hit an
    /// action whose preconditions did not hold — the supplied occurrence
    /// sequence is not a valid linear plan over this epoch, so no causal
    /// analysis can be trusted.
    ReplayPreconditionFailure {
        occurrence: bcinr_mfw_ir::ActionOccurrenceId,
    },
}

impl std::fmt::Display for CausalAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ActionIndexOutOfRange {
                occurrence,
                action_index,
            } => write!(
                f,
                "occurrence {occurrence:?} references out-of-range action index {action_index}"
            ),
            Self::ReplayPreconditionFailure { occurrence } => write!(
                f,
                "occurrence {occurrence:?}'s preconditions did not hold when replaying the plan"
            ),
        }
    }
}

impl std::error::Error for CausalAnalysisError {}

impl CausalAnalyzer for PddlCausalAnalyzer {
    type Epoch = GroundedPlanningEpoch;
    type Error = CausalAnalysisError;

    fn analyze(
        &self,
        epoch: &GroundedPlanningEpoch,
        occurrences: &[ActionOccurrence],
    ) -> Result<CausalPlan, CausalAnalysisError> {
        // Resolve each occurrence's ground action.
        let actions: Vec<&Pddl8GroundAction> = occurrences
            .iter()
            .map(|occ| {
                epoch.actions.get(occ.action as usize).ok_or(
                    CausalAnalysisError::ActionIndexOutOfRange {
                        occurrence: occ.id,
                        action_index: occ.action,
                    },
                )
            })
            .collect::<Result<_, _>>()?;

        // Replay the sequence once, recording the state immediately before
        // each occurrence fires — the real, reachable "relevant state
        // slice" every pairwise check below is grounded in.
        let mut states_before = Vec::with_capacity(occurrences.len());
        let mut state = epoch.initial_state.clone();
        for (occ, action) in occurrences.iter().zip(actions.iter()) {
            states_before.push(state.clone());
            if !action.preconditions.iter().all(|p| state.contains(p)) {
                return Err(CausalAnalysisError::ReplayPreconditionFailure { occurrence: occ.id });
            }
            for d in &action.del_effects {
                state.remove(d);
            }
            for a in &action.add_effects {
                state.insert(a.clone());
            }
        }

        // `precedes`: the plan's own total execution order. This is the
        // order the occurrences actually ran in, not yet reduced to a
        // minimal "necessary ordering" (which would only include pairs
        // `IndependenceRelation` marks `Dependent`) — the full total order
        // is what genuinely happened, and callers that want the reduced
        // form can derive it from `independence.dependent`'s keys.
        //
        // Time complexity: this is an O(n^2) loop in the occurrence count
        // (every unordered pair), building the full total order — not
        // O(1)/O(log n) as this workspace's stated default for primitives
        // would suggest. The independence loop directly below is another,
        // separate O(n^2) pass whose per-pair `analyze_pair` call also
        // clones/mutates `BTreeSet<Pddl8GroundAtom>` state proportional to
        // each action's precondition/effect-list size (via
        // `simulate_two`), so the real cost of this function is at least
        // O(n^2 * k) where k = typical precondition/effect set size.
        let mut precedes = StrictPartialOrder::default();
        for i in 0..occurrences.len() {
            for j in (i + 1)..occurrences.len() {
                precedes.edges.insert(PrecedenceEdge {
                    before: occurrences[i].id,
                    after: occurrences[j].id,
                });
            }
        }

        let mut independent = BTreeMap::new();
        let mut dependent = BTreeMap::new();
        let mut support_edges = BTreeSet::new();

        for i in 0..occurrences.len() {
            for j in (i + 1)..occurrences.len() {
                let pair = ActionPair::new(occurrences[i].id, occurrences[j].id);
                let (witness, reasons, threatened_atoms, edges) = analyze_pair(
                    occurrences[i].id,
                    occurrences[j].id,
                    actions[i],
                    actions[j],
                    &states_before[i],
                );
                support_edges.extend(edges);
                match witness {
                    Some(w) => {
                        independent.insert(pair, w);
                    }
                    None => {
                        dependent.insert(
                            pair,
                            DependenceWitness {
                                reasons,
                                threatened_atoms,
                            },
                        );
                    }
                }
            }
        }

        let mut digest_buf = Vec::new();
        digest_buf.extend_from_slice(epoch.theory_digest.as_bytes());
        digest_buf.extend_from_slice(&(occurrences.len() as u64).to_le_bytes());
        digest_buf.extend_from_slice(&(independent.len() as u64).to_le_bytes());
        digest_buf.extend_from_slice(&(dependent.len() as u64).to_le_bytes());
        let digest = Digest::hash(&digest_buf);

        Ok(CausalPlan {
            epoch: epoch.id,
            occurrences: occurrences.to_vec(),
            precedes,
            independence: IndependenceRelation {
                independent,
                dependent,
            },
            support_edges,
            digest,
        })
    }
}

/// Simulate applying `first` then `second` starting from `base`; `None` if
/// either action's preconditions do not hold at the point it would fire.
fn simulate_two(
    first: &Pddl8GroundAction,
    second: &Pddl8GroundAction,
    base: &BTreeSet<Pddl8GroundAtom>,
) -> Option<BTreeSet<Pddl8GroundAtom>> {
    if !first.preconditions.iter().all(|p| base.contains(p)) {
        return None;
    }
    let mut mid = base.clone();
    for d in &first.del_effects {
        mid.remove(d);
    }
    for a in &first.add_effects {
        mid.insert(a.clone());
    }
    if !second.preconditions.iter().all(|p| mid.contains(p)) {
        return None;
    }
    let mut end = mid;
    for d in &second.del_effects {
        end.remove(d);
    }
    for a in &second.add_effects {
        end.insert(a.clone());
    }
    Some(end)
}

/// Genuinely analyze one action pair. Returns
/// `(Some(witness), reasons, threatened_atoms, edges)` only when both real
/// checks (effects-commute, precondition-stability) pass and neither found a
/// causal-support/delete-interference reason; otherwise
/// `(None, reasons, threatened_atoms, edges)` — see the module doc comment
/// for exactly which sub-witnesses are real vs. vacuous. `threatened_atoms`
/// is the atom-level provenance for a real `DependenceReason::
/// DeleteInterference` finding (empty when that reason is absent, or present
/// only via the conservative "closest available reason" fallback below with
/// no directly identified atom) — the `Dependent`-side mirror of
/// `CausalSupportEdge`'s provenance for `DependenceReason::CausalSupport`.
fn analyze_pair(
    id_a: bcinr_mfw_ir::ActionOccurrenceId,
    id_b: bcinr_mfw_ir::ActionOccurrenceId,
    action_a: &Pddl8GroundAction,
    action_b: &Pddl8GroundAction,
    base: &BTreeSet<Pddl8GroundAtom>,
) -> (
    Option<IndependenceWitness>,
    BTreeSet<DependenceReason>,
    BTreeSet<AtomId>,
    Vec<CausalSupportEdge>,
) {
    let mut reasons = BTreeSet::new();
    let mut support_edges = Vec::new();

    // Causal support: does the other action need an atom this one's effects
    // establish, that was not already true in `base`?
    for atom in &action_b.preconditions {
        if action_a.add_effects.contains(atom) && !base.contains(atom) {
            reasons.insert(DependenceReason::CausalSupport);
            support_edges.push(CausalSupportEdge {
                producer: id_a,
                consumer: id_b,
                fact: SupportObject::Atom(atom_id(atom)),
            });
        }
    }
    for atom in &action_a.preconditions {
        if action_b.add_effects.contains(atom) && !base.contains(atom) {
            reasons.insert(DependenceReason::CausalSupport);
            support_edges.push(CausalSupportEdge {
                producer: id_b,
                consumer: id_a,
                fact: SupportObject::Atom(atom_id(atom)),
            });
        }
    }

    // Delete interference: one action's delete effects threaten the
    // other's preconditions, in either direction.
    let mut threatened: BTreeSet<AtomId> = BTreeSet::new();
    for atom in &action_a.del_effects {
        if action_b.preconditions.contains(atom) {
            reasons.insert(DependenceReason::DeleteInterference);
            threatened.insert(atom_id(atom));
        }
    }
    for atom in &action_b.del_effects {
        if action_a.preconditions.contains(atom) {
            reasons.insert(DependenceReason::DeleteInterference);
            threatened.insert(atom_id(atom));
        }
    }

    // Effects-commute: real forward simulation of both orderings.
    let ab = simulate_two(action_a, action_b, base);
    let ba = simulate_two(action_b, action_a, base);
    let commute = matches!((&ab, &ba), (Some(x), Some(y)) if x == y);
    if !commute && reasons.is_empty() {
        // Effects provably do not commute, but neither direct check above
        // explains why (e.g. one ordering is inapplicable from `base` for a
        // reason other than a direct precondition/delete conflict between
        // exactly these two actions). Conservatively still mark them
        // Dependent — never invent Independent without a passing check —
        // using the closest available reason.
        reasons.insert(DependenceReason::DeleteInterference);
    }

    if reasons.is_empty() && commute {
        let shared_atoms: BTreeSet<AtomId> = action_a
            .preconditions
            .iter()
            .chain(action_a.add_effects.iter())
            .chain(action_a.del_effects.iter())
            .filter(|atom| {
                action_b.preconditions.contains(atom)
                    || action_b.add_effects.contains(atom)
                    || action_b.del_effects.contains(atom)
            })
            .map(atom_id)
            .collect();

        let witness = IndependenceWitness {
            effects_commute: EffectsCommuteWitness {
                commute: true,
                left_effects_digest: effects_digest(action_a),
                right_effects_digest: effects_digest(action_b),
                shared_atoms,
                shared_fluents: BTreeSet::new(),
            },
            preconditions_stable: PreconditionsStableWitness {
                stable: true,
                left_preconditions_digest: preconditions_digest(action_a),
                right_preconditions_digest: preconditions_digest(action_b),
                threatened_atoms: BTreeSet::new(),
            },
            // Vacuous — see this module's doc comment: Pddl8GroundAction has
            // no invariant/numeric-fluent/temporal-constraint slot at all.
            invariants_stable: InvariantsStableWitness {
                stable: true,
                invariants_digest: Digest::ZERO,
                checked_invariants: BTreeSet::new(),
            },
            numeric_flow: NumericFlowWitness {
                commute: true,
                flow_digest: Digest::ZERO,
                touched_fluents: BTreeSet::new(),
            },
            trajectory: TrajectoryWitness {
                consistent: true,
                trajectory_digest: Digest::ZERO,
                checked_constraints: BTreeSet::new(),
            },
        };
        (Some(witness), reasons, threatened, support_edges)
    } else {
        (None, reasons, threatened, support_edges)
    }
}

/// Deterministic (not guaranteed collision-free) 32-bit id for a ground
/// atom — see the module doc comment.
fn atom_id(atom: &Pddl8GroundAtom) -> AtomId {
    let digest = Digest::hash(atom.label().as_bytes());
    let b = digest.as_bytes();
    AtomId(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
}

fn effects_digest(action: &Pddl8GroundAction) -> Digest {
    let mut buf = Vec::new();
    for a in &action.add_effects {
        buf.extend_from_slice(b"+");
        buf.extend_from_slice(a.label().as_bytes());
    }
    for d in &action.del_effects {
        buf.extend_from_slice(b"-");
        buf.extend_from_slice(d.label().as_bytes());
    }
    Digest::hash(&buf)
}

fn preconditions_digest(action: &Pddl8GroundAction) -> Digest {
    let mut buf = Vec::new();
    for p in &action.preconditions {
        buf.extend_from_slice(p.label().as_bytes());
        buf.push(0);
    }
    Digest::hash(&buf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ground::GroundProblem;
    use crate::parse::{domain_from_pddl, problem_from_pddl};
    use bcinr_mfw_ir::{ActionOccurrenceId, EpochBounds, PlanningEpochId};

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
    fn independent_actions_on_disjoint_atoms_are_marked_independent() {
        // Two actions touching entirely disjoint predicates: genuinely
        // independent (commute, no shared preconditions/effects).
        let domain = "(define (domain d) (:predicates (p) (q)) \
                       (:action a1 :parameters () :precondition () :effect (p)) \
                       (:action a2 :parameters () :precondition () :effect (q)))";
        let problem = "(define (problem pr) (:domain d) (:init) (:goal (and (p) (q))))";
        let epoch = epoch_from(domain, problem);
        // action index 0 = a1, 1 = a2 (declaration order, matches ground_schema).
        let occurrences = vec![occ(0, 0), occ(1, 1)];
        let analyzer = PddlCausalAnalyzer;
        let plan = analyzer.analyze(&epoch, &occurrences).unwrap();
        assert_eq!(plan.independence.independent.len(), 1);
        assert!(plan.independence.dependent.is_empty());
    }

    #[test]
    fn causally_dependent_actions_are_marked_dependent_with_causal_support_reason() {
        // a1 establishes p; a2 needs p. Genuinely dependent.
        let domain = "(define (domain d) (:predicates (p) (q)) \
                       (:action a1 :parameters () :precondition () :effect (p)) \
                       (:action a2 :parameters () :precondition (p) :effect (q)))";
        let problem = "(define (problem pr) (:domain d) (:init) (:goal (and (p) (q))))";
        let epoch = epoch_from(domain, problem);
        let occurrences = vec![occ(0, 0), occ(1, 1)];
        let analyzer = PddlCausalAnalyzer;
        let plan = analyzer.analyze(&epoch, &occurrences).unwrap();
        assert!(plan.independence.independent.is_empty());
        assert_eq!(plan.independence.dependent.len(), 1);
        let (_, witness) = plan.independence.dependent.iter().next().unwrap();
        assert!(witness.reasons.contains(&DependenceReason::CausalSupport));
        assert_eq!(plan.support_edges.len(), 1);
    }

    #[test]
    fn delete_interference_is_detected() {
        // a1 deletes p; a2 needs p to already hold. Dependent via
        // DeleteInterference (order matters: a2 must fire before a1, or
        // never after).
        let domain = "(define (domain d) (:predicates (p) (q)) \
                       (:action a1 :parameters () :precondition () :effect (and (not (p)) (q))) \
                       (:action a2 :parameters () :precondition (p) :effect ()))";
        let problem = "(define (problem pr) (:domain d) (:init (p)) (:goal (q)))";
        let epoch = epoch_from(domain, problem);
        // a2 fires first (its precondition p holds initially), then a1.
        let occurrences = vec![occ(0, 1), occ(1, 0)];
        let analyzer = PddlCausalAnalyzer;
        let plan = analyzer.analyze(&epoch, &occurrences).unwrap();
        assert!(plan.independence.independent.is_empty());
        let (_, witness) = plan.independence.dependent.iter().next().unwrap();
        assert!(witness
            .reasons
            .contains(&DependenceReason::DeleteInterference));

        // `threatened_atoms` is the atom-level provenance for this finding
        // (the `Dependent`-side mirror of `CausalSupportEdge`'s provenance
        // for `CausalSupport`) -- before this fix, the atoms threatened by
        // a real DeleteInterference finding were computed (`analyze_pair`'s
        // local `threatened` set) but discarded, never reaching the
        // witness (`clippy::collection_is_never_read`). `p` (ground atom
        // with no args) is the exact predicate a1's delete effect threatens
        // against a2's precondition.
        let p_atom = wasm4pm_compat::pddl::Pddl8GroundAtom {
            pred: "p".to_string(),
            args: vec![],
        };
        assert_eq!(
            witness.threatened_atoms,
            BTreeSet::from([atom_id(&p_atom)]),
            "threatened_atoms must name the specific atom (p) the \
             DeleteInterference finding is about, not be empty"
        );
    }

    /// Covers `analyze_pair`'s conservative fallback (the
    /// `if !commute && reasons.is_empty()` branch): an add/delete conflict
    /// on the *same atom* between two actions with **disjoint, empty
    /// preconditions**, so neither of the two direct checks
    /// (`CausalSupport`, `DeleteInterference`) can see a reason — a1 has no
    /// preconditions and no del_effects, a2 has no preconditions and its
    /// only del_effect (`p`) is never a precondition of a1's — yet AB and BA
    /// orderings genuinely produce different final states (`{}` vs `{p}`,
    /// since `p` starts absent), so `commute` is real and false. Before this
    /// test, no test in the suite exercised this branch at all: deleting the
    /// `if !commute && reasons.is_empty() { reasons.insert(...) }` line, or
    /// inverting its condition, would silently flip this pair to
    /// `Independent` — violating the load-bearing invariant that
    /// independence defaults to `Dependent`, never `Independent`, when
    /// unknown — and nothing would have caught it.
    #[test]
    fn add_delete_conflict_without_shared_precondition_falls_back_to_conservative_dependent() {
        let domain = "(define (domain d) (:predicates (p)) \
                       (:action a1 :parameters () :precondition () :effect (p)) \
                       (:action a2 :parameters () :precondition () :effect (not (p))))";
        let problem = "(define (problem pr) (:domain d) (:init) (:goal (p)))";
        let epoch = epoch_from(domain, problem);
        // a1 (adds p) then a2 (deletes p): both preconditions are trivially
        // satisfied (empty), so replay succeeds regardless of order.
        let occurrences = vec![occ(0, 0), occ(1, 1)];
        let analyzer = PddlCausalAnalyzer;
        let plan = analyzer.analyze(&epoch, &occurrences).unwrap();

        assert!(
            plan.independence.independent.is_empty(),
            "must never be marked Independent: neither commute-check nor a \
             direct reason actually verified independence for this pair"
        );
        assert_eq!(plan.independence.dependent.len(), 1);
        let (_, witness) = plan.independence.dependent.iter().next().unwrap();
        assert!(
            !witness.reasons.is_empty(),
            "the conservative fallback must record a reason, not leave \
             `reasons` empty — an empty-but-Dependent witness is exactly \
             the shape a future regression on this branch would produce"
        );
        assert!(witness
            .reasons
            .contains(&DependenceReason::DeleteInterference));
        assert!(
            witness.threatened_atoms.is_empty(),
            "the conservative fallback names DeleteInterference as the \
             closest available reason without ever identifying a specific \
             threatened atom (neither direct check fired) -- threatened_atoms \
             must stay empty here, distinguishing this placeholder case from \
             a genuine atom-backed DeleteInterference finding \
             (delete_interference_is_detected)"
        );
    }

    #[test]
    fn out_of_range_action_index_is_a_real_error_not_a_panic() {
        let epoch = epoch_from(
            "(define (domain d) (:predicates (p)) \
             (:action a :parameters () :precondition () :effect (p)))",
            "(define (problem pr) (:domain d) (:init) (:goal (p)))",
        );
        let occurrences = vec![occ(0, 99)];
        let analyzer = PddlCausalAnalyzer;
        let err = analyzer.analyze(&epoch, &occurrences).unwrap_err();
        assert!(matches!(
            err,
            CausalAnalysisError::ActionIndexOutOfRange { .. }
        ));
    }
}
