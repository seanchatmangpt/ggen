//! POWL projection contract: witness types for "a PDDL causal plan +
//! concurrency complex was projected into a POWL tape, and the projection
//! preserved source semantics" — **not** `PowlModel` itself.
//!
//! The source design had a slight circularity: "the POWL projection
//! contract lives in `bcinr-mfw-ir`" versus "a `PowlProjection` contains a
//! `PowlModel`, which lives in `bcinr-powl`." Defining a concrete
//! `PowlModel` type here would force this crate to either duplicate
//! `bcinr-powl`'s tape types or depend on `bcinr-powl` directly — both
//! violate the "zero path-dependency on bcinr-pddl or bcinr-powl" rule this
//! crate exists to uphold (they depend on this crate, never the reverse).
//!
//! This module resolves the circularity by making [`PowlProjector::Model`]
//! an **associated type**: `bcinr-powl` plugs in its own concrete
//! `PowlModel` (or whatever wraps `PowlTape`/`PowlTapeLarge`) at the
//! implementation site, while this crate only ever talks about the witness
//! types that attest the projection was semantics-preserving. As the
//! ground truth notes, this witness — "a PDDL-to-POWL projection preserved
//! source semantics" — is genuinely new territory: `bcinr-powl-receipt`'s
//! existing `OcelCausalReceipt`/`PowlReplayFrame` attest to
//! execution/replay conformance of an *already-compiled* tape, not to the
//! compilation step itself.

use std::collections::BTreeMap;

use crate::causal::CausalPlan;
use crate::concurrency::ExecutableConcurrencyComplex;
use crate::digest::Digest;
use crate::ids::{ActionOccurrenceId, PlanningEpochId, PowlNodeId};

/// A bijection between source action occurrences and target POWL tape
/// nodes, held both directions so callers don't have to invert a
/// `BTreeMap` on demand.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct ActionNodeBijection {
    pub action_to_node: BTreeMap<ActionOccurrenceId, PowlNodeId>,
    pub node_to_action: BTreeMap<PowlNodeId, ActionOccurrenceId>,
}

/// Witness that precedence order was preserved by the projection: digests
/// of the source order, the order as naively projected, and the order as
/// mapped through the bijection — a projector should establish these are
/// consistent (e.g. `mapped_order_digest == projected_order_digest`)
/// before returning a witness claiming preservation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OrderPreservationWitness {
    pub source_order_digest: Digest,
    pub projected_order_digest: Digest,
    pub mapped_order_digest: Digest,
}

/// Witness that the executable-concurrency complex was preserved by the
/// projection: digests of the source complex, the source complex mapped
/// through the bijection, and the target (POWL-side) complex.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConcurrencyPreservationWitness {
    pub source_complex_digest: Digest,
    pub mapped_source_digest: Digest,
    pub target_complex_digest: Digest,
}

/// Full witness that a PDDL causal plan + concurrency complex was
/// projected into a POWL model without losing source semantics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PowlProjectionWitness {
    pub source_epoch: PlanningEpochId,
    pub causal_plan_digest: Digest,
    pub source_concurrency_digest: Digest,
    pub action_node_bijection: ActionNodeBijection,
    pub order_witness: OrderPreservationWitness,
    pub concurrency_witness: ConcurrencyPreservationWitness,
    pub digest: Digest,
}

/// Projects a `CausalPlan` + `ExecutableConcurrencyComplex` into a POWL
/// model, returning both the model and a witness that the projection
/// preserved source semantics. `Model` is an associated type so
/// `bcinr-powl` can plug in its own concrete model type without this crate
/// depending on `bcinr-powl` (see the module-level doc comment).
pub trait PowlProjector {
    type Model;
    type Error;

    fn project(
        &self, causal: &CausalPlan, concurrency: &ExecutableConcurrencyComplex,
    ) -> Result<(Self::Model, PowlProjectionWitness), Self::Error>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::causal::{ActionOccurrence, IndependenceRelation, StrictPartialOrder};
    use crate::concurrency::ExecutableConcurrencyComplex;
    use std::collections::BTreeSet;

    struct MockProjector;

    #[derive(Debug)]
    struct MockError;

    /// A stand-in for `bcinr-powl`'s eventual `PowlModel` — deliberately
    /// trivial, this crate has no business knowing the real shape.
    #[derive(Debug, PartialEq, Eq)]
    struct MockModel {
        node_count: usize,
    }

    impl PowlProjector for MockProjector {
        type Model = MockModel;
        type Error = MockError;

        fn project(
            &self, causal: &CausalPlan, _concurrency: &ExecutableConcurrencyComplex,
        ) -> Result<(Self::Model, PowlProjectionWitness), Self::Error> {
            let mut action_to_node = BTreeMap::new();
            let mut node_to_action = BTreeMap::new();
            for (i, occ) in causal.occurrences.iter().enumerate() {
                let node = PowlNodeId(i as u64);
                action_to_node.insert(occ.id, node);
                node_to_action.insert(node, occ.id);
            }
            let witness = PowlProjectionWitness {
                source_epoch: causal.epoch,
                causal_plan_digest: causal.digest,
                source_concurrency_digest: Digest::hash(b"complex"),
                action_node_bijection: ActionNodeBijection {
                    action_to_node,
                    node_to_action,
                },
                order_witness: OrderPreservationWitness {
                    source_order_digest: Digest::ZERO,
                    projected_order_digest: Digest::ZERO,
                    mapped_order_digest: Digest::ZERO,
                },
                concurrency_witness: ConcurrencyPreservationWitness {
                    source_complex_digest: Digest::ZERO,
                    mapped_source_digest: Digest::ZERO,
                    target_complex_digest: Digest::ZERO,
                },
                digest: Digest::hash(b"projection"),
            };
            Ok((
                MockModel {
                    node_count: causal.occurrences.len(),
                },
                witness,
            ))
        }
    }

    #[test]
    fn mock_projector_compiles_and_runs() {
        let causal = CausalPlan {
            epoch: PlanningEpochId(1),
            occurrences: vec![ActionOccurrence {
                id: ActionOccurrenceId(0),
                action: 1,
            }],
            precedes: StrictPartialOrder::default(),
            independence: IndependenceRelation::default(),
            support_edges: BTreeSet::new(),
            digest: Digest::hash(b"causal"),
        };
        let complex = ExecutableConcurrencyComplex {
            event_count: 1,
            minimal_nonfaces: vec![],
            conflict_witnesses: BTreeMap::new(),
            digest: Digest::hash(b"complex"),
        };
        let projector = MockProjector;
        let (model, witness) = projector.project(&causal, &complex).unwrap();
        assert_eq!(model.node_count, 1);
        assert_eq!(witness.action_node_bijection.action_to_node.len(), 1);
    }
}
