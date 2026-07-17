//! Shared semantic identifier newtypes.
//!
//! These are the ID types that both the causal/concurrency IR (this crate)
//! and its eventual PDDL/POWL producers (`bcinr-pddl`, `bcinr-powl`) need to
//! agree on. Widths follow the source design: `PlanningEpochId` is a u128
//! (room for a globally-unique epoch nonce, not just a counter),
//! `ActionOccurrenceId` is a u32 (bounded by `MAX_EPOCH_EVENTS` = 512 anyway,
//! see `event_set.rs`), `ConsequenceHorizonId` is content-addressed via
//! `Digest` rather than a counter. The four profile IDs and `PowlNodeId` are
//! not given an explicit width in the spec; u64 is used for all of them as
//! the workspace-wide default for opaque handle IDs.

use crate::digest::Digest;

macro_rules! id_newtype {
    ($(#[$meta:meta])* $name:ident($inner:ty)) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub $inner);

        impl $name {
            /// The wrapped raw value.
            pub const fn get(self) -> $inner {
                self.0
            }
        }

        impl From<$inner> for $name {
            fn from(v: $inner) -> Self {
                $name(v)
            }
        }
    };
}

id_newtype!(
    /// Identifies one grounded planning epoch (one bounded PDDL grounding +
    /// search run). u128-wide so a globally-unique nonce can be embedded
    /// rather than requiring a monotonic counter.
    PlanningEpochId(u128)
);

id_newtype!(
    /// Identifies one action occurrence within a `CausalPlan`. Bounded in
    /// practice by `MAX_EPOCH_EVENTS` (512, see `event_set.rs`), so u32 is
    /// generous headroom, not a tight bound.
    ActionOccurrenceId(u32)
);

id_newtype!(
    /// Content-addressed identifier for a "consequence horizon" — the
    /// digest IS the identity, there is no separate counter allocation.
    ConsequenceHorizonId(Digest)
);

id_newtype!(
    /// Identifies a named measure profile (e.g. which fitness/precision
    /// weighting a `ConformanceMetrics` computation used). Opaque handle,
    /// u64.
    MeasureProfileId(u64)
);

id_newtype!(
    /// Identifies a named search profile (which search-rail configuration
    /// produced an `ExhaustionWitness`). Opaque handle, u64.
    SearchProfileId(u64)
);

id_newtype!(
    /// Identifies a named selector profile (which portfolio/fairness
    /// selection policy chose an action). Opaque handle, u64.
    SelectorProfileId(u64)
);

id_newtype!(
    /// Identifies a named transformation profile (which semantic-caching /
    /// residualization transform a `SemanticOptimizationContract` licenses).
    /// Opaque handle, u64.
    TransformationProfileId(u64)
);

id_newtype!(
    /// Identifies a node in a POWL tape (`bcinr-powl`'s `PowlTape` /
    /// `PowlTapeLarge`). Opaque handle, u64 — this crate does not know the
    /// concrete `Powl64Op` shape, only that nodes are addressable by ID.
    PowlNodeId(u64)
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn newtypes_roundtrip_and_order() {
        let a = ActionOccurrenceId(1);
        let b = ActionOccurrenceId(2);
        assert!(a < b);
        assert_eq!(a.get(), 1);
        assert_eq!(ActionOccurrenceId::from(5).get(), 5);
    }

    #[test]
    fn consequence_horizon_id_is_content_addressed() {
        let d1 = Digest::hash(b"epoch-1");
        let d2 = Digest::hash(b"epoch-1");
        assert_eq!(ConsequenceHorizonId(d1), ConsequenceHorizonId(d2));
    }
}
