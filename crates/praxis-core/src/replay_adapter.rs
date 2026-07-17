//! `replay_adapter` — fixed 3-node SEQ POWL token model for the LawObject
//! lifecycle (`judge -> admit -> receipt`), replayed via
//! `bcinr_powl_receipt::replay::PowlReplayVerifier` to produce real
//! [`ConformanceMetrics`].
//!
//! This is a *lifecycle-conformance* model, distinct from the BLAKE3 hash
//! chain in `law.rs`/`receipt_record.rs`: it checks that a receipt only ever
//! gets emitted after judge and admit have fired, in that order (POWL
//! token-passing conformance), not that the hash chain itself is intact
//! (that's `crate::receipt_validator`'s `chain_recompute`/`chain_linkage`
//! stages).

use bcinr_powl_receipt::{
    conformance::ConformanceMetrics,
    replay::{PowlReplayFrame, PowlReplayVerifier, ReplayViolation},
};

use crate::receipt_record::ReceiptRecord;

/// Token bits (token-flow state). Disjoint from the `NODE_BIT_*` identity
/// space below, per `PowlReplayFrame`'s node-bit/token-bit separation.
pub const TOK_START: u64 = 1 << 0;
/// Token produced once `judge` has fired.
pub const TOK_JUDGED: u64 = 1 << 1;
/// Token produced once `admit` has fired.
pub const TOK_ADMITTED: u64 = 1 << 2;
/// Terminal token produced once `receipt` has fired.
pub const TOK_DONE: u64 = 1 << 3;

/// 1-hot node-identity bit for `judge`.
pub const NODE_BIT_JUDGE: u64 = 1 << 8;
/// 1-hot node-identity bit for `admit`.
pub const NODE_BIT_ADMIT: u64 = 1 << 9;
/// 1-hot node-identity bit for `receipt`.
pub const NODE_BIT_RECEIPT: u64 = 1 << 10;

/// One step in the fixed `judge -> admit -> receipt` lifecycle.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LifecycleStep {
    /// The object was judged (obligations evaluated).
    Judged,
    /// The object was admitted.
    Admitted,
    /// The object was receipted (chain hash computed).
    Receipted,
}

impl LifecycleStep {
    fn activity(self) -> &'static str {
        match self {
            LifecycleStep::Judged => "judge",
            LifecycleStep::Admitted => "admit",
            LifecycleStep::Receipted => "receipt",
        }
    }

    fn node_id(self) -> u32 {
        match self {
            LifecycleStep::Judged => 1,
            LifecycleStep::Admitted => 2,
            LifecycleStep::Receipted => 3,
        }
    }

    fn node_bit(self) -> u64 {
        match self {
            LifecycleStep::Judged => NODE_BIT_JUDGE,
            LifecycleStep::Admitted => NODE_BIT_ADMIT,
            LifecycleStep::Receipted => NODE_BIT_RECEIPT,
        }
    }

    fn required_tokens(self) -> u64 {
        match self {
            LifecycleStep::Judged => TOK_START,
            LifecycleStep::Admitted => TOK_JUDGED,
            LifecycleStep::Receipted => TOK_ADMITTED,
        }
    }

    fn produces_tokens(self) -> u64 {
        match self {
            LifecycleStep::Judged => TOK_JUDGED,
            LifecycleStep::Admitted => TOK_ADMITTED,
            LifecycleStep::Receipted => TOK_DONE,
        }
    }
}

/// Build the [`PowlReplayFrame`] for one lifecycle step.
#[must_use]
pub fn lifecycle_frame(
    step: LifecycleStep,
    ts_ns: u64,
    object_ids: Vec<String>,
) -> PowlReplayFrame {
    PowlReplayFrame {
        node_id: step.node_id(),
        node_bit: step.node_bit(),
        required_tokens: step.required_tokens(),
        produces_tokens: step.produces_tokens(),
        activity: step.activity().to_string(),
        ts_ns,
        object_ids,
    }
}

/// Replay an explicit sequence of lifecycle steps and return the resulting
/// conformance metrics, or the first violation encountered.
///
/// A lawful in-order sequence (`[Judged, Admitted, Receipted]`) yields
/// fitness `0x0001_0000` (1.0, Q16.16). An out-of-order sequence (e.g.
/// starting with `Admitted`) triggers [`ReplayViolation::TokenNotEnabled`] at
/// the offending step, since its `required_tokens` bit isn't yet present in
/// `enabled_tokens`.
pub fn replay_lifecycle(
    steps: &[(LifecycleStep, u64, Vec<String>)],
) -> Result<ConformanceMetrics, ReplayViolation> {
    let mut verifier = PowlReplayVerifier::new(TOK_START);
    for (step, ts_ns, object_ids) in steps {
        let frame = lifecycle_frame(*step, *ts_ns, object_ids.clone());
        verifier.replay_frame(&frame)?;
    }
    Ok(verifier.finalize())
}

/// Replay a single [`ReceiptRecord`] as an independent, lawful lifecycle
/// instance (`Judged -> Admitted -> Receipted`), with a fresh verifier.
///
/// Receipts only exist for objects that already passed judge+admit by
/// construction (see `LawObject::receipt`/`receipt_with_record`), so this
/// always replays the canonical 3-step lawful order for that record. It is
/// used by [`crate::receipt_validator::ReceiptValidator`]'s `token_replay`
/// stage as a conformance sanity check on the model itself (not as a way to
/// *detect* an out-of-order lifecycle — there is no persisted judge/admit
/// event to reorder against; the interesting negative case is exercised
/// directly via [`replay_lifecycle`] in tests).
pub fn replay_receipt_lifecycle(
    record: &ReceiptRecord,
) -> Result<ConformanceMetrics, ReplayViolation> {
    let steps = [
        (
            LifecycleStep::Judged,
            record.ts_ns,
            record.object_ids.clone(),
        ),
        (
            LifecycleStep::Admitted,
            record.ts_ns,
            record.object_ids.clone(),
        ),
        (
            LifecycleStep::Receipted,
            record.ts_ns,
            record.object_ids.clone(),
        ),
    ];
    replay_lifecycle(&steps)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lawful_in_order_sequence_has_fitness_one() {
        let steps = [
            (LifecycleStep::Judged, 1, vec![]),
            (LifecycleStep::Admitted, 2, vec![]),
            (LifecycleStep::Receipted, 3, vec![]),
        ];
        let metrics = replay_lifecycle(&steps).expect("lawful sequence must replay");
        assert_eq!(metrics.fitness, 0x0001_0000);
    }

    #[test]
    fn out_of_order_sequence_is_token_not_enabled() {
        let steps = [
            (LifecycleStep::Admitted, 1, vec![]), // requires TOK_JUDGED, not present
            (LifecycleStep::Judged, 2, vec![]),
            (LifecycleStep::Receipted, 3, vec![]),
        ];
        let result = replay_lifecycle(&steps);
        assert_eq!(result, Err(ReplayViolation::TokenNotEnabled { node_id: 2 }));
    }

    #[test]
    fn skipping_admit_before_receipt_is_token_not_enabled() {
        let steps = [
            (LifecycleStep::Judged, 1, vec![]),
            (LifecycleStep::Receipted, 2, vec![]), // requires TOK_ADMITTED, not present
        ];
        let result = replay_lifecycle(&steps);
        assert_eq!(result, Err(ReplayViolation::TokenNotEnabled { node_id: 3 }));
    }

    #[test]
    fn replay_receipt_lifecycle_is_lawful_by_construction() {
        let record = ReceiptRecord {
            version: crate::receipt_record::RECEIPT_RECORD_VERSION,
            instruction_id: 1,
            activity_idx: 0,
            activity: None,
            node_kind: 0,
            ts_ns: 100,
            duration_ms: None,
            payload_hash_hex: "11".repeat(32),
            prev_chain_hash_hex: "0".repeat(64),
            chain_hash_hex: "22".repeat(32),
            andon: crate::law::Andon::Green,
            obligation_count: 0,
            object_ids: vec!["law:abc".to_string()],
            signature_hex: None,
        };
        let metrics = replay_receipt_lifecycle(&record).expect("must replay");
        assert_eq!(metrics.fitness, 0x0001_0000);
    }
}
