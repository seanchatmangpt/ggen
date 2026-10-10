//! Single seam for every receipt-chain symbol `ggen-engine` consumes.
//! Part of the praxis retirement plan
//! (`docs/v26_10_10_praxis_retirement_plan.md`): the swap to
//! `graphlaw::receipt_chain` is confined to THIS file — the callsite
//! migration is complete (0 `praxis_core` code references in ggen src);
//! every symbol consumed by `sync.rs`, `verbs/handlers.rs`, and the test
//! files is graphlaw-backed through `crate::receipt_chain_seam` (or
//! `ggen_engine::receipt_chain_seam`).
//!
//! # Symbol → source table (post-flip)
//!
//! | Seam symbol | source now | (previously: praxis_core) |
//! |---|---|---|
//! | `ReceiptRecord` | `graphlaw::receipt_chain::ReceiptRecord` (all 17 fields, `RECEIPT_RECORD_VERSION = 1`) | `receipt_record::ReceiptRecord` |
//! | `RECEIPT_RECORD_VERSION` | `graphlaw::receipt_chain::RECEIPT_RECORD_VERSION` | `receipt_record::RECEIPT_RECORD_VERSION` |
//! | `ChainRule` | `graphlaw::receipt_chain::ChainRule` | `receipt_record::ChainRule` |
//! | `CHAIN_RULE_V2_FOLD` | `graphlaw::receipt_chain::CHAIN_RULE_V2_FOLD` | `receipt_record::CHAIN_RULE_V2_FOLD` |
//! | `ChainStanding` | `graphlaw::receipt_chain::ChainStanding` | `receipt_record::ChainStanding` |
//! | `ChainVerification` | `graphlaw::receipt_chain::ChainVerification` | `receipt_record::ChainVerification` |
//! | `ChainRuleMonotonicity` | `graphlaw::receipt_chain::ChainRuleMonotonicity` | `receipt_record::ChainRuleMonotonicity` |
//! | `verify_chain` | `graphlaw::receipt_chain::ReceiptRecord::verify_chain` | `ReceiptRecord::verify_chain` |
//! | `recompute_chain_hash` | `graphlaw::receipt_chain::ReceiptRecord::recompute_chain_hash` | `ReceiptRecord::recompute_chain_hash` |
//! | `observe_monotonicity` | `graphlaw::receipt_chain::ChainRuleMonotonicity::observe` | `ChainRuleMonotonicity::observe` |
//! | `Andon` | `graphlaw::receipt_chain::Andon` | `law::Andon` (re-exported at crate root) |
//! | `receipt_epoch::*` items | `graphlaw::receipt_chain::epoch` | `receipt_epoch` module |
//!
//! Thin wrappers (`verify_chain`, `recompute_chain_hash`,
//! `observe_monotonicity`) exist so signature drift at the graphlaw
//! boundary is absorbed here; pure data/type symbols are bare re-exports
//! because a wrapper would add nothing. The wrapper error type is
//! `graphlaw::receipt_chain::CoreError` (variant names + `name()` strings
//! match praxis-core's `CoreError` exactly; documented drift absorbed at
//! the seam).

#![allow(dead_code)] // seam is ahead of call-site migration (documented follow-up)

pub use graphlaw::receipt_chain::{Andon, CoreError};
pub use graphlaw::receipt_chain::{
    ChainRule, ChainRuleMonotonicity, ChainStanding, ChainVerification, ReceiptRecord,
    CHAIN_RULE_V2_FOLD, RECEIPT_RECORD_VERSION,
};

/// Re-export of the epoch surface `sync.rs` consumes today, now
/// graphlaw-backed.
pub mod epoch {
    pub use graphlaw::receipt_chain::epoch::{
        read_receipt_epoch, AdmissionDecision, AdmissionItem, AdmissionLedger, AndonLevel,
        CeilingLevel, ComponentLevels, EquivalenceMap, EquivalenceStatus, MigrationReceipt,
        ObligationCount, ObservedOutcome, ReceiptEpochV2, ReceiptEpochV2Builder,
        ReceiptRecordV1Legacy, MIGRATION_LAW_1_TO_2, SCHEMA_V1, SCHEMA_V2,
    };
}

/// Rule-aware verification of a record's stored chain hash.
/// Wraps [`ReceiptRecord::verify_chain`].
pub fn verify_chain(record: &ReceiptRecord) -> Result<ChainVerification, CoreError> {
    record.verify_chain()
}

/// Strict emission-side chain-hash recompute.
/// Wraps [`ReceiptRecord::recompute_chain_hash`].
pub fn recompute_chain_hash(record: &ReceiptRecord) -> Result<[u8; 32], CoreError> {
    record.recompute_chain_hash()
}

/// Downgrade-guard observation in ledger order.
/// Wraps [`ChainRuleMonotonicity::observe`].
pub fn observe_monotonicity(
    tracker: &mut ChainRuleMonotonicity, idx: usize, record: &ReceiptRecord,
    standing: ChainStanding,
) -> Result<(), CoreError> {
    tracker.observe(idx, record, standing)
}
