//! Condition 18 (受領証の移行が受領鎖に記録される -- "the migration is recorded in the
//! receipt chain") persistence test. `receipt_chain_own_history_replay.rs` (condition
//! 19) already reconstructs the real `MigrationReceipt` in memory from this repo's own
//! `.ggen-v2/receipt-log.jsonl` v1->v2 boundary and proves it deterministic across two
//! replays -- but never writes it to disk. This test reuses that exact same
//! already-proven construction (same real records, same `MigrationReceipt::new` call)
//! and additionally persists the result to `.ggen-v2/migration-receipt.json`, closing
//! the gap: no new receipt-computation logic, just a disk-write step.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::fs;
use std::path::Path;

use praxis_core::receipt_epoch::MigrationReceipt;
use praxis_core::receipt_record::ReceiptRecord;
use serde_json::Value;

const RECEIPT_LOG_PATH: &str = "../../.ggen-v2/receipt-log.jsonl";
const MIGRATION_RECEIPT_PATH: &str = "../../.ggen-v2/migration-receipt.json";

fn load_real_records() -> Vec<ReceiptRecord> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(RECEIPT_LOG_PATH);
    let text = fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "real receipt-log.jsonl must exist at {}: {e}",
            path.display()
        )
    });
    text.lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            let v: Value = serde_json::from_str(l).expect("each line is valid JSON");
            let record_json = v.get("record").cloned().unwrap_or(v);
            serde_json::from_value(record_json)
                .expect("each line's record deserializes as ReceiptRecord")
        })
        .collect()
}

/// Builds the real migration receipt from this repo's own history (identical
/// construction to condition 19's `receipt_chain_own_history_replay.rs`) and persists
/// it to `.ggen-v2/migration-receipt.json`. Fails loudly (not silently) if the v1/v2
/// boundary is missing or the file can't be written.
#[test]
fn migration_receipt_is_persisted_from_this_repos_own_real_receipt_chain() {
    let records = load_real_records();
    let boundary = records
        .iter()
        .position(|r| r.schema == praxis_core::receipt_epoch::SCHEMA_V2)
        .expect("this repo's real log contains at least one v2 record");
    assert!(
        boundary > 0,
        "this repo's real log must contain at least one v1 record before the v2 boundary"
    );

    let v1 = &records[boundary - 1];
    let v2s = &records[boundary..];
    assert!(!v2s.is_empty());

    let migration = MigrationReceipt::new(v1.chain_hash_hex.clone(), v2s[0].chain_hash_hex.clone());

    let json = serde_json::to_string_pretty(&migration).expect("MigrationReceipt serializes");
    let out_path = Path::new(env!("CARGO_MANIFEST_DIR")).join(MIGRATION_RECEIPT_PATH);
    fs::write(&out_path, &json).unwrap_or_else(|e| {
        panic!(
            "must be able to write migration receipt to {}: {e}",
            out_path.display()
        )
    });

    // Read back and verify: the persisted file matches the real, deterministic
    // construction -- not a stub, not a fabricated placeholder.
    let read_back = fs::read_to_string(&out_path).expect("just-written file is readable");
    let round_tripped: MigrationReceipt =
        serde_json::from_str(&read_back).expect("persisted JSON deserializes as MigrationReceipt");
    assert_eq!(
        round_tripped, migration,
        "persisted migration receipt must round-trip exactly"
    );
    assert_eq!(round_tripped.final_v1_chain_hash_hex, v1.chain_hash_hex);
    assert_eq!(round_tripped.first_v2_chain_hash_hex, v2s[0].chain_hash_hex);

    eprintln!("MIGRATION_RECEIPT_PERSISTED path={}", out_path.display());
}
