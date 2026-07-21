//! Condition 19 (受領鎖を再生すると同じ憲法判定になる -- "replaying the receipt chain
//! reproduces the same judgment"), tested against THIS repo's own real,
//! production `.ggen-v2/receipt-log.jsonl` history -- not a synthetic
//! fixture. Loads the real v1->v2 boundary and every real v2 record this
//! repo has ever written, builds the real `MigrationReceipt` binding them,
//! and replays the chain twice, asserting the derived (andon, ceiling)
//! aggregate is identical both times.

use std::fs;
use std::path::Path;

use praxis_core::receipt_epoch::{read_receipt_epoch, AndonLevel, CeilingLevel, MigrationReceipt};
use praxis_core::receipt_record::ReceiptRecord;
use serde_json::Value;

const RECEIPT_LOG_PATH: &str = "../../.ggen-v2/receipt-log.jsonl";

fn load_real_records() -> Vec<ReceiptRecord> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(RECEIPT_LOG_PATH);
    let text = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("real receipt-log.jsonl must exist at {path:?}: {e}"));
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

fn replay(
    v1: &ReceiptRecord, migration: &MigrationReceipt, v2s: &[ReceiptRecord],
) -> (AndonLevel, CeilingLevel) {
    assert_eq!(
        migration.final_v1_chain_hash_hex, v1.chain_hash_hex,
        "migration must bind the real final v1 hash"
    );
    let first_v2 = v2s.first().expect("at least one real v2 record");
    assert_eq!(
        migration.first_v2_chain_hash_hex, first_v2.chain_hash_hex,
        "migration must bind the real first v2 hash"
    );
    assert_eq!(
        first_v2.prev_chain_hash_hex, v1.chain_hash_hex,
        "chain continuity must not break across this repo's real v1/v2 boundary"
    );

    let mut prev = first_v2.chain_hash_hex.clone();
    let mut last_epoch = read_receipt_epoch(first_v2).expect("first real v2 record readable");
    for record in &v2s[1..] {
        assert_eq!(
            record.prev_chain_hash_hex, prev,
            "chain continuity within this repo's real v2 history"
        );
        last_epoch = read_receipt_epoch(record).expect("real v2 record readable");
        prev = record.chain_hash_hex.clone();
    }
    (last_epoch.andon, last_epoch.standing_ceiling)
}

#[test]
fn replaying_this_repos_own_real_receipt_chain_twice_is_deterministic() {
    let records = load_real_records();
    let boundary = records
        .iter()
        .position(|r| r.schema == praxis_core::receipt_epoch::SCHEMA_V2)
        .expect("this repo's real log contains at least one v2 record");
    assert!(
        boundary > 0,
        "this repo's real log must contain at least one v1 record before the v2 boundary"
    );

    let v1 = records[boundary - 1].clone();
    let v2s: Vec<ReceiptRecord> = records[boundary..].to_vec();
    assert!(!v2s.is_empty());

    let migration = MigrationReceipt::new(v1.chain_hash_hex.clone(), v2s[0].chain_hash_hex.clone());

    let first = replay(&v1, &migration, &v2s);
    let second = replay(&v1, &migration, &v2s);
    assert_eq!(
        first, second,
        "replaying this repo's own real receipt chain twice must be deterministic"
    );

    eprintln!(
        "MIGRATION_RECEIPT_JSON={}",
        serde_json::to_string_pretty(&migration).unwrap()
    );
}
