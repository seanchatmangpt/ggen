//! Integration tests for the receipt persistence + replay + OCEL export lane:
//! `receipt_record`, `receipt_store`, `receipt_validator`, `replay_adapter`,
//! `ocel_export`.

use praxis_core::{
    law::ReceiptMeta,
    lifecycle::Raw,
    receipt_validator::{FixedClock, ReceiptValidator, SystemClock},
    replay_adapter::{self, LifecycleStep},
    Admit, DefaultLaw, Judge, LawObject, ReceiptStore,
};

/// Fixed 64-hex-char (32-byte) ed25519 seed used only by these tests. Not
/// security-sensitive: it exists so `receipt_with_record()`'s `#[cfg(feature
/// = "signed")]` path has a deterministic `PRAXIS_SIGNING_KEY` to sign
/// against when this crate is built `--features signed`.
#[cfg(feature = "signed")]
const TEST_SIGNING_KEY_HEX: &str =
    "c1c2c3c4c5c6c7c8c9caccbdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedf1234";

/// Set `PRAXIS_SIGNING_KEY` for the duration of the returned guard when the
/// `signed` feature is enabled (`receipt()`/`receipt_with_record()` sign the
/// chain hash and fail closed without a key). This is a self-contained
/// mutex/env guard (integration tests are a separate crate from
/// `praxis_core`, so they cannot reach its `#[cfg(test)] pub(crate)`
/// `signing::test_support` helper) — same pattern `src/ops.rs`'s own tests use.
///
/// Returns `Option<MutexGuard>` rather than a bare `()` in the `not(signed)`
/// case so callers can still write `let _guard = signing_guard();` without
/// tripping clippy's `let_unit_value` lint (a unit-typed `let` binding would).
#[cfg(feature = "signed")]
fn signing_guard() -> Option<std::sync::MutexGuard<'static, ()>> {
    use std::sync::{Mutex, MutexGuard, OnceLock};
    fn env_lock() -> MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(()))
            .lock()
            .unwrap_or_else(|e| e.into_inner())
    }
    let guard = env_lock();
    std::env::set_var("PRAXIS_SIGNING_KEY", TEST_SIGNING_KEY_HEX);
    Some(guard)
}

#[cfg(not(feature = "signed"))]
fn signing_guard() -> Option<()> {
    None
}

fn admitted_value(
    value: serde_json::Value,
) -> LawObject<serde_json::Value, praxis_core::lifecycle::Admitted, DefaultLaw> {
    // `LawObject` intentionally does not derive `Debug` (its phantom
    // stage/law markers are not all `Debug`), so unwrap via `match` instead
    // of `.expect()` (mirrors `praxis-core`'s own `default_law.rs` tests).
    let raw = LawObject::<serde_json::Value, Raw, DefaultLaw>::new(value, vec![]);
    let validated = match DefaultLaw::judge(raw) {
        Ok(v) => v,
        Err(_) => panic!("no obligations should always validate"),
    };
    match DefaultLaw::admit(validated) {
        Ok(a) => a,
        Err(_) => panic!("green andon should always admit"),
    }
}

/// Emit `n` chained receipts (via `receipt_with_record`), deterministic in
/// `ts_ns` and `instruction_id`, returning them in chain order.
fn chained_records(n: u64) -> Vec<praxis_core::ReceiptRecord> {
    let mut records = Vec::new();
    let mut prev = [0u8; 32];
    for i in 1..=n {
        let admitted = admitted_value(serde_json::json!({"i": i}));
        let meta = ReceiptMeta {
            instruction_id: i,
            activity_idx: 0,
            node_kind: 0,
            ts_ns: Some(i * 1_000_000),
            ..Default::default()
        };
        let (receipted, record) = admitted
            .receipt_with_record(&prev, meta)
            .expect("receipt_with_record");
        prev = *receipted.chain_hash().expect("chain hash set");
        records.push(record);
    }
    records
}

#[test]
fn three_chained_receipts_validate_clean() {
    let _guard = signing_guard();
    let records = chained_records(3);
    let verdict = ReceiptValidator::validate(&records, &SystemClock);
    assert!(verdict.ok, "verdict: {verdict:?}");
    assert_eq!(verdict.records_checked, 3);
    for stage in &verdict.stages {
        assert!(
            stage.outcome.is_pass()
                || matches!(
                    stage.outcome,
                    praxis_core::receipt_validator::CheckOutcome::Skip(_)
                ),
            "stage {} failed: {:?}",
            stage.stage,
            stage.outcome
        );
    }
}

#[test]
fn tampering_payload_hash_is_caught_by_chain_recompute() {
    let _guard = signing_guard();
    let mut records = chained_records(2);
    let c = records[0].payload_hash_hex.chars().next().unwrap();
    let replacement = if c == 'a' { 'b' } else { 'a' };
    records[0]
        .payload_hash_hex
        .replace_range(0..1, &replacement.to_string());

    let verdict = ReceiptValidator::validate(&records, &SystemClock);
    assert!(!verdict.ok);
    let stage = verdict
        .stages
        .iter()
        .find(|s| s.stage == "chain_recompute")
        .unwrap();
    assert!(matches!(
        stage.outcome,
        praxis_core::receipt_validator::CheckOutcome::Fail(_)
    ));
}

#[test]
fn broken_linkage_is_caught() {
    let _guard = signing_guard();
    let mut records = chained_records(3);
    records.swap(0, 1);
    let verdict = ReceiptValidator::validate(&records, &SystemClock);
    assert!(!verdict.ok);
    let stage = verdict
        .stages
        .iter()
        .find(|s| s.stage == "chain_linkage")
        .unwrap();
    assert!(matches!(
        stage.outcome,
        praxis_core::receipt_validator::CheckOutcome::Fail(_)
    ));
}

#[test]
fn non_monotonic_instruction_id_is_caught() {
    let _guard = signing_guard();
    let mut records = chained_records(3);
    records[2].instruction_id = records[0].instruction_id;
    let verdict = ReceiptValidator::validate(&records, &SystemClock);
    assert!(!verdict.ok);
    let stage = verdict
        .stages
        .iter()
        .find(|s| s.stage == "monotonic")
        .unwrap();
    assert!(matches!(
        stage.outcome,
        praxis_core::receipt_validator::CheckOutcome::Fail(_)
    ));
}

#[test]
fn fixed_clock_future_timestamp_is_caught() {
    let _guard = signing_guard();
    let records = chained_records(1);
    // FixedClock(0) => "now" is the epoch; any positive ts_ns is "the future".
    let verdict = ReceiptValidator::validate(&records, &FixedClock(0));
    assert!(!verdict.ok);
    let stage = verdict
        .stages
        .iter()
        .find(|s| s.stage == "monotonic")
        .unwrap();
    assert!(matches!(
        stage.outcome,
        praxis_core::receipt_validator::CheckOutcome::Fail(_)
    ));
}

#[test]
fn fixed_clock_present_timestamp_passes_monotonic() {
    let _guard = signing_guard();
    let records = chained_records(1);
    let verdict = ReceiptValidator::validate(&records, &FixedClock(u64::MAX));
    let stage = verdict
        .stages
        .iter()
        .find(|s| s.stage == "monotonic")
        .unwrap();
    assert!(stage.outcome.is_pass());
}

#[test]
fn lawful_lifecycle_replay_has_fitness_one() {
    let steps = [
        (LifecycleStep::Judged, 1, vec![]),
        (LifecycleStep::Admitted, 2, vec![]),
        (LifecycleStep::Receipted, 3, vec![]),
    ];
    let metrics = replay_adapter::replay_lifecycle(&steps).expect("lawful sequence replays");
    assert_eq!(metrics.fitness, 0x0001_0000);
}

#[test]
fn out_of_order_lifecycle_replay_is_a_violation() {
    let steps = [
        (LifecycleStep::Admitted, 1, vec![]),
        (LifecycleStep::Judged, 2, vec![]),
        (LifecycleStep::Receipted, 3, vec![]),
    ];
    let result = replay_adapter::replay_lifecycle(&steps);
    assert!(result.is_err(), "out-of-order sequence must be rejected");
}

#[test]
fn store_append_load_all_and_last_chain_hash_round_trip() {
    let _guard = signing_guard();
    let dir = tempfile::tempdir().expect("tempdir");
    let store = ReceiptStore::open(dir.path()).expect("open store");

    assert_eq!(
        store.last_chain_hash().expect("genesis"),
        praxis_core::receipt_store::GENESIS_CHAIN_HASH
    );

    let records = chained_records(2);
    for record in &records {
        store.append(record).expect("append");
    }

    let loaded = store.load_all().expect("load_all");
    assert_eq!(loaded, records);
    assert_eq!(
        store.last_chain_hash().expect("last"),
        records.last().unwrap().chain_hash().unwrap()
    );
}

#[test]
fn ocel_export_round_trips_with_valid_rfc3339_timestamps() {
    let _guard = signing_guard();
    let records = chained_records(2);
    let ocel = praxis_core::ocel_export::to_ocel(&records);

    assert_eq!(ocel.events.len(), 2);
    for event in &ocel.events {
        // `time` is a `DateTime<FixedOffset>`; round-trip through RFC3339 text
        // to confirm it's not some degenerate/epoch-zero placeholder.
        let text = event.time.to_rfc3339();
        let parsed: chrono::DateTime<chrono::FixedOffset> =
            text.parse().expect("must be valid RFC3339");
        assert_eq!(parsed, event.time);
    }

    let json = serde_json::to_string(&ocel).expect("serialize OCEL");
    let round_tripped: wasm4pm_compat::ocel::OCEL =
        serde_json::from_str(&json).expect("deserialize OCEL");
    assert_eq!(round_tripped.events.len(), ocel.events.len());
}

#[test]
fn tampering_andon_is_caught_by_validator() {
    let _guard = signing_guard();
    let mut records = chained_records(2);
    // Change Andon::Green to Andon::Halted on the second record.
    assert_eq!(records[1].andon, praxis_core::law::Andon::Green);
    records[1].andon = praxis_core::law::Andon::Halted {
        unmet: vec![],
        refusals: vec![],
        at: 0,
    };

    // The validator now says ok: false because chain_recompute fails!
    let verdict = ReceiptValidator::validate(&records, &SystemClock);
    assert!(!verdict.ok);
    let stage = verdict
        .stages
        .iter()
        .find(|s| s.stage == "chain_recompute")
        .unwrap();
    assert!(matches!(
        stage.outcome,
        praxis_core::receipt_validator::CheckOutcome::Fail(_)
    ));
}

#[test]
fn tampering_genesis_prev_chain_hash_is_caught_by_linkage() {
    let _guard = signing_guard();
    let mut records = chained_records(2);
    // Change first record's prev_chain_hash_hex to a dummy value.
    records[0].prev_chain_hash_hex = "33".repeat(32);
    // Recompute and update first record's chain_hash_hex based on the new prev_chain_hash.
    let recomputed = records[0].recompute_chain_hash().expect("recompute");
    records[0].chain_hash_hex = hex::encode(recomputed);

    // Also update second record's prev_chain_hash_hex to link to the new chain_hash_hex.
    records[1].prev_chain_hash_hex = records[0].chain_hash_hex.clone();
    let recomputed_2 = records[1].recompute_chain_hash().expect("recompute 2");
    records[1].chain_hash_hex = hex::encode(recomputed_2);

    // The validator fails because the genesis anchor has been changed to "3333..."!
    let verdict = ReceiptValidator::validate(&records, &SystemClock);
    assert!(!verdict.ok);
    let stage = verdict
        .stages
        .iter()
        .find(|s| s.stage == "chain_linkage")
        .unwrap();
    assert!(matches!(
        stage.outcome,
        praxis_core::receipt_validator::CheckOutcome::Fail(_)
    ));
}
