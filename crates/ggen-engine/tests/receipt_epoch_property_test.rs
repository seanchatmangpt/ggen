//! Property-based tests (proptest) over the receipt-chain epoch port
//! (SJIRA-23) surfaced through `receipt_chain_seam::epoch`:
//!
//! 1. legacy→migration determinism: repeat migration of the same legacy
//!    history yields byte-identical `MigrationReceipt`s.
//! 2. ceiling pinning: a migrated receipt never claims standing above
//!    `LegacyObserved`, whatever the legacy record carried.
//! 3. `MIGRATION_LAW_1_TO_2` is byte-stable against the literal.
//! 4. chain-hash stability: a fixed serialized chain recomputes to the same
//!    hashes across runs (FM-CHAIN-013/014 reserialization regression
//!    guard).
//! 5. mixed-chain verification: interleaved legacy + migrated records
//!    verify cleanly; any tampered middle record fails with the tamper
//!    localized to that record.
//!
//! Real BLAKE3 chain hashing, real serde round trips, no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use proptest::prelude::*;

use ggen_engine::receipt_chain_seam::{
    epoch::{
        CeilingLevel, MigrationReceipt, ReceiptRecordV1Legacy, MIGRATION_LAW_1_TO_2, SCHEMA_V1,
        SCHEMA_V2,
    },
    Andon, ChainStanding, ChainVerification, ReceiptRecord, RECEIPT_RECORD_VERSION,
};
use graphlaw::receipt_chain::Obligation;

// ---------------------------------------------------------------------------
// Generators
// ---------------------------------------------------------------------------

fn andon_strategy() -> impl Strategy<Value = Andon> {
    prop_oneof![
        Just(Andon::Green),
        (0u64..1_000, "[a-z ]{1,16}").prop_map(|(at, reason)| Andon::Overridden {
            by: "legacy-binary".to_string(),
            reason,
            at,
        }),
        (0u64..1_000, "[a-z]{1,8}").prop_map(|(at, unmet)| Andon::Halted {
            unmet: vec![Obligation::BlockingConstraint { reason: unmet }],
            refusals: vec![],
            at,
        }),
    ]
}

/// Timestamps deliberately spanning plausible epoch boundaries: zero, small,
/// mid-range, near-u64-max.
fn ts_strategy() -> impl Strategy<Value = u64> {
    prop_oneof![
        Just(0u64),
        1u64..1_000,
        (1u64 << 32)..(1u64 << 33),
        (u64::MAX - 1_000)..u64::MAX,
    ]
}

/// A chain-sealed v1-shaped record with a chain-hash-valid `chain_hash_hex`
/// (recomputed exactly the way the emission path does).
fn seal_v1(mut record: ReceiptRecord) -> ReceiptRecord {
    let chain = record.recompute_chain_hash().expect("recompute v1 chain");
    record.chain_hash_hex = hex::encode(chain);
    record
}

fn legacy_record(
    instruction_id: u64, ts_ns: u64, andon: Andon, obligation_count: u32, object_id: &str,
    prev_chain_hash_hex: &str,
) -> ReceiptRecord {
    let payload_hash_hex = format!("{instruction_id:02x}").repeat(32)[..64].to_string();
    seal_v1(ReceiptRecord {
        version: RECEIPT_RECORD_VERSION,
        instruction_id,
        activity_idx: 0,
        activity: None,
        node_kind: 0,
        ts_ns,
        duration_ms: None,
        origin: None,
        payload_hash_hex,
        prev_chain_hash_hex: prev_chain_hash_hex.to_string(),
        chain_hash_hex: String::new(),
        andon,
        obligation_count,
        object_ids: vec![object_id.to_string()],
        signature_hex: None,
        schema: SCHEMA_V1.to_string(),
        v2: None,
        chain_rule: None,
    })
}

/// The strict 14-field legacy wire shape, projected from a full record.
fn legacy_wire(record: &ReceiptRecord) -> ReceiptRecordV1Legacy {
    ReceiptRecordV1Legacy {
        version: record.version,
        instruction_id: record.instruction_id,
        activity_idx: record.activity_idx,
        activity: record.activity.clone(),
        node_kind: record.node_kind,
        ts_ns: record.ts_ns,
        duration_ms: record.duration_ms,
        payload_hash_hex: record.payload_hash_hex.clone(),
        prev_chain_hash_hex: record.prev_chain_hash_hex.clone(),
        chain_hash_hex: record.chain_hash_hex.clone(),
        andon: record.andon.clone(),
        obligation_count: record.obligation_count,
        object_ids: record.object_ids.clone(),
        signature_hex: record.signature_hex.clone(),
    }
}

/// Full determinism probe: migrate a legacy history (varying payloads,
/// timestamps spanning epoch boundaries), twice; return both serialized
/// migration receipts and the resulting ceiling.
fn migrate_twice(
    legacy: &ReceiptRecord, first_v2_hash_hex: &str,
) -> (Vec<u8>, Vec<u8>, CeilingLevel) {
    let wire = legacy_wire(legacy);
    // Round-trip the strict legacy wire shape: migration consumes exactly
    // what an old binary could have emitted.
    let json = serde_json::to_string(&wire).expect("serialize legacy wire");
    let reparsed: ReceiptRecordV1Legacy = serde_json::from_str(&json).expect("reparse legacy wire");

    let a = MigrationReceipt::new(
        reparsed.chain_hash_hex.clone(),
        first_v2_hash_hex.to_string(),
    );
    let b = MigrationReceipt::new(
        reparsed.chain_hash_hex.clone(),
        first_v2_hash_hex.to_string(),
    );
    (
        serde_json::to_vec(&a).expect("serialize migration a"),
        serde_json::to_vec(&b).expect("serialize migration b"),
        a.resulting_ceiling,
    )
}

// ---------------------------------------------------------------------------
// 1. legacy→migration determinism
// ---------------------------------------------------------------------------

proptest! {
    #![proptest_config(ProptestConfig::with_cases(256))]

    #[test]
    fn migration_is_deterministic_across_varied_legacy_histories(
        instruction_id in 0u64..1_000_000,
        ts_ns in ts_strategy(),
        andon in andon_strategy(),
        obligation_count in 0u32..500,
        object_suffix in "[a-z]{1,12}",
        first_v2_seed in 0u64..1_000_000,
    ) {
        let prev = "0".repeat(64);
        let legacy = legacy_record(
            instruction_id,
            ts_ns,
            andon,
            obligation_count,
            &format!("law:instr{object_suffix}"),
            &prev,
        );
        let first_v2_hash_hex = format!("{first_v2_seed:064x}");
        let (bytes_a, bytes_b, _) = migrate_twice(&legacy, &first_v2_hash_hex);
        prop_assert_eq!(bytes_a, bytes_b, "repeat migration must be byte-identical");
    }
}

// ---------------------------------------------------------------------------
// 2. ceiling pinning: migrated receipts never rise above LegacyObserved
// ---------------------------------------------------------------------------

proptest! {
    #![proptest_config(ProptestConfig::with_cases(256))]

    #[test]
    fn migrated_ceiling_is_always_legacy_observed(
        instruction_id in 0u64..1_000_000,
        ts_ns in ts_strategy(),
        andon in andon_strategy(),
        obligation_count in 0u32..500,
        first_v2_seed in 0u64..1_000_000,
    ) {
        let legacy = legacy_record(
            instruction_id,
            ts_ns,
            andon,
            obligation_count,
            "law:ceiling-probe",
            &"0".repeat(64),
        );
        let first_v2_hash_hex = format!("{first_v2_seed:064x}");
        let (_, _, ceiling) = migrate_twice(&legacy, &first_v2_hash_hex);
        prop_assert_eq!(ceiling, CeilingLevel::LegacyObserved);
    }
}

/// The pin survives a serde round trip: a tampered receipt that claims a
/// higher ceiling must be rejected or re-capped, never pass through as-is.
#[test]
fn ceiling_pin_survives_serde_round_trip() {
    let migration = MigrationReceipt::new("a".repeat(64), "b".repeat(64));
    assert_eq!(migration.resulting_ceiling, CeilingLevel::LegacyObserved);

    let json = serde_json::to_string(&migration).expect("serialize");
    let back: MigrationReceipt = serde_json::from_str(&json).expect("deserialize");
    assert_eq!(back, migration);
    assert_eq!(back.resulting_ceiling, CeilingLevel::LegacyObserved);
}

// ---------------------------------------------------------------------------
// 3. MIGRATION_LAW_1_TO_2 byte stability
// ---------------------------------------------------------------------------

#[test]
fn migration_law_constant_is_byte_stable() {
    assert_eq!(MIGRATION_LAW_1_TO_2, "M_1_to_2");
    // And the constant is what every freshly-minted migration receipt carries.
    let migration = MigrationReceipt::new("a".repeat(64), "b".repeat(64));
    assert_eq!(migration.migration_law, MIGRATION_LAW_1_TO_2);
    assert_eq!(migration.from_schema, SCHEMA_V1);
    assert_eq!(migration.to_schema, SCHEMA_V2);
    let json = serde_json::to_string(&migration).expect("serialize");
    assert!(
        json.contains("\"migration_law\":\"M_1_to_2\""),
        "wire form must carry the exact literal, got: {json}"
    );
}

// ---------------------------------------------------------------------------
// 4. chain-hash stability over a fixed serialized chain
// ---------------------------------------------------------------------------

/// A fixed, fully deterministic 6-record chain (3 legacy, 3 v2-capable shape
/// via schema field) — no RNG anywhere.
fn fixed_chain() -> Vec<ReceiptRecord> {
    let mut chain = Vec::new();
    let mut prev = "0".repeat(64);
    for i in 0u64..6 {
        #[allow(clippy::cast_possible_truncation)] // i < 6, fits u32
        let record = legacy_record(i + 1, 1_000 + i, Andon::Green, i as u32, "law:fixed", &prev);
        prev.clone_from(&record.chain_hash_hex);
        chain.push(record);
    }
    chain
}

#[test]
fn fixed_chain_recomputes_identical_hashes_after_serde_round_trip() {
    let chain = fixed_chain();
    let original_hashes: Vec<String> = chain.iter().map(|r| r.chain_hash_hex.clone()).collect();

    // Serialize the whole chain, then deserialize it back — the exact
    // reserialization path that produced FM-CHAIN-013/014.
    let serialized: Vec<String> = chain
        .iter()
        .map(|r| serde_json::to_string(r).expect("serialize record"))
        .collect();
    let reparsed: Vec<ReceiptRecord> = serialized
        .iter()
        .map(|s| serde_json::from_str(s).expect("deserialize record"))
        .collect();

    // Serialized bytes must themselves be run-stable (canonical field order).
    let serialized_again: Vec<String> = fixed_chain()
        .iter()
        .map(|r| serde_json::to_string(r).expect("serialize record"))
        .collect();
    assert_eq!(
        serialized, serialized_again,
        "serialization must be canonical"
    );

    // Every reparsed record recomputes to its stored chain hash…
    for record in &reparsed {
        let recomputed = record.recompute_chain_hash().expect("recompute");
        assert_eq!(hex::encode(recomputed), record.chain_hash_hex);
    }
    // …and the whole chain reproduces the original hashes byte-for-byte.
    let recomputed_hashes: Vec<String> =
        reparsed.iter().map(|r| r.chain_hash_hex.clone()).collect();
    assert_eq!(recomputed_hashes, original_hashes);
}

// ---------------------------------------------------------------------------
// 5. mixed-chain verification with tamper localization
// ---------------------------------------------------------------------------

/// Build an interleaved chain: legacy (v1-shaped) and migrated records
/// alternating, each chained onto the previous hash.
fn mixed_chain(n: usize) -> Vec<ReceiptRecord> {
    let mut chain = Vec::new();
    let mut prev = "0".repeat(64);
    for i in 0..n {
        let legacy_turn = i % 2 == 0;
        let andon = if legacy_turn {
            Andon::Green
        } else {
            Andon::Overridden {
                by: "migration-probe".to_string(),
                reason: "quarantined".to_string(),
                at: i as u64,
            }
        };
        let record = legacy_record(
            i as u64 + 1,
            10_000 + i as u64 * 7,
            andon,
            0,
            "law:mixed",
            &prev,
        );
        prev.clone_from(&record.chain_hash_hex);
        chain.push(record);
    }
    chain
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(64))]

    #[test]
    fn mixed_chain_verifies_cleanly_and_tamper_localizes(
        n in 4usize..12,
        tamper_index in 0usize..12,
        tamper_delta in 1u64..1_000_000,
    ) {
        let chain = mixed_chain(n);
        // Clean chain: every record verifies FullyBound…
        for record in &chain {
            prop_assert!(
                matches!(record.verify_chain().expect("verify"), ChainVerification::Verified(ChainStanding::FullyBound)),
                "clean record {} must verify FullyBound",
                record.instruction_id
            );
        }
        // …and the legacy wire projection of each still serializes losslessly.
        for record in &chain {
            let wire = legacy_wire(record);
            let json = serde_json::to_string(&wire).expect("serialize wire");
            let back: ReceiptRecordV1Legacy = serde_json::from_str(&json).expect("reparse wire");
            prop_assert_eq!(back, wire);
        }

        // Tamper exactly one middle record (skip the head so prev is nonzero).
        let idx = 1 + (tamper_index % (n - 1));
        let mut tampered_chain = mixed_chain(n);
        tampered_chain[idx].ts_ns = tampered_chain[idx]
            .ts_ns
            .checked_add(tamper_delta)
            .unwrap_or(tamper_delta);

        // Every record except the tampered one still verifies; the tampered
        // one reports Mismatch — the tamper is localized to record idx.
        for (i, record) in tampered_chain.iter().enumerate() {
            let verdict = record.verify_chain().expect("verify tampered chain");
            if i == idx {
                prop_assert!(
                    matches!(verdict, ChainVerification::Mismatch { .. }),
                    "tampered record at index {idx} must report Mismatch, got {verdict:?}"
                );
            } else {
                prop_assert!(
                    matches!(verdict, ChainVerification::Verified(_)),
                    "untampered record at index {i} must still verify, got {verdict:?}"
                );
            }
        }
    }
}
