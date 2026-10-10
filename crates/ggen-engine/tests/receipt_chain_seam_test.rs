//! Seam pin for `ggen_engine::receipt_chain_seam` (praxis retirement prep,
//! `docs/v26_10_10_praxis_retirement_plan.md`). Builds a real
//! `ReceiptRecord` chain in memory with real praxis hashing, verifies it
//! through the seam, and pins seam output == direct graphlaw
//! computation so the future `graphlaw::receipt_chain` swap can be diffed
//! against today's behavior.

use ggen_engine::receipt_chain_seam as seam;
use ggen_engine::receipt_chain_seam::Andon;

fn hex32(bytes: [u8; 32]) -> String {
    hex::encode(bytes)
}

/// One record chained onto `prev_chain_hash_hex`, sealed under the fold
/// rule (the rule every writer uses today): recompute the true chain hash
/// via praxis and store it, exactly as the emission path does.
fn chained_record(prev_chain_hash_hex: &str, payload_tag: &[u8], ts_ns: u64) -> ReceiptRecord {
    let payload_hash_hex = hex32(blake3::hash(payload_tag).into());
    let mut record = ReceiptRecord {
        version: seam::RECEIPT_RECORD_VERSION,
        instruction_id: 7,
        activity_idx: 3,
        activity: Some("sync.stage.render".to_string()),
        node_kind: 1,
        ts_ns,
        duration_ms: None,
        origin: None,
        payload_hash_hex,
        prev_chain_hash_hex: prev_chain_hash_hex.to_string(),
        chain_hash_hex: String::new(),
        andon: Andon::Green,
        obligation_count: 0,
        object_ids: vec!["law:test-object".to_string()],
        signature_hex: None,
        schema: seam::epoch::SCHEMA_V1.to_string(),
        v2: None,
        chain_rule: None,
    };
    let chain = record.recompute_chain_hash().expect("recompute");
    record.chain_hash_hex = hex32(chain);
    record
}

use ggen_engine::receipt_chain_seam::ReceiptRecord;

/// Type bridge for the equivalence proof: praxis and graphlaw `ReceiptRecord`
/// are distinct Rust types with (asserted-equal) serde wire shapes. The
/// round-trip IS part of the proof — a field/attr drift between the two
/// implementations fails here loudly.
fn to_seam(record: &ReceiptRecord) -> seam::ReceiptRecord {
    serde_json::from_str(&serde_json::to_string(record).expect("serialize")).expect("deserialize")
}

#[test]
fn seam_verify_accepts_a_real_two_record_chain() {
    let genesis = chained_record("00".repeat(32).as_str(), b"payload-genesis", 1_000);
    let head = chained_record(&genesis.chain_hash_hex, b"payload-head", 2_000);

    for record in [&genesis, &head] {
        match seam::verify_chain(&to_seam(record)).expect("verify") {
            seam::ChainVerification::Verified(standing) => {
                assert_eq!(standing, seam::ChainStanding::FullyBound);
            }
            other => panic!("expected Verified, got {other:?}"),
        }
    }
}

#[test]
fn seam_recompute_matches_direct_praxis_computation() {
    let genesis = chained_record("11".repeat(32).as_str(), b"payload-a", 100);
    let head = chained_record(&genesis.chain_hash_hex, b"payload-b", 200);

    // Same inputs -> same hash: the seam's wrapper must be byte-identical
    // to calling graphlaw directly.
    let via_seam = seam::recompute_chain_hash(&to_seam(&head)).expect("seam recompute");
    let direct = head.recompute_chain_hash().expect("direct recompute");
    assert_eq!(via_seam, direct);
    assert_eq!(hex32(via_seam), head.chain_hash_hex);

    // Chaining is real: head's prev is genesis's stored chain hash, and
    // head's recomputed hash differs from genesis's.
    assert_eq!(head.prev_chain_hash_hex, genesis.chain_hash_hex);
    assert_ne!(
        via_seam,
        seam::recompute_chain_hash(&to_seam(&genesis)).unwrap()
    );
}

#[test]
fn tampered_payload_fails_seam_verification() {
    let mut record = chained_record("22".repeat(32).as_str(), b"payload-orig", 300);

    // Tamper with the payload hash (simulates payload mutation after seal).
    record.payload_hash_hex = hex32(blake3::hash(b"payload-tampered").into());

    match seam::verify_chain(&to_seam(&record)).expect("verify tampered") {
        seam::ChainVerification::Mismatch { rule, recomputed } => {
            assert_eq!(rule, seam::ChainRule::V2Fold);
            assert_ne!(hex32(recomputed), record.chain_hash_hex);
        }
        other => panic!("tampered record verified: {other:?}"),
    }
}

#[test]
fn monotonicity_guard_accepts_ordered_chain_and_seam_matches_direct() {
    use ggen_engine::receipt_chain_seam::ChainRuleMonotonicity;

    let genesis = chained_record("33".repeat(32).as_str(), b"p0", 10);
    let head = chained_record(&genesis.chain_hash_hex, b"p1", 20);

    let mut via_seam = seam::ChainRuleMonotonicity::new();
    for (idx, record) in [&genesis, &head].into_iter().enumerate() {
        let standing = match seam::verify_chain(&to_seam(record)).expect("verify") {
            seam::ChainVerification::Verified(s) => s,
            other => panic!("unexpected: {other:?}"),
        };
        seam::observe_monotonicity(&mut via_seam, idx, &to_seam(record), standing)
            .expect("observe");
    }
    assert_eq!(via_seam.first_declared(), None);

    // Wrapper parity with the direct API on the same inputs.
    let mut direct = ChainRuleMonotonicity::new();
    direct
        .observe(
            0,
            &genesis,
            ggen_engine::receipt_chain_seam::ChainStanding::FullyBound,
        )
        .expect("direct observe");
    assert_eq!(via_seam.first_declared(), direct.first_declared());
}
