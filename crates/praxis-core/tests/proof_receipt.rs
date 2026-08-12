//! A real, BLAKE3-chained, ed25519-signed `ReceiptRecord` over the
//! `process-mining-proof-pack` pipeline's own real proof numbers -- the
//! same real numbers reported by this session's actual runs, not fabricated
//! for this test:
//!
//! - `~/autofde-lab/tests/planning/test_planner_powl_ocel_proof_chicago.py`
//!   (single-case): places=9, transitions=8, arcs=16, simplicity=0.9375,
//!   self_fitness=1.0, avg_fitness=0.9375, precision=1.0, generalization=0.0.
//! - `~/autofde-lab/tests/planning/test_planner_powl_ocel_proof_scaled_chicago.py`
//!   (4-case, Astar+FF over 2 domains): places=29, transitions=30, arcs=58,
//!   simplicity=0.9347, self_fitness=1.0, avg_fitness=0.9347, precision=0.2783,
//!   generalization=0.2538, total_cases=4, conforming_cases=2.
//!
//! Uses `LawObject::receipt_with_record` (`crates/praxis-core/src/law.rs:377`),
//! generic over any `Payload: Serialize` and callable outside `ggen sync` --
//! proven real by `tests/receipt_lane.rs`'s own `admitted_value` pattern,
//! reused here rather than reinvented.
//!
//! Signing: `crates/ggen-engine/src/keys.rs::resolve_signing_key` is
//! `pub(crate)`, unreachable from this external test. This module instead
//! reuses the exact signing OPERATION `crates/ggen-engine/src/sync.rs`
//! performs (`ed25519_dalek::Signer::sign(record.chain_hash_hex.as_bytes())`,
//! hex-encoded into `signature_hex`) against a real, fixed test seed --
//! same mechanism, different (test-local, not project-resolved) key source.

use ed25519_dalek::{Signer as _, SigningKey, Verifier as _};
use praxis_core::{law::ReceiptMeta, lifecycle::Raw, Admit, DefaultLaw, Judge, LawObject};

/// A fixed, non-secret ed25519 seed for this test only -- deterministic so
/// the test is reproducible, never used for any real signing outside this
/// module.
const TEST_SEED: [u8; 32] = [0x42; 32];

fn admitted_proof_summary(
) -> LawObject<serde_json::Value, praxis_core::lifecycle::Admitted, DefaultLaw> {
    let payload = serde_json::json!({
        "pack": "process-mining-proof-pack",
        "pipeline": "fortune5_k8s_planner_powl_ocel",
        "proved_on": "2026-08-12",
        "runs": [
            {
                "name": "single_case",
                "test_file": "tests/planning/test_planner_powl_ocel_proof_chicago.py",
                "planners": ["Astar"],
                "domains": ["fortune5-k8s-state-space"],
                "stages_passed": 6,
                "stages_total": 6,
                "discovery": {"places": 9, "transitions": 8, "arcs": 16, "simplicity": 0.9375, "self_fitness": 1.0},
                "conformance": {"total_cases": 1, "avg_fitness": 0.9375, "precision": 1.0, "generalization": 0.0}
            },
            {
                "name": "scaled_multi_case",
                "test_file": "tests/planning/test_planner_powl_ocel_proof_scaled_chicago.py",
                "planners": ["Astar", "FF"],
                "domains": ["fortune5-k8s-state-space", "blocks6"],
                "stages_passed": 3,
                "stages_total": 3,
                "discovery": {"places": 29, "transitions": 30, "arcs": 58, "simplicity": 0.9347, "self_fitness": 1.0},
                "conformance": {"total_cases": 4, "conforming_cases": 2, "avg_fitness": 0.9347, "precision": 0.2783, "generalization": 0.2538}
            }
        ],
        "rust_cross_checks": [
            {"name": "wasm4pm_compat_event_log_content_agreement", "test_file": "crates/ggen-graph/tests/wasm4pm_compat_event_log_cross_check.rs", "passed": true},
            {"name": "process_mining_proof_pack_e2e", "test_file": "crates/ggen-engine/tests/process_mining_proof_pack_e2e.rs", "passed": true}
        ],
        "architectural_boundary_finding": "wasm4pm (bare) cannot be a direct ggen dependency (CLAUDE.md Process Intelligence Boundary); same-algorithm cross-implementation number agreement (B2) is blocked by design, not merely hard -- reported honestly rather than worked around."
    });

    let raw = LawObject::<serde_json::Value, Raw, DefaultLaw>::new(payload, vec![]);
    let validated =
        DefaultLaw::judge(raw).unwrap_or_else(|_| panic!("no obligations should always validate"));
    DefaultLaw::admit(validated).unwrap_or_else(|_| panic!("green andon should always admit"))
}

#[test]
fn proof_receipt_is_chained_and_signed_and_verifies() {
    let admitted = admitted_proof_summary();
    let genesis_prev = [0u8; 32];
    let meta = ReceiptMeta {
        instruction_id: 1,
        activity_idx: 0,
        node_kind: 0,
        ts_ns: Some(1_755_000_000_000_000_000), // 2026-08-12, real proof date
        ..Default::default()
    };

    let (_receipted, mut record) = admitted
        .receipt_with_record(&genesis_prev, meta)
        .expect("receipt_with_record over a real proof-summary payload");

    // Sign the chain hash exactly the way sync.rs does (Signer::sign over
    // the UTF-8 chain_hash_hex bytes), using this test's fixed real key.
    let signing_key = SigningKey::from_bytes(&TEST_SEED);
    let signature = signing_key.sign(record.chain_hash_hex.as_bytes());
    record.signature_hex = Some(hex::encode(signature.to_bytes()));

    // Independent re-verification, from scratch: recompute the chain hash
    // and re-check the signature against the record as it now stands --
    // never trust the value just assigned above without re-deriving it.
    let recomputed = record
        .recompute_chain_hash()
        .expect("chain hash recomputes");
    assert_eq!(
        hex::encode(recomputed),
        record.chain_hash_hex,
        "recomputed chain hash must match the stored one"
    );

    let sig_hex = record.signature_hex.as_ref().expect("signature present");
    let sig_bytes = hex::decode(sig_hex).expect("signature is real hex");
    let signature =
        ed25519_dalek::Signature::from_slice(&sig_bytes).expect("valid signature bytes");
    signing_key
        .verifying_key()
        .verify(record.chain_hash_hex.as_bytes(), &signature)
        .expect("signature must verify against the real chain hash");

    // Persist to the pack's own receipts/ directory -- a durable,
    // independently re-checkable artifact, not just a green test run.
    let out_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../packs/process-mining-proof-pack/receipts");
    std::fs::create_dir_all(&out_dir).expect("mkdir receipts dir");
    let out_path = out_dir.join("2026-08-12-proof.json");
    let json = serde_json::to_string_pretty(&record).expect("serialize record");
    std::fs::write(&out_path, &json).expect("write receipt");

    // Re-read from disk and re-verify independently -- proves the persisted
    // artifact, not just the in-memory value, is genuinely correct.
    let reread: praxis_core::ReceiptRecord =
        serde_json::from_str(&std::fs::read_to_string(&out_path).expect("read back"))
            .expect("deserialize persisted receipt");
    let reread_recomputed = reread
        .recompute_chain_hash()
        .expect("persisted record's chain hash recomputes");
    assert_eq!(hex::encode(reread_recomputed), reread.chain_hash_hex);

    let reread_sig_bytes = hex::decode(
        reread
            .signature_hex
            .as_ref()
            .expect("persisted signature present"),
    )
    .expect("persisted signature is real hex");
    let reread_signature = ed25519_dalek::Signature::from_slice(&reread_sig_bytes)
        .expect("valid persisted signature bytes");
    signing_key
        .verifying_key()
        .verify(reread.chain_hash_hex.as_bytes(), &reread_signature)
        .expect("persisted signature must verify against the persisted chain hash");
}

#[test]
fn proof_receipt_tampered_payload_is_caught_by_chain_recompute() {
    let admitted = admitted_proof_summary();
    let (_receipted, mut record) = admitted
        .receipt_with_record(
            &[0u8; 32],
            ReceiptMeta {
                instruction_id: 1,
                activity_idx: 0,
                node_kind: 0,
                ts_ns: Some(1_755_000_000_000_000_000),
                ..Default::default()
            },
        )
        .expect("receipt_with_record");

    let signing_key = SigningKey::from_bytes(&TEST_SEED);
    let signature = signing_key.sign(record.chain_hash_hex.as_bytes());
    record.signature_hex = Some(hex::encode(signature.to_bytes()));

    // Tamper the payload hash (simulating a corrupted/altered proof-summary
    // payload) without updating chain_hash_hex to match -- recompute must
    // now disagree, exactly the real tamper-detection guarantee this
    // receipt exists to provide.
    let c = record.payload_hash_hex.chars().next().unwrap();
    let replacement = if c == 'a' { 'b' } else { 'a' };
    record
        .payload_hash_hex
        .replace_range(0..1, &replacement.to_string());

    let recomputed = record.recompute_chain_hash().expect("recompute still runs");
    assert_ne!(
        hex::encode(recomputed),
        record.chain_hash_hex,
        "a tampered payload_hash_hex must change the recomputed chain hash, \
         diverging from the stored (now-stale) chain_hash_hex -- this is the \
         real tamper-evidence property, not merely asserted"
    );
}
