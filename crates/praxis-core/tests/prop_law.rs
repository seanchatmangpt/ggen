//! Property-based tests for chain-hash sensitivity and quarantine safety.
//!
//! These generalize the hand-picked perturbation unit tests already in
//! `src/law.rs` (single payload/prev/instruction_id examples) into
//! properties over arbitrary inputs: any single perturbation of the
//! payload, the `prev_chain_hash`, or any individual `ReceiptMeta` field
//! must change the resulting chain hash; identical inputs must always
//! reproduce the identical chain hash; and the Rice Quarantine boundary
//! must never panic, no matter how adversarial the input string is.

use praxis_core::{
    law::ReceiptMeta,
    lifecycle::{Admitted, Raw},
    Admit, DefaultLaw, JsonBoundarySchema, Judge, LawObject, RiceQuarantine,
};
use proptest::prelude::*;

/// The `signed` feature was removed 2026-07-17 alongside this workspace's
/// absolute-path-dependency cleanup (PR #255) — see `tests/receipt_lane.rs`'s
/// `signing_guard` comment. This is now a permanent no-op, kept so call
/// sites below don't need touching.
fn ensure_signing_key() {}

/// Build a fresh `Admitted` law object wrapping `payload`, with no
/// obligations — judge/admit always succeed unconditionally, so every
/// property below is purely about chain-hash sensitivity, not obligation
/// handling.
fn admitted(payload: serde_json::Value) -> LawObject<serde_json::Value, Admitted, DefaultLaw> {
    let raw = LawObject::<serde_json::Value, Raw, DefaultLaw>::new(payload, vec![]);
    let validated = match DefaultLaw::judge(raw) {
        Ok(v) => v,
        Err(_) => panic!("no obligations must always judge cleanly"),
    };
    match DefaultLaw::admit(validated) {
        Ok(a) => a,
        Err(_) => panic!("green andon must always admit"),
    }
}

/// A `ReceiptMeta` with every field fixed except `ts_ns`.
fn fixed_meta(ts_ns: u64) -> ReceiptMeta {
    ReceiptMeta {
        instruction_id: 1,
        activity_idx: 0,
        node_kind: 0,
        ts_ns: Some(ts_ns),
        ..Default::default()
    }
}

proptest! {
    /// Two distinct JSON payloads, receipted with the same `prev_chain_hash`
    /// and `meta`, must produce different chain hashes.
    #[test]
    fn chain_hash_changes_on_payload_perturbation(a in any::<i64>(), b in any::<i64>()) {
        ensure_signing_key();
        prop_assume!(a != b);
        let prev = [0u8; 32];
        let meta = fixed_meta(42);
        let ra = admitted(serde_json::json!({"v": a})).receipt(&prev, meta.clone()).expect("receipt a");
        let rb = admitted(serde_json::json!({"v": b})).receipt(&prev, meta).expect("receipt b");
        prop_assert_ne!(ra.chain_hash(), rb.chain_hash());
    }

    /// Two distinct `prev_chain_hash` values, same payload and meta, must
    /// produce different chain hashes.
    #[test]
    fn chain_hash_changes_on_prev_chain_hash_perturbation(
        a in any::<[u8; 32]>(),
        b in any::<[u8; 32]>(),
    ) {
        ensure_signing_key();
        prop_assume!(a != b);
        let meta = fixed_meta(42);
        let payload = serde_json::json!({"v": 1});
        let ra = admitted(payload.clone()).receipt(&a, meta.clone()).expect("receipt a");
        let rb = admitted(payload).receipt(&b, meta).expect("receipt b");
        prop_assert_ne!(ra.chain_hash(), rb.chain_hash());
    }

    /// Perturbing only `instruction_id` changes the chain hash.
    #[test]
    fn chain_hash_changes_on_instruction_id_perturbation(a in any::<u64>(), b in any::<u64>()) {
        ensure_signing_key();
        prop_assume!(a != b);
        let prev = [0u8; 32];
        let payload = serde_json::json!({"v": 1});
        let meta_a = ReceiptMeta { instruction_id: a, ts_ns: Some(42), ..Default::default() };
        let meta_b = ReceiptMeta { instruction_id: b, ..meta_a.clone() };
        let ra = admitted(payload.clone()).receipt(&prev, meta_a).expect("receipt a");
        let rb = admitted(payload).receipt(&prev, meta_b).expect("receipt b");
        prop_assert_ne!(ra.chain_hash(), rb.chain_hash());
    }

    /// Perturbing only `activity_idx` changes the chain hash.
    #[test]
    fn chain_hash_changes_on_activity_idx_perturbation(a in any::<u16>(), b in any::<u16>()) {
        ensure_signing_key();
        prop_assume!(a != b);
        let prev = [0u8; 32];
        let payload = serde_json::json!({"v": 1});
        let meta_a = ReceiptMeta { instruction_id: 1, activity_idx: a, ts_ns: Some(42), ..Default::default() };
        let meta_b = ReceiptMeta { activity_idx: b, ..meta_a.clone() };
        let ra = admitted(payload.clone()).receipt(&prev, meta_a).expect("receipt a");
        let rb = admitted(payload).receipt(&prev, meta_b).expect("receipt b");
        prop_assert_ne!(ra.chain_hash(), rb.chain_hash());
    }

    /// Perturbing only `node_kind` changes the chain hash.
    #[test]
    fn chain_hash_changes_on_node_kind_perturbation(a in any::<u8>(), b in any::<u8>()) {
        ensure_signing_key();
        prop_assume!(a != b);
        let prev = [0u8; 32];
        let payload = serde_json::json!({"v": 1});
        let meta_a = ReceiptMeta { instruction_id: 1, node_kind: a, ts_ns: Some(42), ..Default::default() };
        let meta_b = ReceiptMeta { node_kind: b, ..meta_a.clone() };
        let ra = admitted(payload.clone()).receipt(&prev, meta_a).expect("receipt a");
        let rb = admitted(payload).receipt(&prev, meta_b).expect("receipt b");
        prop_assert_ne!(ra.chain_hash(), rb.chain_hash());
    }

    /// Perturbing only `ts_ns` changes the chain hash.
    #[test]
    fn chain_hash_changes_on_ts_ns_perturbation(a in any::<u64>(), b in any::<u64>()) {
        ensure_signing_key();
        prop_assume!(a != b);
        let prev = [0u8; 32];
        let payload = serde_json::json!({"v": 1});
        let meta_a = fixed_meta(a);
        let meta_b = fixed_meta(b);
        let ra = admitted(payload.clone()).receipt(&prev, meta_a).expect("receipt a");
        let rb = admitted(payload).receipt(&prev, meta_b).expect("receipt b");
        prop_assert_ne!(ra.chain_hash(), rb.chain_hash());
    }

    /// Identical payload/prev/meta receipted twice must produce the exact
    /// same chain hash (no hidden nondeterminism — e.g. from clocks, when
    /// `ts_ns` is explicitly supplied).
    #[test]
    fn receipt_is_deterministic_for_identical_inputs(v in any::<i64>(), ts in any::<u64>()) {
        ensure_signing_key();
        let prev = [7u8; 32];
        let payload = serde_json::json!({"v": v});
        let meta = fixed_meta(ts);
        let r1 = admitted(payload.clone()).receipt(&prev, meta.clone()).expect("receipt 1");
        let r2 = admitted(payload).receipt(&prev, meta).expect("receipt 2");
        prop_assert_eq!(r1.chain_hash(), r2.chain_hash());
    }

    /// `RiceQuarantine::admit` never panics on arbitrary string input, no
    /// matter how malformed or adversarial — it must always resolve to
    /// `Ok`/`Err`, never unwind.
    #[test]
    fn quarantine_admit_never_panics_on_arbitrary_strings(s in ".*") {
        let schema = JsonBoundarySchema::<serde_json::Value>::new();
        let quarantine = RiceQuarantine::new(schema);
        let _ = quarantine.admit(&s);
    }

    /// Same property, but biased toward almost-valid JSON (braces/brackets)
    /// so proptest spends time near the boundary rather than pure noise.
    #[test]
    fn quarantine_admit_never_panics_on_json_like_strings(
        s in "[{}\\[\\]\"a-zA-Z0-9:,. -]*"
    ) {
        let schema = JsonBoundarySchema::<serde_json::Value>::new();
        let quarantine = RiceQuarantine::new(schema);
        let _ = quarantine.admit(&s);
    }
}
