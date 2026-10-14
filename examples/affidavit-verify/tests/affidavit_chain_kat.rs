//! Known-Answer Tests (KAT) for the affidavit chain law
//! `blake3(prev_hex_ascii || canonical_json(event))`, asserted against the
//! shared fixture pinned by the marketplace side:
//! `packs/affidavit-pack/kat/affidavit_chain_kat.json` (genesis seed
//! `affidavit-v0.1.0-genesis` + 4 event vectors with `canonical_bytes_hex`
//! and expected 64-hex heads).
//!
//! Fixture resolution: the live sibling copy is preferred at test time
//! (`../../../ggen-marketplace/packs/affidavit-pack/kat/affidavit_chain_kat.json`
//! relative to this crate's manifest); if it is absent, the byte-pin copy in
//! `tests/fixtures/affidavit_chain_kat.json` is used. When both are present
//! they must be byte-identical (drift meta-test), so the embedded pin can
//! never silently diverge from the marketplace authority.
//!
//! Byte-pin copy provenance:
//!   source: packs/affidavit-pack/kat/affidavit_chain_kat.json
//!   sha256: 7209dd2204fa8283bdb0bfa8be185236399ba1e12b54766ad482b34dd6405df1
//!
//! The KAT pins genesis seed `affidavit-v0.1.0-genesis`;
//! `GENESIS_SEED` embeds `CARGO_PKG_VERSION`, so this crate must stay at
//! version 0.1.0 for the KAT heads to hold (asserted explicitly below).

#[path = "../src/affidavit_chain.rs"]
mod affidavit_chain;
#[path = "../src/affidavit_types.rs"]
mod affidavit_types;

use affidavit_chain::{recompute_chain, GENESIS_SEED};
use affidavit_types::{canonical_bytes, Blake3Hash, ObjectRef, OperationEvent};
use serde::Deserialize;

const LIVE_SIBLING: &str =
    "../../../ggen-marketplace/packs/affidavit-pack/kat/affidavit_chain_kat.json";
const PINNED_FIXTURE: &str = "tests/fixtures/affidavit_chain_kat.json";

#[derive(Debug, Deserialize)]
struct KatVector {
    canonical_bytes_hex: String,
    chain_head: String,
    event: KatEvent,
}

#[derive(Debug, Deserialize)]
struct KatEvent {
    id: String,
    seq: u64,
    event_type: String,
    objects: Vec<KatObject>,
    payload_commitment: String,
}

#[derive(Debug, Deserialize)]
struct KatObject {
    id: String,
    obj_type: String,
    qualifier: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Kat {
    genesis_head: String,
    genesis_seed: String,
    vectors: Vec<KatVector>,
}

/// Load the KAT: live sibling preferred, byte-pin fallback.
/// Returns `(parsed, source_description)`.
fn load_kat() -> (Kat, &'static str) {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let live = format!("{manifest}/{LIVE_SIBLING}");
    let pinned = format!("{manifest}/{PINNED_FIXTURE}");
    match std::fs::read_to_string(&live) {
        Ok(text) => (serde_json::from_str(&text).expect("parse live sibling KAT"), "live sibling"),
        Err(_) => (
            serde_json::from_str(
                &std::fs::read_to_string(&pinned).expect("pinned KAT fixture must exist"),
            )
            .expect("parse pinned KAT fixture"),
            "pinned fixture",
        ),
    }
}

fn kat_event_to_operation_event(e: &KatEvent) -> OperationEvent {
    OperationEvent {
        id: e.id.clone(),
        seq: e.seq,
        event_type: e.event_type.clone(),
        objects: e
            .objects
            .iter()
            .map(|o| ObjectRef {
                id: o.id.clone(),
                obj_type: o.obj_type.clone(),
                qualifier: o.qualifier.clone(),
            })
            .collect(),
        payload_commitment: Blake3Hash::from_hex(e.payload_commitment.clone()),
    }
}

#[test]
fn genesis_seed_matches_kat_pin() {
    // KAT pins v0.1.0; GENESIS_SEED embeds CARGO_PKG_VERSION. If this crate
    // bumps its version the KAT heads below will break -- this assertion
    // names the real cause instead of leaving a mystery hash mismatch.
    assert_eq!(
        std::str::from_utf8(GENESIS_SEED).unwrap(),
        "affidavit-v0.1.0-genesis",
        "crate version drifted from the KAT's pinned genesis seed (v0.1.0)"
    );
}

#[test]
fn kat_canonical_bytes_match_generated_events() {
    let (kat, source) = load_kat();
    for (i, v) in kat.vectors.iter().enumerate() {
        let event = kat_event_to_operation_event(&v.event);
        let bytes = canonical_bytes(&event).expect("canonical_bytes");
        assert_eq!(
            hex_encode(&bytes),
            v.canonical_bytes_hex,
            "vector {i}: canonical bytes diverge (source: {source})"
        );
    }
}

#[test]
fn kat_chain_heads_match_law() {
    let (kat, source) = load_kat();

    // Genesis link: blake3(GENESIS_SEED) must equal the KAT's genesis_head.
    assert_eq!(
        Blake3Hash::from_bytes(GENESIS_SEED).as_hex(),
        kat.genesis_head,
        "genesis head diverges (source: {source})"
    );

    let events: Vec<OperationEvent> = kat
        .vectors
        .iter()
        .map(|v| kat_event_to_operation_event(&v.event))
        .collect();

    // Full-chain head == last vector's expected head.
    let final_head = recompute_chain(&events).expect("recompute_chain");
    assert_eq!(
        final_head.as_hex(),
        kat.vectors.last().expect("non-empty vectors").chain_head,
        "full-chain head diverges (source: {source})"
    );

    // Each prefix head == that vector's expected head (rolling property).
    for i in 1..=events.len() {
        let head = recompute_chain(&events[..i]).expect("recompute_chain");
        assert_eq!(
            head.as_hex(),
            kat.vectors[i - 1].chain_head,
            "prefix head {i} diverges (source: {source})"
        );
    }
}

#[test]
fn kat_drift_meta_test_live_sibling_matches_pin() {
    // If the live sibling is absent, this lane is BLOCKED (no drift signal)
    // and the pinned fixture alone carried the KAT -- skip silently.
    let manifest = env!("CARGO_MANIFEST_DIR");
    let live = format!("{manifest}/{LIVE_SIBLING}");
    let pinned = format!("{manifest}/{PINNED_FIXTURE}");
    let Ok(live_bytes) = std::fs::read(&live) else {
        return; // BLOCKED: live sibling absent; pin carried the KAT
    };
    let pinned_bytes = std::fs::read(&pinned).expect("pinned fixture must exist");
    assert_eq!(
        live_bytes, pinned_bytes,
        "live marketplace KAT drifted from the byte-pin copy in tests/fixtures/"
    );
}

#[test]
fn kat_sabotage_twin_flipped_byte_fails() {
    // Sabotage: flip one character of vector 0's payload_commitment; the
    // law must produce a different head than the KAT's expected one. If a
    // mutant could pass, the KAT would be vacuous.
    let (kat, _) = load_kat();
    let mut sabotaged = kat_event_to_operation_event(&kat.vectors[0].event);
    let mut pc = sabotaged.payload_commitment.as_hex().to_string();
    let first = pc.remove(0);
    pc.insert(0, if first == '0' { '1' } else { '0' });
    sabotaged.payload_commitment = Blake3Hash::from_hex(pc);

    let events: Vec<OperationEvent> = std::iter::once(sabotaged)
        .chain(
            kat.vectors
                .iter()
                .skip(1)
                .map(|v| kat_event_to_operation_event(&v.event)),
        )
        .collect();

    let head = recompute_chain(&events).expect("recompute_chain");
    assert_ne!(
        head.as_hex(),
        kat.vectors.last().unwrap().chain_head,
        "sabotaged vector must NOT reproduce the KAT head (KAT vacuity check)"
    );
}

fn hex_encode(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}
