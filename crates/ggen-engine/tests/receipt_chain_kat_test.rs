#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::match_wildcard_for_single_variants
)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Shared chain-law KAT court for `ggen_engine::receipt_chain_seam`
//! (Explore finding #1: the fold law ggen ships was unpinned in-tree).
//!
//! Folds the SAME four witnessed PN64/OCEL64 entries as
//! `strata-chain-pack/witness.ttl` (`sw:kat-entry-1`–`4`) and pins, as
//! fail-closed hex literals, BOTH folds over that stream:
//!
//! 1. the cross-repo shared head `05ea1aa0…` (insa/strata logchain 3-input
//!    step `h_i = BLAKE3(h_prev ‖ pn_be ‖ ocel_be ‖ tick_be64)`, provenance
//!    `insa-proof/tests/sync_as_replay.rs`, mirrored in
//!    `strata-chain-pack/witness.ttl sw:sharedKatHead` and
//!    `graphlaw/tests/insa_chain_differential.rs`);
//! 2. the graphlaw admission-frame fold over the same entries (99-byte
//!    `build_admission_frame` frame, double-mixed `chain_from_frame`),
//!    pinned byte-identical to `graphlaw`'s own differential court
//!    `GRAPHLAW_HEAD_HEX`.
//!
//! Expectations are literals only — nothing is recomputed from the code
//! under test to build an expectation. A sabotaged frame byte must move
//! the fold (anti-vacuity for the whole pin).
//!
//! Provenance note (coordinator brief vs. referent): the `54ee6f3cee…`
//! hex named in the task brief is the strata-tx-pack canonical-Tx frame
//! BLAKE3 pin (`packs/strata-tx-pack/templates/tx_model.rs.tera`,
//! b3sum cross-checked) — a DIFFERENT frame family from graphlaw's
//! admission frame. It is NOT assertable against this seam; the frame
//! digest pinned here is the admission-frame digest of `kat-entry-1`,
//! captured once and pinned below.

use ggen_engine::receipt_chain_seam as seam;
use ggen_engine::receipt_chain_seam::{
    build_admission_frame, chain_from_frame, DenialPolarity, ReceiptMeta,
};

use blake3::Hasher;

// ---------------------------------------------------------------------------
// Witnessed fixture (strata-chain-pack/witness.ttl sw:kat-entry-1..4)
// ---------------------------------------------------------------------------

const FIXTURE: [(u64, u64); 4] = [
    (0x0001_0000_0000_0001, 0x0A00_0000_0000_0001),
    (0x0002_0000_0000_0002, 0x0B00_0000_0000_0002),
    (0x0003_0000_0000_0003, 0x0C00_0000_0000_0003),
    (0x0004_0000_0000_0004, 0x0D00_0000_0000_0004),
];

/// Cross-repo shared KAT head over `FIXTURE` (fail-closed literal;
/// `strata-chain-pack/witness.ttl sw:sharedKatHead`).
const SHARED_KAT_HEAD_HEX: &str =
    "05ea1aa096f45968aacb2c996d2b541e4b9fdef24f006e0204e1fa88d33f68a5";

/// graphlaw admission-frame fold over `FIXTURE` (fail-closed literal;
/// byte-identical to `graphlaw/tests/insa_chain_differential.rs`
/// `GRAPHLAW_HEAD_HEX`).
const GRAPHLAW_HEAD_HEX: &str = "95163f2a6b24bebdfd19bf77cc60553640b8c9f5f5c54bc415c3b0da77e77598";

/// BLAKE3 digest of the 99-byte admission frame for `FIXTURE[0]` under the
/// deterministic entry mapping below (captured once, pinned; see module
/// doc re: the unrelated `54ee6f3cee…` strata-tx pin).
const KAT_ENTRY_1_FRAME_DIGEST_HEX: &str =
    "d361093fc658015606a23fd3dd96be3049c5cffb7ddaeaab29b5bb985bd172b9";

const GENESIS: [u8; 32] = [0u8; 32];

fn hex32(bytes: &[u8; 32]) -> String {
    bytes
        .iter()
        .fold(String::new(), |acc, b| format!("{acc}{b:02x}"))
}

// ---------------------------------------------------------------------------
// (1) insa/strata logchain 3-input step (ported byte-for-byte from
// insa-proof/src/sync.rs::step via graphlaw's differential court) — this is
// the recipe the shared KAT head 05ea1aa0… actually attests.
// ---------------------------------------------------------------------------

#[inline]
fn ocel_top_byte(w: u64) -> u8 {
    ((w >> 56) & 0xFF) as u8
}

#[inline]
fn insa_step(h: [u8; 32], pn: u64, ocel: u64) -> [u8; 32] {
    let mut hasher = Hasher::new();
    hasher.update(&h);
    hasher.update(&pn.to_be_bytes());
    hasher.update(&ocel.to_be_bytes());
    hasher.update(&u64::from(ocel_top_byte(ocel)).to_be_bytes());
    *hasher.finalize().as_bytes()
}

fn insa_fold(entries: &[(u64, u64)]) -> [u8; 32] {
    let mut h = GENESIS;
    for &(pn, ocel) in entries {
        h = insa_step(h, pn, ocel);
    }
    h
}

// ---------------------------------------------------------------------------
// (2) graphlaw admission-frame fold through the seam
// ---------------------------------------------------------------------------

fn payload_commitment(pn: u64, ocel: u64) -> [u8; 32] {
    let mut b = Vec::with_capacity(16);
    b.extend_from_slice(&pn.to_be_bytes());
    b.extend_from_slice(&ocel.to_be_bytes());
    *blake3::hash(&b).as_bytes()
}

fn frame_for(
    entry: (u64, u64), idx: usize, prev: [u8; 32],
) -> ggen_engine::receipt_chain_seam::OcelCausalFrame {
    let (pn, ocel) = entry;
    let meta = ReceiptMeta {
        instruction_id: pn,
        activity_idx: u16::try_from(idx).unwrap_or(u16::MAX),
        node_kind: 0,
        ts_ns: None,
        denial: DenialPolarity::ADMITTED,
        andon: seam::Andon::Green,
        object_ids: Vec::new(),
        obligation_count: 0,
    };
    build_admission_frame(&payload_commitment(pn, ocel), &prev, &meta, ocel)
}

fn graphlaw_fold(entries: &[(u64, u64)]) -> [u8; 32] {
    let mut chain = GENESIS;
    for (i, &entry) in entries.iter().enumerate() {
        let frame = frame_for(entry, i, chain);
        chain = chain_from_frame(&chain, &frame);
    }
    chain
}

// ---------------------------------------------------------------------------
// The court
// ---------------------------------------------------------------------------

#[test]
fn shared_kat_head_reproduces_through_insa_recipe_port() {
    assert_eq!(
        hex32(&insa_fold(&FIXTURE)),
        SHARED_KAT_HEAD_HEX,
        "in-test port of the insa/strata logchain step drifted from the \
         cross-repo shared KAT head"
    );
}

#[test]
fn seam_admission_frame_fold_matches_graphlaw_differential_court_pin() {
    let head = hex32(&graphlaw_fold(&FIXTURE));
    assert_eq!(
        head, GRAPHLAW_HEAD_HEX,
        "seam fold of the witnessed entries drifted from the graphlaw \
         differential-court pin"
    );
    // The two recipes are DIFFERENT laws; convergence would mean a wiring
    // bug (graphlaw's court asserts the same divergence property).
    assert_ne!(
        hex32(&insa_fold(&FIXTURE)),
        head,
        "different recipes unexpectedly converged — investigate before trusting"
    );
}

#[test]
fn kat_entry_1_frame_digest_is_pinned() {
    let frame = frame_for(FIXTURE[0], 0, GENESIS);
    let digest = blake3::hash(&frame.to_hash_bytes());
    assert_eq!(
        digest.to_hex().as_str(),
        KAT_ENTRY_1_FRAME_DIGEST_HEX,
        "admission-frame bytes for the first witnessed entry drifted from pin"
    );
}

#[test]
fn sabotaged_frame_byte_moves_the_fold() {
    // Anti-vacuity: flip one byte of entry 3's frame; the chain head must
    // move away from the pinned head. If a frame-byte mutation survives to
    // the same head, the pin proves nothing.
    let mut frames = Vec::new();
    let mut chain = GENESIS;
    for (i, &entry) in FIXTURE.iter().enumerate() {
        frames.push(frame_for(entry, i, chain));
        chain = chain_from_frame(&chain, &frames[i]);
    }
    let pinned = chain;

    chain = GENESIS;
    for (i, &entry) in FIXTURE.iter().enumerate() {
        let mut frame = frame_for(entry, i, chain);
        if i == 2 {
            frame.ts_ns = frame.ts_ns.wrapping_add(1); // one mutated frame byte
        }
        chain = chain_from_frame(&chain, &frame);
    }
    assert_ne!(
        chain, pinned,
        "a mutated frame byte did not move the fold — pin is vacuous"
    );
    assert_eq!(hex32(&pinned), GRAPHLAW_HEAD_HEX);
}

#[test]
fn truncation_monotone_through_the_seam() {
    let full = graphlaw_fold(&FIXTURE);
    let short = graphlaw_fold(&FIXTURE[..3]);
    assert_ne!(short, full, "dropping the last entry was not detected");
    assert_ne!(hex32(&short), GRAPHLAW_HEAD_HEX);
}
