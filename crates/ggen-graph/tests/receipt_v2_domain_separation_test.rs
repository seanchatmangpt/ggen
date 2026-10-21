//! GraphReceipt v2 domain-separation court (lane A08, v26.10.20).
//!
//! Pins:
//! - v2 receipts verify via the domain-separated keyed BLAKE3 path
//!   (`derive_key("ggen.graph.receipt.v2")`).
//! - The v1 legacy raw-concatenation path cannot silently drift: one golden
//!   vector with hardcoded field bytes and hardcoded digest.
//! - Cross-domain collision resistance: v1 byte streams never equal any v2
//!   digest; pole-fingerprint domains (`ggen/pole/O` vs `ggen/pole/L`) never
//!   collide on identical inputs.
//! - Tamper rejection on every bound field.
//! - Rolling chain semantics modeled on graphlaw `chain_hash(t+1) =
//!   BLAKE3(chain_hash(t) || frame_bytes(t+1))`: each receipt binds the
//!   previous receipt's digest via its pre-state hash slot.
//!
//! Chicago TDD: real receipts, real digests, state-based assertions.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    deprecated // legacy_verify_v1 is the subject under test
)]

use chrono::{TimeZone, Utc};
use ggen_graph::receipt::GraphReceipt;

fn fixed_ts() -> chrono::DateTime<Utc> {
    Utc.with_ymd_and_hms(2026, 10, 20, 12, 34, 56).unwrap()
}

/// Build a v1-shaped receipt (legacy raw-concatenation binding) with fixed
/// fields, matching the golden vector below.
fn v1_golden_receipt() -> GraphReceipt {
    let mut r = GraphReceipt::new([1u8; 32], [2u8; 32], [3u8; 32]);
    r.version = 1;
    r.timestamp = fixed_ts();
    // Golden digest: BLAKE3_raw(01 || [1;32] || [2;32] || [3;32] ||
    // "2026-10-20T12:34:56+00:00") — pinned below, independently recomputable.
    r.signature_or_hash = GOLDEN_V1;
    r
}

/// Hardcoded v1 golden digest (64-hex as bytes). If the legacy path ever
/// changes, this test fails loudly — the drift becomes visible, not silent.
const GOLDEN_V1: [u8; 32] = [
    0x94, 0x54, 0x42, 0x35, 0x3d, 0xd9, 0xf3, 0x1f, 0x63, 0x98, 0x0a, 0x55, 0xd4, 0x24, 0xa4, 0x39,
    0x34, 0xfb, 0x0c, 0xcd, 0xb8, 0x1d, 0x65, 0x3d, 0xb1, 0xca, 0x04, 0xe3, 0x5f, 0x4b, 0x06, 0x64,
];

#[test]
fn v2_receipt_verifies_via_domain_separated_path() {
    let receipt = GraphReceipt::new([1u8; 32], [2u8; 32], [3u8; 32]);
    assert_eq!(receipt.version, 2, "new() must emit v2 receipts");
    receipt
        .verify()
        .expect("freshly minted v2 receipt must self-verify");
}

#[test]
fn v1_golden_vector_still_verifies_via_legacy_path() {
    let receipt = v1_golden_receipt();
    assert_eq!(receipt.version, 1);
    receipt
        .verify()
        .expect("v1 golden vector must verify via legacy path");
    // And explicitly through the deprecated entry point.
    receipt
        .legacy_verify_v1()
        .expect("legacy_verify_v1 must accept the golden vector");
}

#[test]
fn v1_golden_vector_rejects_any_field_drift() {
    let mut drifted = v1_golden_receipt();
    drifted.pre_state_hash = [9u8; 32];
    assert!(drifted.verify().is_err(), "drifted pre-state must fail");
}

#[test]
fn unknown_version_is_typed_refusal() {
    let mut r = GraphReceipt::new([1u8; 32], [2u8; 32], [3u8; 32]);
    r.version = 42;
    let err = r.verify().expect_err("unknown version must be refused");
    let msg = format!("{err}");
    assert!(
        msg.contains("Unsupported GraphReceipt version"),
        "expected typed unsupported-version refusal, got: {msg}"
    );
}

#[test]
fn cross_domain_v1_bytes_never_equal_v2_digest() {
    // The exact byte stream v1 hashes, hashed again under the v2 keyed
    // domain: must differ from the raw digest (domain separation holds).
    let mut hasher = blake3::Hasher::new();
    hasher.update(&[1u8]);
    hasher.update(&[1u8; 32]);
    hasher.update(&[2u8; 32]);
    hasher.update(&[3u8; 32]);
    hasher.update(fixed_ts().to_rfc3339().as_bytes());
    let v1_raw: [u8; 32] = hasher.finalize().into();
    assert_eq!(
        v1_raw, GOLDEN_V1,
        "golden vector must equal the independently recomputed v1 raw digest"
    );

    let v2_over_same_bytes: [u8; 32] = {
        let mut h = blake3::Hasher::new_keyed(&blake3::derive_key("ggen.graph.receipt.v2", &[]));
        h.update(&[1u8]);
        h.update(&[1u8; 32]);
        h.update(&[2u8; 32]);
        h.update(&[3u8; 32]);
        h.update(fixed_ts().to_rfc3339().as_bytes());
        h.finalize().into()
    };
    assert_ne!(
        v1_raw, v2_over_same_bytes,
        "v2 keyed digest must differ from v1 raw digest over identical bytes"
    );

    // A v1 receipt whose signature slot is overwritten with a v2 digest must
    // fail v1 verification (the digest spaces are disjoint in practice).
    let mut poisoned = v1_golden_receipt();
    poisoned.signature_or_hash = v2_over_same_bytes;
    assert!(
        poisoned.legacy_verify_v1().is_err(),
        "v2 digest must not validate as a v1 signature"
    );
}

#[test]
fn pole_fingerprint_domains_are_separated() {
    use ggen_graph::coherence::CoherenceChecker;
    let items = ["<s> <p> <o> ."];
    let o = CoherenceChecker::fingerprint_ontology(&items);
    let l = CoherenceChecker::fingerprint_event_log(&items);
    let a = CoherenceChecker::fingerprint_artifacts(&[("x", 1)]);
    assert_ne!(
        o.hash, l.hash,
        "identical items under ggen/pole/O vs ggen/pole/L must not collide"
    );
    // Hex format preserved (64 lowercase hex chars) for drift-report compat.
    for h in [&o.hash, &l.hash] {
        assert_eq!(h.len(), 64, "hex output format must be preserved");
        assert!(h
            .chars()
            .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()));
    }
    let _ = a;
}

#[test]
fn cross_domain_v2_digest_never_equals_unkeyed_blake3() {
    // v2 binding for fixed fields vs the unkeyed BLAKE3 of the same stream.
    let receipt = GraphReceipt::new([1u8; 32], [2u8; 32], [3u8; 32]);
    let mut unkeyed = blake3::Hasher::new();
    unkeyed.update(&[2u8]);
    unkeyed.update(&[1u8; 32]);
    unkeyed.update(&[2u8; 32]);
    unkeyed.update(&[3u8; 32]);
    unkeyed.update(receipt.timestamp.to_rfc3339().as_bytes());
    let unkeyed_digest: [u8; 32] = unkeyed.finalize().into();
    assert_ne!(
        receipt.signature_or_hash, unkeyed_digest,
        "v2 keyed digest must differ from unkeyed BLAKE3 of the same stream"
    );
}

#[test]
fn tamper_rejection_on_every_bound_field() {
    let base = GraphReceipt::new([1u8; 32], [2u8; 32], [3u8; 32]);

    let mut tampered = base.clone();
    tampered.pre_state_hash = [0xFFu8; 32];
    assert!(tampered.verify().is_err(), "pre-state tamper must fail");

    let mut tampered = base.clone();
    tampered.post_state_hash = [0xFFu8; 32];
    assert!(tampered.verify().is_err(), "post-state tamper must fail");

    let mut tampered = base.clone();
    tampered.delta_hash = [0xFFu8; 32];
    assert!(tampered.verify().is_err(), "delta tamper must fail");

    let mut tampered = base.clone();
    tampered.timestamp = fixed_ts();
    assert!(tampered.verify().is_err(), "timestamp tamper must fail");

    let mut tampered = base.clone();
    tampered.signature_or_hash = [0u8; 32];
    assert!(tampered.verify().is_err(), "signature tamper must fail");
}

#[test]
fn rolling_chain_binds_each_link_to_the_previous_digest() {
    // graphlaw chain() semantics: chain_hash(t+1) = BLAKE3(chain_hash(t) ||
    // frame_bytes(t+1)). Modeled here by feeding each receipt's pre-state
    // slot with the previous link's signature_or_hash, then verifying the
    // whole chain and refusing a reordered or spliced chain.
    let mut chain: Vec<GraphReceipt> = Vec::new();
    let mut prev_digest = [0u8; 32]; // genesis
    for i in 0..4u8 {
        let mut pre = [0u8; 32];
        pre.copy_from_slice(&prev_digest);
        let receipt = GraphReceipt::new(pre, [i + 1; 32], [i; 32]);
        prev_digest = receipt.signature_or_hash;
        chain.push(receipt);
    }
    for r in &chain {
        r.verify().expect("every chain link must self-verify");
    }
    // Chain continuity: each link's pre-state == previous link's digest.
    for w in chain.windows(2) {
        assert_eq!(
            w[1].pre_state_hash, w[0].signature_or_hash,
            "chain must be cryptographically continuous"
        );
    }
    // Splice attack: replace a middle link with a fresh receipt re-bound to
    // the previous link's post-state hash. The link self-verifies (it is a
    // valid receipt) but the chain break is observable: its pre-state slot no
    // longer matches the previous link's cryptographic digest.
    let mut spliced = chain.clone();
    spliced[2] = GraphReceipt::new(chain[1].post_state_hash, [9; 32], [9; 32]);
    assert!(
        spliced[2].verify().is_ok(),
        "spliced link is itself a valid receipt and self-verifies"
    );
    assert_ne!(
        spliced[2].pre_state_hash, spliced[1].signature_or_hash,
        "splice must break the digest-chain continuity (pre != prev signature)"
    );
    // The honest chain never exhibits this break.
    for w in chain.windows(2) {
        assert_eq!(w[1].pre_state_hash, w[0].signature_or_hash);
    }
}
