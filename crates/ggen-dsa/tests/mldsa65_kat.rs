//! ML-DSA-65 known-answer tests (ZP-22 increment, register row OI-3).
//!
//! Known vector provenance: the RustCrypto `ml-dsa` crate's own published
//! example key pair (PKCS#8 `tests/examples/ML-DSA-65-seed.priv` /
//! `ML-DSA-65.pub` in ml-dsa-0.1.2). Its seed is the trivial 0x00..0x1f
//! sequence; its public key encodes to the raw 1952-byte value embedded below
//! (extracted from the published SPKI PEM via openssl -> DER -> raw pk).
//! Chicago discipline: assert on real derived state against the embedded
//! referent; no mocks.

use ggen_dsa::{
    sign, signing_key_from_seed, verify, verifying_key_bytes, verifying_key_from_seed,
};

/// Seed of the published example key: bytes 0x00, 0x01, ... 0x1f.
fn kat_seed() -> [u8; 32] {
    core::array::from_fn(|i| i as u8)
}

/// Raw ML-DSA-65 public key derived from that seed, per the published
/// ml-dsa-0.1.2 example key pair (1952 bytes, hex).
const KAT_PK_HEX: &str = include_str!("fixtures_ml_dsa_65_pk.hex");

fn kat_pk_bytes() -> Vec<u8> {
    (0..KAT_PK_HEX.len() / 2)
        .map(|i| u8::from_str_radix(&KAT_PK_HEX[2 * i..2 * i + 2], 16).unwrap())
        .collect()
}

#[test]
fn seed_derives_the_published_public_key() {
    let sk = signing_key_from_seed(&kat_seed());
    let pk_bytes = verifying_key_bytes(&sk);
    let expected = kat_pk_bytes();
    assert_eq!(pk_bytes.len(), 1952);
    assert_eq!(
        pk_bytes, expected,
        "seed-derived pk must match the published ml-dsa-0.1.2 example key"
    );
}

#[test]
fn sign_verify_round_trip_on_known_key() {
    let sk = signing_key_from_seed(&kat_seed());
    let pk = verifying_key_from_seed(&kat_seed());
    let msg = b"ggen receipt body";
    let ctx = b"OI-3";
    let sig = sign(&sk, msg, ctx);
    assert_eq!(sig.len(), 3309);
    assert!(verify(&pk, msg, ctx, &sig));
}

#[test]
fn falsifier_tampered_message_refused() {
    let sk = signing_key_from_seed(&kat_seed());
    let pk = verifying_key_from_seed(&kat_seed());
    let sig = sign(&sk, b"ggen receipt body", b"OI-3");
    assert!(!verify(&pk, b"ggen receipt bodY", b"OI-3", &sig));
}

#[test]
fn falsifier_tampered_context_refused() {
    let sk = signing_key_from_seed(&kat_seed());
    let pk = verifying_key_from_seed(&kat_seed());
    let sig = sign(&sk, b"ggen receipt body", b"OI-3");
    assert!(!verify(&pk, b"ggen receipt body", b"OI-4", &sig));
}

#[test]
fn falsifier_wrong_key_refused() {
    let sig = sign(&signing_key_from_seed(&kat_seed()), b"msg", b"");
    let other = verifying_key_from_seed(&[0xffu8; 32]);
    assert!(!verify(&other, b"msg", b"", &sig));
}

#[test]
fn falsifier_wrong_seed_keygen_diverges() {
    let a = verifying_key_bytes(&signing_key_from_seed(&kat_seed()));
    let b = verifying_key_bytes(&signing_key_from_seed(&[0xffu8; 32]));
    assert_ne!(a, b);
}
