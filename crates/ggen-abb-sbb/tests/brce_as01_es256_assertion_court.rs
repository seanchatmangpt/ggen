//! Chicago court: WebAuthn AS-01 assertion verifies under ES256 / `:prime256v1`.
//!
//! BRCE-WEBAUTHN-STANDING §5 **V7** — known-good fixture → admit; tampered →
//! `REFUSED_*`; alg ≠ ES256 → refuse. AAA, no mocks. Real `p256` ECDSA verify.
//!
//! Standing advanced: **PARTIAL_ALIVE (crypto shape)** only.
//! Does **not** claim hardware WebAuthn ceremony, BRCE broker (`V8`), or
//! Plane-5 DO (`V9`) as `ALIVE`.
//!
//! Falsifier: `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court`

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_abb_sbb::as01_assertion::{admit_as01_assertion, As01Assertion, As01AssertionRefusal};
use ggen_abb_sbb::as01_seat::{AS01_ALG, AS01_CURVE};
use p256::ecdsa::signature::Signer;
use p256::ecdsa::{Signature, SigningKey, VerifyingKey};

fn hex_lower(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for &b in bytes {
        out.push(HEX[(b >> 4) as usize] as char);
        out.push(HEX[(b & 0x0f) as usize] as char);
    }
    out
}

fn decode_hex(input: &str) -> Vec<u8> {
    let s = input.trim();
    assert!(
        s.len() % 2 == 0 && !s.is_empty(),
        "fixture hex must be well-formed"
    );
    let mut out = Vec::with_capacity(s.len() / 2);
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let hi = match bytes[i] {
            b'0'..=b'9' => bytes[i] - b'0',
            b'a'..=b'f' => bytes[i] - b'a' + 10,
            other => panic!("non-hex byte 0x{other:02x}"),
        };
        let lo = match bytes[i + 1] {
            b'0'..=b'9' => bytes[i + 1] - b'0',
            b'a'..=b'f' => bytes[i + 1] - b'a' + 10,
            other => panic!("non-hex byte 0x{other:02x}"),
        };
        out.push((hi << 4) | lo);
        i += 2;
    }
    out
}

fn fixture_signing_key() -> SigningKey {
    let mut seed = [0x11u8; 32];
    seed[31] = 0x42;
    SigningKey::from_bytes(&seed.into()).expect("fixture seed forms a valid P-256 scalar")
}

fn known_good_assertion() -> As01Assertion {
    let signing_key = fixture_signing_key();
    let verifying_key = VerifyingKey::from(&signing_key);
    let encoded = verifying_key.to_encoded_point(false);
    let challenge = b"as01:v7:challenge:fixture-001";
    let signature: Signature = signing_key.sign(challenge);
    As01Assertion {
        seat_id: "seat:as01:v7:fixture-001".to_string(),
        alg: AS01_ALG.to_string(),
        curve: AS01_CURVE.to_string(),
        public_key_sec1_hex: hex_lower(encoded.as_bytes()),
        challenge_hex: hex_lower(challenge),
        signature_hex: hex_lower(&signature.to_bytes()),
    }
}

#[test]
fn known_good_es256_prime256v1_assertion_is_admitted() {
    // Arrange
    let assertion = known_good_assertion();

    // Act
    let result = admit_as01_assertion(&assertion);

    // Assert
    assert!(
        result.is_ok(),
        "known-good ES256/prime256v1 assertion fixture must admit, got {result:?}"
    );
}

#[test]
fn tampered_assertion_signature_is_refused() {
    // Arrange
    let mut assertion = known_good_assertion();
    let mut sig = decode_hex(&assertion.signature_hex);
    sig[0] ^= 0xff;
    assertion.signature_hex = hex_lower(&sig);

    // Act
    let result = admit_as01_assertion(&assertion);

    // Assert
    assert!(
        matches!(result, Err(As01AssertionRefusal::SignatureInvalid)),
        "tampered assertion must refuse with SignatureInvalid, got {result:?}"
    );
    if let Err(err) = result {
        let msg = err.to_string();
        assert!(
            msg.contains("REFUSED_AS01_SIGNATURE_INVALID"),
            "refusal must carry REFUSED_AS01_SIGNATURE_INVALID, got {msg}"
        );
    }
}

#[test]
fn alg_not_es256_is_refused() {
    // Arrange — SovereignLease EdDSA must not pass AS-01 assertion verify
    let mut assertion = known_good_assertion();
    assertion.alg = "EdDSA".to_string();

    // Act
    let result = admit_as01_assertion(&assertion);

    // Assert
    assert!(
        matches!(result, Err(As01AssertionRefusal::WrongAlg { .. })),
        "alg≠ES256 must refuse, got {result:?}"
    );
    if let Err(err) = result {
        let msg = err.to_string();
        assert!(
            msg.contains("REFUSED_AS01_ALG") && msg.contains("ES256"),
            "refusal must name REFUSED_AS01_ALG / ES256, got {msg}"
        );
    }
}

#[test]
fn wrong_curve_is_refused() {
    // Arrange
    let mut assertion = known_good_assertion();
    assertion.curve = "ed25519".to_string();

    // Act
    let result = admit_as01_assertion(&assertion);

    // Assert
    assert!(
        matches!(result, Err(As01AssertionRefusal::WrongCurve { .. })),
        "curve≠prime256v1 must refuse, got {result:?}"
    );
    if let Err(err) = result {
        assert!(err.to_string().contains("REFUSED_AS01_CURVE"));
    }
}
