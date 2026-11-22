//! BRCE + WebAuthn AS-01 assertion verify (ES256 / `:prime256v1`) — crypto shape.
//!
//! BRCE-WEBAUTHN-STANDING §5 **V7**: known-good fixture admits; tampered signature
//! refuses; alg ≠ ES256 refuses. This is the in-repo adapter court that advances
//! standing to **PARTIAL_ALIVE (crypto shape)** without claiming:
//!
//! - full WebAuthn / FIDO2 RP ceremony or hardware authenticator `ALIVE`
//! - live BRCE broker admit (`V8`)
//! - Plane-5 front-door DO (`V9`)
//!
//! Distinct from [`crate::as01_seat`] (presence/shape fence only) and from
//! `ggen-sjira::lease` (Ed25519 / `EdDSA` sovereign lease).

use p256::ecdsa::signature::Verifier;
use p256::ecdsa::{Signature, VerifyingKey};
use p256::elliptic_curve::sec1::FromEncodedPoint;
use p256::{EncodedPoint, PublicKey};
use thiserror::Error;

use crate::as01_seat::{AS01_ALG, AS01_CURVE};

/// Fixture-level AS-01 assertion bound to ES256 / prime256v1.
///
/// Carries SEC1 public key + challenge + fixed-size ECDSA signature (r\|\|s).
/// Does **not** model clientDataJSON, authenticatorData, RP ID hash, or
/// attestation — those remain outside this PARTIAL crypto-shape court.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct As01Assertion {
    pub seat_id: String,
    pub alg: String,
    pub curve: String,
    /// SEC1-encoded P-256 public key as lowercase hex (compressed or uncompressed).
    pub public_key_sec1_hex: String,
    /// Challenge / signed message bytes as lowercase hex.
    pub challenge_hex: String,
    /// 64-byte ECDSA signature (r\|\|s) as lowercase hex.
    pub signature_hex: String,
}

/// Typed refusal of an AS-01 assertion at the V7 crypto-shape gate.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum As01AssertionRefusal {
    /// Algorithm is not the admitted AS-01 COSE alg.
    #[error("REFUSED_AS01_ALG: alg must be {AS01_ALG}, got {got}")]
    WrongAlg { got: String },
    /// Curve is not the admitted AS-01 curve.
    #[error("REFUSED_AS01_CURVE: curve must be {AS01_CURVE}, got {got}")]
    WrongCurve { got: String },
    /// Seat id empty or key/challenge/signature encoding is not well-formed.
    #[error("REFUSED_AS01_ASSERTION_SHAPE: {reason}")]
    Shape { reason: String },
    /// ECDSA P-256 verification failed (tamper / wrong key / wrong challenge).
    #[error("REFUSED_AS01_SIGNATURE_INVALID: ES256/prime256v1 signature did not verify")]
    SignatureInvalid,
}

/// Verify an AS-01 assertion under ES256 / `:prime256v1`.
///
/// Returns `Ok(())` only when alg/curve match the admitted pair and the ECDSA
/// P-256 signature verifies over the challenge. Does not grant DO authority and
/// must not be promoted to Plane-5 `ALIVE`.
pub fn admit_as01_assertion(assertion: &As01Assertion) -> Result<(), As01AssertionRefusal> {
    if assertion.seat_id.trim().is_empty() {
        return Err(As01AssertionRefusal::Shape {
            reason: "seat_id must be non-empty".to_string(),
        });
    }
    if assertion.alg != AS01_ALG {
        return Err(As01AssertionRefusal::WrongAlg {
            got: assertion.alg.clone(),
        });
    }
    if assertion.curve != AS01_CURVE {
        return Err(As01AssertionRefusal::WrongCurve {
            got: assertion.curve.clone(),
        });
    }

    let pk_bytes = decode_hex(&assertion.public_key_sec1_hex).map_err(|reason| {
        As01AssertionRefusal::Shape {
            reason: format!("public_key_sec1_hex: {reason}"),
        }
    })?;
    let challenge = decode_hex(&assertion.challenge_hex).map_err(|reason| {
        As01AssertionRefusal::Shape {
            reason: format!("challenge_hex: {reason}"),
        }
    })?;
    let sig_bytes = decode_hex(&assertion.signature_hex).map_err(|reason| {
        As01AssertionRefusal::Shape {
            reason: format!("signature_hex: {reason}"),
        }
    })?;

    let encoded = EncodedPoint::from_bytes(&pk_bytes).map_err(|_| As01AssertionRefusal::Shape {
        reason: "public_key_sec1_hex is not a valid SEC1 encoded point".to_string(),
    })?;
    let public =
        Option::<PublicKey>::from(PublicKey::from_encoded_point(&encoded)).ok_or_else(|| {
            As01AssertionRefusal::Shape {
                reason: "public_key_sec1_hex does not decode to a P-256 public key".to_string(),
            }
        })?;
    let verifying_key = VerifyingKey::from(public);

    let signature = Signature::from_slice(&sig_bytes).map_err(|_| As01AssertionRefusal::Shape {
        reason: "signature_hex must be a 64-byte ECDSA (r||s) encoding".to_string(),
    })?;

    verifying_key
        .verify(&challenge, &signature)
        .map_err(|_| As01AssertionRefusal::SignatureInvalid)?;

    Ok(())
}

fn decode_hex(input: &str) -> Result<Vec<u8>, String> {
    let s = input.trim();
    if s.is_empty() {
        return Err("empty hex".to_string());
    }
    if s.len() % 2 != 0 {
        return Err("odd-length hex".to_string());
    }
    let mut out = Vec::with_capacity(s.len() / 2);
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let hi = hex_nibble(bytes[i])?;
        let lo = hex_nibble(bytes[i + 1])?;
        out.push((hi << 4) | lo);
        i += 2;
    }
    Ok(out)
}

fn hex_nibble(b: u8) -> Result<u8, String> {
    match b {
        b'0'..=b'9' => Ok(b - b'0'),
        b'a'..=b'f' => Ok(b - b'a' + 10),
        b'A'..=b'F' => Ok(b - b'A' + 10),
        _ => Err(format!("non-hex byte 0x{b:02x}")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use p256::ecdsa::signature::Signer;
    use p256::ecdsa::SigningKey;

    fn hex_lower(bytes: &[u8]) -> String {
        const HEX: &[u8; 16] = b"0123456789abcdef";
        let mut out = String::with_capacity(bytes.len() * 2);
        for &b in bytes {
            out.push(HEX[(b >> 4) as usize] as char);
            out.push(HEX[(b & 0x0f) as usize] as char);
        }
        out
    }

    fn fixture_key() -> SigningKey {
        // Deterministic non-zero scalar seed — RFC-style fixed test key, no RNG.
        let mut seed = [0x11u8; 32];
        seed[31] = 0x42;
        SigningKey::from_bytes(&seed.into()).expect("fixture seed forms a valid P-256 scalar")
    }

    fn known_good_assertion() -> As01Assertion {
        let signing_key = fixture_key();
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
    fn known_good_es256_assertion_is_admitted() {
        let assertion = known_good_assertion();
        assert!(
            admit_as01_assertion(&assertion).is_ok(),
            "known-good ES256/prime256v1 fixture must admit"
        );
    }

    #[test]
    fn tampered_signature_is_refused() {
        let mut assertion = known_good_assertion();
        let mut sig = decode_hex(&assertion.signature_hex).expect("sig hex");
        sig[0] ^= 0xff;
        assertion.signature_hex = hex_lower(&sig);
        let result = admit_as01_assertion(&assertion);
        assert!(
            matches!(result, Err(As01AssertionRefusal::SignatureInvalid)),
            "tampered signature must refuse, got {result:?}"
        );
    }

    #[test]
    fn alg_not_es256_is_refused() {
        let mut assertion = known_good_assertion();
        assertion.alg = "EdDSA".to_string();
        let result = admit_as01_assertion(&assertion);
        assert!(
            matches!(result, Err(As01AssertionRefusal::WrongAlg { .. })),
            "alg≠ES256 must refuse, got {result:?}"
        );
        if let Err(err) = result {
            assert!(err.to_string().contains("REFUSED_AS01_ALG"));
        }
    }
}
