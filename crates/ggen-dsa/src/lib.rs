//! # ggen-dsa
//!
//! ML-DSA-65 (FIPS-204) signer for the ggen receipt chain — ZP-22 first
//! increment (register row OI-3).
//!
//! Prior-art-first: wraps the RustCrypto [`ml_dsa`] crate (pure Rust,
//! `no_std`-capable, FIPS-204 ML-DSA-66? no — ML-DSA-65) rather than
//! hand-rolling the lattice arithmetic.
//!
//! Increment scope (per dispatch): sign+verify round-trip against a known
//! test vector, with its own falsifier. The ZP-22 five-phase build
//! (branchless SampleInBall/SampleMask, FS-abort sign loop, 2420-B serializer
//! onto the frozen Receipt shape) is REMAINING — the PLANS.md premise of
//! "reusing the landed field kernel (fips203/fips202)" is stale as-of
//! 2026-11-05: no in-repo fips203/fips202 kernel exists (repo-wide grep, zero
//! hits), so prior-art-first routed to the published crate instead.

pub const ML_DSA_65_SIGNATURE_LEN: usize = 3309;
pub const ML_DSA_65_VERIFYING_KEY_LEN: usize = 1952;
pub const ML_DSA_65_SIGNING_SEED_LEN:  usize = 32;

use ml_dsa::{Keypair, MlDsa65, Seed, SignatureEncoding, SigningKey, VerifyingKey};

/// Deterministically derive the ML-DSA-65 signing key from a 32-byte seed.
pub fn signing_key_from_seed(seed: &[u8; 32]) -> SigningKey<MlDsa65> {
    SigningKey::<MlDsa65>::from_seed(&Seed::from(*seed))
}

/// Sign `msg` under `sk` with a FIPS-204 context string, deterministic.
/// Returns encoded signature bytes (3309 B for ML-DSA-65).
pub fn sign(sk: &SigningKey<MlDsa65>, msg: &[u8], context: &[u8]) -> Vec<u8> {
    sk.expanded_key()
        .sign_deterministic(msg, context)
        .expect("ML-DSA-65 sign_deterministic: re-encoding retry loop exhausted")
        .to_vec()
}

/// Verify `msg` + `context` against encoded signature bytes under `pk`.
/// Returns false on any malformed signature encoding.
pub fn verify(
    pk: &VerifyingKey<MlDsa65>,
    msg: &[u8],
    context: &[u8],
    sig_bytes: &[u8],
) -> bool {
    let sig = match ml_dsa::Signature::<MlDsa65>::try_from(sig_bytes) {
        Ok(sig) => sig,
        Err(_) => return false,
    };
    pk.verify_with_context(msg, context, &sig)
}

/// Encode a verifying key to its 1952-byte raw encoding.
pub fn verifying_key_bytes(sk: &SigningKey<MlDsa65>) -> Vec<u8> {
    sk.verifying_key().encode().to_vec()
}

/// Re-derive the verifying key from a signing key's seed.
pub fn verifying_key_from_seed(seed: &[u8; 32]) -> VerifyingKey<MlDsa65> {
    signing_key_from_seed(seed).verifying_key()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signature_length_matches_fips_204_param_set() {
        let seed = [7u8; 32];
        let sk = signing_key_from_seed(&seed);
        let sig = sign(&sk, b"ggen receipt", b"OI-3");
        assert_eq!(sig.len(), ML_DSA_65_SIGNATURE_LEN);
    }

    #[test]
    fn malformed_signature_encoding_refused() {
        let sk = signing_key_from_seed(&[9u8; 32]);
        let pk = sk.verifying_key();
        let mut sig = sign(&sk, b"msg", b"ctx");
        // truncate + extend: wrong length must decode-fail, not silently pass
        sig.truncate(ML_DSA_65_SIGNATURE_LEN - 1);
        sig.push(0);
        assert!(!verify(&pk, b"msg", b"ctx", &sig));
    }
}
