//! Ed25519 signing delegation (feature-gated to `signed` feature).
//!
//! This module is a thin wrapper around `chatman_common::signed_receipt`,
//! keeping the ed25519 surface centralized in chatman-common (this crate
//! never touches `ed25519-dalek` directly).
//!
//! A signature produced here is the serialized JSON bytes of a
//! [`chatman_common::signed_receipt::SignedReceipt`] — i.e. the chain hash
//! (hex), the base64 ed25519 signature, and the hex verifying key are all
//! bundled together. This makes the signature self-contained: it can be
//! verified without any out-of-band key material (though `_with_key`
//! variants are provided for callers that want to pin authenticity to a
//! specific known key rather than trusting whatever key is embedded).
//!
//! Key loading is env-first: [`sign_chain_hash`] delegates to
//! `chatman_common::signed_receipt::sign_with_env_key`, which reads
//! `PRAXIS_SIGNING_KEY` (or `PRAXIS_SIGNING_KEY_FILE`). A missing key is a
//! hard error (fail closed) — the `signed` feature exists to guarantee a
//! signature is produced, so silently skipping it would defeat the point.

use chatman_common::signed_receipt::{self, SignedReceipt};

use crate::error::CoreError;

/// Sign a chain hash using the ed25519 key loaded from the environment
/// (`PRAXIS_SIGNING_KEY` / `PRAXIS_SIGNING_KEY_FILE`; see
/// `chatman_common::signed_receipt::signing_key_from_env`).
///
/// Returns the serialized JSON bytes of the resulting `SignedReceipt`. Fails
/// closed: a missing/unreadable key or a signing error is `CoreError::SigningFailed`.
pub fn sign_chain_hash(chain_hash: &[u8; 32]) -> Result<Vec<u8>, CoreError> {
    let hash_hex = hex::encode(chain_hash);
    let signed = signed_receipt::sign_with_env_key(&hash_hex)
        .map_err(|e| CoreError::SigningFailed(e.to_string()))?;
    serde_json::to_vec(&signed).map_err(|e| CoreError::SerializationFailed(e.to_string()))
}

/// Sign a chain hash using an explicit ed25519 signing key (64 lowercase hex
/// chars — the 32-byte seed), bypassing environment lookup.
///
/// Intended for tests and any caller that manages its own key material
/// directly rather than through `PRAXIS_SIGNING_KEY`.
pub fn sign_chain_hash_with_key(
    chain_hash: &[u8; 32],
    signing_key_hex: &str,
) -> Result<Vec<u8>, CoreError> {
    let hash_hex = hex::encode(chain_hash);
    let signed = signed_receipt::sign(&hash_hex, signing_key_hex)
        .map_err(|e| CoreError::SigningFailed(e.to_string()))?;
    serde_json::to_vec(&signed).map_err(|e| CoreError::SerializationFailed(e.to_string()))
}

/// Verify a signature (as produced by [`sign_chain_hash`]) over `chain_hash`,
/// trusting the verifying key embedded in the signature itself.
///
/// This checks integrity (the bytes weren't tampered with after signing) but
/// not authenticity against a specific known key — use
/// [`verify_chain_hash_with_key`] when the caller has an independent source
/// of truth for the verifying key.
pub fn verify_chain_hash(chain_hash: &[u8; 32], signature: &[u8]) -> Result<(), CoreError> {
    let signed: SignedReceipt =
        serde_json::from_slice(signature).map_err(|_| CoreError::SignatureInvalid)?;
    verify_signed_receipt(chain_hash, &signed, &signed.verifying_key.clone())
}

/// Verify a signature (as produced by [`sign_chain_hash`]/[`sign_chain_hash_with_key`])
/// over `chain_hash`, against an explicit verifying key (64 lowercase hex
/// chars) rather than whatever key is embedded in the signature.
///
/// This is the authenticity check: it fails if the signature was produced by
/// a different key than `verifying_key_hex`, even if the signature is
/// otherwise internally consistent.
pub fn verify_chain_hash_with_key(
    chain_hash: &[u8; 32],
    signature: &[u8],
    verifying_key_hex: &str,
) -> Result<(), CoreError> {
    let signed: SignedReceipt =
        serde_json::from_slice(signature).map_err(|_| CoreError::SignatureInvalid)?;
    verify_signed_receipt(chain_hash, &signed, verifying_key_hex)
}

/// Shared verification core: check the embedded chain hash matches the
/// presented one, then delegate the cryptographic check to chatman-common.
fn verify_signed_receipt(
    chain_hash: &[u8; 32],
    signed: &SignedReceipt,
    verifying_key_hex: &str,
) -> Result<(), CoreError> {
    if signed.chain_hash != hex::encode(chain_hash) {
        return Err(CoreError::SignatureInvalid);
    }
    match signed_receipt::verify(signed, verifying_key_hex) {
        Ok(true) => Ok(()),
        Ok(false) | Err(_) => Err(CoreError::SignatureInvalid),
    }
}

#[cfg(test)]
mod tests {
    use chatman_common::signed_receipt::KeyPair;

    use super::*;

    #[test]
    fn sign_verify_round_trip_with_explicit_key() {
        let kp = KeyPair::generate();
        let hash = [9u8; 32];
        let sig = sign_chain_hash_with_key(&hash, &kp.signing_key_hex()).expect("sign");
        assert!(verify_chain_hash(&hash, &sig).is_ok());
        assert!(verify_chain_hash_with_key(&hash, &sig, &kp.verifying_key_hex()).is_ok());
    }

    #[test]
    fn tampered_hash_rejected() {
        let kp = KeyPair::generate();
        let hash = [1u8; 32];
        let sig = sign_chain_hash_with_key(&hash, &kp.signing_key_hex()).expect("sign");
        let other_hash = [2u8; 32];
        assert!(matches!(
            verify_chain_hash(&other_hash, &sig),
            Err(CoreError::SignatureInvalid)
        ));
    }

    #[test]
    fn tampered_signature_rejected() {
        let kp = KeyPair::generate();
        let hash = [3u8; 32];
        let sig = sign_chain_hash_with_key(&hash, &kp.signing_key_hex()).expect("sign");
        let mut sr: SignedReceipt = serde_json::from_slice(&sig).expect("valid json");
        // Flip a character in the base64 signature (keep it valid base64-ish
        // and the same length so it still deserializes).
        let mut chars: Vec<char> = sr.signature.chars().collect();
        let idx = chars.iter().position(|&c| c != 'A').unwrap_or(0);
        chars[idx] = if chars[idx] == 'B' { 'C' } else { 'B' };
        sr.signature = chars.into_iter().collect();
        let tampered = serde_json::to_vec(&sr).expect("serialize");
        assert!(matches!(
            verify_chain_hash(&hash, &tampered),
            Err(CoreError::SignatureInvalid)
        ));
    }

    #[test]
    fn wrong_verifying_key_rejected() {
        let kp1 = KeyPair::generate();
        let kp2 = KeyPair::generate();
        let hash = [4u8; 32];
        let sig = sign_chain_hash_with_key(&hash, &kp1.signing_key_hex()).expect("sign");
        assert!(matches!(
            verify_chain_hash_with_key(&hash, &sig, &kp2.verifying_key_hex()),
            Err(CoreError::SignatureInvalid)
        ));
    }

    #[test]
    fn missing_key_errors() {
        let _guard = crate::signing::test_support::env_lock();
        let hash = [5u8; 32];
        // Ensure neither env var is set for this test's slice of time.
        let had_key = std::env::var("PRAXIS_SIGNING_KEY").ok();
        let had_file = std::env::var("PRAXIS_SIGNING_KEY_FILE").ok();
        std::env::remove_var("PRAXIS_SIGNING_KEY");
        std::env::remove_var("PRAXIS_SIGNING_KEY_FILE");

        let result = sign_chain_hash(&hash);

        // Restore whatever was there before (best-effort; other tests hold
        // the same lock so this is safe from races).
        if let Some(v) = had_key {
            std::env::set_var("PRAXIS_SIGNING_KEY", v);
        }
        if let Some(v) = had_file {
            std::env::set_var("PRAXIS_SIGNING_KEY_FILE", v);
        }

        assert!(matches!(result, Err(CoreError::SigningFailed(_))));
    }
}

/// Test-only helpers shared across the crate's env-var-touching tests
/// (`signing.rs`, `law.rs`, `default_law.rs` all have tests that exercise
/// `receipt()` under the `signed` feature).
#[cfg(test)]
pub(crate) mod test_support {
    use std::sync::{Mutex, MutexGuard, OnceLock};

    /// Fixed 64-hex-char (32-byte) ed25519 seed used only by tests. Not
    /// security-sensitive: it exists so `receipt()`'s `#[cfg(feature =
    /// "signed")]` path has a deterministic `PRAXIS_SIGNING_KEY` to sign
    /// against when tests run under `--features signed`.
    pub(crate) const TEST_SIGNING_KEY_HEX: &str =
        "a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebf00";

    /// Global lock serializing tests that read/write `PRAXIS_SIGNING_KEY*`.
    ///
    /// `std::env` is process-global, so tests that mutate it must not run
    /// concurrently with each other (or with any test calling `receipt()`
    /// while `signed` is enabled, since that reads the same env vars).
    /// Poison is ignored: a panicking test still releases the lock for the
    /// next one rather than poisoning the whole suite.
    pub(crate) fn env_lock() -> MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(()))
            .lock()
            .unwrap_or_else(|e| e.into_inner())
    }

    /// Set `PRAXIS_SIGNING_KEY` to [`TEST_SIGNING_KEY_HEX`] for the duration
    /// of the returned guard, holding [`env_lock`] so this serializes
    /// against any test that temporarily removes the env vars.
    pub(crate) fn with_test_signing_key() -> MutexGuard<'static, ()> {
        let guard = env_lock();
        std::env::set_var("PRAXIS_SIGNING_KEY", TEST_SIGNING_KEY_HEX);
        guard
    }
}
