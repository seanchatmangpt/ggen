//! Cryptographic primitives for MCP+
//!
//! Provides:
//! - SHA-256 and SHA3-256 hashing
//! - Ed25519 signing and verification
//! - Key pair management
//! - Deterministic operations

use crate::error::{McpError, McpResult};
use ed25519_dalek::{Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use sha3::Sha3_256;

/// Hash data using SHA-256
#[inline]
pub fn hash_sha256(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hex::encode(hasher.finalize())
}

/// Hash data using SHA-256, returning bytes
#[inline]
pub fn hash_sha256_bytes(data: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hasher.finalize().into()
}

/// Hash data using SHA3-256
#[inline]
pub fn hash_sha3_256(data: &[u8]) -> String {
    let mut hasher = Sha3_256::new();
    hasher.update(data);
    hex::encode(hasher.finalize())
}

/// Combine two hashes (for Merkle tree construction)
#[inline]
pub fn combine_hashes(left: &[u8; 32], right: &[u8; 32]) -> [u8; 32] {
    let mut combined = [0u8; 64];
    combined[..32].copy_from_slice(left);
    combined[32..].copy_from_slice(right);
    hash_sha256_bytes(&combined)
}

/// Ed25519 signature wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    pub bytes: Vec<u8>,
}

impl Signature {
    pub fn from_bytes(bytes: Vec<u8>) -> McpResult<Self> {
        if bytes.len() != 64 {
            return Err(McpError::InvalidSignature(format!(
                "Expected 64 bytes, got {}",
                bytes.len()
            )));
        }
        Ok(Self { bytes })
    }

    pub fn to_hex(&self) -> String {
        hex::encode(&self.bytes)
    }
}

/// Ed25519 key pair for signing operations
#[derive(Clone)]
pub struct KeyPair {
    signing_key: SigningKey,
    verifying_key: VerifyingKey,
}

impl KeyPair {
    /// Generate a new random key pair
    pub fn generate() -> McpResult<Self> {
        let signing_key = SigningKey::generate(&mut OsRng);
        let verifying_key = signing_key.verifying_key();
        Ok(Self { signing_key, verifying_key })
    }

    /// Create key pair from seed bytes (deterministic)
    pub fn from_seed(seed: &[u8; 32]) -> Self {
        let signing_key = SigningKey::from_bytes(seed);
        let verifying_key = signing_key.verifying_key();
        Self { signing_key, verifying_key }
    }

    /// Get the public key as bytes
    pub fn public_key_bytes(&self) -> [u8; 32] {
        self.verifying_key.to_bytes()
    }

    /// Get the public key hash (SHA-256)
    pub fn public_key_hash(&self) -> String {
        hash_sha256(&self.public_key_bytes())
    }

    /// Sign data
    pub fn sign(&self, data: &[u8]) -> Signature {
        let sig = self.signing_key.sign(data);
        Signature { bytes: sig.to_bytes().to_vec() }
    }

    /// Verify a signature
    pub fn verify(&self, data: &[u8], signature: &Signature) -> McpResult<()> {
        let sig = ed25519_dalek::Signature::from_slice(&signature.bytes)
            .map_err(|e| McpError::InvalidSignature(e.to_string()))?;

        self.verifying_key
            .verify(data, &sig)
            .map_err(|e| McpError::SignatureVerificationFailed(e.to_string()))
    }
}

/// Verify signature with raw public key bytes
pub fn verify_with_public_key(
    data: &[u8],
    signature: &[u8],
    public_key: &[u8; 32],
) -> McpResult<()> {
    let verifying_key = VerifyingKey::from_bytes(public_key)
        .map_err(|e| McpError::InvalidPublicKey(e.to_string()))?;

    let sig = ed25519_dalek::Signature::from_slice(signature)
        .map_err(|e| McpError::InvalidSignature(e.to_string()))?;

    verifying_key
        .verify(data, &sig)
        .map_err(|e| McpError::SignatureVerificationFailed(e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256_determinism() {
        let data = b"hello world";
        let h1 = hash_sha256(data);
        let h2 = hash_sha256(data);
        assert_eq!(h1, h2);
        assert_eq!(h1.len(), 64);
    }

    #[test]
    fn test_sha3_256_different_from_sha2() {
        let data = b"hello world";
        let sha2 = hash_sha256(data);
        let sha3 = hash_sha3_256(data);
        assert_ne!(sha2, sha3);
    }

    #[test]
    fn test_combine_hashes() {
        let left = hash_sha256_bytes(b"left");
        let right = hash_sha256_bytes(b"right");
        let combined = combine_hashes(&left, &right);
        assert_eq!(combined.len(), 32);

        // Deterministic
        let combined2 = combine_hashes(&left, &right);
        assert_eq!(combined, combined2);
    }

    #[test]
    fn test_keypair_from_seed_determinism() {
        let seed = [42u8; 32];
        let kp1 = KeyPair::from_seed(&seed);
        let kp2 = KeyPair::from_seed(&seed);
        assert_eq!(kp1.public_key_bytes(), kp2.public_key_bytes());
    }

    #[test]
    fn test_sign_and_verify() {
        let keypair = KeyPair::generate().unwrap();
        let data = b"test message";
        let signature = keypair.sign(data);
        assert!(keypair.verify(data, &signature).is_ok());
    }

    #[test]
    fn test_verify_wrong_data_fails() {
        let keypair = KeyPair::generate().unwrap();
        let signature = keypair.sign(b"correct");
        assert!(keypair.verify(b"wrong", &signature).is_err());
    }

    #[test]
    fn test_verify_with_public_key() {
        let keypair = KeyPair::generate().unwrap();
        let data = b"test message";
        let signature = keypair.sign(data);
        let public_key = keypair.public_key_bytes();

        assert!(verify_with_public_key(data, &signature.bytes, &public_key).is_ok());
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_sha256_deterministic(data: Vec<u8>) {
            let h1 = hash_sha256(&data);
            let h2 = hash_sha256(&data);
            prop_assert_eq!(h1, h2);
        }

        #[test]
        fn prop_sha256_length(data: Vec<u8>) {
            let hash = hash_sha256(&data);
            prop_assert_eq!(hash.len(), 64);
        }

        #[test]
        fn prop_sign_verify_roundtrip(data: Vec<u8>, seed: [u8; 32]) {
            let keypair = KeyPair::from_seed(&seed);
            let signature = keypair.sign(&data);
            prop_assert!(keypair.verify(&data, &signature).is_ok());
        }

        #[test]
        fn prop_different_data_different_hash(a: Vec<u8>, b: Vec<u8>) {
            prop_assume!(a != b);
            let ha = hash_sha256(&a);
            let hb = hash_sha256(&b);
            prop_assert_ne!(ha, hb);
        }
    }
}
