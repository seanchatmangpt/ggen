//! Cryptographic Operations Module
//!
//! Provides core cryptographic primitives for MCP+:
//! - SHA-256 and SHA3-256 hashing
//! - Ed25519 signing and verification
//! - Key pair generation

use crate::{McpError, McpResult};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use sha2::{Digest, Sha256};
use sha3::Sha3_256;

/// Hash data using SHA-256
pub fn hash_sha256(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    let result = hasher.finalize();
    hex::encode(result)
}

/// Hash data using SHA3-256
pub fn hash_sha3_256(data: &[u8]) -> String {
    let mut hasher = Sha3_256::new();
    hasher.update(data);
    let result = hasher.finalize();
    hex::encode(result)
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

        Ok(Self {
            signing_key,
            verifying_key,
        })
    }

    /// Create key pair from existing signing key bytes
    pub fn from_bytes(bytes: &[u8; 32]) -> McpResult<Self> {
        let signing_key = SigningKey::from_bytes(bytes);
        let verifying_key = signing_key.verifying_key();

        Ok(Self {
            signing_key,
            verifying_key,
        })
    }

    /// Get the public key as bytes
    pub fn public_key_bytes(&self) -> [u8; 32] {
        self.verifying_key.to_bytes()
    }

    /// Get the public key hash (SHA-256 of public key)
    pub fn public_key_hash(&self) -> String {
        hash_sha256(&self.public_key_bytes())
    }

    /// Sign data
    pub fn sign(&self, data: &[u8]) -> McpResult<Vec<u8>> {
        let signature = self.signing_key.sign(data);
        Ok(signature.to_bytes().to_vec())
    }

    /// Verify a signature
    pub fn verify(&self, data: &[u8], signature_bytes: &[u8]) -> McpResult<()> {
        let signature = Signature::from_slice(signature_bytes).map_err(|e| {
            McpError::CryptoError(format!("Invalid signature format: {e}"))
        })?;

        self.verifying_key
            .verify(data, &signature)
            .map_err(|e| McpError::CryptoError(format!("Signature verification failed: {e}")))
    }
}

/// Sign data with a private key
pub fn sign_data(data: &[u8], private_key: &[u8]) -> McpResult<Vec<u8>> {
    if private_key.len() != 32 {
        return Err(McpError::InvalidInput(format!(
            "Private key must be 32 bytes, got {}",
            private_key.len()
        )));
    }

    let mut key_bytes = [0u8; 32];
    key_bytes.copy_from_slice(private_key);

    let keypair = KeyPair::from_bytes(&key_bytes)?;
    keypair.sign(data)
}

/// Verify a signature against a public key
pub fn verify_signature(
    data: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> McpResult<()> {
    if public_key.len() != 32 {
        return Err(McpError::InvalidInput(format!(
            "Public key must be 32 bytes, got {}",
            public_key.len()
        )));
    }

    if signature.len() != 64 {
        return Err(McpError::InvalidInput(format!(
            "Signature must be 64 bytes, got {}",
            signature.len()
        )));
    }

    let mut key_bytes = [0u8; 32];
    key_bytes.copy_from_slice(public_key);

    let verifying_key = VerifyingKey::from_bytes(&key_bytes).map_err(|e| {
        McpError::CryptoError(format!("Invalid public key: {e}"))
    })?;

    let sig = Signature::from_slice(signature).map_err(|e| {
        McpError::CryptoError(format!("Invalid signature format: {e}"))
    })?;

    verifying_key
        .verify(data, &sig)
        .map_err(|e| McpError::CryptoError(format!("Signature verification failed: {e}")))
}

/// Key epoch for managing signing key rotation
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct KeyEpoch {
    pub epoch_id: String,
    pub public_key_hash: String,
    pub epoch_start: chrono::DateTime<chrono::Utc>,
    pub epoch_end: chrono::DateTime<chrono::Utc>,
    pub is_revoked: bool,
}

impl KeyEpoch {
    /// Create a new key epoch
    pub fn new(epoch_id: String, public_key_hash: String, duration_days: i64) -> Self {
        let now = chrono::Utc::now();
        let end = now + chrono::Duration::days(duration_days);

        Self {
            epoch_id,
            public_key_hash,
            epoch_start: now,
            epoch_end: end,
            is_revoked: false,
        }
    }

    /// Check if epoch is currently valid
    pub fn is_valid(&self) -> bool {
        let now = chrono::Utc::now();
        !self.is_revoked && now >= self.epoch_start && now <= self.epoch_end
    }

    /// Revoke the epoch
    pub fn revoke(&mut self) {
        self.is_revoked = true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256_hash() {
        let data = b"hello world";
        let hash = hash_sha256(data);
        assert_eq!(hash.len(), 64);

        // Deterministic
        let hash2 = hash_sha256(data);
        assert_eq!(hash, hash2);
    }

    #[test]
    fn test_sha3_256_hash() {
        let data = b"hello world";
        let hash = hash_sha3_256(data);
        assert_eq!(hash.len(), 64);

        // Different from SHA2
        let sha2_hash = hash_sha256(data);
        assert_ne!(hash, sha2_hash);
    }

    #[test]
    fn test_keypair_generation() {
        let keypair = KeyPair::generate().unwrap();
        assert_eq!(keypair.public_key_bytes().len(), 32);
        assert_eq!(keypair.public_key_hash().len(), 64);
    }

    #[test]
    fn test_sign_and_verify() {
        let keypair = KeyPair::generate().unwrap();
        let data = b"test message";

        let signature = keypair.sign(data).unwrap();
        assert_eq!(signature.len(), 64);

        keypair.verify(data, &signature).unwrap();
    }

    #[test]
    fn test_verify_wrong_data() {
        let keypair = KeyPair::generate().unwrap();
        let data = b"test message";
        let wrong_data = b"wrong message";

        let signature = keypair.sign(data).unwrap();
        let result = keypair.verify(wrong_data, &signature);
        assert!(result.is_err());
    }

    #[test]
    fn test_key_epoch_validity() {
        let epoch = KeyEpoch::new("epoch-001".to_string(), "pubkeyhash".to_string(), 90);
        assert!(epoch.is_valid());
        assert!(!epoch.is_revoked);
    }

    #[test]
    fn test_key_epoch_revocation() {
        let mut epoch = KeyEpoch::new("epoch-001".to_string(), "pubkeyhash".to_string(), 90);
        epoch.revoke();
        assert!(!epoch.is_valid());
        assert!(epoch.is_revoked);
    }
}
