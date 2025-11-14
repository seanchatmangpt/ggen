//! Extended cryptographic verifier with advanced features
//!
//! This module provides the `CryptoVerifierExt` trait which extends the base
//! `CryptoVerifier` trait with additional functionality for batch verification,
//! detached signatures, timestamped signatures, and key management.
//!
//! ## Features
//!
//! - **Batch Verification**: Verify multiple signatures efficiently
//! - **Detached Signatures**: Sign and verify content separately from signatures
//! - **Timestamped Signatures**: Add timestamps to signatures for audit trails
//! - **Key Management**: Secure key storage and retrieval
//!
//! ## Examples
//!
//! ### Batch Verification
//!
//! ```rust,no_run
//! use ggen_marketplace::traits::crypto::{CryptoVerifierExt, VerificationRequest};
//!
//! # async fn example() -> anyhow::Result<()> {
//! let verifier: Box<dyn CryptoVerifierExt> = /* ... */;
//! let requests = vec![
//!     VerificationRequest { content: b"content1".to_vec(), signature: /* ... */ },
//!     VerificationRequest { content: b"content2".to_vec(), signature: /* ... */ },
//! ];
//!
//! let results = verifier.verify_batch(&requests).await?;
//! for result in results {
//!     println!("Verification: {}", result.valid);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Detached Signatures
//!
//! ```rust,no_run
//! use ggen_marketplace::traits::crypto::CryptoVerifierExt;
//!
//! # fn main() -> anyhow::Result<()> {
//! let verifier: Box<dyn CryptoVerifierExt> = /* ... */;
//! let content = b"Hello, World!";
//!
//! // Create detached signature
//! let signature_bytes = verifier.sign_detached(content)?;
//!
//! // Verify detached signature
//! let public_key = /* ... */;
//! let valid = verifier.verify_detached(content, &signature_bytes, &public_key)?;
//! # Ok(())
//! # }
//! ```

use crate::error::Result;
use crate::models::signature::{KeyPair, PublicKey, Signature, VerificationResult};
use async_trait::async_trait;

/// Extended crypto verifier with advanced features
#[async_trait]
pub trait CryptoVerifierExt: super::CryptoVerifier {
    /// Verify multiple signatures in batch
    async fn verify_batch(
        &self, verifications: &[VerificationRequest],
    ) -> Result<Vec<VerificationResult>>;

    /// Create a detached signature
    fn sign_detached(&self, content: &[u8]) -> Result<Vec<u8>>;

    /// Verify a detached signature
    fn verify_detached(
        &self, content: &[u8], signature: &[u8], public_key: &PublicKey,
    ) -> Result<bool>;

    /// Sign with timestamp
    fn sign_with_timestamp(&self, content: &[u8]) -> Result<TimestampedSignature>;

    /// Verify signature with timestamp validation
    fn verify_with_timestamp(
        &self, content: &[u8], signature: &TimestampedSignature,
    ) -> Result<bool>;
}

/// Verification request for batch operations
#[derive(Debug, Clone)]
pub struct VerificationRequest {
    pub content: Vec<u8>,
    pub signature: Signature,
}

/// Signature with timestamp
#[derive(Debug, Clone)]
pub struct TimestampedSignature {
    pub signature: Signature,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub timestamp_signature: Vec<u8>,
}

/// Key management trait for secure key storage
#[async_trait]
pub trait KeyManager: Send + Sync {
    /// Store a key securely
    async fn store_key(&self, key: &KeyPair, name: &str) -> Result<()>;

    /// Retrieve a key by name
    async fn get_key(&self, name: &str) -> Result<KeyPair>;

    /// List all stored keys
    async fn list_keys(&self) -> Result<Vec<KeyInfo>>;

    /// Delete a key
    async fn delete_key(&self, name: &str) -> Result<()>;

    /// Rotate keys
    async fn rotate_key(&self, name: &str) -> Result<KeyPair>;

    /// Export key for backup
    async fn export_key(&self, name: &str, password: &str) -> Result<Vec<u8>>;

    /// Import key from backup
    async fn import_key(&self, name: &str, encrypted_key: &[u8], password: &str) -> Result<()>;
}

/// Key information
#[derive(Debug, Clone)]
pub struct KeyInfo {
    pub name: String,
    pub public_key: PublicKey,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub last_used: Option<chrono::DateTime<chrono::Utc>>,
}

/// Trust management for public keys
#[async_trait]
pub trait TrustManager: Send + Sync {
    /// Add a trusted public key
    async fn add_trusted_key(&self, key: &PublicKey, trust_level: TrustLevel) -> Result<()>;

    /// Remove a trusted key
    async fn remove_trusted_key(&self, fingerprint: &str) -> Result<()>;

    /// Check if a key is trusted
    async fn is_trusted(&self, key: &PublicKey) -> Result<bool>;

    /// Get trust level for a key
    async fn get_trust_level(&self, key: &PublicKey) -> Result<Option<TrustLevel>>;

    /// List all trusted keys
    async fn list_trusted_keys(&self) -> Result<Vec<TrustedKey>>;

    /// Verify chain of trust
    async fn verify_chain(&self, signatures: &[Signature]) -> Result<bool>;
}

/// Trust level for public keys
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TrustLevel {
    Untrusted,
    Marginal,
    Full,
    Ultimate,
}

/// Trusted key information
#[derive(Debug, Clone)]
pub struct TrustedKey {
    pub public_key: PublicKey,
    pub trust_level: TrustLevel,
    pub added_at: chrono::DateTime<chrono::Utc>,
    pub verified_by: Option<String>,
}
