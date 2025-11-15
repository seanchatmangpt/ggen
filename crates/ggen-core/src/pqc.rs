//! Post-Quantum Cryptography (PQC) module for ggen
//!
//! This module provides post-quantum cryptographic signatures using ML-DSA (Dilithium3),
//! a NIST-approved post-quantum signature scheme. It enables quantum-resistant
//! signatures for lockfile integrity verification and package authentication.
//!
//! ## Features
//!
//! - **ML-DSA (Dilithium3)**: NIST-approved post-quantum signature algorithm
//! - **Key Management**: Generate, load, and save key pairs
//! - **Content Signing**: Sign arbitrary content with quantum-resistant signatures
//! - **Signature Verification**: Verify signatures against content and public keys
//! - **SHA-256 Integration**: Content hashing for integrity verification
//!
//! ## Security
//!
//! ML-DSA (Dilithium3) provides:
//! - **Quantum Resistance**: Secure against attacks from quantum computers
//! - **NIST Standard**: Approved for post-quantum cryptography
//! - **Strong Security**: 128-bit security level
//! - **Efficient**: Fast signature generation and verification
//!
//! ## Examples
//!
//! ### Generating a Key Pair
//!
//! ```rust
//! use ggen_core::pqc::PqcSigner;
//!
//! // Generate a new keypair (in-memory, no files needed)
//! let signer = PqcSigner::new();
//! let public_key_b64 = signer.public_key_base64();
//! assert!(!public_key_b64.is_empty());
//! ```
//!
//! ### Signing Content
//!
//! ```rust
//! use ggen_core::pqc::{PqcSigner, PqcVerifier};
//!
//! let signer = PqcSigner::new();
//! let content = b"package content";
//! let signature = signer.sign(content);
//!
//! // Verify signature - decode base64 public key to get bytes
//! use base64::{engine::general_purpose, Engine as _};
//! let public_key_b64 = signer.public_key_base64();
//! let public_key_bytes = general_purpose::STANDARD.decode(&public_key_b64).unwrap();
//! let verifier = PqcVerifier::from_public_key(&public_key_bytes).unwrap();
//! assert!(verifier.verify(content, &signature).unwrap());
//! ```
use base64::{engine::general_purpose, Engine as _};
use ggen_utils::error::{Error, Result};
use pqcrypto_mldsa::mldsa65;
use pqcrypto_traits::sign::{PublicKey, SecretKey, SignedMessage};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::Path;

/// PQC signer for creating quantum-resistant signatures
pub struct PqcSigner {
    secret_key: mldsa65::SecretKey,
    public_key: mldsa65::PublicKey,
}

impl PqcSigner {
    /// Generate a new keypair
    pub fn new() -> Self {
        let (public_key, secret_key) = mldsa65::keypair();
        Self {
            secret_key,
            public_key,
        }
    }

    /// Load keypair from files
    pub fn from_files(secret_key_path: &Path, public_key_path: &Path) -> Result<Self> {
        let sk_bytes = fs::read(secret_key_path)
            .map_err(|e| Error::with_context("Failed to read secret key", &e.to_string()))?;
        let pk_bytes = fs::read(public_key_path)
            .map_err(|e| Error::with_context("Failed to read public key", &e.to_string()))?;

        let secret_key = mldsa65::SecretKey::from_bytes(&sk_bytes)
            .map_err(|_| Error::new("Invalid secret key format"))?;
        let public_key = mldsa65::PublicKey::from_bytes(&pk_bytes)
            .map_err(|_| Error::new("Invalid public key format"))?;

        Ok(Self {
            secret_key,
            public_key,
        })
    }

    /// Save keypair to files
    pub fn save_to_files(&self, secret_key_path: &Path, public_key_path: &Path) -> Result<()> {
        fs::write(secret_key_path, self.secret_key.as_bytes())
            .map_err(|e| Error::with_context("Failed to write secret key", &e.to_string()))?;
        fs::write(public_key_path, self.public_key.as_bytes())
            .map_err(|e| Error::with_context("Failed to write public key", &e.to_string()))?;
        Ok(())
    }

    /// Sign a message (pack content hash)
    pub fn sign(&self, message: &[u8]) -> Vec<u8> {
        let signed = mldsa65::sign(message, &self.secret_key);
        signed.as_bytes().to_vec()
    }

    /// Sign a pack file by its SHA256 hash
    pub fn sign_pack(&self, pack_id: &str, version: &str, sha256: &str) -> String {
        let message = format!("{}:{}:{}", pack_id, version, sha256);
        let signature = self.sign(message.as_bytes());
        general_purpose::STANDARD.encode(&signature)
    }

    /// Get public key as base64
    pub fn public_key_base64(&self) -> String {
        general_purpose::STANDARD.encode(self.public_key.as_bytes())
    }

    /// Get secret key as base64 (for backup/export)
    pub fn secret_key_base64(&self) -> String {
        general_purpose::STANDARD.encode(self.secret_key.as_bytes())
    }
}

impl Default for PqcSigner {
    fn default() -> Self {
        Self::new()
    }
}

/// PQC verifier for checking quantum-resistant signatures
pub struct PqcVerifier {
    public_key: mldsa65::PublicKey,
}

impl PqcVerifier {
    /// Create verifier from public key bytes
    pub fn from_public_key(public_key_bytes: &[u8]) -> Result<Self> {
        let public_key = mldsa65::PublicKey::from_bytes(public_key_bytes)
            .map_err(|_| Error::new("Invalid public key format"))?;
        Ok(Self { public_key })
    }

    /// Create verifier from base64-encoded public key
    pub fn from_base64(public_key_b64: &str) -> Result<Self> {
        let public_key_bytes = general_purpose::STANDARD
            .decode(public_key_b64)
            .map_err(|e| Error::with_context("Failed to decode public key", &e.to_string()))?;
        Self::from_public_key(&public_key_bytes)
    }

    /// Verify a signature
    pub fn verify(&self, message: &[u8], signature: &[u8]) -> Result<bool> {
        let signed_message = SignedMessage::from_bytes(signature)
            .map_err(|_| Error::new("Invalid signature format"))?;

        match mldsa65::open(&signed_message, &self.public_key) {
            Ok(verified_msg) => Ok(verified_msg == message),
            Err(_) => Ok(false),
        }
    }

    /// Verify a pack signature
    pub fn verify_pack(
        &self, pack_id: &str, version: &str, sha256: &str, signature_b64: &str,
    ) -> Result<bool> {
        let message = format!("{}:{}:{}", pack_id, version, sha256);
        let signature = general_purpose::STANDARD
            .decode(signature_b64)
            .map_err(|e| Error::with_context("Failed to decode signature", &e.to_string()))?;
        self.verify(message.as_bytes(), &signature)
    }

    /// Get public key as base64
    pub fn public_key_base64(&self) -> String {
        general_purpose::STANDARD.encode(self.public_key.as_bytes())
    }
}

/// Calculate SHA256 hash of file content
pub fn calculate_sha256_file(path: &Path) -> Result<String> {
    let content =
        fs::read(path).map_err(|e| Error::with_context("Failed to read file", &e.to_string()))?;
    Ok(calculate_sha256(&content))
}

/// Calculate SHA256 hash of bytes
pub fn calculate_sha256(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;

    test!(test_pqc_signer_creation, {
        let signer = PqcSigner::new();
        assert!(!signer.public_key_base64().is_empty());
        assert!(!signer.secret_key_base64().is_empty());
    });

    test!(test_pqc_sign_and_verify, {
        let signer = PqcSigner::new();
        let message = b"test message";

        let signature = signer.sign(message);
        assert!(!signature.is_empty());

        let verifier = PqcVerifier::from_public_key(signer.public_key.as_bytes()).unwrap();
        let verified = verifier.verify(message, &signature).unwrap();
        assert!(verified);
    });

    test!(test_pqc_pack_signature, {
        let signer = PqcSigner::new();
        let pack_id = "io.ggen.test";
        let version = "1.0.0";
        let sha256 = "abc123";

        let signature = signer.sign_pack(pack_id, version, sha256);
        assert!(!signature.is_empty());

        let verifier = PqcVerifier::from_base64(&signer.public_key_base64()).unwrap();
        let verified = verifier
            .verify_pack(pack_id, version, sha256, &signature)
            .unwrap();
        assert!(verified);
    });

    test!(test_pqc_invalid_signature, {
        let signer = PqcSigner::new();
        let verifier = PqcVerifier::from_public_key(signer.public_key.as_bytes()).unwrap();

        let message = b"test message";
        let signature = signer.sign(message);

        // Tamper with message
        let tampered_message = b"tampered message";
        let verified = verifier.verify(tampered_message, &signature).unwrap();
        assert!(!verified);
    });

    test!(test_sha256_calculation, {
        let data = b"test data";
        let hash = calculate_sha256(data);
        // SHA256 of "test data"
        assert_eq!(
            hash,
            "916f0027a575074ce72a331777c3478d6513f786a591bd892da1a577bf2335f9"
        );
    });
}
