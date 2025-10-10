///! Post-Quantum Cryptography (PQC) module for ggen
///!
///! Uses ML-DSA (Dilithium3) - NIST-approved post-quantum signature scheme
///! Provides quantum-resistant signatures for lockfile integrity verification
use anyhow::{Context, Result};
use base64::{engine::general_purpose, Engine as _};
use pqcrypto_dilithium::dilithium3;
use pqcrypto_traits::sign::{PublicKey, SecretKey, SignedMessage};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::Path;

/// PQC signer for creating quantum-resistant signatures
pub struct PqcSigner {
    secret_key: dilithium3::SecretKey,
    public_key: dilithium3::PublicKey,
}

impl PqcSigner {
    /// Generate a new keypair
    pub fn new() -> Self {
        let (public_key, secret_key) = dilithium3::keypair();
        Self {
            secret_key,
            public_key,
        }
    }

    /// Load keypair from files
    pub fn from_files(secret_key_path: &Path, public_key_path: &Path) -> Result<Self> {
        let sk_bytes = fs::read(secret_key_path).context("Failed to read secret key")?;
        let pk_bytes = fs::read(public_key_path).context("Failed to read public key")?;

        let secret_key = dilithium3::SecretKey::from_bytes(&sk_bytes)
            .map_err(|_| anyhow::anyhow!("Invalid secret key format"))?;
        let public_key = dilithium3::PublicKey::from_bytes(&pk_bytes)
            .map_err(|_| anyhow::anyhow!("Invalid public key format"))?;

        Ok(Self {
            secret_key,
            public_key,
        })
    }

    /// Save keypair to files
    pub fn save_to_files(&self, secret_key_path: &Path, public_key_path: &Path) -> Result<()> {
        fs::write(secret_key_path, self.secret_key.as_bytes())
            .context("Failed to write secret key")?;
        fs::write(public_key_path, self.public_key.as_bytes())
            .context("Failed to write public key")?;
        Ok(())
    }

    /// Sign a message (pack content hash)
    pub fn sign(&self, message: &[u8]) -> Vec<u8> {
        let signed = dilithium3::sign(message, &self.secret_key);
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
    public_key: dilithium3::PublicKey,
}

impl PqcVerifier {
    /// Create verifier from public key bytes
    pub fn from_public_key(public_key_bytes: &[u8]) -> Result<Self> {
        let public_key = dilithium3::PublicKey::from_bytes(public_key_bytes)
            .map_err(|_| anyhow::anyhow!("Invalid public key format"))?;
        Ok(Self { public_key })
    }

    /// Create verifier from base64-encoded public key
    pub fn from_base64(public_key_b64: &str) -> Result<Self> {
        let public_key_bytes = general_purpose::STANDARD
            .decode(public_key_b64)
            .context("Failed to decode public key")?;
        Self::from_public_key(&public_key_bytes)
    }

    /// Verify a signature
    pub fn verify(&self, message: &[u8], signature: &[u8]) -> Result<bool> {
        match dilithium3::open(
            &SignedMessage::from_bytes(signature).unwrap(),
            &self.public_key,
        ) {
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
            .context("Failed to decode signature")?;
        self.verify(message.as_bytes(), &signature)
    }

    /// Get public key as base64
    pub fn public_key_base64(&self) -> String {
        general_purpose::STANDARD.encode(self.public_key.as_bytes())
    }
}

/// Calculate SHA256 hash of file content
pub fn calculate_sha256_file(path: &Path) -> Result<String> {
    let content = fs::read(path).context("Failed to read file")?;
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

    #[test]
    fn test_pqc_signer_creation() {
        let signer = PqcSigner::new();
        assert!(!signer.public_key_base64().is_empty());
        assert!(!signer.secret_key_base64().is_empty());
    }

    #[test]
    fn test_pqc_sign_and_verify() {
        let signer = PqcSigner::new();
        let message = b"test message";

        let signature = signer.sign(message);
        assert!(!signature.is_empty());

        let verifier = PqcVerifier::from_public_key(signer.public_key.as_bytes()).unwrap();
        let verified = verifier.verify(message, &signature).unwrap();
        assert!(verified);
    }

    #[test]
    fn test_pqc_pack_signature() {
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
    }

    #[test]
    fn test_pqc_invalid_signature() {
        let signer = PqcSigner::new();
        let verifier = PqcVerifier::from_public_key(signer.public_key.as_bytes()).unwrap();

        let message = b"test message";
        let signature = signer.sign(message);

        // Tamper with message
        let tampered_message = b"tampered message";
        let verified = verifier.verify(tampered_message, &signature).unwrap();
        assert!(!verified);
    }

    #[test]
    fn test_sha256_calculation() {
        let data = b"test data";
        let hash = calculate_sha256(data);
        // SHA256 of "test data"
        assert_eq!(
            hash,
            "916f0027a575074ce72a331777c3478d6513f786a591bd892da1a577bf2335f9"
        );
    }
}
