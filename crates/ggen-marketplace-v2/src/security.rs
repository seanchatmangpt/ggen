//! Cryptographic security for package signing and verification
//!
//! Features:
//! - Ed25519 signature generation and verification
//! - SHA-256 checksums
//! - Public key management
//! - Signature receipts

use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use hex::{decode as hex_decode, encode as hex_encode};
use sha2::{Digest, Sha256};
use tracing::debug;

use crate::error::Result;
use crate::traits::Signable;

/// Cryptographic key pair
pub struct KeyPair {
    signing_key: SigningKey,
    verifying_key: VerifyingKey,
}

impl KeyPair {
    /// Generate a new key pair
    #[must_use]
    pub fn generate() -> Self {
        let secret_bytes: [u8; 32] = rand::random();
        let signing_key = SigningKey::from_bytes(&secret_bytes);
        let verifying_key = VerifyingKey::from(&signing_key);

        debug!("Generated new Ed25519 key pair");

        Self {
            signing_key,
            verifying_key,
        }
    }

    /// Get the public key as hex string
    #[must_use]
    pub fn public_key_hex(&self) -> String {
        hex_encode(self.verifying_key.to_bytes())
    }

    /// Load from private key hex
    ///
    /// # Errors
    ///
    /// * [`Error::CryptoError`] - When the hex string is invalid or the key is not 32 bytes
    #[must_use]
    pub fn from_secret_key(secret_key_hex: &str) -> Result<Self> {
        let secret_bytes = hex_decode(secret_key_hex)
            .map_err(|e| crate::error::Error::CryptoError(format!("Invalid hex: {}", e)))?;

        if secret_bytes.len() != 32 {
            return Err(crate::error::Error::CryptoError(
                "Secret key must be 32 bytes".to_string(),
            ));
        }

        let mut secret_key_array = [0u8; 32];
        secret_key_array.copy_from_slice(&secret_bytes);

        let signing_key = SigningKey::from_bytes(&secret_key_array);
        let verifying_key = VerifyingKey::from(&signing_key);

        Ok(Self {
            signing_key,
            verifying_key,
        })
    }

    /// Get the secret key as hex string (PRIVATE - handle with care)
    #[must_use]
    pub fn secret_key_hex(&self) -> String {
        hex_encode(self.signing_key.to_bytes())
    }
}

/// Signature verifier
pub struct SignatureVerifier {
    key_pair: KeyPair,
}

impl SignatureVerifier {
    /// Create a new verifier with a key pair
    #[must_use]
    pub fn new(key_pair: KeyPair) -> Self {
        Self { key_pair }
    }

    /// Create a verifier from public key only (for verification)
    ///
    /// # Errors
    ///
    /// * [`Error::CryptoError`] - When the hex string is invalid, the key is not 32 bytes, or the key is not a valid Ed25519 public key
    #[must_use]
    pub fn from_public_key(public_key_hex: &str) -> Result<Self> {
        let pub_bytes = hex_decode(public_key_hex)
            .map_err(|e| crate::error::Error::CryptoError(format!("Invalid hex: {}", e)))?;

        if pub_bytes.len() != 32 {
            return Err(crate::error::Error::CryptoError(
                "Public key must be 32 bytes".to_string(),
            ));
        }

        let mut pub_key_array = [0u8; 32];
        pub_key_array.copy_from_slice(&pub_bytes);

        let verifying_key = VerifyingKey::from_bytes(&pub_key_array)
            .map_err(|e| crate::error::Error::CryptoError(format!("Invalid public key: {}", e)))?;

        // Create a dummy signing key (only for signature verification)
        let signing_key = SigningKey::from_bytes(&[0u8; 32]);

        Ok(Self {
            key_pair: KeyPair {
                signing_key,
                verifying_key,
            },
        })
    }

    /// Verify a signature
    ///
    /// # Errors
    ///
    /// * [`Error::SignatureVerificationFailed`] - When the signature hex is invalid or not 64 bytes
    #[must_use]
    pub fn verify_signature(&self, data: &[u8], signature_hex: &str) -> Result<bool> {
        let sig_bytes = hex_decode(signature_hex).map_err(|e| {
            crate::error::Error::SignatureVerificationFailed {
                reason: format!("Invalid hex: {}", e),
            }
        })?;

        if sig_bytes.len() != 64 {
            return Err(crate::error::Error::SignatureVerificationFailed {
                reason: "Signature must be 64 bytes".to_string(),
            });
        }

        let mut sig_array = [0u8; 64];
        sig_array.copy_from_slice(&sig_bytes);

        let signature = Signature::from_bytes(&sig_array);

        match self.key_pair.verifying_key.verify(data, &signature) {
            Ok(_) => {
                debug!("Signature verified successfully");
                Ok(true)
            }
            Err(_) => {
                debug!("Signature verification failed");
                Ok(false)
            }
        }
    }

    /// Get the public key
    #[must_use]
    pub fn public_key(&self) -> String {
        self.key_pair.public_key_hex()
    }
}

impl Signable for SignatureVerifier {
    #[must_use]
    fn sign(&self, data: &[u8]) -> Result<String> {
        let signature = self.key_pair.signing_key.sign(data);
        Ok(hex_encode(signature.to_bytes()))
    }

    #[must_use]
    fn verify(&self, data: &[u8], signature: &str) -> Result<bool> {
        self.verify_signature(data, signature)
    }

    #[must_use]
    fn public_key(&self) -> String {
        self.key_pair.public_key_hex()
    }
}

/// Checksum calculator (SHA-256)
pub struct ChecksumCalculator;

impl ChecksumCalculator {
    /// Calculate SHA-256 checksum
    #[must_use]
    pub fn calculate(data: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data);
        hex_encode(hasher.finalize())
    }

    /// Verify checksum
    ///
    /// # Errors
    ///
    /// This function currently always returns `Ok`. It compares the calculated SHA-256 checksum with the expected value.
    #[must_use]
    pub fn verify(data: &[u8], expected: &str) -> Result<bool> {
        let calculated = Self::calculate(data);
        Ok(calculated == expected)
    }
}

/// Package signature receipt
#[derive(Clone, Debug)]
pub struct SignatureReceipt {
    /// Package name and version
    pub package_identifier: String,
    /// Signature
    pub signature: String,
    /// Public key used
    pub public_key: String,
    /// When signed
    pub signed_at: chrono::DateTime<chrono::Utc>,
    /// Data checksum
    pub data_checksum: String,
}

impl std::fmt::Display for SignatureReceipt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Signature Receipt")?;
        writeln!(f, "Package: {}", self.package_identifier)?;
        writeln!(f, "Signed at: {}", self.signed_at)?;
        writeln!(f, "Public key: {}", &self.public_key[..16])?; // Show first 16 chars
        writeln!(f, "Data checksum: {}", &self.data_checksum[..16])?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_key_pair_generation() {
        let key_pair = KeyPair::generate();
        let pub_key = key_pair.public_key_hex();

        assert!(!pub_key.is_empty());
        assert_eq!(pub_key.len(), 64); // 32 bytes in hex = 64 chars
    }

    #[test]
    fn test_signing_and_verification() {
        let key_pair = KeyPair::generate();
        let verifier = SignatureVerifier::new(key_pair);

        let data = b"test data";
        let signature = verifier.sign(data).unwrap();

        let verified = verifier.verify(data, &signature).unwrap();
        assert!(verified);
    }

    #[test]
    fn test_signature_verification_fails_with_wrong_data() {
        let key_pair = KeyPair::generate();
        let verifier = SignatureVerifier::new(key_pair);

        let data = b"test data";
        let signature = verifier.sign(data).unwrap();

        let wrong_data = b"different data";
        let verified = verifier.verify(wrong_data, &signature).unwrap();
        assert!(!verified);
    }

    #[test]
    fn test_checksum_calculation() {
        let data = b"test data";
        let checksum = ChecksumCalculator::calculate(data);

        assert!(!checksum.is_empty());
        assert_eq!(checksum.len(), 64); // SHA-256 in hex = 64 chars
    }

    #[test]
    fn test_checksum_verification() {
        let data = b"test data";
        let checksum = ChecksumCalculator::calculate(data);

        let verified = ChecksumCalculator::verify(data, &checksum).unwrap();
        assert!(verified);

        let wrong_checksum = "0".repeat(64);
        let verified = ChecksumCalculator::verify(data, &wrong_checksum).unwrap();
        assert!(!verified);
    }

    #[test]
    fn test_key_serialization() {
        let key_pair1 = KeyPair::generate();
        let secret_hex = key_pair1.secret_key_hex();

        let key_pair2 = KeyPair::from_secret_key(&secret_hex).unwrap();

        assert_eq!(key_pair1.public_key_hex(), key_pair2.public_key_hex());
    }
}
