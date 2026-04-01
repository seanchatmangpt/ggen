//! Marketplace cryptographic security using ggen-receipt.
//!
//! This module provides marketplace-specific wrappers around the canonical
//! ggen-receipt implementation for pack signing and verification.

use crate::error::Result;
use crate::traits::Signable;
use chrono::{DateTime, Utc};
use ed25519_dalek::{Signer, Verifier, SigningKey, VerifyingKey};
use ggen_receipt::hash_data;
use tracing::{debug, instrument};

/// Re-export the canonical keypair generation from ggen-receipt.
pub use ggen_receipt::generate_keypair as generate_marketplace_keypair;

/// Marketplace-specific signature wrapper.
///
/// Wraps ggen-receipt's signing functionality for pack operations.
#[derive(Debug, Clone)]
pub struct MarketplaceSignature {
    /// Ed25519 signature as hex string
    pub signature: String,
    /// Public key as hex string
    pub public_key: String,
    /// Data checksum (SHA-256)
    pub checksum: String,
}

impl MarketplaceSignature {
    /// Create a new marketplace signature by signing data.
    ///
    /// # Errors
    ///
    /// Returns error if signing fails.
    #[instrument(
        name = "marketplace.sign",
        skip(signing_key, data),
        fields(
            operation.name = "sign",
            operation.type = "marketplace",
            data_length = data.len(),
            checksum = tracing::field::Empty,
            duration_ms
        )
    )]
    pub fn sign(signing_key: &SigningKey, data: &[u8]) -> Result<Self> {
        let start = std::time::Instant::now();
        let signature = signing_key.sign(data);
        let signature_hex = hex::encode(signature.to_bytes());
        let checksum = hash_data(data);
        let public_key = hex::encode(signing_key.verifying_key().to_bytes());

        debug!("Created marketplace signature for {} bytes of data", data.len());

        let duration = start.elapsed();
        tracing::Span::current().record("checksum", &checksum.as_str());
        tracing::Span::current().record("duration_ms", duration.as_millis());

        Ok(Self {
            signature: signature_hex,
            public_key,
            checksum,
        })
    }

    /// Get the signature hex string.
    #[must_use]
    pub fn as_hex(&self) -> &str {
        &self.signature
    }

    /// Get the checksum.
    #[must_use]
    pub fn checksum(&self) -> &str {
        &self.checksum
    }

    /// Get the public key.
    #[must_use]
    pub fn public_key(&self) -> &str {
        &self.public_key
    }
}

/// Marketplace verifier using ggen-receipt.
///
/// Provides verification functionality for pack signatures.
#[derive(Debug, Clone)]
pub struct MarketplaceVerifier {
    /// Verifying key
    verifying_key: VerifyingKey,
}

impl MarketplaceVerifier {
    /// Create a new verifier from a verifying key.
    #[must_use]
    pub fn new(verifying_key: VerifyingKey) -> Self {
        Self { verifying_key }
    }

    /// Create a verifier from a public key hex string.
    ///
    /// # Errors
    ///
    /// Returns error if the public key hex is invalid.
    pub fn from_public_key_hex(public_key_hex: &str) -> Result<Self> {
        let key_bytes = hex::decode(public_key_hex)
            .map_err(|e| crate::error::Error::CryptoError(format!("Invalid public key hex: {}", e)))?;

        if key_bytes.len() != 32 {
            return Err(crate::error::Error::CryptoError(
                "Public key must be 32 bytes".to_string(),
            ));
        }

        let mut key_array = [0u8; 32];
        key_array.copy_from_slice(&key_bytes);

        let verifying_key = VerifyingKey::from_bytes(&key_array)
            .map_err(|e| crate::error::Error::CryptoError(format!("Invalid verifying key: {}", e)))?;

        Ok(Self { verifying_key })
    }

    /// Verify a marketplace signature.
    ///
    /// # Errors
    ///
    /// Returns error if verification fails.
    #[instrument(
        name = "marketplace.verify_signature",
        skip(self, data, signature),
        fields(
            operation.name = "verify_signature",
            operation.type = "marketplace",
            key_id = tracing::field::Empty,
            signature_valid = tracing::field::Empty,
            data_length = data.len(),
            duration_ms
        )
    )]
    pub fn verify(&self, data: &[u8], signature: &MarketplaceSignature) -> Result<bool> {
        let start = std::time::Instant::now();

        // First verify the signature
        let sig_bytes = hex::decode(&signature.signature)
            .map_err(|e| crate::error::Error::SignatureVerificationFailed {
                reason: format!("Invalid signature hex: {}", e),
            })?;

        use ed25519_dalek::Signature;
        let ed_sig = Signature::from_slice(&sig_bytes)
            .map_err(|_| crate::error::Error::SignatureVerificationFailed {
                reason: "Invalid signature format".to_string(),
            })?;

        let verified = self.verifying_key.verify(data, &ed_sig).is_ok();

        let duration = start.elapsed();

        // Record OTEL span attributes
        tracing::Span::current().record("key_id", &self.public_key_hex()[..8.min(self.public_key_hex().len())]);
        tracing::Span::current().record("signature_valid", verified);
        tracing::Span::current().record("duration_ms", duration.as_millis());

        if verified {
            debug!("Marketplace signature verified successfully");
        } else {
            debug!("Marketplace signature verification failed");
        }

        Ok(verified)
    }

    /// Get the public key as hex string.
    #[must_use]
    pub fn public_key_hex(&self) -> String {
        hex::encode(self.verifying_key.to_bytes())
    }
}

impl Signable for MarketplaceVerifier {
    fn sign(&self, _data: &[u8]) -> Result<String> {
        // Note: This verifier cannot sign (only has verifying key)
        // For signing, use MarketplaceSignature::sign() with a SigningKey
        Err(crate::error::Error::CryptoError(
            "MarketplaceVerifier cannot sign - use MarketplaceSignature with SigningKey".to_string(),
        ))
    }

    fn verify(&self, data: &[u8], signature: &str) -> Result<bool> {
        let sig_bytes = hex::decode(signature)
            .map_err(|e| crate::error::Error::SignatureVerificationFailed {
                reason: format!("Invalid signature hex: {}", e),
            })?;

        use ed25519_dalek::Signature;
        let ed_sig = Signature::from_slice(&sig_bytes)
            .map_err(|_| crate::error::Error::SignatureVerificationFailed {
                reason: "Invalid signature format".to_string(),
            })?;

        Ok(self.verifying_key.verify(data, &ed_sig).is_ok())
    }

    fn public_key(&self) -> String {
        self.public_key_hex()
    }
}

/// Checksum calculator (SHA-256).
///
/// Kept for backward compatibility. Consider using ggen_receipt::hash_data directly.
pub struct ChecksumCalculator;

impl ChecksumCalculator {
    /// Calculate SHA-256 checksum.
    #[must_use]
    pub fn calculate(data: &[u8]) -> String {
        hash_data(data)
    }

    /// Verify checksum.
    ///
    /// # Errors
    ///
    /// This function always returns `Ok`. It compares the calculated SHA-256 checksum with the expected value.
    #[instrument(
        name = "marketplace.verify_digest",
        skip(data, expected),
        fields(
            operation.name = "verify_digest",
            operation.type = "marketplace",
            algorithm = "SHA-256",
            digest_valid = tracing::field::Empty,
            data_length = data.len(),
            duration_ms
        )
    )]
    #[must_use]
    pub fn verify(data: &[u8], expected: &str) -> Result<bool> {
        let start = std::time::Instant::now();
        let calculated = Self::calculate(data);
        let valid = calculated == expected;

        let duration = start.elapsed();
        tracing::Span::current().record("digest_valid", valid);
        tracing::Span::current().record("duration_ms", duration.as_millis());

        Ok(valid)
    }
}

/// Package signature receipt.
///
/// Records the signature information for a package operation.
#[derive(Clone, Debug)]
pub struct SignatureReceipt {
    /// Package name and version
    pub package_identifier: String,
    /// Signature
    pub signature: String,
    /// Public key used
    pub public_key: String,
    /// When signed
    pub signed_at: DateTime<Utc>,
    /// Data checksum
    pub data_checksum: String,
}

impl std::fmt::Display for SignatureReceipt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Signature Receipt")?;
        writeln!(f, "Package: {}", self.package_identifier)?;
        writeln!(f, "Signed at: {}", self.signed_at)?;
        writeln!(f, "Public key: {}...", &self.public_key[..16.min(self.public_key.len())])?;
        writeln!(f, "Data checksum: {}...", &self.data_checksum[..16.min(self.data_checksum.len())])?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marketplace_signature_sign_and_verify() {
        let (signing_key, verifying_key) = generate_marketplace_keypair();

        let data = b"test pack data";
        let signature = MarketplaceSignature::sign(&signing_key, data).unwrap();

        let verifier = MarketplaceVerifier::new(verifying_key);
        let verified = verifier.verify(data, &signature).unwrap();

        assert!(verified);
    }

    #[test]
    fn test_marketplace_signature_checksum() {
        let (signing_key, _) = generate_marketplace_keypair();

        let data = b"test pack data";
        let signature = MarketplaceSignature::sign(&signing_key, data).unwrap();

        // Checksum should match ggen_receipt::hash_data
        let expected_checksum = hash_data(data);
        assert_eq!(signature.checksum(), expected_checksum);
    }

    #[test]
    fn test_marketplace_verifier_from_public_key_hex() {
        let (signing_key, verifying_key) = generate_marketplace_keypair();
        let public_key_hex = hex::encode(verifying_key.to_bytes());

        let verifier = MarketplaceVerifier::from_public_key_hex(&public_key_hex);
        assert!(verifier.is_ok());

        let verifier = verifier.unwrap();
        assert_eq!(verifier.public_key_hex(), public_key_hex);
    }

    #[test]
    fn test_marketplace_verifier_fails_with_wrong_signature() {
        let (signing_key, verifying_key) = generate_marketplace_keypair();

        let data = b"test pack data";
        let signature = MarketplaceSignature::sign(&signing_key, data).unwrap();

        // Modify the data
        let wrong_data = b"different pack data";

        let verifier = MarketplaceVerifier::new(verifying_key);
        let verified = verifier.verify(wrong_data, &signature).unwrap();

        assert!(!verified);
    }

    #[test]
    fn test_checksum_calculator() {
        let data = b"test pack data";
        let checksum = ChecksumCalculator::calculate(data);

        assert_eq!(checksum.len(), 64); // SHA-256 in hex
        assert!(!checksum.is_empty());
    }

    #[test]
    fn test_checksum_verification() {
        let data = b"test pack data";
        let checksum = ChecksumCalculator::calculate(data);

        let verified = ChecksumCalculator::verify(data, &checksum).unwrap();
        assert!(verified);

        let wrong_checksum = "0".repeat(64);
        let verified = ChecksumCalculator::verify(data, &wrong_checksum).unwrap();
        assert!(!verified);
    }

    #[test]
    fn test_signature_receipt_display() {
        let receipt = SignatureReceipt {
            package_identifier: "test-pack-1.0.0".to_string(),
            signature: "abc123...".to_string(),
            public_key: "def456...".to_string(),
            signed_at: Utc::now(),
            data_checksum: "789xyz...".to_string(),
        };

        let display = format!("{}", receipt);
        assert!(display.contains("Signature Receipt"));
        assert!(display.contains("test-pack-1.0.0"));
    }
}
