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
    pub fn public_key_hex(&self) -> String {
        hex_encode(self.verifying_key.to_bytes())
    }

    /// Load from private key hex
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
    pub fn new(key_pair: KeyPair) -> Self {
        Self { key_pair }
    }

    /// Create a verifier from public key only (for verification)
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
    pub fn public_key(&self) -> String {
        self.key_pair.public_key_hex()
    }
}

impl Signable for SignatureVerifier {
    fn sign(&self, data: &[u8]) -> Result<String> {
        let signature = self.key_pair.signing_key.sign(data);
        Ok(hex_encode(signature.to_bytes()))
    }

    fn verify(&self, data: &[u8], signature: &str) -> Result<bool> {
        self.verify_signature(data, signature)
    }

    fn public_key(&self) -> String {
        self.key_pair.public_key_hex()
    }
}

/// Checksum calculator (SHA-256)
pub struct ChecksumCalculator;

impl ChecksumCalculator {
    /// Calculate SHA-256 checksum
    pub fn calculate(data: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data);
        hex_encode(hasher.finalize())
    }

    /// Verify checksum
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
        // Safe truncation - show first 16 chars or entire string if shorter
        let pub_key_preview = if self.public_key.len() > 16 {
            &self.public_key[..16]
        } else {
            &self.public_key
        };
        writeln!(f, "Public key: {}", pub_key_preview)?;

        let checksum_preview = if self.data_checksum.len() > 16 {
            &self.data_checksum[..16]
        } else {
            &self.data_checksum
        };
        writeln!(f, "Data checksum: {}", checksum_preview)?;
        Ok(())
    }
}

/// Result of integrity validation
#[derive(Clone, Debug)]
pub struct IntegrityValidationResult {
    /// Whether the validation passed
    pub is_valid: bool,
    /// Package identifier
    pub package_identifier: String,
    /// Checksum verification result
    pub checksum_valid: bool,
    /// Signature verification result
    pub signature_valid: Option<bool>,
    /// Validation timestamp
    pub validated_at: chrono::DateTime<chrono::Utc>,
    /// Any error messages
    pub errors: Vec<String>,
}

impl IntegrityValidationResult {
    /// Create a successful validation result
    pub fn success(
        package_identifier: String, checksum_valid: bool, signature_valid: Option<bool>,
    ) -> Self {
        Self {
            is_valid: checksum_valid && signature_valid.unwrap_or(true),
            package_identifier,
            checksum_valid,
            signature_valid,
            validated_at: chrono::Utc::now(),
            errors: Vec::new(),
        }
    }

    /// Create a failed validation result
    pub fn failure(package_identifier: String, errors: Vec<String>) -> Self {
        Self {
            is_valid: false,
            package_identifier,
            checksum_valid: false,
            signature_valid: None,
            validated_at: chrono::Utc::now(),
            errors,
        }
    }
}

impl std::fmt::Display for IntegrityValidationResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Integrity Validation Result")?;
        writeln!(f, "Package: {}", self.package_identifier)?;
        writeln!(f, "Valid: {}", self.is_valid)?;
        writeln!(
            f,
            "Checksum: {}",
            if self.checksum_valid { "PASS" } else { "FAIL" }
        )?;
        if let Some(sig_valid) = self.signature_valid {
            writeln!(f, "Signature: {}", if sig_valid { "PASS" } else { "FAIL" })?;
        }
        if !self.errors.is_empty() {
            writeln!(f, "Errors:")?;
            for err in &self.errors {
                writeln!(f, "  - {}", err)?;
            }
        }
        Ok(())
    }
}

/// Comprehensive integrity validator for packages
pub struct IntegrityValidator {
    /// Optional signature verifier for signed packages
    verifier: Option<SignatureVerifier>,
}

impl IntegrityValidator {
    /// Create a new validator without signature verification
    pub fn new() -> Self {
        Self { verifier: None }
    }

    /// Create a new validator with signature verification
    pub fn with_verifier(verifier: SignatureVerifier) -> Self {
        Self {
            verifier: Some(verifier),
        }
    }

    /// Validate package integrity (checksum only)
    pub fn validate_checksum(
        &self, package_identifier: &str, data: &[u8], expected_checksum: &str,
    ) -> IntegrityValidationResult {
        let checksum_valid = match ChecksumCalculator::verify(data, expected_checksum) {
            Ok(valid) => valid,
            Err(e) => {
                return IntegrityValidationResult::failure(
                    package_identifier.to_string(),
                    vec![format!("Checksum verification error: {}", e)],
                );
            }
        };

        if checksum_valid {
            debug!("Checksum validation passed for {}", package_identifier);
        } else {
            debug!("Checksum validation failed for {}", package_identifier);
        }

        IntegrityValidationResult::success(package_identifier.to_string(), checksum_valid, None)
    }

    /// Validate package integrity (checksum and signature)
    pub fn validate_full(
        &self, package_identifier: &str, data: &[u8], expected_checksum: &str,
        signature: Option<&str>,
    ) -> IntegrityValidationResult {
        // First verify checksum
        let checksum_valid = match ChecksumCalculator::verify(data, expected_checksum) {
            Ok(valid) => valid,
            Err(e) => {
                return IntegrityValidationResult::failure(
                    package_identifier.to_string(),
                    vec![format!("Checksum verification error: {}", e)],
                );
            }
        };

        // Then verify signature if provided and verifier is available
        let signature_valid = match (&self.verifier, signature) {
            (Some(verifier), Some(sig)) => match verifier.verify_signature(data, sig) {
                Ok(valid) => Some(valid),
                Err(e) => {
                    return IntegrityValidationResult::failure(
                        package_identifier.to_string(),
                        vec![format!("Signature verification error: {}", e)],
                    );
                }
            },
            (None, Some(_)) => {
                return IntegrityValidationResult::failure(
                    package_identifier.to_string(),
                    vec!["Signature provided but no verifier configured".to_string()],
                );
            }
            _ => None,
        };

        let is_valid = checksum_valid && signature_valid.unwrap_or(true);

        if is_valid {
            debug!(
                "Full integrity validation passed for {}",
                package_identifier
            );
        } else {
            debug!(
                "Full integrity validation failed for {}",
                package_identifier
            );
        }

        IntegrityValidationResult {
            is_valid,
            package_identifier: package_identifier.to_string(),
            checksum_valid,
            signature_valid,
            validated_at: chrono::Utc::now(),
            errors: Vec::new(),
        }
    }

    /// Validate multiple packages in batch
    pub fn validate_batch(
        &self, packages: &[(String, Vec<u8>, String, Option<String>)],
    ) -> Vec<IntegrityValidationResult> {
        packages
            .iter()
            .map(|(id, data, checksum, sig)| self.validate_full(id, data, checksum, sig.as_deref()))
            .collect()
    }
}

impl Default for IntegrityValidator {
    fn default() -> Self {
        Self::new()
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

    #[test]
    fn test_integrity_validator_checksum_only() {
        let validator = IntegrityValidator::new();
        let data = b"test package data";
        let checksum = ChecksumCalculator::calculate(data);

        let result = validator.validate_checksum("test-pkg@1.0.0", data, &checksum);
        assert!(result.is_valid);
        assert!(result.checksum_valid);
        assert!(result.signature_valid.is_none());
    }

    #[test]
    fn test_integrity_validator_checksum_failure() {
        let validator = IntegrityValidator::new();
        let data = b"test package data";
        let wrong_checksum = "0".repeat(64);

        let result = validator.validate_checksum("test-pkg@1.0.0", data, &wrong_checksum);
        assert!(!result.is_valid);
        assert!(!result.checksum_valid);
    }

    #[test]
    fn test_integrity_validator_full_with_signature() {
        let key_pair = KeyPair::generate();
        let verifier = SignatureVerifier::new(key_pair);
        let validator = IntegrityValidator::with_verifier(verifier);

        let data = b"test package data";
        let checksum = ChecksumCalculator::calculate(data);
        let signature = validator.verifier.as_ref().unwrap().sign(data).unwrap();

        let result = validator.validate_full("test-pkg@1.0.0", data, &checksum, Some(&signature));
        assert!(result.is_valid);
        assert!(result.checksum_valid);
        assert_eq!(result.signature_valid, Some(true));
    }

    #[test]
    fn test_integrity_validator_batch() {
        let validator = IntegrityValidator::new();

        let data1 = b"package one";
        let checksum1 = ChecksumCalculator::calculate(data1);

        let data2 = b"package two";
        let checksum2 = ChecksumCalculator::calculate(data2);

        let packages = vec![
            ("pkg1@1.0.0".to_string(), data1.to_vec(), checksum1, None),
            ("pkg2@1.0.0".to_string(), data2.to_vec(), checksum2, None),
        ];

        let results = validator.validate_batch(&packages);
        assert_eq!(results.len(), 2);
        assert!(results.iter().all(|r| r.is_valid));
    }

    #[test]
    fn test_integrity_validation_result_display() {
        let result =
            IntegrityValidationResult::success("test-pkg@1.0.0".to_string(), true, Some(true));
        let display = format!("{}", result);
        assert!(display.contains("test-pkg@1.0.0"));
        assert!(display.contains("PASS"));
    }
}
