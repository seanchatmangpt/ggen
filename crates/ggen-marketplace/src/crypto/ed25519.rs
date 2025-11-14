#![allow(clippy::unwrap_used)] // Test code at end of file uses unwrap
use crate::error::{MarketplaceError, Result};
use crate::models::signature::{KeyPair, PublicKey, Signature, SignatureAlgorithm};
use crate::traits::CryptoVerifier;
use ed25519_dalek::{Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;

/// Ed25519 cryptographic verifier
///
/// Provides digital signature verification using the Ed25519 algorithm.
/// Ed25519 is a modern, high-performance elliptic curve signature scheme.
///
/// # Security
///
/// Ed25519 offers:
/// - 128-bit security level
/// - Fast signature verification (~70,000 verifications/second)
/// - Small signature size (64 bytes)
/// - Deterministic signatures (no random number generation)
///
/// # Example
///
/// ```no_run
/// use ggen_marketplace::crypto::Ed25519Verifier;
/// use ggen_marketplace::traits::CryptoVerifier;
///
/// let verifier = Ed25519Verifier::new();
/// let keypair = verifier.generate_keypair()?;
/// let content = b"Hello, World!";
/// let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
/// let signature = verifier_with_key.sign(content)?;
/// assert!(verifier_with_key.verify(content, &signature)?);
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub struct Ed25519Verifier {
    /// Optional keypair for signing operations
    keypair: Option<KeyPair>,
}

impl Ed25519Verifier {
    /// Create a new Ed25519 verifier without a keypair
    ///
    /// Use `with_keypair()` or `generate_keypair()` to enable signing operations.
    pub fn new() -> Self {
        Self { keypair: None }
    }

    /// Create a new Ed25519 verifier with an existing keypair
    ///
    /// # Arguments
    ///
    /// * `keypair` - The keypair to use for signing operations
    ///
    /// # Example
    ///
    /// ```no_run
    /// use ggen_marketplace::crypto::Ed25519Verifier;
    /// use ggen_marketplace::traits::CryptoVerifier;
    ///
    /// let verifier = Ed25519Verifier::new();
    /// let keypair = verifier.generate_keypair()?;
    /// let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn with_keypair(keypair: KeyPair) -> Self {
        Self {
            keypair: Some(keypair),
        }
    }

    /// Get the current keypair, if available
    pub fn keypair(&self) -> Option<&KeyPair> {
        self.keypair.as_ref()
    }

    /// Convert bytes to SigningKey with proper error handling
    fn bytes_to_signing_key(&self, private_key_bytes: &[u8]) -> Result<SigningKey> {
        if private_key_bytes.len() != 32 {
            return Err(MarketplaceError::verification_error(
                format!(
                    "Invalid private key length: expected 32 bytes, got {}",
                    private_key_bytes.len()
                ),
                "Ed25519 signing",
            ));
        }

        let mut key_array = [0u8; 32];
        key_array.copy_from_slice(private_key_bytes);

        Ok(SigningKey::from_bytes(&key_array))
    }

    /// Convert bytes to VerifyingKey with proper error handling
    fn bytes_to_verifying_key(&self, public_key_bytes: &[u8]) -> Result<VerifyingKey> {
        if public_key_bytes.len() != 32 {
            return Err(MarketplaceError::verification_error(
                format!(
                    "Invalid public key length: expected 32 bytes, got {}",
                    public_key_bytes.len()
                ),
                "Ed25519 verification",
            ));
        }

        let mut key_array = [0u8; 32];
        key_array.copy_from_slice(public_key_bytes);

        VerifyingKey::from_bytes(&key_array).map_err(|e| {
            MarketplaceError::verification_error(
                format!("Invalid public key: {}", e),
                "Ed25519 verification",
            )
        })
    }
}

impl Default for Ed25519Verifier {
    fn default() -> Self {
        Self::new()
    }
}

impl CryptoVerifier for Ed25519Verifier {
    fn sign(&self, content: &[u8]) -> Result<Signature> {
        // Ensure we have a keypair for signing
        let keypair = self.keypair.as_ref().ok_or_else(|| {
            MarketplaceError::verification_error(
                "No keypair available for signing. Use with_keypair() or generate_keypair()",
                "Ed25519 signing",
            )
        })?;

        // Convert private key bytes to SigningKey
        let signing_key = self.bytes_to_signing_key(keypair.private_key_bytes())?;

        // Sign the content
        let signature_bytes = signing_key.sign(content);

        // Create and return Signature struct
        Ok(Signature::new(
            SignatureAlgorithm::Ed25519,
            signature_bytes.to_bytes().to_vec(),
            keypair.public_key.clone(),
        ))
    }

    fn verify(&self, content: &[u8], signature: &Signature) -> Result<bool> {
        // Validate signature algorithm
        if signature.algorithm != SignatureAlgorithm::Ed25519 {
            return Err(MarketplaceError::verification_error(
                format!(
                    "Unsupported signature algorithm: {}. Expected Ed25519",
                    signature.algorithm
                ),
                "Ed25519 verification",
            ));
        }

        // Validate signature length
        if signature.value.len() != 64 {
            return Err(MarketplaceError::verification_error(
                format!(
                    "Invalid signature length: expected 64 bytes, got {}",
                    signature.value.len()
                ),
                "Ed25519 verification",
            ));
        }

        // Convert public key bytes to VerifyingKey
        let verifying_key = self.bytes_to_verifying_key(&signature.public_key.key_data)?;

        // Convert signature bytes to ed25519_dalek::Signature
        let mut sig_array = [0u8; 64];
        sig_array.copy_from_slice(&signature.value);
        let ed25519_signature = ed25519_dalek::Signature::from_bytes(&sig_array);

        // Verify the signature
        match verifying_key.verify(content, &ed25519_signature) {
            Ok(()) => Ok(true),
            Err(_) => Ok(false), // Return false for invalid signatures, not an error
        }
    }

    fn generate_keypair(&self) -> Result<KeyPair> {
        // Generate a new random keypair using OsRng for cryptographic security
        let signing_key = SigningKey::generate(&mut OsRng);
        let verifying_key = signing_key.verifying_key();

        // Extract key bytes
        let private_key_bytes = signing_key.to_bytes().to_vec();
        let public_key_bytes = verifying_key.to_bytes().to_vec();

        // Create KeyPair
        Ok(KeyPair::new(
            SignatureAlgorithm::Ed25519,
            private_key_bytes,
            public_key_bytes,
        ))
    }

    fn import_public_key(&self, pem: &str) -> Result<PublicKey> {
        // Use the existing PEM parsing from PublicKey
        PublicKey::from_pem(pem)
    }

    fn export_public_key(&self, key: &PublicKey) -> Result<String> {
        // Validate it's an Ed25519 key
        if key.algorithm != SignatureAlgorithm::Ed25519 {
            return Err(MarketplaceError::verification_error(
                format!("Cannot export {} key as Ed25519", key.algorithm),
                "public key export",
            ));
        }

        // Use the existing PEM export from PublicKey
        Ok(key.to_pem())
    }

    fn hash_content(&self, content: &[u8]) -> Result<String> {
        use sha2::{Digest, Sha256};

        let mut hasher = Sha256::new();
        hasher.update(content);
        Ok(hex::encode(hasher.finalize()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_content() {
        let verifier = Ed25519Verifier::new();
        let content = b"Hello, World!";

        let hash1 = verifier.hash_content(content).unwrap();
        let hash2 = verifier.hash_content(content).unwrap();

        // Same content should produce same hash
        assert_eq!(hash1, hash2);

        // Hash should be 64 characters (32 bytes in hex)
        assert_eq!(hash1.len(), 64);
    }

    #[test]
    fn test_new_verifier_has_no_keypair() {
        let verifier = Ed25519Verifier::new();
        assert!(verifier.keypair().is_none());
    }

    #[test]
    fn test_with_keypair_stores_keypair() {
        let verifier = Ed25519Verifier::new();
        let keypair = verifier.generate_keypair().unwrap();
        let verifier_with_key = Ed25519Verifier::with_keypair(keypair);
        assert!(verifier_with_key.keypair().is_some());
    }

    #[test]
    fn test_sign_without_keypair_fails() {
        let verifier = Ed25519Verifier::new();
        let content = b"Test content";
        let result = verifier.sign(content);
        assert!(result.is_err());
    }

    #[test]
    fn test_generate_keypair_produces_32_byte_keys() {
        let verifier = Ed25519Verifier::new();
        let keypair = verifier.generate_keypair().unwrap();

        assert_eq!(keypair.public_key.key_data.len(), 32);
        assert_eq!(keypair.private_key_bytes().len(), 32);
        assert_eq!(keypair.public_key.algorithm, SignatureAlgorithm::Ed25519);
    }

    #[test]
    fn test_sign_and_verify_basic() {
        let verifier = Ed25519Verifier::new();
        let keypair = verifier.generate_keypair().unwrap();
        let verifier_with_key = Ed25519Verifier::with_keypair(keypair);

        let content = b"Test message";
        let signature = verifier_with_key.sign(content).unwrap();

        assert_eq!(signature.value.len(), 64);
        assert_eq!(signature.algorithm, SignatureAlgorithm::Ed25519);

        let verified = verifier_with_key.verify(content, &signature).unwrap();
        assert!(verified);
    }

    #[test]
    fn test_verify_with_wrong_content_fails() {
        let verifier = Ed25519Verifier::new();
        let keypair = verifier.generate_keypair().unwrap();
        let verifier_with_key = Ed25519Verifier::with_keypair(keypair);

        let content = b"Original content";
        let signature = verifier_with_key.sign(content).unwrap();

        let wrong_content = b"Different content";
        let verified = verifier_with_key.verify(wrong_content, &signature).unwrap();
        assert!(!verified);
    }

    #[test]
    fn test_deterministic_signatures() {
        let verifier = Ed25519Verifier::new();
        let keypair = verifier.generate_keypair().unwrap();
        let verifier_with_key = Ed25519Verifier::with_keypair(keypair);

        let content = b"Deterministic test";
        let sig1 = verifier_with_key.sign(content).unwrap();
        let sig2 = verifier_with_key.sign(content).unwrap();

        assert_eq!(sig1.value, sig2.value);
    }

    #[test]
    fn test_export_and_import_public_key() {
        let verifier = Ed25519Verifier::new();
        let keypair = verifier.generate_keypair().unwrap();

        let pem = verifier.export_public_key(&keypair.public_key).unwrap();
        assert!(pem.contains("-----BEGIN PUBLIC KEY-----"));
        assert!(pem.contains("-----END PUBLIC KEY-----"));

        let imported = verifier.import_public_key(&pem).unwrap();
        assert_eq!(imported.key_data, keypair.public_key.key_data);
    }
}
