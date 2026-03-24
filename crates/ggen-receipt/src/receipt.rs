//! Receipt implementation with Ed25519 signatures.

use crate::error::{ReceiptError, Result};
use chrono::{DateTime, Utc};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// A cryptographic receipt for an operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Receipt {
    /// Unique identifier for the operation.
    pub operation_id: String,

    /// Timestamp when the operation occurred.
    pub timestamp: DateTime<Utc>,

    /// Hashes of input data.
    pub input_hashes: Vec<String>,

    /// Hashes of output data.
    pub output_hashes: Vec<String>,

    /// Ed25519 signature of the receipt data.
    pub signature: String,

    /// Hash of the previous receipt in the chain (None for genesis).
    pub previous_receipt_hash: Option<String>,
}

impl Receipt {
    /// Creates a new receipt with the given parameters.
    ///
    /// # Arguments
    ///
    /// * `operation_id` - Unique identifier for the operation
    /// * `input_hashes` - Hashes of input data
    /// * `output_hashes` - Hashes of output data
    /// * `previous_receipt_hash` - Hash of the previous receipt (None for genesis)
    ///
    /// # Returns
    ///
    /// A new `Receipt` instance ready to be signed.
    #[must_use]
    pub fn new(
        operation_id: String,
        input_hashes: Vec<String>,
        output_hashes: Vec<String>,
        previous_receipt_hash: Option<String>,
    ) -> Self {
        Self {
            operation_id,
            timestamp: Utc::now(),
            input_hashes,
            output_hashes,
            signature: String::new(),
            previous_receipt_hash,
        }
    }

    /// Signs the receipt with the given signing key.
    ///
    /// # Arguments
    ///
    /// * `signing_key` - Ed25519 signing key
    ///
    /// # Returns
    ///
    /// The signed receipt.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::Serialization` if the receipt cannot be serialized.
    pub fn sign(mut self, signing_key: &SigningKey) -> Result<Self> {
        let message = self.signing_message()?;
        let signature = signing_key.sign(&message);
        self.signature = hex::encode(signature.to_bytes());
        Ok(self)
    }

    /// Verifies the receipt signature with the given verifying key.
    ///
    /// # Arguments
    ///
    /// * `verifying_key` - Ed25519 verifying key
    ///
    /// # Returns
    ///
    /// `Ok(())` if the signature is valid.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::InvalidSignature` if the signature is invalid or malformed.
    /// Returns `ReceiptError::Serialization` if the receipt cannot be serialized.
    pub fn verify(&self, verifying_key: &VerifyingKey) -> Result<()> {
        let message = self.signing_message()?;

        let signature_bytes = hex::decode(&self.signature)
            .map_err(|_| ReceiptError::InvalidSignature)?;

        let signature = Signature::from_slice(&signature_bytes)
            .map_err(|_| ReceiptError::InvalidSignature)?;

        verifying_key
            .verify(&message, &signature)
            .map_err(|_| ReceiptError::InvalidSignature)
    }

    /// Computes the hash of this receipt.
    ///
    /// # Returns
    ///
    /// A hex-encoded SHA-256 hash of the receipt.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::Serialization` if the receipt cannot be serialized.
    pub fn hash(&self) -> Result<String> {
        let json = serde_json::to_string(self)?;
        let mut hasher = Sha256::new();
        hasher.update(json.as_bytes());
        Ok(hex::encode(hasher.finalize()))
    }

    /// Links this receipt to a previous receipt by setting the previous hash.
    ///
    /// # Arguments
    ///
    /// * `previous_receipt` - The previous receipt in the chain
    ///
    /// # Returns
    ///
    /// The updated receipt.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::Serialization` if the previous receipt cannot be hashed.
    pub fn chain(mut self, previous_receipt: &Receipt) -> Result<Self> {
        self.previous_receipt_hash = Some(previous_receipt.hash()?);
        Ok(self)
    }

    /// Generates the message to be signed.
    fn signing_message(&self) -> Result<Vec<u8>> {
        let mut data = Self {
            signature: String::new(),
            ..self.clone()
        };
        data.signature = String::new();

        let json = serde_json::to_string(&data)?;
        Ok(json.into_bytes())
    }
}

/// Generates a new Ed25519 keypair.
///
/// # Returns
///
/// A tuple of (signing_key, verifying_key).
#[must_use]
pub fn generate_keypair() -> (SigningKey, VerifyingKey) {
    let mut rng = rand::thread_rng();
    let signing_key = SigningKey::generate(&mut rng);
    let verifying_key = signing_key.verifying_key();
    (signing_key, verifying_key)
}

/// Computes the SHA-256 hash of arbitrary data.
///
/// # Arguments
///
/// * `data` - Data to hash
///
/// # Returns
///
/// A hex-encoded SHA-256 hash.
#[must_use]
pub fn hash_data(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hex::encode(hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_creation() {
        let receipt = Receipt::new(
            "test-op".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        );

        assert_eq!(receipt.operation_id, "test-op");
        assert_eq!(receipt.input_hashes.len(), 1);
        assert_eq!(receipt.output_hashes.len(), 1);
        assert!(receipt.previous_receipt_hash.is_none());
    }

    #[test]
    fn test_receipt_signing_and_verification() {
        let (signing_key, verifying_key) = generate_keypair();

        let receipt = Receipt::new(
            "test-op".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        );

        let signed_receipt = receipt.sign(&signing_key).expect("signing failed");

        assert!(!signed_receipt.signature.is_empty());
        assert!(signed_receipt.verify(&verifying_key).is_ok());
    }

    #[test]
    fn test_receipt_verification_fails_with_wrong_key() {
        let (signing_key, _) = generate_keypair();
        let (_, wrong_key) = generate_keypair();

        let receipt = Receipt::new(
            "test-op".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        );

        let signed_receipt = receipt.sign(&signing_key).expect("signing failed");

        assert!(signed_receipt.verify(&wrong_key).is_err());
    }

    #[test]
    fn test_receipt_hash() {
        let receipt = Receipt::new(
            "test-op".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        );

        let hash1 = receipt.hash().expect("hashing failed");
        let hash2 = receipt.hash().expect("hashing failed");

        assert_eq!(hash1, hash2);
        assert_eq!(hash1.len(), 64); // SHA-256 produces 64 hex characters
    }

    #[test]
    fn test_receipt_chain() {
        let (signing_key, _) = generate_keypair();

        let receipt1 = Receipt::new(
            "op1".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let receipt2 = Receipt::new(
            "op2".to_string(),
            vec!["input2".to_string()],
            vec!["output2".to_string()],
            None,
        )
        .chain(&receipt1)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

        assert!(receipt2.previous_receipt_hash.is_some());
        assert_eq!(
            receipt2.previous_receipt_hash.as_ref().unwrap(),
            &receipt1.hash().expect("hashing failed")
        );
    }

    #[test]
    fn test_hash_data() {
        let data = b"test data";
        let hash1 = hash_data(data);
        let hash2 = hash_data(data);

        assert_eq!(hash1, hash2);
        assert_eq!(hash1.len(), 64);
    }
}
