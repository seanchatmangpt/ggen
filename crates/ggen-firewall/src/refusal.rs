//! Refusal receipt generation
//!
//! All refused requests receive cryptographic receipts for audit trail.

use crate::IngressChannel;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use uuid::Uuid;

/// Cryptographic receipt for refused requests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefusalReceipt {
    /// Request ID that was refused
    pub request_id: Uuid,

    /// Channel that refused the request
    pub channel: IngressChannel,

    /// Reason for refusal
    pub reason: String,

    /// Timestamp of refusal
    pub refused_at: DateTime<Utc>,

    /// SHA-256 hash of request payload
    pub payload_hash: String,

    /// Receipt ID for tracking
    pub receipt_id: Uuid,

    /// Cryptographic signature of receipt
    pub signature: String,
}

impl RefusalReceipt {
    /// Create new refusal receipt
    pub fn new(
        request_id: Uuid,
        channel: IngressChannel,
        reason: String,
        payload: Vec<u8>,
    ) -> Self {
        let refused_at = Utc::now();
        let receipt_id = Uuid::new_v4();

        // Hash payload
        let payload_hash = Self::hash_payload(&payload);

        // Generate signature
        let signature = Self::generate_signature(
            request_id,
            channel,
            &reason,
            refused_at,
            &payload_hash,
            receipt_id,
        );

        Self {
            request_id,
            channel,
            reason,
            refused_at,
            payload_hash,
            receipt_id,
            signature,
        }
    }

    /// Hash payload with SHA-256
    fn hash_payload(payload: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(payload);
        hex::encode(hasher.finalize())
    }

    /// Generate cryptographic signature for receipt
    fn generate_signature(
        request_id: Uuid,
        channel: IngressChannel,
        reason: &str,
        refused_at: DateTime<Utc>,
        payload_hash: &str,
        receipt_id: Uuid,
    ) -> String {
        let mut hasher = Sha256::new();

        // Combine all fields for signature
        hasher.update(request_id.as_bytes());
        hasher.update(format!("{:?}", channel).as_bytes());
        hasher.update(reason.as_bytes());
        hasher.update(refused_at.to_rfc3339().as_bytes());
        hasher.update(payload_hash.as_bytes());
        hasher.update(receipt_id.as_bytes());

        hex::encode(hasher.finalize())
    }

    /// Verify receipt signature
    pub fn verify(&self) -> bool {
        let expected_signature = Self::generate_signature(
            self.request_id,
            self.channel,
            &self.reason,
            self.refused_at,
            &self.payload_hash,
            self.receipt_id,
        );

        self.signature == expected_signature
    }

    /// Export receipt as JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Import receipt from JSON
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_creation() {
        let request_id = Uuid::new_v4();
        let payload = vec![1, 2, 3, 4, 5];

        let receipt = RefusalReceipt::new(
            request_id,
            IngressChannel::Batch,
            "Rate limit exceeded".to_string(),
            payload,
        );

        assert_eq!(receipt.request_id, request_id);
        assert_eq!(receipt.channel, IngressChannel::Batch);
        assert_eq!(receipt.reason, "Rate limit exceeded");
        assert!(!receipt.payload_hash.is_empty());
        assert!(!receipt.signature.is_empty());
    }

    #[test]
    fn test_receipt_verification() {
        let request_id = Uuid::new_v4();
        let payload = vec![1, 2, 3, 4, 5];

        let receipt = RefusalReceipt::new(
            request_id,
            IngressChannel::Batch,
            "Rate limit exceeded".to_string(),
            payload,
        );

        assert!(receipt.verify());
    }

    #[test]
    fn test_receipt_tamper_detection() {
        let request_id = Uuid::new_v4();
        let payload = vec![1, 2, 3, 4, 5];

        let mut receipt = RefusalReceipt::new(
            request_id,
            IngressChannel::Batch,
            "Rate limit exceeded".to_string(),
            payload,
        );

        // Tamper with reason
        receipt.reason = "Modified reason".to_string();

        // Verification should fail
        assert!(!receipt.verify());
    }

    #[test]
    fn test_receipt_json_roundtrip() {
        let request_id = Uuid::new_v4();
        let payload = vec![1, 2, 3, 4, 5];

        let original = RefusalReceipt::new(
            request_id,
            IngressChannel::Emergency,
            "Channel disabled".to_string(),
            payload,
        );

        let json = original.to_json().expect("Failed to serialize");
        let restored = RefusalReceipt::from_json(&json).expect("Failed to deserialize");

        assert_eq!(original.request_id, restored.request_id);
        assert_eq!(original.channel, restored.channel);
        assert_eq!(original.reason, restored.reason);
        assert_eq!(original.signature, restored.signature);
        assert!(restored.verify());
    }

    #[test]
    fn test_payload_hash_consistency() {
        let payload = vec![1, 2, 3, 4, 5];

        let hash1 = RefusalReceipt::new(
            Uuid::new_v4(),
            IngressChannel::Batch,
            "Test".to_string(),
            payload.clone(),
        ).payload_hash;

        let hash2 = RefusalReceipt::new(
            Uuid::new_v4(),
            IngressChannel::Scheduled,
            "Test".to_string(),
            payload,
        ).payload_hash;

        // Same payload should produce same hash
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_signature_uniqueness() {
        let request_id = Uuid::new_v4();
        let payload = vec![1, 2, 3, 4, 5];

        let receipt1 = RefusalReceipt::new(
            request_id,
            IngressChannel::Batch,
            "Reason 1".to_string(),
            payload.clone(),
        );

        let receipt2 = RefusalReceipt::new(
            request_id,
            IngressChannel::Batch,
            "Reason 2".to_string(),
            payload,
        );

        // Different reasons should produce different signatures
        assert_ne!(receipt1.signature, receipt2.signature);
    }
}
