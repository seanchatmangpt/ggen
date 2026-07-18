//! Cryptographic receipts for consensus decisions with Ed25519 signatures
use chrono::{DateTime, Utc};
use ed25519_dalek::{Signature, VerifyingKey};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use uuid::Uuid;

/// A cryptographic signature from a node on a receipt
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptSignature {
    /// Node ID that created this signature
    pub node_id: u64,
    /// Ed25519 signature as hex string
    pub signature: String,
    /// Timestamp of signature
    pub timestamp: DateTime<Utc>,
}

impl ReceiptSignature {
    /// Create a new receipt signature
    pub fn new(node_id: u64, signature: String) -> Self {
        Self {
            node_id,
            signature,
            timestamp: Utc::now(),
        }
    }

    /// Verify this signature against a content hash and public key
    pub fn verify(&self, content_hash: &str, public_key: &VerifyingKey) -> Result<(), String> {
        // Decode signature from hex
        let sig_bytes = hex::decode(&self.signature)
            .map_err(|e| format!("Failed to decode signature: {}", e))?;
        let signature = Signature::from_bytes(
            sig_bytes
                .as_slice()
                .try_into()
                .map_err(|_| "Invalid signature size".to_string())?,
        );

        // Verify signature over the content hash using the trait
        use ed25519_dalek::Verifier;
        public_key
            .verify(content_hash.as_bytes(), &signature)
            .map_err(|e| format!("Signature verification failed: {}", e))
    }
}

/// Receipt for a consensus decision with cryptographic proof
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    /// Unique ID for this receipt
    pub id: String,
    /// Round number when consensus was reached
    pub round: u64,
    /// View number (primary) at consensus time
    pub view: u64,
    /// The value that was decided
    pub value: String,
    /// SHA256 hash of the consensus decision
    pub content_hash: String,
    /// Signatures from participating nodes (must have 2f+1)
    pub signatures: Vec<ReceiptSignature>,
    /// Timestamp of decision
    pub timestamp: DateTime<Utc>,
    /// Audit trail showing which nodes participated
    pub audit_trail: String,
}

impl Receipt {
    /// Create a new receipt for a consensus decision
    pub fn new(round: u64, view: u64, value: String) -> Self {
        // Hash the decision content for verification
        let content = format!("round:{},view:{},value:{}", round, view, value);
        let content_hash = Self::compute_hash(&content);

        // Generate audit trail
        let audit_trail = format!(
            "Receipt for consensus decision: round={}, view={}, timestamp={}",
            round,
            view,
            Utc::now()
        );

        Self {
            id: Uuid::new_v4().to_string(),
            round,
            view,
            value,
            content_hash,
            signatures: Vec::new(),
            timestamp: Utc::now(),
            audit_trail,
        }
    }

    /// Compute SHA256 hash of content
    pub fn compute_hash(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        hex::encode(hasher.finalize())
    }

    /// Add a signature from a node
    pub fn add_signature(&mut self, node_id: u64, signature: String) -> Result<(), String> {
        // Check for duplicate signatures from same node
        if self.signatures.iter().any(|s| s.node_id == node_id) {
            return Err(format!("Duplicate signature from node {}", node_id));
        }
        self.signatures
            .push(ReceiptSignature::new(node_id, signature));
        Ok(())
    }

    /// Verify receipt has required quorum of signatures (2f+1)
    pub fn has_quorum(&self, quorum_size: usize) -> bool {
        self.signatures.len() >= quorum_size
    }

    /// Verify all signatures in receipt
    pub fn verify_all_signatures(
        &self, public_keys: &HashMap<u64, VerifyingKey>,
    ) -> Result<(), String> {
        for sig in &self.signatures {
            if let Some(public_key) = public_keys.get(&sig.node_id) {
                sig.verify(&self.content_hash, public_key).map_err(|e| {
                    format!(
                        "Signature verification failed for node {}: {:?}",
                        sig.node_id, e
                    )
                })?;
            } else {
                return Err(format!("No public key found for node {}", sig.node_id));
            }
        }
        Ok(())
    }

    /// Generate human-readable audit report
    pub fn audit_report(&self) -> String {
        let signature_list = self
            .signatures
            .iter()
            .map(|s| {
                let sig_preview = if s.signature.len() > 16 {
                    &s.signature[..16]
                } else {
                    &s.signature
                };
                format!("  - Node {}: {} ({})", s.node_id, sig_preview, s.timestamp)
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            "=== Consensus Receipt ===\n\
             ID: {}\n\
             Round: {}\n\
             View: {}\n\
             Value: {}\n\
             Content Hash: {}\n\
             Signatures: {} of {}\n\
             {}\n\
             Signers:\n{}\n\
             Timestamp: {}",
            self.id,
            self.round,
            self.view,
            self.value,
            self.content_hash,
            self.signatures.len(),
            (self.signatures.len() * 2) / 3 + 1, // Implied quorum
            self.audit_trail,
            signature_list,
            self.timestamp
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_creation() {
        let receipt = Receipt::new(1, 0, "test_value".to_string());
        assert_eq!(receipt.round, 1);
        assert_eq!(receipt.view, 0);
        assert_eq!(receipt.value, "test_value");
        assert!(!receipt.content_hash.is_empty());
    }

    #[test]
    fn test_hash_computation() {
        let hash1 = Receipt::compute_hash("content");
        let hash2 = Receipt::compute_hash("content");
        assert_eq!(hash1, hash2);

        let hash3 = Receipt::compute_hash("different");
        assert_ne!(hash1, hash3);
    }

    #[test]
    fn test_add_signature() {
        let mut receipt = Receipt::new(1, 0, "value".to_string());
        assert!(receipt.add_signature(0, "sig1".to_string()).is_ok());
        assert_eq!(receipt.signatures.len(), 1);

        // Duplicate signature should fail
        assert!(receipt.add_signature(0, "sig2".to_string()).is_err());
    }

    #[test]
    fn test_quorum_check() {
        let mut receipt = Receipt::new(1, 0, "value".to_string());
        assert!(!receipt.has_quorum(3));

        receipt.add_signature(0, "sig0".to_string()).unwrap();
        receipt.add_signature(1, "sig1".to_string()).unwrap();
        receipt.add_signature(2, "sig2".to_string()).unwrap();
        assert!(receipt.has_quorum(3));
    }

    #[test]
    fn test_audit_report_generation() {
        let mut receipt = Receipt::new(1, 0, "value".to_string());
        receipt
            .add_signature(0, "signature_from_node_0".to_string())
            .unwrap();
        receipt
            .add_signature(1, "signature_from_node_1".to_string())
            .unwrap();

        let report = receipt.audit_report();
        assert!(report.contains("Round: 1"));
        assert!(report.contains("View: 0"));
        assert!(report.contains("Value: value"));
        assert!(report.contains("Signatures: 2 of"));
    }

    #[test]
    fn test_signature_creation() {
        let sig = ReceiptSignature::new(5, "test_sig".to_string());
        assert_eq!(sig.node_id, 5);
        assert_eq!(sig.signature, "test_sig");
    }
}
