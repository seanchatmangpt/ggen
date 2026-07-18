//! Click Tracking with Cryptographic Receipts
//!
//! Generated from ontology/routing.ttl
//! DO NOT EDIT - regenerate via: ggen sync

use sha2::{Sha256, Digest};
use uuid::Uuid;
use chrono::{DateTime, Utc};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Click receipt with Merkle chain link
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ClickReceipt {
    pub click_id: Uuid,
    pub route_id: Uuid,
    pub timestamp: DateTime<Utc>,
    pub ip_hash: String,
    pub user_agent: Option<String>,
    pub referrer: Option<String>,
    pub hash: String,
    pub prev_hash: Option<String>,
}

/// Receipt generation errors
#[derive(Debug, thiserror::Error)]
pub enum ReceiptError {
    #[error("Invalid IP address: {0}")]
    InvalidIp(String),

    #[error("Hashing error: {0}")]
    HashingError(String),

    #[error("Chain validation failed: expected {expected}, got {actual}")]
    ChainValidationFailed { expected: String, actual: String },

    #[error("Storage error: {0}")]
    StorageError(String),
}

/// Click receipt generator with Merkle chain
pub struct ClickReceiptGenerator {
    /// Last receipt hash for chain linking
    last_hash: Arc<RwLock<Option<String>>>,
}

impl ClickReceiptGenerator {
    /// Create new generator
    pub fn new() -> Self {
        Self {
            last_hash: Arc::new(RwLock::new(None)),
        }
    }

    /// Generate receipt with cryptographic proof
    ///
    /// # Performance
    /// - SHA-256 hashing: ~2μs per receipt
    /// - Chain linking: single RwLock write
    pub async fn generate(
        &self,
        route_id: Uuid,
        visitor_ip: &str,
        user_agent: Option<String>,
        referrer: Option<String>,
    ) -> Result<ClickReceipt, ReceiptError> {
        // Generate UUID v7 (time-ordered)
        let click_id = Uuid::now_v7();
        let timestamp = Utc::now();

        // Hash IP for privacy (GDPR compliance)
        let ip_hash = Self::hash_ip(visitor_ip)?;

        // Get previous hash for Merkle chain
        let prev_hash = {
            let guard = self.last_hash.read().await;
            guard.clone()
        };

        // Create receipt
        let mut receipt = ClickReceipt {
            click_id,
            route_id,
            timestamp,
            ip_hash,
            user_agent,
            referrer,
            hash: String::new(), // Computed below
            prev_hash,
        };

        // Compute SHA-256 hash of receipt content
        receipt.hash = Self::compute_receipt_hash(&receipt)?;

        // Update chain
        {
            let mut guard = self.last_hash.write().await;
            *guard = Some(receipt.hash.clone());
        }

        Ok(receipt)
    }

    /// Hash IP address with SHA-256 (privacy-safe)
    fn hash_ip(ip: &str) -> Result<String, ReceiptError> {
        if ip.is_empty() {
            return Err(ReceiptError::InvalidIp("Empty IP address".to_string()));
        }

        let mut hasher = Sha256::new();
        hasher.update(ip.as_bytes());
        let result = hasher.finalize();
        Ok(format!("{:x}", result))
    }

    /// Compute SHA-256 hash of receipt content
    fn compute_receipt_hash(receipt: &ClickReceipt) -> Result<String, ReceiptError> {
        let content = format!(
            "{}|{}|{}|{}|{}|{}|{}",
            receipt.click_id,
            receipt.route_id,
            receipt.timestamp.to_rfc3339(),
            receipt.ip_hash,
            receipt.user_agent.as_deref().unwrap_or(""),
            receipt.referrer.as_deref().unwrap_or(""),
            receipt.prev_hash.as_deref().unwrap_or("")
        );

        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        let result = hasher.finalize();
        Ok(format!("{:x}", result))
    }

    /// Verify receipt hash integrity
    pub fn verify_receipt(receipt: &ClickReceipt) -> Result<bool, ReceiptError> {
        let computed_hash = Self::compute_receipt_hash(receipt)?;
        Ok(computed_hash == receipt.hash)
    }

    /// Verify Merkle chain integrity
    pub fn verify_chain(receipts: &[ClickReceipt]) -> Result<bool, ReceiptError> {
        for i in 1..receipts.len() {
            let prev = &receipts[i - 1];
            let current = &receipts[i];

            match &current.prev_hash {
                Some(prev_hash) if prev_hash == &prev.hash => continue,
                Some(prev_hash) => {
                    return Err(ReceiptError::ChainValidationFailed {
                        expected: prev.hash.clone(),
                        actual: prev_hash.clone(),
                    });
                }
                None => {
                    return Err(ReceiptError::ChainValidationFailed {
                        expected: prev.hash.clone(),
                        actual: "None".to_string(),
                    });
                }
            }
        }
        Ok(true)
    }
}

impl Default for ClickReceiptGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_receipt_generation() {
        let generator = ClickReceiptGenerator::new();
        let route_id = Uuid::now_v7();

        let receipt = generator
            .generate(route_id, "192.168.1.100", None, None)
            .await
            .expect("generate receipt");

        assert_eq!(receipt.route_id, route_id);
        assert!(!receipt.hash.is_empty());
        assert!(ClickReceiptGenerator::verify_receipt(&receipt).expect("verify"));
    }

    #[tokio::test]
    async fn test_merkle_chain() {
        let generator = ClickReceiptGenerator::new();
        let route_id = Uuid::now_v7();

        let mut receipts = Vec::new();
        for i in 0..10 {
            let receipt = generator
                .generate(
                    route_id,
                    &format!("192.168.1.{}", i),
                    None,
                    None,
                )
                .await
                .expect("generate receipt");
            receipts.push(receipt);
        }

        // Verify chain integrity
        assert!(ClickReceiptGenerator::verify_chain(&receipts).expect("verify chain"));

        // Verify first receipt has no prev_hash
        assert!(receipts[0].prev_hash.is_none());

        // Verify subsequent receipts link correctly
        for i in 1..receipts.len() {
            assert_eq!(
                receipts[i].prev_hash.as_ref().unwrap(),
                &receipts[i - 1].hash
            );
        }
    }

    #[test]
    fn test_ip_hashing() {
        let hash1 = ClickReceiptGenerator::hash_ip("192.168.1.100").expect("hash");
        let hash2 = ClickReceiptGenerator::hash_ip("192.168.1.100").expect("hash");
        let hash3 = ClickReceiptGenerator::hash_ip("192.168.1.101").expect("hash");

        // Same IP produces same hash (deterministic)
        assert_eq!(hash1, hash2);

        // Different IP produces different hash
        assert_ne!(hash1, hash3);

        // Hash is irreversible (not plaintext)
        assert_ne!(hash1, "192.168.1.100");
    }
}
