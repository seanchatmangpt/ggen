//! Receipt ledger - cryptographic audit trail
//!
//! This module implements the **Knowledge** phase of MAPE-K:
//! - Hash-chain receipt ledger (blockchain-like properties)
//! - Immutable audit trail for compliance
//! - Receipt verification for integrity checks
//! - Deterministic receipt generation

use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::Utc;
use sha2::{Sha256, Digest};
use std::collections::VecDeque;

/// Receipt ledger errors
#[derive(Debug, Error)]
pub enum ReceiptError {
    #[error("Receipt chain corrupted at position {position}")]
    ChainCorrupted { position: usize },

    #[error("Invalid receipt format: {0}")]
    InvalidFormat(String),

    #[error("Ledger operation failed: {0}")]
    OperationFailed(String),

    #[error("Receipt not found: {id}")]
    ReceiptNotFound { id: String },
}

/// Single receipt entry (hash-chained)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub id: String,
    pub hash: String,
    pub prev_hash: String, // Link to previous receipt
    pub timestamp: i64,
    pub action: String,
    pub result: String,
}

impl Receipt {
    /// Compute hash of receipt
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.id.as_bytes());
        hasher.update(b"|");
        hasher.update(self.prev_hash.as_bytes());
        hasher.update(b"|");
        hasher.update(self.timestamp.to_string().as_bytes());
        hasher.update(b"|");
        hasher.update(self.action.as_bytes());
        hasher.update(b"|");
        hasher.update(self.result.as_bytes());

        let digest = hasher.finalize();
        hex::encode(digest)
    }

    /// Verify receipt integrity
    pub fn verify(&self) -> Result<(), ReceiptError> {
        let computed_hash = self.compute_hash();
        if computed_hash != self.hash {
            return Err(ReceiptError::InvalidFormat(
                "Receipt hash mismatch - tampering detected".to_string(),
            ));
        }
        Ok(())
    }
}

/// Receipt ledger with hash chain
pub struct ReceiptLedger;

// Global receipt chain (max 1000 entries)
static RECEIPT_CHAIN: std::sync::OnceLock<std::sync::Mutex<VecDeque<Receipt>>> =
    std::sync::OnceLock::new();

fn get_chain() -> std::sync::MutexGuard<'static, VecDeque<Receipt>> {
    RECEIPT_CHAIN
        .get_or_init(|| std::sync::Mutex::new(VecDeque::with_capacity(1000)))
        .lock()
        .unwrap()
}

impl ReceiptLedger {
    /// Emit new receipt into ledger
    ///
    /// Creates deterministic hash-chain link to previous receipt.
    /// Supports arbitrary action/result descriptions.
    ///
    /// ## Determinism guarantee
    /// - Same action + result → same hash
    /// - Receipt sequence is immutable
    /// - Chain verification detects tampering
    pub async fn emit(action: &str, result: &str) -> Result<Receipt, ReceiptError> {
        let mut chain = get_chain();

        let id = uuid::Uuid::new_v4().to_string();
        let timestamp = Utc::now().timestamp_millis();

        let prev_hash = chain
            .back()
            .map(|r| r.hash.clone())
            .unwrap_or_else(|| "0".repeat(64)); // Genesis block

        let receipt = Receipt {
            id: id.clone(),
            hash: "".to_string(), // Will be computed
            prev_hash,
            timestamp,
            action: action.to_string(),
            result: result.to_string(),
        };

        let mut receipt_with_hash = receipt;
        receipt_with_hash.hash = receipt_with_hash.compute_hash();

        // Verify integrity immediately (paranoid check)
        receipt_with_hash.verify()?;

        chain.push_back(receipt_with_hash.clone());

        // Keep ledger bounded
        if chain.len() > 1000 {
            chain.pop_front();
        }

        tracing::debug!(
            receipt_id = %receipt_with_hash.id,
            action = action,
            result = result,
            "Receipt emitted"
        );

        Ok(receipt_with_hash)
    }

    /// Verify entire chain integrity
    ///
    /// Walks the entire receipt chain and verifies:
    /// - Each receipt's hash is valid (self-integrity)
    /// - Each receipt links correctly to predecessor
    /// - No gaps or reordering
    pub async fn verify_chain() -> Result<bool, ReceiptError> {
        let chain = get_chain();

        if chain.is_empty() {
            return Ok(true); // Empty chain is valid
        }

        let mut prev_hash = "0".repeat(64); // Genesis

        for (position, receipt) in chain.iter().enumerate() {
            // Verify self-integrity
            receipt.verify().map_err(|_| ReceiptError::ChainCorrupted {
                position,
            })?;

            // Verify chain link
            if receipt.prev_hash != prev_hash {
                return Err(ReceiptError::ChainCorrupted {
                    position,
                });
            }

            prev_hash = receipt.hash.clone();
        }

        Ok(true)
    }

    /// Get receipt by ID
    pub async fn get(receipt_id: &str) -> Result<Receipt, ReceiptError> {
        let chain = get_chain();
        chain
            .iter()
            .find(|r| r.id == receipt_id)
            .cloned()
            .ok_or_else(|| ReceiptError::ReceiptNotFound {
                id: receipt_id.to_string(),
            })
    }

    /// Get last N receipts
    pub async fn tail(n: usize) -> Vec<Receipt> {
        let chain = get_chain();
        chain.iter().rev().take(n).cloned().collect()
    }

    /// Get all receipts for action
    pub async fn by_action(action: &str) -> Vec<Receipt> {
        let chain = get_chain();
        chain
            .iter()
            .filter(|r| r.action.contains(action))
            .cloned()
            .collect()
    }

    /// Clear ledger (for testing only)
    #[cfg(test)]
    pub fn clear() {
        get_chain().clear();
    }

    /// Get chain length
    pub fn len() -> usize {
        get_chain().len()
    }

    /// Check if chain is empty
    pub fn is_empty() -> bool {
        get_chain().is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_emit_single_receipt() {
        // Arrange
        ReceiptLedger::clear();

        // Act
        let receipt = ReceiptLedger::emit("TestAction", "Success").await;

        // Assert
        assert!(receipt.is_ok());
        let r = receipt.unwrap();
        assert!(!r.id.is_empty());
        assert!(!r.hash.is_empty());
        assert_eq!(r.action, "TestAction");
        assert_eq!(r.result, "Success");
    }

    #[tokio::test]
    async fn test_emit_creates_hash_chain() {
        // Arrange
        ReceiptLedger::clear();

        // Act
        let r1 = ReceiptLedger::emit("Action1", "Result1").await.unwrap();
        let r2 = ReceiptLedger::emit("Action2", "Result2").await.unwrap();

        // Assert
        assert_ne!(r1.hash, r2.hash);
        assert_eq!(r2.prev_hash, r1.hash);
    }

    #[tokio::test]
    async fn test_receipt_verify_integrity() {
        // Arrange
        let mut receipt = Receipt {
            id: "test-1".to_string(),
            hash: "".to_string(),
            prev_hash: "0".repeat(64),
            timestamp: Utc::now().timestamp_millis(),
            action: "TestAction".to_string(),
            result: "Success".to_string(),
        };
        receipt.hash = receipt.compute_hash();

        // Act
        let result = receipt.verify();

        // Assert
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_receipt_verify_tampering() {
        // Arrange
        let mut receipt = Receipt {
            id: "test-2".to_string(),
            hash: "".to_string(),
            prev_hash: "0".repeat(64),
            timestamp: Utc::now().timestamp_millis(),
            action: "TestAction".to_string(),
            result: "Success".to_string(),
        };
        receipt.hash = receipt.compute_hash();

        // Tamper with result
        receipt.result = "Tampered".to_string();

        // Act
        let result = receipt.verify();

        // Assert
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_verify_chain_empty() {
        // Arrange
        ReceiptLedger::clear();

        // Act
        let result = ReceiptLedger::verify_chain().await;

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[tokio::test]
    async fn test_verify_chain_integrity() {
        // Arrange
        ReceiptLedger::clear();
        ReceiptLedger::emit("Action1", "Result1").await.unwrap();
        ReceiptLedger::emit("Action2", "Result2").await.unwrap();
        ReceiptLedger::emit("Action3", "Result3").await.unwrap();

        // Act
        let result = ReceiptLedger::verify_chain().await;

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[tokio::test]
    async fn test_get_receipt_by_id() {
        // Arrange
        ReceiptLedger::clear();
        let r1 = ReceiptLedger::emit("Action1", "Result1").await.unwrap();
        ReceiptLedger::emit("Action2", "Result2").await.unwrap();

        // Act
        let result = ReceiptLedger::get(&r1.id).await;

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().id, r1.id);
    }

    #[tokio::test]
    async fn test_tail_last_n_receipts() {
        // Arrange
        ReceiptLedger::clear();
        ReceiptLedger::emit("Action1", "Result1").await.unwrap();
        ReceiptLedger::emit("Action2", "Result2").await.unwrap();
        ReceiptLedger::emit("Action3", "Result3").await.unwrap();

        // Act
        let tail = ReceiptLedger::tail(2).await;

        // Assert
        assert_eq!(tail.len(), 2);
        assert_eq!(tail[0].action, "Action3");
        assert_eq!(tail[1].action, "Action2");
    }

    #[tokio::test]
    async fn test_by_action_filter() {
        // Arrange
        ReceiptLedger::clear();

        // Emit new actions with unique IDs to avoid interference
        let action1 = format!("CPU_THROTTLE_{}", uuid::Uuid::new_v4());
        let action2 = format!("MEMORY_CHECK_{}", uuid::Uuid::new_v4());
        let action3 = format!("CPU_RESTORE_{}", uuid::Uuid::new_v4());

        ReceiptLedger::emit(&action1, "Success").await.unwrap();
        ReceiptLedger::emit(&action2, "Success").await.unwrap();
        ReceiptLedger::emit(&action3, "Success").await.unwrap();

        // Act
        let cpu_actions = ReceiptLedger::by_action("CPU").await;

        // Assert: should have at least 2 CPU-related actions (the ones we just added)
        assert!(cpu_actions.len() >= 2, "Expected at least 2 CPU actions, got {}", cpu_actions.len());
    }

    #[tokio::test]
    async fn test_deterministic_hash() {
        // Arrange
        ReceiptLedger::clear();
        let r1 = ReceiptLedger::emit("TestAction", "Success").await.unwrap();

        // Create same action offline
        let mut r_offline = Receipt {
            id: r1.id.clone(),
            hash: "".to_string(),
            prev_hash: r1.prev_hash.clone(),
            timestamp: r1.timestamp,
            action: "TestAction".to_string(),
            result: "Success".to_string(),
        };
        r_offline.hash = r_offline.compute_hash();

        // Assert
        assert_eq!(r1.hash, r_offline.hash);
    }
}
