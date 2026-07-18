//! Receipt Module
//!
//! Provides Merkle-chained cryptographic receipts for audit trails.
//! Each receipt chains to the previous via hash, creating tamper-evident history.

use crate::crypto::hash_sha256;
use crate::{McpError, McpResult};
use serde::{Deserialize, Serialize};

/// Receipt outcome type
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReceiptOutcome {
    Success,
    Refused { code: String, message: String },
    Error { code: String, message: String },
}

/// Cryptographic receipt for a single operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub receipt_id: String,
    pub contract_id: String,
    pub envelope_id: String,
    pub manifest_hash: String,
    pub outcome: ReceiptOutcome,
    pub previous_hash: String,
    pub receipt_hash: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub duration_us: u64,
}

impl Receipt {
    /// Create a new receipt
    pub fn new(
        contract_id: String,
        envelope_id: String,
        manifest_hash: String,
        outcome: ReceiptOutcome,
        previous_hash: String,
        duration_us: u64,
    ) -> Self {
        let receipt_id = format!("rcpt-{}", uuid::Uuid::new_v4());
        let timestamp = chrono::Utc::now();

        // Compute receipt hash from contents
        let hash_input = format!(
            "{}|{}|{}|{}|{:?}|{}|{}",
            receipt_id, contract_id, envelope_id, manifest_hash, outcome, previous_hash, timestamp
        );
        let receipt_hash = hash_sha256(hash_input.as_bytes());

        Self {
            receipt_id,
            contract_id,
            envelope_id,
            manifest_hash,
            outcome,
            previous_hash,
            receipt_hash,
            timestamp,
            duration_us,
        }
    }

    /// Create genesis receipt (first in chain)
    pub fn genesis(contract_id: String, envelope_id: String, manifest_hash: String) -> Self {
        Self::new(
            contract_id,
            envelope_id,
            manifest_hash,
            ReceiptOutcome::Success,
            "0".repeat(64), // Genesis has zero hash as previous
            0,
        )
    }

    /// Verify receipt hash integrity
    pub fn verify(&self) -> McpResult<()> {
        let hash_input = format!(
            "{}|{}|{}|{}|{:?}|{}|{}",
            self.receipt_id,
            self.contract_id,
            self.envelope_id,
            self.manifest_hash,
            self.outcome,
            self.previous_hash,
            self.timestamp
        );
        let computed_hash = hash_sha256(hash_input.as_bytes());

        if computed_hash != self.receipt_hash {
            return Err(McpError::ChainError(format!(
                "Receipt hash mismatch: expected {}, got {}",
                self.receipt_hash, computed_hash
            )));
        }

        Ok(())
    }

    /// Serialize to JSON
    pub fn to_json(&self) -> McpResult<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| McpError::SerializationError(e.to_string()))
    }

    /// Deserialize from JSON
    pub fn from_json(json: &str) -> McpResult<Self> {
        serde_json::from_str(json)
            .map_err(|e| McpError::SerializationError(e.to_string()))
    }
}

/// Chain of receipts with Merkle verification
#[derive(Debug, Clone)]
pub struct ReceiptChain {
    receipts: Vec<Receipt>,
    root_hash: String,
}

impl ReceiptChain {
    /// Create chain from receipts
    pub fn from_receipts(receipts: Vec<Receipt>) -> McpResult<Self> {
        if receipts.is_empty() {
            return Err(McpError::ChainError("Cannot create chain from empty receipts".to_string()));
        }

        // Compute Merkle root
        let root_hash = Self::compute_merkle_root(&receipts);

        Ok(Self { receipts, root_hash })
    }

    /// Verify the entire chain
    pub fn verify(&self) -> McpResult<()> {
        if self.receipts.is_empty() {
            return Err(McpError::ChainError("Empty chain".to_string()));
        }

        // Verify each receipt
        for receipt in &self.receipts {
            receipt.verify()?;
        }

        // Verify chain linkage
        for i in 1..self.receipts.len() {
            let prev = &self.receipts[i - 1];
            let curr = &self.receipts[i];

            if curr.previous_hash != prev.receipt_hash {
                return Err(McpError::ChainError(format!(
                    "Chain broken at receipt {}: expected previous_hash {}, got {}",
                    i, prev.receipt_hash, curr.previous_hash
                )));
            }
        }

        // Verify Merkle root
        let computed_root = Self::compute_merkle_root(&self.receipts);
        if computed_root != self.root_hash {
            return Err(McpError::ChainError(format!(
                "Merkle root mismatch: expected {}, got {}",
                self.root_hash, computed_root
            )));
        }

        Ok(())
    }

    /// Compute Merkle root from receipts
    fn compute_merkle_root(receipts: &[Receipt]) -> String {
        if receipts.is_empty() {
            return "0".repeat(64);
        }

        // Get leaf hashes
        let mut hashes: Vec<String> = receipts.iter().map(|r| r.receipt_hash.clone()).collect();

        // Build Merkle tree
        while hashes.len() > 1 {
            let mut next_level = Vec::new();

            for chunk in hashes.chunks(2) {
                let combined = if chunk.len() == 2 {
                    format!("{}{}", chunk[0], chunk[1])
                } else {
                    format!("{}{}", chunk[0], chunk[0]) // Duplicate odd leaf
                };
                next_level.push(hash_sha256(combined.as_bytes()));
            }

            hashes = next_level;
        }

        hashes.into_iter().next().unwrap_or_else(|| "0".repeat(64))
    }

    /// Get the root hash
    pub fn root_hash(&self) -> &str {
        &self.root_hash
    }

    /// Get receipt count
    pub fn len(&self) -> usize {
        self.receipts.len()
    }

    /// Check if chain is empty
    pub fn is_empty(&self) -> bool {
        self.receipts.is_empty()
    }

    /// Get all receipts
    pub fn receipts(&self) -> &[Receipt] {
        &self.receipts
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_creation() {
        let receipt = Receipt::new(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
            ReceiptOutcome::Success,
            "0".repeat(64),
            1000,
        );

        assert!(receipt.receipt_id.starts_with("rcpt-"));
        assert_eq!(receipt.receipt_hash.len(), 64);
    }

    #[test]
    fn test_receipt_verification() {
        let receipt = Receipt::new(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
            ReceiptOutcome::Success,
            "0".repeat(64),
            1000,
        );

        // Should verify successfully
        receipt.verify().unwrap();
    }

    #[test]
    fn test_genesis_receipt() {
        let genesis = Receipt::genesis(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
        );

        assert_eq!(genesis.previous_hash, "0".repeat(64));
        genesis.verify().unwrap();
    }

    #[test]
    fn test_receipt_chain() {
        let genesis = Receipt::genesis(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
        );

        let receipt2 = Receipt::new(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
            ReceiptOutcome::Success,
            genesis.receipt_hash.clone(),
            500,
        );

        let receipt3 = Receipt::new(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
            ReceiptOutcome::Refused {
                code: "ENV-0001".to_string(),
                message: "Time exceeded".to_string(),
            },
            receipt2.receipt_hash.clone(),
            750,
        );

        let chain = ReceiptChain::from_receipts(vec![genesis, receipt2, receipt3]).unwrap();

        assert_eq!(chain.len(), 3);
        chain.verify().unwrap();
    }

    #[test]
    fn test_broken_chain_detection() {
        let genesis = Receipt::genesis(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
        );

        let receipt2 = Receipt::new(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
            ReceiptOutcome::Success,
            "wrong-hash".to_string(), // Wrong previous hash!
            500,
        );

        let chain = ReceiptChain::from_receipts(vec![genesis, receipt2]).unwrap();
        let result = chain.verify();

        assert!(result.is_err());
        if let Err(McpError::ChainError(msg)) = result {
            assert!(msg.contains("Chain broken"));
        }
    }

    #[test]
    fn test_receipt_serialization() {
        let receipt = Receipt::new(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
            ReceiptOutcome::Success,
            "0".repeat(64),
            1000,
        );

        let json = receipt.to_json().unwrap();
        let parsed = Receipt::from_json(&json).unwrap();

        assert_eq!(parsed.receipt_id, receipt.receipt_id);
        assert_eq!(parsed.receipt_hash, receipt.receipt_hash);
    }
}
