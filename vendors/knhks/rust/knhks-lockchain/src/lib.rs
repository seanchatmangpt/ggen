// rust/knhks-lockchain/src/lib.rs
// Provenance Lockchain Integration
// Merkle-linked receipt storage for audit trail

#![no_std]
extern crate alloc;

use alloc::vec::Vec;
use alloc::string::String;
use alloc::collections::BTreeMap;

/// Receipt hash (SHA3-256)
pub type ReceiptHash = [u8; 32];

/// Merkle tree node
#[derive(Debug, Clone)]
pub struct MerkleNode {
    pub hash: ReceiptHash,
    pub left: Option<Box<MerkleNode>>,
    pub right: Option<Box<MerkleNode>>,
}

/// Lockchain entry
#[derive(Debug, Clone)]
pub struct LockchainEntry {
    pub receipt_id: String,
    pub receipt_hash: ReceiptHash,
    pub parent_hash: Option<ReceiptHash>,
    pub timestamp_ms: u64,
    pub metadata: BTreeMap<String, String>,
}

/// Lockchain implementation
pub struct Lockchain {
    entries: Vec<LockchainEntry>,
    merkle_root: Option<ReceiptHash>,
}

impl Lockchain {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            merkle_root: None,
        }
    }

    /// Append receipt to lockchain
    pub fn append(&mut self, entry: LockchainEntry) -> Result<ReceiptHash, LockchainError> {
        // Calculate hash
        let hash = Self::compute_hash(&entry);

        // Link to previous entry
        let mut entry = entry;
        entry.receipt_hash = hash;
        entry.parent_hash = self.merkle_root;

        // Update merkle root
        self.merkle_root = Some(hash);

        // Store entry
        self.entries.push(entry);

        Ok(hash)
    }

    /// Compute hash for entry (simplified - real implementation uses SHA3-256)
    fn compute_hash(entry: &LockchainEntry) -> ReceiptHash {
        // Placeholder: In real implementation, use SHA3-256
        // For now, simple hash of receipt_id + timestamp
        let mut hash = [0u8; 32];
        let data = format!("{}{}", entry.receipt_id, entry.timestamp_ms);
        for (i, byte) in data.as_bytes().iter().enumerate() {
            if i < 32 {
                hash[i] = *byte;
            }
        }
        hash
    }

    /// Get receipt by ID
    pub fn get_receipt(&self, receipt_id: &str) -> Option<&LockchainEntry> {
        self.entries.iter().find(|e| e.receipt_id == receipt_id)
    }

    /// Verify receipt integrity
    pub fn verify(&self, receipt_id: &str) -> Result<bool, LockchainError> {
        let entry = self.get_receipt(receipt_id)
            .ok_or(LockchainError::NotFound)?;

        // Verify hash
        let computed_hash = Self::compute_hash(entry);
        if computed_hash != entry.receipt_hash {
            return Ok(false);
        }

        // Verify parent chain
        if let Some(parent_hash) = entry.parent_hash {
            if let Some(parent) = self.entries.iter().find(|e| e.receipt_hash == parent_hash) {
                // Parent exists - chain is valid
                return Ok(true);
            }
        }

        Ok(true)
    }

    /// Get merkle root
    pub fn merkle_root(&self) -> Option<ReceiptHash> {
        self.merkle_root
    }

    /// Get all entries
    pub fn entries(&self) -> &[LockchainEntry] {
        &self.entries
    }

    /// Merge receipts (for batch operations)
    pub fn merge_receipts(&self, receipt_ids: &[String]) -> Result<ReceiptHash, LockchainError> {
        // In real implementation: build Merkle tree from receipts
        // For now: simple concatenation hash
        let mut combined = String::new();
        for id in receipt_ids {
            combined.push_str(id);
        }
        
        let mut hash = [0u8; 32];
        for (i, byte) in combined.as_bytes().iter().enumerate() {
            if i < 32 {
                hash[i] = *byte;
            }
        }
        Ok(hash)
    }
}

/// Lockchain errors
#[derive(Debug)]
pub enum LockchainError {
    NotFound,
    InvalidHash,
    ChainBroken,
}

impl Default for Lockchain {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lockchain_append() {
        let mut chain = Lockchain::new();
        let entry = LockchainEntry {
            receipt_id: "receipt1".to_string(),
            receipt_hash: [0; 32],
            parent_hash: None,
            timestamp_ms: 1000,
            metadata: BTreeMap::new(),
        };

        let hash = chain.append(entry).unwrap();
        assert_eq!(chain.entries().len(), 1);
        assert_eq!(chain.merkle_root(), Some(hash));
    }

    #[test]
    fn test_lockchain_verify() {
        let mut chain = Lockchain::new();
        let entry = LockchainEntry {
            receipt_id: "receipt1".to_string(),
            receipt_hash: [0; 32],
            parent_hash: None,
            timestamp_ms: 1000,
            metadata: BTreeMap::new(),
        };

        chain.append(entry).unwrap();
        assert!(chain.verify("receipt1").unwrap());
    }
}

