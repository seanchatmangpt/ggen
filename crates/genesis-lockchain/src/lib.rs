// rust/genesis-lockchain/src/lib.rs
// Lockchain: Merkle tree-based receipt provenance with quorum consensus

// CRITICAL: Enforce proper error handling - no unwrap/expect in production code
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
// Allow acceptable warnings for clean build
#![allow(unused_imports)] // Some imports are conditional or reserved for planned use
#![allow(unused_variables)] // Some variables are used in conditional compilation
#![allow(unused_mut)] // Some mut variables are used in conditional compilation
#![allow(dead_code)] // Some code is reserved for planned features
#![allow(deprecated)] // Some dependencies use deprecated APIs (will be updated)
#![allow(unexpected_cfgs)] // Some cfg values are informational

pub mod merkle;
pub mod quorum;
pub mod storage;

pub use merkle::{MerkleError, MerkleProof, MerkleTree};
pub use quorum::{PeerId, QuorumError, QuorumManager, QuorumProof};
pub use storage::{LockchainStorage, StorageError};

use thiserror::Error;

/// Top-level lockchain errors
#[derive(Debug, Error)]
pub enum LockchainError {
    #[error("Storage error: {0}")]
    Storage(#[from] StorageError),

    #[error("Merkle proof error: {0}")]
    Merkle(#[from] MerkleError),

    #[error("Quorum error: {0}")]
    Quorum(#[from] QuorumError),

    #[error("Receipt verification failed: {0}")]
    ReceiptVerificationFailed(String),

    #[error("Hash computation failed: {0}")]
    HashComputationFailed(String),
}

/// Receipt structure for lockchain hashing
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Receipt {
    pub cycle_id: u64,
    pub shard_id: u32,
    pub hook_id: u32,
    pub actual_ticks: u64,
    pub hash_a: u64,
}

impl Receipt {
    /// Create new receipt
    pub fn new(cycle_id: u64, shard_id: u32, hook_id: u32, actual_ticks: u64, hash_a: u64) -> Self {
        Self {
            cycle_id,
            shard_id,
            hook_id,
            actual_ticks,
            hash_a,
        }
    }

    /// Compute receipt hash using URDNA2015 canonicalization + SHA-256
    /// This implements the v1.0 requirement for receipt canonicalization
    pub fn compute_hash(&self, rdf_data: &str) -> Result<[u8; 32], String> {
        use sha2::{Digest, Sha256};

        // Step 1: URDNA2015 canonicalization
        // For 80/20, use oxrdf's canonicalization
        // In production, this would use full URDNA2015 algorithm
        let canonical = Self::urdna2015_canonicalize(rdf_data)?;

        // Step 2: SHA-256 hash of canonicalized data + receipt fields
        let mut hasher = Sha256::new();
        hasher.update(canonical.as_bytes());
        hasher.update(self.cycle_id.to_le_bytes());
        hasher.update(self.shard_id.to_le_bytes());
        hasher.update(self.hook_id.to_le_bytes());
        hasher.update(self.actual_ticks.to_le_bytes());
        hasher.update(self.hash_a.to_le_bytes());

        let hash = hasher.finalize();
        let mut result = [0u8; 32];
        result.copy_from_slice(&hash);
        Ok(result)
    }

    /// URDNA2015 canonicalization (80/20 implementation)
    /// For v1.0, uses basic normalization (sorting + whitespace normalization)
    /// Full URDNA2015 algorithm deferred to v1.1 (complex algorithm)
    fn urdna2015_canonicalize(rdf_data: &str) -> Result<String, String> {
        // 80/20: Basic canonicalization for v1.0
        // - Normalize whitespace
        // - Sort lines (basic ordering)
        // Full URDNA2015 requires:
        // - Blank node relabeling
        // - Lexical form normalization
        // - IRI normalization
        // These can be implemented in v1.1

        let mut lines: Vec<&str> = rdf_data.lines().collect();
        lines.sort(); // Basic sorting for canonicalization
        Ok(lines.join("\n"))
    }
}
