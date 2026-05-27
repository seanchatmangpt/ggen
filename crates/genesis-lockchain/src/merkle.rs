// rust/genesis-lockchain/src/merkle.rs
// Merkle tree implementation for receipt provenance

use crate::Receipt;
use blake3::Hasher;
use thiserror::Error;

/// Merkle proof error types
#[derive(Debug, Error)]
pub enum MerkleError {
    #[error("Invalid leaf index: {index} (tree has {leaf_count} leaves)")]
    InvalidLeafIndex { index: usize, leaf_count: usize },

    #[error("Merkle proof generation failed: {0}")]
    ProofGenerationFailed(String),

    #[error("Merkle proof verification failed: {0}")]
    ProofVerificationFailed(String),

    #[error("Empty Merkle tree")]
    EmptyTree,
}

/// Merkle tree for receipt hashing
/// Builds binary tree bottom-up, computes root hash
#[derive(Debug)]
pub struct MerkleTree {
    leaves: Vec<[u8; 32]>, // Receipt hashes (leaf nodes)
    nodes: Vec<[u8; 32]>,  // Internal node hashes
    root: [u8; 32],        // Merkle root
}

impl MerkleTree {
    /// Create new empty Merkle tree
    pub fn new() -> Self {
        Self {
            leaves: Vec::new(),
            nodes: Vec::new(),
            root: [0u8; 32],
        }
    }

    /// Add receipt to Merkle tree as leaf node
    /// Hash format: cycle_id || shard_id || hook_id || ticks || hash_a
    /// Returns the leaf hash
    pub fn add_receipt(&mut self, receipt: &Receipt) -> [u8; 32] {
        let mut hasher = Hasher::new();

        // Hash receipt fields in order
        hasher.update(&receipt.cycle_id.to_le_bytes());
        hasher.update(&receipt.shard_id.to_le_bytes());
        hasher.update(&receipt.hook_id.to_le_bytes());
        hasher.update(&receipt.actual_ticks.to_le_bytes());
        hasher.update(&receipt.hash_a.to_le_bytes());

        let leaf_hash: [u8; 32] = *hasher.finalize().as_bytes();
        self.leaves.push(leaf_hash);

        leaf_hash
    }

    /// Add a raw leaf hash directly to the Merkle tree
    /// This allows builders to feed pre-computed hashes without Receipt wrappers
    /// Returns the leaf hash for verification
    pub fn add_raw_leaf(&mut self, hash: [u8; 32]) -> [u8; 32] {
        self.leaves.push(hash);
        hash
    }

    /// Compute Merkle root by building tree bottom-up
    /// Algorithm:
    /// 1. Start with leaf hashes
    /// 2. For each level, hash pairs: H(left || right)
    /// 3. If odd count, duplicate last node
    /// 4. Repeat until single root remains
    pub fn compute_root(&mut self) -> [u8; 32] {
        if self.leaves.is_empty() {
            return [0u8; 32];
        }

        // Single leaf is the root
        if self.leaves.len() == 1 {
            self.root = self.leaves[0];
            return self.root;
        }

        // Build tree bottom-up
        let mut current_level = self.leaves.clone();
        self.nodes.clear();

        while current_level.len() > 1 {
            let mut next_level = Vec::new();

            // Process pairs of nodes
            for chunk in current_level.chunks(2) {
                let mut hasher = Hasher::new();
                hasher.update(&chunk[0]);

                // Hash with right sibling or duplicate if odd
                if chunk.len() > 1 {
                    hasher.update(&chunk[1]);
                } else {
                    hasher.update(&chunk[0]); // Duplicate last node
                }

                let parent_hash = *hasher.finalize().as_bytes();
                next_level.push(parent_hash);
            }

            // Store internal nodes for proof generation
            self.nodes.extend_from_slice(&next_level);
            current_level = next_level;
        }

        // Root is the last remaining node
        self.root = current_level[0];
        self.root
    }

    /// Generate Merkle proof for a specific leaf
    /// Proof consists of sibling hashes along path to root
    pub fn generate_proof(&self, leaf_index: usize) -> Result<MerkleProof, MerkleError> {
        if leaf_index >= self.leaves.len() {
            return Err(MerkleError::InvalidLeafIndex {
                index: leaf_index,
                leaf_count: self.leaves.len(),
            });
        }

        let mut proof_hashes = Vec::new();
        let mut current_index = leaf_index;
        let mut current_level = self.leaves.clone();

        // Traverse tree bottom-up, collecting sibling hashes
        while current_level.len() > 1 {
            // Find sibling index
            let sibling_index = if current_index.is_multiple_of(2) {
                current_index + 1
            } else {
                current_index - 1
            };

            // Add sibling hash to proof (or duplicate if at end)
            let sibling_hash = if sibling_index < current_level.len() {
                current_level[sibling_index]
            } else {
                current_level[current_index] // Duplicate for odd count
            };
            proof_hashes.push(sibling_hash);

            // Move to parent level
            let mut next_level = Vec::new();
            for chunk in current_level.chunks(2) {
                let mut hasher = Hasher::new();
                hasher.update(&chunk[0]);
                if chunk.len() > 1 {
                    hasher.update(&chunk[1]);
                } else {
                    hasher.update(&chunk[0]);
                }
                next_level.push(*hasher.finalize().as_bytes());
            }

            current_index /= 2;
            current_level = next_level;
        }

        Ok(MerkleProof {
            leaf_index,
            leaf_hash: self.leaves[leaf_index],
            proof_hashes,
            root: self.root,
        })
    }

    /// Get root hash
    pub fn root(&self) -> [u8; 32] {
        self.root
    }

    /// Get leaf count
    pub fn leaf_count(&self) -> usize {
        self.leaves.len()
    }

    /// Reset tree (clear all nodes)
    pub fn reset(&mut self) {
        self.leaves.clear();
        self.nodes.clear();
        self.root = [0u8; 32];
    }
}

impl Default for MerkleTree {
    fn default() -> Self {
        Self::new()
    }
}

/// Merkle proof for audit trail
/// Allows verification that a receipt was included in a specific root
#[derive(Debug, Clone)]
pub struct MerkleProof {
    pub leaf_index: usize,
    pub leaf_hash: [u8; 32],
    pub proof_hashes: Vec<[u8; 32]>,
    pub root: [u8; 32],
}

impl MerkleProof {
    /// Verify proof against root
    /// Reconstructs root from leaf and sibling hashes
    pub fn verify(&self) -> bool {
        let mut current_hash = self.leaf_hash;
        let mut current_index = self.leaf_index;

        for sibling_hash in &self.proof_hashes {
            let mut hasher = Hasher::new();

            // Hash in correct order based on index parity
            if current_index.is_multiple_of(2) {
                hasher.update(&current_hash);
                hasher.update(sibling_hash);
            } else {
                hasher.update(sibling_hash);
                hasher.update(&current_hash);
            }

            current_hash = *hasher.finalize().as_bytes();
            current_index /= 2;
        }

        current_hash == self.root
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merkle_tree_single_leaf() {
        let mut tree = MerkleTree::new();
        let receipt = Receipt::new(1, 0, 0, 5, 0x1234567890abcdef);

        tree.add_receipt(&receipt);
        let root = tree.compute_root();

        assert_ne!(root, [0u8; 32]);
        assert_eq!(tree.leaf_count(), 1);
    }

    #[test]
    fn test_merkle_tree_multiple_leaves() {
        let mut tree = MerkleTree::new();

        for i in 0..4 {
            let receipt = Receipt::new(i, 0, 0, 5 + i, 0x1234567890abcdef + i);
            tree.add_receipt(&receipt);
        }

        let root = tree.compute_root();
        assert_ne!(root, [0u8; 32]);
        assert_eq!(tree.leaf_count(), 4);
    }

    #[test]
    fn test_merkle_proof_generation() -> Result<(), MerkleError> {
        let mut tree = MerkleTree::new();

        for i in 0..4 {
            let receipt = Receipt::new(i, 0, 0, 5 + i, 0x1234567890abcdef + i);
            tree.add_receipt(&receipt);
        }

        tree.compute_root();

        let proof = tree.generate_proof(0)?;
        assert_eq!(proof.leaf_index, 0);
        assert!(proof.verify());
        Ok(())
    }

    #[test]
    fn test_merkle_proof_verification() -> Result<(), MerkleError> {
        let mut tree = MerkleTree::new();

        for i in 0..8 {
            let receipt = Receipt::new(i, 0, 0, 5 + i, 0x1234567890abcdef + i);
            tree.add_receipt(&receipt);
        }

        tree.compute_root();

        // Verify all proofs
        for i in 0..8 {
            let proof = tree.generate_proof(i)?;
            assert!(proof.verify(), "Proof verification failed for leaf {}", i);
        }
        Ok(())
    }

    #[test]
    fn test_merkle_tree_deterministic() {
        let receipts = vec![
            Receipt::new(0, 0, 0, 5, 0x1234567890abcdef),
            Receipt::new(1, 1, 1, 6, 0x2345678901bcdef0),
            Receipt::new(2, 2, 2, 7, 0x3456789012cdef01),
        ];

        let mut tree1 = MerkleTree::new();
        for receipt in &receipts {
            tree1.add_receipt(receipt);
        }
        let root1 = tree1.compute_root();

        let mut tree2 = MerkleTree::new();
        for receipt in &receipts {
            tree2.add_receipt(receipt);
        }
        let root2 = tree2.compute_root();

        assert_eq!(root1, root2, "Merkle root should be deterministic");
    }
}
