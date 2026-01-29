//! Merkle tree implementation
//!
//! A binary Merkle tree where:
//! - Leaves are hashes of data (receipts, refusals, etc.)
//! - Internal nodes are hash(left || right)
//! - Tree is built bottom-up
//! - Odd leaves are duplicated to make complete binary tree

use mcp_core::crypto::{combine_hashes, hash_sha256_bytes};
use serde::{Deserialize, Serialize};

use crate::proof::{MerkleProof, ProofElement};

/// A Merkle tree for cryptographic verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTree {
    /// Leaf hashes (bottom level)
    leaves: Vec<[u8; 32]>,
    /// All tree levels (leaves at index 0, root at last index)
    levels: Vec<Vec<[u8; 32]>>,
}

impl MerkleTree {
    /// Create an empty Merkle tree
    pub fn new() -> Self {
        Self {
            leaves: Vec::new(),
            levels: Vec::new(),
        }
    }

    /// Create a Merkle tree from a list of leaf hashes
    pub fn from_leaves(leaves: Vec<[u8; 32]>) -> Self {
        let mut tree = Self::new();
        for leaf in leaves {
            tree.add_leaf(leaf);
        }
        tree.rebuild();
        tree
    }

    /// Create a Merkle tree from string hashes (hex-encoded)
    pub fn from_hex_leaves(hex_leaves: &[&str]) -> Result<Self, hex::FromHexError> {
        let mut tree = Self::new();
        for hex_leaf in hex_leaves {
            let bytes = hex::decode(hex_leaf)?;
            if bytes.len() != 32 {
                return Err(hex::FromHexError::InvalidStringLength);
            }
            let mut arr = [0u8; 32];
            arr.copy_from_slice(&bytes);
            tree.add_leaf(arr);
        }
        tree.rebuild();
        Ok(tree)
    }

    /// Add a leaf to the tree (requires rebuild after)
    pub fn add_leaf(&mut self, leaf_hash: [u8; 32]) {
        self.leaves.push(leaf_hash);
    }

    /// Add a leaf from data (hashes the data first)
    pub fn add_data(&mut self, data: &[u8]) {
        self.add_leaf(hash_sha256_bytes(data));
    }

    /// Rebuild the tree after adding leaves
    pub fn rebuild(&mut self) {
        self.levels.clear();

        if self.leaves.is_empty() {
            return;
        }

        // Level 0 is the leaves
        self.levels.push(self.leaves.clone());

        // Build up to the root
        while self.levels.last().map(|l| l.len()).unwrap_or(0) > 1 {
            let current = self.levels.last().unwrap();
            let mut next_level = Vec::new();

            for i in (0..current.len()).step_by(2) {
                let left = &current[i];
                // If odd number of nodes, duplicate the last one
                let right = current.get(i + 1).unwrap_or(left);
                next_level.push(combine_hashes(left, right));
            }

            self.levels.push(next_level);
        }
    }

    /// Get the root hash
    pub fn root(&self) -> Option<[u8; 32]> {
        self.levels.last().and_then(|l| l.first()).copied()
    }

    /// Get the root hash as hex string
    pub fn root_hex(&self) -> Option<String> {
        self.root().map(hex::encode)
    }

    /// Get number of leaves
    pub fn len(&self) -> usize {
        self.leaves.len()
    }

    /// Check if tree is empty
    pub fn is_empty(&self) -> bool {
        self.leaves.is_empty()
    }

    /// Generate a proof for a leaf at the given index
    pub fn proof(&self, index: usize) -> Option<MerkleProof> {
        if index >= self.leaves.len() || self.levels.is_empty() {
            return None;
        }

        let leaf = self.leaves[index];
        let mut elements = Vec::new();
        let mut current_index = index;

        // Walk up the tree, collecting sibling hashes
        for level in &self.levels[..self.levels.len().saturating_sub(1)] {
            let is_right = current_index % 2 == 1;
            let sibling_index = if is_right {
                current_index - 1
            } else {
                current_index + 1
            };

            // Get sibling (or duplicate if odd)
            let sibling = level.get(sibling_index).copied()
                .unwrap_or_else(|| level[current_index]);

            elements.push(ProofElement {
                hash: sibling,
                is_right: !is_right, // sibling position
            });

            current_index /= 2;
        }

        Some(MerkleProof {
            leaf,
            leaf_index: index,
            elements,
            root: self.root().unwrap_or([0u8; 32]),
        })
    }

    /// Verify that a leaf is in the tree (text-blind verification)
    pub fn verify_leaf(&self, leaf_hash: &[u8; 32]) -> bool {
        self.leaves.iter().any(|l| l == leaf_hash)
    }

    /// Get all leaves
    pub fn leaves(&self) -> &[[u8; 32]] {
        &self.leaves
    }

    /// Get tree height (number of levels including leaves)
    pub fn height(&self) -> usize {
        self.levels.len()
    }
}

impl Default for MerkleTree {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_tree() {
        let tree = MerkleTree::new();
        assert!(tree.is_empty());
        assert!(tree.root().is_none());
    }

    #[test]
    fn test_single_leaf() {
        let mut tree = MerkleTree::new();
        let leaf = hash_sha256_bytes(b"hello");
        tree.add_leaf(leaf);
        tree.rebuild();

        assert_eq!(tree.len(), 1);
        assert_eq!(tree.root(), Some(leaf)); // Single leaf is its own root
    }

    #[test]
    fn test_two_leaves() {
        let mut tree = MerkleTree::new();
        let leaf1 = hash_sha256_bytes(b"hello");
        let leaf2 = hash_sha256_bytes(b"world");
        tree.add_leaf(leaf1);
        tree.add_leaf(leaf2);
        tree.rebuild();

        assert_eq!(tree.len(), 2);

        let expected_root = combine_hashes(&leaf1, &leaf2);
        assert_eq!(tree.root(), Some(expected_root));
    }

    #[test]
    fn test_three_leaves_odd_duplication() {
        let mut tree = MerkleTree::new();
        let leaf1 = hash_sha256_bytes(b"a");
        let leaf2 = hash_sha256_bytes(b"b");
        let leaf3 = hash_sha256_bytes(b"c");
        tree.add_leaf(leaf1);
        tree.add_leaf(leaf2);
        tree.add_leaf(leaf3);
        tree.rebuild();

        assert_eq!(tree.len(), 3);
        assert_eq!(tree.height(), 3); // leaves, intermediate, root

        // Manual calculation:
        // Level 0: [leaf1, leaf2, leaf3]
        // Level 1: [hash(leaf1, leaf2), hash(leaf3, leaf3)]
        // Level 2: [hash(level1[0], level1[1])]
        let intermediate1 = combine_hashes(&leaf1, &leaf2);
        let intermediate2 = combine_hashes(&leaf3, &leaf3); // duplicated
        let expected_root = combine_hashes(&intermediate1, &intermediate2);

        assert_eq!(tree.root(), Some(expected_root));
    }

    #[test]
    fn test_from_hex_leaves() {
        let hex1 = "0000000000000000000000000000000000000000000000000000000000000001";
        let hex2 = "0000000000000000000000000000000000000000000000000000000000000002";

        let tree = MerkleTree::from_hex_leaves(&[hex1, hex2]).unwrap();
        assert_eq!(tree.len(), 2);
        assert!(tree.root().is_some());
    }

    #[test]
    fn test_proof_generation_and_verification() {
        let mut tree = MerkleTree::new();
        tree.add_data(b"receipt-1");
        tree.add_data(b"receipt-2");
        tree.add_data(b"receipt-3");
        tree.add_data(b"receipt-4");
        tree.rebuild();

        // Generate proof for each leaf
        for i in 0..4 {
            let proof = tree.proof(i).expect("proof should exist");
            assert!(proof.verify(), "proof {} should verify", i);
        }
    }

    #[test]
    fn test_proof_out_of_bounds() {
        let mut tree = MerkleTree::new();
        tree.add_data(b"only-one");
        tree.rebuild();

        assert!(tree.proof(0).is_some());
        assert!(tree.proof(1).is_none());
    }

    #[test]
    fn test_verify_leaf_presence() {
        let mut tree = MerkleTree::new();
        let leaf = hash_sha256_bytes(b"known-data");
        tree.add_leaf(leaf);
        tree.rebuild();

        assert!(tree.verify_leaf(&leaf));
        assert!(!tree.verify_leaf(&[0u8; 32]));
    }

    #[test]
    fn test_deterministic_root() {
        // Same leaves in same order should produce same root
        let build_tree = || {
            let mut tree = MerkleTree::new();
            tree.add_data(b"a");
            tree.add_data(b"b");
            tree.add_data(b"c");
            tree.rebuild();
            tree.root()
        };

        assert_eq!(build_tree(), build_tree());
    }

    #[test]
    fn test_order_matters() {
        let mut tree1 = MerkleTree::new();
        tree1.add_data(b"a");
        tree1.add_data(b"b");
        tree1.rebuild();

        let mut tree2 = MerkleTree::new();
        tree2.add_data(b"b");
        tree2.add_data(b"a");
        tree2.rebuild();

        assert_ne!(tree1.root(), tree2.root());
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_root_deterministic(leaves: Vec<Vec<u8>>) {
            prop_assume!(!leaves.is_empty());
            prop_assume!(leaves.len() <= 100); // Keep test fast

            let build = || {
                let mut tree = MerkleTree::new();
                for leaf in &leaves {
                    tree.add_data(leaf);
                }
                tree.rebuild();
                tree.root()
            };

            prop_assert_eq!(build(), build());
        }

        #[test]
        fn prop_all_proofs_verify(data: Vec<Vec<u8>>) {
            prop_assume!(!data.is_empty());
            prop_assume!(data.len() <= 50);

            let mut tree = MerkleTree::new();
            for d in &data {
                tree.add_data(d);
            }
            tree.rebuild();

            for i in 0..data.len() {
                let proof = tree.proof(i);
                prop_assert!(proof.is_some(), "proof {} should exist", i);
                prop_assert!(proof.unwrap().verify(), "proof {} should verify", i);
            }
        }

        #[test]
        fn prop_root_hex_length(leaves: Vec<Vec<u8>>) {
            prop_assume!(!leaves.is_empty());

            let mut tree = MerkleTree::new();
            for leaf in &leaves {
                tree.add_data(leaf);
            }
            tree.rebuild();

            if let Some(root_hex) = tree.root_hex() {
                prop_assert_eq!(root_hex.len(), 64);
            }
        }
    }
}
