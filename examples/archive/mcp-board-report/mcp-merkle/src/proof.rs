//! Merkle proof implementation
//!
//! Proofs allow verification that a leaf is part of a Merkle tree
//! without revealing other leaves (text-blind verification).

use mcp_core::crypto::combine_hashes;
use serde::{Deserialize, Serialize};

/// An element in a Merkle proof
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofElement {
    /// The sibling hash at this level
    pub hash: [u8; 32],
    /// Whether the sibling is on the right side
    pub is_right: bool,
}

/// A Merkle proof for a single leaf
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleProof {
    /// The leaf being proven
    pub leaf: [u8; 32],
    /// Index of the leaf in the tree
    pub leaf_index: usize,
    /// Proof elements (path to root)
    pub elements: Vec<ProofElement>,
    /// Expected root hash
    pub root: [u8; 32],
}

impl MerkleProof {
    /// Verify the proof
    pub fn verify(&self) -> bool {
        let computed_root = self.compute_root();
        computed_root == self.root
    }

    /// Compute the root from the leaf and proof elements
    pub fn compute_root(&self) -> [u8; 32] {
        let mut current = self.leaf;

        for element in &self.elements {
            if element.is_right {
                current = combine_hashes(&current, &element.hash);
            } else {
                current = combine_hashes(&element.hash, &current);
            }
        }

        current
    }

    /// Verify against a specific root (text-blind verification)
    pub fn verify_against_root(&self, expected_root: &[u8; 32]) -> bool {
        self.compute_root() == *expected_root
    }

    /// Get proof as hex strings for serialization
    pub fn to_hex(&self) -> MerkleProofHex {
        MerkleProofHex {
            leaf: hex::encode(self.leaf),
            leaf_index: self.leaf_index,
            elements: self.elements.iter().map(|e| ProofElementHex {
                hash: hex::encode(e.hash),
                is_right: e.is_right,
            }).collect(),
            root: hex::encode(self.root),
        }
    }

    /// Get the number of elements in the proof (tree height - 1)
    pub fn height(&self) -> usize {
        self.elements.len()
    }
}

/// Proof element with hex-encoded hash
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofElementHex {
    pub hash: String,
    pub is_right: bool,
}

/// Merkle proof with hex-encoded hashes (for JSON serialization)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleProofHex {
    pub leaf: String,
    pub leaf_index: usize,
    pub elements: Vec<ProofElementHex>,
    pub root: String,
}

impl MerkleProofHex {
    /// Convert to binary proof
    pub fn to_binary(&self) -> Result<MerkleProof, hex::FromHexError> {
        let leaf = Self::decode_hash(&self.leaf)?;
        let root = Self::decode_hash(&self.root)?;
        let elements = self.elements.iter().map(|e| {
            Ok(ProofElement {
                hash: Self::decode_hash(&e.hash)?,
                is_right: e.is_right,
            })
        }).collect::<Result<Vec<_>, hex::FromHexError>>()?;

        Ok(MerkleProof {
            leaf,
            leaf_index: self.leaf_index,
            elements,
            root,
        })
    }

    fn decode_hash(hex_str: &str) -> Result<[u8; 32], hex::FromHexError> {
        let bytes = hex::decode(hex_str)?;
        if bytes.len() != 32 {
            return Err(hex::FromHexError::InvalidStringLength);
        }
        let mut arr = [0u8; 32];
        arr.copy_from_slice(&bytes);
        Ok(arr)
    }
}

/// Verify multiple proofs against the same root
pub fn verify_batch(proofs: &[MerkleProof], expected_root: &[u8; 32]) -> bool {
    proofs.iter().all(|p| p.verify_against_root(expected_root))
}

#[cfg(test)]
mod tests {
    use super::*;
    use mcp_core::crypto::hash_sha256_bytes;

    #[test]
    fn test_proof_verification() {
        // Create a simple 2-leaf tree manually
        let leaf1 = hash_sha256_bytes(b"hello");
        let leaf2 = hash_sha256_bytes(b"world");
        let root = combine_hashes(&leaf1, &leaf2);

        // Proof for leaf1 (left child)
        let proof = MerkleProof {
            leaf: leaf1,
            leaf_index: 0,
            elements: vec![ProofElement {
                hash: leaf2,
                is_right: true, // leaf2 is on the right
            }],
            root,
        };

        assert!(proof.verify());
    }

    #[test]
    fn test_proof_verification_right_leaf() {
        let leaf1 = hash_sha256_bytes(b"hello");
        let leaf2 = hash_sha256_bytes(b"world");
        let root = combine_hashes(&leaf1, &leaf2);

        // Proof for leaf2 (right child)
        let proof = MerkleProof {
            leaf: leaf2,
            leaf_index: 1,
            elements: vec![ProofElement {
                hash: leaf1,
                is_right: false, // leaf1 is on the left
            }],
            root,
        };

        assert!(proof.verify());
    }

    #[test]
    fn test_invalid_proof_wrong_root() {
        let leaf1 = hash_sha256_bytes(b"hello");
        let leaf2 = hash_sha256_bytes(b"world");
        let wrong_root = [0u8; 32];

        let proof = MerkleProof {
            leaf: leaf1,
            leaf_index: 0,
            elements: vec![ProofElement {
                hash: leaf2,
                is_right: true,
            }],
            root: wrong_root,
        };

        assert!(!proof.verify());
    }

    #[test]
    fn test_proof_hex_roundtrip() {
        let leaf1 = hash_sha256_bytes(b"data");
        let leaf2 = hash_sha256_bytes(b"more");
        let root = combine_hashes(&leaf1, &leaf2);

        let proof = MerkleProof {
            leaf: leaf1,
            leaf_index: 0,
            elements: vec![ProofElement {
                hash: leaf2,
                is_right: true,
            }],
            root,
        };

        let hex_proof = proof.to_hex();
        let restored = hex_proof.to_binary().unwrap();

        assert_eq!(proof.leaf, restored.leaf);
        assert_eq!(proof.root, restored.root);
        assert!(restored.verify());
    }

    #[test]
    fn test_verify_against_root() {
        let leaf = hash_sha256_bytes(b"only");
        let proof = MerkleProof {
            leaf,
            leaf_index: 0,
            elements: vec![],
            root: leaf, // Single leaf tree
        };

        assert!(proof.verify_against_root(&leaf));
        assert!(!proof.verify_against_root(&[1u8; 32]));
    }

    #[test]
    fn test_batch_verification() {
        let leaf1 = hash_sha256_bytes(b"a");
        let leaf2 = hash_sha256_bytes(b"b");
        let root = combine_hashes(&leaf1, &leaf2);

        let proofs = vec![
            MerkleProof {
                leaf: leaf1,
                leaf_index: 0,
                elements: vec![ProofElement { hash: leaf2, is_right: true }],
                root,
            },
            MerkleProof {
                leaf: leaf2,
                leaf_index: 1,
                elements: vec![ProofElement { hash: leaf1, is_right: false }],
                root,
            },
        ];

        assert!(verify_batch(&proofs, &root));
        assert!(!verify_batch(&proofs, &[0u8; 32]));
    }

    #[test]
    fn test_proof_height() {
        let proof = MerkleProof {
            leaf: [0u8; 32],
            leaf_index: 0,
            elements: vec![
                ProofElement { hash: [1u8; 32], is_right: true },
                ProofElement { hash: [2u8; 32], is_right: false },
                ProofElement { hash: [3u8; 32], is_right: true },
            ],
            root: [4u8; 32],
        };

        assert_eq!(proof.height(), 3);
    }
}
