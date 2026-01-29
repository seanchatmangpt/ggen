//! MCP+ Merkle Tree Implementation
//!
//! Provides cryptographic Merkle trees for receipt chain verification.
//!
//! Features:
//! - Incremental tree building (add leaves one at a time)
//! - Proof generation and verification
//! - Deterministic root calculation
//! - Text-blind verification (verify without seeing leaf data)

pub mod proof;
pub mod tree;

pub use proof::{MerkleProof, ProofElement};
pub use tree::MerkleTree;
