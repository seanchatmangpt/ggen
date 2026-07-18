//! MCP+ Merkle Tree Implementation
//!
//! Provides cryptographic Merkle trees for receipt chain verification.
//!
//! Features:
//! - Incremental tree building (add leaves one at a time)
//! - Proof generation and verification
//! - Deterministic root calculation
//! - Text-blind verification (verify without seeing leaf data)
//! - Cross-contract proof verification (prove data flow between contracts)
//! - Streaming verification with O(log n) memory
//! - Git-like branching for receipt chains (branch, merge, rebase)

pub mod branch;
pub mod cross;
pub mod proof;
pub mod streaming;
pub mod tree;

pub use branch::{
    Branch, BranchingChain, ChainDiff, ConflictType, MergeConflict, MergeResult, RebaseResult,
    DEFAULT_BRANCH,
};
pub use cross::{ContractLink, DataFlowGraph, FlowVerification, LinkProof};
pub use proof::{MerkleProof, ProofElement};
pub use streaming::{
    BatchVerifyResult, IncrementalMerkle, IncrementalMerkleState, InvalidReason,
    StreamingVerifier, VerifierState, VerifyResult,
};
pub use tree::MerkleTree;
