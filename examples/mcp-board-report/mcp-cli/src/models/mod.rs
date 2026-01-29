//! Data models for MCP+ CLI
//!
//! This module provides CLI-specific models and re-exports core types from mcp-core.

pub mod bundle;
pub mod epoch;
pub mod receipt;

// Re-export CLI types
pub use bundle::{Bundle, BundleMetrics, VerificationResult};
pub use epoch::KeyEpoch;
pub use receipt::{CliReceipt, Receipt};

// Re-export mcp_core types for direct access
pub use mcp_core::types::{
    Capability,
    Envelope,
    Epoch as CoreEpoch,
    EpochStatus,
    EvidenceBundle as CoreEvidenceBundle,
    ExecutionMetrics,
    Receipt as CoreReceipt,
};

// Re-export mcp_core crypto functions
pub use mcp_core::crypto::{hash_sha256, hash_sha3_256, KeyPair, Signature};

// Re-export mcp_core constants
pub use mcp_core::{GENESIS_HASH, PROTOCOL_VERSION};

// Re-export mcp_merkle types
pub use mcp_merkle::{MerkleProof, MerkleTree};
