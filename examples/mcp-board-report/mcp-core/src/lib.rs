//! MCP+ Core Library
//!
//! Shared types, cryptographic primitives, and error handling for MCP+.
//!
//! Core axiom: A = μ(O), μ ∘ μ = μ, hash(A) = hash(μ(O)), O ⊨ Σ

pub mod config;
pub mod crypto;
pub mod error;
pub mod refusal;
pub mod types;

pub use config::{Config, Environment};
pub use crypto::{hash_sha256, hash_sha3_256, KeyPair, Signature};
pub use error::{McpError, McpResult};
pub use refusal::{Refusal, RefusalCategory, RefusalCode};
pub use types::*;

/// Semantic version of the MCP+ protocol
pub const PROTOCOL_VERSION: &str = "1.0.0";

/// Genesis hash for new receipt chains
pub const GENESIS_HASH: &str = "0000000000000000000000000000000000000000000000000000000000000000";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_genesis_hash_length() {
        assert_eq!(GENESIS_HASH.len(), 64);
    }

    #[test]
    fn test_protocol_version() {
        assert!(PROTOCOL_VERSION.starts_with("1."));
    }
}
