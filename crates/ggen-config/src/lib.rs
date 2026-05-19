//! # ggen-config - Configuration and Infrastructure
//!
//! Type-safe configuration parsing, validation, and cryptographic receipt management for ggen.
//!
//! ## Features
//!
//! - Type-safe TOML parsing with serde
//! - Comprehensive configuration schema
//! - Environment variable expansion
//! - Workspace support
//! - Cryptographic receipts (Ed25519 signing and verification)
//! - Receipt chains and envelopes for operation provenance
//! - Rich error handling

pub mod config;
pub mod config_lib;
pub mod receipt;

pub use config_lib::{
    A2AConfig, A2AMessagingConfig, A2AOrchestrationConfig, A2ARetryConfig, A2ATransportConfig,
    AiConfig, ConfigError, ConfigLoader, ConfigValidator, GgenConfig, McpConfig,
    McpDiscoveryConfig, McpTlsConfig, McpToolsConfig, McpTransportConfig, McpZaiConfig,
    ProjectConfig, Result, TemplatesConfig,
};

// Re-export receipt types for convenience
pub use receipt::{
    chain, create_chained_receipt, envelope, error, generate_keypair, hash_data, payload_hash,
    receipt_impl, EnvelopeChain, EnvelopeChainLink, EnvelopeSignature, PayloadRef, Producer,
    Receipt, ReceiptChain, ReceiptEnvelope, ReceiptError, ENVELOPE_SCHEMA, HASH_PREFIX,
    SIGNATURE_ALGORITHM,
};
