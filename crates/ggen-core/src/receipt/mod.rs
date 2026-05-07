//! Cryptographic receipt system for ggen operations.
//!
//! This crate provides a production-ready receipt system with Ed25519 signatures
//! for verifiable operation tracking. Receipts can be chained together to form
//! an auditable trail of operations.
//!
//! # Features
//!
//! - Ed25519 digital signatures for cryptographic verification
//! - SHA-256 hashing for data integrity
//! - Receipt chaining with hash links
//! - Full serialization support with serde
//! - Comprehensive error handling
//!
//! # Examples
//!
//! ## Creating and signing a receipt
//!
//! ```
//! use crate::receipt::{Receipt, generate_keypair};
//!
//! let (signing_key, verifying_key) = generate_keypair();
//!
//! let receipt = Receipt::new(
//!     "my-operation".to_string(),
//!     vec!["input-hash-1".to_string()],
//!     vec!["output-hash-1".to_string()],
//!     None,
//! )
//! .sign(&signing_key)
//! .expect("Failed to sign receipt");
//!
//! // Verify the signature
//! receipt.verify(&verifying_key).expect("Verification failed");
//! ```
//!
//! ## Building a receipt chain
//!
//! ```
//! use crate::receipt::{Receipt, ReceiptChain, generate_keypair};
//!
//! let (signing_key, verifying_key) = generate_keypair();
//!
//! // Create genesis receipt
//! let genesis = Receipt::new(
//!     "genesis-op".to_string(),
//!     vec![],
//!     vec![],
//!     None,
//! )
//! .sign(&signing_key)
//! .expect("Failed to sign");
//!
//! let mut chain = ReceiptChain::from_genesis(genesis.clone())
//!     .expect("Failed to create chain");
//!
//! // Add a linked receipt
//! let receipt2 = Receipt::new(
//!     "second-op".to_string(),
//!     vec![],
//!     vec![],
//!     None,
//! )
//! .chain(&genesis)
//! .expect("Failed to chain")
//! .sign(&signing_key)
//! .expect("Failed to sign");
//!
//! chain.append(receipt2).expect("Failed to append");
//!
//! // Verify the entire chain
//! chain.verify(&verifying_key).expect("Chain verification failed");
//! ```

#![forbid(unsafe_code)]
#![deny(missing_docs)]

pub mod chain;
pub mod envelope;
pub mod error;
pub mod receipt_impl;
pub use self::receipt_impl::{generate_keypair, hash_data, Receipt};
pub use chain::ReceiptChain;
pub use envelope::{
    payload_hash, EnvelopeChain, EnvelopeChainLink, EnvelopeSignature, PayloadRef, Producer,
    ReceiptEnvelope, ENVELOPE_SCHEMA, HASH_PREFIX, SIGNATURE_ALGORITHM,
};
pub use error::{ReceiptError, Result};

/// Convenience function to create a new receipt chained to a parent.
///
/// Creates a receipt for `operation_id` with the given `input_hashes` and
/// `output_hashes`, links it to `parent` via its hash, and signs it with
/// `signing_key`. Returns the signed, chained receipt on success.
///
/// # Example
///
/// ```
/// use crate::receipt::{Receipt, create_chained_receipt, generate_keypair};
///
/// let (key, _vk) = generate_keypair();
/// let parent = Receipt::new("parent-op".to_string(), vec![], vec![], None)
///     .sign(&key).expect("sign");
/// let child = create_chained_receipt(&parent, "child-op".to_string(), vec![], vec![], &key)
///     .expect("chained");
/// assert_eq!(child.previous_receipt_hash, Some(parent.hash().unwrap()));
/// ```
pub fn create_chained_receipt(
    parent: &Receipt, operation_id: String, input_hashes: Vec<String>, output_hashes: Vec<String>,
    signing_key: &ed25519_dalek::SigningKey,
) -> Result<Receipt> {
    Receipt::new(operation_id, input_hashes, output_hashes, None)
        .chain(parent)?
        .sign(signing_key)
}
