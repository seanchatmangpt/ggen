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
//! use ggen_receipt::{Receipt, generate_keypair};
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
//! use ggen_receipt::{Receipt, ReceiptChain, generate_keypair};
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
pub mod error;
pub mod receipt;

pub use chain::ReceiptChain;
pub use error::{ReceiptError, Result};
pub use receipt::{generate_keypair, hash_data, Receipt};
