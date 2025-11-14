//! Cryptographic verification implementations
//!
//! This module provides implementations of the `CryptoVerifier` trait for
//! cryptographic signature verification of packages. Supports Ed25519 signatures
//! and provides a default verifier implementation.
//!
//! ## Features
//!
//! - **Ed25519 signatures**: High-performance elliptic curve signatures
//! - **Content verification**: Verify package integrity and authenticity
//! - **Key management**: Key pair generation and import/export
//! - **Hash computation**: Content hashing for integrity checks

pub mod ed25519;
pub mod verifier;

pub use ed25519::Ed25519Verifier;
pub use verifier::DefaultVerifier;
