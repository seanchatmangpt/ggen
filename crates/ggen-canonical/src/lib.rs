//! Deterministic canonicalization system for ggen
//!
//! This crate provides deterministic canonicalization for multiple formats:
//! - Rust code (via rustfmt)
//! - JSON (sorted keys)
//! - RDF/TTL (canonical triples)
//! - Hash computation for verification
//!
//! All operations are deterministic - same input always produces same output.

use std::fmt;

pub mod hash;
pub mod json;
pub mod rust;
pub mod ttl;

/// Error types for canonicalization operations
#[derive(Debug, thiserror::Error)]
pub enum CanonicalError {
    /// Format-specific error
    #[error("Format error: {0}")]
    Format(String),

    /// IO error during canonicalization
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(String),

    /// Hash computation error
    #[error("Hash error: {0}")]
    Hash(String),

    /// Invalid input
    #[error("Invalid input: {0}")]
    Invalid(String),
}

/// Result type for canonicalization operations
pub type Result<T> = std::result::Result<T, CanonicalError>;

/// Trait for deterministic canonicalization
///
/// Implementors must guarantee that:
/// 1. Same input always produces same output
/// 2. Output is in canonical form
/// 3. Canonical form is stable across versions
pub trait Canonicalizer {
    /// Input type for canonicalization
    type Input;

    /// Output type after canonicalization
    type Output;

    /// Canonicalize the input into a deterministic form
    ///
    /// # Errors
    ///
    /// Returns error if input cannot be canonicalized
    fn canonicalize(&self, input: Self::Input) -> Result<Self::Output>;

    /// Compute a canonical hash of the input
    ///
    /// # Errors
    ///
    /// Returns error if hashing fails
    fn hash(&self, input: Self::Input) -> Result<String>
    where
        Self::Output: AsRef<[u8]>,
    {
        let canonical = self.canonicalize(input)?;
        hash::compute_hash(&canonical)
    }
}

/// Canonical form wrapper
///
/// Guarantees that the contained value is in canonical form
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Canonical<T> {
    inner: T,
}

impl<T> Canonical<T> {
    /// Create a new canonical wrapper (unchecked)
    ///
    /// # Safety
    ///
    /// Caller must ensure the value is actually in canonical form
    pub fn new_unchecked(value: T) -> Self {
        Self { inner: value }
    }

    /// Get the inner value
    pub fn into_inner(self) -> T {
        self.inner
    }

    /// Get a reference to the inner value
    pub fn as_inner(&self) -> &T {
        &self.inner
    }
}

impl<T: fmt::Display> fmt::Display for Canonical<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T: AsRef<[u8]>> AsRef<[u8]> for Canonical<T> {
    fn as_ref(&self) -> &[u8] {
        self.inner.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_canonical_wrapper() {
        let canonical = Canonical::new_unchecked("test");
        assert_eq!(canonical.as_inner(), &"test");
        assert_eq!(canonical.into_inner(), "test");
    }
}
