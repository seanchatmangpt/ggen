//! Deterministic canonicalization primitives.
//!
//! Ported from `ggen-core::canonical` during the ggen-core retirement migration
//! (see `specs/014-ggen-core-replacement/tasks.md`). Only the `json` submodule is ported
//! here — the rest of `ggen-core::canonical` was found to be either dead or already
//! covered elsewhere in the workspace:
//!
//! - `ggen-core::canonical::hash::compute_hash` is byte-for-byte identical to
//!   [`crate::receipt::hash_data`] (both SHA-256 + `hex::encode`) — not re-ported; this
//!   module's [`Canonicalizer::hash`] default method calls `crate::receipt::hash_data`
//!   directly rather than duplicating it.
//! - `ggen-core::canonical::ttl::TtlCanonicalizer` does a naive line-sort of raw TTL text
//!   that drops every line starting with `@` (including `@prefix` declarations) — a real
//!   correctness bug, not just a weaker feature — and is superseded by
//!   `ggen-graph::graph::{canonical,hash}`'s real RDF C14N (blank-node neighborhood-signature
//!   renaming, lexicographic quad sort, BLAKE3 hash). Not ported.
//! - `ggen-core::canonical::rust::RustCanonicalizer` (a rustfmt subprocess wrapper) has zero
//!   external callers anywhere in the workspace. Not ported.
//!
//! `JsonCanonicalizer`/`canonicalize_json`/`canonicalize_json_str` (this module's `json`
//! submodule) have no equivalent anywhere else in the workspace and are the one piece
//! `/Users/sac/ggen/benches/canonical_bench.rs` needs to keep compiling.
//!
//! All operations are deterministic — same input always produces same output.

pub mod json;

/// Error type for canonicalization operations.
///
/// Trimmed down from `ggen-core::canonical::CanonicalError`'s 5 variants to just the one
/// actually constructed by the ported `json` submodule — the `Format`/`Io`/`Hash`/`Invalid`
/// variants existed only for the un-ported `ttl`/`rust`/`hash` submodules.
#[derive(Debug, thiserror::Error)]
pub enum CanonicalError {
    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(String),
}

/// Result type for canonicalization operations
pub type Result<T> = std::result::Result<T, CanonicalError>;

/// Trait for deterministic canonicalization.
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

    /// Compute a canonical hash of the input.
    ///
    /// Delegates to [`crate::receipt::hash_data`] — the crate's one authoritative SHA-256
    /// implementation — rather than a second, duplicate hash routine.
    ///
    /// # Errors
    ///
    /// Returns error if canonicalization fails
    fn hash(&self, input: Self::Input) -> Result<String>
    where
        Self::Output: AsRef<[u8]>,
    {
        let canonical = self.canonicalize(input)?;
        Ok(crate::receipt::hash_data(canonical.as_ref()))
    }
}

/// Canonical form wrapper.
///
/// Guarantees that the contained value is in canonical form.
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
    #[must_use]
    pub fn new_unchecked(value: T) -> Self {
        Self { inner: value }
    }

    /// Get the inner value
    #[must_use]
    pub fn into_inner(self) -> T {
        self.inner
    }

    /// Get a reference to the inner value
    #[must_use]
    pub fn as_inner(&self) -> &T {
        &self.inner
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Canonical<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T: AsRef<[u8]>> AsRef<[u8]> for Canonical<T> {
    fn as_ref(&self) -> &[u8] {
        self.inner.as_ref()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn test_canonical_wrapper() {
        let canonical = Canonical::new_unchecked("test");
        assert_eq!(canonical.as_inner(), &"test");
        assert_eq!(canonical.into_inner(), "test");
    }

    /// Proves `Canonicalizer::hash`'s default method actually reaches
    /// `crate::receipt::hash_data` (the shared SHA-256 implementation), not a stub —
    /// same expected digest as calling `hash_data` directly on the canonicalized bytes.
    #[test]
    fn test_hash_default_method_delegates_to_receipt_hash_data() {
        use crate::canonical::json::JsonCanonicalizer;

        let input = serde_json::json!({"b": 2, "a": 1});
        let canonicalizer = JsonCanonicalizer::new();
        let canonical = canonicalizer.canonicalize(input.clone()).unwrap();
        let expected = crate::receipt::hash_data(canonical.as_ref());

        let actual = canonicalizer.hash(input).unwrap();
        assert_eq!(actual, expected);
        assert_eq!(actual.len(), 64); // SHA-256 = 32 bytes = 64 hex chars
    }
}
