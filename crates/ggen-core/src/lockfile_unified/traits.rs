//! Core trait definitions for unified lockfile system
//!
//! This module defines zero-cost abstractions for lockfile operations,
//! using Rust's type system to encode invariants at compile time.

use ggen_utils::error::Result;
use serde::{de::DeserializeOwned, Serialize};
use std::path::Path;

/// Marker trait for lockfile entry types
///
/// Implementations must be serializable, cloneable, and thread-safe.
/// Uses associated types for zero-cost polymorphism.
pub trait LockEntry: Serialize + DeserializeOwned + Clone + Send + Sync + 'static {
    /// Unique identifier for this entry
    fn id(&self) -> &str;

    /// Version string (semver or custom)
    fn version(&self) -> &str;

    /// Integrity hash (SHA256, lowercase hex)
    fn integrity(&self) -> Option<&str>;

    /// Dependencies as list of entry IDs
    fn dependencies(&self) -> &[String];
}

/// Trait for lockfile containers (the outer structure)
///
/// Generic over the entry type for type safety.
pub trait Lockfile: Serialize + DeserializeOwned + Clone + Send + Sync + 'static {
    /// The entry type this lockfile contains
    type Entry: LockEntry;

    /// Lockfile schema version
    fn schema_version(&self) -> u32;

    /// When the lockfile was generated/updated (ISO 8601)
    fn generated_at(&self) -> &str;

    /// Get all entries as an iterator
    fn entries(&self) -> Box<dyn Iterator<Item = (&str, &Self::Entry)> + '_>;

    /// Get entry by ID
    fn get(&self, id: &str) -> Option<&Self::Entry>;

    /// Insert or update entry
    fn upsert(&mut self, id: String, entry: Self::Entry);

    /// Remove entry by ID, returns removed entry if it existed
    fn remove(&mut self, id: &str) -> Option<Self::Entry>;

    /// Number of entries
    fn len(&self) -> usize;

    /// Check if empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// Serialization format abstraction
///
/// Allows switching between TOML and JSON without changing business logic.
pub trait LockfileFormat: Clone + Send + Sync + 'static {
    /// File extension for this format (without dot)
    const EXTENSION: &'static str;

    /// MIME type for this format
    const MIME_TYPE: &'static str;

    /// Serialize lockfile to string
    fn serialize<L: Lockfile>(lockfile: &L) -> Result<String>;

    /// Deserialize string to lockfile
    fn deserialize<L: Lockfile>(content: &str) -> Result<L>;
}

/// Manager trait for lockfile operations
///
/// Provides CRUD operations with format abstraction.
pub trait LockfileManager: Send + Sync {
    /// The lockfile type this manager handles
    type Lockfile: Lockfile;

    /// The serialization format
    type Format: LockfileFormat;

    /// Path to the lockfile
    fn lockfile_path(&self) -> &Path;

    /// Load lockfile from path, returns None if not exists
    fn load(&self) -> Result<Option<Self::Lockfile>>;

    /// Save lockfile to path
    fn save(&self, lockfile: &Self::Lockfile) -> Result<()>;

    /// Create new empty lockfile
    fn create(&self) -> Self::Lockfile;

    /// Load existing or create new
    fn load_or_create(&self) -> Result<Self::Lockfile> {
        match self.load()? {
            Some(lockfile) => Ok(lockfile),
            None => Ok(self.create()),
        }
    }
}

/// Optional PQC (Post-Quantum Cryptography) signature support
///
/// Entries implementing this trait can be signed with ML-DSA-65 (Dilithium3).
pub trait PqcSignable: LockEntry {
    /// PQC signature (base64-encoded)
    fn pqc_signature(&self) -> Option<&str>;

    /// Public key for verification (base64-encoded)
    fn pqc_pubkey(&self) -> Option<&str>;

    /// Algorithm identifier (e.g., "ML-DSA-65")
    fn pqc_algorithm(&self) -> Option<&str>;

    /// Set PQC signature data
    fn set_pqc(
        &mut self, algorithm: Option<String>, signature: Option<String>, pubkey: Option<String>,
    );
}

/// Cache support for dependency resolution
///
/// Managers implementing this trait use memoization for performance.
pub trait CachingManager: LockfileManager {
    /// Resolve dependencies with caching
    fn resolve_deps_cached(&self, id: &str, version: &str) -> Result<Vec<String>>;

    /// Clear the dependency cache
    fn clear_cache(&self);

    /// Get cache statistics
    fn cache_stats(&self) -> super::cache::CacheStats;
}

/// Validation strategies for lockfile integrity
pub trait Validatable {
    /// Validate internal consistency
    fn validate(&self) -> Result<()>;

    /// Check for circular dependencies
    fn check_circular_deps(&self) -> Result<()>;

    /// Verify integrity hashes match content
    fn verify_integrity(&self) -> Result<()>;
}

#[cfg(test)]
mod tests {
    use super::*;

    // Note: LockEntry, PqcSignable use Serialize + DeserializeOwned + Clone
    // which require Sized, making them intentionally NOT dyn-compatible.
    // This is by design for zero-cost abstraction via monomorphization.
    // Only Validatable is dyn-compatible.
    fn _assert_validatable_object_safe(_: &dyn Validatable) {}
}
