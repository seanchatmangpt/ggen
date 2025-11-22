//! Unified Lockfile Trait System for ggen v4.0
//!
//! This module provides zero-cost abstractions for lockfile operations,
//! unifying the three existing lockfile implementations into a coherent
//! trait-based architecture.
//!
//! ## Architecture
//!
//! The unified lockfile system uses Rust's trait system to encode invariants:
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Trait Hierarchy                          │
//! ├─────────────────────────────────────────────────────────────┤
//! │  LockEntry          - Individual entry with metadata        │
//! │  Lockfile           - Container for entries                 │
//! │  LockfileFormat     - TOML/JSON serialization              │
//! │  LockfileManager    - CRUD operations                       │
//! │  PqcSignable        - Post-quantum signature support        │
//! │  CachingManager     - Dependency resolution cache           │
//! │  Validatable        - Integrity verification                │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Key Features
//!
//! - **Type-First Design**: Types encode invariants at compile time
//! - **Zero-Cost Abstractions**: Generics monomorphize to concrete code
//! - **Backward Compatible**: From<T> implementations for existing types
//! - **PQC Support**: ML-DSA-65 (Dilithium3) signature integration
//! - **Cache Coherency**: Shared LRU cache across managers
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::lockfile_unified::prelude::*;
//!
//! // Use unified manager for any lockfile type
//! let manager = UnifiedLockfileManager::new(Path::new("."));
//! let lockfile = manager.load_or_create()?;
//! ```

pub mod traits;
pub mod entry;
pub mod format;
pub mod cache;
pub mod validation;

pub mod prelude {
    //! Prelude for convenient imports
    pub use super::traits::*;
    pub use super::entry::*;
    pub use super::format::*;
    pub use super::cache::*;
    pub use super::validation::*;
}

// Re-exports for backward compatibility
pub use traits::{LockEntry, Lockfile, LockfileFormat, LockfileManager};
pub use entry::{UnifiedLockEntry, LockSource, PqcSignature, ExtendedMetadata};
pub use format::{TomlFormat, JsonFormat, FormatType, detect_format};
pub use cache::{CoherentCache, CacheStats};
pub use validation::{ValidationResult, IntegrityCheck};
