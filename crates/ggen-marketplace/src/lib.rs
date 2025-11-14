#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness
#![allow(unexpected_cfgs)] // Allow graphql-server feature cfg - potential extension point

// Core modules
pub mod error;
pub mod models;
pub mod traits;
pub mod types;

// OpenTelemetry instrumentation
pub mod telemetry;

// Implementation modules
pub mod backend;
pub mod crypto;
pub mod search;
pub mod storage;

// Template integration
pub mod template_search;

// Re-exports for convenience
pub use backend::LocalRegistry;

pub use crypto::{DefaultVerifier, Ed25519Verifier};
pub use search::TantivySearchEngine;
pub use storage::{FilesystemStore, MemoryStore};
pub use template_search::{
    TemplateSearchEngine, TemplateSearchFilters, TemplateSearchQueryBuilder,
};

// Prelude for common imports
pub mod prelude {
    pub use crate::backend::LocalRegistry;

    pub use crate::crypto::{DefaultVerifier, Ed25519Verifier};
    pub use crate::error::{MarketplaceError, Result};
    pub use crate::models::{ContentId, HashAlgorithm, Package, PackageId, Query, Version};
    pub use crate::storage::{FilesystemStore, MemoryStore};
    pub use crate::traits::{CryptoVerifier, PackageStore, Registry, SearchEngine};
}
