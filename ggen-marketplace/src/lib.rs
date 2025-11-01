// Core modules
pub mod error;
pub mod models;
pub mod traits;
pub mod types;

// Implementation modules
pub mod backend;
pub mod storage;
pub mod crypto;
pub mod search;

// Template integration
pub mod template_search;

// GraphQL API (optional feature)
#[cfg(feature = "graphql")]
pub mod graphql;

// Re-exports for convenience
pub use backend::{CentralizedRegistry, LocalRegistry};

#[cfg(feature = "p2p")]
pub use backend::P2PRegistry;

pub use storage::{FilesystemStore, MemoryStore};
pub use crypto::{Ed25519Verifier, DefaultVerifier};
pub use search::TantivySearchEngine;
pub use template_search::{TemplateSearchEngine, TemplateSearchFilters, TemplateSearchQueryBuilder};

// Prelude for common imports
pub mod prelude {
    pub use crate::backend::{CentralizedRegistry, LocalRegistry};

    #[cfg(feature = "p2p")]
    pub use crate::backend::P2PRegistry;

    pub use crate::crypto::{DefaultVerifier, Ed25519Verifier};
    pub use crate::error::{MarketplaceError, Result};
    pub use crate::models::{
        ContentId, HashAlgorithm, Package, PackageId, Query, Version,
    };
    pub use crate::storage::{FilesystemStore, MemoryStore};
    pub use crate::traits::{CryptoVerifier, PackageStore, Registry, SearchEngine};
}
