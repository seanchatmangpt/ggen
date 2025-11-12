//! Marketplace domain models
//!
//! This module contains domain models and logic for marketplace operations.

pub mod install;
pub mod list;
pub mod p2p;
pub mod p2p_state;
pub mod publish;
pub mod registry;
pub mod search;
pub mod update;
pub mod validate;

// Re-export commonly used types and functions
pub use install::{execute_install, InstallInput, InstallOptions, InstallResult};
pub use list::{execute_list, ListInput, ListOutput};
pub use p2p::{
    execute_p2p_command, BootstrapInput, P2PCommand, P2PInput, PeerInfoInput, PeerListInput,
    PublishInput as P2PPublishInput, SearchInput as P2PSearchInput, StartInput,
};
pub use publish::{execute_publish, PublishInput, PublishOutput};
pub use registry::{
    CacheManager, Dependency, PackageMetadata, Registry, RegistryIndex, VersionMetadata,
};
pub use search::{execute_search, SearchFilters, SearchInput, SearchResult};
pub use update::{execute_update, UpdateInput, UpdateOutput};
pub use validate::{
    validate_all_packages, validate_package, CheckResult, PackageValidation, QualityCheck,
    RequiredCheck,
};

// Legacy types for backwards compatibility
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GpackMetadata {
    pub name: String,
    pub version: String,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchOptions {
    pub query: String,
    pub category: Option<String>,
}
