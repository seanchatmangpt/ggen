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

// Re-export commonly used types and functions
pub use search::{SearchInput, SearchFilters, SearchResult, execute_search};
pub use install::{InstallInput, InstallOptions, InstallResult, execute_install};
pub use list::{ListInput, ListOutput, execute_list};
pub use publish::{PublishInput, PublishOutput, execute_publish};
pub use update::{UpdateInput, UpdateOutput, execute_update};
pub use registry::{
    Registry, CacheManager, PackageMetadata, VersionMetadata, Dependency, RegistryIndex
};
pub use p2p::{
    P2PInput, P2PCommand, StartInput, PublishInput as P2PPublishInput,
    SearchInput as P2PSearchInput, PeerListInput, PeerInfoInput, BootstrapInput,
    execute_p2p_command
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
