//! Marketplace domain models
//!
//! This module contains domain models and logic for marketplace operations.

pub mod install;
pub mod list;
pub mod publish;
pub mod registry;
pub mod search;
pub mod update;

// Re-export commonly used types and functions
pub use search::{SearchArgs, SearchFilters, SearchResult, search_packages, search_and_display};
pub use install::{InstallArgs, InstallOptions, InstallResult, install_package, install_and_report};
pub use list::{ListArgs, list_and_display};
pub use update::{UpdateArgs, update_and_report};
pub use publish::{PublishArgs, publish_and_report};
pub use registry::{
    Registry, CacheManager, PackageMetadata, VersionMetadata, Dependency, RegistryIndex
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
