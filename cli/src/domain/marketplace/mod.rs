//! Marketplace domain models
//!
//! This module contains domain models and logic for marketplace operations.

pub mod install;
pub mod list;
pub mod publish;
pub mod search;
pub mod update;

// Re-export commonly used types and functions
pub use search::{SearchFilters, SearchResult, search_packages, search_and_display};
pub use install::{InstallOptions, InstallResult, install_package, install_and_report};
pub use list::list_and_display;
pub use update::update_and_report;
pub use publish::publish_and_report;

// Placeholder types for phase 1 (to be implemented in phase 2)
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

#[derive(Debug, Clone)]
pub struct Registry;

#[derive(Debug, Clone)]
pub struct CacheManager;
