//! Marketplace domain models
//!
//! This module contains domain models and logic for marketplace operations.

pub mod install;
pub mod search;

// Re-export commonly used types
pub use search::{SearchFilters, SearchResult, search_packages};

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
