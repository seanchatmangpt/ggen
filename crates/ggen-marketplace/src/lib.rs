//! # ggen-marketplace - Standalone Rust library for package marketplace operations
//!
//! This crate provides a complete, standalone implementation of a package marketplace
//! system for discovering, installing, and managing ggen template packs (gpacks). It
//! includes registry management, package storage, search capabilities, and cryptographic
//! verification.
//!
//! ## Architecture
//!
//! The marketplace is built on a trait-based architecture that allows for pluggable
//! implementations of core components:
//!
//! - **Registry**: Package discovery and management operations
//! - **PackageStore**: Content-addressable storage for package binaries
//! - **SearchEngine**: Full-text search with filtering and faceting
//! - **CryptoVerifier**: Cryptographic signature verification
//!
//! ## Features
//!
//! - **Package Management**: Install, update, and remove template packs
//! - **Local Registry**: File-system based registry implementation
//! - **Multiple Storage Backends**: Filesystem and in-memory storage options
//! - **Full-Text Search**: Tantivy-based search engine with advanced querying
//! - **Template Search**: Specialized search for template files within packs
//! - **Cryptographic Verification**: Ed25519 signature support for package integrity
//! - **OpenTelemetry**: Built-in observability support
//! - **Quality Metrics**: Package quality scoring and validation
//!
//! ## Quick Start
//!
//! ### Using the Local Registry
//!
//! ```rust,no_run
//! use ggen_marketplace::prelude::*;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Create a local registry
//! let registry = LocalRegistry::new("./marketplace")?;
//!
//! // Search for packages
//! let query = Query::new("rust web service");
//! let results = registry.search(&query).await?;
//!
//! println!("Found {} packages", results.len());
//! # Ok(())
//! # }
//! ```
//!
//! ### Searching Templates
//!
//! ```rust,no_run
//! use ggen_marketplace::TemplateSearchEngine;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let engine = TemplateSearchEngine::new()?;
//! let results = engine.search("rust cli").await?;
//! # Ok(())
//! # }
//! ```
//!
//! ## Module Organization
//!
//! - `backend` - Registry backend implementations
//! - `crypto` - Cryptographic verification implementations
//! - `models` - Core data models (Package, Version, Query, etc.)
//! - `search` - Search engine implementations
//! - `storage` - Package storage implementations
//! - `traits` - Core trait definitions
//! - `telemetry` - OpenTelemetry instrumentation
//! - `template_search` - Template-specific search functionality

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness
#![allow(unexpected_cfgs)] // Allow graphql-server feature cfg - potential extension point

// Core modules
pub mod assessment_helpers;
pub mod error;
pub mod maturity;
pub mod maturity_evaluator;
pub mod models;
pub mod traits;
pub mod types;
pub mod workflow_analytics;

// OpenTelemetry instrumentation
pub mod telemetry;

// Implementation modules
pub mod backend;
pub mod crypto;
pub mod search;
pub mod storage;

// Template integration
pub mod template_search;

// 8020 Innovation: Guards and validation
pub mod guards;

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

    pub use crate::assessment_helpers::{
        compare_assessments, export_as_csv, export_as_json, filter_by_dimensions, filter_by_level,
        filter_by_score_range, find_for_use_case, generate_all_assessments, get_recommendations,
        sample_packages,
    };
    pub use crate::crypto::{DefaultVerifier, Ed25519Verifier};
    pub use crate::error::{MarketplaceError, Result};
    pub use crate::guards::{
        Guard8020Coverage, Guard8020Result, ValidationReceipt, ValidationReceiptBuilder,
    };
    pub use crate::maturity::{
        AdoptionScore, DocumentationScore, MaintenanceScore, MaturityAssessment, MaturityLevel,
        PerformanceScore, SecurityScore, TestingScore,
    };
    pub use crate::maturity_evaluator::{EvaluationInput, MaturityDashboard, MaturityEvaluator};
    pub use crate::models::{ContentId, HashAlgorithm, Package, PackageId, Query, Version};
    pub use crate::storage::{FilesystemStore, MemoryStore};
    pub use crate::traits::{CryptoVerifier, PackageStore, Registry, SearchEngine};
    pub use crate::workflow_analytics::{
        DirectlyFollowsGraph, ProcessStatistics, WorkflowEvent, WorkflowEventType, WorkflowLog,
        WorkflowTrace,
    };
}
