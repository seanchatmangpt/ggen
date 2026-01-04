//! Marketplace domain models
//!
//! This module contains domain models and logic for marketplace operations.
//!
//! # Architecture
//!
//! NOTE: The CLI (`crates/ggen-cli/src/cmds/marketplace.rs`) uses `ggen-marketplace`
//! directly and bypasses this domain layer for marketplace operations.
//!
//! This module contains:
//! - `adapter`: Legacy trait (not used by CLI, kept for reference)
//! - `registry`: Legacy registry implementation
//! - `search`: Legacy search implementation
//! - `packs`: GGEN Packs command implementation
//! - `packs_services`: Core services for package management

pub mod adapter;
pub mod artifact_generator;
pub mod bundles;
pub mod fmea_validator; // Enterprise FMEA validation for Fortune 500 packages
pub mod guards;
pub mod quality_tiers; // Gold/Silver/Bronze quality tier classification
pub mod hook;
pub mod install;
pub mod list;
pub mod mape_k_integration;
pub mod observability;
pub mod packs;
pub mod packs_services;
pub mod production_readiness;
pub mod publish;
pub mod quality_autopilot;
pub mod receipt_emitter;
pub mod recommender;
pub mod registry;
pub mod search;
pub mod search_advanced;
pub mod types; // Poka-yoke types for error prevention
pub mod update;
pub mod validate;

#[cfg(test)]
#[path = "expert_tests.rs"]
mod expert_tests;

#[cfg(test)]
#[path = "integration_tests.rs"]
mod integration_tests;

/// ## 80/20 Consolidation
///
/// **Public API (20% core operations):** Only the 5 critical operations and their I/O types
/// - install, search, publish, update, list (command layer)
/// - Core domain types (types, registry)
///
/// **Internal Implementation (80% supporting):** Consolidated into private modules
/// - Validation (guards, validate)
/// - Reporting (artifact_generator, receipt_emitter)
/// - Metrics (observability, mape_k_integration)
/// - Suggestions (quality_autopilot, recommender)
/// - Specialized (bundles, packs, production_readiness, search_advanced, v2_adapter)
// ============================================================================
// CORE PUBLIC API (The 20% that delivers 80% of functionality)
// ============================================================================
pub use install::{execute_install, InstallInput, InstallOptions, InstallResult};
pub use list::{execute_list, ListInput, ListOutput};
pub use publish::{execute_publish, PublishInput, PublishOutput};
pub use search::{execute_search, SearchFilters, SearchInput, SearchResult};
pub use update::{execute_update, UpdateInput, UpdateOutput};

// Core domain types
pub use adapter::MarketplaceRegistry;
pub use registry::{PackageMetadata, Registry};
pub use types::{Checksum, NonEmptyQuery, SemanticVersion, ValidatedPackageName};

// ============================================================================
// INTERNAL SUPPORTING TYPES (Still exported for cross-module use, but not primary API)
// ============================================================================

// FMEA validation (enterprise-grade package validation)
pub use fmea_validator::{
    FmeaCategory, FmeaCheck, FmeaCheckResult, FmeaValidationReport, FmeaValidator,
    FmeaValidatorError,
};

// Quality tiers (Gold/Silver/Bronze package classification)
pub use quality_tiers::{
    PackageQualityInfo, QualityThresholds, QualityTier, QualityTierCalculator,
};

// Validation infrastructure (internal, but needed by multiple modules)
pub use guards::{Guard, GuardCheckResult, ValidationReceipt};

// Observability (internal, but needed for metrics collection)
pub use observability::{HealthCheck, HealthStatus};

// Reporting (internal, but needed for documentation generation)
pub use artifact_generator::{generate_packages_markdown, generate_registry_index};
pub use receipt_emitter::{emit_receipt_for_package, ValidationReport};

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
