//! Marketplace domain models
//!
//! This module contains domain models and logic for marketplace operations.
//!
//! # Architecture
//!
//! The marketplace supports both legacy (v1) and RDF-backed (v2) implementations
//! through the `MarketplaceRegistry` adapter trait, enabling gradual migration
//! and parallel execution for validation.
//!
//! - `adapter`: Unified trait for multiple backend implementations
//! - `registry`: Legacy v1 marketplace registry
//! - `search`: Legacy v1 search implementation
//! - Future: `adapter_v2`: RDF-backed v2 implementation

pub mod adapter;
pub mod artifact_generator;
pub mod bundles;
pub mod guards;
pub mod install;
pub mod list;
pub mod mape_k_integration;
pub mod observability;
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

// Re-export commonly used types and functions
pub use adapter::{
    MarketplaceRegistry, PackageInfo, PackagePublish, SearchMatch, VersionInfo,
    ValidationResult, Recommendation, ComparisonResult, DependencyInfo, InstallationManifest,
    PublishSuccess,
};
pub use artifact_generator::{
    generate_packages_markdown, generate_registry_index, write_packages_markdown,
    write_registry_index,
};
pub use bundles::{generate_bundle_docs, BundleInstallManifest, BundleRegistry, SectorBundle};
pub use guards::{Guard, GuardCheckResult, GuardError, GuardResult, Severity, ValidationReceipt};
pub use install::{execute_install, InstallInput, InstallOptions, InstallResult};
pub use list::{execute_list, ListInput, ListOutput};
pub use mape_k_integration::{
    AutonomicMarketplace, AutonomicStatus, MarketplaceHealth, MarketplaceObservation,
    MarketplaceObservationType, ObservationStats, ReceiptObserver,
};
pub use observability::{
    HealthCheck, HealthStatus, MetricsSnapshot, ObservabilitySystem, PerformanceMetric,
};
pub use production_readiness::{
    CheckStatus, DeploymentGuide, ReadinessAssessment, ReadinessCheck, ReadinessChecker,
};
pub use publish::{execute_publish, PublishInput, PublishOutput};
pub use quality_autopilot::{
    apply_template_improvements, generate_improvement_plan, ImprovementPlan, ImprovementSuggestion,
};
pub use receipt_emitter::{
    emit_receipt_for_package, emit_receipts_for_marketplace, generate_validation_report,
    update_production_flags, PackageReport, ValidationReport,
};
pub use recommender::{
    PackageInfo, Recommendation, RecommendationReason, RecommendationSet, Recommender,
    RecommenderConfig,
};
pub use registry::{
    CacheManager, Dependency, PackageMetadata, Registry, RegistryIndex, VersionMetadata,
};
pub use search::{execute_search, SearchFilters, SearchInput, SearchResult};
pub use search_advanced::{
    AdvancedSearchQuery, AdvancedSearchResults, SearchEngine, SearchResultEntry, SearchStatistics,
    SortField,
};
pub use types::{Checksum, NonEmptyQuery, SemanticVersion, ValidatedPackageName};
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
