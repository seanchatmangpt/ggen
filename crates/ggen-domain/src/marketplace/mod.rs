//! Marketplace domain models
//!
//! This module contains domain models and logic for marketplace operations.

pub mod install;
pub mod list;
pub mod publish;
pub mod registry;
pub mod search;
pub mod types; // Poka-yoke types for error prevention
pub mod update;
pub mod validate;
pub mod guards;
pub mod receipt_emitter;

#[cfg(test)]
#[path = "expert_tests.rs"]
mod expert_tests;

// Re-export commonly used types and functions
pub use install::{execute_install, InstallInput, InstallOptions, InstallResult};
pub use list::{execute_list, ListInput, ListOutput};
pub use publish::{execute_publish, PublishInput, PublishOutput};
pub use registry::{
    CacheManager, Dependency, PackageMetadata, Registry, RegistryIndex, VersionMetadata,
};
pub use search::{execute_search, SearchFilters, SearchInput, SearchResult};
pub use types::{Checksum, NonEmptyQuery, SemanticVersion, ValidatedPackageName};
pub use update::{execute_update, UpdateInput, UpdateOutput};
pub use validate::{
    validate_all_packages, validate_package, CheckResult, PackageValidation, QualityCheck,
    RequiredCheck,
};
pub use guards::{
    Guard, GuardCheckResult, GuardError, GuardResult, Severity, ValidationReceipt,
};
pub use receipt_emitter::{
    emit_receipt_for_package, emit_receipts_for_marketplace, generate_validation_report,
    update_production_flags, PackageReport, ValidationReport,
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
