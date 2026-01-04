//! # gpack - Marketplace Package Distribution System
//!
//! This module implements the gpack distribution format for the ggen marketplace.
//! It provides functionality for package format handling, manifest management,
//! crates.io client integration, dependency resolution, validation, caching,
//! lockfile management, search capabilities, and quality scoring.
//!
//! ## Module Structure
//!
//! - `format` - Package format (.gpack) handling and serialization
//! - `manifest` - Package manifest parsing and validation
//! - `crates_client` - Crates.io registry client for package fetching
//! - `resolver` - Dependency resolution with version constraints
//! - `validator` - Package validation with FMEA/poka-yoke patterns
//! - `cache` - Local package caching system
//! - `error` - Error types for gpack operations
//! - `lockfile` - Lockfile generation and verification
//! - `search` - Marketplace search and discovery
//! - `quality` - Package quality scoring and metrics

pub mod cache;
pub mod crates_client;
pub mod error;
pub mod format;
pub mod lockfile;
pub mod manifest;
pub mod quality;
pub mod resolver;
pub mod search;
pub mod validator;

// Re-exports for convenient access

// Cache types (T009)
pub use cache::{
    CacheEntry, CacheManager, CacheStats, CacheTier, MetadataCacheEntry, PackageCache,
    CACHE_DIR, DEFAULT_TTL, METADATA_CACHE_DIR, METADATA_TTL,
};

// Crates.io client
pub use crates_client::CratesClient;

// Error types (T006)
pub use error::{ErrorCategory, GpackError, GpackResult};

// Format types
pub use format::{GpackFormat, PackageArchive};

// Lockfile types (T007)
pub use lockfile::{Lockfile, LockfileEntry, LockfileMetadata, LOCKFILE_FORMAT_VERSION, LOCKFILE_VERSION};

// Manifest types (T005)
pub use manifest::{
    BuildConfig, DetailedDependency, GpackManifest, PackageDependency, PackageMetadata,
    QualityTier, GPACK_SUFFIX, MAX_DESCRIPTION_LENGTH,
};

// Quality scoring
pub use quality::{
    IssueCategory, IssueSeverity, QualityGrade, QualityIssue, QualityMetrics, QualityScore,
    QualityScorer,
};

// Resolver types (T008)
pub use resolver::{DependencyResolver, ResolvedDependency, VersionConstraint};

// Search types
pub use search::{PackageSearch, SearchResult};

// Validator types (T010)
pub use validator::{
    FmeaEntry, FmeaValidation, PackageValidator, PokayokeGuard, PokayokeResult, Severity,
    ValidationError, ValidationResult, ValidationWarning, ValidatorQualityTier,
};
