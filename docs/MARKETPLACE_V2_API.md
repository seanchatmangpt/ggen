# ggen-marketplace-v2 API Reference

## Overview

This document provides comprehensive API documentation for the ggen-marketplace-v2 crate, including all public types, traits, and usage examples.

---

## Table of Contents

1. [Core Traits](#core-traits)
   - [AsyncRepository](#asyncrepository)
   - [Queryable](#queryable)
   - [Installable](#installable)
   - [Validatable](#validatable)
   - [Signable](#signable)
   - [Observable](#observable)
2. [Registry Implementations](#registry-implementations)
   - [Registry](#registry)
   - [RdfRegistry](#rdfregistry)
3. [Search APIs](#search-apis)
   - [SearchEngine](#searchengine)
   - [SparqlSearchEngine](#sparqlsearchengine)
4. [Installation API](#installation-api)
   - [Installer](#installer)
5. [Data Models](#data-models)
6. [Error Handling](#error-handling)

---

## Core Traits

### AsyncRepository

The primary trait for package storage and retrieval operations.

```rust
use async_trait::async_trait;
use ggen_marketplace_v2::prelude::*;

#[async_trait]
pub trait AsyncRepository: Send + Sync {
    /// Iterator type for package collections
    type PackageIterator: Iterator<Item = Package> + Send;

    /// Retrieve a package by its unique identifier
    ///
    /// # Arguments
    /// * `id` - The package identifier
    ///
    /// # Returns
    /// * `Ok(Package)` - The package if found
    /// * `Err(Error::PackageNotFound)` - If package doesn't exist
    ///
    /// # Example
    /// ```rust
    /// use ggen_marketplace_v2::prelude::*;
    ///
    /// async fn get_package_example(registry: &impl AsyncRepository) {
    ///     let id = PackageId::new("my-package").unwrap();
    ///     match registry.get_package(&id).await {
    ///         Ok(pkg) => println!("Found: {}", pkg.metadata.name),
    ///         Err(e) => println!("Not found: {}", e),
    ///     }
    /// }
    /// ```
    async fn get_package(&self, id: &PackageId) -> Result<Package>;

    /// Retrieve a specific version of a package
    ///
    /// # Arguments
    /// * `id` - The package identifier
    /// * `version` - The semantic version to retrieve
    ///
    /// # Returns
    /// * `Ok(Package)` - Package filtered to requested version
    /// * `Err(Error::InvalidVersion)` - If version doesn't exist
    ///
    /// # Example
    /// ```rust
    /// async fn get_version_example(registry: &impl AsyncRepository) {
    ///     let id = PackageId::new("my-package").unwrap();
    ///     let version = PackageVersion::new("1.0.0").unwrap();
    ///
    ///     let pkg = registry.get_package_version(&id, &version).await?;
    ///     assert_eq!(pkg.versions.len(), 1);
    /// }
    /// ```
    async fn get_package_version(
        &self,
        id: &PackageId,
        version: &PackageVersion
    ) -> Result<Package>;

    /// Retrieve all packages in the repository
    ///
    /// # Returns
    /// * `Ok(Vec<Package>)` - All packages
    ///
    /// # Example
    /// ```rust
    /// async fn list_all_example(registry: &impl AsyncRepository) {
    ///     let packages = registry.all_packages().await?;
    ///     println!("Total packages: {}", packages.len());
    /// }
    /// ```
    async fn all_packages(&self) -> Result<Vec<Package>>;

    /// List all available versions of a package
    ///
    /// # Arguments
    /// * `id` - The package identifier
    ///
    /// # Returns
    /// * `Ok(Vec<PackageVersion>)` - All versions (sorted newest first)
    ///
    /// # Example
    /// ```rust
    /// async fn versions_example(registry: &impl AsyncRepository) {
    ///     let id = PackageId::new("my-package").unwrap();
    ///     let versions = registry.list_versions(&id).await?;
    ///
    ///     for version in versions {
    ///         println!("Available: {}", version);
    ///     }
    /// }
    /// ```
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<PackageVersion>>;

    /// Check if a package exists in the repository
    ///
    /// # Arguments
    /// * `id` - The package identifier
    ///
    /// # Returns
    /// * `Ok(true)` - Package exists
    /// * `Ok(false)` - Package not found
    ///
    /// # Example
    /// ```rust
    /// async fn exists_example(registry: &impl AsyncRepository) {
    ///     let id = PackageId::new("my-package").unwrap();
    ///     if registry.package_exists(&id).await? {
    ///         println!("Package is available");
    ///     }
    /// }
    /// ```
    async fn package_exists(&self, id: &PackageId) -> Result<bool>;
}
```

### Queryable

Trait for data sources that support custom queries.

```rust
#[async_trait]
pub trait Queryable: Send + Sync {
    /// The query type accepted by this data source
    type Query: Send + Sync;

    /// The result type returned by queries
    type QueryResult: Send + Sync;

    /// Execute a query against the data source
    ///
    /// # Example
    /// ```rust
    /// // For SPARQL queries
    /// let query = "SELECT ?pkg WHERE { ?pkg rdf:type ggen:Package }";
    /// let results = queryable.query(query).await?;
    /// ```
    async fn query(&self, query: Self::Query) -> Result<Self::QueryResult>;

    /// Explain a query for debugging purposes
    ///
    /// # Returns
    /// Human-readable query execution plan
    fn explain_query(&self, query: &Self::Query) -> String;
}
```

### Installable

Trait for package installation with dependency resolution.

```rust
#[async_trait]
pub trait Installable: Send + Sync {
    /// Install packages according to a manifest
    ///
    /// # Arguments
    /// * `manifest` - Installation manifest with packages and dependencies
    ///
    /// # Returns
    /// * `Ok(InstallationManifest)` - Updated manifest after installation
    ///
    /// # Example
    /// ```rust
    /// async fn install_example(installer: &impl Installable) {
    ///     let manifest = InstallationManifest {
    ///         id: Uuid::new_v4(),
    ///         packages: vec![PackageId::new("my-package").unwrap()],
    ///         dependencies: IndexMap::new(),
    ///         install_path: "/usr/local/ggen".to_string(),
    ///         planned_at: Utc::now(),
    ///     };
    ///
    ///     let result = installer.install(manifest).await?;
    ///     println!("Installed {} packages", result.dependencies.len());
    /// }
    /// ```
    async fn install(&self, manifest: InstallationManifest) -> Result<InstallationManifest>;

    /// Resolve all dependencies for a package
    ///
    /// # Arguments
    /// * `id` - Root package identifier
    /// * `version` - Root package version
    ///
    /// # Returns
    /// * `Ok(Vec<(PackageId, PackageVersion)>)` - Resolved dependency tree
    ///
    /// # Example
    /// ```rust
    /// async fn resolve_example(installer: &impl Installable) {
    ///     let id = PackageId::new("my-package").unwrap();
    ///     let version = PackageVersion::new("1.0.0").unwrap();
    ///
    ///     let deps = installer.resolve_dependencies(&id, &version).await?;
    ///     for (dep_id, dep_version) in deps {
    ///         println!("Depends on: {}@{}", dep_id, dep_version);
    ///     }
    /// }
    /// ```
    async fn resolve_dependencies(
        &self,
        id: &PackageId,
        version: &PackageVersion
    ) -> Result<Vec<(PackageId, PackageVersion)>>;

    /// Simulate installation without making changes
    ///
    /// # Returns
    /// Human-readable installation plan
    async fn dry_run_install(&self, manifest: &InstallationManifest) -> Result<String>;
}
```

### Validatable

Trait for package and manifest validation.

```rust
#[async_trait]
pub trait Validatable: Send + Sync {
    /// Custom validation result type
    type ValidationResult: Send + Sync;

    /// Validate a package
    async fn validate(&self, package: &Package) -> Result<Self::ValidationResult>;

    /// Validate a manifest
    async fn validate_manifest(&self, manifest: &Manifest) -> Result<Self::ValidationResult>;

    /// Check if validation result indicates success
    fn validation_passes(&self, result: &Self::ValidationResult) -> bool;
}
```

### Signable

Trait for cryptographic operations.

```rust
pub trait Signable {
    /// Sign data with private key
    ///
    /// # Arguments
    /// * `data` - Bytes to sign
    ///
    /// # Returns
    /// * `Ok(String)` - Base64-encoded signature
    fn sign(&self, data: &[u8]) -> Result<String>;

    /// Verify a signature
    ///
    /// # Arguments
    /// * `data` - Original data
    /// * `signature` - Base64-encoded signature to verify
    ///
    /// # Returns
    /// * `Ok(true)` - Signature is valid
    /// * `Ok(false)` - Signature is invalid
    fn verify(&self, data: &[u8], signature: &str) -> Result<bool>;

    /// Get the public key
    fn public_key(&self) -> String;
}
```

### Observable

Trait for metrics and event collection.

```rust
#[async_trait]
pub trait Observable: Send + Sync {
    /// Record a numeric metric
    async fn record_metric(&self, name: &str, value: f64) -> Result<()>;

    /// Record an event with data
    async fn record_event(&self, name: &str, data: &str) -> Result<()>;

    /// Get metrics summary
    async fn get_metrics(&self) -> Result<String>;
}
```

---

## Registry Implementations

### Registry

In-memory package registry for testing and lightweight use cases.

```rust
use ggen_marketplace_v2::Registry;

// Create a new registry with capacity
let registry = Registry::new(1000).await;

// Publish a package
let package = Package { /* ... */ };
registry.publish(package).await?;

// Search packages
let results = registry.search("query").await?;

// Get package
let pkg = registry.get_package(&PackageId::new("my-pkg")?).await?;
```

### RdfRegistry

RDF-backed semantic registry using oxigraph.

```rust
use ggen_marketplace_v2::RdfRegistry;
use std::sync::Arc;

// Create RDF registry (initializes ontology automatically)
let registry = RdfRegistry::new();

// Insert package as RDF triples
registry.insert_package_rdf(&package).await?;

// Batch insert for performance
let packages = vec![pkg1, pkg2, pkg3];
let inserted = registry.batch_insert_packages(packages).await?;
println!("Inserted {} packages", inserted);

// Execute SPARQL query directly
let query = r#"
    SELECT ?pkg WHERE {
        ?pkg rdf:type <https://ggen.io/marketplace/classes/Package> .
    }
"#;
let results = registry.query_sparql(query)?;

// Get statistics
let stats = registry.stats();
println!("Total queries: {}", stats.total_queries);
```

#### RdfRegistry Methods

| Method | Description |
|--------|-------------|
| `new()` | Create new registry with initialized ontology |
| `insert_package_rdf(&Package)` | Insert single package as RDF |
| `batch_insert_packages(Vec<Package>)` | Batch insert for efficiency |
| `query_sparql(&str)` | Execute raw SPARQL query |
| `stats()` | Get registry statistics |

---

## Search APIs

### SearchEngine

Basic text-based search engine.

```rust
use ggen_marketplace_v2::SearchEngine;

let engine = SearchEngine::new(registry);

// Simple search
let results = engine.search("database").await?;

// Search with filters
let results = engine.search_with_filters(
    "database",
    SearchFilters::new()
        .with_quality(80)
        .with_author("Alice")
        .with_limit(10)
).await?;
```

### SparqlSearchEngine

Semantic search using SPARQL queries.

```rust
use ggen_marketplace_v2::SparqlSearchEngine;
use oxigraph::store::Store;
use std::sync::Arc;

let store = Arc::new(Store::new()?);
let engine = SparqlSearchEngine::new(store);

// Search by name
let packages = engine.search_by_name("database")?;

// Search by description content
let packages = engine.search_by_description("high performance")?;

// Search by keyword/category
let packages = engine.search_by_keyword("async")?;

// Search by author
let packages = engine.search_by_author("Alice")?;

// Get trending packages (by downloads)
let trending = engine.trending_packages(10)?;

// Get recent packages
let recent = engine.recent_packages(10)?;

// Search by quality score
let quality = engine.search_by_quality(90)?;

// Get all packages
let all = engine.all_packages()?;
```

#### SearchFilters

Builder pattern for search filters.

```rust
use ggen_marketplace_v2::search_sparql::SearchFilters;

let filters = SearchFilters::new()
    .with_quality(80)        // Minimum quality score
    .with_author("Alice")    // Filter by author
    .with_keyword("async")   // Filter by keyword
    .with_limit(50);         // Maximum results

assert_eq!(filters.min_quality, Some(80));
assert_eq!(filters.author, Some("Alice".to_string()));
assert_eq!(filters.keyword, Some("async".to_string()));
assert_eq!(filters.limit, 50);
```

---

## Installation API

### Installer

Package installer with dependency resolution.

```rust
use ggen_marketplace_v2::Installer;

// Create installer with repository
let installer = Installer::new(registry);

// Create installation manifest
let manifest = installer.create_manifest(
    vec![
        PackageId::new("package-a")?,
        PackageId::new("package-b")?,
    ],
    "/usr/local/ggen".to_string()
).await?;

println!("Resolved {} dependencies", manifest.dependencies.len());

// Validate manifest
installer.validate_manifest(&manifest).await?;

// Dry run (simulation)
let plan = installer.dry_run(&manifest).await?;
println!("{}", plan);

// Execute installation
let result = installer.install(manifest).await?;
```

#### InstallationPlan

Result of dry-run installation.

```rust
pub struct InstallationPlan {
    /// Installation ID
    pub id: Uuid,
    /// Packages to install
    pub packages: Vec<PackageInstallPlan>,
    /// Total size in bytes
    pub total_size: u64,
    /// Estimated installation time
    pub estimated_time: Duration,
}

// Display format:
// Installation Plan <uuid>
// Packages: 5
// Total size: 1 MB
// Estimated time: 10.0s
//
//   - package-a@1.0.0 (100 KB)
//   - package-b@2.0.0 (200 KB)
```

---

## Data Models

### PackageId

Validated package identifier.

```rust
use ggen_marketplace_v2::models::PackageId;

// Create validated ID
let id = PackageId::new("my-package")?;

// Validation rules:
// - Non-empty
// - Only alphanumeric, hyphens, underscores
// - Lowercase (auto-normalized)
// - Max 200 characters
// - Cannot start/end with hyphen

// Valid examples
PackageId::new("my-package")?;      // OK
PackageId::new("package_123")?;     // OK
PackageId::new("MY-PACKAGE")?;      // OK (normalized to lowercase)

// Invalid examples
PackageId::new("")?;                // Error: empty
PackageId::new("-invalid")?;        // Error: starts with hyphen
PackageId::new("has spaces")?;      // Error: invalid characters
```

### PackageVersion

Semantic version with validation.

```rust
use ggen_marketplace_v2::models::PackageVersion;

// Create validated version
let version = PackageVersion::new("1.0.0")?;

// Supports semver format
PackageVersion::new("1.0.0")?;           // OK
PackageVersion::new("v1.0.0")?;          // OK (v prefix stripped)
PackageVersion::new("1.0.0-alpha")?;     // OK (pre-release)
PackageVersion::new("1.0.0+build")?;     // OK (build metadata)

// Comparison
let v1 = PackageVersion::new("1.0.0")?;
let v2 = PackageVersion::new("2.0.0")?;
assert!(v2 > v1);
```

### QualityScore

Quality score (1-100) with production readiness checks.

```rust
use ggen_marketplace_v2::models::QualityScore;

let score = QualityScore::new(95)?;

// Quality checks
assert!(score.is_production_ready());  // >= 95
assert!(!score.needs_improvement());   // 80-94
assert!(!score.not_ready());           // < 80

// Display
println!("{}", score);  // "95%"
```

### Package

Complete package with metadata and versions.

```rust
use ggen_marketplace_v2::models::{Package, PackageMetadata, ReleaseInfo};

let package = Package {
    metadata: PackageMetadata::new(
        PackageId::new("my-package")?,
        "My Package",
        "A fantastic package",
        "MIT"
    ),
    latest_version: PackageVersion::new("1.0.0")?,
    versions: vec![
        PackageVersion::new("1.0.0")?,
        PackageVersion::new("0.9.0")?,
    ],
    releases: IndexMap::new(),  // Version -> ReleaseInfo
};
```

### InstallationManifest

Installation plan with resolved dependencies.

```rust
use ggen_marketplace_v2::models::InstallationManifest;

let manifest = InstallationManifest {
    id: Uuid::new_v4(),
    packages: vec![PackageId::new("my-package")?],
    dependencies: {
        let mut deps = IndexMap::new();
        deps.insert(
            PackageId::new("my-package")?,
            PackageVersion::new("1.0.0")?
        );
        deps.insert(
            PackageId::new("dep-package")?,
            PackageVersion::new("2.0.0")?
        );
        deps
    },
    install_path: "/usr/local/ggen".to_string(),
    planned_at: Utc::now(),
};
```

---

## Error Handling

### Error Types

```rust
use ggen_marketplace_v2::error::Error;

#[derive(Error, Debug)]
pub enum Error {
    // Package errors
    #[error("Package not found: {package_id}")]
    PackageNotFound { package_id: String },

    #[error("Invalid package ID format: {reason}")]
    InvalidPackageId { reason: String },

    #[error("Invalid semantic version: {version}, reason: {reason}")]
    InvalidVersion { version: String, reason: String },

    #[error("Package already exists: {package_id}")]
    PackageAlreadyExists { package_id: String },

    #[error("Version {version} already exists for package {package_id}")]
    VersionAlreadyExists { package_id: String, version: String },

    // Dependency errors
    #[error("Dependency resolution failed for {package_id}: {reason}")]
    DependencyResolutionFailed { package_id: String, reason: String },

    // Operation errors
    #[error("Installation failed: {reason}")]
    InstallationFailed { reason: String },

    #[error("Validation failed: {reason}")]
    ValidationFailed { reason: String },

    #[error("Security check failed: {reason}")]
    SecurityCheckFailed { reason: String },

    // RDF/SPARQL errors
    #[error("SPARQL error in query: {query}, reason: {reason}")]
    SparqlError { query: String, reason: String },

    #[error("RDF store error during {operation}: {reason}")]
    RdfStoreError { operation: String, reason: String },

    // ... more error types
}
```

### Error Construction Helpers

```rust
// Create errors using helper methods
let err = Error::package_not_found("my-package");
let err = Error::invalid_package_id("contains spaces");
let err = Error::invalid_version("1.0", "missing patch");
let err = Error::dependency_resolution_failed("pkg", "cycle detected");
let err = Error::search_error("SPARQL syntax error");
```

### Result Type

```rust
use ggen_marketplace_v2::error::Result;

// Alias for std::result::Result<T, Error>
async fn example() -> Result<Package> {
    let id = PackageId::new("my-package")?;  // Uses ? operator
    // ...
}
```

---

## Utility Traits

### Filter

Filter trait for query results.

```rust
pub trait Filter<T>: Send + Sync {
    fn matches(&self, item: &T) -> bool;

    fn filter_items(&self, items: Vec<T>) -> Vec<T> {
        items.into_iter().filter(|item| self.matches(item)).collect()
    }
}
```

### Transformer

Transform items between types.

```rust
pub trait Transformer<T, U>: Send + Sync {
    fn transform(&self, item: T) -> Result<U>;

    async fn transform_batch(&self, items: Vec<T>) -> Result<Vec<U>>;
}
```

### Ranker

Rank search results.

```rust
pub trait Ranker {
    fn rank(&self, results: &mut [SearchResult]);
}

// Default implementation ranks by relevance, then downloads
let ranker = DefaultRanker;
ranker.rank(&mut results);
```

### Cache

Generic caching interface.

```rust
pub trait Cache<K, V>: Send + Sync
where
    K: Send + Sync,
    V: Send + Sync,
{
    fn get(&self, key: &K) -> Option<V>;
    fn insert(&self, key: K, value: V);
    fn remove(&self, key: &K) -> Option<V>;
    fn clear(&self);
    fn size(&self) -> usize;
}
```

### Builder

Type-safe builder pattern.

```rust
pub trait Builder<T>: Sized {
    fn build(self) -> Result<T>;
    fn validate(&self) -> Result<()>;
}
```

---

## Migration API

### MigrationCoordinator

Coordinate v1 to v2 data migration.

```rust
use ggen_marketplace_v2::migration::MigrationCoordinator;

let coordinator = MigrationCoordinator::new(Arc::clone(&v2_registry));

// Full migration
let report = coordinator.migrate_packages(v1_packages).await?;
println!("Success rate: {:.1}%", report.success_rate() * 100.0);

// Incremental migration (skip existing)
let report = coordinator.incremental_migrate(v1_packages).await?;

// Verify migration integrity
let verification = coordinator.verify_migration(v1_packages).await?;
if verification.is_valid() {
    println!("All packages verified!");
}
```

### ConsistencyChecker

Check data consistency between v1 and v2.

```rust
use ggen_marketplace_v2::migration::ConsistencyChecker;

let checker = ConsistencyChecker::new(Arc::clone(&v2_registry));

// Check single package
let result = checker.check_package_consistency(&id, &v1_package).await?;
if !result.is_consistent {
    println!("Differences: {:?}", result.differences);
}

// Periodic check of all packages
let report = checker.periodic_check(v1_packages).await?;
println!("Consistency: {:.1}%", report.consistency_rate() * 100.0);
```

---

## Prelude

Import common types with the prelude:

```rust
use ggen_marketplace_v2::prelude::*;

// Includes:
// - Error, Result
// - Installer
// - MetricsCollector
// - Package, PackageId, PackageMetadata, PackageVersion, Manifest
// - Registry, RdfRegistry
// - SearchEngine, SparqlSearchEngine
// - SignatureVerifier
// - AsyncRepository, Installable, Observable, Queryable, Signable, Validatable
// - V3OptimizedRegistry
// - Validator
```

---

## Best Practices

### 1. Use the Prelude

```rust
use ggen_marketplace_v2::prelude::*;
```

### 2. Prefer RdfRegistry for Production

```rust
// Development/testing
let registry = Registry::new(1000).await;

// Production
let registry = RdfRegistry::new();
```

### 3. Use Batch Operations

```rust
// Slow: individual inserts
for pkg in packages {
    registry.insert_package_rdf(&pkg).await?;
}

// Fast: batch insert
registry.batch_insert_packages(packages).await?;
```

### 4. Handle Errors Properly

```rust
match registry.get_package(&id).await {
    Ok(pkg) => process(pkg),
    Err(Error::PackageNotFound { .. }) => create_package(),
    Err(e) => return Err(e),
}
```

### 5. Use Dry-Run Before Installation

```rust
let plan = installer.dry_run(&manifest).await?;
println!("{}", plan);  // Review before proceeding
let result = installer.install(manifest).await?;
```

---

## Version Compatibility

| API | Stable Since | Notes |
|-----|--------------|-------|
| `AsyncRepository` | 2.0.0 | Core trait |
| `RdfRegistry` | 2.0.0 | RDF backend |
| `SparqlSearchEngine` | 2.0.0 | SPARQL search |
| `MigrationCoordinator` | 2.0.0 | v1->v2 migration |
| `V3OptimizedRegistry` | 2.2.0 | Performance optimized |

---

## See Also

- [Architecture Guide](./MARKETPLACE_V2_ARCHITECTURE.md)
- [Migration Guide](./MARKETPLACE_V2_MIGRATION.md)
- [SPARQL Patterns](./GGEN_V3_SPARQL_PATTERNS_AND_OPTIMIZATION.md)
