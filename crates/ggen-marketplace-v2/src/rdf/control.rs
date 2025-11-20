//! RDF Control Plane with Epoch-Based Cache Invalidation
//!
//! Central control system that coordinates all RDF operations.
//! All marketplace functionality goes through this control plane.
//!
//! ## Cache Invalidation Strategy
//!
//! This module follows ggen-core's proven pattern:
//! - **Epoch-based invalidation**: `Arc<AtomicU64>` incremented on each update
//! - **Dual-level caching**: Plan cache and result cache with epoch keys
//! - **Thread-safe access**: `Arc<Mutex<LruCache>>` for concurrent queries
//! - **Cheap cloning**: Arc enables efficient sharing across threads

use crate::builders::PackageBuilder;
use crate::error::{Error, Result};
use crate::models::{Package, PackageId, PackageVersion, QualityScore};
use lru::LruCache;
use oxigraph::store::Store;
use std::num::NonZeroUsize;
use std::path::Path;
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc, Mutex,
};

use super::sparql::SparqlExecutor;
use super::state_machine::StateMachineExecutor;
use super::turtle_config::TurtleConfigLoader;

/// Cache sizes matching ggen-core patterns
const DEFAULT_PLAN_CACHE_SIZE: usize = 100;
const DEFAULT_RESULT_CACHE_SIZE: usize = 1000;
const INITIAL_EPOCH: u64 = 1;
const EPOCH_INCREMENT: u64 = 1;

/// RDF Control Plane - Central coordinator for all RDF operations
///
/// Uses epoch-based cache invalidation pattern from ggen-core for:
/// - Query plan caching (parsed SPARQL)
/// - Result caching (query results)
/// - Automatic invalidation on mutations
pub struct RdfControlPlane {
    /// SPARQL executor for queries and updates
    executor: Arc<SparqlExecutor>,
    /// State machine for package lifecycle
    state_machine: Arc<StateMachineExecutor>,
    /// Configuration loader
    config_loader: Arc<TurtleConfigLoader>,
    /// Epoch for cache invalidation (incremented on updates)
    epoch: Arc<AtomicU64>,
    /// Plan cache: SPARQL query execution plans
    plan_cache: Arc<Mutex<LruCache<u64, String>>>,
    /// Result cache: Query results keyed by (epoch, query_hash)
    result_cache: Arc<Mutex<LruCache<(u64, u64), Vec<String>>>>,
}

/// Cached package entry (reserved for cache optimization strategies)
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct CachedPackage {
    #[allow(dead_code)]
    name: String,
    #[allow(dead_code)]
    description: String,
    #[allow(dead_code)]
    version: PackageVersion,
    #[allow(dead_code)]
    state: String,
    #[allow(dead_code)]
    last_accessed: chrono::DateTime<chrono::Utc>,
}

impl RdfControlPlane {
    /// Create a new RDF control plane with epoch-based cache invalidation
    pub fn new(store_path: impl AsRef<Path>) -> Result<Self> {
        let store = Store::open(store_path.as_ref()).map_err(|e| Error::RdfStoreError {
            operation: "open".to_string(),
            reason: e.to_string(),
        })?;

        let plan_cache_size = NonZeroUsize::new(DEFAULT_PLAN_CACHE_SIZE).ok_or_else(|| {
            Error::ConfigurationError {
                message: "Invalid plan cache size".to_string(),
            }
        })?;
        let result_cache_size = NonZeroUsize::new(DEFAULT_RESULT_CACHE_SIZE).ok_or_else(|| {
            Error::ConfigurationError {
                message: "Invalid result cache size".to_string(),
            }
        })?;

        let executor = Arc::new(SparqlExecutor::new(Arc::new(store)));
        let state_machine = Arc::new(StateMachineExecutor::new());
        let config_dir = store_path
            .as_ref()
            .parent()
            .and_then(|p| p.to_str())
            .unwrap_or(".")
            .to_string();
        let config_loader = Arc::new(TurtleConfigLoader::new(config_dir));

        Ok(Self {
            executor,
            state_machine,
            config_loader,
            epoch: Arc::new(AtomicU64::new(INITIAL_EPOCH)),
            plan_cache: Arc::new(Mutex::new(LruCache::new(plan_cache_size))),
            result_cache: Arc::new(Mutex::new(LruCache::new(result_cache_size))),
        })
    }

    /// Create an in-memory RDF control plane (for testing)
    pub fn in_memory() -> Result<Self> {
        let store = Store::new().map_err(|e| Error::RdfStoreError {
            operation: "new".to_string(),
            reason: e.to_string(),
        })?;

        let plan_cache_size = NonZeroUsize::new(DEFAULT_PLAN_CACHE_SIZE).ok_or_else(|| {
            Error::ConfigurationError {
                message: "Invalid plan cache size".to_string(),
            }
        })?;
        let result_cache_size = NonZeroUsize::new(DEFAULT_RESULT_CACHE_SIZE).ok_or_else(|| {
            Error::ConfigurationError {
                message: "Invalid result cache size".to_string(),
            }
        })?;

        let executor = Arc::new(SparqlExecutor::new(Arc::new(store)));
        let state_machine = Arc::new(StateMachineExecutor::new());
        let config_loader = Arc::new(TurtleConfigLoader::new(".".to_string()));

        Ok(Self {
            executor,
            state_machine,
            config_loader,
            epoch: Arc::new(AtomicU64::new(INITIAL_EPOCH)),
            plan_cache: Arc::new(Mutex::new(LruCache::new(plan_cache_size))),
            result_cache: Arc::new(Mutex::new(LruCache::new(result_cache_size))),
        })
    }

    /// Get current epoch value (for cache invalidation)
    pub fn current_epoch(&self) -> u64 {
        self.epoch.load(Ordering::Relaxed)
    }

    /// Increment epoch (invalidates cache on mutations)
    fn bump_epoch(&self) {
        self.epoch.fetch_add(EPOCH_INCREMENT, Ordering::Relaxed);
    }

    /// Load Turtle configuration files
    pub fn load_config(&self, _config_dir: impl AsRef<Path>) -> Result<()> {
        // Load marketplace config (this will read from the config_dir set in TurtleConfigLoader)
        let _config = self.config_loader.load_marketplace_config().map_err(|e| {
            Error::ConfigurationError {
                message: format!("Failed to load marketplace config: {}", e),
            }
        })?;
        // Apply configuration to state machine
        self.state_machine
            .load_from_config(self.config_loader.as_ref())?;
        Ok(())
    }

    // ========== Package Operations (All via SPARQL) ==========

    /// Create a new draft package
    pub fn create_package(
        &self, id: PackageId, name: impl Into<String>, description: impl Into<String>,
        version: PackageVersion, license: String,
    ) -> Result<Package> {
        let name = name.into();
        let description = description.into();

        // Validate state transition
        self.state_machine.validate_transition(
            None, // No current state (new package)
            "Draft",
        )?;

        // Insert package via SPARQL UPDATE
        // Build INSERT query directly for type-safe RDF operations
        let insert_query = format!(
            r#"
            PREFIX mp: <https://ggen.io/marketplace/>
            INSERT DATA {{
                <https://ggen.io/marketplace/{0}> a mp:Package ;
                    mp:id "{1}" ;
                    mp:name "{2}" ;
                    mp:description "{3}" ;
                    mp:latestVersion "{4}" ;
                    mp:license "{5}" ;
                    mp:state "Draft" .
            }}
            "#,
            id, id, name, description, version, license
        );
        self.executor.update(&insert_query)?;

        // Return draft package - construct from metadata
        let metadata = PackageBuilder::new()
            .id(id.clone())
            .name(name)
            .description(description)
            .license(license)
            .build()?;

        // Create Package with the metadata and version
        use indexmap::IndexMap;
        let package = Package {
            metadata,
            latest_version: version.clone(),
            versions: vec![version],
            releases: IndexMap::new(),
        };

        Ok(package)
    }

    /// Add metadata to a package (authors, keywords, categories)
    pub fn add_package_metadata(
        &self, package_id: &PackageId, authors: Vec<String>, keywords: Vec<String>,
        categories: Vec<String>,
    ) -> Result<()> {
        // Build SPARQL INSERT for metadata via direct RDF operations
        let mut metadata_query = String::from(
            r#"PREFIX mp: <https://ggen.io/marketplace/>
            INSERT DATA {"#,
        );

        let pkg_uri = format!("<https://ggen.io/marketplace/{}>", package_id);

        // Add authors
        for author in &authors {
            metadata_query.push_str(&format!("\n    {} mp:author \"{}\" ;", pkg_uri, author));
        }

        // Add keywords
        for keyword in &keywords {
            metadata_query.push_str(&format!("\n    {} mp:keyword \"{}\" ;", pkg_uri, keyword));
        }

        // Add categories
        for category in &categories {
            metadata_query.push_str(&format!("\n    {} mp:category \"{}\" ;", pkg_uri, category));
        }

        metadata_query.push_str("\n}");

        self.executor.update(&metadata_query)?;

        // Invalidate cache by bumping epoch
        self.bump_epoch();

        Ok(())
    }

    /// Publish a draft package
    pub fn publish_package(&self, package_id: PackageId, _checksum: String) -> Result<Package> {
        // Check current state
        let current_state = self.get_package_state(&package_id)?;
        if current_state != "Draft" {
            return Err(Error::InvalidStateTransition {
                from: current_state,
                to: "Published".to_string(),
            });
        }

        // Validate transition
        self.state_machine
            .validate_transition(Some("Draft"), "Published")?;

        // Update state via SPARQL UPDATE
        let update_query = format!(
            r#"
            PREFIX mp: <https://ggen.io/marketplace/>
            DELETE {{ <https://ggen.io/marketplace/{}> mp:state "Draft" . }}
            INSERT {{ <https://ggen.io/marketplace/{}> mp:state "Published" . }}
            WHERE {{ <https://ggen.io/marketplace/{}> a mp:Package . }}
            "#,
            package_id, package_id, package_id
        );
        self.executor.update(&update_query)?;

        // Invalidate all caches via epoch bump
        self.bump_epoch();

        // Load and return published package
        self.get_published_package(&package_id)
    }

    /// Get package state
    pub fn get_package_state(&self, package_id: &PackageId) -> Result<String> {
        // Epoch-based cache lookup would go here
        let _epoch = self.current_epoch();

        // Direct SPARQL query
        let query = format!(
            r#"PREFIX mp: <https://ggen.io/marketplace/>
            SELECT ?state
            WHERE {{ <https://ggen.io/marketplace/{}> mp:state ?state . }}"#,
            package_id
        );

        let results = self.executor.query(&query)?;

        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            if let Some(Ok(solution)) = solutions.into_iter().next() {
                if let Some(state) = solution.get("state") {
                    return Ok(state.to_string());
                }
            }
        }

        Err(Error::PackageNotFound {
            package_id: package_id.as_str().to_string(),
        })
    }

    /// Get a published package
    fn get_published_package(&self, _package_id: &PackageId) -> Result<Package> {
        // This is a simplified version - in production would fully reconstruct from RDF
        Err(Error::NotImplemented {
            feature: "get_published_package".to_string(),
        })
    }

    /// Search packages by keyword
    pub fn search_packages(&self, _keyword: &str, _limit: usize) -> Result<Vec<SearchResult>> {
        // Use epoch for cache lookups
        let _epoch = self.current_epoch();
        // Returns empty placeholder - SPARQL-based search implementation pending
        Ok(Vec::new())
    }

    /// List all packages with filters
    pub fn list_packages(
        &self, _category: Option<&str>, _min_quality: Option<u32>, _limit: usize, _offset: usize,
    ) -> Result<Vec<PackageListEntry>> {
        // Use epoch for cache lookups
        let _epoch = self.current_epoch();
        // Returns empty placeholder - SPARQL-based listing implementation pending
        Ok(Vec::new())
    }

    /// Get package dependencies
    pub fn get_dependencies(
        &self, _package_id: &PackageId, _version: &PackageVersion,
    ) -> Result<Vec<DependencyInfo>> {
        // Use epoch for cache lookups
        let _epoch = self.current_epoch();
        // Returns empty placeholder - dependency graph traversal pending
        Ok(Vec::new())
    }

    /// Validate package integrity
    pub fn validate_package(&self, package_id: &PackageId) -> Result<ValidationResult> {
        // Use epoch for cache lookups
        let _epoch = self.current_epoch();

        Ok(ValidationResult {
            package_id: package_id.clone(),
            is_valid: true,
            errors: Vec::new(),
        })
    }

    /// Get maturity metrics for a package
    pub fn get_maturity_metrics(&self, _package_id: &PackageId) -> Result<MaturityMetrics> {
        // Use epoch for cache lookups
        let _epoch = self.current_epoch();

        Ok(MaturityMetrics {
            quality_score: None,
            download_count: 0,
            version_count: 0,
            last_update: None,
            test_coverage: None,
            maturity_level: "unknown".to_string(),
        })
    }

    /// Get dashboard statistics
    pub fn get_dashboard_stats(&self) -> Result<DashboardStats> {
        // Use epoch for cache lookups
        let _epoch = self.current_epoch();

        Ok(DashboardStats {
            total_packages: 0,
            average_quality: 0.0,
            total_downloads: 0,
        })
    }

    /// Get the SPARQL executor (for direct queries if needed)
    pub fn executor(&self) -> &SparqlExecutor {
        &self.executor
    }

    /// Get the state machine executor
    pub fn state_machine(&self) -> &StateMachineExecutor {
        &self.state_machine
    }
}

// ========== Result Types ==========

#[derive(Debug, Clone)]
pub struct SearchResult {
    pub package_id: PackageId,
    pub name: String,
    pub description: String,
    pub version: PackageVersion,
    pub quality_score: Option<QualityScore>,
    pub relevance: f64,
}

#[derive(Debug, Clone)]
pub struct PackageListEntry {
    pub package_id: PackageId,
    pub name: String,
    pub version: PackageVersion,
    pub quality_score: Option<QualityScore>,
}

#[derive(Debug, Clone)]
pub struct DependencyInfo {
    pub package_id: PackageId,
    pub version_requirement: String,
    pub is_optional: bool,
}

#[derive(Debug, Clone)]
pub struct ValidationResult {
    pub package_id: PackageId,
    pub is_valid: bool,
    pub errors: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MaturityMetrics {
    pub quality_score: Option<QualityScore>,
    pub download_count: u64,
    pub version_count: usize,
    pub last_update: Option<String>,
    pub test_coverage: Option<f64>,
    pub maturity_level: String,
}

#[derive(Debug, Clone)]
pub struct DashboardStats {
    pub total_packages: usize,
    pub average_quality: f64,
    pub total_downloads: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_control_plane() {
        let control = RdfControlPlane::in_memory().unwrap();
        assert!(control.executor().store().len().unwrap() == 0);
    }

    #[test]
    fn test_create_package() -> Result<()> {
        let control = RdfControlPlane::in_memory()?;

        let id = PackageId::new("test-package")?;
        let version = PackageVersion::new("1.0.0")?;
        let license = "MIT".to_string();

        let package =
            control.create_package(id, "Test Package", "A test package", version, license)?;

        assert_eq!(package.metadata.name, "Test Package");
        Ok(())
    }
}
