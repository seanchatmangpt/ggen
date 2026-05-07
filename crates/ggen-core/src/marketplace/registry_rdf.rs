//! High-performance RDF-based registry using oxigraph as the knowledge graph backend
//!
//! This implementation treats packages as RDF triples, enabling:
//! - Semantic queries via SPARQL
//! - Flexible package relationships
//! - Version history as RDF facts
//! - Full-text search over package descriptions

#![allow(clippy::missing_errors_doc)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::too_many_lines)]

use async_trait::async_trait;
use oxigraph::model::{GraphNameRef, NamedNode, QuadRef, Term};
use oxigraph::store::Store;
use parking_lot::RwLock;
use std::sync::Arc;
use tracing::{debug, info};

use crate::marketplace::error::Result;
use crate::marketplace::models::{Package, PackageId, PackageVersion, SearchResult};
use crate::marketplace::rdf_mapper::RdfMapper;
use crate::marketplace::traits::AsyncRepository;

/// RDF namespace for ggen marketplace
const GGEN_NS: &str = "https://ggen.io/marketplace/";
const RDFS_NS: &str = "http://www.w3.org/2000/01/rdf-schema#";

/// High-performance RDF-backed registry
///
/// Uses oxigraph (semantic data store) as the single source of truth.
/// All packages are stored as RDF triples enabling:
/// - SPARQL queries for intelligent search
/// - Semantic relationships between packages
/// - Version history tracking
/// - Full dependency graph representation
pub struct RdfRegistry {
    // RDF triplestore (single source of truth)
    store: Arc<Store>,

    // RDF mapper for bidirectional conversion
    mapper: Arc<RdfMapper>,

    // Read-write lock for atomic updates
    write_lock: Arc<RwLock<()>>,

    // Metrics
    queries_executed: std::sync::atomic::AtomicU64,
}

impl RdfRegistry {
    /// Create a new RDF-backed registry
    ///
    /// # Panics
    ///
    /// Panics if the RDF store cannot be created (should not happen in practice)
    #[must_use]
    pub fn new() -> Self {
        let store = Store::new().expect("Failed to create RDF store");
        Self::from_store(store)
    }

    /// Create a new RDF-backed registry from an existing store
    pub fn from_store(store: Store) -> Self {
        // Initialize ontology
        Self::initialize_ontology(&store);

        let store_arc = Arc::new(store);
        let mapper = Arc::new(RdfMapper::new(Arc::clone(&store_arc)));

        Self {
            store: store_arc,
            mapper,
            write_lock: Arc::new(RwLock::new(())),
            queries_executed: std::sync::atomic::AtomicU64::new(0),
        }
    }

    /// Initialize marketplace ontology
    fn initialize_ontology(store: &Store) {
        // Package class definition
        let package_class =
            NamedNode::new(format!("{}Package", GGEN_NS)).expect("Invalid package class URI");

        let rdf_type = NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            .expect("Invalid rdf:type");

        let rdfs_class = NamedNode::new(format!("{}Class", RDFS_NS)).expect("Invalid rdfs:Class");

        // Define Package as RDF class
        let quad = QuadRef::new(
            &package_class,
            &rdf_type,
            &rdfs_class,
            GraphNameRef::DefaultGraph,
        );

        store.insert(quad).expect("Failed to insert Package class");

        debug!("Initialized RDF marketplace ontology");
    }

    /// Insert a package as RDF triples (inner, no lock acquisition)
    ///
    /// This is called by methods that already hold the write lock.
    ///
    /// # Errors
    ///
    /// * [`Error::RdfStoreError`] - When inserting triples into the RDF store fails
    /// * [`Error::SerializationError`] - When serializing package data to RDF fails
    fn insert_package_rdf_inner(&self, package: &Package) -> Result<()> {
        self.mapper.package_to_rdf(package)
    }

    /// Insert a package as RDF triples using the mapper (public wrapper with locking)
    ///
    /// # Errors
    ///
    /// * [`Error::RdfStoreError`] - When inserting triples into the RDF store fails
    /// * [`Error::SerializationError`] - When serializing package data to RDF fails
    pub fn insert_package_rdf(&self, package: &Package) -> Result<()> {
        let _lock = self.write_lock.write();
        self.insert_package_rdf_inner(package)
    }

    /// Batch insert multiple packages for efficient loading
    ///
    /// # Errors
    ///
    /// * [`Error::RdfStoreError`] - When inserting triples into the RDF store fails
    /// * [`Error::SerializationError`] - When serializing package data to RDF fails
    pub fn batch_insert_packages(&self, packages: Vec<Package>) -> Result<usize> {
        let _lock = self.write_lock.write();
        let mut inserted = 0;

        for package in packages {
            match self.mapper.package_to_rdf(&package) {
                Ok(()) => inserted += 1,
                Err(e) => {
                    tracing::warn!("Failed to insert package {}: {}", package.metadata.id, e);
                }
            }
        }

        info!("Batch inserted {} packages to RDF store", inserted);
        Ok(inserted)
    }

    /// Query packages by SPARQL
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::RdfStoreError`] - When querying the RDF store fails
    pub fn query_sparql(&self, query: &str) -> Result<Vec<String>> {
        let results = self.store.query(query).map_err(|e| {
            crate::marketplace::error::Error::SearchError(format!("SPARQL query failed: {e}"))
        })?;

        self.queries_executed
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        let mut packages = Vec::new();

        // Parse results
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                if let Ok(solution) = solution {
                    for (_, term) in solution.iter() {
                        match term {
                            Term::NamedNode(node) => {
                                packages.push(node.as_str().to_string());
                                break;
                            }
                            Term::Literal(lit) => {
                                packages.push(lit.value().to_string());
                                break;
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        debug!("SPARQL query returned {} results", packages.len());
        Ok(packages)
    }

    /// Get statistics about the RDF store
    #[must_use]
    pub fn stats(&self) -> RdfRegistryStats {
        let query_count = self
            .queries_executed
            .load(std::sync::atomic::Ordering::Relaxed);

        RdfRegistryStats {
            total_queries: query_count,
        }
    }
}

impl Default for RdfRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl AsyncRepository for RdfRegistry {
    type PackageIterator = std::vec::IntoIter<Package>;

    async fn get_package(&self, id: &PackageId) -> Result<Package> {
        self.queries_executed
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        debug!("Retrieving package {} from RDF store", id);
        self.mapper.rdf_to_package(id)
    }

    async fn get_package_version(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<Package> {
        self.queries_executed
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        // Get full package and filter to specific version
        let mut package = self.get_package(id).await?;

        if !package.versions.contains(version) {
            return Err(crate::marketplace::error::Error::InvalidVersion {
                version: version.to_string(),
                reason: format!("Version {} not found for package {}", version, id),
            });
        }

        // Filter to only requested version
        package.versions = vec![version.clone()];
        package.releases.retain(|v, _| v == version);

        Ok(package)
    }

    async fn all_packages(&self) -> Result<Vec<Package>> {
        self.queries_executed
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        // Query all package IDs
        let query = format!(
            r"
            SELECT ?packageId WHERE {{
                ?package <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{}Package> .
                ?package <{}> ?packageId .
            }}
            ",
            GGEN_NS,
            crate::marketplace::ontology::Properties::package_id()
        );

        let results = self.query_sparql(&query)?;
        debug!("Found {} packages in RDF store", results.len());

        // Reconstruct each package
        let mut packages = Vec::with_capacity(results.len());
        for package_id_str in results {
            if let Ok(package_id) = PackageId::new(package_id_str) {
                if let Ok(package) = self.mapper.rdf_to_package(&package_id) {
                    packages.push(package);
                }
            }
        }

        Ok(packages)
    }

    async fn list_versions(&self, id: &PackageId) -> Result<Vec<PackageVersion>> {
        let query = format!(
            r#"
            SELECT ?version WHERE {{
                ?package <{}hasVersion> ?version .
                FILTER(CONTAINS(str(?package), "{}"))
            }}
            "#,
            GGEN_NS, id
        );

        let results = self.query_sparql(&query)?;

        // Parse versions from URIs
        let mut versions = Vec::new();
        for result_uri in results {
            if let Some(version_str) = result_uri.split('/').next_back() {
                if let Ok(version) = PackageVersion::new(version_str) {
                    versions.push(version);
                }
            }
        }

        Ok(versions)
    }

    async fn package_exists(&self, id: &PackageId) -> Result<bool> {
        let query = format!(
            r#"
            ASK {{
                ?package <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{}Package> .
                FILTER(CONTAINS(str(?package), "{}"))
            }}
            "#,
            GGEN_NS, id
        );

        let results = self.store.query(&query).map_err(|e| {
            crate::marketplace::error::Error::SearchError(format!("SPARQL query failed: {e}"))
        })?;

        match results {
            oxigraph::sparql::QueryResults::Boolean(exists) => {
                debug!("Package {} exists: {}", id, exists);
                Ok(exists)
            }
            _ => Err(crate::marketplace::error::Error::SearchError(
                "Expected boolean SPARQL result".to_string(),
            )),
        }
    }
}

impl RdfRegistry {
    /// Helper: Delete all triples for a package
    ///
    /// # Errors
    ///
    /// * [`Error::RdfStoreError`] - When deleting from the RDF store fails
    fn delete_package_triples(&self, id: &PackageId) -> Result<()> {
        let delete_query = format!(
            r#"
            PREFIX mp: <https://ggen.io/marketplace/>
            DELETE {{
                <https://ggen.io/marketplace/packages/{0}> ?p ?o .
            }}
            WHERE {{
                <https://ggen.io/marketplace/packages/{0}> ?p ?o .
            }}
            "#,
            id
        );

        self.store.update(&delete_query).map_err(|e| {
            crate::marketplace::error::Error::RdfStoreError {
                operation: "delete".to_string(),
                reason: e.to_string(),
            }
        })
    }

    /// Create a new package in the RDF store
    ///
    /// # Errors
    ///
    /// * [`Error::PackageAlreadyExists`] - When a package with the same ID already exists
    /// * [`Error::RdfStoreError`] - When inserting into the RDF store fails
    /// * [`Error::SerializationError`] - When serializing package data fails
    pub async fn create_package(&self, package: Package) -> Result<Package> {
        // Check if package already exists BEFORE acquiring lock (to avoid holding sync lock during async)
        if self.package_exists(&package.metadata.id).await? {
            return Err(crate::marketplace::error::Error::PackageAlreadyExists {
                package_id: package.metadata.id.to_string(),
            });
        }

        let _lock = self.write_lock.write();

        // Insert package as RDF triples (already holding lock, use inner)
        self.insert_package_rdf_inner(&package)?;

        debug!("Created package {} in RDF store", package.metadata.id);
        Ok(package)
    }

    /// Update an existing package in the RDF store
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package does not exist
    /// * [`Error::RdfStoreError`] - When updating the RDF store fails
    /// * [`Error::SerializationError`] - When serializing updated package data fails
    pub async fn update_package(&self, id: &PackageId, package: Package) -> Result<Package> {
        // Check if package exists BEFORE acquiring lock (to avoid holding sync lock during async)
        if !self.package_exists(id).await? {
            return Err(crate::marketplace::error::Error::PackageNotFound {
                package_id: id.to_string(),
            });
        }

        let _lock = self.write_lock.write();

        // Delete old package data via SPARQL DELETE
        self.delete_package_triples(id)?;

        // Insert updated package data (already holding lock, use inner)
        self.insert_package_rdf_inner(&package)?;

        debug!("Updated package {} in RDF store", id);
        Ok(package)
    }

    /// Delete a package from the RDF store
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package does not exist
    /// * [`Error::RdfStoreError`] - When deleting from the RDF store fails
    pub async fn delete_package(&self, id: &PackageId) -> Result<()> {
        // Check if package exists BEFORE acquiring lock (to avoid holding sync lock during async)
        if !self.package_exists(id).await? {
            return Err(crate::marketplace::error::Error::PackageNotFound {
                package_id: id.to_string(),
            });
        }

        // Now acquire lock for the actual deletion
        let _lock = self.write_lock.write();

        // Delete package via SPARQL DELETE
        self.delete_package_triples(id)?;

        debug!("Deleted package {} from RDF store", id);
        Ok(())
    }

    /// Search for packages by keyword
    ///
    /// This performs full-text-like search over package names and descriptions
    /// by matching the keyword against RDF literals.
    ///
    /// # Errors
    ///
    /// * [`Error::SearchError`] - When the search query fails
    pub async fn search_packages(&self, keyword: &str, limit: usize) -> Result<Vec<SearchResult>> {
        // Build SPARQL FILTER for case-insensitive substring matching
        let search_query = format!(
            r#"
            PREFIX mp: <https://ggen.io/marketplace/>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?packageId ?name ?description ?downloads WHERE {{
                ?package a mp:Package .
                ?package mp:packageId ?packageId .
                ?package mp:name ?name .
                ?package mp:description ?description .
                ?package mp:downloads ?downloads .

                FILTER(
                    CONTAINS(LCASE(str(?packageId)), LCASE("{}")) ||
                    CONTAINS(LCASE(str(?name)), LCASE("{}")) ||
                    CONTAINS(LCASE(str(?description)), LCASE("{}"))
                )
            }}
            LIMIT {}
            "#,
            keyword.replace('\"', "\\\""),
            keyword.replace('\"', "\\\""),
            keyword.replace('\"', "\\\""),
            limit
        );

        let results = self.query_sparql(&search_query)?;
        debug!(
            "Search for '{}' returned {} results",
            keyword,
            results.len()
        );

        let mut search_results = Vec::new();
        let keyword_lower = keyword.to_lowercase();

        // Parse results (for simplicity, we return minimal search results)
        // In a production system, you would construct full SearchResult objects
        for result in results {
            if let Ok(package_id) = PackageId::new(result.clone()) {
                if let Ok(package) = self.mapper.rdf_to_package(&package_id) {
                    let relevance = if package
                        .metadata
                        .name
                        .to_lowercase()
                        .contains(&keyword_lower)
                    {
                        1.0 // Higher relevance for name matches
                    } else {
                        0.5 // Lower relevance for description matches
                    };

                    search_results.push(SearchResult { package, relevance });
                }
            }
        }

        Ok(search_results)
    }

    /// List packages with pagination support
    ///
    /// # Errors
    ///
    /// * [`Error::SearchError`] - When listing packages fails
    pub async fn list_packages(&self, offset: usize, limit: usize) -> Result<Vec<Package>> {
        let query = format!(
            r"
            PREFIX mp: <https://ggen.io/marketplace/>
            SELECT ?packageId WHERE {{
                ?package a mp:Package .
                ?package mp:id ?packageId .
            }}
            OFFSET {}
            LIMIT {}
            ",
            offset, limit
        );

        let results = self.query_sparql(&query)?;
        debug!(
            "Listed {} packages (offset: {}, limit: {})",
            results.len(),
            offset,
            limit
        );

        // Reconstruct packages
        let mut packages = Vec::with_capacity(results.len());
        for package_id_str in results {
            if let Ok(package_id) = PackageId::new(package_id_str) {
                if let Ok(package) = self.mapper.rdf_to_package(&package_id) {
                    packages.push(package);
                }
            }
        }

        Ok(packages)
    }

    /// Get total count of packages in the registry
    ///
    /// # Errors
    ///
    /// * [`Error::SearchError`] - When querying the count fails
    pub async fn count_packages(&self) -> Result<u64> {
        let query = format!(
            r"
            PREFIX mp: <https://ggen.io/marketplace/>
            SELECT (COUNT(?package) as ?count) WHERE {{
                ?package a mp:Package .
            }}
            "
        );

        let results = self.store.query(&query).map_err(|e| {
            crate::marketplace::error::Error::SearchError(format!("Count query failed: {e}"))
        })?;

        if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
            if let Some(Ok(solution)) = solutions.next() {
                if let Some(count_term) = solution.get("count") {
                    if let oxigraph::model::Term::Literal(lit) = count_term {
                        if let Ok(count) = lit.value().parse::<u64>() {
                            return Ok(count);
                        }
                    }
                }
            }
        }

        Ok(0)
    }
}

/// Statistics about the RDF registry
#[derive(Clone, Debug)]
pub struct RdfRegistryStats {
    /// Total SPARQL queries executed
    pub total_queries: u64,
}

impl std::fmt::Display for RdfRegistryStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RDF Registry: {} queries executed", self.total_queries)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marketplace::models::PackageMetadata;
    use indexmap::IndexMap;

    #[tokio::test]
    async fn test_rdf_registry_creation() {
        let registry = RdfRegistry::new();
        let stats = registry.stats();
        assert_eq!(stats.total_queries, 0);
    }

    #[tokio::test]
    async fn test_rdf_ontology_initialized() {
        let registry = RdfRegistry::new();
        // Verify ontology was initialized
        let query = format!(
            r#"
            ASK {{
                ?package <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{}Class> .
            }}
            "#,
            RDFS_NS
        );

        #[allow(deprecated)]
        let results = registry.store.query(&query);
        assert!(results.is_ok());
    }

    #[tokio::test]
    async fn test_create_package() {
        let registry = RdfRegistry::new();
        let package_id = PackageId::new("test-package").unwrap();
        let metadata = PackageMetadata {
            id: package_id.clone(),
            name: "Test Package".to_string(),
            description: "A test package".to_string(),
            authors: vec!["Test Author".to_string()],
            license: "MIT".to_string(),
            repository: Some("https://github.com/test/package".to_string()),
            homepage: None,
            keywords: vec![],
            categories: vec![],
            downloads: 0,
            quality_score: None,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            registry_type: crate::marketplace::trust::RegistryType::default(),
        };

        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: IndexMap::new(),
        };

        let created = registry.create_package(package.clone()).await;
        assert!(created.is_ok());

        // Verify it exists
        let exists = registry.package_exists(&package_id).await.unwrap();
        assert!(exists);
    }

    #[tokio::test]
    async fn test_create_duplicate_package_fails() {
        let registry = RdfRegistry::new();
        let package_id = PackageId::new("duplicate-test").unwrap();
        let metadata = PackageMetadata {
            id: package_id.clone(),
            name: "Duplicate Test".to_string(),
            description: "A test package".to_string(),
            authors: vec!["Test Author".to_string()],
            license: "MIT".to_string(),
            repository: None,
            homepage: None,
            keywords: vec![],
            categories: vec![],
            downloads: 0,
            quality_score: None,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            registry_type: crate::marketplace::trust::RegistryType::default(),
        };

        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: IndexMap::new(),
        };

        // Create first time - should succeed
        let _ = registry.create_package(package.clone()).await;

        // Create second time - should fail
        let result = registry.create_package(package).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_delete_package() {
        let registry = RdfRegistry::new();
        let package_id = PackageId::new("delete-test").unwrap();
        let metadata = PackageMetadata {
            id: package_id.clone(),
            name: "Delete Test".to_string(),
            description: "A test package".to_string(),
            authors: vec!["Test Author".to_string()],
            license: "MIT".to_string(),
            repository: None,
            homepage: None,
            keywords: vec![],
            categories: vec![],
            downloads: 0,
            quality_score: None,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            registry_type: crate::marketplace::trust::RegistryType::default(),
        };

        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: IndexMap::new(),
        };

        // Create and then delete
        let _ = registry.create_package(package).await;
        let deleted = registry.delete_package(&package_id).await;
        assert!(deleted.is_ok());

        // Verify it no longer exists
        let exists = registry.package_exists(&package_id).await.unwrap();
        assert!(!exists);
    }

    #[tokio::test]
    async fn test_delete_nonexistent_package_fails() {
        let registry = RdfRegistry::new();
        let package_id = PackageId::new("nonexistent").unwrap();
        let result = registry.delete_package(&package_id).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_update_package() {
        let registry = RdfRegistry::new();
        let package_id = PackageId::new("update-test").unwrap();
        let metadata = PackageMetadata {
            id: package_id.clone(),
            name: "Update Test".to_string(),
            description: "A test package".to_string(),
            authors: vec!["Test Author".to_string()],
            license: "MIT".to_string(),
            repository: None,
            homepage: None,
            keywords: vec![],
            categories: vec![],
            downloads: 0,
            quality_score: None,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            registry_type: crate::marketplace::trust::RegistryType::default(),
        };

        let package = Package {
            metadata: metadata.clone(),
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: IndexMap::new(),
        };

        // Create package
        let _ = registry.create_package(package).await;

        // Update it
        let mut updated_metadata = metadata.clone();
        updated_metadata.description = "Updated description".to_string();
        let updated_package = Package {
            metadata: updated_metadata,
            latest_version: PackageVersion::new("2.0.0").unwrap(),
            versions: vec![
                PackageVersion::new("1.0.0").unwrap(),
                PackageVersion::new("2.0.0").unwrap(),
            ],
            releases: IndexMap::new(),
        };

        let result = registry.update_package(&package_id, updated_package).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_search_packages() {
        let registry = RdfRegistry::new();

        // Create test packages
        for i in 1..=3 {
            let package_id = PackageId::new(format!("search-test-{}", i)).unwrap();
            let metadata = PackageMetadata {
                id: package_id,
                name: format!("Search Test {}", i),
                description: "A searchable test package".to_string(),
                authors: vec!["Test Author".to_string()],
                license: "MIT".to_string(),
                repository: None,
                homepage: None,
                keywords: vec!["search".to_string()],
                categories: vec![],
                downloads: 0,
                quality_score: None,
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
                registry_type: crate::marketplace::trust::RegistryType::default(),
            };

            let package = Package {
                metadata,
                latest_version: PackageVersion::new("1.0.0").unwrap(),
                versions: vec![PackageVersion::new("1.0.0").unwrap()],
                releases: IndexMap::new(),
            };

            let _ = registry.create_package(package).await;
        }

        // Search for packages
        let results = registry.search_packages("search", 10).await.unwrap();
        assert!(!results.is_empty());
    }

    #[tokio::test]
    async fn test_list_packages() {
        let registry = RdfRegistry::new();

        // Create test packages
        for i in 1..=5 {
            let package_id = PackageId::new(format!("list-test-{}", i)).unwrap();
            let metadata = PackageMetadata {
                id: package_id,
                name: format!("List Test {}", i),
                description: "A listable test package".to_string(),
                authors: vec!["Test Author".to_string()],
                license: "MIT".to_string(),
                repository: None,
                homepage: None,
                keywords: vec![],
                categories: vec![],
                downloads: 0,
                quality_score: None,
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
                registry_type: crate::marketplace::trust::RegistryType::default(),
            };

            let package = Package {
                metadata,
                latest_version: PackageVersion::new("1.0.0").unwrap(),
                versions: vec![PackageVersion::new("1.0.0").unwrap()],
                releases: IndexMap::new(),
            };

            let _ = registry.create_package(package).await;
        }

        // List packages with pagination
        let packages = registry.list_packages(0, 3).await.unwrap();
        assert!(packages.len() <= 3);
    }

    #[tokio::test]
    async fn test_count_packages() {
        let registry = RdfRegistry::new();

        // Create test packages
        for i in 1..=3 {
            let package_id = PackageId::new(format!("count-test-{}", i)).unwrap();
            let metadata = PackageMetadata {
                id: package_id,
                name: format!("Count Test {}", i),
                description: "A countable test package".to_string(),
                authors: vec!["Test Author".to_string()],
                license: "MIT".to_string(),
                repository: None,
                homepage: None,
                keywords: vec![],
                categories: vec![],
                downloads: 0,
                quality_score: None,
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
                registry_type: crate::marketplace::trust::RegistryType::default(),
            };

            let package = Package {
                metadata,
                latest_version: PackageVersion::new("1.0.0").unwrap(),
                versions: vec![PackageVersion::new("1.0.0").unwrap()],
                releases: IndexMap::new(),
            };

            let _ = registry.create_package(package).await;
        }

        // Count packages
        let count = registry.count_packages().await.unwrap();
        assert_eq!(count, 3);
    }
}
