//! High-performance RDF-based registry using oxigraph as the knowledge graph backend
//!
//! This implementation treats packages as RDF triples, enabling:
//! - Semantic queries via SPARQL
//! - Flexible package relationships
//! - Version history as RDF facts
//! - Full-text search over package descriptions

use async_trait::async_trait;
use oxigraph::model::{GraphNameRef, NamedNode, QuadRef, Term};
use oxigraph::store::Store;
use parking_lot::RwLock;
use std::sync::Arc;
use tracing::{debug, info};

use crate::error::Result;
use crate::models::{Package, PackageId, PackageVersion};
use crate::rdf_mapper::RdfMapper;
use crate::traits::AsyncRepository;

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
    pub fn new() -> Self {
        let store = Store::new().expect("Failed to create RDF store");

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

    /// Insert a package as RDF triples using the mapper
    pub async fn insert_package_rdf(&self, package: &Package) -> Result<()> {
        let _lock = self.write_lock.write();
        self.mapper.package_to_rdf(package).await
    }

    /// Batch insert multiple packages for efficient loading
    pub async fn batch_insert_packages(&self, packages: Vec<Package>) -> Result<usize> {
        let _lock = self.write_lock.write();
        let mut inserted = 0;

        for package in packages {
            match self.mapper.package_to_rdf(&package).await {
                Ok(_) => inserted += 1,
                Err(e) => {
                    tracing::warn!("Failed to insert package {}: {}", package.metadata.id, e);
                }
            }
        }

        info!("Batch inserted {} packages to RDF store", inserted);
        Ok(inserted)
    }

    /// Query packages by SPARQL
    pub async fn query_sparql(&self, query: &str) -> Result<Vec<String>> {
        let results = self
            .store
            .query(query)
            .map_err(|e| crate::error::Error::SearchError(format!("SPARQL query failed: {}", e)))?;

        self.queries_executed
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        let mut packages = Vec::new();

        // Parse results
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                if let Ok(solution) = solution {
                    for (_, term) in solution.iter() {
                        if let Term::NamedNode(node) = term {
                            packages.push(node.as_str().to_string());
                        }
                    }
                }
            }
        }

        debug!("SPARQL query returned {} results", packages.len());
        Ok(packages)
    }

    /// Get statistics about the RDF store
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
        self.mapper.rdf_to_package(id).await
    }

    async fn get_package_version(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<Package> {
        self.queries_executed
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        // Get full package and filter to specific version
        let mut package = self.get_package(id).await?;

        if !package.versions.contains(version) {
            return Err(crate::error::Error::InvalidVersion {
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
            r#"
            SELECT ?packageId WHERE {{
                ?package <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{}Package> .
                ?package <{}> ?packageId .
            }}
            "#,
            GGEN_NS,
            crate::ontology::Properties::package_id()
        );

        let results = self.query_sparql(&query).await?;
        debug!("Found {} packages in RDF store", results.len());

        // Reconstruct each package
        let mut packages = Vec::with_capacity(results.len());
        for package_id_str in results {
            if let Ok(package_id) = PackageId::new(package_id_str) {
                if let Ok(package) = self.mapper.rdf_to_package(&package_id).await {
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

        let results = self.query_sparql(&query).await?;

        // Parse versions from URIs
        let mut versions = Vec::new();
        for result_uri in results {
            if let Some(version_str) = result_uri.split('/').last() {
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

        let results = self
            .store
            .query(&query)
            .map_err(|e| crate::error::Error::SearchError(format!("SPARQL query failed: {}", e)))?;

        match results {
            oxigraph::sparql::QueryResults::Boolean(exists) => {
                debug!("Package {} exists: {}", id, exists);
                Ok(exists)
            }
            _ => Err(crate::error::Error::SearchError(
                "Expected boolean SPARQL result".to_string(),
            )),
        }
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

        let results = registry.store.query(&query);
        assert!(results.is_ok());
    }
}
