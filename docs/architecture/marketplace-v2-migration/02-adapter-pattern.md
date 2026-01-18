# Adapter Pattern Architecture - Marketplace V2 Migration

## Overview

The adapter pattern enables transparent switching between marketplace-v1 (tantivy) and marketplace-v2 (RDF/SPARQL) backends with zero breaking changes.

## Unified Interface Design

### Core Trait: MarketplaceBackend

```rust
// ggen-domain/src/marketplace/backend.rs

use async_trait::async_trait;
use std::path::Path;

/// Unified marketplace backend interface
///
/// Both V1 (tantivy) and V2 (RDF) implement this trait for transparent backend switching.
#[async_trait]
pub trait MarketplaceBackend: Send + Sync + 'static {
    /// Search for packages matching query
    async fn search(&self, query: &SearchQuery) -> Result<SearchResults>;

    /// Get detailed package information
    async fn get_package(&self, id: &PackageId) -> Result<PackageDetails>;

    /// List all installed packages
    async fn list_installed(&self, filters: &ListFilters) -> Result<Vec<InstalledPackage>>;

    /// Install a package to target directory
    async fn install_package(&self, request: &InstallRequest) -> Result<InstallResult>;

    /// Publish a package to the marketplace
    async fn publish_package(&self, manifest: &PublishManifest) -> Result<PublishResult>;

    /// Validate package manifest and dependencies
    async fn validate_package(&self, manifest: &PackageManifest) -> Result<ValidationReport>;

    /// Get backend metadata (version, capabilities, etc.)
    fn metadata(&self) -> BackendMetadata;
}

/// Search query parameters (backend-agnostic)
#[derive(Debug, Clone)]
pub struct SearchQuery {
    pub query_text: String,
    pub limit: usize,
    pub offset: usize,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub sort_by: SortCriteria,
}

/// Search results (unified format)
#[derive(Debug, Clone)]
pub struct SearchResults {
    pub packages: Vec<PackageSummary>,
    pub total_count: usize,
    pub query_time_ms: u64,
    pub backend_used: BackendType,
}

/// Package summary (common fields for v1 and v2)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageSummary {
    pub id: PackageId,
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub downloads: u32,
    pub stars: u32,
    pub tags: Vec<String>,
    pub category: Option<String>,
}

/// Package details (extended information)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageDetails {
    #[serde(flatten)]
    pub summary: PackageSummary,
    pub readme: Option<String>,
    pub dependencies: Vec<Dependency>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub published_at: chrono::DateTime<chrono::Utc>,

    // V2-specific fields (optional for v1 compatibility)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cryptographic_signature: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdf_metadata: Option<serde_json::Value>,
}
```

## V1 Adapter Implementation

```rust
// ggen-domain/src/marketplace/v1_adapter.rs

use ggen_marketplace::{TantivySearchEngine, PackageRegistry};

/// V1 marketplace backend (tantivy-based)
pub struct V1Adapter {
    search_engine: TantivySearchEngine,
    registry: PackageRegistry,
    config: V1Config,
}

impl V1Adapter {
    pub fn new(config: V1Config) -> Result<Self> {
        let search_engine = TantivySearchEngine::new(&config.index_path)?;
        let registry = PackageRegistry::new(&config.registry_path)?;

        Ok(Self {
            search_engine,
            registry,
            config,
        })
    }
}

#[async_trait]
impl MarketplaceBackend for V1Adapter {
    async fn search(&self, query: &SearchQuery) -> Result<SearchResults> {
        let start = std::time::Instant::now();

        // Convert unified SearchQuery to V1 format
        let v1_query = self.convert_query(query);

        // Execute V1 search
        let v1_results = self.search_engine.search(&v1_query)?;

        // Convert V1 results to unified format
        let packages = v1_results
            .into_iter()
            .map(|pkg| self.convert_package_summary(pkg))
            .collect();

        Ok(SearchResults {
            packages,
            total_count: packages.len(),
            query_time_ms: start.elapsed().as_millis() as u64,
            backend_used: BackendType::V1,
        })
    }

    async fn get_package(&self, id: &PackageId) -> Result<PackageDetails> {
        let v1_package = self.registry.get_package(&id.to_string())?;
        Ok(self.convert_package_details(v1_package))
    }

    async fn install_package(&self, request: &InstallRequest) -> Result<InstallResult> {
        // Use existing V1 installation logic
        let result = self.registry.install(
            &request.package_id.to_string(),
            &request.target_path,
            request.options.force,
            request.options.skip_dependencies,
        )?;

        Ok(InstallResult {
            package_id: request.package_id.clone(),
            installed_path: result.path,
            dependencies_installed: result.dependencies,
            backend_used: BackendType::V1,
            cryptographic_verification: None,  // V1 doesn't support signing
        })
    }

    // ... other trait methods

    fn metadata(&self) -> BackendMetadata {
        BackendMetadata {
            backend_type: BackendType::V1,
            version: "3.2.0".to_string(),
            capabilities: vec![
                "search".to_string(),
                "install".to_string(),
                "publish".to_string(),
                "list".to_string(),
            ],
            supports_cryptographic_signing: false,
            supports_sparql_queries: false,
            supports_rdf_metadata: false,
        }
    }
}

// Helper methods for V1 adapter
impl V1Adapter {
    fn convert_query(&self, query: &SearchQuery) -> ggen_marketplace::SearchQuery {
        // Convert unified query to V1 format
        ggen_marketplace::SearchQuery {
            text: query.query_text.clone(),
            limit: query.limit,
            category: query.category.clone(),
            // ... other fields
        }
    }

    fn convert_package_summary(&self, v1_pkg: ggen_marketplace::Package) -> PackageSummary {
        PackageSummary {
            id: PackageId::from(v1_pkg.id),
            name: v1_pkg.name,
            version: v1_pkg.version,
            description: v1_pkg.description,
            author: v1_pkg.author,
            downloads: v1_pkg.downloads,
            stars: v1_pkg.stars,
            tags: v1_pkg.tags,
            category: v1_pkg.category,
        }
    }
}
```

## V2 Adapter Implementation

```rust
// ggen-domain/src/marketplace/v2_adapter.rs

use ggen_marketplace_v2::{RdfRegistry, SparqlSearchEngine, Ed25519Signer};

/// V2 marketplace backend (RDF/SPARQL-based)
pub struct V2Adapter {
    rdf_registry: RdfRegistry,
    sparql_engine: SparqlSearchEngine,
    crypto_signer: Ed25519Signer,
    config: V2Config,
}

impl V2Adapter {
    pub fn new(config: V2Config) -> Result<Self> {
        let rdf_registry = RdfRegistry::new(&config.rdf_store_path)?;
        let sparql_engine = SparqlSearchEngine::new(rdf_registry.store())?;
        let crypto_signer = Ed25519Signer::new(&config.keypair_path)?;

        Ok(Self {
            rdf_registry,
            sparql_engine,
            crypto_signer,
            config,
        })
    }
}

#[async_trait]
impl MarketplaceBackend for V2Adapter {
    async fn search(&self, query: &SearchQuery) -> Result<SearchResults> {
        let start = std::time::Instant::now();

        // Convert unified SearchQuery to SPARQL
        let sparql_query = self.build_sparql_query(query);

        // Execute SPARQL search
        let rdf_results = self.sparql_engine.execute(&sparql_query).await?;

        // Convert RDF results to unified format
        let packages = rdf_results
            .into_iter()
            .map(|triple_set| self.convert_rdf_to_package(triple_set))
            .collect();

        Ok(SearchResults {
            packages,
            total_count: packages.len(),
            query_time_ms: start.elapsed().as_millis() as u64,
            backend_used: BackendType::V2,
        })
    }

    async fn get_package(&self, id: &PackageId) -> Result<PackageDetails> {
        let rdf_package = self.rdf_registry.get_package(id).await?;
        Ok(self.convert_rdf_package_details(rdf_package))
    }

    async fn install_package(&self, request: &InstallRequest) -> Result<InstallResult> {
        // V2 installation with cryptographic verification
        let package = self.get_package(&request.package_id).await?;

        // Verify signature if present
        let signature_valid = if let Some(sig) = &package.cryptographic_signature {
            self.crypto_signer.verify(&package, sig)?
        } else {
            false
        };

        // Install package
        let result = self.rdf_registry.install(
            &request.package_id,
            &request.target_path,
            request.options.clone(),
        ).await?;

        Ok(InstallResult {
            package_id: request.package_id.clone(),
            installed_path: result.path,
            dependencies_installed: result.dependencies,
            backend_used: BackendType::V2,
            cryptographic_verification: Some(signature_valid),
        })
    }

    async fn publish_package(&self, manifest: &PublishManifest) -> Result<PublishResult> {
        // V2 publishing with Ed25519 signing
        let signature = self.crypto_signer.sign(&manifest)?;

        let result = self.rdf_registry.publish(manifest, signature).await?;

        Ok(PublishResult {
            package_id: result.package_id,
            published_at: chrono::Utc::now(),
            signature: Some(signature.to_string()),
            backend_used: BackendType::V2,
        })
    }

    fn metadata(&self) -> BackendMetadata {
        BackendMetadata {
            backend_type: BackendType::V2,
            version: "3.0.0".to_string(),
            capabilities: vec![
                "search".to_string(),
                "install".to_string(),
                "publish".to_string(),
                "list".to_string(),
                "sparql-query".to_string(),
                "cryptographic-signing".to_string(),
                "rdf-metadata".to_string(),
            ],
            supports_cryptographic_signing: true,
            supports_sparql_queries: true,
            supports_rdf_metadata: true,
        }
    }
}

// Helper methods for V2 adapter
impl V2Adapter {
    fn build_sparql_query(&self, query: &SearchQuery) -> String {
        // Convert unified query to SPARQL
        format!(
            r#"
            PREFIX pkg: <http://ggen.io/pkg#>
            SELECT ?id ?name ?version ?description ?author ?downloads ?stars
            WHERE {{
                ?id pkg:name ?name .
                ?id pkg:version ?version .
                ?id pkg:description ?description .
                OPTIONAL {{ ?id pkg:author ?author }} .
                OPTIONAL {{ ?id pkg:downloads ?downloads }} .
                OPTIONAL {{ ?id pkg:stars ?stars }} .
                FILTER(CONTAINS(LCASE(?name), LCASE("{}")))
            }}
            LIMIT {}
            "#,
            query.query_text,
            query.limit
        )
    }
}
```

## Dual Backend Adapter (A/B Testing)

```rust
// ggen-domain/src/marketplace/dual_adapter.rs

/// Dual-backend adapter for A/B testing and gradual migration
pub struct DualBackendAdapter {
    v1: V1Adapter,
    v2: V2Adapter,
    strategy: BackendStrategy,
    metrics: Arc<Mutex<BackendMetrics>>,
}

#[derive(Debug, Clone)]
pub enum BackendStrategy {
    /// Always use V1
    V1Only,

    /// Always use V2
    V2Only,

    /// Use V2, fall back to V1 on error
    V2WithFallback,

    /// A/B test: route percentage% to V2, rest to V1
    ABTest { v2_percentage: u8 },

    /// Compare results from both backends (testing mode)
    Compare,
}

impl DualBackendAdapter {
    pub fn new(v1_config: V1Config, v2_config: V2Config, strategy: BackendStrategy) -> Result<Self> {
        Ok(Self {
            v1: V1Adapter::new(v1_config)?,
            v2: V2Adapter::new(v2_config)?,
            strategy,
            metrics: Arc::new(Mutex::new(BackendMetrics::default())),
        })
    }

    /// Select which backend to use based on strategy
    fn select_backend(&self) -> BackendType {
        match self.strategy {
            BackendStrategy::V1Only => BackendType::V1,
            BackendStrategy::V2Only => BackendType::V2,
            BackendStrategy::V2WithFallback => BackendType::V2,
            BackendStrategy::ABTest { v2_percentage } => {
                // Random selection based on percentage
                if rand::random::<u8>() < v2_percentage {
                    BackendType::V2
                } else {
                    BackendType::V1
                }
            }
            BackendStrategy::Compare => BackendType::V2,  // Default to V2 for compare mode
        }
    }
}

#[async_trait]
impl MarketplaceBackend for DualBackendAdapter {
    async fn search(&self, query: &SearchQuery) -> Result<SearchResults> {
        let selected = self.select_backend();

        match self.strategy {
            BackendStrategy::Compare => {
                // Execute on both backends and compare results
                let (v1_result, v2_result) = tokio::join!(
                    self.v1.search(query),
                    self.v2.search(query)
                );

                // Log differences
                self.compare_and_log_results(&v1_result, &v2_result);

                // Return V2 result (or V1 if V2 failed)
                v2_result.or(v1_result)
            }

            BackendStrategy::V2WithFallback => {
                // Try V2 first, fall back to V1 on error
                match self.v2.search(query).await {
                    Ok(result) => {
                        self.record_success(BackendType::V2);
                        Ok(result)
                    }
                    Err(e) => {
                        tracing::warn!("V2 search failed, falling back to V1: {}", e);
                        self.record_fallback();
                        self.v1.search(query).await
                    }
                }
            }

            _ => {
                // Use selected backend
                match selected {
                    BackendType::V1 => self.v1.search(query).await,
                    BackendType::V2 => self.v2.search(query).await,
                }
            }
        }
    }

    // Similar implementations for other methods...

    fn metadata(&self) -> BackendMetadata {
        match self.strategy {
            BackendStrategy::V1Only => self.v1.metadata(),
            BackendStrategy::V2Only => self.v2.metadata(),
            _ => {
                // Combined metadata
                BackendMetadata {
                    backend_type: BackendType::Dual,
                    version: format!("v1:{} v2:{}", self.v1.metadata().version, self.v2.metadata().version),
                    capabilities: {
                        let mut caps = self.v1.metadata().capabilities;
                        caps.extend(self.v2.metadata().capabilities);
                        caps.sort();
                        caps.dedup();
                        caps
                    },
                    supports_cryptographic_signing: self.v2.metadata().supports_cryptographic_signing,
                    supports_sparql_queries: self.v2.metadata().supports_sparql_queries,
                    supports_rdf_metadata: self.v2.metadata().supports_rdf_metadata,
                }
            }
        }
    }
}

impl DualBackendAdapter {
    fn compare_and_log_results(&self, v1: &Result<SearchResults>, v2: &Result<SearchResults>) {
        match (v1, v2) {
            (Ok(v1_res), Ok(v2_res)) => {
                let v1_ids: HashSet<_> = v1_res.packages.iter().map(|p| &p.id).collect();
                let v2_ids: HashSet<_> = v2_res.packages.iter().map(|p| &p.id).collect();

                let only_v1 = v1_ids.difference(&v2_ids).count();
                let only_v2 = v2_ids.difference(&v1_ids).count();
                let both = v1_ids.intersection(&v2_ids).count();

                tracing::info!(
                    "Backend comparison: v1_only={}, v2_only={}, both={}, v1_time={}ms, v2_time={}ms",
                    only_v1, only_v2, both, v1_res.query_time_ms, v2_res.query_time_ms
                );
            }
            (Err(e1), Ok(_)) => {
                tracing::warn!("V1 failed but V2 succeeded: {}", e1);
            }
            (Ok(_), Err(e2)) => {
                tracing::warn!("V2 failed but V1 succeeded: {}", e2);
            }
            (Err(e1), Err(e2)) => {
                tracing::error!("Both backends failed: v1={}, v2={}", e1, e2);
            }
        }
    }

    fn record_success(&self, backend: BackendType) {
        let mut metrics = self.metrics.lock().unwrap();
        match backend {
            BackendType::V1 => metrics.v1_success += 1,
            BackendType::V2 => metrics.v2_success += 1,
            _ => {}
        }
    }

    fn record_fallback(&self) {
        let mut metrics = self.metrics.lock().unwrap();
        metrics.v2_fallbacks += 1;
    }
}
```

## CLI Integration

```rust
// ggen-cli/src/cmds/marketplace.rs

use ggen_domain::marketplace::{MarketplaceBackend, create_backend};

/// Create marketplace backend based on feature flags and configuration
fn create_marketplace_backend() -> Result<Box<dyn MarketplaceBackend>> {
    let config = load_marketplace_config()?;

    #[cfg(all(feature = "marketplace-parallel"))]
    {
        // Dual backend mode
        let v1_config = load_v1_config()?;
        let v2_config = load_v2_config()?;
        let strategy = determine_strategy(&config);

        Ok(Box::new(DualBackendAdapter::new(v1_config, v2_config, strategy)?))
    }

    #[cfg(all(feature = "marketplace-v2", not(feature = "marketplace-parallel")))]
    {
        // V2 only
        let v2_config = load_v2_config()?;
        Ok(Box::new(V2Adapter::new(v2_config)?))
    }

    #[cfg(all(feature = "marketplace-v1", not(feature = "marketplace-parallel")))]
    {
        // V1 only (default)
        let v1_config = load_v1_config()?;
        Ok(Box::new(V1Adapter::new(v1_config)?))
    }
}

// All marketplace commands use the unified backend
#[verb]
fn search(query: String, limit: Option<usize>, category: Option<String>) -> Result<SearchOutput> {
    let backend = create_marketplace_backend()?;

    let query = SearchQuery {
        query_text: query,
        limit: limit.unwrap_or(10),
        category,
        // ... other fields
    };

    execute_async_verb(async move {
        let results = backend.search(&query).await?;

        // Convert to CLI output format
        let output = SearchOutput {
            packages: results.packages.into_iter().map(|p| PackageInfo {
                name: p.name,
                version: p.version,
                description: p.description,
                // ... other fields
            }).collect(),
            total: results.total_count,
        };

        Ok(output)
    })
}
```

## Testing Strategy

### Adapter Tests

```rust
// ggen-domain/tests/marketplace_adapter_tests.rs

#[tokio::test]
async fn test_v1_adapter_search() {
    let adapter = V1Adapter::new(test_config()).unwrap();

    let query = SearchQuery {
        query_text: "web framework".to_string(),
        limit: 10,
        ..Default::default()
    };

    let results = adapter.search(&query).await.unwrap();

    assert_eq!(results.backend_used, BackendType::V1);
    assert!(!results.packages.is_empty());
}

#[tokio::test]
async fn test_v2_adapter_search() {
    let adapter = V2Adapter::new(test_config()).unwrap();

    let query = SearchQuery {
        query_text: "microservice".to_string(),
        limit: 10,
        ..Default::default()
    };

    let results = adapter.search(&query).await.unwrap();

    assert_eq!(results.backend_used, BackendType::V2);
    assert!(!results.packages.is_empty());
}

#[tokio::test]
async fn test_dual_adapter_fallback() {
    let adapter = DualBackendAdapter::new(
        v1_config(),
        v2_config(),
        BackendStrategy::V2WithFallback,
    ).unwrap();

    // Search should succeed even if V2 fails (fallback to V1)
    let query = SearchQuery {
        query_text: "test".to_string(),
        limit: 5,
        ..Default::default()
    };

    let results = adapter.search(&query).await.unwrap();
    assert!(!results.packages.is_empty());
}
```

## Performance Monitoring

```rust
#[derive(Debug, Default)]
struct BackendMetrics {
    v1_success: u64,
    v2_success: u64,
    v1_errors: u64,
    v2_errors: u64,
    v2_fallbacks: u64,
    v1_avg_latency_ms: f64,
    v2_avg_latency_ms: f64,
}

impl BackendMetrics {
    fn print_report(&self) {
        println!("Backend Performance Report:");
        println!("V1: {} successes, {} errors, avg latency: {:.2}ms",
                 self.v1_success, self.v1_errors, self.v1_avg_latency_ms);
        println!("V2: {} successes, {} errors, avg latency: {:.2}ms",
                 self.v2_success, self.v2_errors, self.v2_avg_latency_ms);
        println!("V2→V1 fallbacks: {}", self.v2_fallbacks);
    }
}
```

## Success Criteria

| Criterion | Target | Validation |
|-----------|--------|------------|
| Transparent switching | 100% | CLI code unchanged |
| Backward compatibility | 100% | All v1 tests pass with adapter |
| Feature parity | 100% | All operations available in both backends |
| Fallback success rate | >99% | V2 failures don't break user workflows |
| Performance overhead | <5% | Adapter adds minimal latency |
| A/B test readiness | Week 2 | Dual backend functional |
