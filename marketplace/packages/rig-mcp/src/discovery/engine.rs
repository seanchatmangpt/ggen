//! MCP Tool Discovery Engine
//!
//! High-level engine for concurrent tool discovery across multiple MCP servers
//! with caching, metrics collection, and error handling.

use crate::discovery::{
    cache::{CacheConfig, SchemaCache},
    schema::{SchemaExtractor, ToolSchema},
    transport::{TransportClient, TransportRequest},
    DiscoveryError, DiscoveryFilter, Result,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Semaphore;
use tokio::time::timeout;

/// Configuration for the discovery engine
#[derive(Debug, Clone)]
pub struct DiscoveryConfig {
    /// Request timeout in seconds
    pub timeout_secs: u64,
    /// Maximum concurrent discoveries
    pub max_concurrent: usize,
    /// Cache configuration
    pub cache_config: CacheConfig,
    /// Whether to validate schemas
    pub validate_schemas: bool,
    /// Discovery retries
    pub max_retries: usize,
}

impl Default for DiscoveryConfig {
    fn default() -> Self {
        Self {
            timeout_secs: 30,
            max_concurrent: 10,
            cache_config: CacheConfig::default(),
            validate_schemas: true,
            max_retries: 3,
        }
    }
}

impl DiscoveryConfig {
    /// Create a builder for discovery config
    pub fn builder() -> DiscoveryConfigBuilder {
        DiscoveryConfigBuilder::default()
    }

    /// Set the timeout
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Set the maximum concurrent discoveries
    pub fn with_max_concurrent(mut self, max: usize) -> Self {
        self.max_concurrent = max;
        self
    }

    /// Set the cache configuration
    pub fn with_cache_config(mut self, config: CacheConfig) -> Self {
        self.cache_config = config;
        self
    }

    /// Enable or disable schema validation
    pub fn with_validation(mut self, validate: bool) -> Self {
        self.validate_schemas = validate;
        self
    }
}

/// Builder for DiscoveryConfig
#[derive(Debug, Default)]
pub struct DiscoveryConfigBuilder {
    timeout_secs: u64,
    max_concurrent: usize,
    cache_ttl_secs: u64,
    cache_max_entries: usize,
    validate_schemas: bool,
    max_retries: usize,
}

impl DiscoveryConfigBuilder {
    /// Set the timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Set the maximum concurrent discoveries
    pub fn max_concurrent(mut self, max: usize) -> Self {
        self.max_concurrent = max;
        self
    }

    /// Set the cache TTL
    pub fn cache_ttl(mut self, secs: u64) -> Self {
        self.cache_ttl_secs = secs;
        self
    }

    /// Set the maximum cache entries
    pub fn cache_max_entries(mut self, max: usize) -> Self {
        self.cache_max_entries = max;
        self
    }

    /// Enable or disable schema validation
    pub fn validate(mut self, validate: bool) -> Self {
        self.validate_schemas = validate;
        self
    }

    /// Set the maximum retries
    pub fn retries(mut self, max: usize) -> Self {
        self.max_retries = max;
        self
    }

    /// Build the configuration
    pub fn build(self) -> DiscoveryConfig {
        DiscoveryConfig {
            timeout_secs: self.timeout_secs,
            max_concurrent: self.max_concurrent,
            cache_config: CacheConfig {
                ttl: Duration::from_secs(self.cache_ttl_secs),
                max_entries: self.cache_max_entries,
                ..Default::default()
            },
            validate_schemas: self.validate_schemas,
            max_retries: self.max_retries,
        }
    }
}

/// Result of a single discovery operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoveryResult {
    /// Server URL
    pub server_url: String,
    /// Discovered tools
    pub tools: Vec<ToolSchema>,
    /// Whether the result was from cache
    pub cached: bool,
    /// Discovery duration in milliseconds
    pub duration_ms: u64,
    /// Whether there were any errors
    pub errors: Vec<String>,
}

impl DiscoveryResult {
    /// Create a new discovery result
    pub fn new(server_url: String) -> Self {
        Self {
            server_url,
            tools: Vec::new(),
            cached: false,
            duration_ms: 0,
            errors: Vec::new(),
        }
    }

    /// Add a tool to the result
    pub fn with_tool(mut self, tool: ToolSchema) -> Self {
        self.tools.push(tool);
        self
    }

    /// Add tools to the result
    pub fn with_tools(mut self, tools: Vec<ToolSchema>) -> Self {
        self.tools.extend(tools);
        self
    }

    /// Set cached flag
    pub fn with_cached(mut self, cached: bool) -> Self {
        self.cached = cached;
        self
    }

    /// Set duration
    pub fn with_duration(mut self, duration_ms: u64) -> Self {
        self.duration_ms = duration_ms;
        self
    }

    /// Add an error
    pub fn with_error(mut self, error: String) -> Self {
        self.errors.push(error);
        self
    }

    /// Get the number of tools discovered
    pub fn tool_count(&self) -> usize {
        self.tools.len()
    }
}

/// Discovery metrics for observability
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DiscoveryMetrics {
    /// Total discoveries performed
    pub total_discoveries: u64,
    /// Successful discoveries
    pub successful_discoveries: u64,
    /// Failed discoveries
    pub failed_discoveries: u64,
    /// Total tools discovered
    pub total_tools_discovered: u64,
    /// Cache hits
    pub cache_hits: u64,
    /// Cache misses
    pub cache_misses: u64,
    /// Average discovery time in milliseconds
    pub avg_discovery_time_ms: f64,
    /// Server-specific metrics
    pub server_metrics: Vec<ServerMetrics>,
}

/// Metrics for a specific server
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerMetrics {
    /// Server URL
    pub server_url: String,
    /// Number of times this server was queried
    pub query_count: u64,
    /// Number of successes
    pub success_count: u64,
    /// Number of failures
    pub failure_count: u64,
    /// Average response time in milliseconds
    pub avg_response_time_ms: f64,
    /// Total tools discovered from this server
    pub total_tools: u64,
}

/// MCP Tool Discovery Engine
///
/// The main entry point for discovering tools from MCP servers.
/// Supports concurrent discovery, caching, and filtering.
pub struct DiscoveryEngine {
    /// Discovery configuration
    config: DiscoveryConfig,
    /// Schema cache
    cache: Arc<SchemaCache>,
    /// Schema extractor
    extractor: SchemaExtractor,
    /// Semaphore for concurrency control
    semaphore: Arc<Semaphore>,
    /// Metrics
    metrics: Arc<tokio::sync::RwLock<DiscoveryMetrics>>,
}

impl DiscoveryEngine {
    /// Create a new discovery engine
    pub fn new(config: DiscoveryConfig) -> Self {
        let semaphore = Arc::new(Semaphore::new(config.max_concurrent));
        let cache = Arc::new(SchemaCache::new(config.cache_config.clone()));
        let extractor = SchemaExtractor::new().with_strict(config.validate_schemas);

        Self {
            config,
            cache,
            extractor,
            semaphore,
            metrics: Arc::new(tokio::sync::RwLock::new(DiscoveryMetrics::default())),
        }
    }

    /// Create a new engine with default configuration
    pub fn default_engine() -> Self {
        Self::new(DiscoveryConfig::default())
    }

    /// Discover tools from a single server
    pub async fn discover(
        &self,
        transport: &dyn TransportClient,
    ) -> Result<DiscoveryResult> {
        self.discover_with_filter(transport, &DiscoveryFilter::default())
            .await
    }

    /// Discover tools from a single server with filtering
    pub async fn discover_with_filter(
        &self,
        transport: &dyn TransportClient,
        filter: &DiscoveryFilter,
    ) -> Result<DiscoveryResult> {
        let server_url = transport.endpoint();
        let start = Instant::now();

        // Check cache first
        let _cache_key = format!("{}::all_tools", server_url);

        // Try to get from cache
        if let Some(cached_schemas) = self.cache_from_server(&server_url).await? {
            let filtered = cached_schemas
                .into_iter()
                .filter(|s| filter.matches(s))
                .collect();

            let result = DiscoveryResult::new(server_url.clone())
                .with_tools(filtered)
                .with_cached(true)
                .with_duration(start.elapsed().as_millis() as u64);

            // Update metrics
            self.record_cache_hit(&server_url).await;
            self.update_metrics(&server_url, &result, true).await;

            return Ok(result);
        }

        // Acquire semaphore for concurrency control
        let _permit = timeout(
            Duration::from_secs(self.config.timeout_secs),
            self.semaphore.acquire(),
        )
        .await
        .map_err(|_| DiscoveryError::Timeout(self.config.timeout_secs))?
        .map_err(|_| {
            DiscoveryError::ConcurrencyLimitExceeded(self.config.max_concurrent)
        })?;

        // Make the request with retries
        let request = TransportRequest::new("tools/list")
            .with_timeout(self.config.timeout_secs);

        let response = self
            .send_with_retry(transport, &request, self.config.max_retries)
            .await?;

        let duration_ms = start.elapsed().as_millis() as u64;

        // Extract schemas
        let mut schemas = self.extractor.extract_tools(&response.result)?;

        // Validate if enabled
        if self.config.validate_schemas {
            schemas = self.validate_and_filter_schemas(schemas)?;
        }

        // Apply filter
        let filtered_schemas = schemas
            .iter()
            .filter(|s| filter.matches(s))
            .cloned()
            .collect();

        // Cache the results
        for schema in &schemas {
            let key = SchemaCache::make_key(&server_url, &schema.name);
            self.cache.insert(key, schema.clone()).await?;
        }

        let result = DiscoveryResult::new(server_url.clone())
            .with_tools(filtered_schemas)
            .with_duration(duration_ms);

        // Update metrics
        self.record_cache_miss(&server_url).await;
        self.update_metrics(&server_url, &result, true).await;

        Ok(result)
    }

    /// Discover tools from multiple servers concurrently
    pub async fn discover_all(
        &self,
        transports: &[Arc<dyn TransportClient>],
    ) -> Result<Vec<DiscoveryResult>> {
        self.discover_all_with_filter(transports, &DiscoveryFilter::default())
            .await
    }

    /// Discover tools from multiple servers with filtering
    pub async fn discover_all_with_filter(
        &self,
        transports: &[Arc<dyn TransportClient>],
        filter: &DiscoveryFilter,
    ) -> Result<Vec<DiscoveryResult>> {
        let futures: Vec<_> = transports
            .iter()
            .map(|transport| {
                let filter = filter.clone();
                async move {
                    self.discover_with_filter(transport.as_ref(), &filter)
                        .await
                }
            })
            .collect();

        let results = futures::future::join_all(futures).await;

        // Check for failures
        let successes = results.iter().filter(|r| r.is_ok()).count();
        let total = results.len();

        if successes == 0 {
            return Err(DiscoveryError::AllAttemptsFailed(0, total));
        }

        // Collect results, converting errors to result objects with errors
        let final_results: Vec<DiscoveryResult> = results
            .into_iter()
            .zip(transports.iter())
            .map(|(result, transport)| match result {
                Ok(r) => r,
                Err(e) => DiscoveryResult::new(transport.endpoint())
                    .with_error(format!("Discovery failed: {}", e)),
            })
            .collect();

        // Update total metrics
        let mut metrics = self.metrics.write().await;
        metrics.total_discoveries += total as u64;
        metrics.successful_discoveries += successes as u64;
        metrics.failed_discoveries += (total - successes) as u64;

        Ok(final_results)
    }

    /// Get the cache
    pub fn cache(&self) -> &Arc<SchemaCache> {
        &self.cache
    }

    /// Get current metrics
    pub async fn metrics(&self) -> DiscoveryMetrics {
        self.metrics.read().await.clone()
    }

    /// Reset metrics
    pub async fn reset_metrics(&self) {
        let mut metrics = self.metrics.write().await;
        *metrics = DiscoveryMetrics::default();
    }

    /// Send a request with retry logic
    async fn send_with_retry(
        &self,
        transport: &dyn TransportClient,
        request: &TransportRequest,
        max_retries: usize,
    ) -> Result<crate::discovery::transport::TransportResponse> {
        let mut attempt = 0;

        loop {
            match transport.send(request.clone()).await {
                Ok(response) => return Ok(response),
                Err(_e) if attempt < max_retries => {
                    attempt += 1;
                    let delay = Duration::from_millis(1000 * attempt as u64);
                    tokio::time::sleep(delay).await;
                    tracing::warn!(
                        "Request to {} failed (attempt {}/{}), retrying...",
                        transport.endpoint(),
                        attempt,
                        max_retries
                    );
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Cache all tools from a server
    async fn cache_from_server(&self, server_url: &str) -> Result<Option<Vec<ToolSchema>>> {
        let entries = self.cache.entries().await;
        let server_schemas: Vec<_> = entries
            .into_iter()
            .filter(|(key, _)| key.starts_with(server_url))
            .map(|(_, schema)| schema)
            .collect();

        Ok(if server_schemas.is_empty() {
            None
        } else {
            Some(server_schemas)
        })
    }

    /// Validate and filter schemas, removing invalid ones
    fn validate_and_filter_schemas(&self, schemas: Vec<ToolSchema>) -> Result<Vec<ToolSchema>> {
        let mut valid_schemas = Vec::new();

        for schema in schemas {
            match self.extractor.validate(&schema) {
                Ok(()) => valid_schemas.push(schema),
                Err(e) => {
                    tracing::warn!("Invalid schema {}: {}", schema.name, e);
                    if self.extractor.strict {
                        return Err(DiscoveryError::SchemaValidation(format!(
                            "Schema validation failed: {}",
                            e
                        )));
                    }
                }
            }
        }

        Ok(valid_schemas)
    }

    /// Record a cache hit
    async fn record_cache_hit(&self, server_url: &str) {
        let mut metrics = self.metrics.write().await;
        metrics.cache_hits += 1;

        // Update server-specific metrics
        if let Some(server_metric) = metrics
            .server_metrics
            .iter_mut()
            .find(|m| m.server_url == server_url)
        {
            server_metric.success_count += 1;
        } else {
            metrics.server_metrics.push(ServerMetrics {
                server_url: server_url.to_string(),
                query_count: 1,
                success_count: 1,
                failure_count: 0,
                avg_response_time_ms: 0.0,
                total_tools: 0,
            });
        }
    }

    /// Record a cache miss
    async fn record_cache_miss(&self, _server_url: &str) {
        let mut metrics = self.metrics.write().await;
        metrics.cache_misses += 1;
    }

    /// Update metrics after a discovery
    async fn update_metrics(&self, server_url: &str, result: &DiscoveryResult, success: bool) {
        let mut metrics = self.metrics.write().await;

        metrics.total_tools_discovered += result.tool_count() as u64;

        // Calculate average discovery time
        let total_time = metrics.avg_discovery_time_ms * metrics.total_discoveries as f64;
        let new_count = metrics.total_discoveries as f64 + 1.0;
        metrics.avg_discovery_time_ms = (total_time + result.duration_ms as f64) / new_count;
        metrics.total_discoveries += 1;

        // Update or create server metrics
        if let Some(server_metric) = metrics
            .server_metrics
            .iter_mut()
            .find(|m| m.server_url == server_url)
        {
            server_metric.query_count += 1;
            server_metric.total_tools += result.tool_count() as u64;
            if success {
                server_metric.success_count += 1;
            } else {
                server_metric.failure_count += 1;
            }

            // Update average response time
            let total_time = server_metric.avg_response_time_ms * server_metric.query_count as f64;
            server_metric.avg_response_time_ms =
                (total_time + result.duration_ms as f64) / server_metric.query_count as f64;
        } else {
            metrics.server_metrics.push(ServerMetrics {
                server_url: server_url.to_string(),
                query_count: 1,
                success_count: if success { 1 } else { 0 },
                failure_count: if success { 0 } else { 1 },
                avg_response_time_ms: result.duration_ms as f64,
                total_tools: result.tool_count() as u64,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discovery::transport::{HttpTransport, SseTransport};

    #[test]
    fn test_discovery_config_default() {
        let config = DiscoveryConfig::default();
        assert_eq!(config.timeout_secs, 30);
        assert_eq!(config.max_concurrent, 10);
        assert!(config.validate_schemas);
        assert_eq!(config.max_retries, 3);
    }

    #[test]
    fn test_discovery_config_builder() {
        let config = DiscoveryConfig::builder()
            .timeout(60)
            .max_concurrent(20)
            .cache_ttl(600)
            .cache_max_entries(500)
            .validate(false)
            .retries(5)
            .build();

        assert_eq!(config.timeout_secs, 60);
        assert_eq!(config.max_concurrent, 20);
        assert_eq!(config.cache_config.ttl, Duration::from_secs(600));
        assert_eq!(config.cache_config.max_entries, 500);
        assert!(!config.validate_schemas);
        assert_eq!(config.max_retries, 5);
    }

    #[test]
    fn test_discovery_result_builder() {
        let result = DiscoveryResult::new("http://localhost:3000".to_string())
            .with_tool(ToolSchema::new("test", "Test tool"))
            .with_cached(true)
            .with_duration(150);

        assert_eq!(result.server_url, "http://localhost:3000");
        assert_eq!(result.tool_count(), 1);
        assert!(result.cached);
        assert_eq!(result.duration_ms, 150);
    }

    #[tokio::test]
    async fn test_discovery_engine_creation() {
        let config = DiscoveryConfig::default();
        let engine = DiscoveryEngine::new(config);

        // Verify engine is created
        assert_eq!(engine.config.max_concurrent, 10);
    }

    #[tokio::test]
    async fn test_discovery_engine_default() {
        let engine = DiscoveryEngine::default_engine();
        assert_eq!(engine.config.max_concurrent, 10);
    }

    #[tokio::test]
    async fn test_discovery_metrics() {
        let config = DiscoveryConfig::default();
        let engine = DiscoveryEngine::new(config);

        let metrics = engine.metrics().await;
        assert_eq!(metrics.total_discoveries, 0);
        assert_eq!(metrics.cache_hits, 0);
        assert_eq!(metrics.cache_misses, 0);
    }

    #[tokio::test]
    async fn test_discovery_metrics_reset() {
        let config = DiscoveryConfig::default();
        let engine = DiscoveryEngine::new(config.clone());

        // Simulate some activity
        let mut metrics = engine.metrics.write().await;
        metrics.total_discoveries = 10;
        metrics.successful_discoveries = 8;
        drop(metrics);

        // Reset
        engine.reset_metrics().await;

        // Verify reset
        let metrics = engine.metrics().await;
        assert_eq!(metrics.total_discoveries, 0);
    }

    #[tokio::test]
    async fn test_server_metrics() {
        let server_metric = ServerMetrics {
            server_url: "http://localhost:3000".to_string(),
            query_count: 10,
            success_count: 8,
            failure_count: 2,
            avg_response_time_ms: 150.5,
            total_tools: 50,
        };

        assert_eq!(server_metric.server_url, "http://localhost:3000");
        assert_eq!(server_metric.query_count, 10);
        assert_eq!(server_metric.total_tools, 50);
    }
}
