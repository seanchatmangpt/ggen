//! MCP Tool Discovery Engine
//!
//! This module provides production-ready tool discovery for MCP servers,
//! supporting concurrent discovery, caching, and multi-transport protocols.
//!
//! ## Architecture
//!
//! The discovery engine follows a layered architecture:
//!
//! ```text
//! ┌───────────────────────────────────────────────┐
//! │           DiscoveryEngine (Public API)         │
//! │  - Concurrent discovery with parallel queries  │
//! │  - Cache integration for schema persistence    │
//! │  - Metrics collection for observability       │
//! ├───────────────────────────────────────────────┤
//! │            SchemaExtractor                     │
//! │  - Parses MCP tool/list responses             │
//! │  - Validates JSON Schema definitions          │
//! │  - Converts to unified ToolSchema format      │
//! ├───────────────────────────────────────────────┤
//! │            Transport Clients                  │
//! │  - StdioTransport for local processes         │
//! │  - SseTransport for Server-Sent Events        │
//! │  - HttpTransport for direct HTTP calls        │
//! ├───────────────────────────────────────────────┤
//! │            SchemaCache                        │
//! │  - TTL-based cache expiration                 │
//! │  - Thread-safe with Arc<RwLock<>>            │
//! │  - In-memory with optional persistence        │
//! └───────────────────────────────────────────────┘
//! ```
//!
//! ## Usage
//!
//! ```rust,no_run
//! use rig_mcp_discovery::{DiscoveryEngine, DiscoveryConfig};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let config = DiscoveryConfig::builder()
//!         .timeout(Duration::from_secs(30))
//!         .cache_ttl(Duration::from_secs(300))
//!         .max_concurrent(10)
//!         .build();
//!
//!     let engine = DiscoveryEngine::new(config);
//!
//!     // Discover tools from multiple servers
//!     let server_urls = vec![
//!         "http://localhost:3000/mcp".to_string(),
//!         "http://localhost:3001/mcp".to_string(),
//!     ];
//!
//!     let tools = engine.discover_all(&server_urls).await?;
//!
//!     for tool in tools {
//!         println!("Found tool: {}", tool.name);
//!     }
//!
//!     Ok(())
//! }
//! ```

mod cache;
mod engine;
mod schema;
mod transport;

pub use cache::{SchemaCache, CacheConfig, CacheEntry, CacheStats};
pub use engine::{DiscoveryEngine, DiscoveryConfig, DiscoveryResult, DiscoveryMetrics};
pub use schema::{
    SchemaExtractor, ToolSchema, ParameterSchema, ToolSchemaValidationError,
    extract_schemas, validate_schema,
};
pub use transport::{
    TransportClient, TransportType, TransportRequest, TransportResponse,
    StdioTransport, SseTransport, HttpTransport, TransportError,
};

/// Result type for discovery operations
pub type Result<T> = std::result::Result<T, DiscoveryError>;

/// Errors that can occur during tool discovery
#[derive(Debug, thiserror::Error)]
pub enum DiscoveryError {
    /// Transport-level error (connection, timeout, protocol)
    #[error("transport error: {0}")]
    Transport(#[from] TransportError),

    /// Schema validation error
    #[error("schema validation failed: {0}")]
    SchemaValidation(String),

    /// Cache error
    #[error("cache error: {0}")]
    Cache(String),

    /// Timeout during discovery
    #[error("discovery timeout after {0}s")]
    Timeout(u64),

    /// Invalid server response
    #[error("invalid response from server: {0}")]
    InvalidResponse(String),

    /// Concurrent discovery limit exceeded
    #[error("concurrent discovery limit exceeded: max={0}")]
    ConcurrencyLimitExceeded(usize),

    /// All discovery attempts failed
    #[error("all discovery attempts failed: {0}/{1} succeeded")]
    AllAttemptsFailed(usize, usize),

    /// Configuration error
    #[error("configuration error: {0}")]
    Configuration(String),

    /// JSON parsing error
    #[error("json parsing error: {0}")]
    JsonParse(String),

    /// IO error
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
}

/// Filter options for tool discovery
#[derive(Debug, Clone, Default)]
pub struct DiscoveryFilter {
    /// Include only tools matching these name patterns
    pub name_patterns: Vec<String>,
    /// Exclude tools matching these name patterns
    pub exclude_patterns: Vec<String>,
    /// Include only tools with these tags
    pub tags: Vec<String>,
    /// Minimum schema complexity level (0-10)
    pub min_complexity: Option<u8>,
}

impl DiscoveryFilter {
    /// Create a new filter with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a name pattern to include
    pub fn with_name_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.name_patterns.push(pattern.into());
        self
    }

    /// Add a name pattern to exclude
    pub fn with_exclude_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.exclude_patterns.push(pattern.into());
        self
    }

    /// Add a required tag
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }

    /// Set minimum complexity
    pub fn with_min_complexity(mut self, level: u8) -> Self {
        self.min_complexity = Some(level);
        self
    }

    /// Check if a tool schema matches this filter
    pub fn matches(&self, schema: &ToolSchema) -> bool {
        // Check exclude patterns first
        for pattern in &self.exclude_patterns {
            if schema.name.contains(pattern) {
                return false;
            }
        }

        // Check include patterns if any are specified
        if !self.name_patterns.is_empty() {
            let matches = self.name_patterns.iter().any(|p| schema.name.contains(p));
            if !matches {
                return false;
            }
        }

        // Check tags if any are required
        if !self.tags.is_empty() {
            let has_tags = schema.tags.iter().any(|t| self.tags.contains(t));
            if !has_tags {
                return false;
            }
        }

        // Check complexity
        if let Some(min_complexity) = self.min_complexity {
            if schema.complexity() < min_complexity {
                return false;
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discovery_filter_default() {
        let filter = DiscoveryFilter::new();
        assert!(filter.name_patterns.is_empty());
        assert!(filter.exclude_patterns.is_empty());
        assert!(filter.tags.is_empty());
        assert!(filter.min_complexity.is_none());
    }

    #[test]
    fn test_discovery_filter_builder() {
        let filter = DiscoveryFilter::new()
            .with_name_pattern("test")
            .with_exclude_pattern("internal")
            .with_tag("production")
            .with_min_complexity(5);

        assert_eq!(filter.name_patterns, vec!["test"]);
        assert_eq!(filter.exclude_patterns, vec!["internal"]);
        assert_eq!(filter.tags, vec!["production"]);
        assert_eq!(filter.min_complexity, Some(5));
    }

    #[test]
    fn test_discovery_filter_matches() {
        let filter = DiscoveryFilter::new()
            .with_name_pattern("read")
            .with_exclude_pattern("internal")
            .with_tag("data");

        let mut schema = ToolSchema {
            name: "read_user".to_string(),
            description: "Read a user".to_string(),
            parameters: None,
            tags: vec!["data".to_string(), "user".to_string()],
        };

        assert!(filter.matches(&schema));

        schema.name = "internal_read".to_string();
        assert!(!filter.matches(&schema));

        schema.name = "delete_user".to_string();
        assert!(!filter.matches(&schema));
    }
}
