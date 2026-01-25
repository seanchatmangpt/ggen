//! TAI Gateway: Production-grade API Gateway
//!
//! A comprehensive API gateway implementation with:
//! - Request routing with weighted load balancing and failover
//! - OAuth2/OIDC authentication with Vault integration
//! - Multiple rate limiting strategies (token bucket, sliding window, adaptive)
//! - Health checking and circuit breaker patterns
//! - Request/response transformation pipeline
//! - Full observability with tracing, metrics, and structured logging
//!
//! # Architecture
//!
//! The gateway follows a layered architecture:
//!
//! 1. **Error Handling**: Comprehensive error types with HTTP status mapping
//! 2. **Routing**: Pattern-matching router with weighted upstream selection
//! 3. **Upstream Management**: Service pool with health tracking and failover
//! 4. **Rate Limiting**: Multiple strategies for request throttling
//! 5. **Authentication**: OAuth2/OIDC with JWT validation
//! 6. **Health Checking**: Upstream probes and circuit breaker patterns
//! 7. **Transformation**: Request/response modification pipeline
//! 8. **Observability**: Metrics collection and structured logging

#![warn(missing_docs)]
#![warn(missing_debug_implementations)]
#![warn(unused_results)]

/// Error handling module
pub mod error;

/// Request routing engine
pub mod routing;

/// Rate limiting strategies
pub mod ratelimit;

/// Authentication and authorization
pub mod auth;

/// Health checking and circuit breaker
pub mod health;

/// Request/response transformation
pub mod transform;

/// Observability layer
pub mod observability;

/// Middleware chain
pub mod middleware;

// Re-export commonly used types
pub use error::{GatewayError, GatewayResult, ErrorResponse};
pub use routing::{Router, RouteConfig, RouteRegistry, Upstream, UpstreamPool};
pub use ratelimit::{RateLimitConfig, RateLimitStrategy, ClientId};
pub use auth::{AuthToken, AuthContext, JwtAuthProvider, AuthProvider, InMemoryTokenStore};
pub use health::{CircuitBreaker, CircuitBreakerConfig, CircuitState, HealthProbe, HealthCheckResult};
pub use transform::{RequestTransform, ResponseTransform, TransformationPipeline, TransformRule};
pub use observability::{MetricsCollector, RequestMetrics, LogEvent};

/// Gateway configuration
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct GatewayConfig {
    /// Gateway name
    pub name: String,
    /// Listen address
    pub listen_addr: String,
    /// Listen port
    pub listen_port: u16,
    /// Enable HTTP/2 support
    pub enable_http2: bool,
    /// Enable gRPC support
    pub enable_grpc: bool,
    /// Default request timeout in milliseconds
    pub default_timeout_ms: u64,
    /// Maximum concurrent connections
    pub max_concurrent_connections: Option<u32>,
}

impl Default for GatewayConfig {
    fn default() -> Self {
        Self {
            name: "TAI Gateway".to_string(),
            listen_addr: "127.0.0.1".to_string(),
            listen_port: 8080,
            enable_http2: true,
            enable_grpc: false,
            default_timeout_ms: 30000,
            max_concurrent_connections: None,
        }
    }
}

/// Main gateway instance
#[derive(Debug, Clone)]
pub struct Gateway {
    config: GatewayConfig,
    router: Router,
    auth: std::sync::Arc<dyn auth::AuthProvider>,
    metrics: observability::MetricsCollector,
}

impl Gateway {
    /// Create a new gateway instance
    pub fn new(
        config: GatewayConfig,
        router: Router,
        auth: std::sync::Arc<dyn auth::AuthProvider>,
    ) -> Self {
        Self {
            config,
            router,
            auth,
            metrics: observability::MetricsCollector::new(),
        }
    }

    /// Get the configuration
    pub fn config(&self) -> &GatewayConfig {
        &self.config
    }

    /// Get the router
    pub fn router(&self) -> &Router {
        &self.router
    }

    /// Get the metrics collector
    pub fn metrics(&self) -> &observability::MetricsCollector {
        &self.metrics
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gateway_config_default() {
        let config = GatewayConfig::default();
        assert_eq!(config.name, "TAI Gateway");
        assert_eq!(config.listen_port, 8080);
        assert_eq!(config.enable_http2, true);
    }

    #[test]
    fn test_gateway_creation() {
        let config = GatewayConfig::default();
        let router = Router::default();
        let auth = std::sync::Arc::new(InMemoryTokenStore::new()) as std::sync::Arc<dyn auth::AuthProvider>;

        let gateway = Gateway::new(config, router, auth);
        assert_eq!(gateway.config().listen_port, 8080);
    }
}
