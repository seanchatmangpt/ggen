//! Request routing engine with load distribution and failover support
//!
//! This module provides deterministic request routing, weighted load balancing,
//! and automatic failover to healthy upstream services.

use crate::error::{GatewayError, GatewayResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

pub mod router;
pub mod upstream;

pub use router::Router;
pub use upstream::{Upstream, UpstreamPool};

/// Route configuration matching requests to upstreams
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteConfig {
    /// Unique route identifier
    pub id: String,
    /// Path pattern (regex or glob)
    pub path_pattern: String,
    /// HTTP methods this route accepts
    pub methods: Vec<String>,
    /// List of upstream services (order determines failover priority)
    pub upstreams: Vec<String>,
    /// Weight distribution for load balancing (per upstream)
    pub weights: Option<HashMap<String, u32>>,
    /// Request transformation rules
    pub transformations: Option<Vec<RouteTransformation>>,
    /// Timeout in milliseconds
    pub timeout_ms: Option<u64>,
    /// Enable circuit breaker
    pub circuit_breaker_enabled: bool,
}

impl RouteConfig {
    /// Create a new route configuration
    pub fn new(
        id: impl Into<String>,
        path_pattern: impl Into<String>,
        methods: Vec<String>,
        upstreams: Vec<String>,
    ) -> Self {
        Self {
            id: id.into(),
            path_pattern: path_pattern.into(),
            methods,
            upstreams,
            weights: None,
            transformations: None,
            timeout_ms: Some(30000),
            circuit_breaker_enabled: true,
        }
    }

    /// Set custom weights for upstreams
    pub fn with_weights(mut self, weights: HashMap<String, u32>) -> Self {
        self.weights = Some(weights);
        self
    }

    /// Set timeout in milliseconds
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = Some(timeout_ms);
        self
    }

    /// Enable or disable circuit breaker
    pub fn with_circuit_breaker(mut self, enabled: bool) -> Self {
        self.circuit_breaker_enabled = enabled;
        self
    }
}

/// Route transformation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteTransformation {
    /// Transformation type (e.g., "header_add", "path_rewrite")
    pub transform_type: String,
    /// Source field
    pub source: String,
    /// Target field
    pub target: String,
    /// Transformation value
    pub value: Option<String>,
}

/// Request routing context
#[derive(Debug, Clone)]
pub struct RoutingContext {
    /// Request ID for tracing
    pub request_id: String,
    /// Matched route
    pub route: RouteConfig,
    /// Selected upstream for this request
    pub selected_upstream: String,
    /// Attempt count (for retries)
    pub attempt: u32,
}

impl RoutingContext {
    /// Create a new routing context
    pub fn new(route: RouteConfig, selected_upstream: String) -> Self {
        Self {
            request_id: Uuid::new_v4().to_string(),
            route,
            selected_upstream,
            attempt: 1,
        }
    }

    /// Create a new attempt context
    pub fn next_attempt(mut self) -> Self {
        self.attempt += 1;
        self
    }
}

/// Route registry for centralized route management
#[derive(Debug, Clone)]
pub struct RouteRegistry {
    routes: Arc<RwLock<HashMap<String, RouteConfig>>>,
}

impl RouteRegistry {
    /// Create a new route registry
    pub fn new() -> Self {
        Self {
            routes: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a new route
    pub async fn register(&self, route: RouteConfig) -> GatewayResult<()> {
        let mut routes = self.routes.write().await;
        routes.insert(route.id.clone(), route);
        Ok(())
    }

    /// Get a route by ID
    pub async fn get(&self, route_id: &str) -> GatewayResult<RouteConfig> {
        let routes = self.routes.read().await;
        routes
            .get(route_id)
            .cloned()
            .ok_or_else(|| GatewayError::RoutingFailed(format!("Route not found: {}", route_id)))
    }

    /// List all registered routes
    pub async fn list(&self) -> Vec<RouteConfig> {
        let routes = self.routes.read().await;
        routes.values().cloned().collect()
    }

    /// Remove a route
    pub async fn remove(&self, route_id: &str) -> GatewayResult<()> {
        let mut routes = self.routes.write().await;
        routes
            .remove(route_id)
            .ok_or_else(|| GatewayError::RoutingFailed(format!("Route not found: {}", route_id)))?;
        Ok(())
    }
}

impl Default for RouteRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_route_config_creation() {
        let route = RouteConfig::new(
            "test-route",
            "/api/v1/users/*",
            vec!["GET".to_string()],
            vec!["upstream-1".to_string()],
        );

        assert_eq!(route.id, "test-route");
        assert_eq!(route.path_pattern, "/api/v1/users/*");
        assert_eq!(route.methods, vec!["GET"]);
        assert_eq!(route.timeout_ms, Some(30000));
    }

    #[test]
    fn test_route_config_with_weights() {
        let mut weights = HashMap::new();
        weights.insert("upstream-1".to_string(), 70);
        weights.insert("upstream-2".to_string(), 30);

        let route = RouteConfig::new(
            "weighted-route",
            "/api/v1/orders/*",
            vec!["POST".to_string()],
            vec!["upstream-1".to_string(), "upstream-2".to_string()],
        )
        .with_weights(weights.clone());

        assert_eq!(route.weights, Some(weights));
    }

    #[test]
    fn test_routing_context_creation() {
        let route = RouteConfig::new(
            "test-route",
            "/api/v1/test",
            vec!["GET".to_string()],
            vec!["upstream-1".to_string()],
        );

        let context = RoutingContext::new(route.clone(), "upstream-1".to_string());
        assert_eq!(context.route.id, "test-route");
        assert_eq!(context.selected_upstream, "upstream-1");
        assert_eq!(context.attempt, 1);
    }

    #[test]
    fn test_routing_context_next_attempt() {
        let route = RouteConfig::new(
            "test-route",
            "/api/v1/test",
            vec!["GET".to_string()],
            vec!["upstream-1".to_string()],
        );

        let context = RoutingContext::new(route, "upstream-1".to_string());
        let next = context.next_attempt();
        assert_eq!(next.attempt, 2);
    }

    #[tokio::test]
    async fn test_route_registry_register_and_get() {
        let registry = RouteRegistry::new();
        let route = RouteConfig::new(
            "test-route",
            "/api/v1/test",
            vec!["GET".to_string()],
            vec!["upstream-1".to_string()],
        );

        registry.register(route.clone()).await.unwrap();
        let retrieved = registry.get("test-route").await.unwrap();
        assert_eq!(retrieved.id, route.id);
    }

    #[tokio::test]
    async fn test_route_registry_list() {
        let registry = RouteRegistry::new();
        let route1 = RouteConfig::new(
            "route-1",
            "/api/v1/test",
            vec!["GET".to_string()],
            vec!["upstream-1".to_string()],
        );
        let route2 = RouteConfig::new(
            "route-2",
            "/api/v2/test",
            vec!["POST".to_string()],
            vec!["upstream-2".to_string()],
        );

        registry.register(route1).await.unwrap();
        registry.register(route2).await.unwrap();

        let routes = registry.list().await;
        assert_eq!(routes.len(), 2);
    }

    #[tokio::test]
    async fn test_route_registry_remove() {
        let registry = RouteRegistry::new();
        let route = RouteConfig::new(
            "test-route",
            "/api/v1/test",
            vec!["GET".to_string()],
            vec!["upstream-1".to_string()],
        );

        registry.register(route).await.unwrap();
        registry.remove("test-route").await.unwrap();

        let result = registry.get("test-route").await;
        assert!(result.is_err());
    }
}
