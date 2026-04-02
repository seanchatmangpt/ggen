//! Core router implementation with pattern matching and load balancing

use crate::error::{GatewayError, GatewayResult};
use crate::routing::{RouteConfig, RouteRegistry, RoutingContext, UpstreamPool};
use regex::Regex;
use std::collections::HashMap;
use std::sync::Arc;

/// Router for matching requests to routes and selecting upstreams
#[derive(Debug, Clone)]
pub struct Router {
    registry: RouteRegistry,
    pool: UpstreamPool,
    // Compiled regex patterns for faster matching
    pattern_cache: Arc<tokio::sync::RwLock<HashMap<String, Result<Regex, regex::Error>>>>,
}

impl Router {
    /// Create a new router
    pub fn new(registry: RouteRegistry, pool: UpstreamPool) -> Self {
        Self {
            registry,
            pool,
            pattern_cache: Arc::new(tokio::sync::RwLock::new(HashMap::new())),
        }
    }

    /// Route a request to an upstream
    pub async fn route(
        &self,
        method: &str,
        path: &str,
    ) -> GatewayResult<RoutingContext> {
        // Find matching route
        let route = self.find_matching_route(method, path).await?;

        // Select upstream using weighted load balancing
        let weights = route.weights.clone().unwrap_or_default();
        let selected_upstream = self
            .pool
            .select_weighted(&route.upstreams, &weights)
            .await?;

        Ok(RoutingContext::new(route, selected_upstream.config.id))
    }

    /// Route request with retry support
    pub async fn route_with_retry(
        &self,
        method: &str,
        path: &str,
        max_retries: u32,
    ) -> GatewayResult<RoutingContext> {
        let mut last_error = None;

        for attempt in 0..=max_retries {
            match self.route(method, path).await {
                Ok(mut context) => {
                    context.attempt = attempt + 1;
                    return Ok(context);
                }
                Err(e) => {
                    last_error = Some(e);
                    if attempt < max_retries {
                        // Wait before retry with exponential backoff
                        let backoff_ms = 10 * 2_u64.pow(attempt);
                        tokio::time::sleep(std::time::Duration::from_millis(backoff_ms)).await;
                    }
                }
            }
        }

        Err(last_error.unwrap_or_else(|| {
            GatewayError::RoutingFailed("No routes available".to_string())
        }))
    }

    /// Find matching route for a request
    async fn find_matching_route(
        &self,
        method: &str,
        path: &str,
    ) -> GatewayResult<RouteConfig> {
        let routes = self.registry.list().await;

        for route in routes {
            if !route.methods.contains(&method.to_string()) {
                continue;
            }

            if self.pattern_matches(&route.path_pattern, path).await? {
                return Ok(route);
            }
        }

        Err(GatewayError::RoutingFailed(format!(
            "No route found for {} {}",
            method, path
        )))
    }

    /// Check if a path matches a pattern
    async fn pattern_matches(&self, pattern: &str, path: &str) -> GatewayResult<bool> {
        // Check cache first
        let mut cache = self.pattern_cache.write().await;

        let regex = if let Some(result) = cache.get(pattern) {
            result.clone()?
        } else {
            // Convert glob patterns to regex
            let regex_pattern = self.glob_to_regex(pattern);
            let result = Regex::new(&regex_pattern);
            cache.insert(pattern.to_string(), result.clone());
            result?
        };

        Ok(regex.is_match(path))
    }

    /// Convert glob pattern to regex
    fn glob_to_regex(&self, pattern: &str) -> String {
        let mut regex = String::with_capacity(pattern.len() * 2);
        regex.push('^');

        let mut chars = pattern.chars().peekable();
        while let Some(ch) = chars.next() {
            match ch {
                '?' => regex.push_str("."),
                '*' => {
                    if chars.peek() == Some(&'*') {
                        chars.next();
                        regex.push_str(".*");
                    } else {
                        regex.push_str("[^/]*");
                    }
                }
                '.' => regex.push_str("\\."),
                '+' => regex.push_str("\\+"),
                '^' => regex.push_str("\\^"),
                '$' => regex.push_str("\\$"),
                '(' => regex.push_str("\\("),
                ')' => regex.push_str("\\)"),
                '[' => regex.push_str("\\["),
                ']' => regex.push_str("\\]"),
                '{' => regex.push_str("\\{"),
                '}' => regex.push_str("\\}"),
                '|' => regex.push_str("\\|"),
                '\\' => regex.push_str("\\\\"),
                c => regex.push(c),
            }
        }

        regex.push('$');
        regex
    }
}

impl Default for Router {
    fn default() -> Self {
        Self::new(RouteRegistry::new(), UpstreamPool::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::routing::Upstream;

    #[tokio::test]
    async fn test_router_pattern_matching() {
        let router = Router::default();

        // Test exact path matching
        assert!(router
            .pattern_matches("/api/v1/users", "/api/v1/users")
            .await
            .unwrap());

        // Test glob pattern matching
        assert!(router
            .pattern_matches("/api/v1/users/*", "/api/v1/users/123")
            .await
            .unwrap());

        // Test wildcard patterns
        assert!(router
            .pattern_matches("/api/*/users/*", "/api/v1/users/123")
            .await
            .unwrap());

        // Test non-matching
        assert!(!router
            .pattern_matches("/api/v1/users/*", "/api/v2/users/123")
            .await
            .unwrap());
    }

    #[tokio::test]
    async fn test_router_glob_to_regex() {
        let router = Router::default();

        // Test glob conversion
        let regex = router.glob_to_regex("/api/v1/users/*");
        assert!(Regex::new(&regex).unwrap().is_match("/api/v1/users/123"));

        let regex = router.glob_to_regex("/api/*/users/**");
        assert!(Regex::new(&regex).unwrap().is_match("/api/v1/users/123/orders"));
    }

    #[tokio::test]
    async fn test_router_find_matching_route() {
        let registry = RouteRegistry::new();
        let pool = UpstreamPool::new();
        let router = Router::new(registry.clone(), pool.clone());

        // Register route
        let route = RouteConfig::new(
            "users-route",
            "/api/v1/users/*",
            vec!["GET".to_string(), "POST".to_string()],
            vec!["upstream-1".to_string()],
        );
        registry.register(route).await.unwrap();

        // Register upstream
        pool.register(Upstream::new("upstream-1", "http://localhost:8080"))
            .await
            .unwrap();

        // Test matching
        let found = router.find_matching_route("GET", "/api/v1/users/123").await;
        assert!(found.is_ok());

        // Test non-matching method
        let not_found = router.find_matching_route("DELETE", "/api/v1/users/123").await;
        assert!(not_found.is_err());
    }

    #[tokio::test]
    async fn test_router_route_request() {
        let registry = RouteRegistry::new();
        let pool = UpstreamPool::new();
        let router = Router::new(registry.clone(), pool.clone());

        // Setup route and upstream
        let route = RouteConfig::new(
            "users-route",
            "/api/v1/users/*",
            vec!["GET".to_string()],
            vec!["upstream-1".to_string()],
        );
        registry.register(route).await.unwrap();

        let upstream = Upstream::new("upstream-1", "http://localhost:8080");
        pool.register(upstream).await.unwrap();

        // Mark as healthy
        let instance = pool.get("upstream-1").await.unwrap();
        instance
            .set_health_status(crate::routing::upstream::HealthStatus::Healthy)
            .await;

        // Route request
        let context = router.route("GET", "/api/v1/users/123").await.unwrap();
        assert_eq!(context.route.id, "users-route");
        assert_eq!(context.selected_upstream, "upstream-1");
    }
}
