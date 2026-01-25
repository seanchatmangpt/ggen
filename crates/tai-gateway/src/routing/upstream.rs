//! Upstream service management with health checking and failover

use crate::error::{GatewayError, GatewayResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};
use tokio::sync::RwLock;

/// Upstream service configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Upstream {
    /// Unique upstream identifier
    pub id: String,
    /// Service URL
    pub url: String,
    /// Weight for load balancing (default: 1)
    pub weight: u32,
    /// Maximum concurrent connections
    pub max_connections: Option<u32>,
    /// Health check interval in milliseconds
    pub health_check_interval_ms: u64,
    /// Health check timeout in milliseconds
    pub health_check_timeout_ms: u64,
    /// Health check path (e.g., "/health")
    pub health_check_path: String,
}

impl Upstream {
    /// Create a new upstream configuration
    pub fn new(id: impl Into<String>, url: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            url: url.into(),
            weight: 1,
            max_connections: None,
            health_check_interval_ms: 10000,
            health_check_timeout_ms: 5000,
            health_check_path: "/health".to_string(),
        }
    }

    /// Set weight for this upstream
    pub fn with_weight(mut self, weight: u32) -> Self {
        self.weight = weight;
        self
    }

    /// Set health check path
    pub fn with_health_check_path(mut self, path: impl Into<String>) -> Self {
        self.health_check_path = path.into();
        self
    }
}

/// Upstream health status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "UPPERCASE")]
pub enum HealthStatus {
    Healthy,
    Unhealthy,
    Unknown,
}

/// Tracked upstream instance with health state
#[derive(Debug, Clone)]
pub struct UpstreamInstance {
    config: Upstream,
    healthy: Arc<tokio::sync::Mutex<HealthStatus>>,
    request_count: Arc<AtomicU32>,
    error_count: Arc<AtomicU32>,
}

impl UpstreamInstance {
    /// Create a new upstream instance
    pub fn new(config: Upstream) -> Self {
        Self {
            config,
            healthy: Arc::new(tokio::sync::Mutex::new(HealthStatus::Unknown)),
            request_count: Arc::new(AtomicU32::new(0)),
            error_count: Arc::new(AtomicU32::new(0)),
        }
    }

    /// Get the upstream configuration
    pub fn config(&self) -> &Upstream {
        &self.config
    }

    /// Get current health status
    pub async fn health_status(&self) -> HealthStatus {
        *self.healthy.lock().await
    }

    /// Set health status
    pub async fn set_health_status(&self, status: HealthStatus) {
        *self.healthy.lock().await = status;
    }

    /// Record a successful request
    pub fn record_request(&self) {
        self.request_count.fetch_add(1, Ordering::Relaxed);
    }

    /// Record a failed request
    pub fn record_error(&self) {
        self.error_count.fetch_add(1, Ordering::Relaxed);
    }

    /// Get request statistics
    pub fn stats(&self) -> UpstreamStats {
        UpstreamStats {
            upstream_id: self.config.id.clone(),
            request_count: self.request_count.load(Ordering::Relaxed),
            error_count: self.error_count.load(Ordering::Relaxed),
        }
    }
}

/// Upstream statistics
#[derive(Debug, Clone, Serialize)]
pub struct UpstreamStats {
    pub upstream_id: String,
    pub request_count: u32,
    pub error_count: u32,
}

/// Upstream pool managing multiple upstream instances
#[derive(Debug, Clone)]
pub struct UpstreamPool {
    upstreams: Arc<RwLock<HashMap<String, UpstreamInstance>>>,
}

impl UpstreamPool {
    /// Create a new upstream pool
    pub fn new() -> Self {
        Self {
            upstreams: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register an upstream
    pub async fn register(&self, config: Upstream) -> GatewayResult<()> {
        let instance = UpstreamInstance::new(config);
        let mut upstreams = self.upstreams.write().await;
        upstreams.insert(instance.config.id.clone(), instance);
        Ok(())
    }

    /// Get an upstream by ID
    pub async fn get(&self, id: &str) -> GatewayResult<UpstreamInstance> {
        let upstreams = self.upstreams.read().await;
        upstreams
            .get(id)
            .cloned()
            .ok_or_else(|| GatewayError::RoutingFailed(format!("Upstream not found: {}", id)))
    }

    /// Get all healthy upstreams from a list
    pub async fn get_healthy(
        &self,
        ids: &[String],
    ) -> GatewayResult<Vec<UpstreamInstance>> {
        let upstreams = self.upstreams.read().await;
        let mut healthy = Vec::new();

        for id in ids {
            if let Some(instance) = upstreams.get(id) {
                if instance.health_status().await == HealthStatus::Healthy {
                    healthy.push(instance.clone());
                }
            }
        }

        if healthy.is_empty() {
            return Err(GatewayError::ServiceUnavailable(
                "No healthy upstreams available".to_string(),
            ));
        }

        Ok(healthy)
    }

    /// Select upstream using weighted round-robin
    pub async fn select_weighted(
        &self,
        ids: &[String],
        weights: &HashMap<String, u32>,
    ) -> GatewayResult<UpstreamInstance> {
        let healthy = self.get_healthy(ids).await?;

        // Calculate total weight
        let total_weight: u32 = healthy
            .iter()
            .map(|u| weights.get(&u.config.id).copied().unwrap_or(1))
            .sum();

        if total_weight == 0 {
            return Err(GatewayError::ServiceUnavailable(
                "No valid weights for upstreams".to_string(),
            ));
        }

        // Random selection based on weights
        let random_value = (std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u32)
            % total_weight;

        let mut accumulated = 0;
        for upstream in healthy {
            let weight = weights.get(&upstream.config.id).copied().unwrap_or(1);
            accumulated += weight;

            if random_value < accumulated {
                return Ok(upstream);
            }
        }

        // Fallback to first healthy
        Ok(healthy[0].clone())
    }

    /// List all upstreams
    pub async fn list(&self) -> Vec<UpstreamInstance> {
        let upstreams = self.upstreams.read().await;
        upstreams.values().cloned().collect()
    }

    /// Get statistics for all upstreams
    pub async fn stats(&self) -> Vec<UpstreamStats> {
        let upstreams = self.upstreams.read().await;
        upstreams.values().map(|u| u.stats()).collect()
    }

    /// Remove an upstream
    pub async fn remove(&self, id: &str) -> GatewayResult<()> {
        let mut upstreams = self.upstreams.write().await;
        upstreams
            .remove(id)
            .ok_or_else(|| GatewayError::RoutingFailed(format!("Upstream not found: {}", id)))?;
        Ok(())
    }
}

impl Default for UpstreamPool {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_upstream_creation() {
        let upstream = Upstream::new("upstream-1", "http://localhost:8080");
        assert_eq!(upstream.id, "upstream-1");
        assert_eq!(upstream.url, "http://localhost:8080");
        assert_eq!(upstream.weight, 1);
    }

    #[test]
    fn test_upstream_with_weight() {
        let upstream =
            Upstream::new("upstream-1", "http://localhost:8080").with_weight(50);
        assert_eq!(upstream.weight, 50);
    }

    #[tokio::test]
    async fn test_upstream_instance_health_status() {
        let config = Upstream::new("upstream-1", "http://localhost:8080");
        let instance = UpstreamInstance::new(config);

        assert_eq!(instance.health_status().await, HealthStatus::Unknown);

        instance.set_health_status(HealthStatus::Healthy).await;
        assert_eq!(instance.health_status().await, HealthStatus::Healthy);
    }

    #[tokio::test]
    async fn test_upstream_instance_stats() {
        let config = Upstream::new("upstream-1", "http://localhost:8080");
        let instance = UpstreamInstance::new(config);

        instance.record_request();
        instance.record_request();
        instance.record_error();

        let stats = instance.stats();
        assert_eq!(stats.request_count, 2);
        assert_eq!(stats.error_count, 1);
    }

    #[tokio::test]
    async fn test_upstream_pool_register_and_get() {
        let pool = UpstreamPool::new();
        let config = Upstream::new("upstream-1", "http://localhost:8080");

        pool.register(config).await.unwrap();
        let instance = pool.get("upstream-1").await.unwrap();
        assert_eq!(instance.config.id, "upstream-1");
    }

    #[tokio::test]
    async fn test_upstream_pool_get_healthy() {
        let pool = UpstreamPool::new();

        let config1 = Upstream::new("upstream-1", "http://localhost:8080");
        let config2 = Upstream::new("upstream-2", "http://localhost:8081");

        pool.register(config1).await.unwrap();
        pool.register(config2).await.unwrap();

        let instance1 = pool.get("upstream-1").await.unwrap();
        instance1.set_health_status(HealthStatus::Healthy).await;

        let instance2 = pool.get("upstream-2").await.unwrap();
        instance2.set_health_status(HealthStatus::Unhealthy).await;

        let healthy = pool
            .get_healthy(&["upstream-1".to_string(), "upstream-2".to_string()])
            .await
            .unwrap();

        assert_eq!(healthy.len(), 1);
        assert_eq!(healthy[0].config.id, "upstream-1");
    }

    #[tokio::test]
    async fn test_upstream_pool_list() {
        let pool = UpstreamPool::new();

        pool.register(Upstream::new("upstream-1", "http://localhost:8080"))
            .await
            .unwrap();
        pool.register(Upstream::new("upstream-2", "http://localhost:8081"))
            .await
            .unwrap();

        let upstreams = pool.list().await;
        assert_eq!(upstreams.len(), 2);
    }
}
