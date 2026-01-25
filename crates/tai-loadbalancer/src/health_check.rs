//! Health checking for endpoints

use crate::error::Result;
use crate::service_registry::{Endpoint, ServiceRegistry};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::mpsc;
use tokio::time::interval;

/// Health status of an endpoint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HealthStatus {
    /// Endpoint is healthy and accepting requests
    Healthy,
    /// Endpoint is unhealthy and should be excluded
    Unhealthy,
    /// Endpoint is being checked (transient state)
    Checking,
}

/// Configuration for health checks
#[derive(Debug, Clone)]
pub struct HealthCheckConfig {
    /// Interval between health checks
    pub check_interval: Duration,
    /// Timeout for each health check
    pub check_timeout: Duration,
    /// Number of consecutive failures before marking unhealthy
    pub failure_threshold: usize,
    /// Number of consecutive successes before marking healthy
    pub success_threshold: usize,
    /// Grace period before first health check
    pub grace_period: Duration,
}

impl Default for HealthCheckConfig {
    fn default() -> Self {
        Self {
            check_interval: Duration::from_secs(10),
            check_timeout: Duration::from_secs(5),
            failure_threshold: 3,
            success_threshold: 2,
            grace_period: Duration::from_secs(5),
        }
    }
}

/// Health status tracking for a single endpoint
#[derive(Debug, Clone)]
struct EndpointHealthState {
    status: HealthStatus,
    consecutive_failures: usize,
    consecutive_successes: usize,
    last_check: Option<SystemTime>,
    check_count: usize,
}

impl Default for EndpointHealthState {
    fn default() -> Self {
        Self {
            status: HealthStatus::Checking,
            consecutive_failures: 0,
            consecutive_successes: 0,
            last_check: None,
            check_count: 0,
        }
    }
}

/// Health check metrics
#[derive(Debug, Clone, Default)]
pub struct HealthCheckMetrics {
    /// Total health checks performed
    pub total_checks: u64,
    /// Total failures
    pub total_failures: u64,
    /// Total successes
    pub total_successes: u64,
}

/// Manages health checking for all endpoints
pub struct HealthCheckManager {
    registry: Arc<ServiceRegistry>,
    config: HealthCheckConfig,
    // Map of service → endpoint → health state
    health_states: Arc<DashMap<String, DashMap<String, EndpointHealthState>>>,
    metrics: Arc<parking_lot::RwLock<HealthCheckMetrics>>,
    // Channel for stopping health checks
    shutdown_tx: Arc<parking_lot::Mutex<Option<mpsc::Sender<()>>>>,
}

impl HealthCheckManager {
    /// Create a new health check manager
    pub async fn new(registry: Arc<ServiceRegistry>, config: HealthCheckConfig) -> Result<Self> {
        Ok(Self {
            registry,
            config,
            health_states: Arc::new(DashMap::new()),
            metrics: Arc::new(parking_lot::RwLock::new(HealthCheckMetrics::default())),
            shutdown_tx: Arc::new(parking_lot::Mutex::new(None)),
        })
    }

    /// Start the health check loop
    pub async fn start(&self) -> Result<()> {
        let (tx, mut rx) = mpsc::channel(1);
        *self.shutdown_tx.lock() = Some(tx);

        let registry = self.registry.clone();
        let health_states = self.health_states.clone();
        let config = self.config.clone();
        let metrics = self.metrics.clone();

        // Spawn health check task
        tokio::spawn(async move {
            let mut interval = interval(config.check_interval);

            loop {
                tokio::select! {
                    _ = rx.recv() => {
                        break;
                    }
                    _ = interval.tick() => {
                        // Get all services
                        let services = registry.list_services().await;
                        for service in services {
                            if let Ok(endpoints) = registry.get_endpoints(&service).await {
                                for endpoint in endpoints {
                                    let key = endpoint.address.to_string();
                                    let result = Self::perform_health_check(&endpoint).await;

                                    let mut states = health_states
                                        .entry(service.clone())
                                        .or_insert_with(DashMap::new);

                                    let mut state = states
                                        .entry(key.clone())
                                        .or_insert_with(EndpointHealthState::default);

                                    // Update metrics and state
                                    let mut m = metrics.write();
                                    m.total_checks += 1;

                                    if result.is_ok() {
                                        m.total_successes += 1;
                                        state.consecutive_successes += 1;
                                        state.consecutive_failures = 0;

                                        // Mark as healthy after success_threshold
                                        if state.consecutive_successes >= config.success_threshold {
                                            state.status = HealthStatus::Healthy;
                                        }
                                    } else {
                                        m.total_failures += 1;
                                        state.consecutive_failures += 1;
                                        state.consecutive_successes = 0;

                                        // Mark as unhealthy after failure_threshold
                                        if state.consecutive_failures >= config.failure_threshold {
                                            state.status = HealthStatus::Unhealthy;
                                        }
                                    }

                                    state.last_check = Some(SystemTime::now());
                                    state.check_count += 1;
                                }
                            }
                        }
                    }
                }
            }
        });

        Ok(())
    }

    /// Stop the health check loop
    pub async fn stop(&self) -> Result<()> {
        if let Some(tx) = self.shutdown_tx.lock().take() {
            let _ = tx.send(()).await;
        }
        Ok(())
    }

    /// Check if an endpoint is healthy
    pub async fn is_healthy(&self, service: &str, endpoint: &Endpoint) -> Result<bool> {
        let key = endpoint.address.to_string();

        let states = self
            .health_states
            .entry(service.to_string())
            .or_insert_with(DashMap::new);

        let state = states
            .entry(key)
            .or_insert_with(EndpointHealthState::default);

        Ok(state.status == HealthStatus::Healthy)
    }

    /// Get health status of an endpoint
    pub async fn get_health_status(&self, service: &str, endpoint: &Endpoint) -> Result<HealthStatus> {
        let key = endpoint.address.to_string();

        let states = self
            .health_states
            .entry(service.to_string())
            .or_insert_with(DashMap::new);

        let state = states
            .entry(key)
            .or_insert_with(EndpointHealthState::default);

        Ok(state.status)
    }

    /// Get all healthy endpoints for a service
    pub async fn get_healthy_endpoints(&self, service: &str, endpoints: Vec<Endpoint>) -> Vec<Endpoint> {
        endpoints
            .into_iter()
            .filter(|ep| {
                let key = ep.address.to_string();
                if let Some(states) = self.health_states.get(service) {
                    states
                        .get(&key)
                        .map(|s| s.status == HealthStatus::Healthy)
                        .unwrap_or(true)
                } else {
                    true
                }
            })
            .collect()
    }

    /// Get health check metrics
    pub fn get_metrics(&self) -> HealthCheckMetrics {
        self.metrics.read().clone()
    }

    /// Manually perform a health check (can be overridden for custom checks)
    async fn perform_health_check(endpoint: &Endpoint) -> Result<()> {
        // Simulate health check with timeout
        // In a real implementation, this would do:
        // - TCP connect to the endpoint
        // - gRPC health check
        // - HTTP health endpoint call
        let timeout = Duration::from_secs(5);
        let check = async {
            // For testing: always succeed
            // In production, implement actual health check
            Ok(())
        };

        tokio::time::timeout(timeout, check).await.map_err(|_| {
            crate::error::Error::health_check_failed(format!("Timeout checking {}", endpoint.address))
        })?
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_health_check_manager_creation() {
        let registry = Arc::new(ServiceRegistry::new());
        let config = HealthCheckConfig::default();
        let manager = HealthCheckManager::new(registry, config).await.unwrap();
        assert_eq!(manager.get_metrics().total_checks, 0);
    }

    #[tokio::test]
    async fn test_health_status_tracking() {
        let registry = Arc::new(ServiceRegistry::new());
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        registry
            .register("test-service".to_string(), endpoint.clone())
            .await
            .unwrap();

        let config = HealthCheckConfig::default();
        let manager = HealthCheckManager::new(registry, config).await.unwrap();

        // Check initial status
        let status = manager
            .get_health_status("test-service", &endpoint)
            .await
            .unwrap();
        assert_eq!(status, HealthStatus::Checking);
    }

    #[tokio::test]
    async fn test_get_healthy_endpoints() {
        let registry = Arc::new(ServiceRegistry::new());
        let endpoints = vec![
            Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
        ];

        registry
            .register_batch("test-service".to_string(), endpoints.clone())
            .await
            .unwrap();

        let config = HealthCheckConfig::default();
        let manager = HealthCheckManager::new(registry, config).await.unwrap();

        let healthy = manager
            .get_healthy_endpoints("test-service", endpoints.clone())
            .await;

        // All endpoints should be healthy initially (Checking state treated as healthy)
        assert_eq!(healthy.len(), 2);
    }
}
