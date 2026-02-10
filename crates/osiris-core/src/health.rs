//! Health Monitoring System
//!
//! Monitors the health of OSIRIS components and overall system wellness

use crate::{OSIRISSignal, SignalLevel};
use chrono::{DateTime, Utc};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::{Duration, interval};
use tracing::{debug, error, info, warn};

/// Health status levels
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HealthStatus {
    /// System is healthy
    Healthy,
    /// System has warnings but is operational
    Warning,
    /// System has issues and needs attention
    Critical,
    /// System is down
    Down,
}

impl std::fmt::Display for HealthStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HealthStatus::Healthy => write!(f, "HEALTHY"),
            HealthStatus::Warning => write!(f, "WARNING"),
            HealthStatus::Critical => write!(f, "CRITICAL"),
            HealthStatus::Down => write!(f, "DOWN"),
        }
    }
}

/// Health check result for a component
#[derive(Debug, Clone)]
pub struct HealthCheckResult {
    /// Component name
    pub component: String,
    /// Health status
    pub status: HealthStatus,
    /// Check timestamp
    pub timestamp: DateTime<Utc>,
    /// Additional metrics
    pub metrics: HashMap<String, Value>,
    /// Error message if unhealthy
    pub error: Option<String>,
}

/// Health monitor for OSIRIS components
pub struct HealthMonitor {
    interval_ms: u64,
    components: Arc<RwLock<HashMap<String, ComponentHealth>>>,
    last_full_check: Arc<RwLock<Option<DateTime<Utc>>>>,
}

/// Health information for a component
#[derive(Debug, Clone)]
struct ComponentHealth {
    name: String,
    status: HealthStatus,
    last_check: DateTime<Utc>,
    error_count: usize,
    consecutive_errors: usize,
    metrics: HashMap<String, Value>,
}

impl HealthMonitor {
    /// Create a new health monitor
    pub fn new() -> Self {
        Self {
            interval_ms: 30000, // 30 seconds default
            components: Arc::new(RwLock::new(HashMap::new())),
            last_full_check: Arc::new(RwLock::new(None)),
        }
    }

    /// Create a health monitor with custom interval
    pub fn with_interval(interval_ms: u64) -> Self {
        Self {
            interval_ms,
            components: Arc::new(RwLock::new(HashMap::new())),
            last_full_check: Arc::new(RwLock::new(None)),
        }
    }

    /// Get the health check interval
    pub fn interval_ms(&self) -> u64 {
        self.interval_ms
    }

    /// Register a component for health monitoring
    pub async fn register_component(&self, name: &str) {
        let mut components = self.components.write().await;
        components.insert(
            name.to_string(),
            ComponentHealth {
                name: name.to_string(),
                status: HealthStatus::Healthy,
                last_check: Utc::now(),
                error_count: 0,
                consecutive_errors: 0,
                metrics: HashMap::new(),
            },
        );
        info!("Registered component for health monitoring: {}", name);
    }

    /// Report health for a component
    pub async fn report_health(
        &self,
        component: &str,
        status: HealthStatus,
        metrics: HashMap<String, Value>,
        error: Option<String>,
    ) {
        let mut components = self.components.write().await;

        if let Some(comp_health) = components.get_mut(component) {
            comp_health.status = status;
            comp_health.last_check = Utc::now();
            comp_health.metrics = metrics;

            // Update error tracking
            match status {
                HealthStatus::Healthy => {
                    comp_health.consecutive_errors = 0;
                }
                _ => {
                    comp_health.error_count += 1;
                    comp_health.consecutive_errors += 1;

                    if let Some(err) = error {
                        warn!(
                            "Component {} unhealthy: {} (consecutive errors: {})",
                            component,
                            err,
                            comp_health.consecutive_errors
                        );
                    }
                }
            }
        }
    }

    /// Check health of all components
    pub async fn check_health(&self) -> HealthStatus {
        let components = self.components.read().await;
        let mut overall_status = HealthStatus::Healthy;

        for (name, health) in components.iter() {
            debug!("Checking health of component: {}", name);

            // Check if component needs attention
            if health.status == HealthStatus::Critical
                || health.status == HealthStatus::Down
            {
                overall_status = match overall_status {
                    HealthStatus::Healthy => health.status.clone(),
                    HealthStatus::Warning => HealthStatus::Warning,
                    HealthStatus::Critical => HealthStatus::Critical,
                    HealthStatus::Down => HealthStatus::Down,
                };
            } else if health.status == HealthStatus::Warning && overall_status == HealthStatus::Healthy
            {
                overall_status = HealthStatus::Warning;
            }
        }

        // Update last full check time
        let mut last_check = self.last_full_check.write().await;
        *last_check = Some(Utc::now());

        info!("Overall system health: {}", overall_status);
        overall_status
    }

    /// Run comprehensive health check
    pub async fn run_health_check(&self) -> Result<(), Box<dyn std::error::Error>> {
        let components = self.components.read().await;
        let mut check_results = Vec::new();

        for (name, health) in components.iter() {
            // Check if component is responsive
            let metrics = self.check_component_metrics(name).await?;

            // Determine status based on metrics
            let status = self.evaluate_component_status(name, &metrics).await?;

            let result = HealthCheckResult {
                component: name.clone(),
                status: status.clone(),
                timestamp: Utc::now(),
                metrics,
                error: if status != HealthStatus::Healthy {
                    Some(format!("Component {} has issues", name))
                } else {
                    None
                },
            };

            check_results.push(result);

            // Update component health
            self.report_health(
                name,
                status,
                result.metrics,
                result.error.clone(),
            ).await;
        }

        // Emit health summary signal
        let overall_status = self.check_health().await;
        let signal = OSIRISSignal::new(
            "health_check_summary",
            format!("System health check completed: {}", overall_status),
            match overall_status {
                HealthStatus::Healthy => SignalLevel::Info,
                HealthStatus::Warning => SignalLevel::Warning,
                HealthStatus::Critical | HealthStatus::Down => SignalLevel::Critical,
            },
        );

        // Signal would be emitted here in full implementation
        debug!("Health check signal: {}", signal.message);

        Ok(())
    }

    /// Check metrics for a specific component
    async fn check_component_metrics(&self, component: &str) -> Result<HashMap<String, Value>, Box<dyn std::error::Error>> {
        // Simulate component-specific metric checks
        let metrics = match component {
            "engine" => {
                Ok(serde_json::json!({
                    "memory_usage_mb": 512.0,
                    "cpu_usage_percent": 25.5,
                    "active_patterns": 5,
                    "registered_domains": 3
                }))
            }
            "circuit_breaker" => {
                Ok(serde_json::json!({
                    "state": "closed",
                    "failure_count": 0,
                    "success_count": 42,
                    "is_healthy": true
                }))
            }
            "autonomic" => {
                Ok(serde_json::json!({
                    "current_state": "monitoring",
                    "transitions_count": 10,
                    "is_healthy": true
                }))
            }
            _ => {
                Ok(serde_json::json!({
                    "status": "unknown",
                    "last_seen": Utc::now().to_rfc3339()
                }))
            }
        }?;

        Ok(metrics.as_object().unwrap().clone())
    }

    /// Evaluate component status based on metrics
    async fn evaluate_component_status(&self, component: &str, metrics: &HashMap<String, Value>) -> Result<HealthStatus, Box<dyn std::error::Error>> {
        // Simple evaluation logic - would be more sophisticated in real implementation
        match component {
            "engine" => {
                if let Some(memory) = metrics.get("memory_usage_mb").and_then(|m| m.as_f64()) {
                    if memory > 1024.0 {
                        return Ok(HealthStatus::Critical);
                    } else if memory > 768.0 {
                        return Ok(HealthStatus::Warning);
                    }
                }
                Ok(HealthStatus::Healthy)
            }
            "circuit_breaker" => {
                if let Some(state) = metrics.get("state").and_then(|m| m.as_str()) {
                    match state {
                        "open" => Ok(HealthStatus::Critical),
                        "half_open" => Ok(HealthStatus::Warning),
                        "closed" => Ok(HealthStatus::Healthy),
                        _ => Ok(HealthStatus::Warning),
                    }
                } else {
                    Ok(HealthStatus::Warning)
                }
            }
            "autonomic" => {
                if let Some(is_healthy) = metrics.get("is_healthy").and_then(|m| m.as_bool()) {
                    if is_healthy {
                        Ok(HealthStatus::Healthy)
                    } else {
                        Ok(HealthStatus::Critical)
                    }
                } else {
                    Ok(HealthStatus::Warning)
                }
            }
            _ => Ok(HealthStatus::Warning),
        }
    }

    /// Get component health details
    pub async fn get_component_health(&self, component: &str) -> Option<HealthCheckResult> {
        let components = self.components.read().await;

        components.get(component).map(|comp_health| HealthCheckResult {
            component: comp_health.name.clone(),
            status: comp_health.status.clone(),
            timestamp: comp_health.last_check,
            metrics: comp_health.metrics.clone(),
            error: if comp_health.status != HealthStatus::Healthy {
                Some(format!("Component has issues"))
            } else {
                None
            },
        })
    }

    /// Get all component health
    pub async fn get_all_health(&self) -> Vec<HealthCheckResult> {
        let components = self.components.read().await;

        components
            .iter()
            .map(|(name, health)| HealthCheckResult {
                component: name.clone(),
                status: health.status.clone(),
                timestamp: health.last_check,
                metrics: health.metrics.clone(),
                error: if health.status != HealthStatus::Healthy {
                    Some(format!("Component has issues"))
                } else {
                    None
                },
            })
            .collect()
    }

    /// Start background health monitoring
    pub async fn start_monitoring(&self) -> Result<(), Box<dyn std::error::Error>> {
        let monitor = self.clone();

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_millis(monitor.interval_ms));

            loop {
                interval.tick().await;

                if let Err(e) = monitor.run_health_check().await {
                    error!("Health check failed: {}", e);
                }
            }
        });

        Ok(())
    }
}

impl Clone for HealthMonitor {
    fn clone(&self) -> Self {
        Self {
            interval_ms: self.interval_ms,
            components: self.components.clone(),
            last_full_check: self.last_full_check.clone(),
        }
    }
}

impl std::fmt::Debug for HealthMonitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HealthMonitor")
            .field("interval_ms", &self.interval_ms)
            .field("components_count", &"HashMap<String, ComponentHealth>")
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[tokio::test]
    async fn test_health_monitor_creation() {
        let monitor = HealthMonitor::new();
        assert_eq!(monitor.interval_ms(), 30000);
    }

    #[tokio::test]
    async fn test_component_registration() {
        let monitor = HealthMonitor::new();

        monitor.register_component("test-component").await;

        let health = monitor.get_component_health("test-component").await;
        assert!(health.is_some());
        assert_eq!(health.unwrap().component, "test-component");
    }

    #[tokio::test]
    async fn test_health_reporting() {
        let monitor = HealthMonitor::new();

        monitor.register_component("test-component").await;

        let metrics = HashMap::new();
        monitor.report_health("test-component", HealthStatus::Healthy, metrics, None).await;

        let health = monitor.get_component_health("test-component").await;
        assert!(health.is_some());
        assert_eq!(health.unwrap().status, HealthStatus::Healthy);
    }

    #[tokio::test]
    async fn test_system_health_check() {
        let monitor = HealthMonitor::new();

        monitor.register_component("component1").await;
        monitor.register_component("component2").await;

        // Set one component to critical
        let metrics = HashMap::new();
        monitor.report_health("component1", HealthStatus::Critical, metrics, None).await;

        let status = monitor.check_health().await;
        assert_eq!(status, HealthStatus::Critical);
    }

    #[tokio::test]
    async fn test_all_health_check() {
        let monitor = HealthMonitor::new();

        monitor.register_component("component1").await;
        monitor.register_component("component2").await;

        let all_health = monitor.get_all_health().await;
        assert_eq!(all_health.len(), 2);
    }
}