//! Marketplace observability, metrics collection, and monitoring
//!
//! Provides comprehensive monitoring capabilities for marketplace operations,
//! including performance metrics, error tracking, and health checks.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, SystemTime};

/// System health status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum HealthStatus {
    /// All systems operational
    Healthy,
    /// Degraded performance or warnings
    Degraded,
    /// Critical issues detected
    Unhealthy,
}

/// Performance metric for tracking operation duration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetric {
    /// Operation name
    pub operation: String,

    /// Duration of operation in milliseconds
    pub duration_ms: u64,

    /// Whether operation succeeded
    pub success: bool,

    /// Timestamp
    pub timestamp: String,
}

/// Health check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheck {
    /// Component being checked
    pub component: String,

    /// Current health status
    pub status: HealthStatus,

    /// Latency in milliseconds
    pub latency_ms: u64,

    /// Additional details
    pub details: String,

    /// Last checked
    pub last_checked: String,
}

/// Marketplace metrics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsSnapshot {
    /// Total packages
    pub total_packages: usize,

    /// Production-ready packages
    pub production_ready_count: usize,

    /// Average package score
    pub average_score: f64,

    /// Receipts generated
    pub receipts_generated: usize,

    /// Last receipt timestamp
    pub last_receipt_time: Option<String>,

    /// API response times (ms)
    pub response_times: HashMap<String, u64>,

    /// Error counts by type
    pub error_counts: HashMap<String, usize>,

    /// Uptime percentage
    pub uptime_percent: f64,
}

/// Observability system for marketplace
pub struct ObservabilitySystem {
    metrics: Vec<PerformanceMetric>,
    health_checks: Vec<HealthCheck>,
    start_time: SystemTime,
}

impl ObservabilitySystem {
    /// Create new observability system
    pub fn new() -> Self {
        Self {
            metrics: Vec::new(),
            health_checks: Vec::new(),
            start_time: SystemTime::now(),
        }
    }

    /// Record a performance metric
    pub fn record_metric(&mut self, operation: String, duration: Duration, success: bool) {
        let metric = PerformanceMetric {
            operation,
            duration_ms: duration.as_millis() as u64,
            success,
            timestamp: chrono::Utc::now().to_rfc3339(),
        };
        self.metrics.push(metric);
    }

    /// Get average response time for operation
    pub fn avg_response_time(&self, operation: &str) -> Option<f64> {
        let ops: Vec<_> = self
            .metrics
            .iter()
            .filter(|m| m.operation == operation && m.success)
            .collect();

        if ops.is_empty() {
            None
        } else {
            let total: u64 = ops.iter().map(|m| m.duration_ms).sum();
            Some(total as f64 / ops.len() as f64)
        }
    }

    /// Get success rate for operation
    pub fn success_rate(&self, operation: &str) -> Option<f64> {
        let ops: Vec<_> = self
            .metrics
            .iter()
            .filter(|m| m.operation == operation)
            .collect();

        if ops.is_empty() {
            None
        } else {
            let successful = ops.iter().filter(|m| m.success).count();
            Some(successful as f64 / ops.len() as f64 * 100.0)
        }
    }

    /// Record health check result
    pub fn record_health_check(
        &mut self, component: String, status: HealthStatus, latency: Duration, details: String,
    ) {
        let check = HealthCheck {
            component,
            status,
            latency_ms: latency.as_millis() as u64,
            details,
            last_checked: chrono::Utc::now().to_rfc3339(),
        };
        self.health_checks.push(check);
    }

    /// Get overall system health
    pub fn overall_health(&self) -> HealthStatus {
        if self.health_checks.is_empty() {
            return HealthStatus::Healthy;
        }

        if self
            .health_checks
            .iter()
            .any(|c| c.status == HealthStatus::Unhealthy)
        {
            HealthStatus::Unhealthy
        } else if self
            .health_checks
            .iter()
            .any(|c| c.status == HealthStatus::Degraded)
        {
            HealthStatus::Degraded
        } else {
            HealthStatus::Healthy
        }
    }

    /// Get uptime percentage
    pub fn uptime_percent(&self) -> f64 {
        match self.start_time.elapsed() {
            Ok(_elapsed) => {
                let total_ops = self.metrics.len() as f64;
                if total_ops == 0.0 {
                    100.0
                } else {
                    let successful = self.metrics.iter().filter(|m| m.success).count() as f64;
                    (successful / total_ops) * 100.0
                }
            }
            Err(_) => 0.0,
        }
    }

    /// Generate metrics report
    pub fn generate_report(&self) -> HashMap<String, serde_json::Value> {
        let mut report = HashMap::new();

        // Overall metrics
        report.insert(
            "total_operations".to_string(),
            serde_json::json!(self.metrics.len()),
        );

        report.insert(
            "uptime_percent".to_string(),
            serde_json::json!(self.uptime_percent()),
        );

        report.insert(
            "system_health".to_string(),
            serde_json::json!(format!("{:?}", self.overall_health())),
        );

        // Per-operation metrics
        let mut operation_metrics = HashMap::new();
        let operations: std::collections::HashSet<_> =
            self.metrics.iter().map(|m| m.operation.clone()).collect();

        for op in operations {
            if let Some(avg_time) = self.avg_response_time(&op) {
                if let Some(success_rate) = self.success_rate(&op) {
                    operation_metrics.insert(
                        op,
                        serde_json::json!({
                            "avg_response_time_ms": avg_time,
                            "success_rate_percent": success_rate
                        }),
                    );
                }
            }
        }

        report.insert(
            "operations".to_string(),
            serde_json::json!(operation_metrics),
        );

        report
    }
}

impl Default for ObservabilitySystem {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_metric_recording() {
        let mut obs = ObservabilitySystem::new();
        obs.record_metric("test_op".to_string(), Duration::from_millis(100), true);

        assert_eq!(obs.metrics.len(), 1);
        assert_eq!(obs.metrics[0].duration_ms, 100);
        assert!(obs.metrics[0].success);
    }

    #[test]
    fn test_average_response_time() {
        let mut obs = ObservabilitySystem::new();
        obs.record_metric("op1".to_string(), Duration::from_millis(100), true);
        obs.record_metric("op1".to_string(), Duration::from_millis(200), true);
        obs.record_metric("op1".to_string(), Duration::from_millis(300), true);

        let avg = obs.avg_response_time("op1");
        assert!(avg.is_some());
        assert_eq!(avg.unwrap(), 200.0);
    }

    #[test]
    fn test_success_rate() {
        let mut obs = ObservabilitySystem::new();
        obs.record_metric("op2".to_string(), Duration::from_millis(100), true);
        obs.record_metric("op2".to_string(), Duration::from_millis(100), true);
        obs.record_metric("op2".to_string(), Duration::from_millis(100), false);

        let rate = obs.success_rate("op2");
        assert!(rate.is_some());
        assert_eq!(rate.unwrap(), 66.66666666666666);
    }

    #[test]
    fn test_health_checks() {
        let mut obs = ObservabilitySystem::new();
        obs.record_health_check(
            "api".to_string(),
            HealthStatus::Healthy,
            Duration::from_millis(10),
            "All healthy".to_string(),
        );

        assert_eq!(obs.overall_health(), HealthStatus::Healthy);
    }
}
