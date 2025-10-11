//! Metrics collection and management

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::debug;

use super::super::error::{GovernanceError, Result};
use super::visualization::TimescaleMetrics;

/// Health status of the governance system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthStatus {
    pub overall_status: SystemStatus,
    pub emergency_stop_active: bool,
    pub policy_engine_status: ComponentStatus,
    pub audit_trail_status: ComponentStatus,
    pub safety_controller_status: ComponentStatus,
    pub workflow_status: ComponentStatus,
    pub last_check: DateTime<Utc>,
    pub uptime_seconds: u64,
    pub issues: Vec<HealthIssue>,
}

/// System status levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum SystemStatus {
    Healthy,
    Degraded,
    Critical,
    Down,
}

/// Component status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentStatus {
    pub name: String,
    pub status: SystemStatus,
    pub last_activity: DateTime<Utc>,
    pub metrics: HashMap<String, f64>,
    pub errors: Vec<String>,
}

/// Health issue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthIssue {
    pub severity: IssueSeverity,
    pub component: String,
    pub message: String,
    pub detected_at: DateTime<Utc>,
}

/// Issue severity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
#[serde(rename_all = "lowercase")]
pub enum IssueSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

/// Metrics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsSnapshot {
    pub timestamp: DateTime<Utc>,
    pub decisions_processed: u64,
    pub decisions_approved: u64,
    pub decisions_rejected: u64,
    pub decisions_pending: u64,
    pub policy_violations: u64,
    pub safety_violations: u64,
    pub emergency_stops: u64,
    pub rollbacks: u64,
    pub average_processing_time_ms: f64,
    pub approval_rate: f64,
    pub rejection_rate: f64,
    pub timescale_metrics: TimescaleMetrics,
    pub resource_usage: ResourceUsage,
}

/// Resource utilization metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsage {
    pub memory_mb: f64,
    pub cpu_percent: f64,
    pub disk_mb: f64,
    pub network_kb_per_sec: f64,
}

/// Dashboard configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DashboardConfig {
    pub refresh_interval_seconds: u64,
    pub metrics_retention_hours: u64,
    pub enable_real_time_updates: bool,
}

impl Default for DashboardConfig {
    fn default() -> Self {
        Self {
            refresh_interval_seconds: 5,
            metrics_retention_hours: 168, // 7 days
            enable_real_time_updates: true,
        }
    }
}

/// Metrics collector and dashboard
pub struct Dashboard {
    config: DashboardConfig,
    current_metrics: Arc<RwLock<MetricsSnapshot>>,
    health_status: Arc<RwLock<HealthStatus>>,
    start_time: DateTime<Utc>,
    emergency_stop_active: Arc<RwLock<bool>>,
}

impl Dashboard {
    /// Create a new dashboard
    pub fn new(config: DashboardConfig) -> Self {
        let now = Utc::now();

        let initial_metrics = MetricsSnapshot {
            timestamp: now,
            decisions_processed: 0,
            decisions_approved: 0,
            decisions_rejected: 0,
            decisions_pending: 0,
            policy_violations: 0,
            safety_violations: 0,
            emergency_stops: 0,
            rollbacks: 0,
            average_processing_time_ms: 0.0,
            approval_rate: 0.0,
            rejection_rate: 0.0,
            timescale_metrics: TimescaleMetrics {
                hourly_decisions: Vec::new(),
                hourly_violations: Vec::new(),
                success_rate_trend: Vec::new(),
            },
            resource_usage: ResourceUsage {
                memory_mb: 0.0,
                cpu_percent: 0.0,
                disk_mb: 0.0,
                network_kb_per_sec: 0.0,
            },
        };

        let initial_health = HealthStatus {
            overall_status: SystemStatus::Healthy,
            emergency_stop_active: false,
            policy_engine_status: ComponentStatus {
                name: "policy_engine".to_string(),
                status: SystemStatus::Healthy,
                last_activity: now,
                metrics: HashMap::new(),
                errors: Vec::new(),
            },
            audit_trail_status: ComponentStatus {
                name: "audit_trail".to_string(),
                status: SystemStatus::Healthy,
                last_activity: now,
                metrics: HashMap::new(),
                errors: Vec::new(),
            },
            safety_controller_status: ComponentStatus {
                name: "safety_controller".to_string(),
                status: SystemStatus::Healthy,
                last_activity: now,
                metrics: HashMap::new(),
                errors: Vec::new(),
            },
            workflow_status: ComponentStatus {
                name: "workflow".to_string(),
                status: SystemStatus::Healthy,
                last_activity: now,
                metrics: HashMap::new(),
                errors: Vec::new(),
            },
            last_check: now,
            uptime_seconds: 0,
            issues: Vec::new(),
        };

        Self {
            config,
            current_metrics: Arc::new(RwLock::new(initial_metrics)),
            health_status: Arc::new(RwLock::new(initial_health)),
            start_time: now,
            emergency_stop_active: Arc::new(RwLock::new(false)),
        }
    }

    /// Get current health status
    pub async fn get_health_status(&self) -> Result<HealthStatus> {
        let mut health = self.health_status.write().await;

        // Update uptime
        health.uptime_seconds = (Utc::now() - self.start_time).num_seconds() as u64;
        health.last_check = Utc::now();

        // Check emergency stop status
        let emergency_stop = *self.emergency_stop_active.read().await;
        health.emergency_stop_active = emergency_stop;

        if emergency_stop {
            health.overall_status = SystemStatus::Critical;
            health.issues.push(HealthIssue {
                severity: IssueSeverity::Critical,
                component: "safety_controller".to_string(),
                message: "Emergency stop is active".to_string(),
                detected_at: Utc::now(),
            });
        } else {
            // Calculate overall status based on component statuses
            let statuses = vec![
                &health.policy_engine_status.status,
                &health.audit_trail_status.status,
                &health.safety_controller_status.status,
                &health.workflow_status.status,
            ];

            health.overall_status = if statuses.iter().any(|s| **s == SystemStatus::Down) {
                SystemStatus::Down
            } else if statuses.iter().any(|s| **s == SystemStatus::Critical) {
                SystemStatus::Critical
            } else if statuses.iter().any(|s| **s == SystemStatus::Degraded) {
                SystemStatus::Degraded
            } else {
                SystemStatus::Healthy
            };
        }

        Ok(health.clone())
    }

    /// Get current metrics snapshot
    pub async fn get_metrics_snapshot(&self) -> Result<MetricsSnapshot> {
        let mut metrics = self.current_metrics.write().await;
        metrics.timestamp = Utc::now();

        // Update resource usage
        metrics.resource_usage = self.collect_resource_usage().await?;

        // Calculate rates
        if metrics.decisions_processed > 0 {
            metrics.approval_rate =
                metrics.decisions_approved as f64 / metrics.decisions_processed as f64;
            metrics.rejection_rate =
                metrics.decisions_rejected as f64 / metrics.decisions_processed as f64;
        }

        Ok(metrics.clone())
    }

    /// Update metrics with decision outcome
    pub async fn record_decision(&self, approved: bool, processing_time_ms: f64) -> Result<()> {
        let mut metrics = self.current_metrics.write().await;

        metrics.decisions_processed += 1;
        if approved {
            metrics.decisions_approved += 1;
        } else {
            metrics.decisions_rejected += 1;
        }

        // Update moving average of processing time
        let n = metrics.decisions_processed as f64;
        metrics.average_processing_time_ms =
            (metrics.average_processing_time_ms * (n - 1.0) + processing_time_ms) / n;

        Ok(())
    }

    /// Record policy violation
    pub async fn record_violation(&self) -> Result<()> {
        let mut metrics = self.current_metrics.write().await;
        metrics.policy_violations += 1;
        Ok(())
    }

    /// Record safety violation
    pub async fn record_safety_violation(&self) -> Result<()> {
        let mut metrics = self.current_metrics.write().await;
        metrics.safety_violations += 1;
        Ok(())
    }

    /// Update emergency stop status
    pub async fn update_emergency_status(&self, active: bool) -> Result<()> {
        let mut emergency_stop = self.emergency_stop_active.write().await;
        *emergency_stop = active;

        if active {
            let mut metrics = self.current_metrics.write().await;
            metrics.emergency_stops += 1;
        }

        Ok(())
    }

    /// Record rollback
    pub async fn record_rollback(&self) -> Result<()> {
        let mut metrics = self.current_metrics.write().await;
        metrics.rollbacks += 1;
        Ok(())
    }

    /// Collect current resource usage
    async fn collect_resource_usage(&self) -> Result<ResourceUsage> {
        debug!("Collecting system resource usage");

        // Memory usage
        let memory_mb = self.get_memory_usage().await?;

        // CPU usage
        let cpu_percent = self.get_cpu_usage().await?;

        // Disk usage
        let disk_mb = self.get_disk_usage().await?;

        // Network usage
        let network_kb_per_sec = self.get_network_usage().await?;

        Ok(ResourceUsage {
            memory_mb,
            cpu_percent,
            disk_mb,
            network_kb_per_sec,
        })
    }

    /// Get memory usage in MB
    async fn get_memory_usage(&self) -> Result<f64> {
        let process = std::process::id();
        debug!("Getting memory usage for process {}", process);

        // Mock calculation
        let memory_mb = 128.0 + (process as f64 % 100.0);
        Ok(memory_mb)
    }

    /// Get CPU usage percentage
    async fn get_cpu_usage(&self) -> Result<f64> {
        let start = std::time::Instant::now();

        // Simulate some CPU work
        let mut counter = 0;
        while start.elapsed().as_millis() < 10 {
            counter += 1;
        }

        // Mock CPU usage
        let cpu_percent = 5.0 + (counter as f64 % 20.0);
        Ok(cpu_percent)
    }

    /// Get disk usage in MB
    async fn get_disk_usage(&self) -> Result<f64> {
        let current_dir = std::env::current_dir().map_err(GovernanceError::IoError)?;

        let mut total_size = 0u64;

        // Calculate size of current directory
        if let Ok(entries) = std::fs::read_dir(&current_dir) {
            for entry in entries.flatten() {
                if let Ok(metadata) = entry.metadata() {
                    if metadata.is_file() {
                        total_size += metadata.len();
                    }
                }
            }
        }

        let disk_mb = total_size as f64 / (1024.0 * 1024.0);
        Ok(disk_mb)
    }

    /// Get network usage in KB/s
    async fn get_network_usage(&self) -> Result<f64> {
        let start = std::time::Instant::now();

        // Simulate network activity check
        tokio::time::sleep(std::time::Duration::from_millis(1)).await;

        let elapsed_ms = start.elapsed().as_millis() as f64;
        let network_kb_per_sec = 10.0 + (elapsed_ms % 50.0);

        Ok(network_kb_per_sec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_dashboard_creation() {
        let dashboard = Dashboard::new(DashboardConfig::default());
        let health = dashboard.get_health_status().await.unwrap();
        assert_eq!(health.overall_status, SystemStatus::Healthy);
    }

    #[tokio::test]
    async fn test_metrics_recording() {
        let dashboard = Dashboard::new(DashboardConfig::default());

        dashboard.record_decision(true, 100.0).await.unwrap();
        dashboard.record_decision(false, 150.0).await.unwrap();

        let metrics = dashboard.get_metrics_snapshot().await.unwrap();
        assert_eq!(metrics.decisions_processed, 2);
        assert_eq!(metrics.decisions_approved, 1);
        assert_eq!(metrics.decisions_rejected, 1);
        assert_eq!(metrics.approval_rate, 0.5);
    }

    #[tokio::test]
    async fn test_emergency_stop_status() {
        let dashboard = Dashboard::new(DashboardConfig::default());

        dashboard.update_emergency_status(true).await.unwrap();
        let health = dashboard.get_health_status().await.unwrap();

        assert!(health.emergency_stop_active);
        assert_eq!(health.overall_status, SystemStatus::Critical);
    }
}
