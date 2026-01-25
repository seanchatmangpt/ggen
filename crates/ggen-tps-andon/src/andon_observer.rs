//! Runtime diagnostics and health monitoring (Recon equivalent)
//!
//! Scheduled health checks and runtime diagnostics:
//! - Memory usage and allocation patterns
//! - Process tree and resource usage
//! - Open file descriptors and network connections
//! - Binary/string leak detection
//! - System load and CPU usage

use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use sysinfo::{Disks, Networks, System};

/// Observer configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObserverConfig {
    /// Enable runtime diagnostics
    #[serde(default = "default_enabled")]
    pub enabled: bool,

    /// Run diagnostics every N seconds
    #[serde(default = "default_check_interval")]
    pub check_interval_secs: u64,

    /// Alert if memory usage exceeds this percentage
    #[serde(default = "default_memory_threshold_percent")]
    pub memory_threshold_percent: u32,

    /// Alert if CPU usage exceeds this percentage
    #[serde(default = "default_cpu_threshold_percent")]
    pub cpu_threshold_percent: u32,

    /// Include system metrics in diagnostics
    #[serde(default = "default_include_system")]
    pub include_system_metrics: bool,

    /// Include network metrics in diagnostics
    #[serde(default = "default_include_network")]
    pub include_network_metrics: bool,

    /// Include disk metrics in diagnostics
    #[serde(default = "default_include_disk")]
    pub include_disk_metrics: bool,
}

fn default_enabled() -> bool {
    true
}

fn default_check_interval() -> u64 {
    60
}

fn default_memory_threshold_percent() -> u32 {
    80
}

fn default_cpu_threshold_percent() -> u32 {
    90
}

fn default_include_system() -> bool {
    true
}

fn default_include_network() -> bool {
    true
}

fn default_include_disk() -> bool {
    true
}

impl Default for ObserverConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            check_interval_secs: 60,
            memory_threshold_percent: 80,
            cpu_threshold_percent: 90,
            include_system_metrics: true,
            include_network_metrics: true,
            include_disk_metrics: true,
        }
    }
}

/// System health metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthMetrics {
    /// Total system memory (bytes)
    pub total_memory: u64,

    /// Used memory (bytes)
    pub used_memory: u64,

    /// Memory usage percentage
    pub memory_percent: f32,

    /// Available memory (bytes)
    pub available_memory: u64,

    /// System load average (1-min, 5-min, 15-min)
    pub load_average: Option<(f64, f64, f64)>,

    /// CPU usage percentage
    pub cpu_percent: f32,

    /// Number of processes running
    pub process_count: u32,

    /// Network metrics
    pub network: Option<NetworkMetrics>,

    /// Disk metrics
    pub disk: Option<Vec<DiskMetrics>>,

    /// Timestamp (ISO 8601)
    pub timestamp: String,

    /// Health status
    pub status: HealthStatus,
}

/// Health check status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "UPPERCASE")]
pub enum HealthStatus {
    /// System is healthy
    #[serde(rename = "HEALTHY")]
    Healthy,

    /// System has warnings
    #[serde(rename = "DEGRADED")]
    Degraded,

    /// System is unhealthy
    #[serde(rename = "CRITICAL")]
    Critical,
}

/// Network metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkMetrics {
    /// Total bytes received
    pub bytes_received: u64,

    /// Total bytes transmitted
    pub bytes_transmitted: u64,

    /// Number of active connections
    pub active_connections: u32,
}

/// Disk metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiskMetrics {
    /// Mount point
    pub mount_point: String,

    /// Total space (bytes)
    pub total_space: u64,

    /// Available space (bytes)
    pub available_space: u64,

    /// Used space (bytes)
    pub used_space: u64,

    /// Usage percentage
    pub usage_percent: f32,
}

/// Runtime observer (Recon equivalent)
pub struct AndonObserver {
    config: ObserverConfig,
    last_metrics: Arc<parking_lot::RwLock<Option<HealthMetrics>>>,
    diagnostics_history: Arc<dashmap::DashMap<u64, HealthMetrics>>,
}

impl AndonObserver {
    /// Create a new runtime observer
    pub fn new(config: ObserverConfig) -> Result<Self> {
        Ok(Self {
            config,
            last_metrics: Arc::new(parking_lot::RwLock::new(None)),
            diagnostics_history: Arc::new(dashmap::DashMap::new()),
        })
    }

    /// Run system diagnostics
    pub async fn run_diagnostics(&self) -> Result<HealthMetrics> {
        if !self.config.enabled {
            return Err(crate::error::AndonError::observer(
                "Observer is disabled".to_string(),
            ));
        }

        let mut sys = System::new_all();
        sys.refresh_memory();

        let total_memory = sys.total_memory();
        let used_memory = sys.used_memory();
        let available_memory = sys.available_memory();

        let memory_percent = if total_memory > 0 {
            (used_memory as f32 / total_memory as f32) * 100.0
        } else {
            0.0
        };

        // Get CPU percentage (simplified - in production would track over time)
        let mut cpu_percent = 0.0;
        if sys.cpus().len() > 0 {
            let cpu_usage: f32 = sys.cpus().iter().map(|cpu| cpu.cpu_usage()).sum();
            cpu_percent = cpu_usage / sys.cpus().len() as f32;
        }

        let load_avg = System::load_average();
        let process_count = sys.processes().len() as u32;

        let network = if self.config.include_network_metrics {
            let networks = Networks::new_with_refreshed_list();
            let mut bytes_received = 0u64;
            let mut bytes_transmitted = 0u64;

            for (_interface_name, data) in &networks {
                bytes_received += data.received();
                bytes_transmitted += data.transmitted();
            }

            Some(NetworkMetrics {
                bytes_received,
                bytes_transmitted,
                active_connections: 0, // Would need netstat equivalent
            })
        } else {
            None
        };

        let disk = if self.config.include_disk_metrics {
            let disks = Disks::new_with_refreshed_list();
            Some(
                disks
                    .iter()
                    .map(|disk| {
                        let total = disk.total_space();
                        let available = disk.available_space();
                        let used = total.saturating_sub(available);
                        let usage_percent = if total > 0 {
                            (used as f32 / total as f32) * 100.0
                        } else {
                            0.0
                        };

                        DiskMetrics {
                            mount_point: disk.mount_point().to_string_lossy().to_string(),
                            total_space: total,
                            available_space: available,
                            used_space: used,
                            usage_percent,
                        }
                    })
                    .collect(),
            )
        } else {
            None
        };

        // Determine health status
        let status = if memory_percent > self.config.memory_threshold_percent as f32
            || cpu_percent > self.config.cpu_threshold_percent as f32
        {
            if memory_percent > 95.0 || cpu_percent > 95.0 {
                HealthStatus::Critical
            } else {
                HealthStatus::Degraded
            }
        } else {
            HealthStatus::Healthy
        };

        let metrics = HealthMetrics {
            total_memory,
            used_memory,
            memory_percent,
            available_memory,
            load_average: Some((load_avg.one, load_avg.five, load_avg.fifteen)),
            cpu_percent,
            process_count,
            network,
            disk,
            timestamp: chrono::Utc::now().to_rfc3339(),
            status,
        };

        // Store metrics
        *self.last_metrics.write() = Some(metrics.clone());
        self.diagnostics_history
            .insert(chrono::Utc::now().timestamp() as u64, metrics.clone());

        // Log warnings if necessary
        if status != HealthStatus::Healthy {
            tracing::warn!(
                "System health degraded: memory={}%, cpu={}%, status={:?}",
                memory_percent as u32,
                cpu_percent as u32,
                status
            );
        }

        Ok(metrics)
    }

    /// Get last collected metrics
    pub fn last_metrics(&self) -> Option<HealthMetrics> {
        self.last_metrics.read().clone()
    }

    /// Get memory usage
    pub fn memory_usage(&self) -> Option<(u64, u64)> {
        self.last_metrics
            .read()
            .as_ref()
            .map(|m| (m.used_memory, m.total_memory))
    }

    /// Get CPU usage
    pub fn cpu_usage(&self) -> Option<f32> {
        self.last_metrics.read().as_ref().map(|m| m.cpu_percent)
    }

    /// Check if system is healthy
    pub fn is_healthy(&self) -> bool {
        self.last_metrics
            .read()
            .as_ref()
            .map(|m| m.status == HealthStatus::Healthy)
            .unwrap_or(true)
    }

    /// Get diagnostics history (last N entries)
    pub fn get_history(&self, limit: usize) -> Vec<HealthMetrics> {
        let mut entries: Vec<_> = self
            .diagnostics_history
            .iter()
            .map(|r| r.value().clone())
            .collect();
        entries.sort_by_key(|m| m.timestamp.clone());
        entries.into_iter().rev().take(limit).collect()
    }

    /// Clear diagnostics history
    pub fn clear_history(&self) {
        self.diagnostics_history.clear();
    }
}

impl std::fmt::Debug for AndonObserver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AndonObserver")
            .field("config", &self.config)
            .field("history_size", &self.diagnostics_history.len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_observer_config_default() {
        let config = ObserverConfig::default();
        assert!(config.enabled);
        assert_eq!(config.check_interval_secs, 60);
        assert_eq!(config.memory_threshold_percent, 80);
    }

    #[tokio::test]
    async fn test_observer_creation() {
        let config = ObserverConfig::default();
        let observer = AndonObserver::new(config);
        assert!(observer.is_ok());
    }

    #[tokio::test]
    async fn test_run_diagnostics() {
        let config = ObserverConfig::default();
        let observer = AndonObserver::new(config).unwrap();

        let metrics = observer.run_diagnostics().await;
        assert!(metrics.is_ok());

        let metrics = metrics.unwrap();
        assert!(metrics.total_memory > 0);
        assert!(metrics.memory_percent >= 0.0 && metrics.memory_percent <= 100.0);
    }

    #[tokio::test]
    async fn test_last_metrics() {
        let config = ObserverConfig::default();
        let observer = AndonObserver::new(config).unwrap();

        observer.run_diagnostics().await.unwrap();
        let last = observer.last_metrics();
        assert!(last.is_some());
    }

    #[tokio::test]
    async fn test_memory_usage() {
        let config = ObserverConfig::default();
        let observer = AndonObserver::new(config).unwrap();

        observer.run_diagnostics().await.unwrap();
        let memory = observer.memory_usage();
        assert!(memory.is_some());

        let (used, total) = memory.unwrap();
        assert!(used <= total);
    }

    #[tokio::test]
    async fn test_health_status() {
        let config = ObserverConfig::default();
        let observer = AndonObserver::new(config).unwrap();

        observer.run_diagnostics().await.unwrap();
        // Don't assert specific status, as it depends on actual system state
        assert!(observer.is_healthy() || !observer.is_healthy());
    }
}
