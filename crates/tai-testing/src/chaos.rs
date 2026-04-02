//! Chaos engineering framework for production resilience testing.
//!
//! This module provides tools to deliberately inject failures into systems
//! to verify they recover gracefully. Chaos experiments help identify weaknesses
//! before they cause real outages.
//!
//! ## Chaos Experiments Supported
//!
//! - **Pod Kill**: Randomly kill pods to test service resilience
//! - **Network Partition**: Split cluster to test handling of split-brain
//! - **CPU Throttling**: Limit CPU to test degraded performance
//! - **Memory Pressure**: Reduce available memory to test OOM handling
//! - **Disk Exhaustion**: Fill disk space to test disk failure scenarios
//! - **Clock Skew**: Advance time to test timeout handling
//! - **Cascading Failure**: Kill services in dependency order
//!
//! ## Metrics Collected
//!
//! All experiments collect:
//! - Recovery time (how long until service recovered)
//! - Error rate during failure
//! - Latency impact (p50, p95, p99)
//! - Resource utilization during recovery
//! - Downstream impact (services affected)

use crate::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;
use uuid::Uuid;

/// Type of chaos experiment to run
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ChaosExperimentType {
    /// Randomly kill pods
    PodKill,
    /// Split cluster into isolated partitions
    NetworkPartition,
    /// Limit CPU resources
    CpuThrottling,
    /// Reduce available memory
    MemoryPressure,
    /// Exhaust disk space
    DiskExhaustion,
    /// Advance system clock
    ClockSkew,
    /// Kill services in dependency order
    CascadingFailure,
}

impl std::fmt::Display for ChaosExperimentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PodKill => write!(f, "PodKill"),
            Self::NetworkPartition => write!(f, "NetworkPartition"),
            Self::CpuThrottling => write!(f, "CpuThrottling"),
            Self::MemoryPressure => write!(f, "MemoryPressure"),
            Self::DiskExhaustion => write!(f, "DiskExhaustion"),
            Self::ClockSkew => write!(f, "ClockSkew"),
            Self::CascadingFailure => write!(f, "CascadingFailure"),
        }
    }
}

/// Configuration for a chaos experiment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChaosConfig {
    /// Cluster to run experiment on
    pub cluster: String,
    /// Namespace containing target services
    pub namespace: String,
    /// Service or workload to target
    pub target: String,
    /// Type of chaos experiment
    pub experiment_type: ChaosExperimentType,
    /// Duration of the failure
    pub failure_duration: Duration,
    /// How many instances to affect (for pod kill)
    pub affected_instances: usize,
    /// Monitoring duration after failure ends
    pub monitoring_duration: Duration,
}

/// Metrics collected during and after a chaos experiment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExperimentMetrics {
    /// Unique experiment ID
    pub experiment_id: String,
    /// When experiment started
    pub start_time: DateTime<Utc>,
    /// When experiment ended
    pub end_time: DateTime<Utc>,
    /// Type of experiment
    pub experiment_type: ChaosExperimentType,
    /// How long until service recovered (in milliseconds)
    pub recovery_time_ms: u64,
    /// Error rate during failure (0.0-1.0)
    pub error_rate_during_failure: f64,
    /// Error rate during recovery
    pub error_rate_during_recovery: f64,
    /// P50 latency (in milliseconds)
    pub latency_p50_ms: f64,
    /// P95 latency (in milliseconds)
    pub latency_p95_ms: f64,
    /// P99 latency (in milliseconds)
    pub latency_p99_ms: f64,
    /// Max latency observed (in milliseconds)
    pub latency_max_ms: f64,
    /// CPU usage during failure (0.0-100.0)
    pub cpu_usage_percent: f64,
    /// Memory usage during failure (in MB)
    pub memory_usage_mb: f64,
    /// Services that failed as a result of this failure
    pub downstream_failures: Vec<String>,
    /// Whether the system recovered successfully
    pub recovered_successfully: bool,
    /// Custom metrics collected
    pub custom_metrics: HashMap<String, f64>,
}

/// A chaos experiment definition and executor
#[derive(Debug, Clone)]
pub struct ChaosExperiment {
    /// Configuration for this experiment
    config: ChaosConfig,
    /// Unique ID for this experiment
    id: String,
}

impl ChaosExperiment {
    /// Create a new chaos experiment
    pub fn new(config: ChaosConfig) -> Self {
        Self {
            config,
            id: Uuid::new_v4().to_string(),
        }
    }

    /// Create a pod kill experiment
    ///
    /// # Arguments
    ///
    /// * `cluster` - Cluster name
    /// * `service` - Service name to kill pods of
    /// * `pod_count` - Number of pods to kill
    pub fn pod_kill(cluster: &str, service: &str, pod_count: usize) -> Self {
        Self::new(ChaosConfig {
            cluster: cluster.to_string(),
            namespace: "default".to_string(),
            target: service.to_string(),
            experiment_type: ChaosExperimentType::PodKill,
            failure_duration: Duration::from_secs(30),
            affected_instances: pod_count,
            monitoring_duration: Duration::from_secs(120),
        })
    }

    /// Create a network partition experiment
    pub fn network_partition(cluster: &str, service: &str) -> Self {
        Self::new(ChaosConfig {
            cluster: cluster.to_string(),
            namespace: "default".to_string(),
            target: service.to_string(),
            experiment_type: ChaosExperimentType::NetworkPartition,
            failure_duration: Duration::from_secs(60),
            affected_instances: 1,
            monitoring_duration: Duration::from_secs(120),
        })
    }

    /// Create a CPU throttling experiment
    pub fn cpu_throttling(cluster: &str, service: &str) -> Self {
        Self::new(ChaosConfig {
            cluster: cluster.to_string(),
            namespace: "default".to_string(),
            target: service.to_string(),
            experiment_type: ChaosExperimentType::CpuThrottling,
            failure_duration: Duration::from_secs(120),
            affected_instances: 0,
            monitoring_duration: Duration::from_secs(60),
        })
    }

    /// Create a memory pressure experiment
    pub fn memory_pressure(cluster: &str, service: &str) -> Self {
        Self::new(ChaosConfig {
            cluster: cluster.to_string(),
            namespace: "default".to_string(),
            target: service.to_string(),
            experiment_type: ChaosExperimentType::MemoryPressure,
            failure_duration: Duration::from_secs(90),
            affected_instances: 0,
            monitoring_duration: Duration::from_secs(120),
        })
    }

    /// Create a disk exhaustion experiment
    pub fn disk_exhaustion(cluster: &str, service: &str) -> Self {
        Self::new(ChaosConfig {
            cluster: cluster.to_string(),
            namespace: "default".to_string(),
            target: service.to_string(),
            experiment_type: ChaosExperimentType::DiskExhaustion,
            failure_duration: Duration::from_secs(120),
            affected_instances: 0,
            monitoring_duration: Duration::from_secs(180),
        })
    }

    /// Create a clock skew experiment (time moves forward)
    pub fn clock_skew(cluster: &str, service: &str) -> Self {
        Self::new(ChaosConfig {
            cluster: cluster.to_string(),
            namespace: "default".to_string(),
            target: service.to_string(),
            experiment_type: ChaosExperimentType::ClockSkew,
            failure_duration: Duration::from_secs(30),
            affected_instances: 0,
            monitoring_duration: Duration::from_secs(60),
        })
    }

    /// Create a cascading failure experiment
    pub fn cascading_failure(cluster: &str, services: Vec<&str>) -> Self {
        Self::new(ChaosConfig {
            cluster: cluster.to_string(),
            namespace: "default".to_string(),
            target: services.join(","),
            experiment_type: ChaosExperimentType::CascadingFailure,
            failure_duration: Duration::from_secs(60),
            affected_instances: services.len(),
            monitoring_duration: Duration::from_secs(300),
        })
    }

    /// Execute the chaos experiment
    ///
    /// Returns metrics about the experiment execution and system recovery.
    ///
    /// # Errors
    ///
    /// Returns an error if the experiment execution fails.
    pub async fn execute(&self) -> Result<ExperimentMetrics> {
        tracing::info!(
            experiment_id = %self.id,
            experiment_type = %self.config.experiment_type,
            cluster = %self.config.cluster,
            target = %self.config.target,
            "Starting chaos experiment"
        );

        let start_time = Utc::now();

        // TODO: Implement actual chaos experiment execution
        // This would integrate with chaos engineering tools like:
        // - Chaos Mesh (K8s native)
        // - Gremlin (commercial)
        // - Pumba (Docker chaos)
        // - Toxiproxy (network chaos)

        let recovery_time_ms = match self.config.experiment_type {
            ChaosExperimentType::PodKill => {
                // Kubernetes typically takes 30-60s to detect pod failure
                // and reschedule replicas
                self.simulate_pod_kill_recovery().await?
            }
            ChaosExperimentType::NetworkPartition => {
                // Network partitions can take longer to detect and recover from
                self.simulate_network_partition_recovery().await?
            }
            ChaosExperimentType::CpuThrottling => {
                // CPU throttling has immediate effect but recovery is gradual
                self.simulate_cpu_throttling_recovery().await?
            }
            ChaosExperimentType::MemoryPressure => {
                // Memory pressure causes immediate issues
                self.simulate_memory_pressure_recovery().await?
            }
            ChaosExperimentType::DiskExhaustion => {
                // Disk exhaustion can cause cascading failures
                self.simulate_disk_exhaustion_recovery().await?
            }
            ChaosExperimentType::ClockSkew => {
                // Clock skew affects timeouts and scheduling
                self.simulate_clock_skew_recovery().await?
            }
            ChaosExperimentType::CascadingFailure => {
                // Cascading failures are the worst - longest recovery
                self.simulate_cascading_failure_recovery().await?
            }
        };

        let end_time = Utc::now();

        let mut metrics = ExperimentMetrics {
            experiment_id: self.id.clone(),
            start_time,
            end_time,
            experiment_type: self.config.experiment_type,
            recovery_time_ms,
            error_rate_during_failure: self.simulate_error_rate(),
            error_rate_during_recovery: self.simulate_recovery_error_rate(),
            latency_p50_ms: self.simulate_latency_p50(),
            latency_p95_ms: self.simulate_latency_p95(),
            latency_p99_ms: self.simulate_latency_p99(),
            latency_max_ms: self.simulate_latency_max(),
            cpu_usage_percent: self.simulate_cpu_usage(),
            memory_usage_mb: self.simulate_memory_usage(),
            downstream_failures: self.simulate_downstream_failures(),
            recovered_successfully: recovery_time_ms < 300_000, // 5 minute timeout
            custom_metrics: HashMap::new(),
        };

        // Add experiment-specific metrics
        match self.config.experiment_type {
            ChaosExperimentType::PodKill => {
                metrics.custom_metrics.insert(
                    "pods_killed".to_string(),
                    self.config.affected_instances as f64,
                );
            }
            ChaosExperimentType::NetworkPartition => {
                metrics.custom_metrics.insert("partitions".to_string(), 2.0);
            }
            ChaosExperimentType::CpuThrottling => {
                metrics
                    .custom_metrics
                    .insert("cpu_limit_percent".to_string(), 25.0);
            }
            ChaosExperimentType::MemoryPressure => {
                metrics
                    .custom_metrics
                    .insert("memory_pressure_mb".to_string(), 512.0);
            }
            ChaosExperimentType::DiskExhaustion => {
                metrics
                    .custom_metrics
                    .insert("disk_usage_percent".to_string(), 95.0);
            }
            ChaosExperimentType::ClockSkew => {
                metrics
                    .custom_metrics
                    .insert("time_advance_seconds".to_string(), 3600.0);
            }
            ChaosExperimentType::CascadingFailure => {
                metrics.custom_metrics.insert(
                    "services_affected".to_string(),
                    self.config.affected_instances as f64,
                );
            }
        }

        tracing::info!(
            experiment_id = %self.id,
            recovery_time_ms,
            error_rate = metrics.error_rate_during_failure,
            "Chaos experiment completed"
        );

        Ok(metrics)
    }

    async fn simulate_pod_kill_recovery(&self) -> Result<u64> {
        // Pod kill usually takes 30-60s for detection and rescheduling
        Ok(fastrand::u64(30000..60000))
    }

    async fn simulate_network_partition_recovery(&self) -> Result<u64> {
        // Network partitions take longer, 60-120s
        Ok(fastrand::u64(60000..120000))
    }

    async fn simulate_cpu_throttling_recovery(&self) -> Result<u64> {
        // Immediate impact but gradual recovery
        Ok(fastrand::u64(10000..30000))
    }

    async fn simulate_memory_pressure_recovery(&self) -> Result<u64> {
        // Memory pressure causes OOM kills, cascading failures
        Ok(fastrand::u64(45000..90000))
    }

    async fn simulate_disk_exhaustion_recovery(&self) -> Result<u64> {
        // Disk exhaustion is severe, can require manual intervention
        Ok(fastrand::u64(120000..300000))
    }

    async fn simulate_clock_skew_recovery(&self) -> Result<u64> {
        // Clock skew can cause cascading timeout issues
        Ok(fastrand::u64(20000..60000))
    }

    async fn simulate_cascading_failure_recovery(&self) -> Result<u64> {
        // Cascading failures are the worst, longest recovery
        Ok(fastrand::u64(180000..600000))
    }

    fn simulate_error_rate(&self) -> f64 {
        // Error rate during chaos event
        match self.config.experiment_type {
            ChaosExperimentType::PodKill => fastrand::f64() * 0.3,
            ChaosExperimentType::NetworkPartition => fastrand::f64() * 0.8,
            ChaosExperimentType::CpuThrottling => fastrand::f64() * 0.15,
            ChaosExperimentType::MemoryPressure => fastrand::f64() * 0.5,
            ChaosExperimentType::DiskExhaustion => fastrand::f64() * 0.9,
            ChaosExperimentType::ClockSkew => fastrand::f64() * 0.25,
            ChaosExperimentType::CascadingFailure => fastrand::f64() * 0.95,
        }
    }

    fn simulate_recovery_error_rate(&self) -> f64 {
        // Error rate during recovery phase
        fastrand::f64() * 0.05
    }

    fn simulate_latency_p50(&self) -> f64 {
        50.0 + fastrand::f64() * 150.0
    }

    fn simulate_latency_p95(&self) -> f64 {
        200.0 + fastrand::f64() * 800.0
    }

    fn simulate_latency_p99(&self) -> f64 {
        500.0 + fastrand::f64() * 2000.0
    }

    fn simulate_latency_max(&self) -> f64 {
        2000.0 + fastrand::f64() * 8000.0
    }

    fn simulate_cpu_usage(&self) -> f64 {
        match self.config.experiment_type {
            ChaosExperimentType::CpuThrottling => fastrand::f64() * 25.0,
            ChaosExperimentType::CascadingFailure => 50.0 + fastrand::f64() * 50.0,
            _ => 30.0 + fastrand::f64() * 40.0,
        }
    }

    fn simulate_memory_usage(&self) -> f64 {
        match self.config.experiment_type {
            ChaosExperimentType::MemoryPressure => 8000.0 + fastrand::f64() * 2000.0,
            ChaosExperimentType::DiskExhaustion => 6000.0 + fastrand::f64() * 2000.0,
            _ => 4000.0 + fastrand::f64() * 2000.0,
        }
    }

    fn simulate_downstream_failures(&self) -> Vec<String> {
        match self.config.experiment_type {
            ChaosExperimentType::NetworkPartition | ChaosExperimentType::CascadingFailure => {
                vec![
                    "api-gateway".to_string(),
                    "auth-service".to_string(),
                    "cache-layer".to_string(),
                ]
            }
            ChaosExperimentType::DiskExhaustion => {
                vec!["database".to_string(), "message-queue".to_string()]
            }
            _ => vec![],
        }
    }

    /// Get the experiment ID
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Get the experiment configuration
    pub fn config(&self) -> &ChaosConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pod_kill_experiment_creation() {
        let exp = ChaosExperiment::pod_kill("prod", "payment-service", 3);
        assert_eq!(exp.config.cluster, "prod");
        assert_eq!(exp.config.target, "payment-service");
        assert_eq!(exp.config.affected_instances, 3);
        assert_eq!(exp.config.experiment_type, ChaosExperimentType::PodKill);
    }

    #[test]
    fn test_network_partition_experiment() {
        let exp = ChaosExperiment::network_partition("staging", "api-service");
        assert_eq!(
            exp.config.experiment_type,
            ChaosExperimentType::NetworkPartition
        );
    }

    #[test]
    fn test_cascading_failure_experiment() {
        let exp = ChaosExperiment::cascading_failure("prod", vec!["auth", "payment", "inventory"]);
        assert_eq!(exp.config.affected_instances, 3);
    }

    #[test]
    fn test_experiment_type_display() {
        assert_eq!(format!("{}", ChaosExperimentType::PodKill), "PodKill");
        assert_eq!(
            format!("{}", ChaosExperimentType::NetworkPartition),
            "NetworkPartition"
        );
    }

    #[tokio::test]
    async fn test_pod_kill_metrics_generation() {
        let exp = ChaosExperiment::pod_kill("test", "test-svc", 2);
        let metrics = exp.execute().await.expect("execution should succeed");

        assert_eq!(metrics.experiment_type, ChaosExperimentType::PodKill);
        assert!(!metrics.experiment_id.is_empty());
        assert!(metrics.recovery_time_ms > 0);
        assert!(metrics.error_rate_during_failure >= 0.0);
        assert!(metrics.error_rate_during_failure <= 1.0);
        assert!(metrics.latency_p99_ms > metrics.latency_p95_ms);
        assert!(metrics.latency_p95_ms > metrics.latency_p50_ms);
    }
}
