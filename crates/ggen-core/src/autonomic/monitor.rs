//! Monitor component of the MAPE-K autonomic loop
//!
//! This module implements the Monitor phase of the MAPE-K loop, responsible for:
//! - Collecting cluster metrics via Prometheus
//! - Detecting node crashes and network partitions
//! - Identifying backend failures
//! - Monitoring resource exhaustion
//!
//! ## Design Principles
//!
//! - **Type Safety**: All failure types encoded at compile time
//! - **Zero Cost**: Metric collection uses zero-copy references where possible
//! - **Result-Based**: All fallible operations return `Result<T, E>`
//! - **State-Based Testing**: Chicago TDD pattern with observable outputs
//!
//! ## Architecture
//!
//! ```text
//! ResilienceMonitor
//! ├── PrometheusCollector (metrics collection)
//! ├── FailureDetector (failure detection algorithms)
//! └── ClusterHealthChecker (overall health assessment)
//! ```

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

/// Configuration for the resilience monitor
#[derive(Debug, Clone)]
pub struct MonitorConfig {
    /// Prometheus endpoint URL
    pub prometheus_url: String,
    /// Query timeout duration
    pub query_timeout: Duration,
    /// Node failure detection threshold (seconds without heartbeat)
    pub node_timeout_threshold: Duration,
    /// Network partition detection threshold
    pub partition_threshold: usize,
    /// Resource exhaustion threshold (percentage)
    pub resource_threshold: f64,
}

impl Default for MonitorConfig {
    fn default() -> Self {
        Self {
            prometheus_url: "http://localhost:9090".to_string(),
            query_timeout: Duration::from_secs(5),
            node_timeout_threshold: Duration::from_secs(30),
            partition_threshold: 2,
            resource_threshold: 90.0,
        }
    }
}

/// Cluster metrics collected from Prometheus
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClusterMetrics {
    /// Node heartbeat timestamps
    pub node_heartbeats: HashMap<String, Instant>,
    /// Network connectivity matrix (node_id -> reachable_nodes)
    pub connectivity: HashMap<String, Vec<String>>,
    /// Backend health status
    pub backend_health: HashMap<BackendService, BackendStatus>,
    /// Resource usage by node
    pub resource_usage: HashMap<String, ResourceUsage>,
    /// Collection timestamp
    pub timestamp: Instant,
}

impl ClusterMetrics {
    /// Create empty cluster metrics for testing
    #[must_use]
    pub fn empty() -> Self {
        Self {
            node_heartbeats: HashMap::new(),
            connectivity: HashMap::new(),
            backend_health: HashMap::new(),
            resource_usage: HashMap::new(),
            timestamp: Instant::now(),
        }
    }
}

/// Backend type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BackendService {
    /// PostgreSQL database
    Postgres,
    /// Redis cache
    Redis,
    /// Kafka message broker
    Kafka,
    /// S3 object storage
    S3,
}

/// Backend health status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BackendStatus {
    /// Backend is healthy
    Healthy,
    /// Backend is degraded
    Degraded { error: String },
    /// Backend is unavailable
    Unavailable { error: String },
}

/// Resource usage metrics for a node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsage {
    /// CPU usage percentage (0-100)
    pub cpu_percent: f64,
    /// Memory usage percentage (0-100)
    pub memory_percent: f64,
    /// Disk usage percentage (0-100)
    pub disk_percent: f64,
    /// Network bandwidth usage (bytes/sec)
    pub network_bytes_per_sec: u64,
}

// Re-export ResourceType from analyze module to avoid duplication
pub use super::analyze::ResourceType;

/// Overall cluster health status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HealthStatus {
    /// All systems operational
    Healthy,
    /// Some degradation detected
    Degraded,
    /// Critical failures present
    Critical,
}

/// Current state of the cluster
#[derive(Debug, Clone)]
pub struct ClusterState {
    /// Collected metrics
    pub metrics: ClusterMetrics,
    /// Detected failures
    pub detected_failures: Vec<DetectedFailure>,
    /// Overall health assessment
    pub overall_health: HealthStatus,
    /// State timestamp
    pub timestamp: Instant,
}

/// Detected failure types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DetectedFailure {
    /// Node crash detected
    NodeCrash {
        /// Node identifier
        node_id: String,
        /// Last seen timestamp
        last_seen: Instant,
    },
    /// Network partition detected
    NetworkPartition {
        /// Affected node identifiers
        affected_nodes: Vec<String>,
        /// Detection timestamp
        detected_at: Instant,
    },
    /// Backend failure detected
    BackendFailure {
        /// Backend type
        backend_type: BackendService,
        /// Error message
        error: String,
    },
    /// Resource exhaustion detected
    ResourceExhaustion {
        /// Node identifier
        node_id: String,
        /// Resource type
        resource: ResourceType,
        /// Usage percentage
        usage_percent: f64,
    },
}

/// Prometheus metrics collector
#[derive(Debug, Clone)]
pub struct PrometheusCollector {
    /// Prometheus endpoint URL
    endpoint: String,
    /// Query timeout
    timeout: Duration,
}

impl PrometheusCollector {
    /// Create a new Prometheus collector
    ///
    /// # Arguments
    ///
    /// * `endpoint` - Prometheus API endpoint URL
    /// * `timeout` - Query timeout duration
    #[must_use]
    pub fn new(endpoint: String, timeout: Duration) -> Self {
        Self { endpoint, timeout }
    }

    /// Collect current cluster metrics
    ///
    /// # Errors
    ///
    /// Returns error if Prometheus query fails or response is invalid
    pub async fn collect(&self) -> Result<ClusterMetrics> {
        // In production, this would query Prometheus API
        // For now, return empty metrics as placeholder
        Ok(ClusterMetrics::empty())
    }

    /// Query Prometheus for specific metric
    ///
    /// # Errors
    ///
    /// Returns error if query fails or response is invalid
    pub async fn query(&self, _query: &str) -> Result<serde_json::Value> {
        // Production implementation would use reqwest to query Prometheus
        Ok(serde_json::json!({"status": "success", "data": {"result": []}}))
    }
}

/// Failure detector with multiple detection algorithms
#[derive(Debug, Clone)]
pub struct FailureDetector {
    /// Node timeout threshold
    node_timeout: Duration,
    /// Partition detection threshold
    partition_threshold: usize,
    /// Resource exhaustion threshold
    resource_threshold: f64,
}

impl FailureDetector {
    /// Create a new failure detector
    #[must_use]
    pub fn new(
        node_timeout: Duration, partition_threshold: usize, resource_threshold: f64,
    ) -> Self {
        Self {
            node_timeout,
            partition_threshold,
            resource_threshold,
        }
    }

    /// Detect all failure types from cluster metrics
    ///
    /// # Errors
    ///
    /// Returns error if detection algorithm fails
    pub fn detect(&self, metrics: &ClusterMetrics) -> Result<Vec<DetectedFailure>> {
        let mut failures = Vec::new();

        failures.extend(self.detect_node_crashes(metrics)?);
        failures.extend(self.detect_network_partitions(metrics)?);
        failures.extend(self.detect_backend_failures(metrics)?);
        failures.extend(self.detect_resource_exhaustion(metrics)?);

        Ok(failures)
    }

    /// Detect node crashes based on missing heartbeats
    ///
    /// # Errors
    ///
    /// Returns error if heartbeat analysis fails
    pub fn detect_node_crashes(&self, metrics: &ClusterMetrics) -> Result<Vec<DetectedFailure>> {
        let now = Instant::now();
        let mut crashes = Vec::new();

        for (node_id, &last_seen) in &metrics.node_heartbeats {
            if now.duration_since(last_seen) > self.node_timeout {
                crashes.push(DetectedFailure::NodeCrash {
                    node_id: node_id.clone(),
                    last_seen,
                });
            }
        }

        Ok(crashes)
    }

    /// Detect network partitions using connectivity matrix
    ///
    /// # Errors
    ///
    /// Returns error if partition analysis fails
    pub fn detect_network_partitions(
        &self, metrics: &ClusterMetrics,
    ) -> Result<Vec<DetectedFailure>> {
        let mut partitions = Vec::new();

        // Find isolated nodes (nodes with connectivity below threshold)
        for (node_id, reachable_nodes) in &metrics.connectivity {
            if reachable_nodes.len() < self.partition_threshold {
                partitions.push(DetectedFailure::NetworkPartition {
                    affected_nodes: vec![node_id.clone()],
                    detected_at: Instant::now(),
                });
            }
        }

        Ok(partitions)
    }

    /// Detect backend failures
    ///
    /// # Errors
    ///
    /// Returns error if backend health check fails
    pub fn detect_backend_failures(
        &self, metrics: &ClusterMetrics,
    ) -> Result<Vec<DetectedFailure>> {
        let mut failures = Vec::new();

        for (&backend_type, status) in &metrics.backend_health {
            match status {
                BackendStatus::Unavailable { error } | BackendStatus::Degraded { error } => {
                    failures.push(DetectedFailure::BackendFailure {
                        backend_type,
                        error: error.clone(),
                    });
                }
                BackendStatus::Healthy => {}
            }
        }

        Ok(failures)
    }

    /// Detect resource exhaustion
    ///
    /// # Errors
    ///
    /// Returns error if resource analysis fails
    pub fn detect_resource_exhaustion(
        &self, metrics: &ClusterMetrics,
    ) -> Result<Vec<DetectedFailure>> {
        let mut exhaustions = Vec::new();

        for (node_id, usage) in &metrics.resource_usage {
            if usage.cpu_percent >= self.resource_threshold {
                exhaustions.push(DetectedFailure::ResourceExhaustion {
                    node_id: node_id.clone(),
                    resource: ResourceType::Cpu,
                    usage_percent: usage.cpu_percent,
                });
            }

            if usage.memory_percent >= self.resource_threshold {
                exhaustions.push(DetectedFailure::ResourceExhaustion {
                    node_id: node_id.clone(),
                    resource: ResourceType::Memory,
                    usage_percent: usage.memory_percent,
                });
            }

            if usage.disk_percent >= self.resource_threshold {
                exhaustions.push(DetectedFailure::ResourceExhaustion {
                    node_id: node_id.clone(),
                    resource: ResourceType::Disk,
                    usage_percent: usage.disk_percent,
                });
            }
        }

        Ok(exhaustions)
    }
}

/// Cluster health checker
#[derive(Debug, Clone)]
pub struct ClusterHealthChecker {
    /// Critical failure threshold
    critical_threshold: usize,
}

impl ClusterHealthChecker {
    /// Create a new cluster health checker
    #[must_use]
    pub fn new(critical_threshold: usize) -> Self {
        Self { critical_threshold }
    }

    /// Assess overall cluster health from detected failures
    #[must_use]
    pub fn assess_health(&self, failures: &[DetectedFailure]) -> HealthStatus {
        if failures.is_empty() {
            return HealthStatus::Healthy;
        }

        let critical_count = failures
            .iter()
            .filter(|f| self.is_critical_failure(f))
            .count();

        if critical_count >= self.critical_threshold {
            HealthStatus::Critical
        } else {
            HealthStatus::Degraded
        }
    }

    /// Check if failure is critical
    #[must_use]
    fn is_critical_failure(&self, failure: &DetectedFailure) -> bool {
        matches!(
            failure,
            DetectedFailure::NodeCrash { .. }
                | DetectedFailure::NetworkPartition { .. }
                | DetectedFailure::BackendFailure {
                    backend_type: BackendService::Postgres,
                    ..
                }
        )
    }
}

/// Resilience monitor coordinating metrics collection and failure detection
#[derive(Debug, Clone)]
pub struct ResilienceMonitor {
    /// Metrics collector
    metrics_collector: PrometheusCollector,
    /// Failure detector
    failure_detector: FailureDetector,
    /// Health checker
    health_checker: ClusterHealthChecker,
}

impl ResilienceMonitor {
    /// Create a new resilience monitor
    ///
    /// # Errors
    ///
    /// Returns error if configuration is invalid
    pub fn new(config: MonitorConfig) -> Result<Self> {
        if config.prometheus_url.is_empty() {
            return Err("Prometheus URL cannot be empty".into());
        }

        Ok(Self {
            metrics_collector: PrometheusCollector::new(
                config.prometheus_url,
                config.query_timeout,
            ),
            failure_detector: FailureDetector::new(
                config.node_timeout_threshold,
                config.partition_threshold,
                config.resource_threshold,
            ),
            health_checker: ClusterHealthChecker::new(2),
        })
    }

    /// Observe current cluster state (single observation)
    ///
    /// # Errors
    ///
    /// Returns error if metrics collection or failure detection fails
    pub async fn observe(&self) -> Result<ClusterState> {
        let metrics = self.metrics_collector.collect().await?;
        let detected_failures = self.failure_detector.detect(&metrics)?;
        let overall_health = self.health_checker.assess_health(&detected_failures);

        Ok(ClusterState {
            metrics,
            detected_failures,
            overall_health,
            timestamp: Instant::now(),
        })
    }

    /// Start continuous monitoring with periodic observations
    ///
    /// # Errors
    ///
    /// Returns error if monitoring loop setup fails
    pub async fn start_continuous_monitoring(
        &self, interval: Duration,
    ) -> Result<mpsc::Receiver<ClusterState>> {
        let (tx, rx) = mpsc::channel(100);
        let monitor = self.clone();

        tokio::spawn(async move {
            let mut interval_timer = tokio::time::interval(interval);
            loop {
                interval_timer.tick().await;
                match monitor.observe().await {
                    Ok(state) => {
                        if tx.send(state).await.is_err() {
                            break; // Receiver dropped
                        }
                    }
                    Err(e) => {
                        eprintln!("Monitoring error: {}", e);
                    }
                }
            }
        });

        Ok(rx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_monitor_config_default() {
        // Arrange & Act
        let config = MonitorConfig::default();

        // Assert
        assert_eq!(config.prometheus_url, "http://localhost:9090");
        assert_eq!(config.query_timeout, Duration::from_secs(5));
        assert_eq!(config.node_timeout_threshold, Duration::from_secs(30));
        assert_eq!(config.partition_threshold, 2);
        assert_eq!(config.resource_threshold, 90.0);
    }

    #[test]
    fn test_cluster_metrics_empty() {
        // Arrange & Act
        let metrics = ClusterMetrics::empty();

        // Assert
        assert!(metrics.node_heartbeats.is_empty());
        assert!(metrics.connectivity.is_empty());
        assert!(metrics.backend_health.is_empty());
        assert!(metrics.resource_usage.is_empty());
    }

    #[test]
    fn test_failure_detector_detects_node_crashes() {
        // Arrange
        let detector = FailureDetector::new(Duration::from_secs(30), 2, 90.0);
        let mut metrics = ClusterMetrics::empty();
        let old_timestamp = Instant::now() - Duration::from_secs(60);
        metrics
            .node_heartbeats
            .insert("node-1".to_string(), old_timestamp);

        // Act
        let failures = detector
            .detect_node_crashes(&metrics)
            .expect("Failed to detect node crashes");

        // Assert
        assert_eq!(failures.len(), 1);
        match &failures[0] {
            DetectedFailure::NodeCrash { node_id, last_seen } => {
                assert_eq!(node_id, "node-1");
                assert_eq!(*last_seen, old_timestamp);
            }
            _ => panic!("Expected NodeCrash failure"),
        }
    }

    #[test]
    fn test_failure_detector_detects_network_partitions() {
        // Arrange
        let detector = FailureDetector::new(Duration::from_secs(30), 2, 90.0);
        let mut metrics = ClusterMetrics::empty();
        metrics.connectivity.insert("node-1".to_string(), vec![]); // Isolated node

        // Act
        let failures = detector
            .detect_network_partitions(&metrics)
            .expect("Failed to detect network partitions");

        // Assert
        assert_eq!(failures.len(), 1);
        match &failures[0] {
            DetectedFailure::NetworkPartition { affected_nodes, .. } => {
                assert_eq!(affected_nodes.len(), 1);
                assert_eq!(affected_nodes[0], "node-1");
            }
            _ => panic!("Expected NetworkPartition failure"),
        }
    }

    #[test]
    fn test_failure_detector_detects_backend_failures() {
        // Arrange
        let detector = FailureDetector::new(Duration::from_secs(30), 2, 90.0);
        let mut metrics = ClusterMetrics::empty();
        metrics.backend_health.insert(
            BackendService::Postgres,
            BackendStatus::Unavailable {
                error: "Connection timeout".to_string(),
            },
        );

        // Act
        let failures = detector
            .detect_backend_failures(&metrics)
            .expect("Failed to detect backend failures");

        // Assert
        assert_eq!(failures.len(), 1);
        match &failures[0] {
            DetectedFailure::BackendFailure {
                backend_type,
                error,
            } => {
                assert_eq!(*backend_type, BackendService::Postgres);
                assert_eq!(error, "Connection timeout");
            }
            _ => panic!("Expected BackendFailure"),
        }
    }

    #[test]
    fn test_failure_detector_detects_resource_exhaustion() {
        // Arrange
        let detector = FailureDetector::new(Duration::from_secs(30), 2, 90.0);
        let mut metrics = ClusterMetrics::empty();
        metrics.resource_usage.insert(
            "node-1".to_string(),
            ResourceUsage {
                cpu_percent: 95.0,
                memory_percent: 92.0,
                disk_percent: 50.0,
                network_bytes_per_sec: 1000,
            },
        );

        // Act
        let failures = detector
            .detect_resource_exhaustion(&metrics)
            .expect("Failed to detect resource exhaustion");

        // Assert
        assert_eq!(failures.len(), 2); // CPU and Memory
        assert!(failures.iter().any(|f| matches!(
            f,
            DetectedFailure::ResourceExhaustion {
                resource: ResourceType::Cpu,
                usage_percent: 95.0,
                ..
            }
        )));
        assert!(failures.iter().any(|f| matches!(
            f,
            DetectedFailure::ResourceExhaustion {
                resource: ResourceType::Memory,
                usage_percent: 92.0,
                ..
            }
        )));
    }

    #[test]
    fn test_cluster_health_checker_healthy() {
        // Arrange
        let checker = ClusterHealthChecker::new(2);
        let failures = vec![];

        // Act
        let health = checker.assess_health(&failures);

        // Assert
        assert_eq!(health, HealthStatus::Healthy);
    }

    #[test]
    fn test_cluster_health_checker_degraded() {
        // Arrange
        let checker = ClusterHealthChecker::new(2);
        let failures = vec![DetectedFailure::BackendFailure {
            backend_type: BackendService::Redis,
            error: "Connection failed".to_string(),
        }];

        // Act
        let health = checker.assess_health(&failures);

        // Assert
        assert_eq!(health, HealthStatus::Degraded);
    }

    #[test]
    fn test_cluster_health_checker_critical() {
        // Arrange
        let checker = ClusterHealthChecker::new(2);
        let failures = vec![
            DetectedFailure::NodeCrash {
                node_id: "node-1".to_string(),
                last_seen: Instant::now(),
            },
            DetectedFailure::NetworkPartition {
                affected_nodes: vec!["node-2".to_string()],
                detected_at: Instant::now(),
            },
        ];

        // Act
        let health = checker.assess_health(&failures);

        // Assert
        assert_eq!(health, HealthStatus::Critical);
    }
}
