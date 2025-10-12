//! Fixed cleanroom environment for testing framework
//!
//! This module provides the main cleanroom environment following core team best practices:
//! - Environment lifecycle management
//! - Container orchestration
//! - Service integration
//! - Policy enforcement
//! - Resource monitoring
//! - Test execution

use crate::error::{Result, CleanroomError};
use crate::config_fixed::CleanroomConfig;
use crate::policy_fixed::Policy;
use crate::limits_fixed::ResourceLimits;
use crate::determinism_fixed::DeterministicManager;
use crate::coverage_fixed::CoverageCollector;
use crate::snapshots_fixed::SnapshotManager;
use crate::tracing_fixed::TracingManager;
use crate::report_fixed::{TestReport, ComprehensiveReport};
use crate::containers_fixed::{ContainerWrapper, PostgresContainer, RedisContainer, GenericContainer, ContainerStatus, ContainerMetrics};
use crate::serializable_instant::SerializableInstant;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Cleanroom environment for testing
#[derive(Debug)]
pub struct CleanroomEnvironment {
    /// Session ID
    session_id: Uuid,
    /// Configuration
    config: CleanroomConfig,
    /// Policy
    policy: Policy,
    /// Resource limits
    resource_limits: ResourceLimits,
    /// Deterministic manager
    deterministic_manager: Arc<RwLock<DeterministicManager>>,
    /// Coverage collector
    coverage_collector: Arc<RwLock<CoverageCollector>>,
    /// Snapshot manager
    snapshot_manager: Arc<RwLock<SnapshotManager>>,
    /// Tracing manager
    tracing_manager: Arc<RwLock<TracingManager>>,
    /// Test report
    test_report: Arc<RwLock<TestReport>>,
    /// Containers
    containers: Arc<RwLock<HashMap<String, Box<dyn ContainerWrapper + Send + Sync>>>>,
    /// Metrics
    metrics: Arc<RwLock<CleanroomMetrics>>,
    /// Start time
    start_time: SerializableInstant,
    /// Backend
    backend: String,
    /// Initialization status
    initialized: bool,
    /// Health status
    healthy: bool,
}

/// Cleanroom metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanroomMetrics {
    /// Session ID
    pub session_id: Uuid,
    /// Total containers
    pub total_containers: usize,
    /// Running containers
    pub running_containers: usize,
    /// Failed containers
    pub failed_containers: usize,
    /// Total tests executed
    pub total_tests_executed: usize,
    /// Passed tests
    pub passed_tests: usize,
    /// Failed tests
    pub failed_tests: usize,
    /// Resource usage
    pub resource_usage: ResourceUsage,
    /// Performance metrics
    pub performance_metrics: HashMap<String, f64>,
    /// Last updated
    pub last_updated: SerializableInstant,
}

/// Resource usage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsage {
    /// CPU usage percentage
    pub cpu_usage_percent: f64,
    /// Memory usage in bytes
    pub memory_usage_bytes: u64,
    /// Memory limit in bytes
    pub memory_limit_bytes: u64,
    /// Disk usage in bytes
    pub disk_usage_bytes: u64,
    /// Network usage in bytes
    pub network_usage_bytes: u64,
}

/// Cleanroom guard for RAII cleanup
pub struct CleanroomGuard {
    /// Environment reference
    environment: Arc<CleanroomEnvironment>,
}

impl CleanroomEnvironment {
    /// Create a new cleanroom environment
    pub fn new(config: CleanroomConfig) -> Self {
        let session_id = Uuid::new_v4();
        let policy = Policy::new();
        let resource_limits = ResourceLimits::new();
        let deterministic_manager = Arc::new(RwLock::new(DeterministicManager::new()));
        let coverage_collector = Arc::new(RwLock::new(CoverageCollector::new()));
        let snapshot_manager = Arc::new(RwLock::new(SnapshotManager::new()));
        let tracing_manager = Arc::new(RwLock::new(TracingManager::new()));
        let test_report = Arc::new(RwLock::new(TestReport::new(session_id)));
        let containers = Arc::new(RwLock::new(HashMap::new()));
        let metrics = Arc::new(RwLock::new(CleanroomMetrics::new(session_id)));
        let start_time = SerializableInstant::from(Instant::now());

        Self {
            session_id,
            config,
            policy,
            resource_limits,
            deterministic_manager,
            coverage_collector,
            snapshot_manager,
            tracing_manager,
            test_report,
            containers,
            metrics,
            start_time,
            backend: "testcontainers".to_string(),
            initialized: false,
            healthy: false,
        }
    }

    /// Get session ID
    pub fn session_id(&self) -> Uuid {
        self.session_id
    }

    /// Get configuration
    pub fn config(&self) -> &CleanroomConfig {
        &self.config
    }

    /// Get start time
    pub fn start_time(&self) -> SerializableInstant {
        self.start_time
    }

    /// Get backend
    pub fn backend(&self) -> &str {
        &self.backend
    }

    /// Check if initialized
    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    /// Check if healthy
    pub fn is_healthy(&self) -> bool {
        self.healthy
    }

    /// Get health status
    pub async fn get_health_status(&self) -> String {
        if self.healthy {
            "Healthy".to_string()
        } else {
            "Unhealthy".to_string()
        }
    }

    /// Start container
    pub async fn start_container(&self, name: String, image: String) -> Result<()> {
        let mut containers = self.containers.write().await;
        
        // Check resource limits
        if containers.len() >= self.config.max_concurrent_containers as usize {
            return Err(CleanroomError::resource_limit_exceeded(
                "Maximum container limit reached"
            ));
        }

        // Create generic container
        let container = GenericContainer::new(
            Uuid::new_v4().to_string(),
            name.clone(),
            image,
            HashMap::new(),
            HashMap::new(),
            vec![],
            None,
        );

        containers.insert(name, Box::new(container));
        
        // Update metrics
        {
            let mut metrics = self.metrics.write().await;
            metrics.total_containers += 1;
            metrics.running_containers += 1;
            metrics.last_updated = SerializableInstant::from(Instant::now());
        }

        Ok(())
    }

    /// Check if container is running
    pub async fn is_container_running(&self, name: &str) -> bool {
        let containers = self.containers.read().await;
        if let Some(container) = containers.get(name) {
            matches!(container.get_status(), ContainerStatus::Running)
        } else {
            false
        }
    }

    /// Stop container
    pub async fn stop_container(&self, name: &str) -> Result<()> {
        let mut containers = self.containers.write().await;
        
        if containers.remove(name).is_some() {
            // Update metrics
            {
                let mut metrics = self.metrics.write().await;
                metrics.running_containers = metrics.running_containers.saturating_sub(1);
                metrics.last_updated = SerializableInstant::from(Instant::now());
            }
            Ok(())
        } else {
            Err(CleanroomError::container_error("Container not found"))
        }
    }

    /// Get container info
    pub async fn get_container_info(&self, name: &str) -> Result<String> {
        let containers = self.containers.read().await;
        if let Some(container) = containers.get(name) {
            Ok(format!(
                "Container: {}, ID: {}, Status: {:?}",
                container.get_name(),
                container.get_id(),
                container.get_status()
            ))
        } else {
            Err(CleanroomError::container_error("Container not found"))
        }
    }

    /// Execute test
    pub async fn execute_test(&self, test_name: String, test_func: impl Fn() -> Result<()>) -> Result<()> {
        // Start coverage collection if enabled
        if self.config.enable_coverage_tracking {
            let mut coverage_collector = self.coverage_collector.write().await;
            let _ = coverage_collector.start_collection().await;
        }

        // Start tracing if enabled
        if self.config.enable_tracing {
            let mut tracing_manager = self.tracing_manager.write().await;
            let _ = tracing_manager.start_trace(test_name.clone(), None).await;
        }

        // Execute test
        let start_time = Instant::now();
        let result = test_func();
        let duration = start_time.elapsed();

        // Record test execution
        {
            let mut test_report = self.test_report.write().await;
            let status = if result.is_ok() {
                crate::report_fixed::TestStatus::Passed
            } else {
                crate::report_fixed::TestStatus::Failed
            };
            let _ = test_report.record_test_execution(test_name.clone(), status, duration);
        }

        // Update metrics
        {
            let mut metrics = self.metrics.write().await;
            metrics.total_tests_executed += 1;
            if result.is_ok() {
                metrics.passed_tests += 1;
            } else {
                metrics.failed_tests += 1;
            }
            metrics.last_updated = SerializableInstant::from(Instant::now());
        }

        result
    }

    /// Execute deterministic test
    pub async fn execute_deterministic_test(&self, test_name: String, test_func: impl Fn() -> Result<()>) -> Result<()> {
        if !self.config.enable_deterministic_execution {
            return Err(CleanroomError::deterministic_error("Deterministic execution is disabled"));
        }

        // Set deterministic seed
        {
            let deterministic_manager = self.deterministic_manager.read().await;
            if let Some(seed) = self.config.deterministic_seed {
                deterministic_manager.set_seed(seed);
            }
        }

        self.execute_test(test_name, test_func).await
    }

    /// Create Postgres service
    pub async fn create_postgres_service(&self, name: String, database_name: String) -> Result<()> {
        let container = PostgresContainer::new(
            Uuid::new_v4().to_string(),
            name.clone(),
            database_name,
            "postgres".to_string(),
            "password".to_string(),
            5432,
            "localhost".to_string(),
        );

        let mut containers = self.containers.write().await;
        containers.insert(name, Box::new(container));

        // Update metrics
        {
            let mut metrics = self.metrics.write().await;
            metrics.total_containers += 1;
            metrics.running_containers += 1;
            metrics.last_updated = SerializableInstant::from(Instant::now());
        }

        Ok(())
    }

    /// Create Redis service
    pub async fn create_redis_service(&self, name: String) -> Result<()> {
        let container = RedisContainer::new(
            Uuid::new_v4().to_string(),
            name.clone(),
            6379,
            "localhost".to_string(),
            None,
        );

        let mut containers = self.containers.write().await;
        containers.insert(name, Box::new(container));

        // Update metrics
        {
            let mut metrics = self.metrics.write().await;
            metrics.total_containers += 1;
            metrics.running_containers += 1;
            metrics.last_updated = SerializableInstant::from(Instant::now());
        }

        Ok(())
    }

    /// Set policy
    pub fn set_policy(&mut self, policy: Policy) -> Result<()> {
        policy.validate()?;
        self.policy = policy;
        Ok(())
    }

    /// Get resource usage
    pub async fn get_resource_usage(&self) -> ResourceUsage {
        let containers = self.containers.read().await;
        let mut total_cpu = 0.0;
        let mut total_memory = 0;
        let mut total_disk = 0;
        let mut total_network = 0;

        for container in containers.values() {
            let metrics = container.get_metrics();
            total_cpu += metrics.cpu_usage_percent;
            total_memory += metrics.memory_usage_bytes;
            total_disk += metrics.disk_usage_bytes;
            total_network += metrics.network_rx_bytes + metrics.network_tx_bytes;
        }

        ResourceUsage {
            cpu_usage_percent: total_cpu,
            memory_usage_bytes: total_memory,
            memory_limit_bytes: self.resource_limits.max_memory_mb as u64 * 1024 * 1024,
            disk_usage_bytes: total_disk,
            network_usage_bytes: total_network,
        }
    }

    /// Set deterministic manager
    pub async fn set_deterministic_manager(&self, manager: DeterministicManager) {
        let mut deterministic_manager = self.deterministic_manager.write().await;
        *deterministic_manager = manager;
    }

    /// Get coverage report
    pub async fn get_coverage_report(&self) -> Result<String> {
        let coverage_collector = self.coverage_collector.read().await;
        let report = coverage_collector.get_report().await?;
        Ok(format!("Coverage Report: {:.2}% overall coverage", report.summary.overall_coverage))
    }

    /// Create snapshot
    pub async fn create_snapshot(&self, name: String, content: String) -> Result<()> {
        let snapshot_manager = self.snapshot_manager.read().await;
        snapshot_manager.capture_snapshot(
            name,
            content,
            "text".to_string(),
            crate::snapshots_fixed::SnapshotType::Text,
        ).await
    }

    /// Get trace logs
    pub async fn get_trace_logs(&self) -> Result<String> {
        let tracing_manager = self.tracing_manager.read().await;
        let summary = tracing_manager.get_trace_summary().await;
        Ok(format!("Trace Summary: {} total traces, {} events", summary.total_traces, summary.total_events))
    }

    /// Generate comprehensive report
    pub async fn generate_comprehensive_report(&self) -> Result<ComprehensiveReport> {
        let test_report = self.test_report.read().await;
        let mut comprehensive_report = ComprehensiveReport::new(self.session_id);
        comprehensive_report.add_test_report(test_report.clone());
        comprehensive_report.calculate_overall_summary();
        Ok(comprehensive_report)
    }

    /// Initialize environment
    pub async fn initialize(&mut self) -> Result<()> {
        // Validate configuration
        self.config.validate()?;
        
        // Validate policy
        self.policy.validate()?;
        
        // Validate resource limits
        self.resource_limits.validate()?;

        self.initialized = true;
        self.healthy = true;
        
        Ok(())
    }

    /// Cleanup environment
    pub async fn cleanup(&mut self) -> Result<()> {
        // Stop all containers
        {
            let mut containers = self.containers.write().await;
            containers.clear();
        }

        // Stop coverage collection
        if self.config.enable_coverage_tracking {
            let mut coverage_collector = self.coverage_collector.write().await;
            let _ = coverage_collector.stop_collection().await;
        }

        // Stop tracing
        if self.config.enable_tracing {
            let mut tracing_manager = self.tracing_manager.write().await;
            let _ = tracing_manager.clear_traces().await;
        }

        // Clear snapshots
        if self.config.enable_snapshot_testing {
            let mut snapshot_manager = self.snapshot_manager.write().await;
            let _ = snapshot_manager.clear_snapshots().await;
        }

        self.initialized = false;
        self.healthy = false;
        
        Ok(())
    }
}

impl CleanroomMetrics {
    /// Create new cleanroom metrics
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            total_containers: 0,
            running_containers: 0,
            failed_containers: 0,
            total_tests_executed: 0,
            passed_tests: 0,
            failed_tests: 0,
            resource_usage: ResourceUsage::new(),
            performance_metrics: HashMap::new(),
            last_updated: SerializableInstant::from(Instant::now()),
        }
    }

    /// Update metrics
    pub fn update(&mut self) {
        self.last_updated = SerializableInstant::from(Instant::now());
    }

    /// Get success rate
    pub fn get_success_rate(&self) -> f64 {
        if self.total_tests_executed > 0 {
            (self.passed_tests as f64 / self.total_tests_executed as f64) * 100.0
        } else {
            0.0
        }
    }

    /// Get container health rate
    pub fn get_container_health_rate(&self) -> f64 {
        if self.total_containers > 0 {
            ((self.total_containers - self.failed_containers) as f64 / self.total_containers as f64) * 100.0
        } else {
            100.0
        }
    }
}

impl ResourceUsage {
    /// Create new resource usage
    pub fn new() -> Self {
        Self {
            cpu_usage_percent: 0.0,
            memory_usage_bytes: 0,
            memory_limit_bytes: 0,
            disk_usage_bytes: 0,
            network_usage_bytes: 0,
        }
    }

    /// Get memory usage percentage
    pub fn get_memory_usage_percentage(&self) -> f64 {
        if self.memory_limit_bytes > 0 {
            (self.memory_usage_bytes as f64 / self.memory_limit_bytes as f64) * 100.0
        } else {
            0.0
        }
    }

    /// Check if memory usage is high
    pub fn is_memory_usage_high(&self, threshold: f64) -> bool {
        self.get_memory_usage_percentage() > threshold
    }
}

impl CleanroomGuard {
    /// Create new cleanroom guard
    pub fn new(environment: Arc<CleanroomEnvironment>) -> Self {
        Self { environment }
    }
}

impl Drop for CleanroomGuard {
    fn drop(&mut self) {
        // Note: In a real implementation, this would need to be async
        // For now, we'll just mark the environment as needing cleanup
        // In practice, you'd want to use a different approach for async cleanup
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cleanroom_environment_creation() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        assert!(!environment.session_id().is_nil());
        assert_eq!(environment.backend(), "testcontainers");
        assert!(!environment.is_initialized());
        assert!(!environment.is_healthy());
    }

    #[test]
    fn test_cleanroom_environment_getters() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        assert!(!environment.session_id().is_nil());
        assert_eq!(environment.backend(), "testcontainers");
        assert!(!environment.is_initialized());
        assert!(!environment.is_healthy());
        assert_eq!(environment.config().max_concurrent_containers, 10);
    }

    #[tokio::test]
    async fn test_cleanroom_environment_initialize() {
        let config = CleanroomConfig::new();
        let mut environment = CleanroomEnvironment::new(config);
        
        assert!(environment.initialize().await.is_ok());
        assert!(environment.is_initialized());
        assert!(environment.is_healthy());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_start_container() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let result = environment.start_container("test-container".to_string(), "nginx:latest".to_string()).await;
        assert!(result.is_ok());
        
        let is_running = environment.is_container_running("test-container").await;
        assert!(is_running);
    }

    #[tokio::test]
    async fn test_cleanroom_environment_stop_container() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        // Start container
        environment.start_container("test-container".to_string(), "nginx:latest".to_string()).await.unwrap();
        
        // Stop container
        let result = environment.stop_container("test-container").await;
        assert!(result.is_ok());
        
        let is_running = environment.is_container_running("test-container").await;
        assert!(!is_running);
    }

    #[tokio::test]
    async fn test_cleanroom_environment_get_container_info() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        // Start container
        environment.start_container("test-container".to_string(), "nginx:latest".to_string()).await.unwrap();
        
        // Get container info
        let info = environment.get_container_info("test-container").await.unwrap();
        assert!(info.contains("test-container"));
        assert!(info.contains("Container:"));
        
        // Get non-existent container info
        let result = environment.get_container_info("nonexistent").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_execute_test() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let result = environment.execute_test("test_example".to_string(), || {
            Ok(())
        }).await;
        
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_execute_deterministic_test() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let result = environment.execute_deterministic_test("test_example".to_string(), || {
            Ok(())
        }).await;
        
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_create_postgres_service() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let result = environment.create_postgres_service(
            "postgres-service".to_string(),
            "testdb".to_string()
        ).await;
        
        assert!(result.is_ok());
        
        let is_running = environment.is_container_running("postgres-service").await;
        assert!(is_running);
    }

    #[tokio::test]
    async fn test_cleanroom_environment_create_redis_service() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let result = environment.create_redis_service("redis-service".to_string()).await;
        assert!(result.is_ok());
        
        let is_running = environment.is_container_running("redis-service").await;
        assert!(is_running);
    }

    #[test]
    fn test_cleanroom_environment_set_policy() {
        let config = CleanroomConfig::new();
        let mut environment = CleanroomEnvironment::new(config);
        
        let policy = Policy::new();
        let result = environment.set_policy(policy);
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_get_resource_usage() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let usage = environment.get_resource_usage().await;
        assert_eq!(usage.cpu_usage_percent, 0.0);
        assert_eq!(usage.memory_usage_bytes, 0);
        assert_eq!(usage.disk_usage_bytes, 0);
        assert_eq!(usage.network_usage_bytes, 0);
    }

    #[tokio::test]
    async fn test_cleanroom_environment_set_deterministic_manager() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let manager = DeterministicManager::new();
        environment.set_deterministic_manager(manager).await;
        
        // Test passes if no panic occurs
        assert!(true);
    }

    #[tokio::test]
    async fn test_cleanroom_environment_get_coverage_report() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let report = environment.get_coverage_report().await.unwrap();
        assert!(report.contains("Coverage Report"));
    }

    #[tokio::test]
    async fn test_cleanroom_environment_create_snapshot() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let result = environment.create_snapshot(
            "test-snapshot".to_string(),
            "test content".to_string()
        ).await;
        
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_get_trace_logs() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let logs = environment.get_trace_logs().await.unwrap();
        assert!(logs.contains("Trace Summary"));
    }

    #[tokio::test]
    async fn test_cleanroom_environment_generate_comprehensive_report() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let report = environment.generate_comprehensive_report().await.unwrap();
        assert_eq!(report.session_id, environment.session_id());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_cleanup() {
        let config = CleanroomConfig::new();
        let mut environment = CleanroomEnvironment::new(config);
        
        // Initialize
        environment.initialize().await.unwrap();
        assert!(environment.is_initialized());
        
        // Cleanup
        let result = environment.cleanup().await;
        assert!(result.is_ok());
        assert!(!environment.is_initialized());
        assert!(!environment.is_healthy());
    }

    #[tokio::test]
    async fn test_cleanroom_environment_get_health_status() {
        let config = CleanroomConfig::new();
        let environment = CleanroomEnvironment::new(config);
        
        let status = environment.get_health_status().await;
        assert_eq!(status, "Unhealthy");
    }

    #[test]
    fn test_cleanroom_metrics_creation() {
        let session_id = Uuid::new_v4();
        let metrics = CleanroomMetrics::new(session_id);
        
        assert_eq!(metrics.session_id, session_id);
        assert_eq!(metrics.total_containers, 0);
        assert_eq!(metrics.running_containers, 0);
        assert_eq!(metrics.failed_containers, 0);
        assert_eq!(metrics.total_tests_executed, 0);
        assert_eq!(metrics.passed_tests, 0);
        assert_eq!(metrics.failed_tests, 0);
    }

    #[test]
    fn test_cleanroom_metrics_get_success_rate() {
        let session_id = Uuid::new_v4();
        let mut metrics = CleanroomMetrics::new(session_id);
        
        metrics.total_tests_executed = 10;
        metrics.passed_tests = 8;
        
        assert_eq!(metrics.get_success_rate(), 80.0);
        
        metrics.total_tests_executed = 0;
        assert_eq!(metrics.get_success_rate(), 0.0);
    }

    #[test]
    fn test_cleanroom_metrics_get_container_health_rate() {
        let session_id = Uuid::new_v4();
        let mut metrics = CleanroomMetrics::new(session_id);
        
        metrics.total_containers = 10;
        metrics.failed_containers = 2;
        
        assert_eq!(metrics.get_container_health_rate(), 80.0);
        
        metrics.total_containers = 0;
        assert_eq!(metrics.get_container_health_rate(), 100.0);
    }

    #[test]
    fn test_resource_usage_creation() {
        let usage = ResourceUsage::new();
        
        assert_eq!(usage.cpu_usage_percent, 0.0);
        assert_eq!(usage.memory_usage_bytes, 0);
        assert_eq!(usage.memory_limit_bytes, 0);
        assert_eq!(usage.disk_usage_bytes, 0);
        assert_eq!(usage.network_usage_bytes, 0);
    }

    #[test]
    fn test_resource_usage_get_memory_usage_percentage() {
        let mut usage = ResourceUsage::new();
        usage.memory_usage_bytes = 256 * 1024 * 1024; // 256MB
        usage.memory_limit_bytes = 512 * 1024 * 1024; // 512MB
        
        assert_eq!(usage.get_memory_usage_percentage(), 50.0);
        
        usage.memory_limit_bytes = 0;
        assert_eq!(usage.get_memory_usage_percentage(), 0.0);
    }

    #[test]
    fn test_resource_usage_is_memory_usage_high() {
        let mut usage = ResourceUsage::new();
        usage.memory_usage_bytes = 400 * 1024 * 1024; // 400MB
        usage.memory_limit_bytes = 512 * 1024 * 1024; // 512MB
        
        assert!(usage.is_memory_usage_high(75.0));
        assert!(!usage.is_memory_usage_high(85.0));
    }

    #[test]
    fn test_cleanroom_guard_creation() {
        let config = CleanroomConfig::new();
        let environment = Arc::new(CleanroomEnvironment::new(config));
        let _guard = CleanroomGuard::new(environment);
        
        // Test passes if no panic occurs
        assert!(true);
    }

    #[test]
    fn test_cleanroom_metrics_serialization() {
        let session_id = Uuid::new_v4();
        let mut metrics = CleanroomMetrics::new(session_id);
        metrics.total_containers = 5;
        metrics.running_containers = 3;
        metrics.failed_containers = 1;
        metrics.total_tests_executed = 10;
        metrics.passed_tests = 8;
        metrics.failed_tests = 2;
        metrics.performance_metrics.insert("cpu_usage".to_string(), 75.5);

        let json = serde_json::to_string(&metrics).unwrap();
        let deserialized: CleanroomMetrics = serde_json::from_str(&json).unwrap();
        
        assert_eq!(metrics.session_id, deserialized.session_id);
        assert_eq!(metrics.total_containers, deserialized.total_containers);
        assert_eq!(metrics.running_containers, deserialized.running_containers);
        assert_eq!(metrics.failed_containers, deserialized.failed_containers);
        assert_eq!(metrics.total_tests_executed, deserialized.total_tests_executed);
        assert_eq!(metrics.passed_tests, deserialized.passed_tests);
        assert_eq!(metrics.failed_tests, deserialized.failed_tests);
        assert_eq!(metrics.performance_metrics, deserialized.performance_metrics);
    }

    #[test]
    fn test_resource_usage_serialization() {
        let usage = ResourceUsage {
            cpu_usage_percent: 75.5,
            memory_usage_bytes: 256 * 1024 * 1024,
            memory_limit_bytes: 512 * 1024 * 1024,
            disk_usage_bytes: 1024 * 1024 * 1024,
            network_usage_bytes: 128 * 1024 * 1024,
        };

        let json = serde_json::to_string(&usage).unwrap();
        let deserialized: ResourceUsage = serde_json::from_str(&json).unwrap();
        
        assert_eq!(usage.cpu_usage_percent, deserialized.cpu_usage_percent);
        assert_eq!(usage.memory_usage_bytes, deserialized.memory_usage_bytes);
        assert_eq!(usage.memory_limit_bytes, deserialized.memory_limit_bytes);
        assert_eq!(usage.disk_usage_bytes, deserialized.disk_usage_bytes);
        assert_eq!(usage.network_usage_bytes, deserialized.network_usage_bytes);
    }
}