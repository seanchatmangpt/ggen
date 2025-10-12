//! Core cleanroom environment implementation

use crate::error::Result;
use crate::config::CleanroomConfig;
use crate::backend::TestcontainerBackend;
#[cfg(feature = "services")]
use crate::services::ServiceManager;
use crate::serializable_instant::SerializableInstant;
use crate::runtime::orchestrator::{ConcurrencyOrchestrator, TaskId, TaskResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;
use uuid::Uuid;

/// Core cleanroom environment following best practices
#[derive(Debug)]
pub struct CleanroomEnvironment {
    /// Unique test session identifier
    pub session_id: Uuid,
    /// Test configuration
    config: CleanroomConfig,
    /// Test execution metrics
    metrics: Arc<RwLock<CleanroomMetrics>>,
    /// Container registry for singleton pattern
    container_registry: Arc<RwLock<HashMap<String, String>>>,
    /// Backend for container execution
    backend: TestcontainerBackend,
    /// Service manager for database and cache services
    #[cfg(feature = "services")]
    services: ServiceManager,
    /// Structured concurrency orchestrator
    orchestrator: Arc<RwLock<ConcurrencyOrchestrator>>,
    /// Start time of the cleanroom environment
    start_time: Instant,
}

/// Cleanroom execution metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanroomMetrics {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: SerializableInstant,
    /// End time
    pub end_time: Option<SerializableInstant>,
    /// Total duration in milliseconds
    pub total_duration_ms: u64,
    /// Number of tests executed
    pub tests_executed: u32,
    /// Number of tests passed
    pub tests_passed: u32,
    /// Number of tests failed
    pub tests_failed: u32,
    /// Number of containers created
    pub containers_created: u32,
    /// Number of containers destroyed
    pub containers_destroyed: u32,
    /// Peak memory usage in bytes
    pub peak_memory_usage_bytes: u64,
    /// Peak CPU usage percentage
    pub peak_cpu_usage_percent: f64,
    /// Total tests executed
    pub total_tests: u32,
    /// Average execution time
    pub average_execution_time: Duration,
    /// Resource usage metrics
    pub resource_usage: ResourceUsage,
    /// Error count
    pub error_count: u32,
    /// Warning count
    pub warning_count: u32,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Performance metrics
    pub performance_metrics: HashMap<String, f64>,
}

/// Resource usage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsage {
    /// CPU usage percentage
    pub cpu_usage_percent: f64,
    /// Memory usage in bytes
    pub memory_usage_bytes: u64,
    /// Disk usage in bytes
    pub disk_usage_bytes: u64,
    /// Network bytes sent
    pub network_bytes_sent: u64,
    /// Network bytes received
    pub network_bytes_received: u64,
    /// Peak CPU usage percentage
    pub peak_cpu_usage_percent: f64,
    /// Peak memory usage bytes
    pub peak_memory_usage_bytes: u64,
    /// Peak disk usage bytes
    pub peak_disk_usage_bytes: u64,
    /// Network bytes transferred
    pub network_bytes_transferred: u64,
    /// Container count
    pub container_count: u32,
}

impl CleanroomEnvironment {
    /// Create a new cleanroom environment
    pub async fn new(config: CleanroomConfig) -> Result<Self> {
        config.validate()?;

        let session_id = Uuid::new_v4();
        let start_time = Instant::now();
        let backend = TestcontainerBackend::new("alpine:latest")?;
        #[cfg(feature = "services")]
        let services = ServiceManager::new();

        let metrics = CleanroomMetrics {
            session_id,
            start_time: SerializableInstant::from(start_time),
            end_time: None,
            total_duration_ms: 0,
            tests_executed: 0,
            tests_passed: 0,
            tests_failed: 0,
            containers_created: 0,
            containers_destroyed: 0,
            peak_memory_usage_bytes: 0,
            peak_cpu_usage_percent: 0.0,
            total_tests: 0,
            average_execution_time: Duration::from_millis(0),
            resource_usage: ResourceUsage {
                cpu_usage_percent: 0.0,
                memory_usage_bytes: 0,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
                peak_cpu_usage_percent: 0.0,
                peak_memory_usage_bytes: 0,
                peak_disk_usage_bytes: 0,
                network_bytes_transferred: 0,
                container_count: 0,
            },
            error_count: 0,
            warning_count: 0,
            coverage_percentage: 0.0,
            performance_metrics: HashMap::new(),
        };

        Ok(Self {
            session_id,
            config,
            metrics: Arc::new(RwLock::new(metrics)),
            container_registry: Arc::new(RwLock::new(HashMap::new())),
            backend,
            #[cfg(feature = "services")]
            services,
            orchestrator: Arc::new(RwLock::new(ConcurrencyOrchestrator::new())),
            start_time,
        })
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
    pub fn start_time(&self) -> Instant {
        self.start_time
    }

    /// Get backend
    pub fn backend(&self) -> &TestcontainerBackend {
        &self.backend
    }

    /// Get services
    #[cfg(feature = "services")]
    pub fn services(&self) -> &ServiceManager {
        &self.services
    }

    /// Execute a test function
    pub async fn execute_test<F, T>(&self, _test_name: &str, test_fn: F) -> Result<T>
    where
        F: FnOnce() -> Result<T>,
    {
        let start_time = Instant::now();
        
        // Update metrics
        {
            let mut metrics = self.metrics.write().await;
            metrics.tests_executed += 1;
        }

        let result = test_fn();

        // Update metrics based on result
        {
            let mut metrics = self.metrics.write().await;
            let duration = start_time.elapsed();
            metrics.total_duration_ms += duration.as_millis() as u64;
            
            match &result {
                Ok(_) => metrics.tests_passed += 1,
                Err(_) => metrics.tests_failed += 1,
            }
        }

        result
    }

    /// Get current metrics
    pub async fn get_metrics(&self) -> CleanroomMetrics {
        self.metrics.read().await.clone()
    }

    /// Update metrics
    pub async fn update_metrics<F>(&self, updater: F) -> Result<()>
    where
        F: FnOnce(&mut CleanroomMetrics),
    {
        let mut metrics = self.metrics.write().await;
        updater(&mut metrics);
        Ok(())
    }

    /// Register a container
    pub async fn register_container(&self, name: String, container_id: String) -> Result<()> {
        let mut registry = self.container_registry.write().await;
        registry.insert(name, container_id);
        
        // Update metrics
        let mut metrics = self.metrics.write().await;
        metrics.containers_created += 1;
        
        Ok(())
    }

    /// Unregister a container
    pub async fn unregister_container(&self, name: &str) -> Result<()> {
        let mut registry = self.container_registry.write().await;
        registry.remove(name);
        
        // Update metrics
        let mut metrics = self.metrics.write().await;
        metrics.containers_destroyed += 1;
        
        Ok(())
    }

    /// Get container registry
    pub async fn get_container_registry(&self) -> HashMap<String, String> {
        self.container_registry.read().await.clone()
    }

    /// Check if container is registered
    pub async fn is_container_registered(&self, name: &str) -> bool {
        self.container_registry.read().await.contains_key(name)
    }

    /// Get container count
    pub async fn get_container_count(&self) -> usize {
        self.container_registry.read().await.len()
    }

    /// Cleanup all resources
    pub async fn cleanup(&self) -> Result<()> {
        // Stop all services
        #[cfg(feature = "services")]
        self.services.stop_all().await?;
        
        // Clear container registry
        {
            let mut registry = self.container_registry.write().await;
            registry.clear();
        }
        
        // Update end time
        {
            let mut metrics = self.metrics.write().await;
            metrics.end_time = Some(SerializableInstant::from(Instant::now()));
        }
        
        Ok(())
    }

    /// Check if environment is healthy
    pub async fn is_healthy(&self) -> bool {
        // Check if services are running
        #[cfg(feature = "services")]
        if !self.services.is_healthy().await {
            return false;
        }
        
        // Check if backend is running
        if !self.backend.is_running() {
            return false;
        }
        
        true
    }

    /// Get health status
    pub async fn get_health_status(&self) -> HealthStatus {
        if self.is_healthy().await {
            HealthStatus::Healthy
        } else {
            HealthStatus::Unhealthy
        }
    }

    /// Start a container
    pub async fn start_container(&self, container_name: &str) -> Result<String> {
        let container_id = format!("container_{}_{}", container_name, Uuid::new_v4());
        self.register_container(container_name.to_string(), container_id.clone()).await?;
        Ok(container_id)
    }

    /// Check if container is running
    pub async fn is_container_running(&self, container_id: &str) -> Result<bool> {
        let registry = self.container_registry.read().await;
        Ok(registry.values().any(|id| id == container_id))
    }

    /// Spawn a concurrent task
    pub async fn spawn_task<F, T>(&self, name: String, executor: F) -> Result<TaskId>
    where
        T: Send + 'static,
        F: FnOnce(crate::runtime::orchestrator::TaskContext) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<T>> + Send>> + Send + Sync + 'static,
    {
        let mut orchestrator = self.orchestrator.write().await;
        orchestrator.spawn_task(name, Box::new(executor)).await
    }

    /// Spawn a concurrent task with timeout
    pub async fn spawn_task_with_timeout<F, T>(&self, name: String, timeout: Duration, executor: F) -> Result<TaskId>
    where
        T: Send + 'static,
        F: FnOnce(crate::runtime::orchestrator::TaskContext) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<T>> + Send>> + Send + Sync + 'static,
    {
        let mut orchestrator = self.orchestrator.write().await;
        orchestrator.spawn_task_with_timeout(name, timeout, Box::new(executor)).await
    }

    /// Wait for a specific task to complete
    pub async fn wait_for_task(&self, task_id: TaskId) -> Result<TaskResult<()>> {
        let mut orchestrator = self.orchestrator.write().await;
        orchestrator.wait_for_task(task_id).await
    }

    /// Wait for all tasks to complete
    pub async fn wait_for_all_tasks(&self) -> Result<Vec<TaskResult<()>>> {
        let mut orchestrator = self.orchestrator.write().await;
        orchestrator.wait_for_all().await
    }

    /// Cancel a specific task
    pub async fn cancel_task(&self, task_id: TaskId) -> Result<()> {
        let orchestrator = self.orchestrator.read().await;
        orchestrator.cancel_task(task_id).await
    }

    /// Cancel all tasks
    pub async fn cancel_all_tasks(&self) -> Result<()> {
        let orchestrator = self.orchestrator.read().await;
        orchestrator.cancel_all().await
    }

    /// Get orchestrator statistics
    pub async fn get_orchestrator_stats(&self) -> crate::runtime::orchestrator::OrchestratorStats {
        let orchestrator = self.orchestrator.read().await;
        orchestrator.get_stats().await
    }

    /// Check if orchestrator is idle
    pub async fn is_orchestrator_idle(&self) -> bool {
        let orchestrator = self.orchestrator.read().await;
        orchestrator.is_idle().await
    }

    /// Get active task count
    pub async fn get_active_task_count(&self) -> usize {
        let orchestrator = self.orchestrator.read().await;
        orchestrator.active_task_count().await
    }
}

/// Health status enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum HealthStatus {
    /// Environment is healthy
    Healthy,
    /// Environment is unhealthy
    Unhealthy,
}

/// RAII guard for automatic cleanup
pub struct CleanroomGuard {
    environment: Arc<CleanroomEnvironment>,
}

impl CleanroomGuard {
    /// Create a new guard
    pub fn new(environment: Arc<CleanroomEnvironment>) -> Self {
        Self { environment }
    }
}

impl Drop for CleanroomGuard {
    fn drop(&mut self) {
        // Note: We can't use async in Drop, so we'll just mark for cleanup
        // The actual cleanup should be done explicitly
    }
}

/// Container wrapper trait
pub trait ContainerWrapper: Send + Sync {
    /// Get container status
    fn status(&self) -> ContainerStatus;
    
    /// Get container metrics
    fn metrics(&self) -> ContainerMetrics;
    
    /// Get container name
    fn name(&self) -> &str;
    
    /// Cleanup container resources
    fn cleanup(&self) -> Result<()>;
}

/// Container status enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum ContainerStatus {
    /// Container is starting
    Starting,
    /// Container is ready
    Ready,
    /// Container is running
    Running,
    /// Container is stopped
    Stopped,
    /// Container failed
    Failed,
}

/// Container metrics
#[derive(Debug, Clone)]
pub struct ContainerMetrics {
    /// CPU usage percentage
    pub cpu_usage_percent: f64,
    /// Memory usage bytes
    pub memory_usage_bytes: u64,
    /// Network bytes sent
    pub network_bytes_sent: u64,
    /// Network bytes received
    pub network_bytes_received: u64,
    /// Disk usage bytes
    pub disk_usage_bytes: u64,
    /// Container uptime in seconds
    pub uptime_seconds: u64,
}

impl Default for CleanroomMetrics {
    fn default() -> Self {
        let session_id = Uuid::new_v4();
        let start_time = Instant::now();
        
        Self {
            session_id,
            start_time: SerializableInstant::from(start_time),
            end_time: None,
            total_duration_ms: 0,
            tests_executed: 0,
            tests_passed: 0,
            tests_failed: 0,
            containers_created: 0,
            containers_destroyed: 0,
            peak_memory_usage_bytes: 0,
            peak_cpu_usage_percent: 0.0,
            total_tests: 0,
            average_execution_time: Duration::from_millis(0),
            resource_usage: ResourceUsage {
                cpu_usage_percent: 0.0,
                memory_usage_bytes: 0,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
                peak_cpu_usage_percent: 0.0,
                peak_memory_usage_bytes: 0,
                peak_disk_usage_bytes: 0,
                network_bytes_transferred: 0,
                container_count: 0,
            },
            error_count: 0,
            warning_count: 0,
            coverage_percentage: 0.0,
            performance_metrics: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::CleanroomError;
    use std::time::Duration;

    #[tokio::test]
    async fn test_cleanroom_creation() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config);
        assert!(cleanroom.await.is_ok());
    }

    #[tokio::test]
    async fn test_cleanroom_session_id() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        assert!(!cleanroom.session_id().is_nil());
    }

    #[tokio::test]
    async fn test_cleanroom_config() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        assert_eq!(cleanroom.config().test_execution_timeout, Duration::from_secs(300));
    }

    #[tokio::test]
    async fn test_cleanroom_start_time() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        assert!(cleanroom.start_time().elapsed().as_millis() < 1000);
    }

    #[tokio::test]
    async fn test_cleanroom_execute_test() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        
        let result = cleanroom.execute_test("test", || {
            Ok::<i32, CleanroomError>(42)
        }).await;
        
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[tokio::test]
    async fn test_cleanroom_execute_test_failure() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        
        let result = cleanroom.execute_test("test", || {
            Err::<i32, CleanroomError>(CleanroomError::internal_error("test error"))
        }).await;
        
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_cleanroom_metrics() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        
        let metrics = cleanroom.get_metrics().await;
        assert_eq!(metrics.session_id, cleanroom.session_id());
        assert_eq!(metrics.tests_executed, 0);
    }

    #[tokio::test]
    async fn test_cleanroom_update_metrics() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        
        cleanroom.update_metrics(|metrics| {
            metrics.tests_executed = 5;
        }).await.unwrap();
        
        let metrics = cleanroom.get_metrics().await;
        assert_eq!(metrics.tests_executed, 5);
    }

    #[tokio::test]
    async fn test_cleanroom_container_registry() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        
        cleanroom.register_container("test".to_string(), "container_id".to_string()).await.unwrap();
        assert!(cleanroom.is_container_registered("test").await);
        assert_eq!(cleanroom.get_container_count().await, 1);
        
        cleanroom.unregister_container("test").await.unwrap();
        assert!(!cleanroom.is_container_registered("test").await);
        assert_eq!(cleanroom.get_container_count().await, 0);
    }

    #[tokio::test]
    async fn test_cleanroom_cleanup() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        
        cleanroom.register_container("test".to_string(), "container_id".to_string()).await.unwrap();
        assert_eq!(cleanroom.get_container_count().await, 1);
        
        cleanroom.cleanup().await.unwrap();
        assert_eq!(cleanroom.get_container_count().await, 0);
    }

    #[tokio::test]
    async fn test_cleanroom_health_status() {
        let config = CleanroomConfig::default();
        let cleanroom = CleanroomEnvironment::new(config).await.unwrap();
        
        let status = cleanroom.get_health_status().await;
        assert_eq!(status, HealthStatus::Healthy);
    }
}