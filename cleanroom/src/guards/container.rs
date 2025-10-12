//! Container-specific RAII guards
//!
//! This module provides specialized guards for container lifecycle management,
//! including health monitoring, resource tracking, and automatic cleanup.

use crate::error::Result;
use crate::cleanroom::CleanroomEnvironment;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;

/// Container health status
#[derive(Debug, Clone, PartialEq)]
pub enum ContainerHealth {
    /// Container is healthy
    Healthy,
    /// Container is unhealthy
    Unhealthy,
    /// Container health is unknown
    Unknown,
}

/// Container resource usage
#[derive(Debug, Clone)]
pub struct ContainerResources {
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
}

/// Enhanced container guard with health monitoring
pub struct ContainerHealthGuard {
    environment: Arc<CleanroomEnvironment>,
    container_id: String,
    health_status: Arc<RwLock<ContainerHealth>>,
    resource_usage: Arc<RwLock<ContainerResources>>,
    created_at: Instant,
    last_health_check: Arc<RwLock<Option<Instant>>>,
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
}

impl ContainerHealthGuard {
    /// Create a new container health guard
    pub fn new(environment: Arc<CleanroomEnvironment>, container_id: String) -> Self {
        Self {
            environment,
            container_id,
            health_status: Arc::new(RwLock::new(ContainerHealth::Unknown)),
            resource_usage: Arc::new(RwLock::new(ContainerResources {
                cpu_usage_percent: 0.0,
                memory_usage_bytes: 0,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            })),
            created_at: Instant::now(),
            last_health_check: Arc::new(RwLock::new(None)),
            cleanup_actions: Vec::new(),
        }
    }

    /// Add a cleanup action
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get the container ID
    pub fn container_id(&self) -> &str {
        &self.container_id
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Get the current health status
    pub async fn health_status(&self) -> ContainerHealth {
        self.health_status.read().await.clone()
    }

    /// Get the current resource usage
    pub async fn resource_usage(&self) -> ContainerResources {
        self.resource_usage.read().await.clone()
    }

    /// Check if the container is healthy
    pub async fn is_healthy(&self) -> bool {
        matches!(self.health_status().await, ContainerHealth::Healthy)
    }

    /// Update health status
    pub async fn update_health_status(&self, status: ContainerHealth) {
        let mut health = self.health_status.write().await;
        *health = status;
        
        let mut last_check = self.last_health_check.write().await;
        *last_check = Some(Instant::now());
    }

    /// Update resource usage
    pub async fn update_resource_usage(&self, resources: ContainerResources) {
        let mut usage = self.resource_usage.write().await;
        *usage = resources;
    }

    /// Get the last health check time
    pub async fn last_health_check(&self) -> Option<Instant> {
        self.last_health_check.read().await.clone()
    }

    /// Perform a health check
    pub async fn perform_health_check(&self) -> Result<ContainerHealth> {
        // Simulate health check - in real implementation, this would
        // interact with the actual container
        let is_registered = self.environment.is_container_registered(&self.container_id).await;
        
        let status = if is_registered {
            ContainerHealth::Healthy
        } else {
            ContainerHealth::Unhealthy
        };
        
        self.update_health_status(status.clone()).await;
        Ok(status)
    }

    /// Get container uptime
    pub fn uptime(&self) -> std::time::Duration {
        self.created_at.elapsed()
    }

    /// Check if the container is still registered
    pub async fn is_registered(&self) -> bool {
        self.environment.is_container_registered(&self.container_id).await
    }

    /// Manually trigger cleanup
    pub async fn cleanup(self) -> Result<()> {
        // Execute custom cleanup actions
        for action in self.cleanup_actions {
            action();
        }

        // Unregister container
        self.environment.unregister_container(&self.container_id).await?;
        
        Ok(())
    }
}

impl Drop for ContainerHealthGuard {
    fn drop(&mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }

        // Attempt to unregister container (best effort)
        let environment = self.environment.clone();
        let container_id = self.container_id.clone();
        tokio::spawn(async move {
            let _ = environment.unregister_container(&container_id).await;
        });
    }
}

/// Container resource guard with usage tracking
pub struct ContainerResourceGuard {
    environment: Arc<CleanroomEnvironment>,
    container_id: String,
    max_cpu_percent: f64,
    max_memory_bytes: u64,
    max_disk_bytes: u64,
    created_at: Instant,
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
}

impl ContainerResourceGuard {
    /// Create a new container resource guard
    pub fn new(
        environment: Arc<CleanroomEnvironment>,
        container_id: String,
        max_cpu_percent: f64,
        max_memory_bytes: u64,
        max_disk_bytes: u64,
    ) -> Self {
        Self {
            environment,
            container_id,
            max_cpu_percent,
            max_memory_bytes,
            max_disk_bytes,
            created_at: Instant::now(),
            cleanup_actions: Vec::new(),
        }
    }

    /// Add a cleanup action
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get the container ID
    pub fn container_id(&self) -> &str {
        &self.container_id
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Get resource limits
    pub fn resource_limits(&self) -> (f64, u64, u64) {
        (self.max_cpu_percent, self.max_memory_bytes, self.max_disk_bytes)
    }

    /// Check if resource usage is within limits
    pub async fn check_resource_limits(&self, resources: &ContainerResources) -> bool {
        resources.cpu_usage_percent <= self.max_cpu_percent
            && resources.memory_usage_bytes <= self.max_memory_bytes
            && resources.disk_usage_bytes <= self.max_disk_bytes
    }

    /// Get container uptime
    pub fn uptime(&self) -> std::time::Duration {
        self.created_at.elapsed()
    }

    /// Check if the container is still registered
    pub async fn is_registered(&self) -> bool {
        self.environment.is_container_registered(&self.container_id).await
    }

    /// Manually trigger cleanup
    pub async fn cleanup(self) -> Result<()> {
        // Execute custom cleanup actions
        for action in self.cleanup_actions {
            action();
        }

        // Unregister container
        self.environment.unregister_container(&self.container_id).await?;
        
        Ok(())
    }
}

impl Drop for ContainerResourceGuard {
    fn drop(&mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }

        // Attempt to unregister container (best effort)
        let environment = self.environment.clone();
        let container_id = self.container_id.clone();
        tokio::spawn(async move {
            let _ = environment.unregister_container(&container_id).await;
        });
    }
}

/// Container lifecycle guard with automatic health monitoring
pub struct ContainerLifecycleGuard {
    environment: Arc<CleanroomEnvironment>,
    container_id: String,
    health_check_interval: std::time::Duration,
    health_check_task: Option<tokio::task::JoinHandle<()>>,
    created_at: Instant,
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
}

impl ContainerLifecycleGuard {
    /// Create a new container lifecycle guard
    pub fn new(
        environment: Arc<CleanroomEnvironment>,
        container_id: String,
        health_check_interval: std::time::Duration,
    ) -> Self {
        Self {
            environment,
            container_id,
            health_check_interval,
            health_check_task: None,
            created_at: Instant::now(),
            cleanup_actions: Vec::new(),
        }
    }

    /// Start health monitoring
    pub fn start_health_monitoring(mut self) -> Self {
        let environment = self.environment.clone();
        let container_id = self.container_id.clone();
        let interval = self.health_check_interval;
        
        let task = tokio::spawn(async move {
            let mut interval_timer = tokio::time::interval(interval);
            
            loop {
                interval_timer.tick().await;
                
                // Perform health check
                let is_registered = environment.is_container_registered(&container_id).await;
                if !is_registered {
                    println!("Container {} is no longer registered, stopping health monitoring", container_id);
                    break;
                }
                
                // In a real implementation, this would perform actual health checks
                println!("Health check for container {}: OK", container_id);
            }
        });
        
        self.health_check_task = Some(task);
        self
    }

    /// Add a cleanup action
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get the container ID
    pub fn container_id(&self) -> &str {
        &self.container_id
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Get container uptime
    pub fn uptime(&self) -> std::time::Duration {
        self.created_at.elapsed()
    }

    /// Check if the container is still registered
    pub async fn is_registered(&self) -> bool {
        self.environment.is_container_registered(&self.container_id).await
    }

    /// Manually trigger cleanup
    pub async fn cleanup(mut self) -> Result<()> {
        // Stop health monitoring task
        if let Some(task) = self.health_check_task.take() {
            task.abort();
        }

        // Execute custom cleanup actions
        for action in self.cleanup_actions {
            action();
        }

        // Unregister container
        self.environment.unregister_container(&self.container_id).await?;
        
        Ok(())
    }
}

impl Drop for ContainerLifecycleGuard {
    fn drop(&mut self) {
        // Stop health monitoring task
        if let Some(task) = self.health_check_task.take() {
            task.abort();
        }

        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }

        // Attempt to unregister container (best effort)
        let environment = self.environment.clone();
        let container_id = self.container_id.clone();
        tokio::spawn(async move {
            let _ = environment.unregister_container(&container_id).await;
        });
    }
}

/// Convenience function to create a container health guard
pub fn container_health_guard(
    environment: Arc<CleanroomEnvironment>,
    container_id: String,
) -> ContainerHealthGuard {
    ContainerHealthGuard::new(environment, container_id)
}

/// Convenience function to create a container resource guard
pub fn container_resource_guard(
    environment: Arc<CleanroomEnvironment>,
    container_id: String,
    max_cpu_percent: f64,
    max_memory_bytes: u64,
    max_disk_bytes: u64,
) -> ContainerResourceGuard {
    ContainerResourceGuard::new(environment, container_id, max_cpu_percent, max_memory_bytes, max_disk_bytes)
}

/// Convenience function to create a container lifecycle guard
pub fn container_lifecycle_guard(
    environment: Arc<CleanroomEnvironment>,
    container_id: String,
    health_check_interval: std::time::Duration,
) -> ContainerLifecycleGuard {
    ContainerLifecycleGuard::new(environment, container_id, health_check_interval)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::CleanroomConfig;
    use std::time::Duration;

    #[tokio::test]
    async fn test_container_health_guard() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let environment_arc = Arc::new(environment);
        
        let container_id = "test-container".to_string();
        let guard = ContainerHealthGuard::new(environment_arc.clone(), container_id.clone());
        
        assert_eq!(guard.container_id(), "test-container");
        assert!(guard.is_registered().await);
        
        let health = guard.perform_health_check().await.unwrap();
        assert_eq!(health, ContainerHealth::Healthy);
        
        assert!(guard.is_healthy().await);
    }

    #[tokio::test]
    async fn test_container_resource_guard() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let environment_arc = Arc::new(environment);
        
        let container_id = "test-container".to_string();
        let guard = ContainerResourceGuard::new(
            environment_arc.clone(),
            container_id.clone(),
            80.0,
            1024 * 1024 * 1024, // 1GB
            10 * 1024 * 1024 * 1024, // 10GB
        );
        
        assert_eq!(guard.container_id(), "test-container");
        
        let (cpu, memory, disk) = guard.resource_limits();
        assert_eq!(cpu, 80.0);
        assert_eq!(memory, 1024 * 1024 * 1024);
        assert_eq!(disk, 10 * 1024 * 1024 * 1024);
        
        let resources = ContainerResources {
            cpu_usage_percent: 50.0,
            memory_usage_bytes: 512 * 1024 * 1024,
            disk_usage_bytes: 5 * 1024 * 1024 * 1024,
            network_bytes_sent: 1000,
            network_bytes_received: 2000,
        };
        
        assert!(guard.check_resource_limits(&resources).await);
    }

    #[tokio::test]
    async fn test_container_lifecycle_guard() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let environment_arc = Arc::new(environment);
        
        let container_id = "test-container".to_string();
        let guard = ContainerLifecycleGuard::new(
            environment_arc.clone(),
            container_id.clone(),
            Duration::from_millis(100),
        ).start_health_monitoring();
        
        assert_eq!(guard.container_id(), "test-container");
        assert!(guard.is_registered().await);
        
        // Let it run for a bit
        tokio::time::sleep(Duration::from_millis(250)).await;
        
        // Guard will be dropped here, stopping health monitoring
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let environment_arc = Arc::new(environment);
        
        let _health_guard = container_health_guard(environment_arc.clone(), "test".to_string());
        let _resource_guard = container_resource_guard(
            environment_arc.clone(),
            "test".to_string(),
            80.0,
            1024 * 1024 * 1024,
            10 * 1024 * 1024 * 1024,
        );
        let _lifecycle_guard = container_lifecycle_guard(
            environment_arc,
            "test".to_string(),
            Duration::from_millis(100),
        );
        
        // All guards will be dropped here, triggering cleanup
    }
}
