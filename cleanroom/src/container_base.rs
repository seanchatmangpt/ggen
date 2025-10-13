//! Base container struct for reducing repetition
//!
//! This module provides a base container struct that eliminates repetitive
//! field definitions across different container types.

use crate::cleanroom::{ContainerMetrics, ContainerStatus};
use crate::error::Result;
use crate::policy::Policy;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;

/// Base container fields shared across all container types
///
/// This struct contains the common fields used by all container implementations,
/// eliminating repetitive field definitions.
#[derive(Debug)]
pub struct ContainerBase {
    /// Container status
    pub status: Arc<RwLock<ContainerStatus>>,
    /// Container metrics
    pub metrics: Arc<RwLock<ContainerMetrics>>,
    /// Security policy
    pub policy: Policy,
    /// Container start time
    pub start_time: Instant,
}

impl ContainerBase {
    /// Create a new container base with default values
    pub fn new() -> Self {
        Self {
            status: Arc::new(RwLock::new(ContainerStatus::Starting)),
            metrics: Arc::new(RwLock::new(ContainerMetrics::default())),
            policy: Policy::default(),
            start_time: Instant::now(),
        }
    }

    /// Create a new container base with custom policy
    pub fn with_policy(policy: Policy) -> Self {
        Self {
            status: Arc::new(RwLock::new(ContainerStatus::Starting)),
            metrics: Arc::new(RwLock::new(ContainerMetrics::default())),
            policy,
            start_time: Instant::now(),
        }
    }

    /// Get container status
    pub async fn get_status(&self) -> ContainerStatus {
        let status = self.status.read().await;
        status.clone()
    }

    /// Set container status
    pub async fn set_status(&self, status: ContainerStatus) -> Result<()> {
        let mut current_status = self.status.write().await;
        *current_status = status;
        Ok(())
    }

    /// Get container metrics
    pub async fn get_metrics(&self) -> ContainerMetrics {
        let metrics = self.metrics.read().await;
        metrics.clone()
    }

    /// Update container metrics
    pub async fn update_metrics<F>(&self, updater: F) -> Result<()>
    where
        F: FnOnce(&mut ContainerMetrics),
    {
        let mut metrics = self.metrics.write().await;
        updater(&mut metrics);
        Ok(())
    }

    /// Get container uptime in seconds
    pub fn uptime_seconds(&self) -> u64 {
        self.start_time.elapsed().as_secs()
    }

    /// Check if container is running
    pub async fn is_running(&self) -> bool {
        matches!(self.get_status().await, ContainerStatus::Running)
    }

    /// Check if container is ready
    pub async fn is_ready(&self) -> bool {
        matches!(self.get_status().await, ContainerStatus::Ready)
    }

    /// Wait for container to be ready
    pub async fn wait_for_ready(&self, timeout: std::time::Duration) -> Result<()> {
        let start = Instant::now();
        
        while start.elapsed() < timeout {
            if self.is_ready().await {
                return Ok(());
            }
            
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        }
        
        Err(crate::error_helpers::timeout_error(
            "Container failed to become ready",
            Some("Timeout waiting for container readiness"),
        ))
    }

    /// Update uptime in metrics
    pub async fn update_uptime(&self) -> Result<()> {
        self.update_metrics(|metrics| {
            metrics.uptime_seconds = self.uptime_seconds();
        }).await
    }
}

impl Default for ContainerBase {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait for containers that use the base implementation
///
/// This trait provides common functionality for containers that embed
/// the ContainerBase struct.
pub trait BaseContainer {
    /// Get reference to the base container
    fn base(&self) -> &ContainerBase;

    /// Get mutable reference to the base container
    fn base_mut(&mut self) -> &mut ContainerBase;

    /// Get container name (to be implemented by specific containers)
    fn container_name(&self) -> &str;

    /// Get container status
    async fn get_status(&self) -> ContainerStatus {
        self.base().get_status().await
    }

    /// Set container status
    async fn set_status(&self, status: ContainerStatus) -> Result<()> {
        self.base().set_status(status).await
    }

    /// Get container metrics
    async fn get_metrics(&self) -> ContainerMetrics {
        self.base().get_metrics().await
    }

    /// Update container metrics
    async fn update_metrics<F>(&self, updater: F) -> Result<()>
    where
        F: FnOnce(&mut ContainerMetrics),
    {
        self.base().update_metrics(updater).await
    }

    /// Get container uptime
    fn uptime_seconds(&self) -> u64 {
        self.base().uptime_seconds()
    }

    /// Check if container is running
    async fn is_running(&self) -> bool {
        self.base().is_running().await
    }

    /// Check if container is ready
    async fn is_ready(&self) -> bool {
        self.base().is_ready().await
    }

    /// Wait for container to be ready
    async fn wait_for_ready(&self, timeout: std::time::Duration) -> Result<()> {
        self.base().wait_for_ready(timeout).await
    }

    /// Update uptime in metrics
    async fn update_uptime(&self) -> Result<()> {
        self.base().update_uptime().await
    }
}

/// Macro to implement BaseContainer trait
///
/// This macro eliminates repetitive BaseContainer implementations
/// by providing the common functionality.
///
/// # Usage
///
/// ```rust
/// impl_base_container!(PostgresContainer, base, "postgres");
/// ```
#[macro_export]
macro_rules! impl_base_container {
    ($container_type:ident, $base_field:ident, $name:expr) => {
        impl BaseContainer for $container_type {
            fn base(&self) -> &ContainerBase {
                &self.$base_field
            }

            fn base_mut(&mut self) -> &mut ContainerBase {
                &mut self.$base_field
            }

            fn container_name(&self) -> &str {
                $name
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_container_base() {
        let base = ContainerBase::new();
        
        assert_eq!(base.get_status().await, ContainerStatus::Starting);
        assert_eq!(base.uptime_seconds(), 0);
        
        base.set_status(ContainerStatus::Running).await.unwrap();
        assert_eq!(base.get_status().await, ContainerStatus::Running);
        assert!(base.is_running().await);
    }

    #[tokio::test]
    async fn test_metrics_update() {
        let base = ContainerBase::new();
        
        base.update_metrics(|metrics| {
            metrics.cpu_usage_percent = 5.0;
            metrics.memory_usage_bytes = 128 * 1024 * 1024;
        }).await.unwrap();
        
        let metrics = base.get_metrics().await;
        assert_eq!(metrics.cpu_usage_percent, 5.0);
        assert_eq!(metrics.memory_usage_bytes, 128 * 1024 * 1024);
    }

    #[tokio::test]
    async fn test_uptime_update() {
        let base = ContainerBase::new();
        
        // Wait a bit to ensure uptime > 0
        tokio::time::sleep(Duration::from_millis(10)).await;
        
        base.update_uptime().await.unwrap();
        let metrics = base.get_metrics().await;
        assert!(metrics.uptime_seconds > 0);
    }
}
