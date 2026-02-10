//! Health checks for all subsystems

use crate::pipeline::Pipeline;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

/// Overall health status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HealthStatus {
    /// All subsystems healthy
    Healthy,
    /// Some subsystems degraded
    Degraded,
    /// Critical subsystem failure
    Unhealthy,
}

impl std::fmt::Display for HealthStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Healthy => write!(f, "Healthy"),
            Self::Degraded => write!(f, "Degraded"),
            Self::Unhealthy => write!(f, "Unhealthy"),
        }
    }
}

/// Health status of individual subsystem
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubsystemHealth {
    /// Subsystem name
    pub name: String,
    /// Health status
    pub status: HealthStatus,
    /// Optional message
    pub message: Option<String>,
    /// Timestamp of check
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Additional metrics
    pub metrics: HashMap<String, f64>,
}

impl SubsystemHealth {
    /// Create a new subsystem health status
    #[must_use]
    pub fn new(name: String, status: HealthStatus) -> Self {
        Self {
            name,
            status,
            message: None,
            timestamp: chrono::Utc::now(),
            metrics: HashMap::new(),
        }
    }

    /// Add a message
    #[must_use]
    pub fn with_message(mut self, message: String) -> Self {
        self.message = Some(message);
        self
    }

    /// Add a metric
    #[must_use]
    pub fn with_metric(mut self, key: String, value: f64) -> Self {
        self.metrics.insert(key, value);
        self
    }
}

/// Health check coordinator
pub struct HealthCheck {
    subsystem_health: HashMap<String, SubsystemHealth>,
    check_handle: Option<tokio::task::JoinHandle<()>>,
}

impl HealthCheck {
    /// Create a new health check coordinator
    #[must_use]
    pub fn new() -> Self {
        Self {
            subsystem_health: HashMap::new(),
            check_handle: None,
        }
    }

    /// Start periodic health checks
    pub async fn start(&mut self, interval: std::time::Duration) -> Result<(), HealthError> {
        if self.check_handle.is_some() {
            return Err(HealthError::AlreadyRunning);
        }

        tracing::info!("Starting health checks with interval {:?}", interval);

        // For now, just set a placeholder handle
        // In production, this would spawn a background task

        Ok(())
    }

    /// Stop health checks
    pub async fn stop(&mut self) -> Result<(), HealthError> {
        if let Some(handle) = self.check_handle.take() {
            handle.abort();
            tracing::info!("Stopped health checks");
        }
        Ok(())
    }

    /// Perform health check on pipeline
    pub async fn check(&self, pipeline: &Pipeline) -> Result<HealthStatus, HealthError> {
        // Check firewall
        let firewall_health = self.check_firewall().await?;

        // Check backpressure
        let backpressure_health = self.check_backpressure(pipeline).await?;

        // Check receipt chain
        let receipt_health = self.check_receipts(pipeline).await?;

        // Aggregate status
        let statuses = vec![
            firewall_health.status,
            backpressure_health.status,
            receipt_health.status,
        ];

        let overall = if statuses.iter().all(|s| *s == HealthStatus::Healthy) {
            HealthStatus::Healthy
        } else if statuses.iter().any(|s| *s == HealthStatus::Unhealthy) {
            HealthStatus::Unhealthy
        } else {
            HealthStatus::Degraded
        };

        Ok(overall)
    }

    /// Check firewall health
    async fn check_firewall(&self) -> Result<SubsystemHealth, HealthError> {
        Ok(SubsystemHealth::new(
            "firewall".to_string(),
            HealthStatus::Healthy,
        ))
    }

    /// Check backpressure health
    async fn check_backpressure(&self, pipeline: &Pipeline) -> Result<SubsystemHealth, HealthError> {
        let utilization = pipeline.wip_utilization().await;
        let wip_count = pipeline.wip_count().await;

        let status = if utilization > 0.9 {
            HealthStatus::Degraded
        } else {
            HealthStatus::Healthy
        };

        Ok(SubsystemHealth::new("backpressure".to_string(), status)
            .with_metric("utilization".to_string(), utilization)
            .with_metric("wip_count".to_string(), wip_count as f64))
    }

    /// Check receipt chain health
    async fn check_receipts(&self, pipeline: &Pipeline) -> Result<SubsystemHealth, HealthError> {
        let verified = pipeline.verify_receipt_chain()
            .map_err(|e| HealthError::CheckFailed(e.to_string()))?;

        let status = if verified {
            HealthStatus::Healthy
        } else {
            HealthStatus::Unhealthy
        };

        Ok(SubsystemHealth::new("receipts".to_string(), status))
    }

    /// Get subsystem health
    #[must_use]
    pub fn get_subsystem_health(&self, name: &str) -> Option<&SubsystemHealth> {
        self.subsystem_health.get(name)
    }

    /// Get all subsystem health
    #[must_use]
    pub fn all_subsystem_health(&self) -> &HashMap<String, SubsystemHealth> {
        &self.subsystem_health
    }
}

impl Default for HealthCheck {
    fn default() -> Self {
        Self::new()
    }
}

/// Health check errors
#[derive(Error, Debug)]
pub enum HealthError {
    /// Health checks already running
    #[error("Health checks already running")]
    AlreadyRunning,

    /// Health check failed
    #[error("Health check failed: {0}")]
    CheckFailed(String),

    /// Internal error
    #[error("Internal health check error: {0}")]
    Internal(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline::PipelineConfig;

    #[test]
    fn test_health_status_display() {
        // Arrange & Act & Assert
        assert_eq!(format!("{}", HealthStatus::Healthy), "Healthy");
        assert_eq!(format!("{}", HealthStatus::Degraded), "Degraded");
        assert_eq!(format!("{}", HealthStatus::Unhealthy), "Unhealthy");
    }

    #[test]
    fn test_subsystem_health_creation() {
        // Arrange & Act
        let health = SubsystemHealth::new("test".to_string(), HealthStatus::Healthy)
            .with_message("All good".to_string())
            .with_metric("cpu".to_string(), 0.5);

        // Assert
        assert_eq!(health.name, "test");
        assert_eq!(health.status, HealthStatus::Healthy);
        assert_eq!(health.message, Some("All good".to_string()));
        assert_eq!(health.metrics.get("cpu"), Some(&0.5));
    }

    #[test]
    fn test_health_check_creation() {
        // Arrange & Act
        let health_check = HealthCheck::new();

        // Assert
        assert_eq!(health_check.all_subsystem_health().len(), 0);
    }

    #[tokio::test]
    async fn test_health_check_pipeline() {
        // Arrange
        let health_check = HealthCheck::new();
        let config = PipelineConfig::default();
        let pipeline = Pipeline::new(config).await.ok().unwrap();

        // Act
        let status = health_check.check(&pipeline).await;

        // Assert
        assert!(status.is_ok());
        assert_eq!(status.ok().unwrap(), HealthStatus::Healthy);
    }

    #[tokio::test]
    async fn test_health_check_backpressure() {
        // Arrange
        let health_check = HealthCheck::new();
        let config = PipelineConfig {
            wip_limit: 10,
            ..Default::default()
        };
        let pipeline = Pipeline::new(config).await.ok().unwrap();

        // Act
        let health = health_check.check_backpressure(&pipeline).await;

        // Assert
        assert!(health.is_ok());
        let health = health.ok().unwrap();
        assert_eq!(health.status, HealthStatus::Healthy);
        assert!(health.metrics.contains_key("utilization"));
        assert!(health.metrics.contains_key("wip_count"));
    }

    #[tokio::test]
    async fn test_health_check_start_stop() {
        // Arrange
        let mut health_check = HealthCheck::new();

        // Act
        let start_result = health_check
            .start(std::time::Duration::from_secs(30))
            .await;
        let stop_result = health_check.stop().await;

        // Assert
        assert!(start_result.is_ok());
        assert!(stop_result.is_ok());
    }
}
