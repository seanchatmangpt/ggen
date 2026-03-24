//! Integration layer connecting all A2A/TPS systems
//!
//! This crate orchestrates the full pipeline:
//! firewall → packet → backpressure → a2a → jidoka → receipt
//!
//! # Architecture
//!
//! The SystemCoordinator manages lifecycle and coordination across:
//! - Firewall: Admission control through 3 ingress channels
//! - Packet: Work order validation and routing
//! - Backpressure: WIP limits and kanban flow control
//! - A2A: Task state machine protocol
//! - Jidoka: Quality gates and andon signals
//! - Receipt: Cryptographic operation verification
//!
//! # Example
//!
//! ```no_run
//! use ggen_integration::{SystemCoordinator, IntegrationConfig};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let config = IntegrationConfig::default();
//!     let mut coordinator = SystemCoordinator::new(config).await?;
//!
//!     coordinator.start().await?;
//!
//!     // System running...
//!
//!     coordinator.shutdown().await?;
//!     Ok(())
//! }
//! ```

#![forbid(unsafe_code)]
#![deny(missing_docs)]

pub mod health;
pub mod lifecycle;
pub mod pipeline;

use serde::{Deserialize, Serialize};
use thiserror::Error;

pub use health::{HealthCheck, HealthStatus, SubsystemHealth};
pub use lifecycle::{LifecycleError, LifecycleState};
pub use pipeline::{Pipeline, PipelineConfig, PipelineError};

/// Configuration for the integration layer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntegrationConfig {
    /// Pipeline configuration
    pub pipeline: PipelineConfig,
    /// Enable health checks
    pub enable_health_checks: bool,
    /// Health check interval in seconds
    pub health_check_interval_secs: u64,
    /// Graceful shutdown timeout in seconds
    pub shutdown_timeout_secs: u64,
}

impl Default for IntegrationConfig {
    fn default() -> Self {
        Self {
            pipeline: PipelineConfig::default(),
            enable_health_checks: true,
            health_check_interval_secs: 30,
            shutdown_timeout_secs: 60,
        }
    }
}

/// System coordinator managing all TPS subsystems
pub struct SystemCoordinator {
    config: IntegrationConfig,
    pipeline: Pipeline,
    health_check: HealthCheck,
    state: LifecycleState,
}

impl SystemCoordinator {
    /// Create a new system coordinator
    pub async fn new(config: IntegrationConfig) -> Result<Self> {
        let pipeline = Pipeline::new(config.pipeline.clone()).await?;
        let health_check = HealthCheck::new();

        Ok(Self {
            config,
            pipeline,
            health_check,
            state: LifecycleState::Stopped,
        })
    }

    /// Start the system coordinator
    pub async fn start(&mut self) -> Result<()> {
        if self.state != LifecycleState::Stopped {
            return Err(IntegrationError::InvalidStateTransition {
                from: self.state,
                to: LifecycleState::Starting,
            });
        }

        tracing::info!("Starting system coordinator");
        self.state = LifecycleState::Starting;

        // Start pipeline
        self.pipeline.start().await?;

        // Start health checks if enabled
        if self.config.enable_health_checks {
            self.health_check
                .start(std::time::Duration::from_secs(
                    self.config.health_check_interval_secs,
                ))
                .await?;
        }

        self.state = LifecycleState::Running;
        tracing::info!("System coordinator started successfully");

        Ok(())
    }

    /// Shutdown the system coordinator gracefully
    pub async fn shutdown(&mut self) -> Result<()> {
        if self.state != LifecycleState::Running {
            return Err(IntegrationError::InvalidStateTransition {
                from: self.state,
                to: LifecycleState::Stopping,
            });
        }

        tracing::info!("Shutting down system coordinator");
        self.state = LifecycleState::Stopping;

        // Stop health checks
        if self.config.enable_health_checks {
            self.health_check.stop().await?;
        }

        // Stop pipeline with timeout
        let timeout = std::time::Duration::from_secs(self.config.shutdown_timeout_secs);
        tokio::time::timeout(timeout, self.pipeline.shutdown())
            .await
            .map_err(|_| IntegrationError::ShutdownTimeout(timeout))??;

        self.state = LifecycleState::Stopped;
        tracing::info!("System coordinator shutdown complete");

        Ok(())
    }

    /// Get current lifecycle state
    #[must_use]
    pub const fn state(&self) -> LifecycleState {
        self.state
    }

    /// Get overall system health
    pub async fn health(&self) -> Result<HealthStatus> {
        self.health_check
            .check(&self.pipeline)
            .await
            .map_err(IntegrationError::Health)
    }

    /// Get pipeline reference (immutable)
    #[must_use]
    pub const fn pipeline(&self) -> &Pipeline {
        &self.pipeline
    }

    /// Get mutable pipeline reference
    pub fn pipeline_mut(&mut self) -> &mut Pipeline {
        &mut self.pipeline
    }
}

/// Integration errors
#[derive(Error, Debug)]
pub enum IntegrationError {
    /// Pipeline error
    #[error("Pipeline error: {0}")]
    Pipeline(#[from] PipelineError),

    /// Health check error
    #[error("Health check error: {0}")]
    Health(#[from] health::HealthError),

    /// Invalid state transition
    #[error("Invalid state transition from {from:?} to {to:?}")]
    InvalidStateTransition {
        /// Current state
        from: LifecycleState,
        /// Target state
        to: LifecycleState,
    },

    /// Shutdown timeout
    #[error("Shutdown timeout after {0:?}")]
    ShutdownTimeout(std::time::Duration),

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),
}

/// Result type for integration operations
pub type Result<T> = std::result::Result<T, IntegrationError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integration_config_default() {
        // Arrange & Act
        let config = IntegrationConfig::default();

        // Assert
        assert!(config.enable_health_checks);
        assert_eq!(config.health_check_interval_secs, 30);
        assert_eq!(config.shutdown_timeout_secs, 60);
    }

    #[tokio::test]
    async fn test_system_coordinator_creation() {
        // Arrange
        let config = IntegrationConfig::default();

        // Act
        let coordinator = SystemCoordinator::new(config).await;

        // Assert
        assert!(coordinator.is_ok());
        let coordinator = coordinator.ok().unwrap();
        assert_eq!(coordinator.state(), LifecycleState::Stopped);
    }

    #[tokio::test]
    async fn test_system_coordinator_invalid_state_transition() {
        // Arrange
        let config = IntegrationConfig::default();
        let mut coordinator = SystemCoordinator::new(config).await.ok().unwrap();

        // Act - attempt to shutdown when not running
        let result = coordinator.shutdown().await;

        // Assert
        assert!(result.is_err());
        if let Err(IntegrationError::InvalidStateTransition { from, to }) = result {
            assert_eq!(from, LifecycleState::Stopped);
            assert_eq!(to, LifecycleState::Stopping);
        }
    }
}
