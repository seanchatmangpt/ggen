//! Cleanroom Testing Framework - Core Team Best Practices Implementation
//!
//! This crate provides a production-ready cleanroom testing framework using testcontainers
//! following core team best practices for reliability, performance, and maintainability.
//!
//! ## Core Team Best Practices Implemented
//!
//! - **Standardized testcontainers version (0.22)** across all projects
//! - **Singleton container pattern** for performance optimization
//! - **Container customizers** for flexible configuration
//! - **Proper lifecycle management** with RAII
//! - **Resource cleanup and error handling**
//! - **Performance monitoring and metrics collection**
//! - **Security boundaries and isolation**
//! - **Deterministic execution with fixed seeds**
//!
//! ## Quick Start
//!
//! ```rust
//! use cleanroom::{CleanroomEnvironment, CleanroomConfig, PostgresContainer, RedisContainer};
//! use std::time::Duration;
//!
//! #[tokio::test]
//! async fn test_my_application() {
//!     // Create cleanroom environment with best practices
//!     let config = CleanroomConfig::default();
//!     let environment = CleanroomEnvironment::new(config).await.unwrap();
//!
//!     // Get or create PostgreSQL container (singleton pattern)
//!     let postgres = environment.get_or_create_container("postgres", || {
//!         PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
//!     }).await.unwrap();
//!
//!     // Wait for container to be ready
//!     postgres.wait_for_ready().await.unwrap();
//!
//!     // Execute test with proper lifecycle management
//!     let result = environment.execute_test("database_test", || {
//!         // Your test logic here
//!         Ok("test_passed")
//!     }).await.unwrap();
//!
//!     // Cleanup is automatic via RAII
//! }
//! ```
//!
//! ## Features
//!
//! - **Singleton Containers**: Start containers once per test suite for performance
//! - **Resource Monitoring**: Track CPU, memory, disk, and network usage
//! - **Security Isolation**: Network, filesystem, and process isolation
//! - **Deterministic Execution**: Fixed seeds for reproducible tests
//! - **Coverage Tracking**: Track test coverage and execution paths
//! - **Snapshot Testing**: Capture and compare test outputs
//! - **Tracing & Observability**: Detailed tracing and metrics collection
//! - **Error Handling**: Comprehensive error handling and recovery
//! - **Performance Monitoring**: Real-time performance monitoring and alerting
//!
//! ## Architecture
//!
//! ```mermaid
//! graph TB
//!     A[CleanroomEnvironment] --> B[ContainerRegistry]
//!     A --> C[PolicyEnforcement]
//!     A --> D[DeterministicManager]
//!     A --> E[CoverageTracker]
//!     A --> F[SnapshotManager]
//!     A --> G[TracingManager]
//!     A --> H[ResourceLimits]
//!     A --> I[RedactionManager]
//!     A --> J[TestReport]
//!     
//!     B --> K[PostgresContainer]
//!     B --> L[RedisContainer]
//!     B --> M[GenericContainer]
//!     
//!     K --> N[ContainerWrapper]
//!     L --> N
//!     M --> N
//! ```

pub mod cleanroom;
pub mod containers;
pub mod error;
pub mod policy;
pub mod determinism;
pub mod coverage;
pub mod snapshots;
pub mod tracing;
pub mod limits;
pub mod redaction;
pub mod report;
pub mod backend;
pub mod services;
pub mod scenario;
pub mod runtime;
pub mod config;
pub mod skip;
pub mod artifacts;
pub mod assertions;
pub mod attest;
pub mod prelude;

// Re-export main types for convenience
pub use cleanroom::{
    CleanroomEnvironment,
    CleanroomConfig,
    CleanroomGuard,
    ContainerWrapper,
    ContainerStatus,
    ContainerMetrics,
    SecurityPolicy,
    PerformanceMonitoringConfig,
    PerformanceThresholds,
    ContainerCustomizer,
    VolumeMount,
    PortMapping,
    ContainerResourceLimits,
    HealthCheckConfig,
    CleanroomMetrics,
    ResourceUsage,
};

pub use containers::{
    PostgresContainer,
    RedisContainer,
    GenericContainer,
};

pub use error::{Result, CleanroomError};
pub use policy::Policy;
pub use determinism::DeterministicManager;
pub use coverage::CoverageTracker;
pub use snapshots::SnapshotManager;
pub use tracing::TracingManager;
pub use limits::ResourceLimits;
pub use redaction::RedactionManager;
pub use report::TestReport;

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Create a new cleanroom environment with default configuration
pub async fn new_cleanroom() -> Result<CleanroomEnvironment> {
    CleanroomEnvironment::new(CleanroomConfig::default()).await
}

/// Create a new cleanroom environment with custom configuration
pub async fn new_cleanroom_with_config(config: CleanroomConfig) -> Result<CleanroomEnvironment> {
    CleanroomEnvironment::new(config).await
}

/// Create a cleanroom guard for automatic cleanup
pub fn create_cleanroom_guard(environment: Arc<CleanroomEnvironment>) -> CleanroomGuard {
    CleanroomGuard::new(environment)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_cleanroom_creation() {
        let environment = new_cleanroom().await;
        assert!(environment.is_ok());
    }
    
    #[tokio::test]
    async fn test_cleanroom_with_config() {
        let config = CleanroomConfig::default();
        let environment = new_cleanroom_with_config(config).await;
        assert!(environment.is_ok());
    }
}