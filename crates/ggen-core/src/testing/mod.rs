//! Testing utilities for ggen
//!
//! This module provides comprehensive testing infrastructure including:
//! - Chaos engineering and failure injection capabilities
//! - Docker container management via testcontainers
//! - Health check mechanisms with configurable timeouts
//! - Port mapping and discovery
//! - Automatic cleanup on drop
//! - Erlang distributed cluster management
//!
//! ## Modules
//!
//! - [`chaos`] - Chaos engineering scenarios and execution
//! - [`failure_injector`] - Container failure injection utilities
//! - [`docker_client`] - Docker CLI wrapper for container management
//! - [`testcontainers`] - Testcontainers integration for production validation
//! - [`erlang_cluster`] - Erlang distributed cluster management

pub mod chaos;
pub mod docker_client;
pub mod erlang_cluster;
pub mod failure_injector;
pub mod testcontainers;

// Re-export commonly used types
pub use chaos::{ChaosExecutor, ChaosScenario, RecoveryResult};
pub use docker_client::{DockerClient, DockerCommand};
pub use erlang_cluster::{ClusterMetrics, ErlangClusterManager, ErlangNode, MessagingMetrics};
pub use failure_injector::{FailureInjector, InjectionResult};
pub use testcontainers::{ContainerConfig, ContainerManager, HealthCheck};
