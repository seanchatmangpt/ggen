//! Service fixtures for testing with external dependencies
//!
//! Provides typed handles for common services like PostgreSQL and Redis
//! with health checks, connection info, and RAII teardown.

use crate::error::Result;

pub mod postgres;
pub mod redis;

/// Base trait for all service fixtures
pub trait Service: Send + Sync {
    /// Service name for diagnostics
    fn name(&self) -> &str;

    /// Check if service is healthy and ready
    fn health_check(&self) -> Result<bool>;

    /// Get connection information for tests
    fn connection_info(&self) -> Result<ConnectionInfo>;

    /// Start the service
    fn start(&mut self) -> Result<()>;

    /// Stop the service
    fn stop(&mut self) -> Result<()>;
}

/// Connection information for services
#[derive(Debug, Clone)]
pub struct ConnectionInfo {
    /// Host address
    pub host: String,
    /// Port number
    pub port: u16,
    /// Additional connection parameters
    pub params: std::collections::HashMap<String, String>,
}

// Re-export service implementations
pub use postgres::Postgres;
pub use redis::Redis;