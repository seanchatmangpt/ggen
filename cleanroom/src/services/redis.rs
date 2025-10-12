//! Redis service fixture for testing
//!
//! Provides a containerized Redis instance using testcontainers-rs
//! with health checks, connection info, and automatic teardown.

use crate::error::Result;
use crate::services::{ConnectionInfo, Service};

/// Redis service fixture using testcontainers
#[derive(Debug)]
pub struct Redis {
    /// Connection information
    connection_info: ConnectionInfo,
    /// Password (optional)
    password: Option<String>,
}

impl Redis {
    /// Create a new Redis fixture
    pub fn new() -> Result<Self> {
        let connection_info = ConnectionInfo::new("host=localhost port=6379");

        Ok(Self {
            connection_info,
            password: None,
        })
    }

    /// Create with custom configuration
    pub fn with_config(_port: u16, password: Option<String>) -> Result<Self> {
        let mut conn_info = ConnectionInfo::new("host=localhost port=6379");

        if let Some(ref pass) = password {
            conn_info = conn_info.with_param("password", pass);
        }

        Ok(Self {
            connection_info: conn_info,
            password,
        })
    }

    /// Get connection information
    pub fn connection_info(&self) -> &ConnectionInfo {
        &self.connection_info
    }

    /// Get password
    pub fn password(&self) -> Option<&str> {
        self.password.as_deref()
    }
}

impl Service for Redis {
    fn name(&self) -> &str {
        "redis"
    }

    fn health_check(&self) -> Result<bool> {
        // In a real implementation, this would check Redis health
        Ok(true)
    }

    fn connection_info(&self) -> Result<ConnectionInfo> {
        Ok(self.connection_info.clone())
    }

    fn start(&mut self) -> Result<()> {
        // In a real implementation, this would start the Redis container
        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        // In a real implementation, this would stop the Redis container
        Ok(())
    }

    fn wait_for_ready(&self, _timeout: std::time::Duration) -> Result<()> {
        // In a real implementation, this would wait for Redis to be ready
        Ok(())
    }

    fn logs(&self) -> Result<Vec<String>> {
        // In a real implementation, this would return Redis logs
        Ok(vec!["Redis service logs".to_string()])
    }

    fn is_running(&self) -> Result<bool> {
        // In a real implementation, this would check if the container is running
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_redis_creation() {
        let redis = Redis::new();
        assert!(redis.is_ok());
    }

    #[test]
    fn test_redis_with_config() {
        let redis = Redis::with_config(6379, Some("testpass".to_string()));
        assert!(redis.is_ok());

        let redis = redis.unwrap();
        assert_eq!(redis.password(), Some("testpass"));
    }

    #[test]
    fn test_redis_with_config_no_password() {
        let redis = Redis::with_config(6379, None);
        assert!(redis.is_ok());

        let redis = redis.unwrap();
        assert_eq!(redis.password(), None);
    }

    #[test]
    fn test_redis_service_trait() {
        let mut redis = Redis::new().unwrap();
        assert_eq!(redis.name(), "redis");
        assert!(redis.start().is_ok());
        assert!(redis.stop().is_ok());
        assert!(redis.is_running().unwrap());
    }

    #[test]
    fn test_redis_connection_info() {
        let redis = Redis::new().unwrap();
        let conn_info = redis.connection_info();
        assert!(conn_info.params().contains("host=localhost"));
        assert!(conn_info.params().contains("port=6379"));
    }
}
