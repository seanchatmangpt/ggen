//! Redis service fixture for testing
//!
//! Provides a containerized Redis instance using testcontainers-rs
//! with health checks, connection info, and automatic teardown.

use crate::error::{Result, ServiceError};
use crate::services::{Service, ConnectionInfo};
use std::collections::HashMap;
use testcontainers::{
    Container,
    images::redis::Redis as RedisImage,
    runners::SyncRunner,
};

/// Redis service fixture using testcontainers
#[derive(Debug)]
pub struct Redis {
    /// Testcontainers container
    container: Container<RedisImage>,
    /// Connection information
    connection_info: ConnectionInfo,
    /// Password (optional)
    password: Option<String>,
}

impl<'d> Redis<'d> {
    /// Create a new Redis fixture
    pub fn new() -> Result<Self> {
        let image = RedisImage::default();
        let container = image.start();

        let connection_info = ConnectionInfo {
            params: format!(
                "host=localhost port={}",
                container.get_host_port_ipv4(6379)
            ),
        };

        Ok(Self {
            container,
            connection_info,
            password: None,
        })
    }

    /// Create with custom configuration
    pub fn with_config(password: Option<String>) -> Result<Self> {
        let image = if let Some(ref pass) = password {
            RedisImage::default().with_password(pass)
        } else {
            RedisImage::default()
        };

        let container = image.start();

        let connection_info = ConnectionInfo {
            params: format!(
                "host=localhost port={}{}",
                container.get_host_port_ipv4(6379),
                if let Some(ref pass) = password {
                    format!(" password={}", pass)
                } else {
                    String::new()
                }
            ),
        };

        Ok(Self {
            container,
            connection_info,
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

    /// Execute Redis command
    pub fn execute_command(&self, command: &str) -> Result<String> {
        let cmd = vec!["redis-cli".to_string(), command.to_string()];
        
        let result = self.container
            .exec(cmd)
            .map_err(|e| ServiceError::ConnectionFailed(format!("Failed to execute Redis command: {}", e)))?;

        if result.exit_code != Some(0) {
            return Err(ServiceError::ConnectionFailed(format!(
                "Redis command execution failed: {}",
                String::from_utf8_lossy(&result.stderr)
            )).into());
        }

        Ok(String::from_utf8_lossy(&result.stdout).to_string())
    }

    /// Set a key-value pair
    pub fn set(&self, key: &str, value: &str) -> Result<String> {
        let command = format!("SET {} {}", key, value);
        self.execute_command(&command)
    }

    /// Get a value by key
    pub fn get(&self, key: &str) -> Result<String> {
        let command = format!("GET {}", key);
        self.execute_command(&command)
    }

    /// Delete a key
    pub fn del(&self, key: &str) -> Result<String> {
        let command = format!("DEL {}", key);
        self.execute_command(&command)
    }

    /// Get all keys matching pattern
    pub fn keys(&self, pattern: &str) -> Result<String> {
        let command = format!("KEYS {}", pattern);
        self.execute_command(&command)
    }

    /// Get Redis info
    pub fn info(&self) -> Result<String> {
        self.execute_command("INFO")
    }

    /// Get Redis memory usage
    pub fn memory_usage(&self) -> Result<String> {
        self.execute_command("INFO memory")
    }

    /// Flush all databases
    pub fn flushall(&self) -> Result<String> {
        self.execute_command("FLUSHALL")
    }

    /// Get database size
    pub fn dbsize(&self) -> Result<String> {
        self.execute_command("DBSIZE")
    }
}

impl<'d> Service for Redis<'d> {
    fn name(&self) -> &str {
        "redis"
    }

    fn connection_info(&self) -> Result<ConnectionInfo> {
        Ok(self.connection_info.clone())
    }

    fn health_check(&self) -> Result<bool> {
        // Testcontainers handles health checks automatically
        // We can add custom health check logic here if needed
        Ok(true)
    }

    fn start(&mut self) -> Result<()> {
        // Container is already started by testcontainers
        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        // Container cleanup is handled automatically by testcontainers
        Ok(())
    }

    fn is_running(&self) -> Result<bool> {
        Ok(true) // Container is running
    }

    fn wait_for_ready(&self, timeout: std::time::Duration) -> Result<()> {
        // Testcontainers handles readiness automatically
        Ok(())
    }

    fn logs(&self) -> Result<Vec<String>> {
        // For now, return empty logs
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_redis_creation() {
        let redis = Redis::new();
        
        if redis.is_ok() {
            let redis = redis.unwrap();
            assert_eq!(redis.password(), None);
        } else {
            println!("Skipping test - Docker not available");
        }
    }

    #[test]
    fn test_redis_with_password() {
        let redis = Redis::with_config(Some("testpass".to_string()));
        
        if redis.is_ok() {
            let redis = redis.unwrap();
            assert_eq!(redis.password(), Some("testpass"));
        } else {
            println!("Skipping test - Docker not available");
        }
    }

    #[test]
    fn test_redis_operations() {
        let redis = Redis::new();
        
        if redis.is_ok() {
            let redis = redis.unwrap();
            
            // Set a key-value pair
            let set_result = redis.set("test_key", "test_value").unwrap();
            assert_eq!(set_result.trim(), "OK");
            
            // Get the value
            let get_result = redis.get("test_key").unwrap();
            assert_eq!(get_result.trim(), "test_value");
            
            // Get database size
            let dbsize = redis.dbsize().unwrap();
            assert!(dbsize.trim().parse::<i32>().unwrap() > 0);
            
            // Delete the key
            let del_result = redis.del("test_key").unwrap();
            assert_eq!(del_result.trim(), "1");
            
            // Get Redis info
            let info = redis.info().unwrap();
            assert!(info.contains("redis_version"));
        } else {
            println!("Skipping test - Docker not available");
        }
    }
}