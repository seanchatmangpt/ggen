//! Redis service fixture for testing
//!
//! Provides a containerized Redis instance with health checks,
//! connection info, and automatic teardown.

use crate::error::{Result, ServiceError};
use crate::services::{ConnectionInfo, Service};
use std::collections::HashMap;
use std::process::Command;
use std::thread;
use std::time::{Duration, Instant};

/// Redis service fixture
/// WIP: Implement Redis service fixture
#[derive(Debug)]
pub struct Redis {
    /// Container name for cleanup
    container_name: String,
    /// Port mapping
    port: u16,
    /// Password (optional)
    password: Option<String>,
    /// Started flag
    started: bool,
}

impl Redis {
    /// Create a new Redis fixture
    pub fn new() -> Result<Self> {
        let container_name = format!("cleanroom-redis-{}", rand::random::<u32>());

        Ok(Self {
            container_name,
            port: 6379,
            password: None,
            started: false,
        })
    }

    /// Create with custom configuration
    pub fn with_config(port: u16, password: Option<String>) -> Result<Self> {
        let container_name = format!("cleanroom-redis-{}", rand::random::<u32>());

        Ok(Self {
            container_name,
            port,
            password,
            started: false,
        })
    }

    /// Start Redis container
    fn start_container(&mut self) -> Result<()> {
        if self.started {
            return Ok(());
        }

        // Check if Docker is available
        let docker_available = Command::new("docker")
            .arg("version")
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false);

        if !docker_available {
            return Err(ServiceError::Msg("Docker not available for Redis fixture".into()).into());
        }

        // Pull Redis image
        let pull_output = Command::new("docker")
            .arg("pull")
            .arg("redis:7-alpine")
            .output()
            .map_err(|e| ServiceError::Msg(format!("Failed to pull Redis image: {}", e)))?;

        if !pull_output.status.success() {
            return Err(ServiceError::Msg(format!(
                "Failed to pull Redis image: {}",
                String::from_utf8_lossy(&pull_output.stderr)
            ))
            .into());
        }

        // Build docker run command
        let mut docker_cmd = Command::new("docker");
        docker_cmd
            .arg("run")
            .arg("-d")
            .arg("--name")
            .arg(&self.container_name)
            .arg("-p")
            .arg(format!("{}:6379", self.port));

        // Add password if provided
        if let Some(ref password) = self.password {
            docker_cmd
                .arg("-e")
                .arg(format!("REDIS_PASSWORD={}", password));
        }

        docker_cmd.arg("redis:7-alpine");

        // Add auth command if password is set
        if self.password.is_some() {
            docker_cmd
                .arg("redis-server")
                .arg("--requirepass")
                .arg(self.password.as_ref().unwrap());
        }

        // Start Redis container
        let start_output = docker_cmd
            .output()
            .map_err(|e| ServiceError::Msg(format!("Failed to start Redis container: {}", e)))?;

        if !start_output.status.success() {
            return Err(ServiceError::Msg(format!(
                "Failed to start Redis container: {}",
                String::from_utf8_lossy(&start_output.stderr)
            ))
            .into());
        }

        self.started = true;
        Ok(())
    }

    /// Wait for Redis to be ready
    fn wait_for_ready(&self) -> Result<()> {
        let max_wait = Duration::from_secs(30);
        let start = Instant::now();
        let check_interval = Duration::from_millis(500);

        while start.elapsed() < max_wait {
            if self.health_check()? {
                return Ok(());
            }
            thread::sleep(check_interval);
        }

        Err(ServiceError::StartupTimeout(format!(
            "Redis container {} did not become ready within {} seconds",
            self.container_name,
            max_wait.as_secs()
        ))
        .into())
    }
}

impl Service for Redis {
    fn name(&self) -> &str {
        "redis"
    }

    fn health_check(&self) -> Result<bool> {
        if !self.started {
            return Ok(false);
        }

        // Check if container is running
        let status_output = Command::new("docker")
            .arg("ps")
            .arg("-q")
            .arg("-f")
            .arg(format!("name={}", self.container_name))
            .output()
            .map_err(|e| {
                ServiceError::HealthCheckFailed(format!("Failed to check container status: {}", e))
            })?;

        if status_output.stdout.is_empty() {
            return Ok(false);
        }

        // Try to ping Redis
        let mut redis_cmd = Command::new("docker");
        redis_cmd
            .arg("exec")
            .arg(&self.container_name)
            .arg("redis-cli");

        // Add auth if password is set
        if let Some(ref password) = self.password {
            redis_cmd.arg("-a").arg(password);
        }

        let ping_output = redis_cmd
            .arg("ping")
            .output()
            .map_err(|e| ServiceError::HealthCheckFailed(format!("Failed to ping Redis: {}", e)))?;

        Ok(ping_output.status.success()
            && String::from_utf8_lossy(&ping_output.stdout).contains("PONG"))
    }

    fn connection_info(&self) -> Result<ConnectionInfo> {
        if !self.started {
            return Err(ServiceError::Msg("Redis service not started".into()).into());
        }

        let mut params = HashMap::new();
        if let Some(ref password) = self.password {
            params.insert("password".to_string(), password.clone());
        }

        Ok(ConnectionInfo {
            host: "localhost".to_string(),
            port: self.port,
            params,
        })
    }

    fn start(&mut self) -> Result<()> {
        self.start_container()?;
        self.wait_for_ready()?;
        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        if !self.started {
            return Ok(());
        }

        // Stop and remove container
        let _stop_output = Command::new("docker")
            .arg("stop")
            .arg(&self.container_name)
            .output();

        let _remove_output = Command::new("docker")
            .arg("rm")
            .arg(&self.container_name)
            .output();

        self.started = false;
        Ok(())
    }
}

impl Drop for Redis {
    fn drop(&mut self) {
        let _ = self.stop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_redis_creation() {
        let redis = Redis::new().unwrap();
        assert_eq!(redis.name(), "redis");
        assert_eq!(redis.port, 6379);
        assert!(redis.password.is_none());
        assert!(!redis.started);
    }

    #[test]
    fn test_redis_with_config() {
        let redis = Redis::with_config(6380, Some("mypass".to_string())).unwrap();
        assert_eq!(redis.port, 6380);
        assert_eq!(redis.password, Some("mypass".to_string()));
    }

    #[test]
    fn test_redis_without_password() {
        let redis = Redis::with_config(6381, None).unwrap();
        assert_eq!(redis.port, 6381);
        assert!(redis.password.is_none());
    }

    #[test]
    fn test_redis_health_check_not_started() {
        let redis = Redis::new().unwrap();
        assert!(!redis.health_check().unwrap());
    }

    #[test]
    fn test_redis_connection_info_not_started() {
        let redis = Redis::new().unwrap();
        assert!(redis.connection_info().is_err());
    }
}
