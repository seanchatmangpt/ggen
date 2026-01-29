//! Testcontainers integration for production-ready testing
//!
//! Provides high-level container management with health checks, port discovery,
//! and automatic cleanup using RAII pattern.

use super::docker_client::DockerClient;
use ggen_utils::error::{Context, Result};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use std::thread;

/// Container configuration
///
/// Defines the configuration for a Docker container including image, ports,
/// environment variables, and health check settings.
#[derive(Debug, Clone)]
pub struct ContainerConfig {
    /// Docker image name and tag
    pub image: String,
    /// Port mappings (host:container)
    pub ports: Vec<String>,
    /// Environment variables
    pub env_vars: HashMap<String, String>,
    /// Health check configuration
    pub health_check: HealthCheck,
    /// Container name (optional)
    pub name: Option<String>,
    /// Volume mounts (host:container)
    pub volumes: Vec<String>,
    /// Additional docker run arguments
    pub extra_args: Vec<String>,
}

impl ContainerConfig {
    /// Create a new container configuration
    #[must_use]
    pub fn new(image: impl Into<String>) -> Self {
        Self {
            image: image.into(),
            ports: Vec::new(),
            env_vars: HashMap::new(),
            health_check: HealthCheck::default(),
            name: None,
            volumes: Vec::new(),
            extra_args: Vec::new(),
        }
    }

    /// Create a Redis container configuration
    #[must_use]
    pub fn redis() -> Self {
        Self::new("redis:latest")
            .with_port("6379")
            .with_health_check(HealthCheck::redis())
    }

    /// Create a PostgreSQL container configuration
    #[must_use]
    pub fn postgres() -> Self {
        Self::new("postgres:latest")
            .with_port("5432")
            .with_env("POSTGRES_PASSWORD", "password")
            .with_env("POSTGRES_USER", "postgres")
            .with_env("POSTGRES_DB", "test")
            .with_health_check(HealthCheck::postgres())
    }

    /// Add a port mapping (publishes random host port)
    #[must_use]
    pub fn with_port(mut self, container_port: &str) -> Self {
        self.ports.push(container_port.to_string());
        self
    }

    /// Add a port mapping with explicit host port
    #[must_use]
    pub fn with_port_mapping(mut self, host_port: &str, container_port: &str) -> Self {
        self.ports.push(format!("{}:{}", host_port, container_port));
        self
    }

    /// Add an environment variable
    #[must_use]
    pub fn with_env(mut self, key: &str, value: &str) -> Self {
        self.env_vars.insert(key.to_string(), value.to_string());
        self
    }

    /// Set the health check configuration
    #[must_use]
    pub fn with_health_check(mut self, health_check: HealthCheck) -> Self {
        self.health_check = health_check;
        self
    }

    /// Set the health check timeout
    #[must_use]
    pub fn with_health_timeout(mut self, timeout: Duration) -> Self {
        self.health_check.timeout = timeout;
        self
    }

    /// Set the container name
    #[must_use]
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Add a volume mount
    #[must_use]
    pub fn with_volume(mut self, volume: &str) -> Self {
        self.volumes.push(volume.to_string());
        self
    }

    /// Add extra docker run arguments
    #[must_use]
    pub fn with_extra_args(mut self, args: Vec<String>) -> Self {
        self.extra_args = args;
        self
    }

    /// Build the docker run arguments
    fn build_run_args(&self) -> Vec<String> {
        let mut args = Vec::new();

        // Add port mappings
        for port in &self.ports {
            args.push("-p".to_string());
            args.push(port.clone());
        }

        // Add environment variables
        for (key, value) in &self.env_vars {
            args.push("-e".to_string());
            args.push(format!("{}={}", key, value));
        }

        // Add volume mounts
        for volume in &self.volumes {
            args.push("-v".to_string());
            args.push(volume.clone());
        }

        // Add container name
        if let Some(name) = &self.name {
            args.push("--name".to_string());
            args.push(name.clone());
        }

        // Add extra arguments
        args.extend(self.extra_args.clone());

        args
    }
}

/// Health check configuration
///
/// Defines how to verify that a container is healthy and ready.
#[derive(Debug, Clone)]
pub struct HealthCheck {
    /// Maximum time to wait for health check to pass
    pub timeout: Duration,
    /// Time to wait between health check attempts
    pub interval: Duration,
    /// Custom health check command (executed inside container)
    pub command: Option<Vec<String>>,
    /// Health check type (TCP, HTTP, command)
    pub check_type: HealthCheckType,
}

impl Default for HealthCheck {
    fn default() -> Self {
        Self {
            timeout: Duration::from_secs(30),
            interval: Duration::from_millis(500),
            command: None,
            check_type: HealthCheckType::None,
        }
    }
}

impl HealthCheck {
    /// Create a health check with no verification
    #[must_use]
    pub fn none() -> Self {
        Self::default()
    }

    /// Create a Redis health check
    #[must_use]
    pub fn redis() -> Self {
        Self {
            timeout: Duration::from_secs(30),
            interval: Duration::from_millis(500),
            command: Some(vec!["redis-cli".to_string(), "ping".to_string()]),
            check_type: HealthCheckType::Command,
        }
    }

    /// Create a PostgreSQL health check
    #[must_use]
    pub fn postgres() -> Self {
        Self {
            timeout: Duration::from_secs(30),
            interval: Duration::from_millis(500),
            command: Some(vec![
                "pg_isready".to_string(),
                "-U".to_string(),
                "postgres".to_string(),
            ]),
            check_type: HealthCheckType::Command,
        }
    }

    /// Create a custom command health check
    #[must_use]
    pub fn command(command: Vec<String>) -> Self {
        Self {
            timeout: Duration::from_secs(30),
            interval: Duration::from_millis(500),
            command: Some(command),
            check_type: HealthCheckType::Command,
        }
    }

    /// Set the timeout duration
    #[must_use]
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    /// Set the interval duration
    #[must_use]
    pub fn with_interval(mut self, interval: Duration) -> Self {
        self.interval = interval;
        self
    }
}

/// Health check type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HealthCheckType {
    /// No health check
    None,
    /// Execute command inside container
    Command,
    /// TCP connection check (future)
    Tcp,
    /// HTTP endpoint check (future)
    Http,
}

/// Container manager with automatic cleanup
///
/// Manages a Docker container lifecycle with automatic cleanup on drop (RAII pattern).
pub struct ContainerManager {
    /// Container ID
    container_id: String,
    /// Docker client
    client: DockerClient,
    /// Container configuration
    config: ContainerConfig,
    /// Whether container is running
    running: bool,
}

impl ContainerManager {
    /// Start a new container with the given configuration
    ///
    /// # Arguments
    ///
    /// * `config` - Container configuration
    ///
    /// # Returns
    ///
    /// Returns a container manager on success
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to start or health check fails
    pub fn new(config: ContainerConfig) -> Result<Self> {
        let client = DockerClient::new();

        // Build docker run arguments
        let args = config.build_run_args();

        // Start container
        let container_id = client
            .run(&config.image, &args)
            .context("Failed to start container")?;

        let mut manager = Self {
            container_id,
            client,
            config,
            running: true,
        };

        // Wait for health check to pass
        manager.wait_for_health()?;

        Ok(manager)
    }

    /// Get the container ID
    #[must_use]
    pub fn container_id(&self) -> &str {
        &self.container_id
    }

    /// Get the host port for a container port
    ///
    /// # Arguments
    ///
    /// * `container_port` - The container port (e.g., "6379")
    ///
    /// # Returns
    ///
    /// Returns the host port number on success
    ///
    /// # Errors
    ///
    /// Returns an error if port mapping cannot be determined
    pub fn port(&self, container_port: &str) -> Result<u16> {
        let port_spec = format!("{}/tcp", container_port);
        self.client
            .port(&self.container_id, &port_spec)
            .context("Failed to get port mapping")
    }

    /// Get the first mapped port (convenience method)
    ///
    /// # Returns
    ///
    /// Returns the first host port number on success
    ///
    /// # Errors
    ///
    /// Returns an error if no ports are mapped or port cannot be determined
    pub fn first_port(&self) -> Result<u16> {
        let first_port = self
            .config
            .ports
            .first()
            .ok_or_else(|| ggen_utils::error::Error::new("No ports configured"))?;

        // Extract container port from mapping (e.g., "8080:80" -> "80")
        let container_port = if first_port.contains(':') {
            first_port
                .split(':')
                .last()
                .ok_or_else(|| ggen_utils::error::Error::new("Invalid port mapping"))?
        } else {
            first_port
        };

        self.port(container_port)
    }

    /// Execute a command inside the container
    ///
    /// # Arguments
    ///
    /// * `command` - Command and arguments to execute
    ///
    /// # Returns
    ///
    /// Returns the command output on success
    ///
    /// # Errors
    ///
    /// Returns an error if the command fails
    pub fn exec(&self, command: &[String]) -> Result<String> {
        self.client
            .exec(&self.container_id, command)
            .context("Failed to execute command in container")
    }

    /// Get container logs
    ///
    /// # Arguments
    ///
    /// * `tail` - Number of lines to show from end of logs (None for all)
    ///
    /// # Returns
    ///
    /// Returns the container logs on success
    ///
    /// # Errors
    ///
    /// Returns an error if logs cannot be retrieved
    pub fn logs(&self, tail: Option<usize>) -> Result<String> {
        self.client
            .logs(&self.container_id, tail)
            .context("Failed to get container logs")
    }

    /// Pause the container
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to pause
    pub fn pause(&mut self) -> Result<()> {
        self.client
            .pause(&self.container_id)
            .context("Failed to pause container")?;
        self.running = false;
        Ok(())
    }

    /// Unpause the container
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to unpause
    pub fn unpause(&mut self) -> Result<()> {
        self.client
            .unpause(&self.container_id)
            .context("Failed to unpause container")?;
        self.running = true;
        Ok(())
    }

    /// Stop the container
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to stop
    pub fn stop(&mut self) -> Result<()> {
        if self.running {
            self.client
                .stop(&self.container_id)
                .context("Failed to stop container")?;
            self.running = false;
        }
        Ok(())
    }

    /// Check if the container is healthy
    ///
    /// # Returns
    ///
    /// Returns true if the health check passes
    fn is_healthy(&self) -> bool {
        match self.config.health_check.check_type {
            HealthCheckType::None => true,
            HealthCheckType::Command => {
                if let Some(command) = &self.config.health_check.command {
                    self.exec(command).is_ok()
                } else {
                    true
                }
            }
            HealthCheckType::Tcp | HealthCheckType::Http => {
                // TODO: Implement TCP and HTTP health checks
                true
            }
        }
    }

    /// Wait for the container to become healthy
    ///
    /// # Errors
    ///
    /// Returns an error if health check times out
    fn wait_for_health(&mut self) -> Result<()> {
        let start = Instant::now();
        let timeout = self.config.health_check.timeout;
        let interval = self.config.health_check.interval;

        while start.elapsed() < timeout {
            if self.is_healthy() {
                return Ok(());
            }
            thread::sleep(interval);
        }

        Err(ggen_utils::error::Error::new(&format!(
            "Health check timed out after {:?}",
            timeout
        )))
    }
}

impl Drop for ContainerManager {
    /// Automatically stop and remove container on drop (RAII pattern)
    fn drop(&mut self) {
        // Ignore errors during cleanup - best effort
        let _ = self.client.stop(&self.container_id);
        let _ = self.client.rm(&self.container_id, true);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_container_config_creation() {
        // Arrange & Act
        let config = ContainerConfig::new("redis:latest");

        // Assert
        assert_eq!(config.image, "redis:latest");
        assert!(config.ports.is_empty());
        assert!(config.env_vars.is_empty());
    }

    #[test]
    fn test_container_config_redis() {
        // Arrange & Act
        let config = ContainerConfig::redis();

        // Assert
        assert_eq!(config.image, "redis:latest");
        assert_eq!(config.ports, vec!["6379"]);
    }

    #[test]
    fn test_container_config_postgres() {
        // Arrange & Act
        let config = ContainerConfig::postgres();

        // Assert
        assert_eq!(config.image, "postgres:latest");
        assert_eq!(config.ports, vec!["5432"]);
        assert_eq!(config.env_vars.get("POSTGRES_PASSWORD"), Some(&"password".to_string()));
    }

    #[test]
    fn test_container_config_with_port() {
        // Arrange & Act
        let config = ContainerConfig::new("nginx")
            .with_port("80");

        // Assert
        assert_eq!(config.ports, vec!["80"]);
    }

    #[test]
    fn test_container_config_with_port_mapping() {
        // Arrange & Act
        let config = ContainerConfig::new("nginx")
            .with_port_mapping("8080", "80");

        // Assert
        assert_eq!(config.ports, vec!["8080:80"]);
    }

    #[test]
    fn test_container_config_with_env() {
        // Arrange & Act
        let config = ContainerConfig::new("app")
            .with_env("KEY", "value");

        // Assert
        assert_eq!(config.env_vars.get("KEY"), Some(&"value".to_string()));
    }

    #[test]
    fn test_container_config_with_name() {
        // Arrange & Act
        let config = ContainerConfig::new("redis")
            .with_name("test-redis");

        // Assert
        assert_eq!(config.name, Some("test-redis".to_string()));
    }

    #[test]
    fn test_container_config_build_run_args() {
        // Arrange
        let config = ContainerConfig::new("redis")
            .with_port("6379")
            .with_env("KEY", "value")
            .with_name("test-redis");

        // Act
        let args = config.build_run_args();

        // Assert
        assert!(args.contains(&"-p".to_string()));
        assert!(args.contains(&"6379".to_string()));
        assert!(args.contains(&"-e".to_string()));
        assert!(args.contains(&"KEY=value".to_string()));
        assert!(args.contains(&"--name".to_string()));
        assert!(args.contains(&"test-redis".to_string()));
    }

    #[test]
    fn test_health_check_default() {
        // Arrange & Act
        let health_check = HealthCheck::default();

        // Assert
        assert_eq!(health_check.timeout, Duration::from_secs(30));
        assert_eq!(health_check.interval, Duration::from_millis(500));
        assert_eq!(health_check.check_type, HealthCheckType::None);
    }

    #[test]
    fn test_health_check_redis() {
        // Arrange & Act
        let health_check = HealthCheck::redis();

        // Assert
        assert_eq!(health_check.check_type, HealthCheckType::Command);
        assert_eq!(
            health_check.command,
            Some(vec!["redis-cli".to_string(), "ping".to_string()])
        );
    }

    #[test]
    fn test_health_check_postgres() {
        // Arrange & Act
        let health_check = HealthCheck::postgres();

        // Assert
        assert_eq!(health_check.check_type, HealthCheckType::Command);
        assert!(health_check.command.is_some());
    }

    #[test]
    fn test_health_check_with_timeout() {
        // Arrange & Act
        let health_check = HealthCheck::default()
            .with_timeout(Duration::from_secs(60));

        // Assert
        assert_eq!(health_check.timeout, Duration::from_secs(60));
    }

    #[test]
    fn test_health_check_with_interval() {
        // Arrange & Act
        let health_check = HealthCheck::default()
            .with_interval(Duration::from_secs(1));

        // Assert
        assert_eq!(health_check.interval, Duration::from_secs(1));
    }
}
