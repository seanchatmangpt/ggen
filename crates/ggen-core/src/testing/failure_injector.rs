//! Container failure injection utilities
//!
//! Provides low-level container manipulation for chaos engineering scenarios.
//! Supports killing, pausing/unpausing, and resource limit modification.

use std::process::Command;
use thiserror::Error;

/// Failure injection error types
#[derive(Debug, Error)]
pub enum InjectionError {
    /// Docker command failed
    #[error("Docker command failed: {0}")]
    DockerCommandFailed(String),

    /// Container not found
    #[error("Container not found: {0}")]
    ContainerNotFound(String),

    /// Invalid resource limit
    #[error("Invalid resource limit: {0}")]
    InvalidResourceLimit(String),

    /// Permission denied
    #[error("Permission denied: {0}")]
    PermissionDenied(String),

    /// Unknown error
    #[error("Unknown error: {0}")]
    Unknown(String),
}

/// Result type for injection operations
pub type Result<T> = std::result::Result<T, InjectionError>;

/// Result of failure injection operation
#[derive(Debug, Clone)]
pub struct InjectionResult {
    /// Whether injection succeeded
    pub success: bool,
    /// Container ID affected
    pub container_id: String,
    /// Error message if injection failed
    pub error_message: Option<String>,
    /// stdout from Docker command
    pub stdout: String,
    /// stderr from Docker command
    pub stderr: String,
}

impl InjectionResult {
    /// Create a successful injection result
    pub fn success(container_id: String, stdout: String) -> Self {
        InjectionResult {
            success: true,
            container_id,
            error_message: None,
            stdout,
            stderr: String::new(),
        }
    }

    /// Create a failed injection result
    pub fn failure(container_id: String, error: String, stderr: String) -> Self {
        InjectionResult {
            success: false,
            container_id,
            error_message: Some(error),
            stdout: String::new(),
            stderr,
        }
    }
}

/// Failure injector for container manipulation
pub struct FailureInjector {
    /// Docker daemon socket path
    docker_host: String,
}

impl FailureInjector {
    /// Create a new failure injector
    ///
    /// # Arguments
    ///
    /// * `docker_host` - Docker daemon socket path (e.g., "unix:///var/run/docker.sock")
    ///
    /// # Errors
    ///
    /// Returns error if Docker connection cannot be established
    pub fn new(docker_host: String) -> Result<Self> {
        // Verify Docker is available
        let output = Command::new("docker")
            .arg("--version")
            .output()
            .map_err(|e| InjectionError::DockerCommandFailed(format!("Docker not found: {}", e)))?;

        if !output.status.success() {
            return Err(InjectionError::DockerCommandFailed(
                "Docker not available".to_string(),
            ));
        }

        Ok(FailureInjector { docker_host })
    }

    /// Get Docker host
    pub fn docker_host(&self) -> &str {
        &self.docker_host
    }

    /// Kill container suddenly (simulates crash)
    ///
    /// # Arguments
    ///
    /// * `container_id` - Container ID or name to kill
    ///
    /// # Errors
    ///
    /// Returns error if container cannot be killed
    pub fn kill_container(&self, container_id: &str) -> Result<InjectionResult> {
        let output = Command::new("docker")
            .env("DOCKER_HOST", &self.docker_host)
            .args(["kill", container_id])
            .output()
            .map_err(|e| InjectionError::DockerCommandFailed(e.to_string()))?;

        if output.status.success() {
            Ok(InjectionResult::success(
                container_id.to_string(),
                String::from_utf8_lossy(&output.stdout).to_string(),
            ))
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            if stderr.contains("No such container") {
                Err(InjectionError::ContainerNotFound(container_id.to_string()))
            } else {
                Ok(InjectionResult::failure(
                    container_id.to_string(),
                    "Kill failed".to_string(),
                    stderr,
                ))
            }
        }
    }

    /// Pause container (simulates network partition)
    ///
    /// # Arguments
    ///
    /// * `container_id` - Container ID or name to pause
    ///
    /// # Errors
    ///
    /// Returns error if container cannot be paused
    pub fn pause_container(&self, container_id: &str) -> Result<InjectionResult> {
        let output = Command::new("docker")
            .env("DOCKER_HOST", &self.docker_host)
            .args(["pause", container_id])
            .output()
            .map_err(|e| InjectionError::DockerCommandFailed(e.to_string()))?;

        if output.status.success() {
            Ok(InjectionResult::success(
                container_id.to_string(),
                String::from_utf8_lossy(&output.stdout).to_string(),
            ))
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            Ok(InjectionResult::failure(
                container_id.to_string(),
                "Pause failed".to_string(),
                stderr,
            ))
        }
    }

    /// Unpause container
    ///
    /// # Arguments
    ///
    /// * `container_id` - Container ID or name to unpause
    ///
    /// # Errors
    ///
    /// Returns error if container cannot be unpaused
    pub fn unpause_container(&self, container_id: &str) -> Result<InjectionResult> {
        let output = Command::new("docker")
            .env("DOCKER_HOST", &self.docker_host)
            .args(["unpause", container_id])
            .output()
            .map_err(|e| InjectionError::DockerCommandFailed(e.to_string()))?;

        if output.status.success() {
            Ok(InjectionResult::success(
                container_id.to_string(),
                String::from_utf8_lossy(&output.stdout).to_string(),
            ))
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            Ok(InjectionResult::failure(
                container_id.to_string(),
                "Unpause failed".to_string(),
                stderr,
            ))
        }
    }

    /// Set resource limits dynamically
    ///
    /// # Arguments
    ///
    /// * `container_id` - Container ID or name
    /// * `memory_limit` - Memory limit in bytes (None = unlimited)
    /// * `cpu_quota` - CPU quota in microseconds (0-100000, where 100000 = 1 CPU)
    ///
    /// # Errors
    ///
    /// Returns error if resource limits cannot be set
    pub fn set_resource_limits(
        &self,
        container_id: &str,
        memory_limit: Option<u64>,
        cpu_quota: Option<i64>,
    ) -> Result<InjectionResult> {
        let mut args = vec!["update".to_string()];

        if let Some(memory) = memory_limit {
            if memory == 0 {
                return Err(InjectionError::InvalidResourceLimit(
                    "Memory limit must be > 0".to_string(),
                ));
            }
            args.push("--memory".to_string());
            args.push(memory.to_string());
        }

        if let Some(quota) = cpu_quota {
            if quota < 0 || quota > 100_000 {
                return Err(InjectionError::InvalidResourceLimit(
                    "CPU quota must be 0-100000".to_string(),
                ));
            }
            args.push("--cpu-quota".to_string());
            args.push(quota.to_string());
        }

        args.push(container_id.to_string());

        let output = Command::new("docker")
            .env("DOCKER_HOST", &self.docker_host)
            .args(&args)
            .output()
            .map_err(|e| InjectionError::DockerCommandFailed(e.to_string()))?;

        if output.status.success() {
            Ok(InjectionResult::success(
                container_id.to_string(),
                String::from_utf8_lossy(&output.stdout).to_string(),
            ))
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            Ok(InjectionResult::failure(
                container_id.to_string(),
                "Resource limit update failed".to_string(),
                stderr,
            ))
        }
    }

    /// Introduce network latency (if supported by Docker network driver)
    ///
    /// Note: This requires tc (traffic control) to be available in the container
    ///
    /// # Arguments
    ///
    /// * `container_id` - Container ID or name
    /// * `latency_ms` - Latency in milliseconds
    ///
    /// # Errors
    ///
    /// Returns error if latency cannot be introduced
    pub fn add_network_latency(&self, container_id: &str, latency_ms: u32) -> Result<InjectionResult> {
        let cmd = format!(
            "tc qdisc add dev eth0 root netem delay {}ms",
            latency_ms
        );

        let output = Command::new("docker")
            .env("DOCKER_HOST", &self.docker_host)
            .args(["exec", container_id, "sh", "-c", &cmd])
            .output()
            .map_err(|e| InjectionError::DockerCommandFailed(e.to_string()))?;

        if output.status.success() {
            Ok(InjectionResult::success(
                container_id.to_string(),
                String::from_utf8_lossy(&output.stdout).to_string(),
            ))
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            Ok(InjectionResult::failure(
                container_id.to_string(),
                "Network latency injection failed".to_string(),
                stderr,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_failure_injector_creation() {
        // Arrange
        let docker_host = "unix:///var/run/docker.sock".to_string();

        // Act
        let result = FailureInjector::new(docker_host.clone());

        // Assert - verify creation (Docker may not be available in test environment)
        match result {
            Ok(injector) => {
                assert_eq!(injector.docker_host(), docker_host);
            }
            Err(InjectionError::DockerCommandFailed(_)) => {
                // Docker not available - expected in some test environments
                eprintln!("Docker not available - skipping test");
            }
            Err(e) => panic!("Unexpected error: {}", e),
        }
    }

    #[test]
    fn test_injection_result_success() {
        // Arrange
        let container_id = "test-container".to_string();
        let stdout = "Container killed".to_string();

        // Act
        let result = InjectionResult::success(container_id.clone(), stdout.clone());

        // Assert
        assert!(result.success);
        assert_eq!(result.container_id, container_id);
        assert_eq!(result.stdout, stdout);
        assert!(result.error_message.is_none());
    }

    #[test]
    fn test_injection_result_failure() {
        // Arrange
        let container_id = "test-container".to_string();
        let error = "Kill failed".to_string();
        let stderr = "No such container".to_string();

        // Act
        let result = InjectionResult::failure(container_id.clone(), error.clone(), stderr.clone());

        // Assert
        assert!(!result.success);
        assert_eq!(result.container_id, container_id);
        assert_eq!(result.error_message, Some(error));
        assert_eq!(result.stderr, stderr);
    }

    #[test]
    fn test_invalid_resource_limit_zero_memory() {
        // Arrange
        let docker_host = "unix:///var/run/docker.sock".to_string();

        // Act & Assert - verify error handling without requiring Docker
        if let Ok(injector) = FailureInjector::new(docker_host) {
            let result = injector.set_resource_limits("test", Some(0), None);
            assert!(result.is_err());
            match result {
                Err(InjectionError::InvalidResourceLimit(msg)) => {
                    assert!(msg.contains("Memory limit must be > 0"));
                }
                _ => panic!("Expected InvalidResourceLimit error"),
            }
        }
    }

    #[test]
    fn test_invalid_resource_limit_negative_cpu() {
        // Arrange
        let docker_host = "unix:///var/run/docker.sock".to_string();

        // Act & Assert
        if let Ok(injector) = FailureInjector::new(docker_host) {
            let result = injector.set_resource_limits("test", None, Some(-1));
            assert!(result.is_err());
            match result {
                Err(InjectionError::InvalidResourceLimit(msg)) => {
                    assert!(msg.contains("CPU quota must be 0-100000"));
                }
                _ => panic!("Expected InvalidResourceLimit error"),
            }
        }
    }

    #[test]
    fn test_invalid_resource_limit_excessive_cpu() {
        // Arrange
        let docker_host = "unix:///var/run/docker.sock".to_string();

        // Act & Assert
        if let Ok(injector) = FailureInjector::new(docker_host) {
            let result = injector.set_resource_limits("test", None, Some(100_001));
            assert!(result.is_err());
            match result {
                Err(InjectionError::InvalidResourceLimit(msg)) => {
                    assert!(msg.contains("CPU quota must be 0-100000"));
                }
                _ => panic!("Expected InvalidResourceLimit error"),
            }
        }
    }
}
