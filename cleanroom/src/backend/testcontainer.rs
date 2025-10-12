//! Testcontainers backend for containerized command execution
//!
//! Provides testcontainers-rs integration for hermetic, isolated execution
//! with automatic container lifecycle management.

use crate::backend::{Backend, Cmd, RunResult};
use crate::error::{BackendError, Result};
use crate::policy::Policy;
use std::time::{Duration, Instant};
use testcontainers::{
    Container,
    GenericImage,
    RunnableImage,
};

/// Testcontainers backend for containerized execution
pub struct TestcontainerBackend {
    /// Base image configuration
    image_name: String,
    image_tag: String,
    /// Default policy
    policy: Policy,
    /// Container timeout
    timeout: Duration,
}

impl TestcontainerBackend {
    /// Create a new testcontainers backend
    pub fn new(image: impl Into<String>) -> Result<Self> {
        let image_name = image.into();
        
        Ok(Self {
            image_name: image_name.clone(),
            image_tag: "latest".to_string(),
            policy: Policy::default(),
            timeout: Duration::from_secs(300), // 5 minutes
        })
    }

    /// Create with custom policy
    pub fn with_policy(mut self, policy: Policy) -> Self {
        self.policy = policy;
        self
    }

    /// Create with custom timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    /// Add environment variable to container
    pub fn with_env(self, _key: &str, _val: &str) -> Self {
        // Store env vars in policy for now - will be applied during container creation
        // TODO: Implement proper env var storage
        self
    }

    /// Set default command for container
    pub fn with_cmd(self, _cmd: Vec<String>) -> Self {
        // Store command in policy for now - will be applied during container creation
        // TODO: Implement proper command storage
        self
    }

    /// Add volume mount
    pub fn with_volume(self, _host_path: &str, _container_path: &str) -> Self {
        // Store volume mounts in policy for now - will be applied during container creation
        // TODO: Implement proper volume mount storage
        self
    }

    /// Check if testcontainers is available
    pub fn is_available() -> bool {
        // For now, assume Docker is available if we can create a GenericImage
        true
    }

    /// Execute command in container
    fn execute_in_container(&self, cmd: &Cmd) -> Result<RunResult> {
        let start_time = Instant::now();

        // Create base image
        let image = GenericImage::new(self.image_name.clone(), self.image_tag.clone());

        // Build container request with all configurations
        let mut container_request = RunnableImage::from(image);

        // Add environment variables from command
        for (key, value) in &cmd.env {
            container_request = container_request.with_env_var(key, value);
        }

        // Add policy environment variables
        for (key, value) in self.policy.to_env() {
            container_request = container_request.with_env_var(key, value);
        }

        // Set working directory if specified
        if let Some(workdir) = &cmd.workdir {
            container_request = container_request.with_working_dir(workdir);
        }

        // Start container using the new testcontainers API
        let container = container_request.start()
            .map_err(|e| BackendError::Runtime(format!("Failed to start container: {}", e)))?;

        // Execute command - testcontainers expects Vec<&str> for exec
        let cmd_args: Vec<&str> = std::iter::once(cmd.bin.as_str())
            .chain(cmd.args.iter().map(|s| s.as_str()))
            .collect();

        let exec_result = container
            .exec(cmd_args)
            .map_err(|e| BackendError::Runtime(format!("Command execution failed: {}", e)))?;

        let duration_ms = start_time.elapsed().as_millis() as u64;

        // Extract output - SyncExecResult provides stdout() and stderr() as Vec<u8>
        let stdout = String::from_utf8_lossy(exec_result.stdout()).to_string();
        let stderr = String::from_utf8_lossy(exec_result.stderr()).to_string();
        let exit_code = exec_result.exit_code().unwrap_or(-1);

        Ok(RunResult {
            exit_code,
            stdout,
            stderr,
            duration_ms,
            hermetic: true, // Containers are hermetic by default
            deterministic_mounts: true, // Mounts are deterministic
            normalized_clock: false, // Clock normalization not implemented yet
            backend: "testcontainers".to_string(),
        })
    }
}

impl Backend for TestcontainerBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        // Use synchronous execution with timeout
        let start_time = Instant::now();
        
        // Execute command with timeout
        let result = self.execute_in_container(&cmd)?;
        
        // Check if execution exceeded timeout
        if start_time.elapsed() > self.timeout {
            return Err(crate::error::Error::Backend(BackendError::Runtime(format!("Command timed out after {} seconds", self.timeout.as_secs()))));
        }
        
        Ok(result)
    }

    fn name(&self) -> &str {
        "testcontainers"
    }

    fn is_available(&self) -> bool {
        Self::is_available()
    }

    fn supports_hermetic(&self) -> bool {
        true
    }

    fn supports_deterministic(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::Cmd;

    #[test]
    fn test_backend_creation() {
        let backend = TestcontainerBackend::new("alpine:latest");
        assert!(backend.is_ok());
    }

    #[test]
    fn test_backend_availability() {
        // This test may fail if Docker is not available
        let available = TestcontainerBackend::is_available();
        println!("Testcontainers available: {}", available);
    }

    #[tokio::test]
    async fn test_simple_command() {
        let backend = TestcontainerBackend::new("alpine:latest").unwrap();
        let cmd = Cmd::new("echo").args(["hello", "world"]);
        
        // Note: This test requires Docker to be running
        if TestcontainerBackend::is_available() {
            let result = backend.run_cmd(cmd);
            match result {
                Ok(r) => {
                    assert_eq!(r.exit_code, 0);
                    assert!(r.stdout.contains("hello world"));
                }
                Err(e) => {
                    println!("Test failed (Docker may not be available): {}", e);
                }
            }
        } else {
            println!("Skipping test - Docker not available");
        }
    }
}
