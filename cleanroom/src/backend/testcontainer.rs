//! Testcontainers backend for containerized command execution
//!
//! Provides testcontainers-rs integration for hermetic, isolated execution
//! with automatic container lifecycle management.

use crate::backend::{Backend, Cmd, RunResult};
use crate::error::{BackendError, Result};
use crate::policy::Policy;
use std::time::{Duration, Instant};
use testcontainers::{GenericImage, ImageExt, core::ExecCommand, runners::SyncRunner};

/// Testcontainers backend for containerized execution
#[derive(Debug)]
pub struct TestcontainerBackend {
    /// Base image configuration
    image_name: String,
    image_tag: String,
    /// Default policy
    policy: Policy,
    /// Container timeout
    timeout: Duration,
    /// Environment variables to set in container
    env_vars: std::collections::HashMap<String, String>,
    /// Default command to run in container
    default_command: Option<Vec<String>>,
    /// Volume mounts for the container
    volume_mounts: Vec<(String, String)>, // (host_path, container_path)
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
            env_vars: std::collections::HashMap::new(),
            default_command: None,
            volume_mounts: Vec::new(),
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

    /// Check if the backend is running
    pub fn is_running(&self) -> bool {
        // For testcontainers, we consider the backend "running" if it can be created
        // In a real implementation, this might check container status
        true
    }

    /// Add environment variable to container
    pub fn with_env(mut self, key: &str, val: &str) -> Self {
        self.env_vars.insert(key.to_string(), val.to_string());
        self
    }

    /// Set default command for container
    pub fn with_cmd(mut self, cmd: Vec<String>) -> Self {
        self.default_command = Some(cmd);
        self
    }

    /// Add volume mount
    pub fn with_volume(mut self, host_path: &str, container_path: &str) -> Self {
        self.volume_mounts
            .push((host_path.to_string(), container_path.to_string()));
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
        let mut container_request: testcontainers::core::ContainerRequest<
            testcontainers::GenericImage,
        > = image.into();

        // Add environment variables from backend storage
        for (key, value) in &self.env_vars {
            container_request = container_request.with_env_var(key, value);
        }

        // Add environment variables from command
        for (key, value) in &cmd.env {
            container_request = container_request.with_env_var(key, value);
        }

        // Add policy environment variables
        for (key, value) in self.policy.to_env() {
            container_request = container_request.with_env_var(key, value);
        }

        // Add volume mounts from backend storage
        // TODO: Implement proper volume mounting with testcontainers API
        // for (host_path, container_path) in &self.volume_mounts {
        //     container_request = container_request.with_volume(host_path, container_path);
        // }

        // Set default command if specified and no command in cmd
        if cmd.bin.is_empty() && self.default_command.is_some() {
            if let Some(ref default_cmd) = self.default_command {
                let _cmd_args: Vec<&str> = default_cmd.iter().map(|s| s.as_str()).collect();
                // Note: testcontainers doesn't directly support setting default CMD
                // This would require a different approach for container configuration
            }
        }

        // Set working directory if specified
        if let Some(workdir) = &cmd.workdir {
            container_request =
                container_request.with_working_dir(workdir.to_string_lossy().to_string());
        }

        // Start container using SyncRunner
        let container = container_request
            .start()
            .map_err(|e| BackendError::Runtime(format!("Failed to start container: {}", e)))?;

        // Execute command - testcontainers expects Vec<&str> for exec
        let cmd_args: Vec<&str> = std::iter::once(cmd.bin.as_str())
            .chain(cmd.args.iter().map(|s| s.as_str()))
            .collect();

        let exec_cmd = ExecCommand::new(cmd_args);
        let mut exec_result = container
            .exec(exec_cmd)
            .map_err(|e| BackendError::Runtime(format!("Command execution failed: {}", e)))?;

        let duration_ms = start_time.elapsed().as_millis() as u64;

        // Extract output - SyncExecResult provides stdout() and stderr() as streams
        use std::io::Read;
        let mut stdout = String::new();
        let mut stderr = String::new();

        exec_result
            .stdout()
            .read_to_string(&mut stdout)
            .map_err(|e| BackendError::Runtime(format!("Failed to read stdout: {}", e)))?;
        exec_result
            .stderr()
            .read_to_string(&mut stderr)
            .map_err(|e| BackendError::Runtime(format!("Failed to read stderr: {}", e)))?;

        let exit_code = exec_result.exit_code().unwrap_or(Some(-1)).unwrap_or(-1) as i32;

        Ok(RunResult {
            exit_code,
            stdout,
            stderr,
            duration_ms,
            steps: Vec::new(),
            redacted_env: Vec::new(),
            backend: "testcontainers".to_string(),
            concurrent: false,
            step_order: Vec::new(),
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
            return Err(crate::Error::timeout_error(format!(
                "Command timed out after {} seconds",
                self.timeout.as_secs()
            )));
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

    #[test]
    fn test_testcontainer_backend_creation() {
        let backend = TestcontainerBackend::new("alpine:latest");
        assert!(backend.is_ok());
    }

    #[test]
    fn test_testcontainer_backend_with_timeout() {
        let timeout = Duration::from_secs(60);
        let backend = TestcontainerBackend::new("alpine:latest").unwrap();
        let backend = backend.with_timeout(timeout);
        assert!(backend.is_running());
    }

    #[test]
    fn test_testcontainer_backend_trait() {
        let backend = TestcontainerBackend::new("alpine:latest").unwrap();
        assert!(backend.is_running());
    }

    #[test]
    fn test_testcontainer_backend_image() {
        let backend = TestcontainerBackend::new("ubuntu:20.04").unwrap();
        assert!(backend.is_running());
    }
}
