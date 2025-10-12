//! Docker backend - runs commands in Docker containers with security constraints
//!
//! Provides containerized execution with deterministic surfaces:
//! non-root user, read-only rootfs, tmpfs workdir, network isolation, etc.

use super::{Backend, Cmd, RunResult};
use crate::error::{BackendError, Result};
use std::process::Command;
use std::time::Instant;

/// Docker execution backend
#[derive(Debug)]
pub struct DockerBackend {
    /// Docker image to use
    image: String,
    /// Docker binary path (defaults to "docker")
    docker_bin: String,
}

impl DockerBackend {
    /// Create a new Docker backend with the specified image
    pub fn new(image: impl Into<String>) -> Self {
        Self {
            image: image.into(),
            docker_bin: "docker".to_string(),
        }
    }

    /// Set custom Docker binary path
    pub fn docker_bin(mut self, bin: impl Into<String>) -> Self {
        self.docker_bin = bin.into();
        self
    }

    /// Check if Docker is available on the system
    pub fn is_available(&self) -> bool {
        Command::new(&self.docker_bin)
            .arg("version")
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false)
    }

    /// Pull the Docker image if not present
    pub fn ensure_image(&self) -> Result<()> {
        if !self.is_available() {
            return Err(BackendError::EngineNotFound {
                engine: self.docker_bin.clone(),
            }
            .into());
        }

        // Check if image exists locally
        let output = Command::new(&self.docker_bin)
            .arg("images")
            .arg("-q")
            .arg(&self.image)
            .output()
            .map_err(|e| BackendError::Runtime(format!("failed to list images: {}", e)))?;

        let image_exists = !output.stdout.is_empty();

        if !image_exists {
            // Pull the image
            let output = Command::new(&self.docker_bin)
                .arg("pull")
                .arg(&self.image)
                .output()
                .map_err(|e| {
                    BackendError::ImageError(format!("failed to pull {}: {}", self.image, e))
                })?;

            if !output.status.success() {
                return Err(BackendError::ImageError(format!(
                    "failed to pull {}: {}",
                    self.image,
                    String::from_utf8_lossy(&output.stderr)
                ))
                .into());
            }
        }

        Ok(())
    }
}

impl Default for DockerBackend {
    fn default() -> Self {
        Self::new("rust:1-slim")
    }
}

impl Backend for DockerBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        // Ensure image is available
        self.ensure_image()?;

        let start = Instant::now();

        let mut docker_cmd = Command::new(&self.docker_bin);

        // Base container configuration
        docker_cmd
            .arg("run")
            .arg("--rm") // Remove container after execution
            .arg("--user=1000:1000") // Non-root user
            .arg("--cap-drop=ALL") // Drop all capabilities
            .arg("--read-only") // Read-only root filesystem
            .arg("--tmpfs=/tmp:rw,nodev,nosuid,size=100m") // Tmpfs for /tmp
            .arg("--tmpfs=/workdir:rw,nodev,nosuid,size=100m") // Tmpfs for workdir
            .arg("--workdir=/workdir") // Set working directory
            .arg("--network=none") // No network access by default
            .arg("--memory=256m") // Memory limit
            .arg("--cpus=0.5") // CPU limit
            .arg("--pids-limit=128") // PID limit
            .arg("--security-opt=no-new-privileges") // Prevent privilege escalation
            .arg("--security-opt=apparmor=unconfined"); // AppArmor profile (if available)

        // Mount binary and dependencies if needed
        // For now, assume the command is available in the container
        docker_cmd.arg(&self.image);

        // Add the command to execute
        docker_cmd.arg(&cmd.bin);
        for arg in &cmd.args {
            docker_cmd.arg(arg);
        }

        // Apply environment variables
        for (key, value) in &cmd.env {
            docker_cmd.arg("--env").arg(format!("{}={}", key, value));
        }

        // Execute the containerized command
        let output = docker_cmd
            .output()
            .map_err(|e| BackendError::Runtime(format!("failed to execute docker: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;
        let result = RunResult::from_output(output, duration_ms);

        Ok(result)
    }

    fn name(&self) -> &str {
        "docker"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_docker_backend_creation() {
        let backend = DockerBackend::new("ubuntu:22.04");
        assert_eq!(backend.image, "ubuntu:22.04");
        assert_eq!(backend.name(), "docker");
    }

    #[test]
    fn test_docker_backend_default() {
        let backend = DockerBackend::default();
        assert_eq!(backend.image, "rust:1-slim");
    }

    #[test]
    fn test_docker_backend_availability() {
        let backend = DockerBackend::new("ubuntu:22.04");
        // This test may fail if Docker is not installed
        // In a real test suite, we'd mock this or skip if Docker is not available
        let _available = backend.is_available();
    }

    #[test]
    fn test_docker_backend_with_custom_bin() {
        let backend = DockerBackend::new("ubuntu:22.04").docker_bin("podman");
        assert_eq!(backend.docker_bin, "podman");
    }
}
