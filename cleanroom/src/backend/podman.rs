//! Podman backend - runs commands in Podman containers with security constraints
//!
//! Provides containerized execution with deterministic surfaces using Podman.
//! Mirrors Docker backend behavior for compatibility.

use super::{Backend, Cmd, RunResult};
use crate::error::{BackendError, CleanroomError, Result};
use std::process::Command;
use std::time::Instant;

/// Podman execution backend
#[derive(Debug)]
pub struct PodmanBackend {
    /// Podman image to use
    image: String,
    /// Podman binary path (defaults to "podman")
    podman_bin: String,
}

impl PodmanBackend {
    /// Create a new Podman backend with the specified image
    pub fn new(image: impl Into<String>) -> Self {
        Self {
            image: image.into(),
            podman_bin: "podman".to_string(),
        }
    }

    /// Set custom Podman binary path
    pub fn podman_bin(mut self, bin: impl Into<String>) -> Self {
        self.podman_bin = bin.into();
        self
    }

    /// Check if Podman is available on the system
    pub fn is_available(&self) -> bool {
        Command::new(&self.podman_bin)
            .arg("version")
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false)
    }

    /// Pull the Podman image if not present
    pub fn ensure_image(&self) -> Result<()> {
        if !self.is_available() {
            return Err(BackendError::EngineNotFound {
                engine: self.podman_bin.clone(),
            }
            .into());
        }

        // Check if image exists locally
        let output = Command::new(&self.podman_bin)
            .arg("images")
            .arg("-q")
            .arg(&self.image)
            .output()
            .map_err(|e| BackendError::Runtime(format!("failed to list images: {}", e)))?;

        let image_exists = !output.stdout.is_empty();

        if !image_exists {
            // Pull the image
            let output = Command::new(&self.podman_bin)
                .arg("pull")
                .arg(&self.image)
                .output()
                .map_err(|e| {
                    CleanroomError::Backend(BackendError::ImageError(format!(
                        "failed to pull {}: {}",
                        self.image, e
                    )))
                })?;

            if !output.status.success() {
                return Err(CleanroomError::Backend(BackendError::ImageError(format!(
                    "failed to pull {}: {}",
                    self.image,
                    String::from_utf8_lossy(&output.stderr)
                ))));
            }
        }

        Ok(())
    }
}

impl Default for PodmanBackend {
    fn default() -> Self {
        Self::new("rust:1-slim")
    }
}

impl Backend for PodmanBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        // Ensure image is available
        self.ensure_image()?;

        let start = Instant::now();

        let mut podman_cmd = Command::new(&self.podman_bin);

        // Base container configuration (Podman equivalent of Docker flags)
        podman_cmd
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
            .arg("--security-opt=no-new-privileges"); // Prevent privilege escalation

        // Mount binary and dependencies if needed
        // For now, assume the command is available in the container
        podman_cmd.arg(&self.image);

        // Add the command to execute
        podman_cmd.arg(&cmd.bin);
        for arg in &cmd.args {
            podman_cmd.arg(arg);
        }

        // Apply environment variables
        for (key, value) in &cmd.env {
            podman_cmd.arg("--env").arg(format!("{}={}", key, value));
        }

        // Execute the containerized command
        let output = podman_cmd
            .output()
            .map_err(|e| BackendError::Runtime(format!("failed to execute podman: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;
        let result = RunResult::from_output(output, duration_ms);

        Ok(result)
    }

    fn name(&self) -> &str {
        "podman"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_podman_backend_creation() {
        let backend = PodmanBackend::new("ubuntu:22.04");
        assert_eq!(backend.image, "ubuntu:22.04");
        assert_eq!(backend.name(), "podman");
    }

    #[test]
    fn test_podman_backend_default() {
        let backend = PodmanBackend::default();
        assert_eq!(backend.image, "rust:1-slim");
    }

    #[test]
    fn test_podman_backend_availability() {
        let backend = PodmanBackend::new("ubuntu:22.04");
        // This test may fail if Podman is not installed
        // In a real test suite, we'd mock this or skip if Podman is not available
        let _available = backend.is_available();
    }

    #[test]
    fn test_podman_backend_with_custom_bin() {
        let backend = PodmanBackend::new("ubuntu:22.04").podman_bin("docker");
        assert_eq!(backend.podman_bin, "docker");
    }
}
