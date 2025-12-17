//! Container configuration and lifecycle management
//!
//! Manages testcontainer configuration, volume mounts, and lifecycle operations.

use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;
use crate::error::{ContainerError, Result};

/// Volume mount configuration
#[derive(Debug, Clone)]
pub struct VolumeMount {
    /// Host path
    pub host_path: PathBuf,
    /// Container path
    pub container_path: PathBuf,
    /// Whether mounted as read-only
    pub read_only: bool,
}

impl VolumeMount {
    /// Create a new volume mount
    pub fn new(host_path: impl Into<PathBuf>, container_path: impl Into<PathBuf>) -> Self {
        VolumeMount {
            host_path: host_path.into(),
            container_path: container_path.into(),
            read_only: false,
        }
    }

    /// Create a read-only volume mount
    pub fn read_only(mut self) -> Self {
        self.read_only = true;
        self
    }
}

/// Container configuration for testcontainers
#[derive(Debug, Clone)]
pub struct ContainerConfig {
    /// Docker image name (e.g., "ubuntu")
    pub image: String,
    /// Image tag (e.g., "22.04")
    pub tag: String,
    /// Environment variables
    pub env_vars: HashMap<String, String>,
    /// Volume mounts
    pub volumes: Vec<VolumeMount>,
    /// Container startup timeout
    pub startup_timeout: Duration,
    /// Command override (entrypoint)
    pub command: Option<Vec<String>>,
    /// Container cleanup on success
    pub cleanup_on_success: bool,
    /// Container cleanup on failure
    pub cleanup_on_failure: bool,
    /// Keep logs before cleanup
    pub keep_logs: bool,
}

impl Default for ContainerConfig {
    fn default() -> Self {
        ContainerConfig {
            image: "ubuntu".to_string(),
            tag: "22.04".to_string(),
            env_vars: HashMap::new(),
            volumes: Vec::new(),
            startup_timeout: Duration::from_secs(120),
            command: None,
            cleanup_on_success: true,
            cleanup_on_failure: true,
            keep_logs: true,
        }
    }
}

impl ContainerConfig {
    /// Create a new default container config
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the Docker image
    pub fn with_image(mut self, image: impl Into<String>) -> Self {
        self.image = image.into();
        self
    }

    /// Set the image tag
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tag = tag.into();
        self
    }

    /// Add an environment variable
    pub fn with_env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env_vars.insert(key.into(), value.into());
        self
    }

    /// Add multiple environment variables
    pub fn with_envs(mut self, envs: HashMap<String, String>) -> Self {
        self.env_vars.extend(envs);
        self
    }

    /// Add a volume mount
    pub fn with_volume(mut self, volume: VolumeMount) -> Self {
        self.volumes.push(volume);
        self
    }

    /// Set startup timeout
    pub fn with_startup_timeout(mut self, timeout: Duration) -> Self {
        self.startup_timeout = timeout;
        self
    }

    /// Set the container command
    pub fn with_command(mut self, command: Vec<String>) -> Self {
        self.command = Some(command);
        self
    }

    /// Validate container configuration
    pub fn validate(&self) -> Result<()> {
        if self.image.is_empty() {
            return Err(ContainerError::Configuration(
                "Image name cannot be empty".to_string(),
            ).into());
        }

        if self.tag.is_empty() {
            return Err(ContainerError::Configuration(
                "Image tag cannot be empty".to_string(),
            ).into());
        }

        for volume in &self.volumes {
            if !volume.host_path.to_string_lossy().chars().any(|_| true) {
                return Err(ContainerError::Configuration(
                    format!("Invalid host path: {:?}", volume.host_path),
                ).into());
            }

            if !volume.container_path.to_string_lossy().chars().any(|_| true) {
                return Err(ContainerError::Configuration(
                    format!("Invalid container path: {:?}", volume.container_path),
                ).into());
            }
        }

        Ok(())
    }

    /// Get the full image reference
    pub fn image_ref(&self) -> String {
        format!("{}:{}", self.image, self.tag)
    }

    /// Check if any volumes are configured
    pub fn has_volumes(&self) -> bool {
        !self.volumes.is_empty()
    }

    /// Check if any environment variables are configured
    pub fn has_env_vars(&self) -> bool {
        !self.env_vars.is_empty()
    }

    /// Set cleanup behavior on success
    pub fn cleanup_on_success(mut self, cleanup: bool) -> Self {
        self.cleanup_on_success = cleanup;
        self
    }

    /// Set cleanup behavior on failure
    pub fn cleanup_on_failure(mut self, cleanup: bool) -> Self {
        self.cleanup_on_failure = cleanup;
        self
    }

    /// Set whether to keep logs before cleanup
    pub fn keep_logs(mut self, keep: bool) -> Self {
        self.keep_logs = keep;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_volume_mount_creation() {
        let mount = VolumeMount::new("/host/path", "/container/path");
        assert_eq!(mount.host_path, PathBuf::from("/host/path"));
        assert_eq!(mount.container_path, PathBuf::from("/container/path"));
        assert!(!mount.read_only);
    }

    #[test]
    fn test_volume_mount_read_only() {
        let mount = VolumeMount::new("/host/path", "/container/path").read_only();
        assert!(mount.read_only);
    }

    #[test]
    fn test_container_config_default() {
        let config = ContainerConfig::default();
        assert_eq!(config.image, "ubuntu");
        assert_eq!(config.tag, "22.04");
        assert_eq!(config.startup_timeout, Duration::from_secs(120));
        assert!(config.cleanup_on_success);
        assert!(config.cleanup_on_failure);
    }

    #[test]
    fn test_container_config_builder() {
        let config = ContainerConfig::new()
            .with_image("alpine")
            .with_tag("latest")
            .with_env("KEY", "value")
            .with_volume(VolumeMount::new("/src", "/dst"));

        assert_eq!(config.image, "alpine");
        assert_eq!(config.tag, "latest");
        assert_eq!(config.env_vars.get("KEY"), Some(&"value".to_string()));
        assert_eq!(config.volumes.len(), 1);
    }

    #[test]
    fn test_container_config_image_ref() {
        let config = ContainerConfig::new()
            .with_image("ubuntu")
            .with_tag("22.04");
        assert_eq!(config.image_ref(), "ubuntu:22.04");
    }

    #[test]
    fn test_container_config_validate() {
        let config = ContainerConfig::new()
            .with_image("ubuntu")
            .with_tag("22.04")
            .with_volume(VolumeMount::new("/host", "/container"));

        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_container_config_has_volumes() {
        let config1 = ContainerConfig::new();
        assert!(!config1.has_volumes());

        let config2 = ContainerConfig::new()
            .with_volume(VolumeMount::new("/host", "/container"));
        assert!(config2.has_volumes());
    }

    #[test]
    fn test_container_config_has_env_vars() {
        let config1 = ContainerConfig::new();
        assert!(!config1.has_env_vars());

        let config2 = ContainerConfig::new().with_env("KEY", "value");
        assert!(config2.has_env_vars());
    }
}
