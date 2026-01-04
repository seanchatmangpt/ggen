//! Poka-Yoke (Error Prevention) Framework
//!
//! Implements comprehensive error prevention and validation mechanisms to ensure
//! tests fail fast and provide clear error messages.

use crate::error::{ContainerError, PlatformError, Result};
use std::process::Command;
use std::time::{Duration, Instant};

/// Pre-flight checks to verify prerequisites before running tests
#[derive(Debug, Clone)]
pub struct PreFlightChecks {
    /// Whether to check Docker availability
    pub check_docker: bool,
    /// Whether to check act availability
    pub check_act: bool,
    /// Whether to check testcontainers availability
    pub check_testcontainers: bool,
    /// Timeout for checks
    pub timeout: Duration,
}

impl Default for PreFlightChecks {
    fn default() -> Self {
        PreFlightChecks {
            check_docker: true,
            check_act: false,
            check_testcontainers: true,
            timeout: Duration::from_secs(10),
        }
    }
}

impl PreFlightChecks {
    /// Create a new pre-flight check configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable Docker check
    pub fn with_docker_check(mut self, check: bool) -> Self {
        self.check_docker = check;
        self
    }

    /// Enable act check
    pub fn with_act_check(mut self, check: bool) -> Self {
        self.check_act = check;
        self
    }

    /// Enable testcontainers check
    pub fn with_testcontainers_check(mut self, check: bool) -> Self {
        self.check_testcontainers = check;
        self
    }

    /// Set timeout for checks
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    /// Run all enabled pre-flight checks
    pub fn run(&self) -> Result<()> {
        let start = Instant::now();

        if self.check_docker {
            self.check_docker_available()?;
        }

        if self.check_act {
            self.check_act_available()?;
        }

        if self.check_testcontainers {
            self.check_testcontainers_available()?;
        }

        let elapsed = start.elapsed();
        if elapsed > self.timeout {
            return Err(PlatformError::DetectionFailed(format!(
                "Pre-flight checks exceeded timeout: {:?}",
                elapsed
            ))
            .into());
        }

        Ok(())
    }

    /// Check if Docker is available and running
    fn check_docker_available(&self) -> Result<()> {
        let output = Command::new("docker")
            .arg("ps")
            .output()
            .map_err(|_| PlatformError::DockerUnavailable)?;

        if !output.status.success() {
            return Err(PlatformError::DockerUnavailable.into());
        }

        Ok(())
    }

    /// Check if act is installed
    fn check_act_available(&self) -> Result<()> {
        let output = Command::new("act")
            .arg("--version")
            .output()
            .map_err(|_| {
                PlatformError::DetectionFailed(
                    "act is not installed. Install with: brew install act".to_string(),
                )
            })?;

        if !output.status.success() {
            return Err(PlatformError::DetectionFailed(
                "act command failed".to_string(),
            )
            .into());
        }

        Ok(())
    }

    /// Check if testcontainers can create containers
    fn check_testcontainers_available(&self) -> Result<()> {
        // This is a basic check - in practice, we'd try to create a test container
        // For now, we verify Docker is available (which testcontainers requires)
        self.check_docker_available()
    }
}

/// Timeout wrapper for operations
pub struct TimeoutGuard {
    timeout: Duration,
    start: Instant,
}

impl TimeoutGuard {
    /// Create a new timeout guard
    pub fn new(timeout: Duration) -> Self {
        TimeoutGuard {
            timeout,
            start: Instant::now(),
        }
    }

    /// Check if timeout has been exceeded
    pub fn check(&self) -> Result<()> {
        if self.start.elapsed() > self.timeout {
            return Err(ContainerError::Timeout(self.timeout).into());
        }
        Ok(())
    }

    /// Get elapsed time
    pub fn elapsed(&self) -> Duration {
        self.start.elapsed()
    }

    /// Get remaining time
    pub fn remaining(&self) -> Duration {
        self.timeout.saturating_sub(self.elapsed())
    }
}

/// Error detection and fail-fast behavior
pub struct ErrorDetector {
    /// Maximum number of errors before failing
    max_errors: usize,
    /// Current error count
    error_count: usize,
    /// Error messages
    errors: Vec<String>,
}

impl ErrorDetector {
    /// Create a new error detector
    pub fn new(max_errors: usize) -> Self {
        ErrorDetector {
            max_errors,
            error_count: 0,
            errors: Vec::new(),
        }
    }

    /// Record an error (fail-fast if max_errors exceeded)
    pub fn record_error(&mut self, error: String) -> Result<()> {
        self.error_count += 1;
        self.errors.push(error);

        if self.error_count > self.max_errors {
            return Err(PlatformError::DetectionFailed(format!(
                "Too many errors ({}): {}",
                self.error_count,
                self.errors.join("; ")
            ))
            .into());
        }

        Ok(())
    }

    /// Check if any errors were recorded
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get all error messages
    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    /// Clear all errors
    pub fn clear(&mut self) {
        self.error_count = 0;
        self.errors.clear();
    }
}

/// Cleanup verification to ensure resources are properly cleaned up
pub struct CleanupVerifier {
    /// Initial resource count (e.g., containers)
    initial_count: usize,
    /// Resource identifier
    resource_type: String,
}

impl CleanupVerifier {
    /// Create a new cleanup verifier
    pub fn new(resource_type: impl Into<String>) -> Result<Self> {
        let resource_type = resource_type.into();
        let initial_count = Self::count_resources(&resource_type)?;

        Ok(CleanupVerifier {
            initial_count,
            resource_type,
        })
    }

    /// Count current resources
    fn count_resources(resource_type: &str) -> Result<usize> {
        match resource_type {
            "docker_containers" => {
                let output = Command::new("docker")
                    .args(&["ps", "-a", "--format", "{{.ID}}"])
                    .output()
                    .map_err(|_| PlatformError::DockerUnavailable)?;

                Ok(String::from_utf8_lossy(&output.stdout)
                    .lines()
                    .filter(|l| !l.is_empty())
                    .count())
            }
            _ => Err(PlatformError::DetectionFailed(format!(
                "Unknown resource type: {}",
                resource_type
            ))
            .into()),
        }
    }

    /// Verify cleanup (check that resource count hasn't increased significantly)
    pub fn verify(&self, max_increase: usize) -> Result<()> {
        let current_count = Self::count_resources(&self.resource_type)?;
        let increase = current_count.saturating_sub(self.initial_count);

        if increase > max_increase {
            return Err(ContainerError::CleanupFailed(format!(
                "Resource cleanup failed: {} {} increased by {} (max: {})",
                self.resource_type, self.resource_type, increase, max_increase
            ))
            .into());
        }

        Ok(())
    }

    /// Get initial count
    pub fn initial_count(&self) -> usize {
        self.initial_count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pre_flight_checks_default() {
        let checks = PreFlightChecks::default();
        assert!(checks.check_docker);
        assert!(!checks.check_act);
        assert!(checks.check_testcontainers);
    }

    #[test]
    fn test_timeout_guard_creation() {
        let guard = TimeoutGuard::new(Duration::from_secs(10));
        assert!(guard.check().is_ok());
        assert!(guard.elapsed() < Duration::from_secs(1));
    }

    #[test]
    fn test_error_detector() {
        let mut detector = ErrorDetector::new(3);
        assert!(!detector.has_errors());

        assert!(detector.record_error("Error 1".to_string()).is_ok());
        assert!(detector.has_errors());
        assert_eq!(detector.errors().len(), 1);

        assert!(detector.record_error("Error 2".to_string()).is_ok());
        assert!(detector.record_error("Error 3".to_string()).is_ok());
        assert!(detector.record_error("Error 4".to_string()).is_err()); // Exceeds max
    }
}


