//! Configuration for cleanroom testing environments

use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Configuration for cleanroom testing environment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanroomConfig {
    /// Test timeout duration
    pub timeout: Duration,

    /// Enable performance benchmarking
    pub enable_benchmarking: bool,

    /// Enable detailed logging
    pub enable_logging: bool,

    /// Test concurrency level
    pub concurrency: usize,

    /// Enable scalability testing
    pub enable_scalability: bool,

    /// Number of iterations for scalability tests
    pub scalability_iterations: usize,

    /// Enable error handling validation
    pub enable_error_validation: bool,

    /// Auto-cleanup temporary directories
    pub auto_cleanup: bool,
}

impl Default for CleanroomConfig {
    fn default() -> Self {
        Self {
            timeout: Duration::from_secs(300),
            enable_benchmarking: true,
            enable_logging: true,
            concurrency: 4,
            enable_scalability: false,
            scalability_iterations: 100,
            enable_error_validation: true,
            auto_cleanup: true,
        }
    }
}

/// Builder for cleanroom configuration
pub struct CleanroomConfigBuilder {
    config: CleanroomConfig,
}

impl CleanroomConfigBuilder {
    /// Create a new config builder
    pub fn new() -> Self {
        Self {
            config: CleanroomConfig::default(),
        }
    }

    /// Set timeout duration
    pub fn timeout(mut self, duration: Duration) -> Self {
        self.config.timeout = duration;
        self
    }

    /// Enable or disable benchmarking
    pub fn benchmarking(mut self, enabled: bool) -> Self {
        self.config.enable_benchmarking = enabled;
        self
    }

    /// Enable or disable logging
    pub fn logging(mut self, enabled: bool) -> Self {
        self.config.enable_logging = enabled;
        self
    }

    /// Set concurrency level
    pub fn concurrency(mut self, level: usize) -> Self {
        self.config.concurrency = level;
        self
    }

    /// Enable scalability testing
    pub fn scalability(mut self, enabled: bool, iterations: usize) -> Self {
        self.config.enable_scalability = enabled;
        self.config.scalability_iterations = iterations;
        self
    }

    /// Enable error validation
    pub fn error_validation(mut self, enabled: bool) -> Self {
        self.config.enable_error_validation = enabled;
        self
    }

    /// Enable or disable auto-cleanup
    pub fn auto_cleanup(mut self, enabled: bool) -> Self {
        self.config.auto_cleanup = enabled;
        self
    }

    /// Build the configuration
    pub fn build(self) -> CleanroomConfig {
        self.config
    }
}

impl Default for CleanroomConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}
