//! Cleanroom environment management

use crate::{CleanroomConfig, PerformanceMetrics, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::time::Instant;
use tempfile::TempDir;
use uuid::Uuid;

/// Cleanroom test environment
pub struct CleanroomEnv {
    /// Unique test ID
    pub test_id: Uuid,

    /// Temporary directory for isolation
    temp_dir: TempDir,

    /// Configuration
    config: CleanroomConfig,

    /// Performance metrics
    metrics: Option<PerformanceMetrics>,

    /// Start time
    start_time: Instant,
}

/// Test execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    /// Test ID
    pub test_id: Uuid,

    /// Test status
    pub status: TestStatus,

    /// Duration in milliseconds
    pub duration_ms: u64,

    /// Performance metrics
    pub metrics: Option<PerformanceMetrics>,

    /// Errors encountered
    pub errors: Vec<String>,
}

/// Test execution status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TestStatus {
    /// Test passed
    Passed,

    /// Test failed
    Failed,

    /// Test skipped
    Skipped,

    /// Test timed out
    Timeout,
}

impl CleanroomEnv {
    /// Create a new cleanroom environment
    pub fn new(config: CleanroomConfig) -> Result<Self> {
        let test_id = Uuid::new_v4();
        let temp_dir = TempDir::new()?;

        let metrics = if config.enable_benchmarking {
            Some(PerformanceMetrics::new())
        } else {
            None
        };

        if config.enable_logging {
            tracing::info!(
                test_id = %test_id,
                temp_dir = ?temp_dir.path(),
                "Created cleanroom environment"
            );
        }

        Ok(Self {
            test_id,
            temp_dir,
            config,
            metrics,
            start_time: Instant::now(),
        })
    }

    /// Get the cleanroom temporary directory path
    pub fn path(&self) -> &Path {
        self.temp_dir.path()
    }

    /// Create a subdirectory in the cleanroom
    pub fn create_dir(&self, name: &str) -> Result<PathBuf> {
        let path = self.temp_dir.path().join(name);
        std::fs::create_dir_all(&path)?;
        Ok(path)
    }

    /// Write a file in the cleanroom
    pub fn write_file(&self, path: &str, content: &str) -> Result<PathBuf> {
        let file_path = self.temp_dir.path().join(path);

        // Create parent directories if needed
        if let Some(parent) = file_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        std::fs::write(&file_path, content)?;
        Ok(file_path)
    }

    /// Read a file from the cleanroom
    pub fn read_file(&self, path: &str) -> Result<String> {
        let file_path = self.temp_dir.path().join(path);
        Ok(std::fs::read_to_string(file_path)?)
    }

    /// Run a test function in the cleanroom
    pub fn run_test<F>(&mut self, test_fn: F) -> TestResult
    where
        F: FnOnce(&Self) -> Result<()>,
    {
        let start = Instant::now();
        let mut errors = Vec::new();

        let status = match test_fn(self) {
            Ok(()) => TestStatus::Passed,
            Err(e) => {
                errors.push(e.to_string());
                TestStatus::Failed
            }
        };

        let duration_ms = start.elapsed().as_millis() as u64;

        TestResult {
            test_id: self.test_id,
            status,
            duration_ms,
            metrics: self.metrics.clone(),
            errors,
        }
    }

    /// Get elapsed time since cleanroom creation
    pub fn elapsed_ms(&self) -> u64 {
        self.start_time.elapsed().as_millis() as u64
    }

    /// Record a performance metric
    pub fn record_metric(&mut self, name: impl Into<String>, value: f64) {
        if let Some(ref mut metrics) = self.metrics {
            metrics.record(name, value);
        }
    }

    /// Get test ID
    pub fn test_id(&self) -> Uuid {
        self.test_id
    }

    /// Get configuration
    pub fn config(&self) -> &CleanroomConfig {
        &self.config
    }
}

impl Drop for CleanroomEnv {
    fn drop(&mut self) {
        if self.config.enable_logging {
            tracing::info!(
                test_id = %self.test_id,
                duration_ms = self.elapsed_ms(),
                "Cleanroom environment cleanup"
            );
        }
        // TempDir automatically cleans up on drop
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cleanroom_env_creation() {
        let env = CleanroomEnv::new(CleanroomConfig::default())
            .expect("Failed to create cleanroom");

        assert!(env.path().exists());
    }

    #[test]
    fn test_cleanroom_file_operations() {
        let env = CleanroomEnv::new(CleanroomConfig::default())
            .expect("Failed to create cleanroom");

        // Write file
        let file_path = env.write_file("test.txt", "Hello, cleanroom!")
            .expect("Failed to write file");

        assert!(file_path.exists());

        // Read file
        let content = env.read_file("test.txt")
            .expect("Failed to read file");

        assert_eq!(content, "Hello, cleanroom!");
    }

    #[test]
    fn test_cleanroom_test_execution() {
        let mut env = CleanroomEnv::new(CleanroomConfig::default())
            .expect("Failed to create cleanroom");

        let result = env.run_test(|env| {
            env.write_file("test.txt", "test content")?;
            let content = env.read_file("test.txt")?;
            assert_eq!(content, "test content");
            Ok(())
        });

        assert_eq!(result.status, TestStatus::Passed);
        assert_eq!(result.errors.len(), 0);
    }
}
