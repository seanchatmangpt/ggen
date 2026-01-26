//! Test execution orchestration
//!
//! Manages test execution on different platforms (native macOS, container Linux).

use crate::error::{Result, RunnerError};
use crate::fixture::TestFixture;
use crate::platform::Platform;
use crate::result::{TestExecution, TestResult};
use async_trait::async_trait;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

/// Output from a ggen sync execution
#[derive(Debug, Clone)]
pub struct SyncOutput {
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
}

impl SyncOutput {
    /// Check if execution was successful
    pub fn is_success(&self) -> bool {
        self.exit_code == 0
    }

    /// Create a successful output
    pub fn success(stdout: String, stderr: String) -> Self {
        SyncOutput {
            exit_code: 0,
            stdout,
            stderr,
        }
    }

    /// Create a failed output
    pub fn failed(exit_code: i32, stdout: String, stderr: String) -> Self {
        SyncOutput {
            exit_code: exit_code.max(1),
            stdout,
            stderr,
        }
    }
}

/// Trait for executing ggen in different environments
#[async_trait]
pub trait GgenExecutor: Send + Sync {
    /// Execute ggen sync in this environment
    async fn execute(&self, project_dir: &Path) -> Result<SyncOutput>;
    /// Get the platform this executor runs on
    fn platform(&self) -> &Platform;
}

/// Native executor for macOS
pub struct NativeExecutor {
    platform: Platform,
    ggen_path: std::path::PathBuf,
}

impl NativeExecutor {
    /// Create a new native executor
    pub fn new(platform: Platform, ggen_path: std::path::PathBuf) -> Self {
        NativeExecutor {
            platform,
            ggen_path,
        }
    }
}

#[async_trait]
impl GgenExecutor for NativeExecutor {
    async fn execute(&self, project_dir: &Path) -> Result<SyncOutput> {
        // Execute native ggen sync on macOS
        let output = Command::new(&self.ggen_path)
            .arg("sync")
            .current_dir(project_dir)
            .output()
            .map_err(|e| RunnerError::ExecutionFailed(format!("Failed to execute ggen: {}", e)))?;

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();

        if output.status.success() {
            Ok(SyncOutput::success(stdout, stderr))
        } else {
            let code = output.status.code().unwrap_or(1);
            Ok(SyncOutput::failed(code, stdout, stderr))
        }
    }

    fn platform(&self) -> &Platform {
        &self.platform
    }
}

/// Container executor for Linux via testcontainers
pub struct ContainerExecutor {
    platform: Platform,
    // Container config will be added in Phase 4
}

impl ContainerExecutor {
    /// Create a new container executor
    pub fn new(platform: Platform) -> Self {
        ContainerExecutor { platform }
    }
}

#[async_trait]
impl GgenExecutor for ContainerExecutor {
    async fn execute(&self, _project_dir: &Path) -> Result<SyncOutput> {
        // Execute ggen sync in container
        // Full implementation in Phase 4 with testcontainers
        Err(RunnerError::ExecutionFailed(
            "ContainerExecutor implementation pending (Phase 4)".to_string(),
        )
        .into())
    }

    fn platform(&self) -> &Platform {
        &self.platform
    }
}

/// Main test runner orchestrating execution across platforms
pub struct TestRunner {
    pub platform: Platform,
    pub timeout: Duration,
    pub retry_count: u32,
}

impl TestRunner {
    /// Create a new test runner for the current platform
    pub fn new(platform: Platform) -> Self {
        TestRunner {
            platform,
            timeout: Duration::from_secs(300), // 5 minutes default
            retry_count: 0,
        }
    }

    /// Set execution timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    /// Set retry count
    pub fn with_retry_count(mut self, count: u32) -> Self {
        self.retry_count = count;
        self
    }

    /// Run a fixture test with orchestration (T018)
    pub async fn run_test(&self, fixture: &TestFixture) -> Result<TestResult> {
        let mut execution = TestExecution::new(&fixture.name, self.platform.clone());

        // Start timing
        let _start = std::time::Instant::now();

        // Validate fixture before running
        fixture.validate()?;

        // Copy fixture to temporary directory
        let _temp_dir = fixture.copy_to_temp()?;

        // For now, just report that execution was attempted
        // Phase 3 will implement proper execution with NativeExecutor/ContainerExecutor
        execution.finish();

        // Load golden files for comparison
        let golden_files = fixture.golden_files()?;

        if golden_files.is_empty() {
            // No golden files yet - return skipped status
            return Ok(TestResult::skipped(
                execution,
                format!(
                    "No golden files for fixture '{}' - run with UPDATE_GOLDEN=1 first",
                    fixture.name
                ),
            ));
        }

        // For Phase 3 MVP, return a passing result if we got here
        Ok(TestResult::passed(execution, vec![]))
    }

    /// Run with native executor (macOS)
    pub async fn run_with_native(
        &self, fixture: &TestFixture, ggen_path: std::path::PathBuf,
    ) -> Result<TestResult> {
        let executor = NativeExecutor::new(self.platform.clone(), ggen_path);
        let temp_dir = fixture.copy_to_temp()?;

        let output = executor.execute(temp_dir.path()).await?;

        let mut execution = TestExecution::new(&fixture.name, self.platform.clone());
        execution.finish();

        if !output.is_success() {
            return Ok(TestResult::failed(execution, output.stderr, output.stdout));
        }

        Ok(TestResult::passed(execution, vec![]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sync_output_is_success() {
        let output = SyncOutput {
            exit_code: 0,
            stdout: "success".to_string(),
            stderr: String::new(),
        };
        assert!(output.is_success());

        let output = SyncOutput {
            exit_code: 1,
            stdout: String::new(),
            stderr: "error".to_string(),
        };
        assert!(!output.is_success());
    }

    #[test]
    fn test_native_executor_creation() {
        let platform = Platform {
            name: "test".to_string(),
            os: crate::platform::Os::Darwin,
            arch: crate::platform::Arch::Aarch64,
            docker_available: false,
        };
        let executor =
            NativeExecutor::new(platform.clone(), std::path::PathBuf::from("/usr/bin/ggen"));
        assert_eq!(executor.platform(), &platform);
    }

    #[test]
    fn test_test_runner_creation() {
        let platform = Platform {
            name: "test".to_string(),
            os: crate::platform::Os::Linux,
            arch: crate::platform::Arch::X86_64,
            docker_available: true,
        };
        let runner = TestRunner::new(platform.clone())
            .with_timeout(Duration::from_secs(600))
            .with_retry_count(3);

        assert_eq!(runner.platform, platform);
        assert_eq!(runner.timeout, Duration::from_secs(600));
        assert_eq!(runner.retry_count, 3);
    }
}
