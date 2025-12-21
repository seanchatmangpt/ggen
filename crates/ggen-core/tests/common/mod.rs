//! Common test utilities and fixtures for ggen integration testing
//! 
//! This module provides:
//! - Fixture loaders for test ontologies and configurations
//! - TempDir wrappers for test isolation
//! - Assertion helpers for file operations
//! - Test environment setup utilities

use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Test environment providing isolated file system operations
pub struct TestEnv {
    temp_dir: TempDir,
    _fixtures: Vec<String>,
}

impl TestEnv {
    /// Create a new test environment with temporary directory
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let temp_dir = TempDir::new()?;
        Ok(TestEnv {
            temp_dir,
            _fixtures: vec![],
        })
    }

    /// Get the temporary directory path
    pub fn path(&self) -> &Path {
        self.temp_dir.path()
    }

    /// Load a fixture file into test environment
    pub fn load_fixture(&self, fixture_name: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        let fixture_path = PathBuf::from("crates/ggen-core/tests/fixtures")
            .join(fixture_name);
        Ok(fs::read(fixture_path)?)
    }

    /// Assert that a file exists in the test environment
    pub fn assert_file_exists(&self, relative_path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let path = self.path().join(relative_path);
        if !path.exists() {
            return Err(format!("File does not exist: {:?}", path).into());
        }
        Ok(())
    }

    /// Assert that a file contains expected content
    pub fn assert_file_contains(
        &self,
        relative_path: &str,
        expected: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let path = self.path().join(relative_path);
        let content = fs::read_to_string(&path)?;
        if !content.contains(expected) {
            return Err(format!(
                "File {:?} does not contain: {}",
                path, expected
            ).into());
        }
        Ok(())
    }
}

impl Default for TestEnv {
    fn default() -> Self {
        Self::new().expect("Failed to create test environment")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_creates_temp_dir() {
        let env = TestEnv::new().expect("Failed to create TestEnv");
        assert!(env.path().exists(), "Temp directory should exist");
    }

    #[test]
    fn test_env_can_load_fixtures() {
        let env = TestEnv::new().expect("Failed to create TestEnv");
        let fixture = env.load_fixture("minimal.toml").expect("Failed to load fixture");
        assert!(!fixture.is_empty(), "Fixture should contain data");
    }
}
