//! v3 Refactor Test Utilities
//!
//! Common utilities for Chicago TDD approach: real objects, state verification

use std::path::{Path, PathBuf};
use std::fs;
use tempfile::TempDir;

/// Test environment for creating real files and directories
pub struct TestEnvironment {
    pub temp_dir: TempDir,
}

impl TestEnvironment {
    /// Create a new test environment with temporary directory
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            temp_dir: TempDir::new()?,
        })
    }

    /// Create a real template file
    pub fn create_template(&self, name: &str, content: &str) -> Result<PathBuf> {
        let path = self.temp_dir.path().join(name);
        fs::write(&path, content)?;
        Ok(path)
    }

    /// Create a real RDF file
    pub fn create_rdf(&self, name: &str, content: &str) -> Result<PathBuf> {
        let path = self.temp_dir.path().join(name);
        fs::write(&path, content)?;
        Ok(path)
    }

    /// Create output directory
    pub fn create_output_dir(&self) -> PathBuf {
        let path = self.temp_dir.path().join("output");
        fs::create_dir_all(&path).unwrap();
        path
    }

    /// Get path to temp directory
    pub fn path(&self) -> &Path {
        self.temp_dir.path()
    }
}

/// Save baseline for comparison
pub fn save_baseline(name: &str, content: &str) -> Result<(), Box<dyn std::error::Error>> {
    let baseline_dir = PathBuf::from("tests/v3-refactor/baselines/outputs");
    fs::create_dir_all(&baseline_dir)?;
    fs::write(baseline_dir.join(name), content)?;
    Ok(())
}

/// Load baseline for comparison
pub fn load_baseline(name: &str) -> Result<String, Box<dyn std::error::Error>> {
    let baseline_path = PathBuf::from("tests/v3-refactor/baselines/outputs").join(name);
    Ok(fs::read_to_string(&baseline_path)?)
}

/// Compare actual output with baseline
pub fn compare_with_baseline(name: &str, actual: &str) -> Result<bool, Box<dyn std::error::Error>> {
    let baseline = load_baseline(name)?;
    Ok(actual == baseline)
}

