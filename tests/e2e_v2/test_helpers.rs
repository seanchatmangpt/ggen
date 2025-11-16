// Shared test utilities for E2E validation

use ggen_utils::error::{Error, Result};
use std::path::{Path, PathBuf};
use std::process::Command as StdCommand;
use tempfile::TempDir;

/// Setup a clean test workspace
pub fn setup_workspace() -> Result<TempDir> {
    TempDir::new().map_err(Into::into)
}

/// Verify a Rust project builds successfully
pub fn verify_rust_project_builds(project_dir: &Path) -> Result<()> {
    let output = StdCommand::new("cargo")
        .arg("check")
        .current_dir(project_dir)
        .output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(Error::new(&format!(
            "Rust project failed to build:\n{}",
            stderr
        )));
    }

    Ok(())
}

/// Verify a file exists and contains expected content
pub fn verify_file_contains(file_path: &Path, expected: &str) -> Result<()> {
    let content = std::fs::read_to_string(file_path)?;
    if !content.contains(expected) {
        return Err(Error::new(&format!(
            "File {} does not contain expected text: {}",
            file_path.display(),
            expected
        )));
    }
    Ok(())
}

/// Get the ggen binary path
#[allow(dead_code)]
pub fn ggen_bin() -> PathBuf {
    assert_cmd::cargo::cargo_bin("ggen")
}

/// Check if network is available (for online tests)
pub fn is_network_available() -> bool {
    std::net::TcpStream::connect("1.1.1.1:80").is_ok()
}
