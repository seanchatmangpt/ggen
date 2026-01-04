//! Fixture loading utilities for gpack tests
//!
//! Feature: 014-marketplace-gpack T003

use std::path::{Path, PathBuf};
use std::fs;

/// Get the path to the fixtures directory
pub fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("gpack")
        .join("fixtures")
}

/// Get the path to manifest fixtures
pub fn manifest_fixtures_dir() -> PathBuf {
    fixtures_dir().join("manifests")
}

/// Get the path to lockfile fixtures
pub fn lockfile_fixtures_dir() -> PathBuf {
    fixtures_dir().join("lockfiles")
}

/// Get the path to FMEA fixtures
pub fn fmea_fixtures_dir() -> PathBuf {
    fixtures_dir().join("fmea")
}

/// Load a manifest fixture by name
pub fn load_manifest_fixture(name: &str) -> String {
    let path = manifest_fixtures_dir().join(name);
    fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to load manifest fixture {:?}: {}", path, e))
}

/// Load a lockfile fixture by name
pub fn load_lockfile_fixture(name: &str) -> String {
    let path = lockfile_fixtures_dir().join(name);
    fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to load lockfile fixture {:?}: {}", path, e))
}

/// Load a FMEA fixture by name
pub fn load_fmea_fixture(name: &str) -> String {
    let path = fmea_fixtures_dir().join(name);
    fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to load FMEA fixture {:?}: {}", path, e))
}

/// Create a temporary directory for test package operations
pub fn create_temp_cache_dir() -> tempfile::TempDir {
    tempfile::TempDir::new()
        .expect("Failed to create temporary directory")
}

/// Generate a minimal valid manifest TOML
pub fn minimal_valid_manifest(name: &str, version: &str) -> String {
    format!(r#"
[package]
name = "{name}"
version = "{version}"
description = "Test package"
authors = ["Test Author <test@example.com>"]
license = "MIT"
quality_tier = "bronze"
"#)
}

/// Generate a minimal valid lockfile
pub fn minimal_lockfile() -> String {
    r#"
version = 1

[metadata]
ggen_version = "5.0.2"

[packages]
"#.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixtures_dir_exists() {
        assert!(fixtures_dir().exists(), "Fixtures directory should exist");
    }

    #[test]
    fn test_load_valid_manifest() {
        let content = load_manifest_fixture("valid_simple.toml");
        assert!(content.contains("[package]"));
        assert!(content.contains("hello-world-gpack"));
    }

    #[test]
    fn test_minimal_valid_manifest() {
        let manifest = minimal_valid_manifest("test-gpack", "1.0.0");
        assert!(manifest.contains("test-gpack"));
        assert!(manifest.contains("1.0.0"));
    }
}
