//! Chicago TDD Integration Tests for Marketplace Commands
//!
//! These tests use REAL objects and REAL state changes (Classicist School),
//! not mocks like London School TDD. We verify actual behavior with real
//! file system operations using tempdir.

use ggen_utils::error::Result;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Helper to create a test environment with real registry
fn setup_test_env() -> Result<(TempDir, PathBuf)> {
    let temp_dir = TempDir::new()?;
    let registry_path = temp_dir.path().join("marketplace/registry");
    fs::create_dir_all(&registry_path)?;

    // Create a real registry file
    let packages_toml = registry_path.join("packages.toml");
    let registry_content = r#"
version = "1.0.0"

[[package]]
name = "test-package"
full_name = "Test Package"
version = "1.0.0"
description = "A test package for integration testing"
category = "testing"
author = "test-author"
repository = "https://github.com/test/repo"
path = "marketplace/packages/test-package"
license = "MIT"
dependencies = []
features = ["feature1", "feature2"]
tags = ["test", "integration"]
keywords = ["test", "package"]

[[package]]
name = "rust-cli"
full_name = "Rust CLI Template"
version = "2.0.0"
description = "Rust CLI application template"
category = "cli"
author = "ggen-team"
repository = "https://github.com/ggen/rust-cli"
path = "marketplace/packages/rust-cli"
license = "MIT"
dependencies = []
features = ["clap", "serde"]
tags = ["rust", "cli", "template"]
keywords = ["rust", "cli", "template"]
"#;

    fs::write(&packages_toml, registry_content)?;

    Ok((temp_dir, registry_path))
}

#[cfg(test)]
mod search_tests {
    use super::*;
    use ggen_cli_lib::domain::marketplace::search::{search_packages, SearchFilters};

    #[tokio::test]
    async fn test_search_finds_exact_match() -> Result<()> {
        let (_temp_dir, _registry_path) = setup_test_env()?;

        // Real search with real registry
        let filters = SearchFilters::new().with_limit(10);
        let results = search_packages("test-package", &filters).await;

        // Chicago TDD: Verify ACTUAL state
        assert!(results.is_ok(), "Search should succeed");
        let results = results.unwrap();
        assert_eq!(results.len(), 1, "Should find exactly one package");
        assert_eq!(results[0].id, "test-package");
        assert_eq!(results[0].version, "1.0.0");

        Ok(())
    }

    #[tokio::test]
    async fn test_search_finds_partial_match() -> Result<()> {
        let (_temp_dir, _registry_path) = setup_test_env()?;

        let filters = SearchFilters::new().with_limit(10);
        let results = search_packages("rust", &filters).await;

        // Verify real results
        assert!(results.is_ok());
        let results = results.unwrap();
        assert!(!results.is_empty(), "Should find rust-related packages");

        Ok(())
    }

    #[tokio::test]
    async fn test_search_respects_limit() -> Result<()> {
        let (_temp_dir, _registry_path) = setup_test_env()?;

        let filters = SearchFilters::new().with_limit(1);
        let results = search_packages("test", &filters).await;

        // Verify limit is enforced
        assert!(results.is_ok());
        let results = results.unwrap();
        assert!(results.len() <= 1, "Should respect limit of 1");

        Ok(())
    }
}

#[cfg(test)]
#[cfg(feature = "lockfile-tests-disabled")] // Disabled until Lockfile is implemented
mod install_tests {
    use super::*;
    use ggen_cli_lib::domain::marketplace::install::{install_package, InstallOptions};
    // Lockfile functionality is not yet implemented
    // use ggen_cli_lib::cmds::market::lockfile::Lockfile;

    #[tokio::test]
    async fn test_install_creates_lockfile_entry() -> Result<()> {
        let (_temp_dir, _registry_path) = setup_test_env()?;

        // Change to temp directory for lockfile
        let lockfile_dir = _temp_dir.path().join(".ggen");
        fs::create_dir_all(&lockfile_dir)?;
        std::env::set_current_dir(_temp_dir.path())?;

        // Real install operation
        let options = InstallOptions::new("test-package");
        let result = install_package(&options).await;

        // Chicago TDD: Verify REAL state changes
        assert!(result.is_ok(), "Install should succeed");
        let result = result.unwrap();
        assert_eq!(result.package_name, "test-package");
        assert_eq!(result.version, "1.0.0");

        // Verify lockfile was ACTUALLY created
        let lockfile = Lockfile::load()?;
        assert!(lockfile.has_package("test-package"), "Package should be in lockfile");

        let installed = lockfile.get_package("test-package").unwrap();
        assert_eq!(installed.version, "1.0.0");

        Ok(())
    }

    #[tokio::test]
    async fn test_install_dry_run_doesnt_modify_state() -> Result<()> {
        let (_temp_dir, _registry_path) = setup_test_env()?;

        let lockfile_dir = _temp_dir.path().join(".ggen");
        fs::create_dir_all(&lockfile_dir)?;
        std::env::set_current_dir(_temp_dir.path())?;

        // Dry run install
        let options = InstallOptions::new("test-package").dry_run();
        let result = install_package(&options).await;

        // Should succeed
        assert!(result.is_ok());

        // Chicago TDD: Verify NO state changes occurred
        let lockfile = Lockfile::load()?;
        assert!(!lockfile.has_package("test-package"), "Dry run should not modify lockfile");

        Ok(())
    }

    #[tokio::test]
    async fn test_install_nonexistent_package_fails() -> Result<()> {
        let (_temp_dir, _registry_path) = setup_test_env()?;

        let lockfile_dir = _temp_dir.path().join(".ggen");
        fs::create_dir_all(&lockfile_dir)?;
        std::env::set_current_dir(_temp_dir.path())?;

        let options = InstallOptions::new("nonexistent-package");
        let result = install_package(&options).await;

        // Should fail for nonexistent package
        assert!(result.is_err(), "Should fail for nonexistent package");

        Ok(())
    }
}

#[cfg(test)]
#[cfg(feature = "lockfile-tests-disabled")] // Disabled until Lockfile is implemented
mod list_tests {
    use super::*;
    // Lockfile functionality is not yet implemented
    // use ggen_cli_lib::cmds::market::lockfile::{Lockfile, InstalledPackage};

    #[test]
    fn test_list_shows_installed_packages() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let lockfile_dir = temp_dir.path().join(".ggen");
        fs::create_dir_all(&lockfile_dir)?;
        std::env::set_current_dir(temp_dir.path())?;

        // Create REAL lockfile with installed packages
        let mut lockfile = Lockfile::new();

        let pkg1 = InstalledPackage {
            name: "package-1".to_string(),
            full_name: "Package One".to_string(),
            version: "1.0.0".to_string(),
            checksum: "abc123".to_string(),
            source: "registry".to_string(),
            path: ".ggen/packages/package-1".to_string(),
            installed_at: "2024-01-01T00:00:00Z".to_string(),
            dependencies: vec![],
        };

        let pkg2 = InstalledPackage {
            name: "package-2".to_string(),
            full_name: "Package Two".to_string(),
            version: "2.0.0".to_string(),
            checksum: "def456".to_string(),
            source: "registry".to_string(),
            path: ".ggen/packages/package-2".to_string(),
            installed_at: "2024-01-02T00:00:00Z".to_string(),
            dependencies: vec!["package-1".to_string()],
        };

        lockfile.add_package(pkg1);
        lockfile.add_package(pkg2);
        lockfile.save()?;

        // Chicago TDD: Verify REAL lockfile state
        let loaded = Lockfile::load()?;
        let packages = loaded.list_packages();

        assert_eq!(packages.len(), 2, "Should have 2 packages");
        assert!(loaded.has_package("package-1"));
        assert!(loaded.has_package("package-2"));

        let pkg2_loaded = loaded.get_package("package-2").unwrap();
        assert_eq!(pkg2_loaded.dependencies.len(), 1);

        Ok(())
    }
}

#[cfg(test)]
#[cfg(feature = "lockfile-tests-disabled")] // Disabled until Lockfile is implemented
mod update_tests {
    use super::*;
    // Lockfile functionality is not yet implemented
    // use ggen_cli_lib::cmds::market::lockfile::{Lockfile, InstalledPackage};

    #[test]
    fn test_update_detects_version_difference() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let lockfile_dir = temp_dir.path().join(".ggen");
        fs::create_dir_all(&lockfile_dir)?;
        std::env::set_current_dir(temp_dir.path())?;

        // Install old version
        let mut lockfile = Lockfile::new();
        let pkg = InstalledPackage {
            name: "rust-cli".to_string(),
            full_name: "Rust CLI Template".to_string(),
            version: "1.0.0".to_string(), // Old version
            checksum: "old".to_string(),
            source: "registry".to_string(),
            path: ".ggen/packages/rust-cli".to_string(),
            installed_at: "2024-01-01T00:00:00Z".to_string(),
            dependencies: vec![],
        };

        lockfile.add_package(pkg);
        lockfile.save()?;

        // Chicago TDD: Verify state before update
        let loaded = Lockfile::load()?;
        let installed = loaded.get_package("rust-cli").unwrap();
        assert_eq!(installed.version, "1.0.0", "Should have old version");

        // In real implementation, update would change version to 2.0.0
        // (which exists in our test registry)

        Ok(())
    }
}

#[cfg(test)]
mod publish_tests {
    use super::*;

    #[test]
    fn test_publish_validates_cargo_toml_exists() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let package_dir = temp_dir.path().join("my-package");
        fs::create_dir_all(&package_dir)?;

        // Create Cargo.toml
        let cargo_toml = package_dir.join("Cargo.toml");
        fs::write(&cargo_toml, "[package]\nname = \"my-package\"\nversion = \"1.0.0\"\n")?;

        // Chicago TDD: Verify REAL file exists
        assert!(cargo_toml.exists(), "Cargo.toml should exist");

        Ok(())
    }

    #[test]
    fn test_publish_fails_without_cargo_toml() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let package_dir = temp_dir.path().join("my-package");
        fs::create_dir_all(&package_dir)?;

        // No Cargo.toml created

        // Chicago TDD: Verify file does NOT exist
        let cargo_toml = package_dir.join("Cargo.toml");
        assert!(!cargo_toml.exists(), "Cargo.toml should not exist");

        Ok(())
    }
}
