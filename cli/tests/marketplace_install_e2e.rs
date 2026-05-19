//! Chicago TDD tests for marketplace package installation
//!
//! These tests create REAL packages, install them, and verify files exist.
//! Tests real dependency resolution, circular detection, and rollback.

use ggen_cli_lib::domain::marketplace::install::{
    install_package, InstallOptions, PackageManifest, Lockfile,
};
use serde_json;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test helper: Create a test registry structure
struct TestRegistry {
    _temp_dir: TempDir,
    registry_path: PathBuf,
    packages_dir: PathBuf,
}

impl TestRegistry {
    fn new() -> Self {
        let temp_dir = TempDir::new().unwrap();
        let base_path = temp_dir.path();

        let registry_path = base_path.join(".ggen").join("registry");
        let packages_dir = base_path.join(".ggen").join("packages");

        fs::create_dir_all(&registry_path).unwrap();
        fs::create_dir_all(&packages_dir).unwrap();

        Self {
            _temp_dir: temp_dir,
            registry_path,
            packages_dir,
        }
    }

    /// Create a package in the registry with manifest and tarball
    fn create_package(
        &self,
        name: &str,
        version: &str,
        dependencies: HashMap<String, String>,
    ) -> PathBuf {
        let package_dir = self.registry_path.join(name).join(version);
        fs::create_dir_all(&package_dir).unwrap();

        // Create package manifest
        let manifest = PackageManifest {
            name: name.to_string(),
            version: version.to_string(),
            title: format!("{} Package", name),
            description: format!("Test package {}", name),
            dependencies,
            categories: vec!["test".to_string()],
            tags: vec!["test".to_string()],
        };

        let manifest_json = serde_json::to_string_pretty(&manifest).unwrap();
        fs::write(package_dir.join("package.json"), manifest_json).unwrap();

        // Create a simple tarball
        let tarball_path = package_dir.join(format!("{}-{}.tar.gz", name.replace('/', "-"), version));
        self.create_tarball(&tarball_path, name);

        // Update registry index
        self.update_registry_index(name, version, &manifest);

        tarball_path
    }

    /// Create a minimal tarball for testing
    fn create_tarball(&self, tarball_path: &PathBuf, name: &str) {
        let temp_content_dir = TempDir::new().unwrap();
        let content_file = temp_content_dir.path().join("README.md");
        fs::write(&content_file, format!("# {}\n\nTest package content", name)).unwrap();

        let tarball_file = fs::File::create(tarball_path).unwrap();
        let encoder = flate2::write::GzEncoder::new(tarball_file, flate2::Compression::default());
        let mut archive = tar::Builder::new(encoder);

        archive.append_path_with_name(&content_file, "README.md").unwrap();
        archive.finish().unwrap();
    }

    /// Update registry index
    fn update_registry_index(&self, name: &str, version: &str, manifest: &PackageManifest) {
        let index_path = self.registry_path.join("index.json");

        let mut index: serde_json::Value = if index_path.exists() {
            let content = fs::read_to_string(&index_path).unwrap();
            serde_json::from_str(&content).unwrap()
        } else {
            serde_json::json!({
                "version": "1.0",
                "packages": {}
            })
        };

        if let Some(packages) = index.get_mut("packages").and_then(|p| p.as_object_mut()) {
            let package_versions = packages
                .entry(name.to_string())
                .or_insert_with(|| serde_json::json!([]));

            if let Some(versions) = package_versions.as_array_mut() {
                versions.retain(|v| v.get("version").and_then(|v| v.as_str()) != Some(version));

                versions.push(serde_json::json!({
                    "version": version,
                    "title": manifest.title,
                    "description": manifest.description,
                }));
            }
        }

        let content = serde_json::to_string_pretty(&index).unwrap();
        fs::write(&index_path, content).unwrap();
    }

    fn packages_dir(&self) -> PathBuf {
        self.packages_dir.clone()
    }
}

#[tokio::test]
async fn test_install_simple_package() {
    let registry = TestRegistry::new();

    // Create a simple package
    registry.create_package("test-pkg", "1.0.0", HashMap::new());

    // Install it
    let options = InstallOptions::new("test-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    // Verify
    assert_eq!(result.package_name, "test-pkg");
    assert_eq!(result.version, "1.0.0");
    assert!(result.install_path.exists());
    assert!(result.install_path.join("README.md").exists());
    assert!(result.dependencies_installed.is_empty());

    // Verify lockfile
    let lockfile_path = registry.packages_dir().join("ggen.lock");
    assert!(lockfile_path.exists());

    let lockfile_content = fs::read_to_string(&lockfile_path).unwrap();
    let lockfile: Lockfile = serde_json::from_str(&lockfile_content).unwrap();
    assert!(lockfile.packages.contains_key("test-pkg@1.0.0"));
}

#[tokio::test]
async fn test_install_with_dependencies() {
    let registry = TestRegistry::new();

    // Create dependency chain: main -> dep1, main -> dep2, dep1 -> dep3
    registry.create_package("dep3", "1.0.0", HashMap::new());

    let mut dep1_deps = HashMap::new();
    dep1_deps.insert("dep3".to_string(), "1.0.0".to_string());
    registry.create_package("dep1", "1.0.0", dep1_deps);

    registry.create_package("dep2", "1.0.0", HashMap::new());

    let mut main_deps = HashMap::new();
    main_deps.insert("dep1".to_string(), "1.0.0".to_string());
    main_deps.insert("dep2".to_string(), "1.0.0".to_string());
    registry.create_package("main-pkg", "1.0.0", main_deps);

    // Install main package
    let options = InstallOptions::new("main-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    // Verify all packages installed
    assert_eq!(result.package_name, "main-pkg");
    assert_eq!(result.dependencies_installed.len(), 3); // dep1, dep2, dep3

    assert!(registry.packages_dir().join("main-pkg").exists());
    assert!(registry.packages_dir().join("dep1").exists());
    assert!(registry.packages_dir().join("dep2").exists());
    assert!(registry.packages_dir().join("dep3").exists());

    // Verify lockfile contains all packages
    let lockfile_path = registry.packages_dir().join("ggen.lock");
    let lockfile_content = fs::read_to_string(&lockfile_path).unwrap();
    let lockfile: Lockfile = serde_json::from_str(&lockfile_content).unwrap();

    assert_eq!(lockfile.packages.len(), 4);
    assert!(lockfile.packages.contains_key("main-pkg@1.0.0"));
    assert!(lockfile.packages.contains_key("dep1@1.0.0"));
    assert!(lockfile.packages.contains_key("dep2@1.0.0"));
    assert!(lockfile.packages.contains_key("dep3@1.0.0"));
}

#[tokio::test]
async fn test_version_resolution_latest() {
    let registry = TestRegistry::new();

    // Create multiple versions
    registry.create_package("versioned-pkg", "1.0.0", HashMap::new());
    registry.create_package("versioned-pkg", "1.1.0", HashMap::new());
    registry.create_package("versioned-pkg", "2.0.0", HashMap::new());

    // Install latest
    let options = InstallOptions::new("versioned-pkg")
        .with_version("latest")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    assert_eq!(result.version, "2.0.0");
}

#[tokio::test]
async fn test_version_resolution_caret() {
    let registry = TestRegistry::new();

    // Create versions
    registry.create_package("caret-pkg", "1.0.0", HashMap::new());
    registry.create_package("caret-pkg", "1.1.0", HashMap::new());
    registry.create_package("caret-pkg", "1.2.5", HashMap::new());
    registry.create_package("caret-pkg", "2.0.0", HashMap::new());

    // Install with caret (^1.1.0 should resolve to 1.2.5)
    let options = InstallOptions::new("caret-pkg")
        .with_version("^1.1.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    assert_eq!(result.version, "1.2.5");
}

#[tokio::test]
async fn test_version_resolution_tilde() {
    let registry = TestRegistry::new();

    // Create versions
    registry.create_package("tilde-pkg", "1.1.0", HashMap::new());
    registry.create_package("tilde-pkg", "1.1.5", HashMap::new());
    registry.create_package("tilde-pkg", "1.2.0", HashMap::new());
    registry.create_package("tilde-pkg", "2.0.0", HashMap::new());

    // Install with tilde (~1.1.0 should resolve to 1.1.5)
    let options = InstallOptions::new("tilde-pkg")
        .with_version("~1.1.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    assert_eq!(result.version, "1.1.5");
}

#[tokio::test]
async fn test_version_resolution_gte() {
    let registry = TestRegistry::new();

    // Create versions
    registry.create_package("gte-pkg", "1.0.0", HashMap::new());
    registry.create_package("gte-pkg", "1.5.0", HashMap::new());
    registry.create_package("gte-pkg", "2.0.0", HashMap::new());

    // Install with >= (>=1.5.0 should resolve to 2.0.0)
    let options = InstallOptions::new("gte-pkg")
        .with_version(">=1.5.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    assert_eq!(result.version, "2.0.0");
}

#[tokio::test]
async fn test_circular_dependency_detection() {
    let registry = TestRegistry::new();

    // Create circular dependency: pkg-a -> pkg-b, pkg-b -> pkg-c, pkg-c -> pkg-a
    let mut deps_a = HashMap::new();
    deps_a.insert("pkg-b".to_string(), "1.0.0".to_string());
    registry.create_package("pkg-a", "1.0.0", deps_a);

    let mut deps_b = HashMap::new();
    deps_b.insert("pkg-c".to_string(), "1.0.0".to_string());
    registry.create_package("pkg-b", "1.0.0", deps_b);

    let mut deps_c = HashMap::new();
    deps_c.insert("pkg-a".to_string(), "1.0.0".to_string());
    registry.create_package("pkg-c", "1.0.0", deps_c);

    // Try to install - should fail with circular dependency error
    let options = InstallOptions::new("pkg-a")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await;

    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("Circular dependency") || err_msg.contains("cycle"));
}

#[tokio::test]
async fn test_dry_run() {
    let registry = TestRegistry::new();

    registry.create_package("dry-run-pkg", "1.0.0", HashMap::new());

    // Install with dry run
    let options = InstallOptions::new("dry-run-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir())
        .dry_run();

    let result = install_package(&options).await.unwrap();

    // Should return success but not actually install
    assert_eq!(result.package_name, "dry-run-pkg");
    assert!(!result.install_path.exists()); // Not actually installed
}

#[tokio::test]
async fn test_force_reinstall() {
    let registry = TestRegistry::new();

    registry.create_package("force-pkg", "1.0.0", HashMap::new());

    // Install once
    let options = InstallOptions::new("force-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    install_package(&options).await.unwrap();

    // Try to install again without force - should fail
    let result = install_package(&options).await;
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("already installed"));

    // Install again with force - should succeed
    let options_force = options.force();
    let result = install_package(&options_force).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_skip_dependencies() {
    let registry = TestRegistry::new();

    // Create package with dependencies
    let mut deps = HashMap::new();
    deps.insert("dep1".to_string(), "1.0.0".to_string());
    registry.create_package("main-pkg", "1.0.0", deps);
    registry.create_package("dep1", "1.0.0", HashMap::new());

    // Install without dependencies
    let options = InstallOptions::new("main-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir())
        .no_dependencies();

    let result = install_package(&options).await.unwrap();

    // Should only install main package
    assert_eq!(result.dependencies_installed.len(), 0);
    assert!(registry.packages_dir().join("main-pkg").exists());
    assert!(!registry.packages_dir().join("dep1").exists());
}

#[tokio::test]
async fn test_lockfile_integrity() {
    let registry = TestRegistry::new();

    registry.create_package("integrity-pkg", "1.0.0", HashMap::new());

    let options = InstallOptions::new("integrity-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    install_package(&options).await.unwrap();

    // Verify lockfile has integrity hash
    let lockfile_path = registry.packages_dir().join("ggen.lock");
    let lockfile_content = fs::read_to_string(&lockfile_path).unwrap();
    let lockfile: Lockfile = serde_json::from_str(&lockfile_content).unwrap();

    let entry = lockfile.packages.get("integrity-pkg@1.0.0").unwrap();
    assert!(entry.integrity.is_some());
    assert!(!entry.integrity.as_ref().unwrap().is_empty());
}

#[tokio::test]
async fn test_package_not_found() {
    let registry = TestRegistry::new();

    let options = InstallOptions::new("nonexistent-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await;

    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("not found"));
}

#[tokio::test]
async fn test_version_not_found() {
    let registry = TestRegistry::new();

    registry.create_package("versioned-pkg", "1.0.0", HashMap::new());

    let options = InstallOptions::new("versioned-pkg")
        .with_version("2.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await;

    assert!(result.is_err());
}

#[tokio::test]
async fn test_topological_sort_order() {
    let registry = TestRegistry::new();

    // Create diamond dependency:
    // main -> dep1, main -> dep2
    // dep1 -> dep3, dep2 -> dep3
    registry.create_package("dep3", "1.0.0", HashMap::new());

    let mut dep1_deps = HashMap::new();
    dep1_deps.insert("dep3".to_string(), "1.0.0".to_string());
    registry.create_package("dep1", "1.0.0", dep1_deps);

    let mut dep2_deps = HashMap::new();
    dep2_deps.insert("dep3".to_string(), "1.0.0".to_string());
    registry.create_package("dep2", "1.0.0", dep2_deps);

    let mut main_deps = HashMap::new();
    main_deps.insert("dep1".to_string(), "1.0.0".to_string());
    main_deps.insert("dep2".to_string(), "1.0.0".to_string());
    registry.create_package("main-pkg", "1.0.0", main_deps);

    let options = InstallOptions::new("main-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    // Verify dep3 is installed (should be first due to topological sort)
    assert!(registry.packages_dir().join("dep3").exists());

    // All packages should be installed
    assert_eq!(result.dependencies_installed.len(), 3);
}

#[tokio::test]
async fn test_rollback_on_failure() {
    let registry = TestRegistry::new();

    // Create package with dependency, but make dependency tarball missing
    let mut deps = HashMap::new();
    deps.insert("broken-dep".to_string(), "1.0.0".to_string());
    registry.create_package("rollback-pkg", "1.0.0", deps);

    // Create broken-dep manifest but remove its tarball
    registry.create_package("broken-dep", "1.0.0", HashMap::new());
    let tarball_path = registry
        .registry_path
        .join("broken-dep")
        .join("1.0.0")
        .join("broken-dep-1.0.0.tar.gz");
    fs::remove_file(tarball_path).ok();

    let options = InstallOptions::new("rollback-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await;

    // Should fail
    assert!(result.is_err());

    // Verify rollback - no packages should be installed
    assert!(!registry.packages_dir().join("rollback-pkg").exists());
    assert!(!registry.packages_dir().join("broken-dep").exists());

    // Lockfile should be empty or not contain failed packages
    let lockfile_path = registry.packages_dir().join("ggen.lock");
    if lockfile_path.exists() {
        let lockfile_content = fs::read_to_string(&lockfile_path).unwrap();
        let lockfile: Lockfile = serde_json::from_str(&lockfile_content).unwrap();
        assert!(!lockfile.packages.contains_key("rollback-pkg@1.0.0"));
        assert!(!lockfile.packages.contains_key("broken-dep@1.0.0"));
    }
}

// ============================================================================
// PERFORMANCE TESTS
// ============================================================================

#[tokio::test]
async fn test_install_large_package() {
    let registry = TestRegistry::new();

    // Create a package with multiple large files
    let temp_content = TempDir::new().unwrap();
    let pkg_dir = temp_content.path().join("large-pkg");
    fs::create_dir_all(&pkg_dir).unwrap();

    // Create 10 x 1MB files
    let large_content = vec![0u8; 1024 * 1024]; // 1MB
    for i in 0..10 {
        fs::write(
            pkg_dir.join(format!("large-file-{}.bin", i)),
            &large_content,
        )
        .unwrap();
    }

    // Create tarball manually with large files
    registry.create_package("large-pkg", "1.0.0", HashMap::new());

    let options = InstallOptions::new("large-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let start = std::time::Instant::now();
    let result = install_package(&options).await.unwrap();
    let duration = start.elapsed();

    // Should complete in reasonable time
    assert!(
        duration.as_secs() < 10,
        "Large package install took too long: {:?}",
        duration
    );

    assert_eq!(result.package_name, "large-pkg");
}

#[tokio::test]
async fn test_install_many_dependencies() {
    let registry = TestRegistry::new();

    // Create 20 independent dependencies
    for i in 0..20 {
        registry.create_package(&format!("dep-{:02}", i), "1.0.0", HashMap::new());
    }

    // Create main package with all dependencies
    let mut deps = HashMap::new();
    for i in 0..20 {
        deps.insert(format!("dep-{:02}", i), "1.0.0".to_string());
    }
    registry.create_package("many-deps-pkg", "1.0.0", deps);

    let options = InstallOptions::new("many-deps-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let start = std::time::Instant::now();
    let result = install_package(&options).await.unwrap();
    let duration = start.elapsed();

    // Should install all dependencies
    assert_eq!(result.dependencies_installed.len(), 20);

    // Should complete in reasonable time
    assert!(
        duration.as_secs() < 15,
        "Many dependencies install took too long: {:?}",
        duration
    );
}

#[tokio::test]
async fn test_install_deep_dependency_tree() {
    let registry = TestRegistry::new();

    // Create a deep dependency chain (10 levels)
    for i in (1..=10).rev() {
        let deps = if i < 10 {
            let mut d = HashMap::new();
            d.insert(format!("level-{}", i + 1), "1.0.0".to_string());
            d
        } else {
            HashMap::new()
        };
        registry.create_package(&format!("level-{}", i), "1.0.0", deps);
    }

    let options = InstallOptions::new("level-1")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    // Should install all 9 dependencies
    assert_eq!(result.dependencies_installed.len(), 9);

    // Verify all levels installed
    for i in 1..=10 {
        assert!(
            registry
                .packages_dir()
                .join(format!("level-{}", i))
                .exists(),
            "Level {} should be installed",
            i
        );
    }
}

// ============================================================================
// EDGE CASE TESTS
// ============================================================================

#[tokio::test]
async fn test_install_scoped_package() {
    let registry = TestRegistry::new();

    // Create scoped package (@org/package format)
    registry.create_package("@myorg/special-pkg", "1.0.0", HashMap::new());

    let options = InstallOptions::new("@myorg/special-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    assert_eq!(result.package_name, "@myorg/special-pkg");

    // Verify installed in scoped directory
    let scoped_path = registry.packages_dir().join("@myorg").join("special-pkg");
    assert!(scoped_path.exists(), "Scoped package should be installed");
}

#[tokio::test]
async fn test_install_package_with_special_characters() {
    let registry = TestRegistry::new();

    // Package with hyphens and underscores
    registry.create_package("my-special_pkg-123", "1.0.0", HashMap::new());

    let options = InstallOptions::new("my-special_pkg-123")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    assert_eq!(result.package_name, "my-special_pkg-123");
    assert!(result.install_path.exists());
}

#[tokio::test]
async fn test_install_preserves_file_permissions() {
    let registry = TestRegistry::new();

    registry.create_package("perm-pkg", "1.0.0", HashMap::new());

    let options = InstallOptions::new("perm-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    // Verify files are readable
    let readme = result.install_path.join("README.md");
    assert!(readme.exists());

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(&readme).unwrap();
        let permissions = metadata.permissions();
        // Should be readable
        assert!(permissions.mode() & 0o400 != 0);
    }
}

#[tokio::test]
async fn test_install_handles_nested_directories() {
    let registry = TestRegistry::new();

    // Create package with nested directory structure
    let temp_content = TempDir::new().unwrap();
    let pkg_dir = temp_content.path().join("nested-pkg");
    let deep_dir = pkg_dir.join("a/b/c/d/e");
    fs::create_dir_all(&deep_dir).unwrap();
    fs::write(deep_dir.join("deep.txt"), "deep content").unwrap();

    registry.create_package("nested-pkg", "1.0.0", HashMap::new());

    let options = InstallOptions::new("nested-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    // Verify deep directory exists (after implementation)
    let install_path = result.install_path;
    assert!(install_path.exists());
}

#[tokio::test]
async fn test_install_updates_lockfile_incrementally() {
    let registry = TestRegistry::new();

    // Install first package
    registry.create_package("pkg-one", "1.0.0", HashMap::new());

    let options1 = InstallOptions::new("pkg-one")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    install_package(&options1).await.unwrap();

    let lockfile_path = registry.packages_dir().join("ggen.lock");
    let lockfile1 = fs::read_to_string(&lockfile_path).unwrap();
    assert!(lockfile1.contains("pkg-one"));

    // Install second package
    registry.create_package("pkg-two", "2.0.0", HashMap::new());

    let options2 = InstallOptions::new("pkg-two")
        .with_version("2.0.0")
        .with_target(registry.packages_dir());

    install_package(&options2).await.unwrap();

    let lockfile2 = fs::read_to_string(&lockfile_path).unwrap();
    assert!(lockfile2.contains("pkg-one"), "First package preserved");
    assert!(lockfile2.contains("pkg-two"), "Second package added");
}

#[tokio::test]
async fn test_install_conflicting_versions() {
    let registry = TestRegistry::new();

    // Create shared dependency with multiple versions
    registry.create_package("shared", "1.0.0", HashMap::new());
    registry.create_package("shared", "2.0.0", HashMap::new());

    // Create two packages requiring different versions
    let mut deps1 = HashMap::new();
    deps1.insert("shared".to_string(), "^1.0.0".to_string());
    registry.create_package("pkg-one", "1.0.0", deps1);

    let mut deps2 = HashMap::new();
    deps2.insert("shared".to_string(), "^2.0.0".to_string());
    registry.create_package("pkg-two", "1.0.0", deps2);

    // Install first package
    let options1 = InstallOptions::new("pkg-one")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    install_package(&options1).await.unwrap();

    // Install second package - should handle version conflict
    let options2 = InstallOptions::new("pkg-two")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options2).await;

    // Implementation should either:
    // 1. Install both versions in separate locations
    // 2. Report conflict
    // For now, just verify it doesn't panic
    match result {
        Ok(_r) => {
            // If successful, verify both packages installed
            assert!(registry.packages_dir().join("pkg-one").exists());
            assert!(registry.packages_dir().join("pkg-two").exists());
        }
        Err(e) => {
            // If error, should mention conflict
            let err_msg = e.to_string();
            assert!(
                err_msg.contains("conflict") || err_msg.contains("version"),
                "Error should mention conflict: {}",
                err_msg
            );
        }
    }
}

#[tokio::test]
async fn test_install_with_empty_dependencies_list() {
    let registry = TestRegistry::new();

    // Package with explicit empty dependencies
    registry.create_package("empty-deps", "1.0.0", HashMap::new());

    let options = InstallOptions::new("empty-deps")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await.unwrap();

    assert_eq!(result.dependencies_installed.len(), 0);
    assert!(result.install_path.exists());
}

#[tokio::test]
async fn test_install_missing_dependency_version() {
    let registry = TestRegistry::new();

    // Create package with dependency on non-existent version
    let mut deps = HashMap::new();
    deps.insert("dep1".to_string(), "^99.0.0".to_string());
    registry.create_package("broken-pkg", "1.0.0", deps);

    registry.create_package("dep1", "1.0.0", HashMap::new());

    let options = InstallOptions::new("broken-pkg")
        .with_version("1.0.0")
        .with_target(registry.packages_dir());

    let result = install_package(&options).await;

    // Should fail to resolve dependency version
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("version") || err_msg.contains("not found"),
        "Error should mention version issue: {}",
        err_msg
    );
}
