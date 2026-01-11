//! Chicago TDD Integration Tests for Marketplace Package Installation
//!
//! These tests use the Chicago School TDD approach:
//! - Test with REAL filesystem operations
//! - Create REAL package tarballs
//! - Verify REAL installation state
//! - Test REAL dependency resolution
//! - No mocks for critical paths

use anyhow::{Context, Result};
use serde_json::json;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Test helper to create a real package tarball with metadata
struct TestPackage {
    name: String,
    version: String,
    dependencies: Vec<(String, String)>,
    temp_dir: TempDir,
}

impl TestPackage {
    fn new(name: &str, version: &str) -> Result<Self> {
        Ok(Self {
            name: name.to_string(),
            version: version.to_string(),
            dependencies: vec![],
            temp_dir: TempDir::new()?,
        })
    }

    fn with_dependency(mut self, dep_name: &str, dep_version: &str) -> Self {
        self.dependencies
            .push((dep_name.to_string(), dep_version.to_string()));
        self
    }

    fn build(&self) -> Result<PathBuf> {
        let pkg_dir = self.temp_dir.path().join(&self.name);
        fs::create_dir_all(&pkg_dir)?;

        // Create package.json with real metadata
        let mut deps = serde_json::Map::new();
        for (name, version) in &self.dependencies {
            deps.insert(name.clone(), json!(version));
        }

        let package_json = json!({
            "name": self.name,
            "version": self.version,
            "dependencies": deps,
        });

        fs::write(
            pkg_dir.join("package.json"),
            serde_json::to_string_pretty(&package_json)?,
        )?;

        // Create a sample file to verify extraction
        fs::write(pkg_dir.join("README.md"), format!("# {}", self.name))?;

        // Create tarball
        let tarball_path = self
            .temp_dir
            .path()
            .join(format!("{}-{}.tar.gz", self.name, self.version));

        // TODO: In Phase 2, use tar/flate2 to create real tarball
        // For now, create a placeholder file
        fs::write(&tarball_path, b"placeholder tarball")?;

        Ok(tarball_path)
    }
}

/// Test installation environment with isolated directories
struct TestEnv {
    temp_dir: TempDir,
    packages_dir: PathBuf,
    cache_dir: PathBuf,
}

impl TestEnv {
    fn new() -> Result<Self> {
        let temp_dir = TempDir::new()?;
        let packages_dir = temp_dir.path().join("packages");
        let cache_dir = temp_dir.path().join(".cache");

        fs::create_dir_all(&packages_dir)?;
        fs::create_dir_all(&cache_dir)?;

        Ok(Self {
            temp_dir,
            packages_dir,
            cache_dir,
        })
    }

    fn packages_path(&self) -> &Path {
        &self.packages_dir
    }

    fn cache_path(&self) -> &Path {
        &self.cache_dir
    }

    fn lockfile_path(&self) -> PathBuf {
        self.temp_dir.path().join("ggen.lock")
    }

    fn package_install_path(&self, name: &str) -> PathBuf {
        self.packages_dir.join(name)
    }
}

// ============================================================================
// BASIC INSTALLATION TESTS
// ============================================================================

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2 when install_package is implemented
async fn test_install_simple_package_no_deps() -> Result<()> {
    let env = TestEnv::new()?;

    // Create a real package
    let pkg = TestPackage::new("simple-pkg", "1.0.0")?;
    let tarball = pkg.build()?;

    // TODO: Phase 2 - Install from tarball
    // let options = InstallOptions::new("simple-pkg")
    //     .with_version("1.0.0")
    //     .with_target(env.packages_path().to_path_buf());
    // let result = install_package(&options).await?;

    // Verify installation
    let install_path = env.package_install_path("simple-pkg");
    assert!(
        install_path.exists(),
        "Package should be installed at {:?}",
        install_path
    );
    assert!(
        install_path.join("package.json").exists(),
        "package.json should exist"
    );
    assert!(
        install_path.join("README.md").exists(),
        "Files should be extracted"
    );

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_package_with_version() -> Result<()> {
    let env = TestEnv::new()?;

    let pkg = TestPackage::new("versioned-pkg", "2.3.1")?;
    let _ = pkg.build()?;

    // TODO: Install with specific version
    // Verify correct version installed
    let pkg_json_path = env
        .package_install_path("versioned-pkg")
        .join("package.json");
    let content = fs::read_to_string(pkg_json_path)?;
    let metadata: serde_json::Value = serde_json::from_str(&content)?;

    assert_eq!(metadata["version"], "2.3.1");

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_creates_lockfile() -> Result<()> {
    let env = TestEnv::new()?;

    let pkg = TestPackage::new("lock-pkg", "1.0.0")?;
    let _ = pkg.build()?;

    // TODO: Install package
    // Verify lockfile created and contains package
    let lockfile_path = env.lockfile_path();
    assert!(lockfile_path.exists(), "Lockfile should be created");

    let lockfile_content = fs::read_to_string(lockfile_path)?;
    assert!(
        lockfile_content.contains("lock-pkg"),
        "Lockfile should contain package name"
    );
    assert!(
        lockfile_content.contains("1.0.0"),
        "Lockfile should contain version"
    );

    Ok(())
}

// ============================================================================
// DEPENDENCY RESOLUTION TESTS
// ============================================================================

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_with_flat_dependencies() -> Result<()> {
    let env = TestEnv::new()?;

    // Create dependency packages
    let dep1 = TestPackage::new("dep-one", "1.0.0")?;
    let _ = dep1.build()?;

    let dep2 = TestPackage::new("dep-two", "2.0.0")?;
    let _ = dep2.build()?;

    // Create main package with dependencies
    let main_pkg = TestPackage::new("main-pkg", "1.0.0")?
        .with_dependency("dep-one", "^1.0.0")
        .with_dependency("dep-two", "^2.0.0");
    let _ = main_pkg.build()?;

    // TODO: Install main package with dependencies
    // Verify all packages installed
    assert!(env.package_install_path("main-pkg").exists());
    assert!(env.package_install_path("dep-one").exists());
    assert!(env.package_install_path("dep-two").exists());

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_with_nested_dependencies() -> Result<()> {
    let env = TestEnv::new()?;

    // Level 3 - leaf dependency
    let leaf = TestPackage::new("leaf-dep", "1.0.0")?;
    let _ = leaf.build()?;

    // Level 2 - depends on leaf
    let mid = TestPackage::new("mid-dep", "1.0.0")?.with_dependency("leaf-dep", "^1.0.0");
    let _ = mid.build()?;

    // Level 1 - depends on mid
    let top = TestPackage::new("top-pkg", "1.0.0")?.with_dependency("mid-dep", "^1.0.0");
    let _ = top.build()?;

    // TODO: Install top package, should pull entire tree
    // Verify complete dependency tree installed
    assert!(env.package_install_path("top-pkg").exists());
    assert!(env.package_install_path("mid-dep").exists());
    assert!(env.package_install_path("leaf-dep").exists());

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_dedupe_shared_dependency() -> Result<()> {
    let env = TestEnv::new()?;

    // Shared dependency
    let shared = TestPackage::new("shared-dep", "3.0.0")?;
    let _ = shared.build()?;

    // Two packages depending on same version
    let pkg1 = TestPackage::new("pkg-one", "1.0.0")?.with_dependency("shared-dep", "^3.0.0");
    let _ = pkg1.build()?;

    let pkg2 = TestPackage::new("pkg-two", "1.0.0")?.with_dependency("shared-dep", "^3.0.0");
    let _ = pkg2.build()?;

    // TODO: Install both packages
    // Verify shared-dep installed only once
    let shared_path = env.package_install_path("shared-dep");
    assert!(shared_path.exists());

    // Check lockfile shows single version
    let lockfile_content = fs::read_to_string(env.lockfile_path())?;
    let shared_count = lockfile_content.matches("shared-dep").count();
    assert_eq!(shared_count, 1, "Shared dependency should appear once");

    Ok(())
}

// ============================================================================
// VERSION RESOLUTION TESTS
// ============================================================================

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_latest_version() -> Result<()> {
    let env = TestEnv::new()?;

    // Create multiple versions
    let v1 = TestPackage::new("versioned", "1.0.0")?;
    let _ = v1.build()?;

    let v2 = TestPackage::new("versioned", "1.1.0")?;
    let _ = v2.build()?;

    let v3 = TestPackage::new("versioned", "2.0.0")?;
    let _ = v3.build()?;

    // TODO: Install without version (should get latest)
    // Verify version 2.0.0 installed
    let pkg_json = env.package_install_path("versioned").join("package.json");
    let content = fs::read_to_string(pkg_json)?;
    let metadata: serde_json::Value = serde_json::from_str(&content)?;

    assert_eq!(metadata["version"], "2.0.0");

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_caret_version() -> Result<()> {
    let env = TestEnv::new()?;

    // Available versions
    let _ = TestPackage::new("caret-pkg", "1.0.0")?.build()?;
    let _ = TestPackage::new("caret-pkg", "1.2.5")?.build()?;
    let _ = TestPackage::new("caret-pkg", "1.9.9")?.build()?;
    let _ = TestPackage::new("caret-pkg", "2.0.0")?.build()?;

    // TODO: Install with ^1.0.0 (should get 1.9.9, not 2.0.0)
    let pkg_json = env.package_install_path("caret-pkg").join("package.json");
    let content = fs::read_to_string(pkg_json)?;
    let metadata: serde_json::Value = serde_json::from_str(&content)?;

    assert_eq!(metadata["version"], "1.9.9");

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_tilde_version() -> Result<()> {
    let env = TestEnv::new()?;

    // Available versions
    let _ = TestPackage::new("tilde-pkg", "1.2.0")?.build()?;
    let _ = TestPackage::new("tilde-pkg", "1.2.3")?.build()?;
    let _ = TestPackage::new("tilde-pkg", "1.2.9")?.build()?;
    let _ = TestPackage::new("tilde-pkg", "1.3.0")?.build()?;

    // TODO: Install with ~1.2.0 (should get 1.2.9, not 1.3.0)
    let pkg_json = env.package_install_path("tilde-pkg").join("package.json");
    let content = fs::read_to_string(pkg_json)?;
    let metadata: serde_json::Value = serde_json::from_str(&content)?;

    assert_eq!(metadata["version"], "1.2.9");

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_exact_version() -> Result<()> {
    let env = TestEnv::new()?;

    let _ = TestPackage::new("exact-pkg", "1.0.0")?.build()?;
    let _ = TestPackage::new("exact-pkg", "1.0.1")?.build()?;

    // TODO: Install with exact version 1.0.0
    let pkg_json = env.package_install_path("exact-pkg").join("package.json");
    let content = fs::read_to_string(pkg_json)?;
    let metadata: serde_json::Value = serde_json::from_str(&content)?;

    assert_eq!(metadata["version"], "1.0.0");

    Ok(())
}

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_nonexistent_package() -> Result<()> {
    let env = TestEnv::new()?;

    // TODO: Attempt to install package that doesn't exist
    // Should return error
    // let result = install_package(...).await;
    // assert!(result.is_err());
    // assert!(result.unwrap_err().to_string().contains("not found"));

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_circular_dependency() -> Result<()> {
    let env = TestEnv::new()?;

    // Create circular dependencies: A -> B -> C -> A
    let pkg_a = TestPackage::new("circular-a", "1.0.0")?.with_dependency("circular-b", "^1.0.0");
    let _ = pkg_a.build()?;

    let pkg_b = TestPackage::new("circular-b", "1.0.0")?.with_dependency("circular-c", "^1.0.0");
    let _ = pkg_b.build()?;

    let pkg_c = TestPackage::new("circular-c", "1.0.0")?.with_dependency("circular-a", "^1.0.0");
    let _ = pkg_c.build()?;

    // TODO: Attempt to install, should detect circular dependency
    // let result = install_package(...).await;
    // assert!(result.is_err());
    // assert!(result.unwrap_err().to_string().contains("circular"));

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_missing_dependency() -> Result<()> {
    let env = TestEnv::new()?;

    // Package with dependency that doesn't exist
    let pkg = TestPackage::new("broken-pkg", "1.0.0")?.with_dependency("missing-dep", "^1.0.0");
    let _ = pkg.build()?;

    // TODO: Attempt to install, should fail gracefully
    // let result = install_package(...).await;
    // assert!(result.is_err());
    // assert!(result.unwrap_err().to_string().contains("missing-dep"));

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_conflicting_versions() -> Result<()> {
    let env = TestEnv::new()?;

    // Create packages with incompatible dependency versions
    let shared_v1 = TestPackage::new("shared", "1.0.0")?;
    let _ = shared_v1.build()?;

    let shared_v2 = TestPackage::new("shared", "2.0.0")?;
    let _ = shared_v2.build()?;

    let pkg1 = TestPackage::new("pkg-one", "1.0.0")?.with_dependency("shared", "^1.0.0");
    let _ = pkg1.build()?;

    let pkg2 = TestPackage::new("pkg-two", "1.0.0")?.with_dependency("shared", "^2.0.0");
    let _ = pkg2.build()?;

    // TODO: Install both, should handle version conflict
    // Either install both versions or report conflict

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_rollback_on_failure() -> Result<()> {
    let env = TestEnv::new()?;

    // Create package that will partially install then fail
    let dep1 = TestPackage::new("good-dep", "1.0.0")?;
    let _ = dep1.build()?;

    let pkg = TestPackage::new("failing-pkg", "1.0.0")?
        .with_dependency("good-dep", "^1.0.0")
        .with_dependency("bad-dep", "^1.0.0"); // This doesn't exist
    let _ = pkg.build()?;

    // TODO: Attempt install, should fail and rollback
    // Verify no partial installation remains
    assert!(
        !env.package_install_path("failing-pkg").exists(),
        "Failed package should be cleaned up"
    );
    assert!(
        !env.package_install_path("good-dep").exists(),
        "Dependencies should be rolled back"
    );

    Ok(())
}

// ============================================================================
// ADVANCED FEATURE TESTS
// ============================================================================

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_force_overwrite() -> Result<()> {
    let env = TestEnv::new()?;

    // Install package first time
    let pkg_v1 = TestPackage::new("overwrite-pkg", "1.0.0")?;
    let _ = pkg_v1.build()?;
    // TODO: install

    // Modify installed package
    let custom_file = env.package_install_path("overwrite-pkg").join("custom.txt");
    fs::write(&custom_file, "custom content")?;

    // Install again with force (newer version)
    let pkg_v2 = TestPackage::new("overwrite-pkg", "2.0.0")?;
    let _ = pkg_v2.build()?;
    // TODO: install with force=true

    // Verify new version installed and custom file removed
    let pkg_json = env
        .package_install_path("overwrite-pkg")
        .join("package.json");
    let content = fs::read_to_string(pkg_json)?;
    let metadata: serde_json::Value = serde_json::from_str(&content)?;
    assert_eq!(metadata["version"], "2.0.0");
    assert!(!custom_file.exists(), "Custom file should be removed");

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_without_force_preserves_existing() -> Result<()> {
    let env = TestEnv::new()?;

    // Install package
    let pkg = TestPackage::new("preserve-pkg", "1.0.0")?;
    let _ = pkg.build()?;
    // TODO: install

    // Modify installed package
    let custom_file = env.package_install_path("preserve-pkg").join("custom.txt");
    fs::write(&custom_file, "keep this")?;

    // Attempt to install again without force
    // TODO: install with force=false, should fail or skip
    // Verify custom file still exists
    assert!(custom_file.exists(), "Custom file should be preserved");
    let content = fs::read_to_string(&custom_file)?;
    assert_eq!(content, "keep this");

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_dry_run_no_changes() -> Result<()> {
    let env = TestEnv::new()?;

    let pkg = TestPackage::new("dry-run-pkg", "1.0.0")?;
    let _ = pkg.build()?;

    // TODO: Install with dry_run=true
    // Verify nothing actually installed
    assert!(
        !env.package_install_path("dry-run-pkg").exists(),
        "Dry run should not install package"
    );
    assert!(
        !env.lockfile_path().exists(),
        "Dry run should not create lockfile"
    );

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_no_dependencies_flag() -> Result<()> {
    let env = TestEnv::new()?;

    // Create dependency
    let dep = TestPackage::new("skip-dep", "1.0.0")?;
    let _ = dep.build()?;

    // Create main package with dependency
    let pkg = TestPackage::new("main-pkg", "1.0.0")?.with_dependency("skip-dep", "^1.0.0");
    let _ = pkg.build()?;

    // TODO: Install with with_dependencies=false
    // Verify main package installed but not dependency
    assert!(env.package_install_path("main-pkg").exists());
    assert!(
        !env.package_install_path("skip-dep").exists(),
        "Dependencies should be skipped"
    );

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_updates_lockfile_incrementally() -> Result<()> {
    let env = TestEnv::new()?;

    // Install first package
    let pkg1 = TestPackage::new("pkg-one", "1.0.0")?;
    let _ = pkg1.build()?;
    // TODO: install

    let lockfile_content = fs::read_to_string(env.lockfile_path())?;
    assert!(lockfile_content.contains("pkg-one"));

    // Install second package
    let pkg2 = TestPackage::new("pkg-two", "2.0.0")?;
    let _ = pkg2.build()?;
    // TODO: install

    let lockfile_content = fs::read_to_string(env.lockfile_path())?;
    assert!(
        lockfile_content.contains("pkg-one"),
        "First package preserved"
    );
    assert!(lockfile_content.contains("pkg-two"), "Second package added");

    Ok(())
}

// ============================================================================
// PERFORMANCE TESTS
// ============================================================================

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_large_package() -> Result<()> {
    let env = TestEnv::new()?;

    // Create package with large files (10MB total)
    let pkg = TestPackage::new("large-pkg", "1.0.0")?;
    let pkg_dir = pkg.temp_dir.path().join("large-pkg");
    fs::create_dir_all(&pkg_dir)?;

    // Create 10 x 1MB files
    let large_content = vec![0u8; 1024 * 1024]; // 1MB
    for i in 0..10 {
        fs::write(
            pkg_dir.join(format!("large-file-{}.bin", i)),
            &large_content,
        )?;
    }

    let _ = pkg.build()?;

    // TODO: Install and measure time
    let start = std::time::Instant::now();
    // install_package(...).await?;
    let duration = start.elapsed();

    // Should complete in reasonable time (< 5 seconds)
    assert!(
        duration.as_secs() < 5,
        "Large package install took too long: {:?}",
        duration
    );

    // Verify all files extracted
    for i in 0..10 {
        let file_path = env
            .package_install_path("large-pkg")
            .join(format!("large-file-{}.bin", i));
        assert!(file_path.exists(), "File {} should be extracted", i);
    }

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_many_dependencies() -> Result<()> {
    let env = TestEnv::new()?;

    // Create 50 dependencies
    let mut dependencies = vec![];
    for i in 0..50 {
        let dep = TestPackage::new(&format!("dep-{:02}", i), "1.0.0")?;
        let _ = dep.build()?;
        dependencies.push((format!("dep-{:02}", i), "^1.0.0".to_string()));
    }

    // Create main package with all dependencies
    let mut pkg = TestPackage::new("many-deps-pkg", "1.0.0")?;
    for (name, version) in &dependencies {
        pkg = pkg.with_dependency(name, version);
    }
    let _ = pkg.build()?;

    // TODO: Install with many dependencies
    let start = std::time::Instant::now();
    // install_package(...).await?;
    let duration = start.elapsed();

    // Should complete in reasonable time
    assert!(
        duration.as_secs() < 10,
        "Many dependencies install took too long: {:?}",
        duration
    );

    // Verify all dependencies installed
    for (name, _) in &dependencies {
        assert!(
            env.package_install_path(name).exists(),
            "Dependency {} should be installed",
            name
        );
    }

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_parallel_requests() -> Result<()> {
    let env = TestEnv::new()?;

    // Create multiple independent packages
    let packages = vec!["pkg-a", "pkg-b", "pkg-c", "pkg-d", "pkg-e"];
    for name in &packages {
        let pkg = TestPackage::new(name, "1.0.0")?;
        let _ = pkg.build()?;
    }

    // TODO: Install all packages in parallel
    let start = std::time::Instant::now();
    let mut handles = vec![];
    for name in &packages {
        // spawn parallel install tasks
        // handles.push(tokio::spawn(install_package(...)));
    }
    // join all
    let duration = start.elapsed();

    // Parallel should be faster than sequential
    // (This is a basic smoke test, not a rigorous benchmark)

    // Verify all installed
    for name in &packages {
        assert!(
            env.package_install_path(name).exists(),
            "Package {} should be installed",
            name
        );
    }

    Ok(())
}

// ============================================================================
// EDGE CASE TESTS
// ============================================================================

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_with_special_characters_in_name() -> Result<()> {
    let env = TestEnv::new()?;

    // Package name with @scope/name format
    let pkg = TestPackage::new("@scope/special-pkg", "1.0.0")?;
    let _ = pkg.build()?;

    // TODO: Install scoped package
    // Verify installed in correct location (scope subdirectory)
    let scoped_path = env.packages_path().join("@scope").join("special-pkg");
    assert!(scoped_path.exists(), "Scoped package should be installed");

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_with_empty_dependencies() -> Result<()> {
    let env = TestEnv::new()?;

    let pkg = TestPackage::new("empty-deps", "1.0.0")?;
    let _ = pkg.build()?;

    // TODO: Install package with no dependencies
    assert!(env.package_install_path("empty-deps").exists());

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_maintains_file_permissions() -> Result<()> {
    let env = TestEnv::new()?;

    // Create package with executable file
    let pkg = TestPackage::new("exec-pkg", "1.0.0")?;
    let pkg_dir = pkg.temp_dir.path().join("exec-pkg");
    fs::create_dir_all(&pkg_dir)?;

    let script_path = pkg_dir.join("script.sh");
    fs::write(&script_path, "#!/bin/bash\necho 'hello'")?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755); // rwxr-xr-x
        fs::set_permissions(&script_path, perms)?;
    }

    let _ = pkg.build()?;

    // TODO: Install package
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let installed_script = env.package_install_path("exec-pkg").join("script.sh");
        let perms = fs::metadata(installed_script)?.permissions();
        assert_eq!(
            perms.mode() & 0o777,
            0o755,
            "Permissions should be preserved"
        );
    }

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_handles_symlinks() -> Result<()> {
    let env = TestEnv::new()?;

    // Create package with symlink
    let pkg = TestPackage::new("symlink-pkg", "1.0.0")?;
    let pkg_dir = pkg.temp_dir.path().join("symlink-pkg");
    fs::create_dir_all(&pkg_dir)?;

    fs::write(pkg_dir.join("target.txt"), "target content")?;

    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(pkg_dir.join("target.txt"), pkg_dir.join("link.txt"))?;
    }

    let _ = pkg.build()?;

    // TODO: Install package with symlink
    #[cfg(unix)]
    {
        let installed_link = env.package_install_path("symlink-pkg").join("link.txt");
        assert!(
            installed_link.exists(),
            "Symlink should be preserved or resolved"
        );
    }

    Ok(())
}

#[tokio::test]
#[ignore] // TODO: Enable in Phase 2
async fn test_install_handles_nested_directories() -> Result<()> {
    let env = TestEnv::new()?;

    // Create package with deep directory structure
    let pkg = TestPackage::new("nested-pkg", "1.0.0")?;
    let pkg_dir = pkg.temp_dir.path().join("nested-pkg");
    let deep_dir = pkg_dir.join("a/b/c/d/e/f");
    fs::create_dir_all(&deep_dir)?;
    fs::write(deep_dir.join("deep.txt"), "deep file")?;

    let _ = pkg.build()?;

    // TODO: Install package with nested structure
    let installed_deep = env
        .package_install_path("nested-pkg")
        .join("a/b/c/d/e/f/deep.txt");
    assert!(
        installed_deep.exists(),
        "Deeply nested file should be extracted"
    );

    Ok(())
}
