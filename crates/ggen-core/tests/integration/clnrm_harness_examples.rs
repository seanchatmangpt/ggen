//! Example tests demonstrating clnrm_harness usage
//!
//! These examples show best practices for using the test harness
//! in various testing scenarios.

use anyhow::Result;

use super::clnrm_harness::{LifecycleConfig, TestHarness};
use std::collections::HashMap;

/// Example: Basic marketplace search test
#[tokio::test]
async fn example_marketplace_search() -> Result<()> {
    // Create test harness
    let harness = TestHarness::new().await?;

    // Create marketplace fixture with pre-loaded packages
    let fixture = harness.marketplace_fixture().await?;

    // Search for rust packages
    let results = fixture.search("rust").await?;

    // Verify results
    assert!(!results.is_empty());
    assert!(results.iter().any(|p| p.tags.contains(&"rust".to_string())));

    Ok(())
}

/// Example: Package resolution with version
#[tokio::test]
async fn example_package_resolution() -> Result<()> {
    let harness = TestHarness::new().await?;
    let fixture = harness.marketplace_fixture().await?;

    // Resolve specific version
    let resolved = fixture.resolve("rust-web-service", Some("1.0.0")).await?;
    assert_eq!(resolved.version, "1.0.0");

    // Resolve latest version (no version specified)
    let latest = fixture.resolve("rust-web-service", None).await?;
    assert_eq!(latest.version, "1.1.0"); // Latest version

    Ok(())
}

/// Example: Testing package updates
#[tokio::test]
async fn example_package_updates() -> Result<()> {
    let harness = TestHarness::new().await?;
    let mut fixture = harness.marketplace_fixture().await?;

    // Check initial package count
    let initial_packages = fixture.list_packages().await?;
    let initial_count = initial_packages.len();

    // Add a new package
    let new_package = ggen_core::registry::PackMetadata {
        id: "new-test-package".to_string(),
        name: "New Test Package".to_string(),
        description: "A newly added test package".to_string(),
        tags: vec!["test".to_string()],
        keywords: vec!["example".to_string()],
        category: Some("testing".to_string()),
        author: Some("Test Author".to_string()),
        latest_version: "1.0.0".to_string(),
        versions: {
            let mut versions = HashMap::new();
            versions.insert(
                "1.0.0".to_string(),
                ggen_core::registry::VersionMetadata {
                    version: "1.0.0".to_string(),
                    git_url: "https://github.com/test/new-package.git".to_string(),
                    git_rev: "v1.0.0".to_string(),
                    manifest_url: None,
                    sha256: "e".repeat(64),
                },
            );
            versions
        },
        downloads: Some(0),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    };

    fixture.add_package(new_package).await?;

    // Verify package was added
    let updated_packages = fixture.list_packages().await?;
    assert_eq!(updated_packages.len(), initial_count + 1);

    // Verify we can find the new package
    let search_results = fixture.search("new-test-package").await?;
    assert!(!search_results.is_empty());

    Ok(())
}

/// Example: Lifecycle initialization test
#[tokio::test]
async fn example_lifecycle_init() -> Result<()> {
    let harness = TestHarness::new().await?;

    // Create lifecycle config
    let config = LifecycleConfig {
        name: "example-project".to_string(),
        environment: "development".to_string(),
        settings: HashMap::new(),
    };

    // Create lifecycle fixture
    let fixture = harness.lifecycle_fixture(config).await?;

    // Initialize project
    let init_result = fixture.run_phase("init").await?;
    assert!(init_result.success);
    assert_eq!(init_result.phase, "init");

    // Verify project structure
    assert!(fixture.project_dir.join("Cargo.toml").exists());
    assert!(fixture.project_dir.join("src/main.rs").exists());

    Ok(())
}

/// Example: Complete lifecycle workflow
#[tokio::test]
async fn example_complete_lifecycle() -> Result<()> {
    let harness = TestHarness::new().await?;

    let mut settings = HashMap::new();
    settings.insert("target".to_string(), "x86_64-unknown-linux-gnu".to_string());

    let config = LifecycleConfig {
        name: "full-lifecycle-test".to_string(),
        environment: "staging".to_string(),
        settings,
    };

    let fixture = harness.lifecycle_fixture(config).await?;

    // Run complete lifecycle
    let phases = vec!["init", "build", "test", "validate"];

    for phase in phases {
        let result = fixture.run_phase(phase).await?;
        assert!(
            result.success,
            "Phase {} failed: {}",
            phase,
            result.message
        );
    }

    Ok(())
}

/// Example: Error handling - graceful failure
#[tokio::test]
async fn example_error_handling() -> Result<()> {
    let harness = TestHarness::new().await?;
    let fixture = harness.marketplace_fixture().await?;

    // Try to resolve non-existent package
    let result = fixture.resolve("nonexistent-package-12345", None).await;

    // Should return error, not panic
    assert!(result.is_err());

    // Error should contain context
    let err = result.unwrap_err();
    let err_msg = format!("{:?}", err);
    assert!(
        err_msg.contains("nonexistent") || err_msg.contains("not found"),
        "Error should indicate package not found"
    );

    Ok(())
}

/// Example: Parallel test execution with isolation
#[tokio::test]
async fn example_parallel_isolation_1() -> Result<()> {
    let harness = TestHarness::new().await?;
    let fixture = harness.marketplace_fixture().await?;

    // Each test gets isolated environment
    let isolation_id = harness.isolation_id();
    assert!(!isolation_id.is_empty());

    // Operations won't conflict with other tests
    let results = fixture.search("rust").await?;
    assert!(!results.is_empty());

    Ok(())
}

/// Example: Another parallel test demonstrating isolation
#[tokio::test]
async fn example_parallel_isolation_2() -> Result<()> {
    let harness = TestHarness::new().await?;
    let fixture = harness.marketplace_fixture().await?;

    // Different isolation ID from test 1
    let isolation_id = harness.isolation_id();
    assert!(!isolation_id.is_empty());

    // Can run concurrently without conflicts
    let results = fixture.search("database").await?;
    assert!(!results.is_empty());

    Ok(())
}

/// Example: Custom test data with marketplace fixture
#[tokio::test]
async fn example_custom_test_data() -> Result<()> {
    let harness = TestHarness::new().await?;
    let fixture = harness.marketplace_fixture().await?;

    // Access pre-loaded test packages
    let web_service = fixture.get_package("rust-web-service")?;
    assert_eq!(web_service.name, "Rust Web Service");
    assert!(web_service.downloads.unwrap_or(0) > 0);

    let postgres = fixture.get_package("postgresql-setup")?;
    assert_eq!(postgres.category.as_deref(), Some("database"));

    let cli = fixture.get_package("cli-template")?;
    assert_eq!(cli.latest_version, "2.0.0");

    Ok(())
}

/// Example: Lifecycle validation with custom settings
#[tokio::test]
async fn example_lifecycle_validation() -> Result<()> {
    let harness = TestHarness::new().await?;

    let mut settings = HashMap::new();
    settings.insert("min_coverage".to_string(), "80".to_string());
    settings.insert("lint_level".to_string(), "strict".to_string());

    let config = LifecycleConfig {
        name: "validated-project".to_string(),
        environment: "production".to_string(),
        settings,
    };

    let fixture = harness.lifecycle_fixture(config.clone()).await?;

    // Initialize and validate
    fixture.run_phase("init").await?;
    let validation = fixture.run_phase("validate").await?;

    assert!(validation.success);
    assert_eq!(fixture.config.settings.get("min_coverage"), Some(&"80".to_string()));

    Ok(())
}

/// Example: Testing container lifecycle
#[tokio::test]
async fn example_container_lifecycle() -> Result<()> {
    let harness = TestHarness::new().await?;

    // Register a test container
    harness
        .register_container(
            "test-container-1".to_string(),
            "registry".to_string(),
            None, // No cleanup callback for this example
        )
        .await?;

    // Container data directory is created
    let container_dir = harness.temp_dir().join("test-container-1");
    assert!(container_dir.exists());

    // Cleanup is automatic
    harness.cleanup().await?;

    Ok(())
}

/// Example: Performance testing with phase results
#[tokio::test]
async fn example_performance_tracking() -> Result<()> {
    let harness = TestHarness::new().await?;

    let config = LifecycleConfig {
        name: "perf-test".to_string(),
        environment: "test".to_string(),
        settings: HashMap::new(),
    };

    let fixture = harness.lifecycle_fixture(config).await?;

    // Track phase durations
    let build_result = fixture.run_phase("build").await?;
    let test_result = fixture.run_phase("test").await?;

    // Both should complete quickly in test mode
    assert!(build_result.duration_ms < 1000);
    assert!(test_result.duration_ms < 1000);

    Ok(())
}
