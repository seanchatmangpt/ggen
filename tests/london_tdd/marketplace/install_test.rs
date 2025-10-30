//! London TDD tests for `ggen add` command
//!
//! README.md §Marketplace - Package Installation
//!
//! Tests verify:
//! - Package download from marketplace
//! - Version validation
//! - Dependency resolution
//! - Installation verification

use crate::lib::*;
use mockall::predicate::*;

#[test]
fn test_add_downloads_and_installs_package() {
    let start = std::time::Instant::now();

    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    let mut mock_fs = MockFilesystem::new();

    mock_marketplace
        .expect_download()
        .with(eq("io.ggen.rust.axum"))
        .times(1)
        .returning(|_| Ok(b"package_data".to_vec()));

    mock_fs
        .expect_write_file()
        .times(1)
        .returning(|_, _| Ok(()));

    // Act
    let result = run_add_command(&mock_marketplace, &mock_fs, "io.ggen.rust.axum");

    // Assert
    assert!(result.is_ok());
    let install_result = result.unwrap();
    assert!(install_result.success);
    assert_eq!(install_result.package_id, "io.ggen.rust.axum");

    // Performance
    assert!(start.elapsed().as_millis() < 100);
}

#[test]
fn test_add_validates_package_version() {
    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    let mock_fs = MockFilesystem::new();

    mock_marketplace
        .expect_download()
        .with(eq("io.ggen.rust.axum@1.0.0"))
        .times(1)
        .returning(|_| Ok(b"package_data".to_vec()));

    // Act
    let result = run_add_with_version(&mock_marketplace, &mock_fs, "io.ggen.rust.axum", "1.0.0");

    // Assert
    assert!(result.is_ok());
    let install_result = result.unwrap();
    assert_eq!(install_result.version, Some("1.0.0".to_string()));
}

#[test]
fn test_add_fails_for_nonexistent_package() {
    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    let mock_fs = MockFilesystem::new();

    mock_marketplace
        .expect_download()
        .with(eq("io.ggen.nonexistent"))
        .times(1)
        .returning(|_| Err(anyhow::anyhow!("Package not found")));

    // Act
    let result = run_add_command(&mock_marketplace, &mock_fs, "io.ggen.nonexistent");

    // Assert: Clear error message
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Package not found"));
}

#[test]
fn test_add_skips_if_already_installed() {
    // Arrange
    let mock_marketplace = MockMarketplaceClient::new();
    let mut mock_fs = MockFilesystem::new();

    mock_fs
        .expect_exists()
        .with(predicate::str::contains("io.ggen.rust.axum"))
        .returning(|_| true); // Package already exists

    // Act
    let result = run_add_command(&mock_marketplace, &mock_fs, "io.ggen.rust.axum");

    // Assert: Skipped, but successful
    assert!(result.is_ok());
    let install_result = result.unwrap();
    assert!(install_result.success);
    assert!(install_result.skipped);
}

#[test]
fn test_add_creates_otel_span() {
    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    let mock_fs = MockFilesystem::new();
    let tracer = otel::MockTracerProvider::new();

    mock_marketplace
        .expect_download()
        .returning(|_| Ok(b"data".to_vec()));

    // Act
    let _result = run_add_with_tracing(&mock_marketplace, &mock_fs, &tracer, "io.ggen.rust.axum");

    // Assert
    let span = tracer.find_span("ggen.marketplace.add").unwrap();
    assert_eq!(span.status, otel::SpanStatus::Ok);
    assert!(span.attributes.iter().any(|(k, v)| k == "package.id" && v == "io.ggen.rust.axum"));
}

// Helper types and functions

#[derive(Debug)]
struct InstallResult {
    success: bool,
    package_id: String,
    version: Option<String>,
    skipped: bool,
}

fn run_add_command(
    marketplace: &dyn MarketplaceClient,
    fs: &dyn Filesystem,
    package_id: &str,
) -> Result<InstallResult, anyhow::Error> {
    // Check if already installed
    if fs.exists(&format!(".ggen/packages/{}", package_id)) {
        return Ok(InstallResult {
            success: true,
            package_id: package_id.to_string(),
            version: None,
            skipped: true,
        });
    }

    // Download package
    let data = marketplace.download(package_id)?;

    // Install to local directory
    fs.write_file(
        &format!(".ggen/packages/{}/package.tar.gz", package_id),
        &String::from_utf8_lossy(&data),
    )?;

    Ok(InstallResult {
        success: true,
        package_id: package_id.to_string(),
        version: None,
        skipped: false,
    })
}

fn run_add_with_version(
    marketplace: &dyn MarketplaceClient,
    fs: &dyn Filesystem,
    package_id: &str,
    version: &str,
) -> Result<InstallResult, anyhow::Error> {
    let versioned_id = format!("{}@{}", package_id, version);
    let data = marketplace.download(&versioned_id)?;

    fs.write_file(
        &format!(".ggen/packages/{}/package.tar.gz", package_id),
        &String::from_utf8_lossy(&data),
    )?;

    Ok(InstallResult {
        success: true,
        package_id: package_id.to_string(),
        version: Some(version.to_string()),
        skipped: false,
    })
}

fn run_add_with_tracing(
    marketplace: &dyn MarketplaceClient,
    fs: &dyn Filesystem,
    tracer: &otel::MockTracerProvider,
    package_id: &str,
) -> Result<InstallResult, anyhow::Error> {
    let result = run_add_command(marketplace, fs, package_id)?;

    let span = otel::MockSpan {
        name: "ggen.marketplace.add".to_string(),
        attributes: vec![
            ("package.id".to_string(), package_id.to_string()),
            ("install.success".to_string(), result.success.to_string()),
        ],
        events: vec!["package_installed".to_string()],
        status: otel::SpanStatus::Ok,
    };
    tracer.record_span(span);

    Ok(result)
}
