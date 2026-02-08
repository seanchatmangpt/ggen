//! End-to-End Path Traversal Prevention Integration Tests
//!
//! Tests path traversal attack prevention across the entire ggen system:
//! - Template loading
//! - RDF file access
//! - Output file generation
//! - Configuration file access
//!
//! Uses Chicago TDD: AAA pattern, real file system, observable outputs

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test fixture providing isolated filesystem environment
struct PathTraversalFixture {
    workspace: TempDir,
    templates_dir: PathBuf,
    output_dir: PathBuf,
    secrets_dir: TempDir,
}

impl PathTraversalFixture {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let workspace = TempDir::new()?;
        let templates_dir = workspace.path().join("templates");
        let output_dir = workspace.path().join("output");
        let secrets_dir = TempDir::new()?;

        fs::create_dir_all(&templates_dir)?;
        fs::create_dir_all(&output_dir)?;

        // Create a secret file outside workspace
        let secret_file = secrets_dir.path().join("secrets.txt");
        fs::write(&secret_file, "API_KEY=super_secret_key_12345")?;

        // Create legitimate template
        let template_path = templates_dir.join("valid.tera");
        fs::write(&template_path, "Hello {{ name }}")?;

        Ok(Self {
            workspace,
            templates_dir,
            output_dir,
            secrets_dir,
        })
    }

    fn workspace_path(&self) -> &std::path::Path {
        self.workspace.path()
    }

    fn secrets_path(&self) -> &std::path::Path {
        self.secrets_dir.path()
    }
}

// ============================================================================
// CATEGORY 1: Template Path Traversal Prevention
// ============================================================================

#[test]
fn test_template_path_traversal_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;

    let malicious_paths = vec![
        "../../../etc/passwd",
        "..\\..\\..\\windows\\system32\\config\\sam",
        "/etc/passwd",
        "../../secrets.txt",
        "templates/../../../secrets.txt",
    ];

    for malicious_path in malicious_paths {
        // Act: Attempt to load template with traversal path
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--template")
            .arg(malicious_path)
            .arg("--output")
            .arg(fixture.output_dir.join("test.txt"))
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Command fails with clear error message
        assert
            .failure()
            .stderr(predicate::str::contains("path").or(predicate::str::contains("invalid")));

        // Verify: Secret file was NOT accessed
        let secret_content = fs::read_to_string(fixture.secrets_path().join("secrets.txt"))?;
        assert!(
            secret_content.contains("API_KEY"),
            "Secret file should be unchanged"
        );
    }

    Ok(())
}

#[test]
fn test_symlink_attack_prevention() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::symlink;

        // Create symlink to secrets directory
        let symlink_path = fixture.templates_dir.join("malicious_link");
        symlink(fixture.secrets_path(), &symlink_path)?;

        // Act: Attempt to load template through symlink
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--template")
            .arg(symlink_path.join("secrets.txt"))
            .arg("--output")
            .arg(fixture.output_dir.join("test.txt"))
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Should fail with security error
        assert.failure();

        // Verify: Output file was NOT created
        assert!(!fixture.output_dir.join("test.txt").exists());
    }

    Ok(())
}

#[test]
fn test_null_byte_injection_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;

    // Null byte can truncate path in C APIs: "safe.txt\0../../secrets.txt"
    let malicious_paths = vec!["template.tera\0/etc/passwd", "safe\0/../../../secrets.txt"];

    for malicious_path in malicious_paths {
        // Act
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--template")
            .arg(malicious_path)
            .arg("--output")
            .arg(fixture.output_dir.join("test.txt"))
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Null bytes should be rejected
        assert.failure();
    }

    Ok(())
}

#[test]
fn test_unicode_normalization_attacks_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;

    // Unicode characters that normalize to "../"
    let malicious_paths = vec![
        "template\u{2024}\u{2024}/secrets.txt", // FULLWIDTH characters
        "template\u{FF0E}\u{FF0E}/secrets.txt", // Fullwidth period
    ];

    for malicious_path in malicious_paths {
        // Act
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--template")
            .arg(malicious_path)
            .arg("--output")
            .arg(fixture.output_dir.join("test.txt"))
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Unicode normalization attacks should fail
        assert.failure();
    }

    Ok(())
}

// ============================================================================
// CATEGORY 2: RDF File Path Traversal Prevention
// ============================================================================

#[test]
fn test_rdf_file_path_traversal_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;
    let ontology_dir = fixture.workspace_path().join("ontology");
    fs::create_dir_all(&ontology_dir)?;

    let malicious_rdf_paths = vec!["../../../etc/hosts", "../../secrets.txt", "/etc/passwd"];

    for malicious_path in malicious_rdf_paths {
        // Act: Attempt to load RDF file with traversal
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--ontology")
            .arg(malicious_path)
            .arg("--template")
            .arg(fixture.templates_dir.join("valid.tera"))
            .arg("--output")
            .arg(fixture.output_dir.join("test.txt"))
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Should fail with validation error
        assert.failure();
    }

    Ok(())
}

// ============================================================================
// CATEGORY 3: Output File Path Traversal Prevention
// ============================================================================

#[test]
fn test_output_path_traversal_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;

    let malicious_output_paths = vec![
        "../../../tmp/malicious.txt",
        "/etc/cron.d/malicious",
        "../../secrets_overwrite.txt",
    ];

    for malicious_path in malicious_output_paths {
        // Act: Attempt to write outside workspace
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--template")
            .arg(fixture.templates_dir.join("valid.tera"))
            .arg("--output")
            .arg(malicious_path)
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Should fail with sandbox violation
        assert.failure().stderr(
            predicate::str::contains("sandbox")
                .or(predicate::str::contains("permission"))
                .or(predicate::str::contains("invalid")),
        );

        // Verify: No files created outside workspace
        let workspace_files: Vec<_> = fs::read_dir(fixture.workspace_path())?
            .filter_map(Result::ok)
            .collect();

        // Should only have templates and output directories
        assert!(
            workspace_files.len() <= 3,
            "No extra files should be created"
        );
    }

    Ok(())
}

// ============================================================================
// CATEGORY 4: Error Message Information Leakage Prevention
// ============================================================================

#[test]
fn test_error_messages_dont_leak_system_paths() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;
    let malicious_path = "../../../etc/passwd";

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--template")
        .arg(malicious_path)
        .arg("--output")
        .arg(fixture.output_dir.join("test.txt"))
        .current_dir(fixture.workspace_path())
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Error message should NOT contain system paths
    assert!(!stderr.contains("/etc/"), "Should not leak /etc/ path");
    assert!(
        !stderr.contains("C:\\"),
        "Should not leak Windows system paths"
    );
    assert!(
        !stderr.contains("/home/"),
        "Should not leak user home paths"
    );
    assert!(!stderr.contains("/root/"), "Should not leak root paths");

    // Should contain generic error message
    assert!(
        stderr.contains("invalid") || stderr.contains("denied") || stderr.contains("path"),
        "Should have generic error message"
    );

    Ok(())
}

#[test]
fn test_legitimate_paths_work() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = PathTraversalFixture::new()?;
    let template_path = fixture.templates_dir.join("valid.tera");
    let output_path = fixture.output_dir.join("output.txt");

    // Act: Use legitimate paths within workspace
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--template")
        .arg(&template_path)
        .arg("--output")
        .arg(&output_path)
        .current_dir(fixture.workspace_path())
        .assert();

    // Assert: Should succeed
    assert.success();

    // Verify: Output file was created in correct location
    assert!(output_path.exists(), "Output file should be created");

    Ok(())
}
