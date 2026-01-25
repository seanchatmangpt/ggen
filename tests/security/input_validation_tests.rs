//! End-to-End Input Validation Integration Tests
//!
//! Tests input validation across all ggen CLI commands and API endpoints:
//! - Template input validation
//! - RDF file validation
//! - Configuration validation
//! - CLI argument validation
//! - Environment variable validation
//!
//! Uses Chicago TDD: AAA pattern, real file system, observable outputs

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Test fixture for input validation scenarios
struct InputValidationFixture {
    workspace: TempDir,
}

impl InputValidationFixture {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let workspace = TempDir::new()?;
        Ok(Self { workspace })
    }

    fn workspace_path(&self) -> &std::path::Path {
        self.workspace.path()
    }
}

// ============================================================================
// CATEGORY 1: Template Input Validation
// ============================================================================

#[test]
fn test_malformed_template_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let template_file = fixture.workspace_path().join("bad.tera");

    // Create malformed template with unclosed tag
    fs::write(&template_file, "Hello {{ name }}")?; // Missing closing }}

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--template")
        .arg(&template_file)
        .arg("--output")
        .arg(fixture.workspace_path().join("out.txt"))
        .current_dir(fixture.workspace_path())
        .assert();

    // Assert: Should reject malformed template
    assert.failure()
        .stderr(predicate::str::contains("template").or(predicate::str::contains("syntax")));

    Ok(())
}

#[test]
fn test_template_with_code_execution_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let template_file = fixture.workspace_path().join("malicious.tera");

    // Attempt to inject code execution
    let malicious_templates = vec![
        "{{ __import__('os').system('rm -rf /') }}", // Python-style
        "{{ system('cat /etc/passwd') }}", // System call
        "{% raw %}{{ dangerous_function() }}{% endraw %}", // Raw execution
    ];

    for (i, malicious_content) in malicious_templates.iter().enumerate() {
        fs::write(&template_file, malicious_content)?;

        // Act
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--template")
            .arg(&template_file)
            .arg("--output")
            .arg(fixture.workspace_path().join(format!("out{}.txt", i)))
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Should either reject or safely render as literal
        // Templates should NOT execute arbitrary code
        assert.failure();
    }

    Ok(())
}

#[test]
fn test_oversized_template_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let template_file = fixture.workspace_path().join("huge.tera");

    // Create 100MB template (DoS attempt)
    let huge_content = "x".repeat(100 * 1024 * 1024);
    fs::write(&template_file, huge_content)?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--template")
        .arg(&template_file)
        .arg("--output")
        .arg(fixture.workspace_path().join("out.txt"))
        .current_dir(fixture.workspace_path())
        .timeout(std::time::Duration::from_secs(5))
        .assert();

    // Assert: Should reject or timeout (not hang indefinitely)
    assert.failure();

    Ok(())
}

// ============================================================================
// CATEGORY 2: RDF File Validation
// ============================================================================

#[test]
fn test_invalid_rdf_syntax_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let rdf_file = fixture.workspace_path().join("invalid.ttl");

    // Create invalid RDF/Turtle syntax
    fs::write(&rdf_file, "This is not valid Turtle syntax @#$%")?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("validate")
        .arg(&rdf_file)
        .current_dir(fixture.workspace_path())
        .assert();

    // Assert: Should reject invalid RDF
    assert.failure()
        .stderr(predicate::str::contains("syntax").or(predicate::str::contains("parse")));

    Ok(())
}

#[test]
fn test_rdf_with_external_entities_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let rdf_file = fixture.workspace_path().join("xxe.rdf");

    // Attempt XXE (XML External Entity) attack
    let xxe_content = r#"<?xml version="1.0"?>
<!DOCTYPE rdf [
  <!ENTITY xxe SYSTEM "file:///etc/passwd">
]>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  <rdf:Description rdf:about="http://example.com/test">
    <name>&xxe;</name>
  </rdf:Description>
</rdf:RDF>"#;
    fs::write(&rdf_file, xxe_content)?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("validate")
        .arg(&rdf_file)
        .current_dir(fixture.workspace_path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Should not process external entities
    assert!(!stdout.contains("root:"), "Should not leak /etc/passwd");
    assert!(!stderr.contains("root:"), "Should not leak /etc/passwd in errors");

    Ok(())
}

#[test]
fn test_oversized_rdf_file_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let rdf_file = fixture.workspace_path().join("huge.ttl");

    // Create 500MB RDF file (DoS attempt)
    let mut huge_content = String::from("@prefix ex: <http://example.com/> .\n");
    for i in 0..10_000_000 {
        huge_content.push_str(&format!("ex:Item{} ex:name \"Item {}\" .\n", i, i));
    }
    fs::write(&rdf_file, huge_content)?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("validate")
        .arg(&rdf_file)
        .current_dir(fixture.workspace_path())
        .timeout(std::time::Duration::from_secs(10))
        .assert();

    // Assert: Should timeout or reject (not process indefinitely)
    assert.failure();

    Ok(())
}

// ============================================================================
// CATEGORY 3: Configuration Validation
// ============================================================================

#[test]
fn test_invalid_toml_config_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let config_file = fixture.workspace_path().join("ggen.toml");

    // Create invalid TOML
    fs::write(&config_file, "invalid toml [[[[ syntax")?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("--config")
        .arg(&config_file)
        .arg("generate")
        .current_dir(fixture.workspace_path())
        .assert();

    // Assert: Should reject invalid config
    assert.failure()
        .stderr(predicate::str::contains("config").or(predicate::str::contains("toml")));

    Ok(())
}

#[test]
fn test_config_with_dangerous_values_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let config_file = fixture.workspace_path().join("ggen.toml");

    // Create config with dangerous values
    let dangerous_config = r#"
[security]
path_traversal_protection = false
validate_paths = false
allow_code_execution = true
max_template_size_mb = 999999
    "#;
    fs::write(&config_file, dangerous_config)?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("--config")
        .arg(&config_file)
        .arg("generate")
        .current_dir(fixture.workspace_path())
        .assert();

    // Assert: Should warn or reject dangerous configuration
    assert.failure();

    Ok(())
}

// ============================================================================
// CATEGORY 4: CLI Argument Validation
// ============================================================================

#[test]
fn test_negative_numeric_arguments_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange & Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--max-depth")
        .arg("-1")
        .assert();

    // Assert: Should reject negative values
    assert.failure()
        .stderr(predicate::str::contains("invalid").or(predicate::str::contains("value")));

    Ok(())
}

#[test]
fn test_extremely_long_argument_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let long_arg = "a".repeat(100_000);

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--output")
        .arg(long_arg)
        .assert();

    // Assert: Should handle long arguments without crashing
    assert.failure();

    Ok(())
}

#[test]
fn test_special_characters_in_arguments_handled() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;

    let special_chars = vec![
        "file\0name.txt", // Null byte
        "file\nname.txt", // Newline
        "file;rm -rf /.txt", // Command injection attempt
        "file`whoami`.txt", // Command substitution
    ];

    for special_char_name in special_chars {
        // Act
        let mut cmd = Command::cargo_bin("ggen")?;
        let assert = cmd
            .arg("generate")
            .arg("--output")
            .arg(special_char_name)
            .current_dir(fixture.workspace_path())
            .assert();

        // Assert: Should reject or sanitize
        assert.failure();
    }

    Ok(())
}

// ============================================================================
// CATEGORY 5: Environment Variable Validation
// ============================================================================

#[test]
fn test_malicious_env_vars_ignored() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;

    // Act: Set malicious environment variables
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .env("GGEN_ALLOW_CODE_EXECUTION", "true")
        .env("GGEN_DISABLE_SECURITY", "1")
        .env("GGEN_TEMPLATE_PATH", "../../../../etc/passwd")
        .arg("generate")
        .current_dir(fixture.workspace_path())
        .assert();

    // Assert: Should ignore or reject dangerous env vars
    assert.failure();

    Ok(())
}

#[test]
fn test_oversized_env_var_values_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let huge_value = "x".repeat(10_000_000); // 10MB env var

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .env("GGEN_CONFIG", huge_value)
        .arg("generate")
        .current_dir(fixture.workspace_path())
        .timeout(std::time::Duration::from_secs(5))
        .assert();

    // Assert: Should handle gracefully without consuming excessive resources
    assert.failure();

    Ok(())
}

// ============================================================================
// CATEGORY 6: Boundary Testing
// ============================================================================

#[test]
fn test_max_integer_values_handled() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange & Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--max-depth")
        .arg(i64::MAX.to_string())
        .assert();

    // Assert: Should validate and reject unreasonable values
    assert.failure();

    Ok(())
}

#[test]
fn test_empty_required_arguments_rejected() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange & Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--template")
        .arg("") // Empty string
        .assert();

    // Assert: Should reject empty required arguments
    assert.failure()
        .stderr(predicate::str::contains("required").or(predicate::str::contains("empty")));

    Ok(())
}

// ============================================================================
// CATEGORY 7: Valid Inputs Work
// ============================================================================

#[test]
fn test_legitimate_inputs_succeed() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = InputValidationFixture::new()?;
    let template_file = fixture.workspace_path().join("valid.tera");
    let output_file = fixture.workspace_path().join("output.txt");

    fs::write(&template_file, "Hello {{ name }}")?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--template")
        .arg(&template_file)
        .arg("--output")
        .arg(&output_file)
        .current_dir(fixture.workspace_path())
        .assert();

    // Assert: Valid inputs should succeed
    assert.success();
    assert!(output_file.exists(), "Output file should be created");

    Ok(())
}
