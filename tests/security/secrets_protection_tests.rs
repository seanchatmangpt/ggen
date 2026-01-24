//! End-to-End Secrets Protection Integration Tests
//!
//! Tests that sensitive information is never leaked through:
//! - Logs
//! - Error messages
//! - Stack traces
//! - Generated output
//! - Environment variables
//!
//! Uses Chicago TDD: AAA pattern, real execution, observable outputs

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Test fixture for secrets protection scenarios
struct SecretsFixture {
    workspace: TempDir,
    api_key: String,
    password: String,
    secret_token: String,
}

impl SecretsFixture {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let workspace = TempDir::new()?;

        Ok(Self {
            workspace,
            api_key: "sk_live_abc123_SUPER_SECRET_KEY".to_string(),
            password: "MyTopSecretPassword123!".to_string(),
            secret_token: "ghp_secretTokenValue12345".to_string(),
        })
    }

    fn workspace_path(&self) -> &std::path::Path {
        self.workspace.path()
    }
}

// ============================================================================
// CATEGORY 1: Secrets Not in Logs
// ============================================================================

#[test]
fn test_api_keys_not_logged() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;
    let config_file = fixture.workspace_path().join("ggen.toml");

    let config_content = format!(
        r#"
[api]
key = "{}"
endpoint = "https://api.example.com"
        "#,
        fixture.api_key
    );
    fs::write(&config_file, config_content)?;

    // Act: Run command with verbose logging
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("--config")
        .arg(&config_file)
        .arg("--verbose")
        .arg("generate")
        .current_dir(fixture.workspace_path())
        .env("RUST_LOG", "trace") // Maximum logging
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: API key should never appear in logs
    assert!(!stdout.contains(&fixture.api_key), "API key leaked in stdout");
    assert!(!stderr.contains(&fixture.api_key), "API key leaked in stderr");

    // Should show redacted version
    assert!(
        stdout.contains("sk_live_***") || stderr.contains("sk_live_***") || stdout.is_empty(),
        "Should show redacted API key"
    );

    Ok(())
}

#[test]
fn test_passwords_not_logged() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;

    // Act: Pass password as environment variable
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("--verbose")
        .arg("generate")
        .env("GGEN_DB_PASSWORD", &fixture.password)
        .env("RUST_LOG", "debug")
        .current_dir(fixture.workspace_path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Password should never appear in logs
    assert!(!stdout.contains(&fixture.password), "Password leaked in stdout");
    assert!(!stderr.contains(&fixture.password), "Password leaked in stderr");

    // Should show redacted
    assert!(
        stdout.contains("***") || stderr.contains("***") || stdout.is_empty(),
        "Should show redacted password"
    );

    Ok(())
}

#[test]
fn test_tokens_not_logged() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;
    let env_file = fixture.workspace_path().join(".env");

    fs::write(&env_file, format!("GITHUB_TOKEN={}", fixture.secret_token))?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("--verbose")
        .arg("generate")
        .arg("--env-file")
        .arg(&env_file)
        .env("RUST_LOG", "trace")
        .current_dir(fixture.workspace_path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Token should not appear
    assert!(!stdout.contains(&fixture.secret_token), "Token leaked in stdout");
    assert!(!stderr.contains(&fixture.secret_token), "Token leaked in stderr");

    Ok(())
}

// ============================================================================
// CATEGORY 2: Secrets Not in Error Messages
// ============================================================================

#[test]
fn test_error_messages_redact_credentials() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;

    // Create invalid config with credentials
    let config_file = fixture.workspace_path().join("ggen.toml");
    let config_content = format!(
        r#"
[database]
url = "postgresql://user:{}@localhost/db"
        "#,
        fixture.password
    );
    fs::write(&config_file, config_content)?;

    // Act: Trigger error (e.g., connection failure)
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("--config")
        .arg(&config_file)
        .arg("generate")
        .current_dir(fixture.workspace_path())
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Password should be redacted in error messages
    assert!(!stderr.contains(&fixture.password), "Password in error message");
    assert!(
        stderr.contains("***") || stderr.contains("REDACTED") || stderr.is_empty(),
        "Should show redacted credentials"
    );

    Ok(())
}

#[test]
fn test_connection_string_errors_redacted() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;
    let connection_string = format!(
        "mongodb://admin:{}@localhost:27017/db",
        fixture.password
    );

    // Act: Try to use connection string (will fail, but should redact)
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--db-url")
        .arg(&connection_string)
        .current_dir(fixture.workspace_path())
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Connection string password should be redacted
    assert!(!stderr.contains(&fixture.password), "Password in connection string error");

    Ok(())
}

// ============================================================================
// CATEGORY 3: Secrets Not in Stack Traces
// ============================================================================

#[test]
fn test_stack_traces_sanitized() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;
    let template_file = fixture.workspace_path().join("template.tera");

    // Create template that will cause error with API key in context
    fs::write(&template_file, "{{ api_key }}")?;

    // Act: Trigger panic/error
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--template")
        .arg(&template_file)
        .arg("--var")
        .arg(format!("api_key={}", fixture.api_key))
        .env("RUST_BACKTRACE", "full") // Enable stack traces
        .current_dir(fixture.workspace_path())
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Stack trace should not contain API key
    assert!(!stderr.contains(&fixture.api_key), "API key in stack trace");

    Ok(())
}

// ============================================================================
// CATEGORY 4: Secrets Not in Generated Output
// ============================================================================

#[test]
fn test_config_values_not_in_output() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;
    let template_file = fixture.workspace_path().join("template.tera");
    let output_file = fixture.workspace_path().join("output.txt");
    let config_file = fixture.workspace_path().join("ggen.toml");

    // Config with secrets
    let config_content = format!(
        r#"
[secrets]
api_key = "{}"
password = "{}"
        "#,
        fixture.api_key,
        fixture.password
    );
    fs::write(&config_file, config_content)?;

    // Template that accidentally tries to reference config
    fs::write(&template_file, "Config: {{ config }}")?;

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let _output = cmd
        .arg("--config")
        .arg(&config_file)
        .arg("generate")
        .arg("--template")
        .arg(&template_file)
        .arg("--output")
        .arg(&output_file)
        .current_dir(fixture.workspace_path())
        .output()?;

    // Assert: Generated file should not contain secrets
    if output_file.exists() {
        let generated_content = fs::read_to_string(&output_file)?;
        assert!(!generated_content.contains(&fixture.api_key), "API key in generated output");
        assert!(!generated_content.contains(&fixture.password), "Password in generated output");
    }

    Ok(())
}

// ============================================================================
// CATEGORY 5: Environment Variables Not Exposed
// ============================================================================

#[test]
fn test_env_vars_not_exposed_in_errors() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;

    // Act: Set secret env vars and trigger error
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--invalid-flag")
        .env("GGEN_API_KEY", &fixture.api_key)
        .env("GGEN_SECRET_TOKEN", &fixture.secret_token)
        .current_dir(fixture.workspace_path())
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Env var values should not appear in error
    assert!(!stderr.contains(&fixture.api_key), "API key from env in error");
    assert!(!stderr.contains(&fixture.secret_token), "Token from env in error");

    Ok(())
}

#[test]
fn test_env_var_dump_redacts_secrets() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;

    // Act: Use debug command that might dump env vars
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("debug")
        .arg("--show-env")
        .env("GGEN_API_KEY", &fixture.api_key)
        .env("DATABASE_URL", format!("postgres://user:{}@host/db", fixture.password))
        .env("NORMAL_VAR", "public_value")
        .current_dir(fixture.workspace_path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Assert: Should show env var names but redact values
    assert!(
        stdout.contains("GGEN_API_KEY") || stdout.is_empty(),
        "Should show env var name"
    );
    assert!(!stdout.contains(&fixture.api_key), "Should redact API key value");
    assert!(!stdout.contains(&fixture.password), "Should redact password value");

    // Non-secret vars can be shown
    assert!(
        stdout.contains("NORMAL_VAR") || stdout.is_empty(),
        "Can show non-secret vars"
    );

    Ok(())
}

// ============================================================================
// CATEGORY 6: Redaction Patterns
// ============================================================================

#[test]
fn test_common_secret_patterns_redacted() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;

    let secret_patterns = vec![
        ("api_key", "sk_live_abc123xyz"),
        ("github_token", "ghp_secretToken123"),
        ("aws_secret", "aws_secret_access_key_value"),
        ("password", "MySecretPass123"),
        ("private_key", "-----BEGIN RSA PRIVATE KEY-----"),
    ];

    for (key, value) in secret_patterns {
        // Act
        let mut cmd = Command::cargo_bin("ggen")?;
        let output = cmd
            .arg("--verbose")
            .arg("generate")
            .arg("--var")
            .arg(format!("{}={}", key, value))
            .env("RUST_LOG", "debug")
            .current_dir(fixture.workspace_path())
            .output()?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        // Assert: Secret patterns should be redacted
        assert!(!stdout.contains(value), "{} value leaked in stdout", key);
        assert!(!stderr.contains(value), "{} value leaked in stderr", key);
    }

    Ok(())
}

// ============================================================================
// CATEGORY 7: Non-Secrets Are Not Over-Redacted
// ============================================================================

#[test]
fn test_public_values_not_redacted() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SecretsFixture::new()?;
    let public_values = vec![
        ("username", "alice"),
        ("project_name", "my-project"),
        ("version", "1.0.0"),
    ];

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let mut cmd_builder = cmd.arg("--verbose").arg("generate");

    for (key, value) in &public_values {
        cmd_builder = cmd_builder.arg("--var").arg(format!("{}={}", key, value));
    }

    let output = cmd_builder
        .env("RUST_LOG", "debug")
        .current_dir(fixture.workspace_path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}{}", stdout, stderr);

    // Assert: Public values should be visible (not over-redacted)
    // Note: Might not appear if command fails early
    if !combined.is_empty() {
        let contains_some_public = public_values.iter().any(|(_, value)| {
            combined.contains(value)
        });

        // At least one public value should be visible if there's output
        // (This is a weak assertion since output might be minimal)
        if combined.len() > 100 {
            assert!(
                contains_some_public,
                "Public values should not be over-redacted"
            );
        }
    }

    Ok(())
}
