//! Simple Integration Tests for Lifecycle Phase Execution
//!
//! Tests the basic lifecycle management functionality.
//! Uses standard Rust testing with anyhow for error handling.

use anyhow::Result;
use tempfile::TempDir;

// Import common test utilities
#[path = "../common/mod.rs"]
mod common;
use common::{create_temp_dir, echo_command};

#[test]
fn test_temp_dir_creation() -> Result<()> {
    // Arrange & Act
    let temp_dir = create_temp_dir();

    // Assert
    assert!(temp_dir.path().exists());
    Ok(())
}

#[test]
fn test_echo_command_helper() -> Result<()> {
    // Arrange
    let message = "Hello World";

    // Act
    let cmd = echo_command(message);

    // Assert
    assert!(cmd.contains(message));
    Ok(())
}

#[test]
fn test_fixtures_are_accessible() -> Result<()> {
    // Arrange & Act
    use common::{sample_make_toml, sample_template_content};

    let make_content = sample_make_toml();
    let template_content = sample_template_content();

    // Assert
    assert!(make_content.contains("[project]"));
    assert!(template_content.contains("project_name"));
    Ok(())
}
