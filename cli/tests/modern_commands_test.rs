//! Integration tests for modern noun-verb CLI commands
//!
//! Tests use:
//! - assert_cmd for CLI execution
//! - assert_fs for temporary filesystem operations
//! - fake for generating realistic test data

use assert_cmd::prelude::*;
use assert_fs::TempDir;
use std::process::Command;
use std::str;

/// Helper function to get the project root directory
fn project_root() -> String {
    std::env::var("CARGO_MANIFEST_DIR")
        .map(|dir| {
            std::path::Path::new(&dir)
                .parent()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
        })
        .unwrap_or_else(|_| "/Users/sac/ggen".to_string())
}

// ============================================================================
// SWARM COMMAND TESTS
// ============================================================================

#[test]
fn test_swarm_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "swarm", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute swarm help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Ultrathink swarm orchestration"));
    assert!(stdout.contains("server"));
    assert!(stdout.contains("status"));
    assert!(stdout.contains("register"));
    assert!(stdout.contains("submit"));
}

#[test]
fn test_swarm_status() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "swarm", "status"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute swarm status");

    // Status command should succeed (even if no swarm is running)
    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Swarm Status") || stdout.contains("Status command pending"));
}

#[test]
fn test_swarm_list_agents() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "swarm", "list-agents"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute swarm list-agents");

    // Should succeed even with no agents
    assert!(output.status.success());
}

#[test]
fn test_swarm_config() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "swarm", "config"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute swarm config");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Configuration") || stdout.contains("config"));
}

#[test]
fn test_swarm_invalid_subcommand() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "swarm", "invalid-cmd"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute swarm with invalid subcommand");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error") || stderr.contains("unrecognized"));
}

// ============================================================================
// ULTRATHINK COMMAND TESTS
// ============================================================================

#[test]
fn test_ultrathink_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ultrathink", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ultrathink help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Ultrathink swarm intelligence"));
    assert!(stdout.contains("start"));
    assert!(stdout.contains("status"));
    assert!(stdout.contains("task"));
    assert!(stdout.contains("sync"));
}

#[test]
fn test_ultrathink_status() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ultrathink", "status"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ultrathink status");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Swarm Status") || stdout.contains("Status:"));
}

#[test]
fn test_ultrathink_intelligence() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ultrathink", "intelligence"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ultrathink intelligence");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Intelligence") || stdout.contains("metrics"));
}

#[test]
fn test_ultrathink_invalid_subcommand() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ultrathink", "invalid-cmd"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ultrathink with invalid subcommand");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error") || stderr.contains("unrecognized"));
}

// ============================================================================
// AUTONOMOUS COMMAND TESTS
// ============================================================================

#[test]
fn test_autonomous_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "autonomous", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute autonomous help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Autonomous graph evolution"));
    assert!(stdout.contains("evolve"));
    assert!(stdout.contains("regenerate"));
    assert!(stdout.contains("status"));
    assert!(stdout.contains("approve"));
}

#[test]
fn test_autonomous_status() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "autonomous", "status"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute autonomous status");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Status") || stdout.contains("autonomous"));
}

#[test]
fn test_autonomous_evolve_requires_requirements() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "autonomous", "evolve"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute autonomous evolve");

    // Should fail without --requirements argument
    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("required") || stderr.contains("requirements"));
}

#[test]
fn test_autonomous_evolve_help() {
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "autonomous",
            "evolve",
            "--help",
        ])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute autonomous evolve help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("requirements"));
    assert!(stdout.contains("provider"));
    assert!(stdout.contains("model"));
}

#[test]
fn test_autonomous_regenerate_help() {
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "autonomous",
            "regenerate",
            "--help",
        ])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute autonomous regenerate help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Regenerate all artifacts"));
}

// ============================================================================
// AI VALIDATE COMMAND TESTS
// ============================================================================

#[test]
fn test_ai_validate_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "validate", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai validate help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Validate templates"));
    assert!(stdout.contains("--template"));
    assert!(stdout.contains("--strict"));
}

#[test]
fn test_ai_validate_requires_template() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "validate"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai validate");

    // Should fail without --template argument
    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("required") || stderr.contains("template"));
}

#[test]
fn test_ai_validate_nonexistent_file() {
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "ai",
            "validate",
            "--template",
            "/nonexistent/file.tmpl",
        ])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai validate with nonexistent file");

    // Should fail gracefully
    assert!(!output.status.success());
}

// ============================================================================
// AI GENERATE COMMAND TESTS
// ============================================================================

#[test]
fn test_ai_generate_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "generate", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai generate help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Generate templates using AI"));
    assert!(stdout.contains("--description"));
    assert!(stdout.contains("--validate"));
    assert!(stdout.contains("--mock"));
}

#[test]
fn test_ai_generate_requires_description() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "generate"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai generate");

    // Should fail without --description argument
    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("required") || stderr.contains("description"));
}

#[test]
fn test_ai_generate_mock_mode() {
    let temp_dir = TempDir::new().unwrap();
    let output_file = temp_dir.path().join("test.tmpl");

    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "ai",
            "generate",
            "--description",
            "A simple test template",
            "--mock",
            "--output",
            output_file.to_str().unwrap(),
        ])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai generate in mock mode");

    // Mock mode should succeed
    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Generated") || stdout.contains("Success"));
}

// ============================================================================
// AI FROM-SOURCE COMMAND TESTS
// ============================================================================

#[test]
fn test_ai_from_source_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "from-source", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai from-source help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Generate template from existing source"));
    assert!(stdout.contains("--source-file"));
    assert!(stdout.contains("--language"));
    assert!(stdout.contains("--extract-variables"));
}

#[test]
fn test_ai_from_source_requires_source_file() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "from-source"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai from-source");

    // Should fail without --source-file argument
    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("required") || stderr.contains("source-file"));
}

#[test]
fn test_ai_from_source_nonexistent_file() {
    let output = Command::new("cargo")
        .args(&[
            "run",
            "--bin",
            "ggen",
            "--",
            "ai",
            "from-source",
            "--source-file",
            "/nonexistent/file.rs",
        ])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai from-source with nonexistent file");

    // Should fail gracefully
    assert!(!output.status.success());
}

// ============================================================================
// AI PROJECT COMMAND TESTS
// ============================================================================

#[test]
fn test_ai_project_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "project", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai project help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Generate complete template projects"));
    assert!(stdout.contains("--description"));
    assert!(stdout.contains("--name"));
    assert!(stdout.contains("--language"));
}

#[test]
fn test_ai_project_requires_description_and_name() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "project"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai project");

    // Should fail without required arguments
    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(
        stderr.contains("required") && (stderr.contains("description") || stderr.contains("name"))
    );
}

// ============================================================================
// AI SPARQL COMMAND TESTS
// ============================================================================

#[test]
fn test_ai_sparql_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "sparql", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai sparql help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Generate SPARQL queries"));
    assert!(stdout.contains("--description"));
}

#[test]
fn test_ai_sparql_requires_description() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "sparql"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai sparql");

    // Should fail without --description argument
    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("required") || stderr.contains("description"));
}

// ============================================================================
// AI FRONTMATTER COMMAND TESTS
// ============================================================================

#[test]
fn test_ai_frontmatter_help() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "frontmatter", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai frontmatter help");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("Generate frontmatter"));
    assert!(stdout.contains("--description"));
}

#[test]
fn test_ai_frontmatter_requires_description() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "ai", "frontmatter"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute ai frontmatter");

    // Should fail without --description argument
    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("required") || stderr.contains("description"));
}

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

#[test]
fn test_invalid_top_level_command() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "nonexistent-command"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute invalid command");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error") || stderr.contains("unrecognized"));
}

#[test]
fn test_help_flag() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "--help"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute help flag");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("ggen"));
    assert!(stdout.contains("swarm"));
    assert!(stdout.contains("ultrathink"));
    assert!(stdout.contains("autonomous"));
}

#[test]
fn test_version_flag() {
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "--version"])
        .current_dir(project_root())
        .output()
        .expect("Failed to execute version flag");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("0.2.5") || stdout.contains("ggen"));
}
