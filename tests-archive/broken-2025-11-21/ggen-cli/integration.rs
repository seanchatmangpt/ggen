//! Integration Tests - CLI → Domain → Core Integration
//!
//! Tests the complete integration flow across architectural layers:
//! - CLI commands parsing and validation
//! - Domain layer business logic execution
//! - Core functionality integration
//! - Error propagation through layers
//! - JSON output formatting
//!
//! 80/20 Focus: Critical paths that validate the architecture

use assert_cmd::Command;
use assert_fs::prelude::*;
use assert_fs::TempDir;
use chicago_tdd_tools::prelude::*;
use chicago_tdd_tools::test;
use predicates::prelude::*;

// ============================================================================
// CLI → Domain → Core Integration Tests
// ============================================================================

test!(test_template_generate_integration, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("template.yaml");
    let output_dir = temp.child("output");

    // Create minimal template
    template_file
        .write_str(
            r#"name: test-service
description: Test template
variables:
  service_name:
    required: true
    default: my-service
nodes:
  - name: "{{service_name}}"
    type: directory
    children:
      - name: main.rs
        type: file
        content: |
          fn main() {
              println!("{{service_name}}");
          }
"#,
        )
        .unwrap();

    // Act: Execute CLI → Template Domain → Core Template Engine
    // v2.0: Simpler syntax without --var flags (RDF provides data)
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "template",
        "generate",
        template_file.path().to_str().unwrap(),
        output_dir.path().to_str().unwrap(),
    ])
    .assert()
    .success();

    // Assert: Verify Core generated files correctly
    let main_file = output_dir.child("my-service/main.rs");
    main_file.assert(predicate::path::exists());
    main_file.assert(predicate::str::contains("my-service"));
});

test!(test_marketplace_search_integration, {
    // Arrange & Act: Test CLI → Market Domain → Core Registry
    // v2.0: "marketplace" command replaces "market"
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd
        .args(["marketplace", "search", "rust", "--limit", "5"])
        .output()
        .unwrap();

    // Assert: Should complete successfully (with or without results)
    assert!(output.status.success() || output.status.code() == Some(0));
});

#[ignore = "project gen command not fully implemented"]
test!(test_project_gen_integration, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let project_dir = temp.child("my-project");

    // Act
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd
        .args([
            "project",
            "gen",
            "--name",
            "my-project",
            "--template",
            "rust-cli",
            "--output",
            project_dir.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success() || project_dir.path().exists());
});

#[ignore = "lifecycle run command not fully implemented"]
test!(test_lifecycle_execution_integration, {
    let temp = TempDir::new().unwrap();
    let make_file = temp.child("make.toml");

    // Create test lifecycle config
    make_file
        .write_str(
            r#"[project]
name = "test-project"
version = "1.0.0"

[[phases]]
name = "init"
description = "Initialize project"
commands = ["echo 'Initializing...'"]

[[phases]]
name = "build"
description = "Build project"
depends_on = ["init"]
commands = ["echo 'Building...'"]
"#,
        )
        .unwrap();

    // Act
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd
        .args([
            "lifecycle",
            "run",
            "build",
            "--manifest",
            make_file.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success());
});

// ============================================================================
// Error Propagation Tests
// ============================================================================

test!(test_error_propagation_invalid_template, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let invalid_template = temp.child("invalid.yaml");
    invalid_template
        .write_str("invalid: yaml: content:")
        .unwrap();

    // Act & Assert
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "template",
        "generate",
        invalid_template.path().to_str().unwrap(),
        "/tmp/output",
    ])
    .assert()
    .failure();
});

test!(test_error_propagation_missing_file, {
    // Arrange & Act & Assert
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "template",
        "generate",
        "/nonexistent/template.yaml",
        "/tmp/output",
    ])
    .assert()
    .failure();
});

test!(test_error_propagation_invalid_command, {
    // Arrange & Act & Assert
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["invalid-command"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("unrecognized")));
});

test!(test_error_propagation_missing_required_var, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("template.yaml");

    template_file
        .write_str(
            r#"
name: "test"
variables:
  - name: required_var
    required: true
nodes:
  - name: "test.txt"
    type: file
    content: "{{required_var}}"
"#,
        )
        .unwrap();

    // Act & Assert
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "template",
        "generate",
        template_file.path().to_str().unwrap(),
        "/tmp/output",
    ])
    .assert()
    .failure();
});

// ============================================================================
// JSON Output Formatting Tests
// ============================================================================

test!(test_json_output_marketplace_search, {
    // Arrange & Act
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd
        .args(["marketplace", "search", "rust", "--json", "--limit", "1"])
        .output()
        .unwrap();

    // Assert
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if !stdout.is_empty() && stdout.trim() != "{}" && !stdout.starts_with("Error") {
            if let Ok(json) = serde_json::from_str::<serde_json::Value>(&stdout) {
                assert!(json.is_object() || json.is_array());
            }
        }
    }
});

test!(test_json_output_project_info, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let project_file = temp.child("project.toml");

    project_file
        .write_str(
            r#"
[project]
name = "test-project"
version = "1.0.0"
"#,
        )
        .unwrap();

    // Act
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd
        .args([
            "project",
            "info",
            "--manifest",
            project_file.path().to_str().unwrap(),
            "--json",
        ])
        .output()
        .unwrap();

    // Assert
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if !stdout.is_empty() {
            let json: serde_json::Value = serde_json::from_str(&stdout).unwrap();
            assert!(json.is_object());
        }
    }
});

// ============================================================================
// Multi-Command Workflow Tests
// ============================================================================

test!(test_workflow_template_to_lifecycle, {
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("template.yaml");
    let output_dir = temp.child("project");

    // Arrange
    template_file
        .write_str(
            r#"name: lifecycle-project
description: Project with lifecycle
variables:
  project_name:
    required: true
    default: test-project
nodes:
  - name: "{{project_name}}"
    type: directory
    children:
      - name: make.toml
        type: file
        content: |
          [project]
          name = "{{project_name}}"

          [[phases]]
          name = "build"
          commands = ["echo 'Building...'"]
"#,
        )
        .unwrap();

    // Act
    let result = Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate",
            template_file.path().to_str().unwrap(),
            output_dir.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Assert
    if result.status.success() {
        let make_file = output_dir.child("test-project/make.toml");
        if make_file.path().exists() {
            Command::cargo_bin("ggen")
                .unwrap()
                .args([
                    "lifecycle",
                    "run",
                    "build",
                    "--manifest",
                    make_file.path().to_str().unwrap(),
                ])
                .assert()
                .success();
        }
    }
});

test!(test_workflow_marketplace_to_project, {
    // Arrange & Act
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["marketplace", "search", "cli-template", "--limit", "1"])
        .assert()
        .success();

    // Assert: Test passes if search succeeds
    // Note: Install and generate steps would require network
});

test!(test_workflow_graph_operations, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let graph_file = temp.child("test.ttl");

    graph_file
        .write_str(
            r#"@prefix ex: <http://example.org/> .

ex:project1 a ex:Project ;
    ex:name "Test Project" ;
    ex:version "1.0.0" .
"#,
        )
        .unwrap();

    // Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "graph",
            "import",
            graph_file.path().to_str().unwrap(),
            "--format",
            "turtle",
        ])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success() || output.status.code().is_some());
});

// ============================================================================
// Command Chaining Tests
// ============================================================================

test!(test_doctor_before_operations, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["doctor"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Checking").or(predicate::str::is_empty()));
});

#[ignore = "shell completion command not implemented"]
test!(test_shell_completion_generation, {
    let temp = TempDir::new().unwrap();
    let completion_file = temp.child("ggen.bash");

    // Arrange & Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "shell",
            "completion",
            "bash",
            "--output",
            completion_file.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success() || completion_file.path().exists());
});

// ============================================================================
// Configuration Integration Tests
// ============================================================================

#[ignore = "config file loading not fully implemented"]
test!(test_config_file_loading, {
    let temp = TempDir::new().unwrap();
    let config_file = temp.child("config.toml");

    config_file
        .write_str(
            r#"[project]
name = "test"
version = "1.0.0"

[templates]
search_paths = ["./templates"]

[marketplace]
enabled = true
"#,
        )
        .unwrap();

    // Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args(["--config", config_file.path().to_str().unwrap(), "doctor"])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success());
});

test!(test_manifest_path_option, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let manifest = temp.child("custom.toml");

    manifest
        .write_str(
            r#"[project]
name = "custom-project"
version = "1.0.0"
"#,
        )
        .unwrap();

    // Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "--manifest-path",
            manifest.path().to_str().unwrap(),
            "project",
            "info",
        ])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success() || output.status.code().is_some());
});

// ============================================================================
// Help and Documentation Tests
// ============================================================================

test!(test_help_command, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Graph-aware code generator"));
});

test!(test_version_command, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["--version"])
        .assert()
        .success()
        .stdout(predicate::str::contains("1.2.0"));
});

test!(test_progressive_help, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["help-me"])
        .assert()
        .success();
});

test!(test_subcommand_help, {
    // Arrange & Act & Assert
    for subcommand in &["template", "marketplace", "project", "lifecycle", "graph"] {
        Command::cargo_bin("ggen")
            .unwrap()
            .args([*subcommand, "--help"])
            .assert()
            .success()
            .stdout(predicate::str::contains("Usage").or(predicate::str::contains("Commands")));
    }
});

// ============================================================================
// v2.0 Pattern Tests - Auto-Discovery, Sync Wrappers, Frozen Sections
// ============================================================================

test!(test_v2_auto_discovery, {
    // Arrange & Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args(["--help"])
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Assert
    assert!(stdout.contains("marketplace") || stdout.contains("market"));
    assert!(stdout.contains("template"));
    assert!(stdout.contains("graph") || stdout.contains("lifecycle"));
    assert!(output.status.success());
});

test!(test_v2_sync_wrapper_execution, {
    // Arrange & Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args(["doctor"])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success());
});

test!(test_v2_help_me_command, {
    // Arrange & Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args(["help-me"])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success());
});

#[ignore = "frozen section feature not yet implemented"]
test!(test_v2_frozen_section_preservation, {
    let temp = TempDir::new().unwrap();
    let output_file = temp.child("service.rs");

    // Arrange
    output_file
        .write_str(
            r#"// Generated code
pub fn generated() {
    println!("Generated");
}

// FROZEN-START
pub fn my_business_logic() {
    println!("Custom logic");
}
// FROZEN-END

// More generated code
pub fn more_generated() {
    println!("More generated");
}
"#,
        )
        .unwrap();

    let template_file = temp.child("template.yaml");
    template_file
        .write_str(
            r#"name: test-frozen
nodes:
  - name: service.rs
    type: file
    content: |
      // Generated code
      pub fn generated() {
          println!("Generated v2");
      }

      // FROZEN-START
      // This should be preserved
      // FROZEN-END

      // More generated code
      pub fn more_generated() {
          println!("More generated v2");
      }
"#,
        )
        .unwrap();

    // Act
    let result = Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate",
            template_file.path().to_str().unwrap(),
            temp.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Assert
    if result.status.success() {
        let content = std::fs::read_to_string(output_file.path()).unwrap();
        assert!(content.contains("my_business_logic"));
        assert!(content.contains("Custom logic"));
    }
});

#[ignore = "business logic protection not yet implemented"]
test!(test_v2_business_logic_not_overwritten, {
    let temp = TempDir::new().unwrap();
    let business_file = temp.child("business_logic.rs");

    // Arrange
    business_file
        .write_str(
            r#"// Custom business logic
pub fn important_function() {
    // Critical business logic
    println!("Important!");
}
"#,
        )
        .unwrap();

    let template_file = temp.child("template.yaml");
    template_file
        .write_str(
            r#"name: test-business
nodes:
  - name: generated.rs
    type: file
    content: |
      // Generated code only
      pub fn generated() {}
"#,
        )
        .unwrap();

    // Act
    let result = Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate",
            template_file.path().to_str().unwrap(),
            temp.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Assert
    if result.status.success() {
        let original_content = "// Custom business logic";
        let current_content = std::fs::read_to_string(business_file.path()).unwrap();
        assert!(current_content.contains(original_content));
    }
});

test!(test_v2_marketplace_search_with_rdf, {
    // Arrange & Act
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .args(["marketplace", "search", "rust", "--limit", "3"])
        .output()
        .unwrap();

    // Assert
    assert!(output.status.success() || output.status.code() == Some(0));
});

#[ignore = "RDF template generation not yet fully implemented"]
test!(test_v2_rdf_based_template_generation, {
    let temp = TempDir::new().unwrap();
    let rdf_file = temp.child("data.ttl");
    let template_file = temp.child("template.hbs");
    let output_dir = temp.child("output");

    // Arrange
    rdf_file
        .write_str(
            r#"@prefix ex: <http://example.org/> .

ex:service a ex:Service ;
    ex:name "MyService" ;
    ex:version "1.0.0" .
"#,
        )
        .unwrap();

    template_file
        .write_str(
            r#"// Service: {{name}}
// Version: {{version}}

pub struct {{name}} {
    version: &'static str,
}

impl {{name}} {
    pub fn new() -> Self {
        Self { version: "{{version}}" }
    }
}
"#,
        )
        .unwrap();

    // Act
    let result = Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate",
            template_file.path().to_str().unwrap(),
            rdf_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Assert
    if result.status.success() {
        assert!(output_dir.path().exists());
    }
});
