//! End-to-End Tests - Complete User Workflows
//!
//! Tests full execution paths from CLI invocation to final output:
//! - Complete template generation workflow
//! - Full marketplace search and install flow
//! - Complete project generation workflow
//!
//! 80/20 Focus: Real-world user scenarios

use assert_cmd::Command;
use assert_fs::prelude::*;
use assert_fs::TempDir;
use chicago_tdd_tools::prelude::*;
use predicates::prelude::*;
use std::fs;

// ============================================================================
// E2E: Complete Template Generation Workflow
// ============================================================================

test!(e2e_template_generate_complete, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("microservice.yaml");
    let output_dir = temp.child("my-service");

    template_file
        .write_str(
            r#"
name: "rust-microservice"
description: "Production-ready Rust microservice"
version: "1.0.0"

variables:
  - name: service_name
    required: true
    description: "Name of the microservice"
  - name: port
    default: "8080"
    description: "HTTP port"
  - name: database
    default: "postgres"
    description: "Database type"

nodes:
  - name: "{{service_name}}"
    type: directory
    children:
      - name: "src"
        type: directory
        children:
          - name: "main.rs"
            type: file
            content: |
              use axum::{Router, Server};
              use std::net::SocketAddr;

              #[tokio::main]
              async fn main() {
                  let app = Router::new();
                  let addr = SocketAddr::from(([127, 0, 0, 1], {{port}}));

                  println!("🚀 {{service_name}} listening on {}", addr);
                  Server::bind(&addr)
                      .serve(app.into_make_service())
                      .await
                      .unwrap();
              }
          - name: "lib.rs"
            type: file
            content: |
              pub mod routes;
              pub mod models;
      - name: "Cargo.toml"
        type: file
        content: |
          [package]
          name = "{{service_name}}"
          version = "0.1.0"
          edition = "2021"

          [dependencies]
          axum = "0.7"
          tokio = { version = "1", features = ["full"] }
      - name: ".env.example"
        type: file
        content: |
          DATABASE_URL={{database}}://localhost/{{service_name}}
          PORT={{port}}
      - name: "README.md"
        type: file
        content: |
          # {{service_name}}

          Production-ready Rust microservice.

          ## Quick Start

          ```bash
          cargo run
          ```

          Server runs on port {{port}}.
"#,
        )
        .unwrap();

    // Act
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
            "--var",
            "service_name=payment-api",
            "--var",
            "port=3000",
            "--var",
            "database=postgres",
        ])
        .assert()
        .success();

    // Assert
    let service_dir = output_dir.child("payment-api");
    service_dir.assert(predicate::path::exists());

    let main_rs = service_dir.child("src/main.rs");
    main_rs.assert(predicate::path::exists());
    main_rs.assert(predicate::str::contains("payment-api"));
    main_rs.assert(predicate::str::contains("3000"));

    let cargo_toml = service_dir.child("Cargo.toml");
    cargo_toml.assert(predicate::path::exists());
    cargo_toml.assert(predicate::str::contains("payment-api"));

    let env_example = service_dir.child(".env.example");
    env_example.assert(predicate::path::exists());
    env_example.assert(predicate::str::contains("postgres"));
    env_example.assert(predicate::str::contains("3000"));

    let readme = service_dir.child("README.md");
    readme.assert(predicate::path::exists());
    readme.assert(predicate::str::contains("payment-api"));
});

test!(e2e_template_with_nested_structure, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("webapp.yaml");
    let output_dir = temp.child("webapp");

    template_file
        .write_str(
            r#"
name: "web-application"
variables:
  - name: app_name
    required: true

nodes:
  - name: "{{app_name}}"
    type: directory
    children:
      - name: "frontend"
        type: directory
        children:
          - name: "src"
            type: directory
            children:
              - name: "App.tsx"
                type: file
                content: "export const App = () => <div>{{app_name}}</div>;"
          - name: "package.json"
            type: file
            content: '{"name": "{{app_name}}-frontend"}'
      - name: "backend"
        type: directory
        children:
          - name: "src"
            type: directory
            children:
              - name: "main.rs"
                type: file
                content: "fn main() { println!(\"{{app_name}}\"); }"
          - name: "Cargo.toml"
            type: file
            content: '[package]\nname = "{{app_name}}-backend"'
      - name: "docker-compose.yml"
        type: file
        content: |
          version: '3.8'
          services:
            frontend:
              build: ./frontend
            backend:
              build: ./backend
"#,
        )
        .unwrap();

    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
            "--var",
            "app_name=my-app",
        ])
        .assert()
        .success();

    // Assert
    let app_dir = output_dir.child("my-app");
    app_dir
        .child("frontend/src/App.tsx")
        .assert(predicate::path::exists());
    app_dir
        .child("backend/src/main.rs")
        .assert(predicate::path::exists());
    app_dir
        .child("docker-compose.yml")
        .assert(predicate::path::exists());
});

// ============================================================================
// E2E: Marketplace Search and Discovery
// ============================================================================

test!(e2e_marketplace_search_complete, {
    // Arrange & Act & Assert
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["market", "search", "rust", "--limit", "10"])
        .assert()
        .success();
});

test!(e2e_marketplace_search_with_filters, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "market",
            "search",
            "microservice",
            "--category",
            "backend",
            "--limit",
            "5",
        ])
        .assert()
        .success();
});

test!(e2e_marketplace_package_info, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["market", "info", "rust-cli-template"])
        .assert()
        .success();
});

test!(e2e_marketplace_list_installed, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["market", "list"])
        .assert()
        .success();
});

// ============================================================================
// E2E: Complete Project Generation Workflow
// ============================================================================

test!(e2e_project_gen_complete, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let project_dir = temp.child("my-cli-tool");

    // Act
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "project",
            "gen",
            "--name",
            "my-cli-tool",
            "--template",
            "rust-cli",
            "--output",
            project_dir.path().to_str().unwrap(),
            "--description",
            "A command-line tool",
        ])
        .assert()
        .success();

    // Assert
    project_dir.assert(predicate::path::exists());
});

test!(e2e_project_with_git_init, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let project_dir = temp.child("git-project");

    // Act
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "project",
            "gen",
            "--name",
            "git-project",
            "--template",
            "rust-lib",
            "--output",
            project_dir.path().to_str().unwrap(),
            "--git",
        ])
        .assert()
        .success();

    // Assert: Verify project directory was created with expected structure
    assert!(
        project_dir.path().exists(),
        "Project directory should be created"
    );
    assert!(
        project_dir.path().join("Cargo.toml").exists(),
        "Cargo.toml should be generated"
    );
    assert!(
        project_dir.path().join("src").exists(),
        "src directory should be created"
    );
});

// ============================================================================
// E2E: Complete Lifecycle Workflow
// ============================================================================

test!(e2e_lifecycle_complete_workflow, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let make_file = temp.child("make.toml");

    make_file
        .write_str(
            r#"
[project]
name = "production-service"
version = "1.0.0"

[[phases]]
name = "clean"
description = "Clean build artifacts"
commands = [
    "echo 'Cleaning...'",
    "rm -rf target/"
]

[[phases]]
name = "install"
description = "Install dependencies"
depends_on = ["clean"]
commands = [
    "echo 'Installing dependencies...'"
]

[[phases]]
name = "build"
description = "Build the project"
depends_on = ["install"]
commands = [
    "echo 'Building project...'",
    "cargo build --release"
]

[[phases]]
name = "test"
description = "Run tests"
depends_on = ["build"]
commands = [
    "echo 'Running tests...'",
    "cargo test"
]

[[phases]]
name = "deploy"
description = "Deploy to production"
depends_on = ["test"]
commands = [
    "echo 'Deploying to production...'"
]
"#,
        )
        .unwrap();

    // Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "lifecycle",
            "run",
            "deploy",
            "--manifest",
            make_file.path().to_str().unwrap(),
        ])
        .assert()
        .success();
});

test!(e2e_lifecycle_list_phases, {
    let temp = TempDir::new().unwrap();
    let make_file = temp.child("make.toml");

    make_file
        .write_str(
            r#"
[project]
name = "test"

[[phases]]
name = "build"
commands = ["echo 'build'"]

[[phases]]
name = "test"
commands = ["echo 'test'"]
"#,
        )
        .unwrap();

    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "lifecycle",
            "list",
            "--manifest",
            make_file.path().to_str().unwrap(),
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("build").or(predicate::str::contains("Phases")));
});

// ============================================================================
// E2E: Graph Operations Workflow
// ============================================================================

test!(e2e_graph_import_and_query, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let graph_file = temp.child("project-graph.ttl");

    graph_file
        .write_str(
            r#"
@prefix ex: <http://example.org/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .

ex:project1 a ex:Project ;
    dc:title "My Project" ;
    dc:description "A test project" ;
    ex:version "1.0.0" ;
    ex:language "Rust" .

ex:project2 a ex:Project ;
    dc:title "Another Project" ;
    dc:description "Another test" ;
    ex:version "2.0.0" ;
    ex:language "Python" .
"#,
        )
        .unwrap();

    // Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "graph",
            "import",
            graph_file.path().to_str().unwrap(),
            "--format",
            "turtle",
        ])
        .assert()
        .success();

    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "graph",
            "query",
            "SELECT ?title WHERE { ?s dc:title ?title }",
            "--format",
            "sparql",
        ])
        .assert()
        .success();
});

// ============================================================================
// E2E: AI Integration Workflow
// ============================================================================

#[cfg(feature = "live-llm-tests")]
test!(e2e_ai_generate_template, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let output_file = temp.child("ai-template.yaml");

    // Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "ai",
            "generate",
            "--prompt",
            "Create a Rust REST API template",
            "--output",
            output_file.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    output_file.assert(predicate::path::exists());
});

// ============================================================================
// E2E: Multi-Step Real-World Scenarios
// ============================================================================

test!(e2e_scenario_new_microservice_project, {
    // Arrange
    let temp = TempDir::new().unwrap();

    // Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["market", "search", "microservice", "--limit", "5"])
        .assert()
        .success();

    let project_dir = temp.child("payment-service");
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "project",
            "gen",
            "--name",
            "payment-service",
            "--template",
            "rust-microservice",
            "--output",
            project_dir.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    Command::cargo_bin("ggen")
        .unwrap()
        .args(["doctor"])
        .assert()
        .success();
});

test!(e2e_scenario_template_development, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("custom-template.yaml");
    let test_output = temp.child("test-output");

    template_file
        .write_str(
            r##"
name: "custom-template"
variables:
  - name: project_name
    required: true
nodes:
  - name: "{{project_name}}"
    type: directory
    children:
      - name: "README.md"
        type: file
        content: "# {{project_name}}"
"##,
        )
        .unwrap();

    // Act
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            test_output.path().to_str().unwrap(),
            "--var",
            "project_name=test-project",
        ])
        .assert()
        .success();

    // Assert
    test_output
        .child("test-project/README.md")
        .assert(predicate::path::exists());
});

// ============================================================================
// E2E: Error Recovery Scenarios
// ============================================================================

test!(e2e_recovery_invalid_template_graceful, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let bad_template = temp.child("bad.yaml");
    bad_template.write_str("!!!invalid yaml!!!").unwrap();

    // Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            bad_template.path().to_str().unwrap(),
            "--output",
            "/tmp/output",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
});

test!(e2e_recovery_network_timeout_graceful, {
    // Arrange & Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "market",
            "search",
            "nonexistent-package-xyz-123",
            "--limit",
            "1",
        ])
        .assert()
        .success();
});

// ============================================================================
// E2E: Performance Scenarios
// ============================================================================

test!(e2e_performance_large_template, {
    // Arrange
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("large-template.yaml");
    let output_dir = temp.child("large-output");

    let mut nodes = String::from(
        r#"
name: "large-project"
variables:
  - name: project_name
    required: true
nodes:
  - name: "{{project_name}}"
    type: directory
    children:
"#,
    );

    for i in 0..50 {
        nodes.push_str(&format!(
            r#"
      - name: "file_{}.txt"
        type: file
        content: "Content for file {}"
"#,
            i, i
        ));
    }

    template_file.write_str(&nodes).unwrap();

    // Act & Assert
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
            "--var",
            "project_name=large-project",
        ])
        .assert()
        .success();
});
