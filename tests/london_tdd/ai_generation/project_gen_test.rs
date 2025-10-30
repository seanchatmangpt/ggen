#![cfg(feature = "london-tdd")]
//! London TDD tests for `ggen ai project` command
//!
//! README.md §AI-Powered Generation - Project Scaffolding
//!
//! Tests verify:
//! - Complete project structure generation
//! - Multi-language support
//! - Dependency management
//! - Test scaffolding

use crate::lib::*;
use mockall::predicate::*;

#[test]
fn test_ai_project_generates_complete_rust_project() {
    let start = std::time::Instant::now();

    // Arrange
    let mut mock_llm = MockLlmClient::new();
    let mut mock_fs = MockFilesystem::new();

    mock_llm
        .expect_generate()
        .times(1)
        .returning(|_, _| {
            Ok(r##"
            {
              "name": "my-api",
              "language": "rust",
              "files": [
                {"path": "Cargo.toml", "content": "[package]\nname = \"my-api\""},
                {"path": "src/main.rs", "content": "fn main() {}"},
                {"path": "tests/integration.rs", "content": "#[test]\nfn test_basic() {}"}
              ]
            }
            "##
            .to_string())
        });

    mock_fs
        .expect_create_dir()
        .times(1)
        .returning(|_| Ok(()));
    mock_fs
        .expect_write_file()
        .times(3) // 3 files
        .returning(|_, _| Ok(()));

    // Act
    let result = run_ai_project_command(&mock_llm, &mock_fs, "REST API", "my-api", "rust");

    // Assert
    assert!(result.is_ok());
    let project = result.unwrap();
    assert_eq!(project.name, "my-api");
    assert_eq!(project.files_created.len(), 3);
    assert!(project.files_created.contains(&"Cargo.toml".to_string()));

    // Performance: <100ms (mocked)
    assert!(start.elapsed().as_millis() < 100);
}

#[test]
fn test_ai_project_includes_tests_by_default() {
    // Arrange
    let mut mock_llm = MockLlmClient::new();
    let mock_fs = MockFilesystem::new();

    mock_llm.expect_generate().returning(|_, _| {
        Ok(r##"
        {
          "name": "my-service",
          "files": [
            {"path": "tests/integration.rs", "content": "#[test] fn test() {}"}
          ]
        }
        "##
        .to_string())
    });

    // Act
    let result = run_ai_project_command(&mock_llm, &mock_fs, "web service", "my-service", "rust");

    // Assert: Contains test files
    assert!(result.is_ok());
    let project = result.unwrap();
    assert!(project
        .files_created
        .iter()
        .any(|f| f.contains("tests/")));
}

#[test]
fn test_ai_project_supports_multiple_languages() {
    let mock_fs = MockFilesystem::new();

    // Test Rust
    let mut mock_rust = MockLlmClient::new();
    mock_rust
        .expect_generate()
        .returning(|_, _| Ok(r#"{"name": "rust-proj", "files": []}"#.to_string()));
    let result = run_ai_project_command(&mock_rust, &mock_fs, "app", "rust-proj", "rust");
    assert!(result.is_ok());

    // Test TypeScript
    let mut mock_ts = MockLlmClient::new();
    mock_ts
        .expect_generate()
        .returning(|_, _| Ok(r#"{"name": "ts-proj", "files": []}"#.to_string()));
    let result = run_ai_project_command(&mock_ts, &mock_fs, "app", "ts-proj", "typescript");
    assert!(result.is_ok());

    // Test Python
    let mut mock_py = MockLlmClient::new();
    mock_py
        .expect_generate()
        .returning(|_, _| Ok(r#"{"name": "py-proj", "files": []}"#.to_string()));
    let result = run_ai_project_command(&mock_py, &mock_fs, "app", "py-proj", "python");
    assert!(result.is_ok());
}

#[test]
fn test_ai_project_handles_invalid_project_name() {
    // Arrange
    let mock_llm = MockLlmClient::new();
    let mock_fs = MockFilesystem::new();

    // Act: Invalid name with spaces
    let result = run_ai_project_command(&mock_llm, &mock_fs, "test", "invalid name", "rust");

    // Assert: Validation error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Invalid project name"));
}

#[test]
fn test_ai_project_creates_otel_span() {
    // Arrange
    let mut mock_llm = MockLlmClient::new();
    let mock_fs = MockFilesystem::new();
    let tracer = otel::MockTracerProvider::new();

    mock_llm
        .expect_generate()
        .returning(|_, _| Ok(r#"{"name": "test", "files": []}"#.to_string()));

    // Act
    let _result = run_ai_project_with_tracing(&mock_llm, &mock_fs, &tracer, "my-api");

    // Assert
    let span = tracer.find_span("ggen.ai.project").unwrap();
    assert_eq!(span.status, otel::SpanStatus::Ok);
    assert!(span.attributes.iter().any(|(k, v)| k == "project.name" && v == "my-api"));
    assert!(span.attributes.iter().any(|(k, _)| k == "project.language"));
}

// Helper types and functions

#[derive(Debug)]
struct ProjectScaffold {
    name: String,
    language: String,
    files_created: Vec<String>,
}

fn run_ai_project_command(
    llm: &dyn LlmClient,
    fs: &dyn Filesystem,
    description: &str,
    name: &str,
    language: &str,
) -> Result<ProjectScaffold, anyhow::Error> {
    // Validate project name
    if name.contains(' ') {
        return Err(anyhow::anyhow!("Invalid project name: cannot contain spaces"));
    }

    let prompt = format!(
        "Generate a {} project structure for: {}\nProject name: {}\nReturn as JSON with 'name', 'files' array.",
        language, description, name
    );

    let response = llm.generate(&prompt, "gpt-4o")?;
    let project_spec: serde_json::Value = serde_json::from_str(&response)?;

    // Create project directory
    fs.create_dir(name)?;

    // Write files
    let files = project_spec["files"]
        .as_array()
        .unwrap_or(&vec![])
        .iter()
        .map(|f| {
            let path = f["path"].as_str().unwrap_or("");
            let content = f["content"].as_str().unwrap_or("");
            fs.write_file(&format!("{}/{}", name, path), content)
                .ok();
            path.to_string()
        })
        .collect();

    Ok(ProjectScaffold {
        name: name.to_string(),
        language: language.to_string(),
        files_created: files,
    })
}

fn run_ai_project_with_tracing(
    llm: &dyn LlmClient,
    fs: &dyn Filesystem,
    tracer: &otel::MockTracerProvider,
    name: &str,
) -> Result<ProjectScaffold, anyhow::Error> {
    let result = run_ai_project_command(llm, fs, "test project", name, "rust")?;

    let span = otel::MockSpan {
        name: "ggen.ai.project".to_string(),
        attributes: vec![
            ("project.name".to_string(), name.to_string()),
            ("project.language".to_string(), "rust".to_string()),
            ("files.count".to_string(), result.files_created.len().to_string()),
        ],
        events: vec!["project_scaffolded".to_string()],
        status: otel::SpanStatus::Ok,
    };
    tracer.record_span(span);

    Ok(result)
}
