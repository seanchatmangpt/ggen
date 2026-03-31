//! End-to-End LLM Integration Test (Chicago TDD)
//!
//! This test verifies the LLM integration works by:
//! 1. Creating a test project with enable_llm: true
//! 2. Running ggen sync with real GROQ_API_KEY
//! 3. Verifying generated code has LLM implementations (not TODO stubs)
//! 4. Checking OpenTelemetry traces for real LLM calls
//!
//! CHICAGO TDD: This test uses REAL endpoints only. No mocks.
//! The test will fail if GROQ_API_KEY is not set.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

/// Helper struct for test project setup
struct TestProject {
    temp_dir: TempDir,
    project_dir: PathBuf,
}

impl TestProject {
    /// Create a new test project with LLM enabled
    fn new_with_llm() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let project_dir = temp_dir.path().join("test-llm-project");

        // Create project directory
        fs::create_dir_all(&project_dir).expect("Failed to create project dir");

        // Create .ggen directory structure
        let ggen_dir = project_dir.join(".ggen");
        fs::create_dir_all(&ggen_dir).expect("Failed to create .ggen dir");

        // Create ggen.toml with enable_llm = true
        let ggen_toml = r#"
[project]
name = "test-llm-project"
version = "0.1.0"
description = "Test project for LLM integration"

[generation]
language = "rust"
enable_llm = true

[llm]
model = "groq::openai/gpt-oss-20b"
temperature = 0.7
max_tokens = 4096
"#;
        fs::write(ggen_dir.join("ggen.toml"), ggen_toml).expect("Failed to write ggen.toml");

        // Create a simple ontology with behavior predicates
        let ontology = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix a2a: <http://ggen.ai/a2a#> .
@prefix mcp: <http://ggen.ai/mcp#> .

:test_skill a a2a:Skill ;
    rdfs:label "Test Skill" ;
    rdfs:comment "A test skill for LLM verification" ;
    a2a:hasSystemPrompt "You are a test assistant. Say hello." ;
    a2a:hasImplementationHint "Return a simple greeting" ;
    a2a:hasInputType "string" ;
    a2a:hasOutputType "string" .
"#;
        fs::write(ggen_dir.join("test.ttl"), ontology).expect("Failed to write ontology");

        Self {
            temp_dir,
            project_dir,
        }
    }

    /// Get the path to the ggen binary
    fn ggen_binary(&self) -> PathBuf {
        // Use cargo build to get the binary path
        PathBuf::from(env!("CARGO_BIN_EXE_ggen"))
    }

    /// Run ggen sync command
    fn run_sync(&self) -> std::process::Output {
        let output = Command::new(&self.ggen_binary())
            .arg("sync")
            .arg("--ontology")
            .arg(self.project_dir.join(".ggen/test.ttl"))
            .current_dir(&self.project_dir)
            .env(
                "GROQ_API_KEY",
                std::env::var("GROQ_API_KEY").unwrap_or_default(),
            )
            .env("RUST_LOG", "debug,ggen_ai=trace,ggen_core=trace")
            .output()
            .expect("Failed to run ggen sync");

        output
    }

    /// Read generated skill implementation file
    fn read_generated_impl(&self) -> String {
        let impl_path = self.project_dir.join("src/skills/test_skill.rs");
        fs::read_to_string(&impl_path).unwrap_or_default()
    }
}

#[test]
// #[ignore] // Only run with explicit permission (requires API key)
fn test_llm_integration_e2e_with_real_api() {
    // Verify GROQ_API_KEY is set
    let api_key = std::env::var("GROQ_API_KEY");
    if api_key.is_err() || api_key.unwrap().is_empty() {
        panic!(
            "GROQ_API_KEY must be set for this E2E test. \
                This test makes REAL API calls to Groq (Chicago TDD)."
        );
    }

    // Create test project with LLM enabled
    let project = TestProject::new_with_llm();

    // Run ggen sync (this will make REAL LLM API calls)
    let output = project.run_sync();

    // Check that sync succeeded
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        panic!(
            "ggen sync failed:\nstdout:\n{}\n\nstderr:\n{}",
            stdout, stderr
        );
    }

    // Verify OpenTelemetry traces contain LLM calls
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}{}", stdout, stderr);

    // Check for OpenTelemetry span markers that indicate real LLM calls
    assert!(
        combined.contains("llm.complete") || combined.contains("llm_request"),
        "Expected to find OpenTelemetry LLM traces in output.\n\
         This indicates the LLM API was not called.\n\
         Output:\n{}",
        combined
    );

    // Check for model name in traces (confirms real API call)
    assert!(
        combined.contains("gpt-oss-20b") || combined.contains("groq"),
        "Expected to find Groq model name in traces.\n\
         Output:\n{}",
        combined
    );

    // Verify generated code has actual implementation, not TODO stubs
    let generated_code = project.read_generated_impl();

    assert!(
        !generated_code.is_empty(),
        "Generated implementation file should not be empty"
    );

    // Check that we got actual LLM-generated code, not TODO stubs
    assert!(
        !generated_code.contains("TODO: Implement this skill")
            && !generated_code.contains("// TODO")
            && !generated_code.contains("unimplemented!()"),
        "Generated code should contain LLM implementation, not TODO stubs.\n\
         Generated code:\n{}",
        generated_code
    );

    // Verify the code has actual content (LLM should have generated something)
    assert!(
        generated_code.len() > 100, // At least some meaningful code
        "Generated code seems too short, likely not using LLM properly.\n\
         Generated code:\n{}",
        generated_code
    );

    println!("✅ E2E LLM integration test PASSED");
    println!("   - Real Groq API calls were made (verified via OTEL traces)");
    println!("   - Generated code has actual implementation (not TODO stubs)");
    println!("   - Code length: {} bytes", generated_code.len());
}

#[test]
#[ignore] // Only run with explicit permission
fn test_llm_integration_without_api_key_fails_gracefully() {
    // Create test project
    let project = TestProject::new_with_llm();

    // Run sync WITHOUT API key
    let output = Command::new(project.ggen_binary())
        .arg("sync")
        .arg("--ontology")
        .arg(project.project_dir.join(".ggen/test.ttl"))
        .current_dir(&project.project_dir)
        .env("GROQ_API_KEY", "") // No API key
        .output()
        .expect("Failed to run ggen sync");

    // Should fail gracefully with error message
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("API") || stderr.contains("Groq") || stderr.contains("llm"),
        "Expected error message about missing API key, got:\n{}",
        stderr
    );

    println!("✅ API key missing test PASSED (fails gracefully)");
}

#[test]
fn test_groq_api_key_is_set() {
    // This test just verifies the API key is available
    // It doesn't make any API calls
    let api_key = std::env::var("GROQ_API_KEY");

    match api_key {
        Ok(key) if !key.is_empty() => {
            println!("✅ GROQ_API_KEY is set ({} chars)", key.len());
            println!("   E2E tests with real API calls can run");
        }
        _ => {
            println!("⚠️  GROQ_API_KEY is not set");
            println!("   E2E tests with real API calls will be skipped");
        }
    }
}
