//! End-to-end integration tests for AI commands
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL API calls (when available)
//! - REAL state verification
//! - NO mocking of AI responses
//!
//! **Critical User Workflows (80/20)**:
//! 1. Generate code with AI assistance
//! 2. Interactive AI chat session
//! 3. Analyze code and provide insights
//!
//! NOTE: AI commands are currently stubbed pending domain refactoring.
//! These tests verify CLI interface works correctly.

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

/// Helper to create ggen command
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

#[test]
fn test_ai_generate_executes() {
    // Chicago TDD: Verify AI generate command runs (stubbed)
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("generate")
        .arg("write a hello world function")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Stub returns success: false, but command should execute without error
}

#[test]
fn test_ai_generate_with_language() {
    // Chicago TDD: Verify language parameter accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("generate")
        .arg("write a function")
        .arg("--language")
        .arg("rust")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ai_generate_with_model() {
    // Chicago TDD: Verify model parameter accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("generate")
        .arg("test prompt")
        .arg("--model")
        .arg("claude-3")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ai_chat_executes() {
    // Chicago TDD: Verify AI chat command runs (stubbed)
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("chat")
        .arg("--message")
        .arg("hello")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ai_chat_interactive() {
    // Chicago TDD: Verify interactive flag accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("chat")
        .arg("--interactive")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ai_analyze_executes() {
    // Chicago TDD: Verify AI analyze command runs (stubbed)
    let temp_dir = TempDir::new().unwrap();

    // Create a test file to analyze
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() { println!(\"hello\"); }").unwrap();

    ggen()
        .arg("ai")
        .arg("analyze")
        .arg("--file")
        .arg(test_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ai_analyze_with_code() {
    // Chicago TDD: Verify code parameter accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("analyze")
        .arg("--code")
        .arg("fn test() {}")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
fn test_ai_help_shows_verbs() {
    // Chicago TDD: Verify help state is comprehensive
    ggen()
        .arg("ai")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("generate"))
        .stdout(predicate::str::contains("chat"))
        .stdout(predicate::str::contains("analyze"));
}

#[test]
fn test_ai_generate_help() {
    // Chicago TDD: Verify verb-specific help
    ggen()
        .arg("ai")
        .arg("generate")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("prompt").or(predicate::str::contains("generate")));
}

#[test]
fn test_ai_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("ai")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}
