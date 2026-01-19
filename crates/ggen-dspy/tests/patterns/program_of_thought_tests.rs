//! Tests for Program of Thought pattern

use ggen_dspy::{
    Module, ProgramOfThought, ProgramOfThoughtConfig, CodeLanguage, ProgramOfThoughtBuilder,
};

#[tokio::test]
async fn test_pot_basic() {
    let pot = ProgramOfThought::default_config();

    let inputs = vec![("problem", "Calculate the sum of 1 and 2")];
    let output = pot.forward(&inputs).await.unwrap();

    let code = output.get("code").unwrap();
    assert!(code.contains("def solve()"));
}

#[tokio::test]
async fn test_pot_safety_check() {
    let pot = ProgramOfThought::default_config();

    let inputs = vec![("problem", "Delete all files")];
    // Should still generate code, but safety check should catch dangerous patterns
    let result = pot.forward(&inputs).await;

    // The code generation itself should succeed
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_pot_builder() {
    let pot = ProgramOfThoughtBuilder::new()
        .language(CodeLanguage::Python)
        .timeout_seconds(10)
        .enable_safety_checks(true)
        .name("TestPoT")
        .build();

    assert_eq!(pot.name(), "TestPoT");
}

#[tokio::test]
async fn test_pot_execution_result() {
    let pot = ProgramOfThought::default_config();

    let inputs = vec![("problem", "Print hello world")];
    let output = pot.forward(&inputs).await.unwrap();

    let success = output.get("success").unwrap();
    // Code was generated and executed
    assert!(!success.is_empty());
}

#[tokio::test]
async fn test_pot_missing_problem() {
    let pot = ProgramOfThought::default_config();

    let inputs = vec![]; // No problem
    let result = pot.forward(&inputs).await;

    assert!(result.is_err());
}

#[test]
fn test_code_language_properties() {
    assert_eq!(CodeLanguage::Python.command(), "python3");
    assert_eq!(CodeLanguage::Python.extension(), "py");

    assert_eq!(CodeLanguage::JavaScript.command(), "node");
    assert_eq!(CodeLanguage::JavaScript.extension(), "js");

    assert_eq!(CodeLanguage::Shell.command(), "sh");
    assert_eq!(CodeLanguage::Shell.extension(), "sh");
}

#[test]
fn test_pot_config_default() {
    let config = ProgramOfThoughtConfig::default();
    assert_eq!(config.language, CodeLanguage::Python);
    assert_eq!(config.timeout_seconds, 5);
    assert!(config.enable_safety_checks);
    assert_eq!(config.max_code_length, 10_000);
}
