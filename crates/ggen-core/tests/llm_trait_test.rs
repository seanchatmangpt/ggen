//! LLM Service Trait Unit Tests
//!
//! These tests verify the LlmService trait works correctly for dependency injection.

use ggen_core::codegen::pipeline::LlmService;
use std::error::Error;

// Mock LLM service for testing
#[derive(Clone)]
struct TestLlmService {
    response_template: String,
}

impl TestLlmService {
    fn new(response_template: &str) -> Self {
        Self {
            response_template: response_template.to_string(),
        }
    }
}

impl LlmService for TestLlmService {
    fn clone_box(&self) -> Box<dyn LlmService> {
        Box::new(self.clone())
    }
    fn generate_skill_impl(
        &self, skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
    ) -> std::result::Result<String, Box<dyn Error + Send + Sync>> {
        let response = self
            .response_template
            .replace("{{skill_name}}", skill_name)
            .replace("{{system_prompt}}", system_prompt)
            .replace("{{implementation_hint}}", implementation_hint)
            .replace("{{language}}", language);

        Ok(response)
    }
}

#[test]
fn test_llm_trait_basic() {
    let service = TestLlmService::new("impl {{skill_name}} for {{language}}");

    let result = service.generate_skill_impl(
        "search_files",
        "Search files by pattern",
        "Use glob",
        "rust",
    );

    assert!(result.is_ok());
    let impl_code = result.unwrap();
    assert!(impl_code.contains("search_files"));
    assert!(impl_code.contains("rust"));
}

#[test]
fn test_llm_trait_error_handling() {
    // Test a service that returns errors
    #[derive(Clone)]
    struct FailingLlmService;

    impl LlmService for FailingLlmService {
        fn clone_box(&self) -> Box<dyn LlmService> {
            Box::new(self.clone())
        }
        fn generate_skill_impl(
            &self, _skill_name: &str, _system_prompt: &str, _implementation_hint: &str,
            _language: &str,
        ) -> std::result::Result<String, Box<dyn Error + Send + Sync>> {
            Err("LLM service unavailable".into())
        }
    }

    let service = FailingLlmService;
    let result = service.generate_skill_impl("test", "desc", "hint", "rust");

    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("unavailable"));
}

#[test]
fn test_llm_trait_language_variants() {
    let service = TestLlmService::new("// {{language}}: {{skill_name}}");

    // Test Rust
    let rust_result = service
        .generate_skill_impl("skill1", "desc", "hint", "rust")
        .unwrap();
    assert!(rust_result.contains("rust:"));

    // Test Elixir
    let elixir_result = service
        .generate_skill_impl("skill2", "desc", "hint", "elixir")
        .unwrap();
    assert!(elixir_result.contains("elixir:"));

    // Test TypeScript
    let ts_result = service
        .generate_skill_impl("skill3", "desc", "hint", "typescript")
        .unwrap();
    assert!(ts_result.contains("typescript:"));
}
