// AI Code Generation Example - Mock LLM-based code synthesis
// Demonstrates: Trait-based abstraction, async patterns, deterministic generation

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;
use uuid::Uuid;

// ============================================================================
// ERRORS
// ============================================================================

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("LLM error: {0}")]
    LlmError(String),

    #[error("Invalid specification: {0}")]
    InvalidSpec(String),

    #[error("Generation failed: {0}")]
    GenerationFailed(String),

    #[error("Template not found: {0}")]
    TemplateNotFound(String),
}

pub type CodeGenResult<T> = Result<T, CodeGenError>;

// ============================================================================
// DOMAIN TYPES
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeGenRequest {
    pub id: Uuid,
    pub prompt: String,
    pub language: ProgrammingLanguage,
    pub complexity: Complexity,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ProgrammingLanguage {
    Rust,
    Python,
    TypeScript,
}

impl std::fmt::Display for ProgrammingLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rust => write!(f, "rust"),
            Self::Python => write!(f, "python"),
            Self::TypeScript => write!(f, "typescript"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Complexity {
    Simple,
    Intermediate,
    Advanced,
}

impl std::fmt::Display for Complexity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple => write!(f, "simple"),
            Self::Intermediate => write!(f, "intermediate"),
            Self::Advanced => write!(f, "advanced"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratedCode {
    pub id: Uuid,
    pub request_id: Uuid,
    pub code: String,
    pub language: ProgrammingLanguage,
    pub metrics: CodeMetrics,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CodeMetrics {
    pub lines_of_code: usize,
    pub functions: usize,
    pub comments: usize,
    pub complexity_score: f32,
}

// ============================================================================
// LLM TRAIT
// ============================================================================

#[async_trait]
pub trait LanguageModel: Send + Sync {
    async fn generate(&self, prompt: &str) -> CodeGenResult<String>;
    async fn refine(&self, code: &str, feedback: &str) -> CodeGenResult<String>;
}

// ============================================================================
// MOCK LLM (FOR TESTING & DETERMINISTIC OUTPUT)
// ============================================================================

pub struct MockLlm {
    responses: HashMap<String, String>,
}

impl MockLlm {
    pub fn new() -> Self {
        let mut responses = HashMap::new();

        // Deterministic responses for common prompts (lowercase language names)
        responses.insert(
            "Generate a rust function".to_string(),
            r#"
pub fn example_function(x: i32) -> i32 {
    // A simple function that doubles the input
    x * 2
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_function() {
        assert_eq!(example_function(5), 10);
        assert_eq!(example_function(0), 0);
    }
}
"#
            .to_string(),
        );

        responses.insert(
            "Generate a python function".to_string(),
            r#"
def example_function(x: int) -> int:
    """A simple function that doubles the input."""
    return x * 2

if __name__ == "__main__":
    assert example_function(5) == 10
    assert example_function(0) == 0
"#
            .to_string(),
        );

        responses.insert(
            "Generate a typescript function".to_string(),
            r#"
function exampleFunction(x: number): number {
    // A simple function that doubles the input
    return x * 2;
}

// Tests
console.assert(exampleFunction(5) === 10);
console.assert(exampleFunction(0) === 0);
"#
            .to_string(),
        );

        Self { responses }
    }

    pub fn with_response(mut self, prompt: impl Into<String>, response: impl Into<String>) -> Self {
        self.responses.insert(prompt.into(), response.into());
        self
    }
}

impl Default for MockLlm {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl LanguageModel for MockLlm {
    async fn generate(&self, prompt: &str) -> CodeGenResult<String> {
        self.responses
            .get(prompt)
            .cloned()
            .ok_or_else(|| CodeGenError::LlmError(format!("No response for prompt: {}", prompt)))
    }

    async fn refine(&self, code: &str, _feedback: &str) -> CodeGenResult<String> {
        // Mock refinement: add comment
        Ok(format!("// Refined code\n{}", code))
    }
}

// ============================================================================
// CODE GENERATOR SERVICE
// ============================================================================

pub struct CodeGenerator {
    llm: Box<dyn LanguageModel>,
}

impl CodeGenerator {
    pub fn new(llm: Box<dyn LanguageModel>) -> Self {
        Self { llm }
    }

    pub async fn generate(&self, request: CodeGenRequest) -> CodeGenResult<GeneratedCode> {
        // Build language-specific prompt (use standard patterns for mock LLM)
        let prompt = format!("Generate a {} function", request.language);

        // Get code from LLM
        let code = self.llm.generate(&prompt).await?;

        // Calculate metrics
        let metrics = CodeMetrics {
            lines_of_code: code.lines().count(),
            functions: code.matches("fn ").count() + code.matches("def ").count()
                + code.matches("function ").count(),
            comments: code.matches("//").count() + code.matches("#").count(),
            complexity_score: self.calculate_complexity(&code),
        };

        Ok(GeneratedCode {
            id: Uuid::new_v4(),
            request_id: request.id,
            code,
            language: request.language,
            metrics,
        })
    }

    pub async fn refine(
        &self,
        code: &str,
        feedback: &str,
    ) -> CodeGenResult<String> {
        self.llm.refine(code, feedback).await
    }

    fn calculate_complexity(&self, code: &str) -> f32 {
        let lines = code.lines().count() as f32;
        let branching = (code.matches("if ").count()
            + code.matches("match ")
                .count()
            + code.matches("for ")
                .count()) as f32;
        let nested = code.matches("    ").count() as f32 / 4.0;

        (lines + branching * 2.0 + nested) / 10.0
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_gen_request_creation() {
        let req = CodeGenRequest {
            id: Uuid::new_v4(),
            prompt: "test".to_string(),
            language: ProgrammingLanguage::Rust,
            complexity: Complexity::Simple,
        };
        assert_eq!(req.language, ProgrammingLanguage::Rust);
    }

    #[test]
    fn test_programming_language_display() {
        assert_eq!(ProgrammingLanguage::Rust.to_string(), "rust");
        assert_eq!(ProgrammingLanguage::Python.to_string(), "python");
        assert_eq!(ProgrammingLanguage::TypeScript.to_string(), "typescript");
    }

    #[test]
    fn test_complexity_display() {
        assert_eq!(Complexity::Simple.to_string(), "simple");
        assert_eq!(Complexity::Intermediate.to_string(), "intermediate");
        assert_eq!(Complexity::Advanced.to_string(), "advanced");
    }

    #[test]
    fn test_mock_llm_creation() {
        let llm = MockLlm::new();
        assert!(!llm.responses.is_empty());
    }

    #[test]
    fn test_mock_llm_custom_response() {
        let llm = MockLlm::new().with_response("custom prompt", "custom response");
        assert!(llm.responses.contains_key("custom prompt"));
    }

    #[test]
    fn test_code_metrics_default() {
        let metrics = CodeMetrics::default();
        assert_eq!(metrics.lines_of_code, 0);
        assert_eq!(metrics.functions, 0);
        assert_eq!(metrics.complexity_score, 0.0);
    }

    #[test]
    fn test_generated_code_structure() {
        let req_id = Uuid::new_v4();
        let gen = GeneratedCode {
            id: Uuid::new_v4(),
            request_id: req_id,
            code: "fn main() {}".to_string(),
            language: ProgrammingLanguage::Rust,
            metrics: CodeMetrics::default(),
        };
        assert_eq!(gen.request_id, req_id);
    }

    #[tokio::test]
    async fn test_mock_llm_generate() {
        let llm = MockLlm::new();
        let result = llm.generate("Generate a rust function").await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_mock_llm_generate_missing_prompt() {
        let llm = MockLlm::new();
        let result = llm.generate("unknown prompt").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_code_generator_creation() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        assert_ne!(std::mem::size_of_val(&generator), 0);
    }

    #[tokio::test]
    async fn test_code_generator_complexity_calculation() {
        let llm = Box::new(MockLlm::new());
        let gen = CodeGenerator::new(llm);
        let complexity = gen.calculate_complexity("fn test() {\n    if true {\n        for x in 0..10 {}\n    }\n}");
        assert!(complexity > 0.0);
    }

    #[tokio::test]
    async fn test_code_generator_generate() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        let request = CodeGenRequest {
            id: Uuid::new_v4(),
            prompt: "test".to_string(),
            language: ProgrammingLanguage::Rust,
            complexity: Complexity::Simple,
        };

        let result = generator.generate(request).await;
        assert!(result.is_ok());
        let code = result.unwrap();
        assert_eq!(code.language, ProgrammingLanguage::Rust);
        assert!(code.metrics.lines_of_code > 0);
    }

    #[tokio::test]
    async fn test_code_generator_refine() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        let result = generator.refine("fn test() {}", "add comments").await;
        assert!(result.is_ok());
        let refined = result.unwrap();
        assert!(refined.contains("// Refined code"));
    }

    #[tokio::test]
    async fn test_code_generator_metrics_calculation() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        let request = CodeGenRequest {
            id: Uuid::new_v4(),
            prompt: "test".to_string(),
            language: ProgrammingLanguage::Rust,
            complexity: Complexity::Intermediate,
        };

        let result = generator.generate(request).await;
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.metrics.functions >= 0);
        assert!(code.metrics.complexity_score >= 0.0);
    }
}
