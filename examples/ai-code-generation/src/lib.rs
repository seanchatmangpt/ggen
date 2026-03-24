// AI Code Generation Example - Mock LLM-based code synthesis
// Demonstrates: Trait-based abstraction, async patterns, deterministic generation, MCP tool integration

pub mod mcp_tools;
pub mod rdf_spec;

pub use rdf_spec::{RdfSpecParser, CodeGenSpec, ComplexityLevel, ValidationRule, ValidationType};

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;
use uuid::Uuid;
use mcp_tools::{McpTools, SyntaxCheckRequest, FormatCheckRequest, CompileRequest, TestRequest};

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    pub syntax_valid: bool,
    pub format_valid: bool,
    pub compile_valid: bool,
    pub tests_pass: bool,
    pub formatted_code: String,
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
    mcp_tools: McpTools,
}

impl CodeGenerator {
    pub fn new(llm: Box<dyn LanguageModel>) -> Self {
        Self {
            llm,
            mcp_tools: McpTools::new(),
        }
    }

    pub fn with_mcp_tools(llm: Box<dyn LanguageModel>, mcp_tools: McpTools) -> Self {
        Self { llm, mcp_tools }
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

    /// Validate generated code using MCP tools
    pub fn validate_syntax(&self, code: &str, language: &ProgrammingLanguage) -> CodeGenResult<bool> {
        let request = SyntaxCheckRequest {
            code: code.to_string(),
            language: language.to_string(),
        };
        match self.mcp_tools.syntax_check(request) {
            Ok(result) => Ok(result.valid),
            Err(e) => Err(CodeGenError::GenerationFailed(format!("Syntax check failed: {}", e))),
        }
    }

    /// Check code formatting using MCP tools
    pub fn validate_format(&self, code: &str, language: &ProgrammingLanguage) -> CodeGenResult<String> {
        let request = FormatCheckRequest {
            code: code.to_string(),
            language: language.to_string(),
            style: "standard".to_string(),
        };
        match self.mcp_tools.format_check(request) {
            Ok(result) => Ok(result.formatted_code.unwrap_or_else(|| code.to_string())),
            Err(e) => Err(CodeGenError::GenerationFailed(format!("Format check failed: {}", e))),
        }
    }

    /// Compile code using MCP tools
    pub fn validate_compile(&self, code: &str, language: &ProgrammingLanguage) -> CodeGenResult<bool> {
        let request = CompileRequest {
            code: code.to_string(),
            language: language.to_string(),
            framework: None,
        };
        match self.mcp_tools.compile(request) {
            Ok(result) => {
                if result.success {
                    Ok(true)
                } else {
                    Err(CodeGenError::GenerationFailed(
                        format!("Compilation failed: {}", result.errors.join("; "))
                    ))
                }
            }
            Err(e) => Err(CodeGenError::GenerationFailed(format!("Compilation check failed: {}", e))),
        }
    }

    /// Run tests on code using MCP tools
    pub fn validate_tests(&self, code: &str, language: &ProgrammingLanguage) -> CodeGenResult<bool> {
        let framework = match language {
            ProgrammingLanguage::Rust => "standard",
            ProgrammingLanguage::Python => "pytest",
            ProgrammingLanguage::TypeScript => "jest",
        };
        let request = TestRequest {
            code: code.to_string(),
            language: language.to_string(),
            test_framework: framework.to_string(),
        };
        match self.mcp_tools.test(request) {
            Ok(result) => Ok(result.passed),
            Err(e) => Err(CodeGenError::GenerationFailed(format!("Test execution failed: {}", e))),
        }
    }

    /// Full validation pipeline
    pub fn validate_all(&self, code: &str, language: &ProgrammingLanguage) -> CodeGenResult<ValidationReport> {
        let syntax_valid = self.validate_syntax(code, language)?;
        let formatted_code = self.validate_format(code, language)?;
        let compile_valid = self.validate_compile(&formatted_code, language)?;
        let tests_pass = self.validate_tests(&formatted_code, language).unwrap_or(false);

        Ok(ValidationReport {
            syntax_valid,
            format_valid: true,
            compile_valid,
            tests_pass,
            formatted_code,
        })
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

    #[test]
    fn test_validation_report_creation() {
        let report = ValidationReport {
            syntax_valid: true,
            format_valid: true,
            compile_valid: true,
            tests_pass: true,
            formatted_code: "fn main() {}".to_string(),
        };
        assert!(report.syntax_valid);
        assert!(report.compile_valid);
    }

    #[test]
    fn test_code_generator_validate_syntax() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_syntax("fn main() {}", &ProgrammingLanguage::Rust);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_format() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_format("fn main() {}", &ProgrammingLanguage::Rust);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_compile() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_compile("fn main() {}", &ProgrammingLanguage::Rust);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_tests() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let code_with_tests = r#"
#[test]
fn test_example() {
    assert_eq!(2 + 2, 4);
}
"#;
        let result = generator.validate_tests(code_with_tests, &ProgrammingLanguage::Rust);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_all() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_all("fn main() {}", &ProgrammingLanguage::Rust);
        assert!(result.is_ok());
        let report = result.unwrap();
        assert!(report.syntax_valid);
    }

    #[test]
    fn test_code_generator_validate_python() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_syntax("def main(): pass", &ProgrammingLanguage::Python);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_typescript() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_syntax("function main() {}", &ProgrammingLanguage::TypeScript);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_with_mcp_tools() {
        let llm = Box::new(MockLlm::new());
        let mcp_tools = mcp_tools::McpTools::new();
        let generator = CodeGenerator::with_mcp_tools(llm, mcp_tools);
        assert_ne!(std::mem::size_of_val(&generator), 0);
    }

    #[tokio::test]
    async fn test_code_generator_full_pipeline() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        let request = CodeGenRequest {
            id: Uuid::new_v4(),
            prompt: "Generate a function".to_string(),
            language: ProgrammingLanguage::Rust,
            complexity: Complexity::Simple,
        };

        let generated = generator.generate(request).await.unwrap();
        let validation = generator.validate_all(&generated.code, &ProgrammingLanguage::Rust).unwrap();
        assert!(validation.syntax_valid);
    }

    #[test]
    fn test_code_generator_validate_format_python() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_format("def test(): pass", &ProgrammingLanguage::Python);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_format_typescript() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_format("function test() {}", &ProgrammingLanguage::TypeScript);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_compile_python() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_compile("x = 5", &ProgrammingLanguage::Python);
        assert!(result.is_ok());
    }

    #[test]
    fn test_code_generator_validate_compile_typescript() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);
        let result = generator.validate_compile("const x = 5;", &ProgrammingLanguage::TypeScript);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validation_report_serialization() {
        let report = ValidationReport {
            syntax_valid: true,
            format_valid: true,
            compile_valid: true,
            tests_pass: false,
            formatted_code: "fn main() {}".to_string(),
        };
        let json = serde_json::to_string(&report).unwrap();
        assert!(json.contains("\"syntax_valid\":true"));
    }

    #[test]
    fn test_validation_report_all_checks_pass() {
        let report = ValidationReport {
            syntax_valid: true,
            format_valid: true,
            compile_valid: true,
            tests_pass: true,
            formatted_code: "fn main() {}".to_string(),
        };
        let all_pass = report.syntax_valid && report.format_valid && report.compile_valid && report.tests_pass;
        assert!(all_pass);
    }

    #[test]
    fn test_validation_report_partial_failure() {
        let report = ValidationReport {
            syntax_valid: true,
            format_valid: true,
            compile_valid: false,
            tests_pass: false,
            formatted_code: "broken code".to_string(),
        };
        let all_pass = report.syntax_valid && report.format_valid && report.compile_valid && report.tests_pass;
        assert!(!all_pass);
    }

    // ========================================================================
    // RDF SPECIFICATION INTEGRATION TESTS
    // ========================================================================

    #[test]
    fn test_rdf_spec_parser_integration() {
        use crate::rdf_spec::RdfSpecParser;
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/api-spec";

        parser.add_triple(spec_id, "name", "RESTfulAPI");
        parser.add_triple(spec_id, "description", "A RESTful API specification");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Rust");
        parser.add_triple(spec_id, "framework", "Actix");
        parser.add_triple(spec_id, "complexityLevel", "Advanced");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert_eq!(spec.name, "RESTfulAPI");
        assert_eq!(spec.output_language, "Rust");
    }

    #[test]
    fn test_rdf_spec_with_validation_rules() {
        use crate::rdf_spec::{RdfSpecParser, ValidationType};
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/validated-spec";
        let syntax_val = "http://example.com/val1";
        let compile_val = "http://example.com/val2";

        parser.add_triple(spec_id, "name", "ValidatedSpec");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Rust");
        parser.add_triple(spec_id, "validationStep", syntax_val);
        parser.add_triple(spec_id, "validationStep", compile_val);
        parser.add_triple(syntax_val, "type", "Syntax");
        parser.add_triple(syntax_val, "enabled", "true");
        parser.add_triple(compile_val, "type", "Compile");
        parser.add_triple(compile_val, "enabled", "true");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert_eq!(spec.validation_steps.len(), 2);
        assert_eq!(spec.validation_steps[0].rule_type, ValidationType::Syntax);
        assert_eq!(spec.validation_steps[1].rule_type, ValidationType::Compile);
    }

    // ========================================================================
    // AGENT-DRIVEN GENERATION TESTS
    // ========================================================================

    #[tokio::test]
    async fn test_agent_generates_code_from_spec() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        let request = CodeGenRequest {
            id: Uuid::new_v4(),
            prompt: "Generate a rust function".to_string(),
            language: ProgrammingLanguage::Rust,
            complexity: Complexity::Intermediate,
        };

        let result = generator.generate(request).await;
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.code.contains("fn"));
        assert!(code.metrics.lines_of_code > 0);
    }

    #[test]
    fn test_mcp_tools_integration() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        let rust_code = r#"
fn add(a: i32, b: i32) -> i32 {
    a + b
}
"#;

        let result = generator.validate_syntax(rust_code, &ProgrammingLanguage::Rust);
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_code_validation_pipeline() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        let rust_code = "fn test() -> i32 { 42 }";

        let syntax_valid = generator.validate_syntax(rust_code, &ProgrammingLanguage::Rust).unwrap();
        assert!(syntax_valid);

        let formatted = generator.validate_format(rust_code, &ProgrammingLanguage::Rust).unwrap();
        assert!(!formatted.is_empty());
    }

    // ========================================================================
    // ERROR HANDLING TESTS
    // ========================================================================

    #[test]
    fn test_code_gen_error_variants() {
        let err1 = CodeGenError::LlmError("test error".to_string());
        let err2 = CodeGenError::InvalidSpec("invalid spec".to_string());
        let err3 = CodeGenError::GenerationFailed("failed".to_string());
        let err4 = CodeGenError::TemplateNotFound("template".to_string());

        assert_eq!(format!("{:?}", err1).contains("LlmError"), true);
        assert_eq!(format!("{:?}", err2).contains("InvalidSpec"), true);
        assert_eq!(format!("{:?}", err3).contains("GenerationFailed"), true);
        assert_eq!(format!("{:?}", err4).contains("TemplateNotFound"), true);
    }

    #[test]
    fn test_mcp_tool_error_variants() {
        use crate::mcp_tools::McpToolError;
        let err1 = McpToolError::ExecutionFailed("test".to_string());
        let err2 = McpToolError::SyntaxError("syntax".to_string());
        let err3 = McpToolError::CompilationError("compile".to_string());

        assert_eq!(format!("{:?}", err1).contains("ExecutionFailed"), true);
        assert_eq!(format!("{:?}", err2).contains("SyntaxError"), true);
        assert_eq!(format!("{:?}", err3).contains("CompilationError"), true);
    }

    // ========================================================================
    // LANGUAGE COVERAGE TESTS
    // ========================================================================

    #[test]
    fn test_rust_code_generation() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        assert!(generator
            .validate_syntax("fn test() {}", &ProgrammingLanguage::Rust)
            .unwrap());
    }

    #[test]
    fn test_python_code_generation() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        assert!(generator
            .validate_syntax("def test(): pass", &ProgrammingLanguage::Python)
            .unwrap());
    }

    #[test]
    fn test_typescript_code_generation() {
        let llm = Box::new(MockLlm::new());
        let generator = CodeGenerator::new(llm);

        assert!(generator
            .validate_syntax("function test() {}", &ProgrammingLanguage::TypeScript)
            .unwrap());
    }

    // ========================================================================
    // METRICS AND ANALYSIS TESTS
    // ========================================================================

    #[test]
    fn test_code_metrics_calculation() {
        let code = r#"
fn add(a: i32, b: i32) -> i32 {
    // Add two numbers
    a + b
}
"#;
        let lines = code.lines().count();
        assert!(lines > 0);

        let functions = code.matches("fn ").count();
        assert_eq!(functions, 1);

        let comments = code.matches("//").count();
        assert_eq!(comments, 1);
    }

    #[test]
    fn test_complexity_calculation() {
        let simple_code = "fn test() { 42 }";
        let complex_code = "fn test() { if x { for i in 0..10 { match y { } } } }";

        assert!(simple_code.len() < complex_code.len());
    }

    // ========================================================================
    // MOCK LLM TESTS
    // ========================================================================

    #[test]
    fn test_mock_llm_default() {
        let llm = MockLlm::default();
        assert!(!llm.responses.is_empty());
    }

    #[test]
    fn test_mock_llm_with_custom_response() {
        let llm = MockLlm::new()
            .with_response("custom prompt", "custom response");
        assert!(llm.responses.contains_key("custom prompt"));
    }

    #[tokio::test]
    async fn test_mock_llm_generate() {
        let llm = MockLlm::new();
        let result = llm.generate("Generate a rust function").await;
        assert!(result.is_ok());
        assert!(result.unwrap().contains("fn"));
    }

    #[tokio::test]
    async fn test_mock_llm_refine() {
        let llm = MockLlm::new();
        let result = llm.refine("fn test() {}", "add comments").await;
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Refined"));
    }

    // ========================================================================
    // SERIALIZATION TESTS
    // ========================================================================

    #[test]
    fn test_code_gen_request_serialization() {
        let request = CodeGenRequest {
            id: Uuid::new_v4(),
            prompt: "test prompt".to_string(),
            language: ProgrammingLanguage::Rust,
            complexity: Complexity::Simple,
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("rust"));
        assert!(json.contains("test prompt"));

        let deserialized: CodeGenRequest = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.language, ProgrammingLanguage::Rust);
    }

    #[test]
    fn test_generated_code_serialization() {
        let code = GeneratedCode {
            id: Uuid::new_v4(),
            request_id: Uuid::new_v4(),
            code: "fn test() {}".to_string(),
            language: ProgrammingLanguage::Rust,
            metrics: CodeMetrics {
                lines_of_code: 1,
                functions: 1,
                comments: 0,
                complexity_score: 0.5,
            },
        };

        let json = serde_json::to_string(&code).unwrap();
        assert!(json.contains("rust"));
        assert!(json.contains("fn test"));
    }
}
