// MCP Tool Wrappers for Code Generation Validation
// Demonstrates: Code validation through MCP tools, agent-driven validation

use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum McpToolError {
    #[error("Tool execution failed: {0}")]
    ExecutionFailed(String),

    #[error("Syntax error: {0}")]
    SyntaxError(String),

    #[error("Format error: {0}")]
    FormatError(String),

    #[error("Compilation error: {0}")]
    CompilationError(String),

    #[error("Test failure: {0}")]
    TestFailure(String),

    #[error("Tool not available: {0}")]
    ToolNotAvailable(String),
}

pub type McpResult<T> = Result<T, McpToolError>;

// ============================================================================
// MCP TOOL SCHEMAS
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxCheckRequest {
    pub code: String,
    pub language: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxCheckResult {
    pub valid: bool,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatCheckRequest {
    pub code: String,
    pub language: String,
    pub style: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatCheckResult {
    pub formatted: bool,
    pub issues: Vec<FormatIssue>,
    pub formatted_code: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatIssue {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompileRequest {
    pub code: String,
    pub language: String,
    pub framework: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompileResult {
    pub success: bool,
    pub output: String,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestRequest {
    pub code: String,
    pub language: String,
    pub test_framework: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    pub passed: bool,
    pub total_tests: usize,
    pub passed_tests: usize,
    pub failed_tests: usize,
    pub output: String,
    pub coverage: Option<f32>,
}

// ============================================================================
// MCP TOOLS INTERFACE
// ============================================================================

pub struct McpTools {
    // In a real scenario, these would be HTTP clients to MCP servers
    // For now, we simulate tool execution
}

impl McpTools {
    pub fn new() -> Self {
        Self {}
    }

    /// Validate syntax of generated code
    pub fn syntax_check(&self, request: SyntaxCheckRequest) -> McpResult<SyntaxCheckResult> {
        // Simulate syntax checking based on language
        match request.language.to_lowercase().as_str() {
            "rust" => self.check_rust_syntax(&request.code),
            "python" => self.check_python_syntax(&request.code),
            "typescript" => self.check_typescript_syntax(&request.code),
            _ => Err(McpToolError::ToolNotAvailable(format!(
                "Syntax checker not available for: {}",
                request.language
            ))),
        }
    }

    /// Check code formatting
    pub fn format_check(&self, request: FormatCheckRequest) -> McpResult<FormatCheckResult> {
        // Simulate format checking
        let has_issues = !request.code.starts_with("    ") && !request.code.is_empty();

        Ok(FormatCheckResult {
            formatted: !has_issues,
            issues: if has_issues {
                vec![FormatIssue {
                    line: 1,
                    column: 1,
                    message: "Indentation should be 4 spaces".to_string(),
                }]
            } else {
                vec![]
            },
            formatted_code: if has_issues {
                Some(format!("    {}", request.code))
            } else {
                None
            },
        })
    }

    /// Compile/check code compilation
    pub fn compile(&self, request: CompileRequest) -> McpResult<CompileResult> {
        match request.language.to_lowercase().as_str() {
            "rust" => self.compile_rust(&request.code, request.framework),
            "python" => self.compile_python(&request.code),
            "typescript" => self.compile_typescript(&request.code),
            _ => Err(McpToolError::ToolNotAvailable(format!(
                "Compiler not available for: {}",
                request.language
            ))),
        }
    }

    /// Run tests on generated code
    pub fn test(&self, request: TestRequest) -> McpResult<TestResult> {
        match request.language.to_lowercase().as_str() {
            "rust" => self.test_rust(&request.code),
            "python" => self.test_python(&request.code),
            "typescript" => self.test_typescript(&request.code),
            _ => Err(McpToolError::ToolNotAvailable(format!(
                "Test runner not available for: {}",
                request.language
            ))),
        }
    }

    // ========================================================================
    // LANGUAGE-SPECIFIC IMPLEMENTATIONS
    // ========================================================================

    fn check_rust_syntax(&self, code: &str) -> McpResult<SyntaxCheckResult> {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // Check for basic Rust syntax issues
        if !code.contains("fn ") && !code.is_empty() {
            warnings.push("No functions defined".to_string());
        }

        if code.contains("unwrap()") {
            warnings.push("Found unwrap() - consider using Result types".to_string());
        }

        Ok(SyntaxCheckResult {
            valid: errors.is_empty(),
            errors,
            warnings,
        })
    }

    fn check_python_syntax(&self, code: &str) -> McpResult<SyntaxCheckResult> {
        let mut errors = Vec::new();
        let warnings = Vec::new();

        // Check for basic Python syntax issues
        if code.contains("import ") && !code.contains("from ") && code.contains("as") {
            // Valid import pattern
        } else if code.contains("import ") {
            // Valid import
        }

        Ok(SyntaxCheckResult {
            valid: errors.is_empty(),
            errors,
            warnings,
        })
    }

    fn check_typescript_syntax(&self, code: &str) -> McpResult<SyntaxCheckResult> {
        let mut errors = Vec::new();
        let warnings = Vec::new();

        // Check for basic TypeScript syntax issues
        if code.contains("function ") || code.contains("=>") || code.contains("class ") {
            // Valid TypeScript
        }

        Ok(SyntaxCheckResult {
            valid: errors.is_empty(),
            errors,
            warnings,
        })
    }

    fn compile_rust(&self, code: &str, _framework: Option<String>) -> McpResult<CompileResult> {
        // Simulate Rust compilation
        let has_basic_structure = code.contains("fn ") || code.contains("struct ");

        if has_basic_structure {
            Ok(CompileResult {
                success: true,
                output: "Compiling... OK".to_string(),
                errors: vec![],
                warnings: vec![],
            })
        } else {
            Ok(CompileResult {
                success: false,
                output: "Compilation failed".to_string(),
                errors: vec!["No valid function or struct definitions".to_string()],
                warnings: vec![],
            })
        }
    }

    fn compile_python(&self, code: &str) -> McpResult<CompileResult> {
        // Simulate Python compilation
        let has_valid_syntax = !code.trim().is_empty();

        Ok(CompileResult {
            success: has_valid_syntax,
            output: if has_valid_syntax { "OK" } else { "Syntax error" }.to_string(),
            errors: if has_valid_syntax {
                vec![]
            } else {
                vec!["Invalid Python syntax".to_string()]
            },
            warnings: vec![],
        })
    }

    fn compile_typescript(&self, code: &str) -> McpResult<CompileResult> {
        // Simulate TypeScript compilation - check for any valid TS syntax
        let is_valid = code.contains("function ")
            || code.contains("class ")
            || code.contains("const ")
            || code.contains("let ")
            || code.contains("var ")
            || code.contains("=>")
            || !code.trim().is_empty();

        Ok(CompileResult {
            success: is_valid,
            output: if is_valid { "Compilation OK" } else { "Type error" }.to_string(),
            errors: if is_valid {
                vec![]
            } else {
                vec!["No valid type definitions".to_string()]
            },
            warnings: vec![],
        })
    }

    fn test_rust(&self, code: &str) -> McpResult<TestResult> {
        let has_tests = code.contains("#[test]") || code.contains("#[tokio::test]");

        Ok(TestResult {
            passed: has_tests,
            total_tests: if has_tests { 1 } else { 0 },
            passed_tests: if has_tests { 1 } else { 0 },
            failed_tests: 0,
            output: if has_tests {
                "test result: ok. 1 passed".to_string()
            } else {
                "No tests found".to_string()
            },
            coverage: Some(if has_tests { 0.85 } else { 0.0 }),
        })
    }

    fn test_python(&self, code: &str) -> McpResult<TestResult> {
        let has_tests = code.contains("def test_");

        Ok(TestResult {
            passed: has_tests,
            total_tests: if has_tests { 1 } else { 0 },
            passed_tests: if has_tests { 1 } else { 0 },
            failed_tests: 0,
            output: if has_tests {
                "1 passed".to_string()
            } else {
                "No tests found".to_string()
            },
            coverage: Some(if has_tests { 0.75 } else { 0.0 }),
        })
    }

    fn test_typescript(&self, code: &str) -> McpResult<TestResult> {
        let has_tests = code.contains("describe(") && code.contains("it(");

        Ok(TestResult {
            passed: has_tests,
            total_tests: if has_tests { 1 } else { 0 },
            passed_tests: if has_tests { 1 } else { 0 },
            failed_tests: 0,
            output: if has_tests {
                "1 passing".to_string()
            } else {
                "No tests found".to_string()
            },
            coverage: Some(if has_tests { 0.80 } else { 0.0 }),
        })
    }
}

impl Default for McpTools {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syntax_check_request_creation() {
        let req = SyntaxCheckRequest {
            code: "fn main() {}".to_string(),
            language: "rust".to_string(),
        };
        assert_eq!(req.language, "rust");
    }

    #[test]
    fn test_format_check_request_creation() {
        let req = FormatCheckRequest {
            code: "fn main() {}".to_string(),
            language: "rust".to_string(),
            style: "rustfmt".to_string(),
        };
        assert_eq!(req.style, "rustfmt");
    }

    #[test]
    fn test_compile_request_creation() {
        let req = CompileRequest {
            code: "fn main() {}".to_string(),
            language: "rust".to_string(),
            framework: Some("actix".to_string()),
        };
        assert!(req.framework.is_some());
    }

    #[test]
    fn test_test_request_creation() {
        let req = TestRequest {
            code: "fn test() {}".to_string(),
            language: "rust".to_string(),
            test_framework: "standard".to_string(),
        };
        assert_eq!(req.test_framework, "standard");
    }

    #[test]
    fn test_mcp_tools_creation() {
        let tools = McpTools::new();
        assert_eq!(std::mem::size_of_val(&tools), 0);
    }

    #[test]
    fn test_rust_syntax_check_valid() {
        let tools = McpTools::new();
        let result = tools.check_rust_syntax("fn main() {}").unwrap();
        assert!(result.valid);
    }

    #[test]
    fn test_rust_syntax_check_warning() {
        let tools = McpTools::new();
        let result = tools.check_rust_syntax("fn main() { value.unwrap() }").unwrap();
        assert!(!result.warnings.is_empty());
    }

    #[test]
    fn test_syntax_check_unknown_language() {
        let tools = McpTools::new();
        let req = SyntaxCheckRequest {
            code: "test".to_string(),
            language: "golang".to_string(),
        };
        let result = tools.syntax_check(req);
        assert!(result.is_err());
    }

    #[test]
    fn test_format_check_unformatted() {
        let tools = McpTools::new();
        let req = FormatCheckRequest {
            code: "fn main() {}".to_string(),
            language: "rust".to_string(),
            style: "rustfmt".to_string(),
        };
        let result = tools.format_check(req).unwrap();
        assert!(!result.formatted);
    }

    #[test]
    fn test_format_check_formatted() {
        let tools = McpTools::new();
        let req = FormatCheckRequest {
            code: "    fn main() {}".to_string(),
            language: "rust".to_string(),
            style: "rustfmt".to_string(),
        };
        let result = tools.format_check(req).unwrap();
        assert!(result.formatted);
    }

    #[test]
    fn test_compile_rust_valid() {
        let tools = McpTools::new();
        let req = CompileRequest {
            code: "fn main() {}".to_string(),
            language: "rust".to_string(),
            framework: None,
        };
        let result = tools.compile(req).unwrap();
        assert!(result.success);
    }

    #[test]
    fn test_compile_rust_invalid() {
        let tools = McpTools::new();
        let req = CompileRequest {
            code: "invalid code".to_string(),
            language: "rust".to_string(),
            framework: None,
        };
        let result = tools.compile(req).unwrap();
        assert!(!result.success);
    }

    #[test]
    fn test_compile_unknown_language() {
        let tools = McpTools::new();
        let req = CompileRequest {
            code: "test".to_string(),
            language: "golang".to_string(),
            framework: None,
        };
        let result = tools.compile(req);
        assert!(result.is_err());
    }

    #[test]
    fn test_test_rust_with_tests() {
        let tools = McpTools::new();
        let code = "
#[test]
fn test_example() {
    assert_eq!(2 + 2, 4);
}";
        let req = TestRequest {
            code: code.to_string(),
            language: "rust".to_string(),
            test_framework: "standard".to_string(),
        };
        let result = tools.test(req).unwrap();
        assert!(result.passed);
        assert_eq!(result.total_tests, 1);
    }

    #[test]
    fn test_test_rust_without_tests() {
        let tools = McpTools::new();
        let req = TestRequest {
            code: "fn main() {}".to_string(),
            language: "rust".to_string(),
            test_framework: "standard".to_string(),
        };
        let result = tools.test(req).unwrap();
        assert!(!result.passed);
        assert_eq!(result.total_tests, 0);
    }

    #[test]
    fn test_syntax_check_result_serialization() {
        let result = SyntaxCheckResult {
            valid: true,
            errors: vec![],
            warnings: vec!["unused variable".to_string()],
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"valid\":true"));
    }

    #[test]
    fn test_compile_result_serialization() {
        let result = CompileResult {
            success: true,
            output: "OK".to_string(),
            errors: vec![],
            warnings: vec![],
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"success\":true"));
    }

    #[test]
    fn test_test_result_serialization() {
        let result = TestResult {
            passed: true,
            total_tests: 5,
            passed_tests: 5,
            failed_tests: 0,
            output: "All tests passed".to_string(),
            coverage: Some(0.92),
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"passed\":true"));
    }

    #[test]
    fn test_format_issue_creation() {
        let issue = FormatIssue {
            line: 5,
            column: 10,
            message: "Indentation error".to_string(),
        };
        assert_eq!(issue.line, 5);
    }

    #[test]
    fn test_python_syntax_check() {
        let tools = McpTools::new();
        let result = tools.check_python_syntax("def test(): pass").unwrap();
        assert!(result.valid);
    }

    #[test]
    fn test_typescript_syntax_check() {
        let tools = McpTools::new();
        let result = tools.check_typescript_syntax("function test() {}").unwrap();
        assert!(result.valid);
    }

    #[test]
    fn test_compile_python() {
        let tools = McpTools::new();
        let req = CompileRequest {
            code: "x = 5".to_string(),
            language: "python".to_string(),
            framework: None,
        };
        let result = tools.compile(req).unwrap();
        assert!(result.success);
    }

    #[test]
    fn test_compile_typescript() {
        let tools = McpTools::new();
        let req = CompileRequest {
            code: "const x: number = 5;".to_string(),
            language: "typescript".to_string(),
            framework: None,
        };
        let result = tools.compile(req).unwrap();
        assert!(result.success);
    }

    #[test]
    fn test_test_python_with_tests() {
        let tools = McpTools::new();
        let req = TestRequest {
            code: "def test_example(): assert 2 + 2 == 4".to_string(),
            language: "python".to_string(),
            test_framework: "pytest".to_string(),
        };
        let result = tools.test(req).unwrap();
        assert!(result.passed);
    }

    #[test]
    fn test_test_typescript_with_tests() {
        let tools = McpTools::new();
        let req = TestRequest {
            code: "describe('test', () => { it('should pass', () => { expect(2+2).toBe(4); }); })".to_string(),
            language: "typescript".to_string(),
            test_framework: "jest".to_string(),
        };
        let result = tools.test(req).unwrap();
        assert!(result.passed);
    }
}
