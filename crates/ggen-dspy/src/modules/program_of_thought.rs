//! Program of Thought pattern - Code generation and execution
//!
//! Generates executable code to solve problems, combining reasoning with computation.

use crate::{DspyError, Module, ModuleOutput, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::process::{Command, Stdio};

/// Execution result from code execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionResult {
    /// Generated code
    pub code: String,

    /// Standard output
    pub stdout: String,

    /// Standard error
    pub stderr: String,

    /// Exit code
    pub exit_code: i32,

    /// Execution time in milliseconds
    pub execution_time_ms: u64,
}

impl ExecutionResult {
    /// Check if execution was successful
    pub fn is_success(&self) -> bool {
        self.exit_code == 0 && self.stderr.is_empty()
    }

    /// Get output or error
    pub fn output_or_error(&self) -> &str {
        if self.is_success() {
            &self.stdout
        } else {
            &self.stderr
        }
    }
}

/// Language for code execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CodeLanguage {
    /// Python 3
    Python,

    /// JavaScript/Node.js
    JavaScript,

    /// Rust
    Rust,

    /// Shell script
    Shell,
}

impl CodeLanguage {
    /// Get the interpreter/compiler command
    fn command(&self) -> &str {
        match self {
            Self::Python => "python3",
            Self::JavaScript => "node",
            Self::Rust => "rustc",
            Self::Shell => "sh",
        }
    }

    /// Get file extension
    fn extension(&self) -> &str {
        match self {
            Self::Python => "py",
            Self::JavaScript => "js",
            Self::Rust => "rs",
            Self::Shell => "sh",
        }
    }
}

/// Program of Thought configuration
#[derive(Debug, Clone)]
pub struct ProgramOfThoughtConfig {
    /// Programming language to use
    pub language: CodeLanguage,

    /// Timeout for code execution in seconds
    pub timeout_seconds: u64,

    /// Enable safety checks
    pub enable_safety_checks: bool,

    /// Maximum code length
    pub max_code_length: usize,
}

impl Default for ProgramOfThoughtConfig {
    fn default() -> Self {
        Self {
            language: CodeLanguage::Python,
            timeout_seconds: 5,
            enable_safety_checks: true,
            max_code_length: 10_000,
        }
    }
}

/// Program of Thought module
///
/// Generates and executes code to solve problems that require computation.
pub struct ProgramOfThought {
    config: ProgramOfThoughtConfig,
    name: String,
}

impl ProgramOfThought {
    /// Create a new Program of Thought module
    pub fn new(config: ProgramOfThoughtConfig) -> Self {
        Self {
            config,
            name: "ProgramOfThought".to_string(),
        }
    }

    /// Create with default configuration
    pub fn default_config() -> Self {
        Self::new(ProgramOfThoughtConfig::default())
    }

    /// Set custom name
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Generate code for the given problem
    async fn generate_code(&self, problem: &str) -> Result<String> {
        // TODO: Use LLM to generate code
        // For now, return a simple template

        match self.config.language {
            CodeLanguage::Python => {
                Ok(format!(r#"# Problem: {}
# Generated code
def solve():
    # TODO: Implement solution
    return "42"

if __name__ == "__main__":
    result = solve()
    print(result)
"#, problem))
            }
            CodeLanguage::JavaScript => {
                Ok(format!(r#"// Problem: {}
function solve() {{
    // TODO: Implement solution
    return "42";
}}

console.log(solve());
"#, problem))
            }
            _ => {
                Err(DspyError::ModuleError(
                    format!("Code generation not implemented for {:?}", self.config.language)
                ))
            }
        }
    }

    /// Safety check for generated code
    fn safety_check(&self, code: &str) -> Result<()> {
        if !self.config.enable_safety_checks {
            return Ok(());
        }

        // Check code length
        if code.len() > self.config.max_code_length {
            return Err(DspyError::ValidationError(
                format!("Code exceeds maximum length: {} > {}",
                    code.len(), self.config.max_code_length)
            ));
        }

        // Check for dangerous operations
        let dangerous_patterns = [
            "import os",
            "import sys",
            "eval(",
            "exec(",
            "__import__",
            "subprocess",
            "rm -rf",
            "delete",
        ];

        for pattern in &dangerous_patterns {
            if code.contains(pattern) {
                return Err(DspyError::ValidationError(
                    format!("Code contains potentially dangerous pattern: {}", pattern)
                ));
            }
        }

        Ok(())
    }

    /// Execute the generated code
    async fn execute_code(&self, code: &str) -> Result<ExecutionResult> {
        self.safety_check(code)?;

        let start_time = std::time::Instant::now();

        // Create temporary file for code
        let temp_dir = std::env::temp_dir();
        let file_name = format!("pot_{}.{}",
            uuid::Uuid::new_v4(),
            self.config.language.extension()
        );
        let file_path = temp_dir.join(&file_name);

        // Write code to file
        std::fs::write(&file_path, code)
            .map_err(|e| DspyError::IoError(e))?;

        // Execute code
        let output = Command::new(self.config.language.command())
            .arg(&file_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| DspyError::ModuleError(format!("Failed to spawn process: {}", e)))?
            .wait_with_output()
            .map_err(|e| DspyError::ModuleError(format!("Failed to wait for process: {}", e)))?;

        let execution_time = start_time.elapsed();

        // Clean up
        let _ = std::fs::remove_file(&file_path);

        Ok(ExecutionResult {
            code: code.to_string(),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            exit_code: output.status.code().unwrap_or(-1),
            execution_time_ms: execution_time.as_millis() as u64,
        })
    }
}

#[async_trait]
impl Module for ProgramOfThought {
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // Extract problem/question
        let problem = inputs.iter()
            .find(|(key, _)| *key == "problem" || *key == "question")
            .map(|(_, value)| *value)
            .ok_or_else(|| DspyError::MissingInput("problem or question".to_string()))?;

        // Generate code
        let code = self.generate_code(problem).await?;

        // Execute code
        let exec_result = self.execute_code(&code).await?;

        // Build output
        let mut output = ModuleOutput::new();
        output.set("problem", problem);
        output.set("code", exec_result.code.clone());
        output.set("stdout", exec_result.stdout.clone());
        output.set("stderr", exec_result.stderr.clone());
        output.set("exit_code", exec_result.exit_code.to_string());
        output.set("execution_time_ms", exec_result.execution_time_ms.to_string());
        output.set("success", exec_result.is_success().to_string());

        // Set answer from stdout or error
        output.set("answer", exec_result.output_or_error().trim());

        Ok(output)
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Builder for ProgramOfThought
pub struct ProgramOfThoughtBuilder {
    config: ProgramOfThoughtConfig,
    name: String,
}

impl ProgramOfThoughtBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            config: ProgramOfThoughtConfig::default(),
            name: "ProgramOfThought".to_string(),
        }
    }

    /// Set programming language
    pub fn language(mut self, language: CodeLanguage) -> Self {
        self.config.language = language;
        self
    }

    /// Set timeout
    pub fn timeout_seconds(mut self, timeout: u64) -> Self {
        self.config.timeout_seconds = timeout;
        self
    }

    /// Enable/disable safety checks
    pub fn enable_safety_checks(mut self, enable: bool) -> Self {
        self.config.enable_safety_checks = enable;
        self
    }

    /// Set maximum code length
    pub fn max_code_length(mut self, max_length: usize) -> Self {
        self.config.max_code_length = max_length;
        self
    }

    /// Set custom name
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Build the ProgramOfThought module
    pub fn build(self) -> ProgramOfThought {
        ProgramOfThought {
            config: self.config,
            name: self.name,
        }
    }
}

impl Default for ProgramOfThoughtBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_language() {
        assert_eq!(CodeLanguage::Python.command(), "python3");
        assert_eq!(CodeLanguage::Python.extension(), "py");
        assert_eq!(CodeLanguage::JavaScript.command(), "node");
    }

    #[test]
    fn test_pot_config() {
        let config = ProgramOfThoughtConfig::default();
        assert_eq!(config.language, CodeLanguage::Python);
        assert_eq!(config.timeout_seconds, 5);
        assert!(config.enable_safety_checks);
    }

    #[tokio::test]
    async fn test_generate_code() {
        let pot = ProgramOfThought::default_config();
        let code = pot.generate_code("Calculate 2 + 2").await.unwrap();
        assert!(code.contains("def solve()"));
    }

    #[test]
    fn test_safety_check() {
        let pot = ProgramOfThought::default_config();

        // Safe code should pass
        let safe_code = "print('Hello')";
        assert!(pot.safety_check(safe_code).is_ok());

        // Dangerous code should fail
        let dangerous_code = "import os; os.system('rm -rf /')";
        assert!(pot.safety_check(dangerous_code).is_err());
    }

    #[tokio::test]
    async fn test_pot_builder() {
        let pot = ProgramOfThoughtBuilder::new()
            .language(CodeLanguage::Python)
            .timeout_seconds(10)
            .name("TestPoT")
            .build();

        assert_eq!(pot.name(), "TestPoT");
        assert_eq!(pot.config.timeout_seconds, 10);
    }

    #[test]
    fn test_execution_result() {
        let result = ExecutionResult {
            code: "print('test')".to_string(),
            stdout: "test\n".to_string(),
            stderr: String::new(),
            exit_code: 0,
            execution_time_ms: 50,
        };

        assert!(result.is_success());
        assert_eq!(result.output_or_error(), "test\n");
    }
}
