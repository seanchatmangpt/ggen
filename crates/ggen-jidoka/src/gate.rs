//! Quality gates for Jidoka system
//!
//! Each gate performs a specific quality check and returns an andon signal.

use crate::{AndonSignal, JidokaError, Result, Signal};
use async_trait::async_trait;
use regex::Regex;
use tokio::process::Command as AsyncCommand;

/// Gate trait for quality checks
#[async_trait]
pub trait Gate: Signal {
    /// Execute the gate check
    async fn execute(&self) -> Result<AndonSignal>;
}

/// Compiler gate - checks for compilation errors
#[derive(Debug, Clone)]
pub struct CompilerGate {
    /// Working directory for compilation
    pub working_dir: String,
}

impl CompilerGate {
    /// Create a new compiler gate
    #[must_use]
    pub fn new(working_dir: impl Into<String>) -> Self {
        Self {
            working_dir: working_dir.into(),
        }
    }

    /// Parse compiler output for errors
    fn parse_output(output: &str) -> AndonSignal {
        let error_pattern = Regex::new(r"error(\[E\d+\])?:").unwrap_or_else(|_| {
            // This should never fail with a literal regex
            panic!("Invalid regex pattern")
        });
        let warning_pattern = Regex::new(r"warning:").unwrap_or_else(|_| {
            panic!("Invalid regex pattern")
        });

        if error_pattern.is_match(output) {
            AndonSignal::Red
        } else if warning_pattern.is_match(output) {
            AndonSignal::Yellow
        } else {
            AndonSignal::Green
        }
    }
}

#[async_trait]
impl Signal for CompilerGate {
    async fn check(&self) -> Result<AndonSignal> {
        self.execute().await
    }

    fn name(&self) -> &str {
        "Compiler Gate"
    }

    fn description(&self) -> &str {
        "Checks for compilation errors using cargo check"
    }
}

#[async_trait]
impl Gate for CompilerGate {
    async fn execute(&self) -> Result<AndonSignal> {
        let output = AsyncCommand::new("cargo")
            .arg("check")
            .arg("--message-format=short")
            .current_dir(&self.working_dir)
            .output()
            .await
            .map_err(|e| JidokaError::CommandFailed(e.to_string()))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let combined = format!("{stdout}{stderr}");

        Ok(Self::parse_output(&combined))
    }
}

/// Test gate - checks for test failures
#[derive(Debug, Clone)]
pub struct TestGate {
    /// Working directory for tests
    pub working_dir: String,
}

impl TestGate {
    /// Create a new test gate
    #[must_use]
    pub fn new(working_dir: impl Into<String>) -> Self {
        Self {
            working_dir: working_dir.into(),
        }
    }

    /// Parse test output for failures
    fn parse_output(output: &str) -> AndonSignal {
        let failure_pattern = Regex::new(r"test .* \.\.\. FAILED").unwrap_or_else(|_| {
            panic!("Invalid regex pattern")
        });
        let error_pattern =
            Regex::new(r"error:").unwrap_or_else(|_| panic!("Invalid regex pattern"));

        if failure_pattern.is_match(output) || error_pattern.is_match(output) {
            AndonSignal::Red
        } else if output.contains("warning") {
            AndonSignal::Yellow
        } else {
            AndonSignal::Green
        }
    }
}

#[async_trait]
impl Signal for TestGate {
    async fn check(&self) -> Result<AndonSignal> {
        self.execute().await
    }

    fn name(&self) -> &str {
        "Test Gate"
    }

    fn description(&self) -> &str {
        "Checks for test failures using cargo test"
    }
}

#[async_trait]
impl Gate for TestGate {
    async fn execute(&self) -> Result<AndonSignal> {
        let output = AsyncCommand::new("cargo")
            .arg("test")
            .arg("--")
            .arg("--nocapture")
            .current_dir(&self.working_dir)
            .output()
            .await
            .map_err(|e| JidokaError::CommandFailed(e.to_string()))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let combined = format!("{stdout}{stderr}");

        Ok(Self::parse_output(&combined))
    }
}

/// Lint gate - checks for clippy warnings
#[derive(Debug, Clone)]
pub struct LintGate {
    /// Working directory for linting
    pub working_dir: String,
}

impl LintGate {
    /// Create a new lint gate
    #[must_use]
    pub fn new(working_dir: impl Into<String>) -> Self {
        Self {
            working_dir: working_dir.into(),
        }
    }

    /// Parse clippy output
    fn parse_output(output: &str) -> AndonSignal {
        let error_pattern = Regex::new(r"error:").unwrap_or_else(|_| {
            panic!("Invalid regex pattern")
        });
        let warning_pattern = Regex::new(r"warning:").unwrap_or_else(|_| {
            panic!("Invalid regex pattern")
        });

        if error_pattern.is_match(output) {
            AndonSignal::Red
        } else if warning_pattern.is_match(output) {
            AndonSignal::Yellow
        } else {
            AndonSignal::Green
        }
    }
}

#[async_trait]
impl Signal for LintGate {
    async fn check(&self) -> Result<AndonSignal> {
        self.execute().await
    }

    fn name(&self) -> &str {
        "Lint Gate"
    }

    fn description(&self) -> &str {
        "Checks for clippy warnings using cargo clippy"
    }
}

#[async_trait]
impl Gate for LintGate {
    async fn execute(&self) -> Result<AndonSignal> {
        let output = AsyncCommand::new("cargo")
            .arg("clippy")
            .arg("--")
            .arg("-D")
            .arg("warnings")
            .current_dir(&self.working_dir)
            .output()
            .await
            .map_err(|e| JidokaError::CommandFailed(e.to_string()))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let combined = format!("{stdout}{stderr}");

        Ok(Self::parse_output(&combined))
    }
}

/// SHACL gate - validates RDF specifications
#[derive(Debug, Clone)]
pub struct SHACLGate {
    /// Path to the TTL file to validate
    pub ttl_path: String,
}

impl SHACLGate {
    /// Create a new SHACL gate
    #[must_use]
    pub fn new(ttl_path: impl Into<String>) -> Self {
        Self {
            ttl_path: ttl_path.into(),
        }
    }

    /// Parse validation output
    fn parse_output(output: &str, exit_code: i32) -> AndonSignal {
        if exit_code != 0 {
            AndonSignal::Red
        } else if output.contains("warning") || output.contains("WARN") {
            AndonSignal::Yellow
        } else {
            AndonSignal::Green
        }
    }
}

#[async_trait]
impl Signal for SHACLGate {
    async fn check(&self) -> Result<AndonSignal> {
        self.execute().await
    }

    fn name(&self) -> &str {
        "SHACL Gate"
    }

    fn description(&self) -> &str {
        "Validates RDF specifications using SHACL"
    }
}

#[async_trait]
impl Gate for SHACLGate {
    async fn execute(&self) -> Result<AndonSignal> {
        let output = AsyncCommand::new("ggen")
            .arg("validate")
            .arg(&self.ttl_path)
            .output()
            .await
            .map_err(|e| JidokaError::CommandFailed(e.to_string()))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let combined = format!("{stdout}{stderr}");
        let exit_code = output.status.code().unwrap_or(-1);

        Ok(Self::parse_output(&combined, exit_code))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_gate_parse_output_green() {
        // Arrange
        let output = "   Compiling ggen-jidoka v0.1.0\n    Finished dev [unoptimized + debuginfo]";

        // Act
        let signal = CompilerGate::parse_output(output);

        // Assert
        assert_eq!(signal, AndonSignal::Green);
    }

    #[test]
    fn test_compiler_gate_parse_output_yellow() {
        // Arrange
        let output = "warning: unused variable `x`\n --> src/main.rs:5:9";

        // Act
        let signal = CompilerGate::parse_output(output);

        // Assert
        assert_eq!(signal, AndonSignal::Yellow);
    }

    #[test]
    fn test_compiler_gate_parse_output_red() {
        // Arrange
        let output = "error[E0425]: cannot find value `y` in this scope\n --> src/main.rs:10:5";

        // Act
        let signal = CompilerGate::parse_output(output);

        // Assert
        assert_eq!(signal, AndonSignal::Red);
    }

    #[test]
    fn test_test_gate_parse_output_green() {
        // Arrange
        let output = "test result: ok. 15 passed; 0 failed; 0 ignored; 0 measured";

        // Act
        let signal = TestGate::parse_output(output);

        // Assert
        assert_eq!(signal, AndonSignal::Green);
    }

    #[test]
    fn test_test_gate_parse_output_red() {
        // Arrange
        let output = "test should_validate ... FAILED\ntest result: FAILED. 14 passed; 1 failed";

        // Act
        let signal = TestGate::parse_output(output);

        // Assert
        assert_eq!(signal, AndonSignal::Red);
    }

    #[test]
    fn test_lint_gate_parse_output_green() {
        // Arrange
        let output = "    Checking ggen-jidoka v0.1.0\n    Finished dev [unoptimized + debuginfo]";

        // Act
        let signal = LintGate::parse_output(output);

        // Assert
        assert_eq!(signal, AndonSignal::Green);
    }

    #[test]
    fn test_lint_gate_parse_output_yellow() {
        // Arrange
        let output = "warning: this argument is passed by value\n --> src/lib.rs:42:18";

        // Act
        let signal = LintGate::parse_output(output);

        // Assert
        assert_eq!(signal, AndonSignal::Yellow);
    }

    #[test]
    fn test_shacl_gate_parse_output_green() {
        // Arrange
        let output = "Validation successful";
        let exit_code = 0;

        // Act
        let signal = SHACLGate::parse_output(output, exit_code);

        // Assert
        assert_eq!(signal, AndonSignal::Green);
    }

    #[test]
    fn test_shacl_gate_parse_output_red() {
        // Arrange
        let output = "Validation failed: missing required property";
        let exit_code = 1;

        // Act
        let signal = SHACLGate::parse_output(output, exit_code);

        // Assert
        assert_eq!(signal, AndonSignal::Red);
    }

    #[test]
    fn test_compiler_gate_new() {
        // Arrange & Act
        let gate = CompilerGate::new("/home/user/ggen");

        // Assert
        assert_eq!(gate.working_dir, "/home/user/ggen");
        assert_eq!(gate.name(), "Compiler Gate");
    }

    #[test]
    fn test_test_gate_new() {
        // Arrange & Act
        let gate = TestGate::new("/home/user/ggen");

        // Assert
        assert_eq!(gate.working_dir, "/home/user/ggen");
        assert_eq!(gate.name(), "Test Gate");
    }

    #[test]
    fn test_lint_gate_new() {
        // Arrange & Act
        let gate = LintGate::new("/home/user/ggen");

        // Assert
        assert_eq!(gate.working_dir, "/home/user/ggen");
        assert_eq!(gate.name(), "Lint Gate");
    }

    #[test]
    fn test_shacl_gate_new() {
        // Arrange & Act
        let gate = SHACLGate::new("/home/user/ggen/spec.ttl");

        // Assert
        assert_eq!(gate.ttl_path, "/home/user/ggen/spec.ttl");
        assert_eq!(gate.name(), "SHACL Gate");
    }
}
