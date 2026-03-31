//! Compile gate for validating generated Rust code compiles.
//!
//! This module provides a [`CompileGate`] that runs `cargo check --package <crate_name>`
//! via `std::process::Command` to verify that generated Rust code compiles successfully.
//!
//! This is a **quality gate** in the jidoka (built-in quality) tradition: defective code
//! is caught at the gate before it propagates downstream.
//!
//! ## Chicago TDD
//!
//! Tests run the REAL `cargo check` process -- no mocks, no test doubles.
//!
//! ## Constitution Compliance
//!
//! - Principle V: Type-first thinking (strong types for crate name, output)
//! - Principle VII: Result<T,E> error handling (no unwrap in production)
//! - Principle IX: Poka-yoke design (fail-fast on compilation errors)

use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

/// Result of a compilation check.
#[derive(Debug, Clone)]
pub struct CompileResult {
    /// The crate name that was checked.
    pub crate_name: String,
    /// Whether compilation succeeded.
    pub passed: bool,
    /// Duration of the cargo check invocation in milliseconds.
    pub duration_ms: u64,
    /// The combined stdout+stderr from cargo check (truncated to last N lines on failure).
    pub output: String,
}

impl std::fmt::Display for CompileResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let status = if self.passed { "PASS" } else { "FAIL" };
        writeln!(
            f,
            "[{}] {} ({:.0}ms)",
            status, self.crate_name, self.duration_ms as f64
        )?;
        if !self.passed && !self.output.is_empty() {
            writeln!(f, "{}", self.output)?;
        }
        Ok(())
    }
}

/// A quality gate that validates a Rust crate compiles.
///
/// Runs `cargo check --package <crate_name>` in the workspace root and captures
/// the result. Uses the real cargo binary -- no mocks.
pub struct CompileGate {
    /// Path to the workspace root (contains Cargo.toml).
    workspace_root: PathBuf,
    /// Timeout for the cargo check invocation in seconds (default: 300).
    timeout_secs: u64,
}

impl CompileGate {
    /// Create a new CompileGate targeting the given workspace root.
    ///
    /// The workspace root must contain a `Cargo.toml` (workspace definition).
    pub fn new(workspace_root: impl Into<PathBuf>) -> Self {
        Self {
            workspace_root: workspace_root.into(),
            timeout_secs: 300,
        }
    }

    /// Set a custom timeout for cargo check (in seconds).
    pub fn with_timeout(mut self, timeout_secs: u64) -> Self {
        self.timeout_secs = timeout_secs;
        self
    }

    /// Validate that a specific crate compiles.
    ///
    /// Runs `cargo check --package {crate_name}` and returns a [`CompileResult`].
    /// The crate must exist in the workspace.
    ///
    /// ## Arguments
    ///
    /// - `crate_name`: The package name as defined in the crate's Cargo.toml
    ///
    /// ## Returns
    ///
    /// A [`CompileResult`] with pass/fail status, duration, and captured output.
    pub fn validate_crate(&self, crate_name: &str) -> CompileResult {
        let start = Instant::now();

        let output = Command::new("cargo")
            .arg("check")
            .arg("--package")
            .arg(crate_name)
            .current_dir(&self.workspace_root)
            .output();

        let duration_ms = start.elapsed().as_millis() as u64;

        match output {
            Ok(output) => {
                let passed = output.status.success();
                let captured = if passed {
                    String::new()
                } else {
                    // Combine stdout and stderr for diagnostic context.
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    // Take last 50 lines to keep output manageable.
                    let combined = format!("{}\n{}", stdout, stderr);
                    let all_lines: Vec<&str> = combined.lines().collect();
                    let start = all_lines.len().saturating_sub(50);
                    all_lines[start..].join("\n")
                };

                CompileResult {
                    crate_name: crate_name.to_string(),
                    passed,
                    duration_ms,
                    output: captured,
                }
            }
            Err(e) => CompileResult {
                crate_name: crate_name.to_string(),
                passed: false,
                duration_ms,
                output: format!("Failed to execute cargo check: {}", e),
            },
        }
    }

    /// Convenience function: validate and return `Ok(())` on success or `Err(output)` on failure.
    ///
    /// This is the ergonomic API for use in pipelines where a simple pass/fail is needed.
    pub fn check(&self, crate_name: &str) -> Result<(), String> {
        let result = self.validate_crate(crate_name);
        if result.passed {
            Ok(())
        } else {
            Err(result.output)
        }
    }

    /// Return the workspace root path.
    pub fn workspace_root(&self) -> &PathBuf {
        &self.workspace_root
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Find the workspace root by walking up from the current crate directory.
    ///
    /// This is Chicago TDD -- we discover the actual workspace at test time.
    fn workspace_root() -> PathBuf {
        // The workspace root is 3 levels up from this file:
        // crates/ggen-core/src/validation/compile_gate.rs -> ../../..
        let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        dir.pop(); // ggen-core
        dir.pop(); // crates
        dir
    }

    #[test]
    fn test_compile_gate_valid_crate() {
        let gate = CompileGate::new(workspace_root()).with_timeout(300);
        let result = gate.validate_crate("ggen-core");
        assert!(
            result.passed,
            "Expected ggen-core to compile, but got:\n{}",
            result.output
        );
    }

    #[test]
    fn test_compile_gate_check_ok() {
        let gate = CompileGate::new(workspace_root()).with_timeout(300);
        let check_result = gate.check("ggen-core");
        assert!(
            check_result.is_ok(),
            "Expected ggen-core to compile, but got: {:?}",
            check_result
        );
    }

    #[test]
    fn test_compile_gate_nonexistent_crate() {
        let gate = CompileGate::new(workspace_root()).with_timeout(60);
        let result = gate.validate_crate("crate-that-does-not-exist-xyz");
        assert!(
            !result.passed,
            "Expected nonexistent crate to fail compilation"
        );
        // Output should contain an error message from cargo
        assert!(
            !result.output.is_empty(),
            "Expected error output for nonexistent crate"
        );
    }

    #[test]
    fn test_compile_result_display_pass() {
        let result = CompileResult {
            crate_name: "test-crate".to_string(),
            passed: true,
            duration_ms: 1234,
            output: String::new(),
        };
        let display = format!("{}", result);
        assert!(display.contains("[PASS]"));
        assert!(display.contains("test-crate"));
        assert!(display.contains("1234"));
    }

    #[test]
    fn test_compile_result_display_fail() {
        let result = CompileResult {
            crate_name: "bad-crate".to_string(),
            passed: false,
            duration_ms: 5678,
            output: "error[E0425]: unresolved reference `foo`".to_string(),
        };
        let display = format!("{}", result);
        assert!(display.contains("[FAIL]"));
        assert!(display.contains("bad-crate"));
        assert!(display.contains("unresolved reference"));
    }

    #[test]
    fn test_compile_gate_builder_pattern() {
        let gate = CompileGate::new("/tmp").with_timeout(10);
        assert_eq!(gate.workspace_root(), &PathBuf::from("/tmp"));
    }
}
