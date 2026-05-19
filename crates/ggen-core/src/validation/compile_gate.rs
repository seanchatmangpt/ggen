//! Compile gate for validating generated Rust code compiles.
//!
//! This module provides a [`CompileGate`] that runs `cargo check` to verify that
//! generated Rust code compiles successfully. Supports both workspace packages
//! and standalone manifest paths (e.g., generated MCP servers).
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
//! - Principle V: Type-first thinking (strong types for targets, errors)
//! - Principle VII: Result<T,E> error handling (no unwrap in production)
//! - Principle IX: Poka-yoke design (fail-fast on compilation errors)
//!
//! ## Pipeline Integration (μ₄ - Validate Stage)
//!
//! This gate is designed for integration into the μ₄ (validate) stage of the
//! five-stage code generation pipeline (μ₁-μ₅):
//!
//! - μ₁ (load): Load RDF ontology
//! - μ₂ (extract): Extract skill definitions
//! - μ₃ (generate): Generate code (potentially with LLM assistance)
//! - **μ₄ (validate): Quality gate validation ← COMPILE GATE OPERATES HERE**
//! - μ₅ (emit): Write generated files
//!
//! ## Example
//!
//! ```rust
//! // μ₄: Validate generated MCP server before emission
//! use ggen_core::validation::compile_gate::{CompileGate, CompileTarget};
//!
//! let gate = CompileGate::new("/path/to/generated")?;
//! let result = gate.validate(&CompileTarget::ManifestPath(
//!     "/path/to/generated/mcp-server/Cargo.toml".into()
//! ))?;
//!
//! if result.passed {
//!     // μ₅: Emit the generated files
//! } else {
//!     return Err(result.output.into());
//! }
//! ```

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};
use tokio::time::timeout as tokio_timeout;

/// Compilation target: either a workspace package or a standalone manifest path.
#[derive(Debug, Clone)]
pub enum CompileTarget {
    /// A workspace package identified by name (uses `--package <name>`).
    Package(String),
    /// A standalone Cargo.toml manifest path (uses `--manifest-path <path>`).
    ManifestPath(PathBuf),
}

/// Typed error for compilation operations.
#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    /// Root path not found or not a directory.
    #[error("Root path not found or not a directory: {0:?}")]
    RootNotFound(PathBuf),

    /// Manifest file not found at the specified path.
    #[error("Manifest file not found: {0:?}")]
    ManifestNotFound(PathBuf),

    /// Compilation failed for the target.
    #[error("Compilation failed for {target}:\n{output}")]
    CompilationFailed { target: String, output: String },

    /// Failed to execute cargo command.
    #[error("Execution failed: {0}")]
    ExecutionFailed(String),

    /// Operation timed out.
    #[error("Timeout after {0:?}")]
    Timeout(Duration),

    /// Tokio runtime error.
    #[error("Tokio runtime error: {0}")]
    RuntimeError(String),
}

/// Result type for compilation operations.
pub type Result<T> = std::result::Result<T, CompileError>;

/// Result of a compilation check.
#[derive(Debug, Clone)]
pub struct CompileResult {
    /// The target name (package name or manifest path) that was checked.
    pub target_name: String,
    /// Whether compilation succeeded.
    pub passed: bool,
    /// Duration of the cargo check invocation in milliseconds.
    pub duration_ms: u64,
    /// The combined stdout+stderr from cargo check (truncated to last N lines on failure).
    pub output: String,
}

impl CompileResult {
    /// Create a successful compilation result.
    pub fn success(target_name: impl Into<String>, duration_ms: u64) -> Self {
        Self {
            target_name: target_name.into(),
            passed: true,
            duration_ms,
            output: String::new(),
        }
    }

    /// Create a failed compilation result.
    pub fn failure(target_name: impl Into<String>, duration_ms: u64, output: String) -> Self {
        Self {
            target_name: target_name.into(),
            passed: false,
            duration_ms,
            output,
        }
    }
}

impl std::fmt::Display for CompileResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let status = if self.passed { "PASS" } else { "FAIL" };
        writeln!(
            f,
            "[{}] {} ({:.0}ms)",
            status, self.target_name, self.duration_ms as f64
        )?;
        if !self.passed && !self.output.is_empty() {
            writeln!(f, "{}", self.output)?;
        }
        Ok(())
    }
}

/// A quality gate that validates a Rust crate compiles.
///
/// Runs `cargo check` either on workspace packages (`--package <name>`) or
/// standalone manifests (`--manifest-path <path>`). Uses the real cargo
/// binary -- no mocks.
///
/// ## Example
///
/// ```rust
/// use ggen_core::validation::compile_gate::{CompileGate, CompileTarget};
///
/// // Workspace package
/// let gate = CompileGate::new("/path/to/workspace")?;
/// let result = gate.validate(&CompileTarget::Package("ggen-core".to_string()))?;
///
/// // Standalone manifest (e.g., generated MCP server)
/// let result = gate.validate(&CompileTarget::ManifestPath(
///     "/generated/mcp-server/Cargo.toml".into()
/// ))?;
/// ```
pub struct CompileGate {
    /// Path to the root directory (workspace root or generated code directory).
    root: PathBuf,
    /// Timeout for cargo check operations.
    timeout: Duration,
}

impl CompileGate {
    /// Create a new CompileGate targeting the given root directory.
    ///
    /// The root must exist and be a directory. For workspace packages, this is
    /// the workspace root (contains Cargo.toml). For standalone manifests, this
    /// can be any parent directory.
    ///
    /// ## Errors
    ///
    /// Returns `CompileError::RootNotFound` if the path does not exist or is not a directory.
    pub fn new(root: impl Into<PathBuf>) -> Result<Self> {
        let root = root.into();
        if !root.exists() {
            return Err(CompileError::RootNotFound(root.clone()));
        }
        if !root.is_dir() {
            return Err(CompileError::RootNotFound(root));
        }
        Ok(Self {
            root,
            timeout: Duration::from_secs(300),
        })
    }

    /// Set a custom timeout for cargo check operations.
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    /// Return the root path.
    pub fn root(&self) -> &PathBuf {
        &self.root
    }

    /// Return the configured timeout.
    pub fn timeout(&self) -> Duration {
        self.timeout
    }

    /// Validate a compilation target (workspace package or standalone manifest).
    ///
    /// This is the primary validation API. It runs `cargo check` with appropriate
    /// flags based on the target type and enforces a timeout to prevent hangs.
    ///
    /// ## Arguments
    ///
    /// - `target`: Either `CompileTarget::Package(name)` or `CompileTarget::ManifestPath(path)`
    ///
    /// ## Returns
    ///
    /// A `Result<CompileResult, CompileError>` with pass/fail status and diagnostic output.
    ///
    /// ## Errors
    ///
    /// - `CompileError::ManifestNotFound`: Manifest path does not exist (for ManifestPath targets)
    /// - `CompileError::Timeout`: Operation exceeded configured timeout
    /// - `CompileError::ExecutionFailed`: Failed to execute cargo command
    /// - `CompileError::RuntimeError`: Tokio runtime error
    pub fn validate(&self, target: &CompileTarget) -> Result<CompileResult> {
        let start = Instant::now();

        // Build the cargo check command based on target type
        let mut cmd = Command::new("cargo");
        cmd.arg("check");

        let target_name = match target {
            CompileTarget::Package(name) => {
                cmd.arg("--package").arg(name);
                name.clone()
            }
            CompileTarget::ManifestPath(path) => {
                if !path.exists() {
                    return Err(CompileError::ManifestNotFound(path.clone()));
                }
                cmd.arg("--manifest-path").arg(path);
                path.display().to_string()
            }
        };

        cmd.current_dir(&self.root);

        // Run with timeout using tokio
        let output = self.run_with_timeout(cmd, target_name.clone())?;

        let duration_ms = start.elapsed().as_millis() as u64;

        if output.status.success() {
            Ok(CompileResult::success(target_name, duration_ms))
        } else {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            let combined = format!("{}\n{}", stdout, stderr);
            let lines: Vec<&str> = combined.lines().collect();
            let start_idx = lines.len().saturating_sub(50);
            let truncated = lines[start_idx..].join("\n");

            Ok(CompileResult::failure(target_name, duration_ms, truncated))
        }
    }

    /// Run a command with a timeout using tokio.
    ///
    /// This prevents `cargo check` from hanging indefinitely on malformed code
    /// or dependency issues.
    fn run_with_timeout(&self, cmd: Command, _target_name: String) -> Result<std::process::Output> {
        let timeout_duration = self.timeout;

        // Create a new tokio runtime for this operation
        let rt = tokio::runtime::Runtime::new()
            .map_err(|e| CompileError::RuntimeError(e.to_string()))?;

        rt.block_on(async {
            // Convert std::process::Command to tokio::process::Command
            let mut cmd_async = tokio::process::Command::from(cmd);
            let output_future = cmd_async.output();

            // Apply timeout
            match tokio_timeout(timeout_duration, output_future).await {
                Ok(Ok(output)) => Ok(output),
                Ok(Err(e)) => Err(CompileError::ExecutionFailed(e.to_string())),
                Err(_) => Err(CompileError::Timeout(timeout_duration)),
            }
        })
    }

    /// Convenience function: validate a workspace package by name.
    ///
    /// Returns `Ok(())` if compilation succeeds, `Err(CompileError)` otherwise.
    pub fn check_package(&self, package_name: &str) -> Result<()> {
        let result = self.validate(&CompileTarget::Package(package_name.to_string()))?;
        if result.passed {
            Ok(())
        } else {
            Err(CompileError::CompilationFailed {
                target: package_name.to_string(),
                output: result.output,
            })
        }
    }

    /// Convenience function: validate a standalone manifest path.
    ///
    /// Returns `Ok(())` if compilation succeeds, `Err(CompileError)` otherwise.
    pub fn check_manifest(&self, manifest_path: &Path) -> Result<()> {
        let result = self.validate(&CompileTarget::ManifestPath(manifest_path.to_path_buf()))?;
        if result.passed {
            Ok(())
        } else {
            Err(CompileError::CompilationFailed {
                target: manifest_path.display().to_string(),
                output: result.output,
            })
        }
    }

    // ========== Backward Compatibility Layer ==========

    /// Validate that a specific workspace package compiles (legacy API).
    ///
    /// This method is kept for backward compatibility. New code should use
    /// `validate(&CompileTarget::Package(name))` instead.
    ///
    /// ## Arguments
    ///
    /// - `crate_name`: The package name as defined in the crate's Cargo.toml
    ///
    /// ## Returns
    ///
    /// A [`CompileResult`] with pass/fail status, duration, and captured output.
    pub fn validate_crate(&self, crate_name: &str) -> CompileResult {
        // Convert to new API
        match self.validate(&CompileTarget::Package(crate_name.to_string())) {
            Ok(result) => result,
            Err(e) => CompileResult {
                target_name: crate_name.to_string(),
                passed: false,
                duration_ms: 0,
                output: e.to_string(),
            },
        }
    }

    /// Convenience function: validate and return `Ok(())` on success or `Err(output)` on failure (legacy API).
    ///
    /// This is the ergonomic API for use in pipelines where a simple pass/fail is needed.
    /// This method is kept for backward compatibility. New code should use
    /// `check_package(name)` instead.
    ///
    /// Note: This returns `std::result::Result<(), String>` for backward compatibility,
    /// not the typed `Result<(), CompileError>`.
    pub fn check(&self, crate_name: &str) -> std::result::Result<(), String> {
        let result = self.validate_crate(crate_name);
        if result.passed {
            Ok(())
        } else {
            Err(result.output)
        }
    }

    /// Return the workspace root path (legacy API).
    ///
    /// This method is kept for backward compatibility. New code should use `root()` instead.
    #[deprecated(since = "6.1.0", note = "Use `root()` instead")]
    pub fn workspace_root(&self) -> &PathBuf {
        &self.root
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
        let gate = CompileGate::new(workspace_root()).expect("Failed to create CompileGate").with_timeout(Duration::from_secs(300));
        let result = gate.validate_crate("ggen-core");
        assert!(
            result.passed,
            "Expected ggen-core to compile, but got:\n{}",
            result.output
        );
    }

    #[test]
    fn test_compile_gate_check_ok() {
        let gate = CompileGate::new(workspace_root()).expect("Failed to create CompileGate").with_timeout(Duration::from_secs(300));
        let check_result = gate.check("ggen-core");
        assert!(
            check_result.is_ok(),
            "Expected ggen-core to compile, but got: {:?}",
            check_result
        );
    }

    #[test]
    fn test_compile_gate_nonexistent_crate() {
        let gate = CompileGate::new(workspace_root()).expect("Failed to create CompileGate").with_timeout(Duration::from_secs(60));
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
            target_name: "test-crate".to_string(),
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
            target_name: "bad-crate".to_string(),
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
        let gate = CompileGate::new("/tmp").expect("Failed to create CompileGate").with_timeout(Duration::from_secs(10));
        assert_eq!(gate.root(), &PathBuf::from("/tmp"));
    }

    // ========== Phase 1-3 Tests: CompileTarget, CompileError, Timeout ==========

    #[test]
    fn test_compile_target_package() {
        let target = CompileTarget::Package("ggen-core".to_string());
        match target {
            CompileTarget::Package(name) => assert_eq!(name, "ggen-core"),
            _ => panic!("Expected Package variant"),
        }
    }

    #[test]
    fn test_compile_target_manifest_path() {
        let path = PathBuf::from("/tmp/Cargo.toml");
        let target = CompileTarget::ManifestPath(path.clone());
        match target {
            CompileTarget::ManifestPath(p) => assert_eq!(p, path),
            _ => panic!("Expected ManifestPath variant"),
        }
    }

    #[test]
    fn test_compile_gate_validate_package_new_api() {
        let gate = CompileGate::new(workspace_root()).expect("Failed to create CompileGate");
        let result = gate.validate(&CompileTarget::Package("ggen-core".to_string()));
        assert!(
            result.is_ok(),
            "Expected validation to succeed: {:?}",
            result
        );
        let compile_result = result.unwrap();
        assert!(
            compile_result.passed,
            "Expected ggen-core to compile: {}",
            compile_result.output
        );
    }

    #[test]
    fn test_compile_gate_check_package_convenience() {
        let gate = CompileGate::new(workspace_root()).expect("Failed to create CompileGate");
        let result = gate.check_package("ggen-core");
        assert!(
            result.is_ok(),
            "Expected check_package to succeed: {:?}",
            result
        );
    }

    #[test]
    fn test_compile_gate_check_manifest_nonexistent() {
        let gate = CompileGate::new(workspace_root()).expect("Failed to create CompileGate");
        let result = gate.check_manifest(Path::new("/nonexistent/Cargo.toml"));
        assert!(
            result.is_err(),
            "Expected check_manifest to fail for nonexistent path"
        );
        match result {
            Err(CompileError::ManifestNotFound(path)) => {
                assert_eq!(path, PathBuf::from("/nonexistent/Cargo.toml"));
            }
            Err(e) => panic!("Expected ManifestNotFound error, got: {}", e),
            Ok(_) => panic!("Expected error, got Ok"),
        }
    }

    #[test]
    fn test_compile_error_root_not_found() {
        let error = CompileError::RootNotFound(PathBuf::from("/nonexistent"));
        assert!(error.to_string().contains("Root path not found"));
    }

    #[test]
    fn test_compile_error_manifest_not_found() {
        let error = CompileError::ManifestNotFound(PathBuf::from("/tmp/Cargo.toml"));
        assert!(error.to_string().contains("Manifest file not found"));
    }

    #[test]
    fn test_compile_error_compilation_failed() {
        let error = CompileError::CompilationFailed {
            target: "test-package".to_string(),
            output: "error[E0425]: unresolved reference".to_string(),
        };
        let error_str = error.to_string();
        assert!(error_str.contains("Compilation failed"));
        assert!(error_str.contains("test-package"));
        assert!(error_str.contains("unresolved reference"));
    }

    #[test]
    fn test_compile_error_timeout() {
        let error = CompileError::Timeout(Duration::from_secs(300));
        assert!(error.to_string().contains("Timeout after"));
    }

    #[test]
    fn test_compile_gate_timeout_configuration() {
        let gate = CompileGate::new("/tmp").expect("Failed to create CompileGate");
        assert_eq!(gate.timeout(), Duration::from_secs(300));

        let gate_with_custom = gate.with_timeout(Duration::from_secs(120));
        assert_eq!(gate_with_custom.timeout(), Duration::from_secs(120));
    }

    #[test]
    fn test_compile_gate_new_nonexistent_directory() {
        let result = CompileGate::new("/nonexistent-directory-xyz123");
        assert!(
            result.is_err(),
            "Expected error for nonexistent directory"
        );
        match result {
            Err(CompileError::RootNotFound(_)) => {}
            Err(e) => panic!("Expected RootNotFound error, got: {}", e),
            Ok(_) => panic!("Expected error, got Ok"),
        }
    }

    #[test]
    fn test_compile_gate_new_file_not_directory() {
        // Create a temporary file (not a directory)
        let temp_file = workspace_root().join("Cargo.toml");
        if temp_file.exists() {
            let result = CompileGate::new(&temp_file);
            assert!(
                result.is_err(),
                "Expected error when path is a file, not a directory"
            );
            match result {
                Err(CompileError::RootNotFound(_)) => {}
                Err(e) => panic!("Expected RootNotFound error, got: {}", e),
                Ok(_) => panic!("Expected error, got Ok"),
            }
        }
    }
}
