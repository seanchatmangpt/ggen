//! Backend implementations for cleanroom testing
//!
//! This module provides backend implementations following core team best practices.

use crate::error::Result;
use crate::policy::Policy;
use std::collections::HashMap;
use std::path::PathBuf;

// Module structure for backends
pub mod testcontainer;

pub use testcontainer::TestcontainerBackend;

/// Command to execute with all configuration
#[derive(Debug, Clone)]
pub struct Cmd {
    /// Binary or executable path
    pub bin: String,
    /// Arguments to pass to the command
    pub args: Vec<String>,
    /// Working directory
    pub workdir: Option<PathBuf>,
    /// Environment variables
    pub env: HashMap<String, String>,
    /// Policy constraints
    pub policy: Policy,
}

/// Result of a command execution
#[derive(Debug, Clone)]
pub struct RunResult {
    /// Exit code of the command
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Duration of execution in milliseconds
    pub duration_ms: u64,
    /// Individual step results
    pub steps: Vec<crate::scenario::StepResult>,
    /// Environment variables that were redacted in forensics
    pub redacted_env: Vec<String>,
    /// Backend used for execution
    pub backend: String,
    /// Whether execution was concurrent
    pub concurrent: bool,
    /// Step execution order (for deterministic ordering)
    pub step_order: Vec<String>,
}

impl RunResult {
    /// Create a new run result
    pub fn new(exit_code: i32, stdout: String, stderr: String, duration_ms: u64) -> Self {
        Self {
            exit_code,
            stdout,
            stderr,
            duration_ms,
            steps: Vec::new(),
            redacted_env: Vec::new(),
            backend: "unknown".to_string(),
            concurrent: false,
            step_order: Vec::new(),
        }
    }

    /// Check if the command succeeded
    pub fn success(&self) -> bool {
        self.exit_code == 0
    }

    /// Check if the command failed
    pub fn failed(&self) -> bool {
        self.exit_code != 0
    }
}

impl Cmd {
    /// Create a new command
    pub fn new(bin: impl Into<String>) -> Self {
        Self {
            bin: bin.into(),
            args: Vec::new(),
            workdir: None,
            env: HashMap::new(),
            policy: Policy::default(),
        }
    }

    /// Add an argument
    pub fn arg(mut self, arg: impl Into<String>) -> Self {
        self.args.push(arg.into());
        self
    }

    /// Add multiple arguments
    pub fn args(mut self, args: &[&str]) -> Self {
        for arg in args {
            self.args.push(arg.to_string());
        }
        self
    }

    /// Set working directory
    pub fn workdir(mut self, workdir: PathBuf) -> Self {
        self.workdir = Some(workdir);
        self
    }

    /// Set environment variable
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    /// Set policy
    pub fn policy(mut self, policy: Policy) -> Self {
        self.policy = policy;
        self
    }
}

/// Trait for backend execution environments
pub trait Backend: Send + Sync + std::fmt::Debug {
    /// Run a command in the backend
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult>;
    /// Get the name of the backend
    fn name(&self) -> &str;
    /// Check if the backend is available
    fn is_available(&self) -> bool;
    /// Check if the backend supports hermetic execution
    fn supports_hermetic(&self) -> bool;
    /// Check if the backend supports deterministic execution
    fn supports_deterministic(&self) -> bool;
}

/// Auto-backend wrapper for testcontainers
#[derive(Debug)]
pub struct AutoBackend {
    /// The underlying testcontainers backend
    inner: TestcontainerBackend,
}

impl AutoBackend {
    /// Create a new AutoBackend with testcontainers
    pub fn new(backend: TestcontainerBackend) -> Self {
        Self { inner: backend }
    }

    /// Create testcontainers backend with default image
    pub fn detect() -> Result<Self> {
        let backend = TestcontainerBackend::new("alpine:latest")?;
        Ok(Self { inner: backend })
    }

    /// Create backend from name (only supports testcontainers now)
    pub fn from_name(name: &str) -> Result<Self> {
        match name {
            "testcontainers" | "auto" => Self::detect(),
            _ => Err(crate::error::CleanroomError::new(
                crate::error::ErrorKind::ConfigurationError,
                format!("Unknown backend: {}. Only 'testcontainers' and 'auto' are supported", name)
            )),
        }
    }

    /// Get the resolved backend name
    pub fn resolved_backend(&self) -> String {
        self.inner.name().to_string()
    }

    /// Check if testcontainers backend is available
    pub fn is_backend_available(name: &str) -> bool {
        match name {
            "testcontainers" | "auto" => TestcontainerBackend::is_available(),
            _ => false,
        }
    }
}

impl Backend for AutoBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        self.inner.run_cmd(cmd)
    }

    fn name(&self) -> &str {
        self.inner.name()
    }

    fn is_available(&self) -> bool {
        self.inner.is_available()
    }

    fn supports_hermetic(&self) -> bool {
        self.inner.supports_hermetic()
    }

    fn supports_deterministic(&self) -> bool {
        self.inner.supports_deterministic()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_result_creation() {
        let result = RunResult::new(
            0,
            "output".to_string(),
            "error".to_string(),
            1000,
        );
        
        assert!(result.success());
        assert!(!result.failed());
        assert_eq!(result.exit_code, 0);
        assert_eq!(result.stdout, "output");
        assert_eq!(result.stderr, "error");
        assert_eq!(result.duration_ms, 1000);
    }

    #[test]
    fn test_run_result_failure() {
        let result = RunResult::new(
            1,
            "output".to_string(),
            "error".to_string(),
            500,
        );
        
        assert!(!result.success());
        assert!(result.failed());
        assert_eq!(result.exit_code, 1);
    }

    #[test]
    fn test_cmd_creation() {
        let cmd = Cmd::new("echo");
        assert_eq!(cmd.bin, "echo");
        assert!(cmd.args.is_empty());
        assert!(cmd.workdir.is_none());
        assert!(cmd.env.is_empty());
    }

    #[test]
    fn test_cmd_with_args() {
        let cmd = Cmd::new("echo")
            .arg("hello")
            .arg("world");
        
        assert_eq!(cmd.args.len(), 2);
        assert_eq!(cmd.args[0], "hello");
        assert_eq!(cmd.args[1], "world");
    }

    #[test]
    fn test_cmd_with_workdir() {
        let cmd = Cmd::new("ls").workdir(PathBuf::from("/tmp"));
        assert_eq!(cmd.workdir, Some(PathBuf::from("/tmp")));
    }

    #[test]
    fn test_cmd_with_env() {
        let cmd = Cmd::new("env").env("TEST", "value");
        assert_eq!(cmd.env.get("TEST"), Some(&"value".to_string()));
    }
}