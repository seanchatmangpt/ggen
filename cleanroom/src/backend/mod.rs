//! Backend abstraction for running commands in different execution environments

use crate::error::Result;
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::process::Output;

// Module structure for backends
pub mod testcontainer;

pub use testcontainer::TestcontainerBackend;

/// Command to execute with all configuration
#[derive(Debug, Clone)]
pub struct Cmd {
    /// Binary or executable path
    pub bin: String,
    /// Command arguments
    pub args: Vec<String>,
    /// Environment variables
    pub env: Vec<(String, String)>,
    /// Working directory
    pub workdir: Option<String>,
    /// Timeout in milliseconds
    pub timeout_ms: Option<u64>,
}

impl Cmd {
    /// Create a new command
    pub fn new(bin: impl Into<String>) -> Self {
        Self {
            bin: bin.into(),
            args: Vec::new(),
            env: Vec::new(),
            workdir: None,
            timeout_ms: None,
        }
    }

    /// Add arguments
    pub fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        self.args
            .extend(args.into_iter().map(|s| s.as_ref().to_string()));
        self
    }

    /// Add environment variable
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.push((key.into(), value.into()));
        self
    }

    /// Set working directory
    pub fn workdir(mut self, dir: impl Into<String>) -> Self {
        self.workdir = Some(dir.into());
        self
    }

    /// Set timeout
    pub fn timeout_ms(mut self, ms: u64) -> Self {
        self.timeout_ms = Some(ms);
        self
    }
}

/// Result of a command execution
#[derive(Debug, Clone)]
pub struct RunResult {
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Execution duration in milliseconds
    pub duration_ms: u64,
    /// Whether execution was hermetic (no network access)
    pub hermetic: bool,
    /// Whether mounts were deterministic
    pub deterministic_mounts: bool,
    /// Whether clock was normalized
    pub normalized_clock: bool,
    /// Backend used for execution
    pub backend: String,
}

impl RunResult {
    /// Create from process Output
    pub fn from_output(output: Output, duration_ms: u64) -> Self {
        Self {
            exit_code: output.status.code().unwrap_or(-1),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            duration_ms,
            hermetic: false, // Will be set by backend
            deterministic_mounts: false, // Will be set by backend
            normalized_clock: false, // Will be set by backend
            backend: "unknown".to_string(), // Will be set by backend
        }
    }

    /// Create with backend-specific execution metadata
    pub fn with_backend_metadata(
        mut self,
        backend_name: &str,
        hermetic: bool,
        deterministic_mounts: bool,
        normalized_clock: bool,
    ) -> Self {
        self.backend = backend_name.to_string();
        self.hermetic = hermetic;
        self.deterministic_mounts = deterministic_mounts;
        self.normalized_clock = normalized_clock;
        self
    }

    /// Check if command succeeded
    pub fn success(&self) -> bool {
        self.exit_code == 0
    }

    /// Check if execution was hermetic
    pub fn is_hermetic(&self) -> bool {
        self.hermetic
    }

    /// Check if mounts were deterministic
    pub fn has_deterministic_mounts(&self) -> bool {
        self.deterministic_mounts
    }

    /// Check if clock was normalized
    pub fn has_normalized_clock(&self) -> bool {
        self.normalized_clock
    }
}

/// Backend trait for executing commands in isolated environments
pub trait Backend: Send + Sync {
    /// Run a command and return result
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult>;

    /// Get backend name for diagnostics
    fn name(&self) -> &str;

    /// Check if this backend is available for use
    fn is_available(&self) -> bool {
        true
    }

    /// Check if this backend supports hermetic execution
    fn supports_hermetic(&self) -> bool {
        false
    }

    /// Check if this backend supports deterministic execution
    fn supports_deterministic(&self) -> bool {
        false
    }
}

/// Async backend trait for container orchestration
#[async_trait::async_trait]
pub trait AsyncBackend: Send + Sync {
    /// Prepare execution environment
    async fn prepare(&self, cfg: &Config) -> Result<Prepared>;

    /// Run command in prepared environment
    async fn run(&self, p: &Prepared, args: &[&str]) -> Result<RunOutput>;

    /// Teardown prepared environment
    async fn teardown(&self, p: Prepared) -> Result<()>;

    /// Get backend name
    fn name(&self) -> &str;
}

/// Configuration for async backends
#[derive(Debug, Clone)]
pub struct Config {
    /// Working directory
    pub workdir: Option<String>,
    /// Environment variables
    pub env: std::collections::HashMap<String, String>,
    /// Policy constraints
    pub policy: crate::policy::Policy,
}

/// Output from async backend execution
#[derive(Debug, Clone)]
pub struct RunOutput {
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Execution duration in milliseconds
    pub duration_ms: u64,
}

/// Prepared execution environment (RAII guard)
#[derive(Debug)]
pub struct Prepared {
    /// Working directory
    pub workdir: Option<PathBuf>,
    /// Environment variables
    pub env: HashMap<String, String>,
    /// Policy constraints
    pub policy: crate::policy::Policy,
}

/// Auto-backend wrapper for testcontainers
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
        let backend = TestcontainerBackend::new("rust:1-slim")?;
        Ok(Self { inner: backend })
    }

    /// Create backend from name (only supports testcontainers now)
    pub fn from_name(name: &str) -> Result<Self> {
        match name {
            "testcontainers" | "auto" => Self::detect(),
            _ => Err(crate::error::BackendError::Runtime(
                format!("Unknown backend: {}. Only 'testcontainers' and 'auto' are supported", name)
            ).into()),
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
