//! Backend abstraction for running commands in different execution environments

use crate::error::Result;
use std::fmt::Debug;
use std::process::Output;

// Module structure for backends
pub mod docker;
pub mod local;
pub mod podman;
pub use docker::DockerBackend;
pub use local::LocalBackend;
pub use podman::PodmanBackend;

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
}

impl RunResult {
    /// Create from process Output
    pub fn from_output(output: Output, duration_ms: u64) -> Self {
        Self {
            exit_code: output.status.code().unwrap_or(-1),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            duration_ms,
        }
    }

    /// Check if command succeeded
    pub fn success(&self) -> bool {
        self.exit_code == 0
    }
}

/// Backend trait for executing commands
pub trait Backend: Send + Sync {
    /// Run a command and return result (synchronous version)
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult>;

    /// Get backend name for diagnostics
    fn name(&self) -> &str;

    /// Check if this backend is available for use
    fn is_available(&self) -> bool {
        true
    }
}

/// Async backend trait for container orchestration
#[async_trait::async_trait]
pub trait AsyncBackend: Send + Sync {
    /// Prepare execution environment
    async fn prepare(&self, cfg: &crate::runtime::runner::Config) -> Result<Prepared>;

    /// Run command in prepared environment
    async fn run(&self, p: &Prepared, args: &[&str]) -> Result<crate::runtime::runner::RunOutput>;

    /// Teardown prepared environment
    async fn teardown(&self, p: Prepared) -> Result<()>;

    /// Get backend name
    fn name(&self) -> &str;
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

/// Auto-detect and select the best available backend
pub struct AutoBackend {
    /// The underlying backend implementation
    pub inner: Box<dyn Backend>,
}

impl AutoBackend {
    /// Create a new AutoBackend with a specific backend
    pub fn new(backend: Box<dyn Backend>) -> Self {
        Self { inner: backend }
    }

    /// Detect the best available backend
    pub fn detect() -> Result<Self> {
        // Try Podman first (daemonless, rootless by default)
        if PodmanBackend::new("rust:1-slim").is_available() {
            return Ok(Self {
                inner: Box::new(PodmanBackend::new("rust:1-slim")),
            });
        }

        // Try Docker second (most isolated)
        if DockerBackend::new("rust:1-slim").is_available() {
            return Ok(Self {
                inner: Box::new(DockerBackend::new("rust:1-slim")),
            });
        }

        // Fall back to local backend
        Ok(Self {
            inner: Box::new(LocalBackend::new()),
        })
    }
}

impl Backend for AutoBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        self.inner.run_cmd(cmd)
    }

    fn name(&self) -> &str {
        self.inner.name()
    }
}
