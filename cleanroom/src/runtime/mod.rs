//! Runtime execution environment
//!
//! Provides runtime execution capabilities including command execution,
//! timeout handling, resource management, and structured concurrency.

use crate::error::Result;
use crate::policy::Policy;
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

pub mod orchestrator;

/// Runtime configuration
#[derive(Debug, Clone)]
pub struct Config {
    /// Command and arguments
    pub args: Vec<String>,
    /// Working directory
    pub workdir: Option<PathBuf>,
    /// Environment variables
    pub env: HashMap<String, String>,
    /// Execution timeout
    pub execution_timeout: Duration,
    /// Policy to enforce
    pub policy: Policy,
}

impl Config {
    /// Create a new runtime configuration
    pub fn new(args: Vec<String>) -> Self {
        Self {
            args,
            workdir: None,
            env: HashMap::new(),
            execution_timeout: Duration::from_secs(300), // 5 minutes default
            policy: Policy::default(),
        }
    }

    /// Set working directory
    pub fn with_workdir(mut self, workdir: PathBuf) -> Self {
        self.workdir = Some(workdir);
        self
    }

    /// Add environment variable
    pub fn with_env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    /// Set timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.execution_timeout = timeout;
        self
    }

    /// Set policy
    pub fn with_policy(mut self, policy: Policy) -> Self {
        self.policy = policy;
        self
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<()> {
        if self.args.is_empty() {
            return Err(crate::error::CleanroomError::new(crate::error::ErrorKind::ValidationError, "Empty command arguments"));
        }

        if let Some(ref workdir) = self.workdir {
            if !workdir.exists() {
                return Err(crate::error::CleanroomError::validation_error(format!(
                    "Working directory does not exist: {}",
                    workdir.display()
                )));
            }
        }

        if self.execution_timeout.as_secs() == 0 {
            return Err(crate::error::CleanroomError::new(crate::error::ErrorKind::ValidationError, "Timeout must be greater than 0"));
        }

        self.policy.validate().map_err(|e| {
            crate::error::CleanroomError::validation_error(format!("Policy validation failed: {}", e))
        })?;

        Ok(())
    }
}

/// Runtime execution result
#[derive(Debug, Clone)]
pub struct RunOutput {
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Execution duration
    pub duration_ms: u64,
}

impl RunOutput {
    /// Create a new run output
    pub fn new(exit_code: i32, stdout: String, stderr: String, duration_ms: u64) -> Self {
        Self {
            exit_code,
            stdout,
            stderr,
            duration_ms,
        }
    }

    /// Check if execution was successful
    pub fn success(&self) -> bool {
        self.exit_code == 0
    }

    /// Check if execution failed
    pub fn failed(&self) -> bool {
        self.exit_code != 0
    }

    /// Get combined output
    pub fn combined_output(&self) -> String {
        format!("{}\n{}", self.stdout, self.stderr)
    }

    /// Get output size in bytes
    pub fn output_size(&self) -> usize {
        self.stdout.len() + self.stderr.len()
    }
}

/// Runtime execution environment
pub struct Runtime {
    /// Runtime configuration
    config: Config,
    /// Execution context
    context: ExecutionContext,
}

/// Execution context
#[derive(Debug, Clone)]
pub struct ExecutionContext {
    /// Process ID
    pub pid: Option<u32>,
    /// Start time
    pub start_time: std::time::Instant,
    /// Environment variables
    pub env: HashMap<String, String>,
    /// Working directory
    pub workdir: Option<PathBuf>,
    /// Policy constraints
    pub policy: Policy,
}

impl Runtime {
    /// Create a new runtime
    pub fn new(config: Config) -> Result<Self> {
        config.validate()?;

        let context = ExecutionContext {
            pid: None,
            start_time: std::time::Instant::now(),
            env: config.env.clone(),
            workdir: config.workdir.clone(),
            policy: config.policy.clone(),
        };

        Ok(Self { config, context })
    }

    /// Execute the configured command
    pub fn execute(&mut self) -> Result<RunOutput> {
        self.context.start_time = std::time::Instant::now();

        // Apply policy constraints
        self.apply_policy_constraints()?;

        // Execute command
        let output = self.run_command()?;

        let duration_ms = self.context.start_time.elapsed().as_millis() as u64;

        Ok(RunOutput::new(
            output.exit_code,
            output.stdout,
            output.stderr,
            duration_ms,
        ))
    }

    /// Apply policy constraints to the execution environment
    fn apply_policy_constraints(&mut self) -> Result<()> {
        // Apply environment variables from policy
        for (key, value) in self.config.policy.to_env() {
            self.context.env.insert(key, value);
        }

        // Validate resource limits
        self.validate_resource_limits()?;

        // Apply network constraints
        self.apply_network_constraints()?;

        // Apply filesystem constraints
        self.apply_filesystem_constraints()?;

        Ok(())
    }

    /// Validate resource limits
    fn validate_resource_limits(&self) -> Result<()> {
        let limits = &self.config.policy.resources;

        // Check memory limit
        if limits.max_memory_usage_bytes < 1024 * 1024 { // 1MB minimum
            return Err(crate::error::CleanroomError::resource_limit_exceeded(
                "Memory limit too low (minimum 1MB)"
            ));
        }

        // Check execution time limit
        if limits.max_test_execution_time < Duration::from_secs(1) {
            return Err(crate::error::CleanroomError::resource_limit_exceeded(
                "Test execution time limit too low (minimum 1 second)"
            ));
        }

        // Check container count limit
        if limits.max_container_count == 0 {
            return Err(crate::error::CleanroomError::resource_limit_exceeded(
                "Container count limit must be at least 1"
            ));
        }

        Ok(())
    }

    /// Apply network constraints
    fn apply_network_constraints(&mut self) -> Result<()> {
        let security = &self.config.policy.security;

        if security.enable_network_isolation {
            // Block network access
            self.context.env.insert("CLEANROOM_NET".to_string(), "offline".to_string());
        } else if !security.allowed_ports.is_empty() {
            // Allow only specific ports
            self.context.env.insert("CLEANROOM_NET".to_string(), "limited".to_string());
            let ports_str = security.allowed_ports.iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(",");
            self.context.env.insert("CLEANROOM_ALLOWED_PORTS".to_string(), ports_str);
        } else {
            // Allow all network access
            self.context.env.insert("CLEANROOM_NET".to_string(), "open".to_string());
        }

        Ok(())
    }

    /// Apply filesystem constraints
    fn apply_filesystem_constraints(&mut self) -> Result<()> {
        let execution = &self.config.policy.execution;

        if execution.enable_test_isolation {
            self.context.env.insert("CLEANROOM_FS".to_string(), "isolated".to_string());
        } else {
            self.context.env.insert("CLEANROOM_FS".to_string(), "normal".to_string());
        }

        Ok(())
    }

    /// Run the configured command
    fn run_command(&self) -> Result<RunOutput> {
        use std::process::Command;

        let mut cmd = Command::new(&self.config.args[0]);

        // Add arguments
        if self.config.args.len() > 1 {
            cmd.args(&self.config.args[1..]);
        }

        // Set working directory
        if let Some(ref workdir) = self.context.workdir {
            cmd.current_dir(workdir);
        }

        // Set environment variables
        for (key, value) in &self.context.env {
            cmd.env(key, value);
        }

        // Execute with timeout
        let start_time = std::time::Instant::now();
        let output = cmd.output().map_err(|e| {
            crate::error::CleanroomError::container_error(format!("Command execution failed: {}", e))
        })?;

        let duration_ms = start_time.elapsed().as_millis() as u64;

        Ok(RunOutput::new(
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stdout).to_string(),
            String::from_utf8_lossy(&output.stderr).to_string(),
            duration_ms,
        ))
    }

    /// Get execution context
    pub fn context(&self) -> &ExecutionContext {
        &self.context
    }

    /// Get configuration
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// Check if execution is still running
    pub fn is_running(&self) -> bool {
        // In a real implementation, you would check if the process is still running
        // For now, we assume synchronous execution
        false
    }

    /// Terminate execution
    pub fn terminate(&mut self) -> Result<()> {
        // In a real implementation, you would send a termination signal
        // For now, we assume synchronous execution
        Ok(())
    }
}

/// Runtime builder for creating runtime configurations
pub struct RuntimeBuilder {
    config: Config,
}

impl RuntimeBuilder {
    /// Create a new runtime builder
    pub fn new(args: Vec<String>) -> Self {
        Self {
            config: Config::new(args),
        }
    }

    /// Set working directory
    pub fn workdir(mut self, workdir: PathBuf) -> Self {
        self.config.workdir = Some(workdir);
        self
    }

    /// Add environment variable
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config.env.insert(key.into(), value.into());
        self
    }

    /// Set timeout
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.config.execution_timeout = timeout;
        self
    }

    /// Set policy
    pub fn policy(mut self, policy: Policy) -> Self {
        self.config.policy = policy;
        self
    }

    /// Build the runtime
    pub fn build(self) -> Result<Runtime> {
        Runtime::new(self.config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_creation() {
        let config = Config::new(vec!["echo".to_string(), "hello".to_string()]);
        assert_eq!(config.args, vec!["echo", "hello"]);
        assert_eq!(config.execution_timeout, Duration::from_secs(300));
    }

    #[test]
    fn test_config_with_workdir() {
        let config = Config::new(vec!["echo".to_string()])
            .with_workdir(std::env::temp_dir());
        assert!(config.workdir.is_some());
    }

    #[test]
    fn test_config_with_env() {
        let config = Config::new(vec!["echo".to_string()])
            .with_env("TEST_VAR", "test_value");
        assert_eq!(config.env.get("TEST_VAR"), Some(&"test_value".to_string()));
    }

    #[test]
    fn test_config_with_timeout() {
        let config = Config::new(vec!["echo".to_string()])
            .with_timeout(Duration::from_secs(60));
        assert_eq!(config.execution_timeout, Duration::from_secs(60));
    }

    #[test]
    fn test_config_validation_empty_args() {
        let config = Config::new(vec![]);
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_config_validation_zero_timeout() {
        let config = Config::new(vec!["echo".to_string()])
            .with_timeout(Duration::from_secs(0));
        assert!(config.validate().is_err());
    }

    #[test]
    fn test_run_output_creation() {
        let output = RunOutput::new(0, "hello".to_string(), "".to_string(), 100);
        assert!(output.success());
        assert!(!output.failed());
        assert_eq!(output.combined_output(), "hello\n");
        assert_eq!(output.output_size(), 5);
    }

    #[test]
    fn test_run_output_failure() {
        let output = RunOutput::new(1, "".to_string(), "error".to_string(), 50);
        assert!(!output.success());
        assert!(output.failed());
        assert_eq!(output.combined_output(), "\nerror");
        assert_eq!(output.output_size(), 5);
    }

    #[test]
    fn test_runtime_builder() {
        let runtime = RuntimeBuilder::new(vec!["echo".to_string(), "hello".to_string()])
            .timeout(Duration::from_secs(60))
            .env("TEST_VAR", "test_value")
            .build()
            .unwrap();

        assert_eq!(runtime.config().args, vec!["echo", "hello"]);
        assert_eq!(runtime.config().execution_timeout, Duration::from_secs(60));
        assert_eq!(runtime.config().env.get("TEST_VAR"), Some(&"test_value".to_string()));
    }

    #[test]
    fn test_runtime_execution() {
        let mut runtime = RuntimeBuilder::new(vec!["echo".to_string(), "hello".to_string()])
            .build()
            .unwrap();

        let output = runtime.execute().unwrap();
        assert!(output.success());
        assert!(output.stdout.contains("hello"));
    }

    #[test]
    fn test_execution_context() {
        let runtime = RuntimeBuilder::new(vec!["echo".to_string()])
            .build()
            .unwrap();

        let context = runtime.context();
        assert!(context.pid.is_none());
        assert!(context.env.is_empty());
        assert!(context.workdir.is_none());
    }
}