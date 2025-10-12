//! Command runner with deterministic surfaces and RAII lifecycle

use crate::error::{BackendError, Result};
use crate::policy::Policy;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

/// Command execution output
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

/// Command execution configuration
#[derive(Debug, Clone)]
pub struct Config {
    /// Command and arguments
    pub args: Vec<String>,

    /// Working directory
    pub workdir: Option<PathBuf>,

    /// Environment variables
    pub env: HashMap<String, String>,

    /// Execution timeout
    pub timeout: Duration,

    /// Policy to enforce
    pub policy: Policy,
}

/// Command runner with policy enforcement and RAII lifecycle
pub struct Runner {
    config: Config,
    _prepared: Option<Prepared>,
}

/// Prepared execution environment (RAII guard)
pub struct Prepared {
    /// Working directory
    pub workdir: Option<PathBuf>,
    /// Environment variables
    pub env: HashMap<String, String>,
    /// Policy constraints
    pub policy: Policy,
}

impl Prepared {
    /// Create a prepared execution environment
    pub fn new(config: &Config) -> Result<Self> {
        // Apply policy constraints
        let policy = config.policy.clone();

        // Set up environment
        let mut env = config.env.clone();
        for (key, value) in policy.to_env() {
            env.insert(key, value);
        }

        Ok(Self {
            workdir: config.workdir.clone(),
            env,
            policy,
        })
    }
}

impl Runner {
    /// Create a new runner with configuration
    pub fn new(config: Config) -> Self {
        Self {
            config,
            _prepared: None,
        }
    }

    /// Prepare execution environment (RAII setup)
    pub fn prepare(&self) -> Result<Prepared> {
        Prepared::new(&self.config)
    }

    /// Execute the command with deterministic surfaces
    pub fn run(&self, prepared: &Prepared) -> Result<RunOutput> {
        if self.config.args.is_empty() {
            return Err(BackendError::Runtime("empty command".to_string()).into());
        }

        let start = std::time::Instant::now();

        let mut cmd = Command::new(&self.config.args[0]);

        // Add arguments
        if self.config.args.len() > 1 {
            cmd.args(&self.config.args[1..]);
        }

        // Set working directory
        if let Some(ref workdir) = prepared.workdir {
            cmd.current_dir(workdir);
        }

        // Set environment variables
        for (key, value) in &prepared.env {
            cmd.env(key, value);
        }

        // Execute with timeout
        // WIP: Implement proper timeout mechanism
        let output = cmd.output()
            .map_err(|e| BackendError::Runtime(format!("command failed: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;

        Ok(RunOutput {
            exit_code: output.status.code().unwrap_or(-1),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            duration_ms,
        })
    }

    /// Execute and return status code only
    pub fn run_status(&self, prepared: &Prepared) -> Result<i32> {
        let output = self.run(prepared)?;
        Ok(output.exit_code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runner_echo() {
        let config = Config {
            args: vec!["echo".to_string(), "hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            timeout: Duration::from_secs(5),
            policy: Policy::permissive(),
        };

        let runner = Runner::new(config);
        let prepared = runner.prepare().unwrap();
        let output = runner.run(&prepared).unwrap();

        assert_eq!(output.exit_code, 0);
        assert!(output.stdout.contains("hello"));
    }

    #[test]
    fn test_runner_status() {
        let config = Config {
            args: vec!["true".to_string()],
            workdir: None,
            env: HashMap::new(),
            timeout: Duration::from_secs(5),
            policy: Policy::permissive(),
        };

        let runner = Runner::new(config);
        let prepared = runner.prepare().unwrap();
        let status = runner.run_status(&prepared).unwrap();
        assert_eq!(status, 0);
    }
}
