//! Local backend for running commands directly on the host system.

use crate::backend::{AsyncBackend, Backend, Cmd, Prepared, RunResult};
use crate::error::{BackendError, Result};
use crate::runtime::runner::{Config as RunnerConfig, RunOutput};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

/// Local backend that executes commands directly on the host system.
///
/// This backend provides minimal isolation and is primarily useful for:
/// - Fast iteration during development
/// - Systems where containerization is unavailable
/// - Testing the cleanroom framework itself
///
/// # Limitations
///
/// Many determinism surfaces cannot be enforced on local backend:
/// - Time: Cannot freeze system time
/// - Network: Cannot enforce offline mode without iptables
/// - Filesystem: Cannot enforce read-only rootfs
/// - Process: Cannot drop capabilities
///
/// Warnings will be logged when constraints cannot be enforced.
#[derive(Debug)]
pub struct LocalBackend;

impl LocalBackend {
    /// Create a new local backend.
    pub fn new() -> Self {
        Self
    }

    /// Check if local backend is available (always true)
    pub fn is_available(&self) -> bool {
        true
    }
}

impl Backend for LocalBackend {
    fn is_available(&self) -> bool {
        true
    }

    fn name(&self) -> &str {
        "local"
    }

    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        // READY: Determinism constraints applied with documented limitations
        let start = Instant::now();

        // Build command
        let mut command = Command::new(&cmd.bin);
        command.args(&cmd.args);

        // Apply environment-based constraints
        // Local backend has limitations - document what we can and cannot enforce
        for (key, value) in &cmd.env {
            command.env(key, value);

            // Warn about constraints that cannot be enforced locally
            #[cfg(feature = "tracing")]
            {
                match key.as_str() {
                    "CLEANROOM_NET" if value == "offline" => {
                        tracing::warn!("Network isolation cannot be enforced in local backend");
                    }
                    "CLEANROOM_FS" if value == "readonly" => {
                        tracing::warn!(
                            "Filesystem read-only mode cannot be enforced in local backend"
                        );
                    }
                    "CLEANROOM_PROC" if value == "strict" => {
                        tracing::warn!(
                            "Strict process isolation cannot be enforced in local backend"
                        );
                    }
                    _ => {}
                }
            }
        }

        // Set working directory if specified
        if let Some(ref workdir) = cmd.workdir {
            command.current_dir(workdir);
        }

        // Execute command (timeout enforcement delegated to external supervision)
        let output = command
            .output()
            .map_err(|e| BackendError::Runtime(format!("Failed to execute {}: {}", cmd.bin, e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;

        // Check soft timeout limit
        if let Some(timeout_ms) = cmd.timeout_ms {
            if duration_ms > timeout_ms {
                #[cfg(feature = "tracing")]
                tracing::warn!(
                    "Command exceeded soft timeout: {}ms > {}ms (not enforced in local backend)",
                    duration_ms,
                    timeout_ms
                );
            }
        }

        Ok(RunResult::from_output(output, duration_ms))
    }
}

impl Default for LocalBackend {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_local_backend_creation() {
        let backend = LocalBackend::new();
        assert_eq!(backend.name(), "local");
    }

    #[test]
    fn test_local_backend_echo() {
        let backend = LocalBackend::new();
        let cmd = Cmd::new("echo").args(["hello", "world"]);

        let result = backend.run_cmd(cmd);
        assert!(result.is_ok());

        let run_result = result.expect("local backend should execute successfully");
        assert_eq!(run_result.exit_code, 0);
        assert!(run_result.stdout.contains("hello world"));
    }

    #[test]
    fn test_local_backend_nonzero_exit() {
        let backend = LocalBackend::new();
        let cmd = Cmd::new("sh").args(["-c", "exit 42"]);

        let result = backend.run_cmd(cmd);
        assert!(result.is_ok());

        let run_result = result.expect("local backend should execute successfully");
        assert_eq!(run_result.exit_code, 42);
    }
}

// Async backend implementation for LocalBackend
#[async_trait::async_trait]
impl AsyncBackend for LocalBackend {
    async fn prepare(&self, cfg: &RunnerConfig) -> Result<Prepared> {
        let mut env = cfg.env.clone();

        // Apply policy constraints to environment
        let policy_env = cfg.policy.to_env();
        for (key, value) in policy_env {
            env.insert(key, value);
        }

        Ok(Prepared {
            workdir: cfg.workdir.clone(),
            env,
            policy: cfg.policy.clone(),
        })
    }

    async fn run(&self, p: &Prepared, args: &[&str]) -> Result<RunOutput> {
        let start = std::time::Instant::now();

        let mut cmd = Command::new(args[0]);
        if args.len() > 1 {
            cmd.args(&args[1..]);
        }

        if let Some(ref workdir) = p.workdir {
            cmd.current_dir(workdir);
        }

        for (key, value) in &p.env {
            cmd.env(key, value);
        }

        let output = cmd
            .output()
            .map_err(|e| BackendError::Runtime(format!("command failed: {}", e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;

        Ok(RunOutput {
            exit_code: output.status.code().unwrap_or(-1),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            duration_ms,
        })
    }

    async fn teardown(&self, _p: Prepared) -> Result<()> {
        // Local backend doesn't need teardown
        Ok(())
    }

    fn name(&self) -> &str {
        "local"
    }
}
