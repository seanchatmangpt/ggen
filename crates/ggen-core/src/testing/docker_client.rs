//! Docker CLI wrapper for container management
//!
//! Provides a type-safe wrapper around Docker CLI commands with comprehensive
//! error handling and output parsing.

use ggen_utils::error::{Context, Result};
use std::collections::HashMap;
use std::process::Command;
use std::time::Duration;

/// Docker command execution wrapper
///
/// Provides type-safe execution of Docker CLI commands with error handling.
#[derive(Debug, Clone)]
pub struct DockerClient {
    /// Docker binary path (defaults to "docker")
    binary: String,
    /// Default command timeout
    timeout: Duration,
}

impl Default for DockerClient {
    fn default() -> Self {
        Self::new()
    }
}

impl DockerClient {
    /// Create a new Docker client with default settings
    #[must_use]
    pub fn new() -> Self {
        Self {
            binary: "docker".to_string(),
            timeout: Duration::from_secs(30),
        }
    }

    /// Create a Docker client with custom binary path
    #[must_use]
    pub fn with_binary(binary: impl Into<String>) -> Self {
        Self {
            binary: binary.into(),
            timeout: Duration::from_secs(30),
        }
    }

    /// Set the default command timeout
    #[must_use]
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout;
        self
    }

    /// Execute a Docker command and return stdout
    ///
    /// # Errors
    ///
    /// Returns an error if the Docker command fails or produces non-zero exit code
    pub fn execute(&self, cmd: &DockerCommand) -> Result<String> {
        let mut command = Command::new(&self.binary);
        command.args(cmd.args());

        let output = command
            .output()
            .context("Failed to execute docker command")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ggen_utils::error::Error::new(&format!(
                "Docker command failed: {}",
                stderr
            )));
        }

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        Ok(stdout.trim().to_string())
    }

    /// Run a container and return the container ID
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to start
    pub fn run(&self, image: &str, args: &[String]) -> Result<String> {
        let mut cmd_args = vec!["run".to_string(), "-d".to_string()];
        cmd_args.extend_from_slice(args);
        cmd_args.push(image.to_string());

        let cmd = DockerCommand::new("run", &cmd_args);
        self.execute(&cmd)
    }

    /// Stop a running container
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to stop
    pub fn stop(&self, container_id: &str) -> Result<()> {
        let cmd = DockerCommand::new("stop", &[container_id.to_string()]);
        self.execute(&cmd)?;
        Ok(())
    }

    /// Remove a container
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to be removed
    pub fn rm(&self, container_id: &str, force: bool) -> Result<()> {
        let mut args = vec![];
        if force {
            args.push("-f".to_string());
        }
        args.push(container_id.to_string());

        let cmd = DockerCommand::new("rm", &args);
        self.execute(&cmd)?;
        Ok(())
    }

    /// Execute a command inside a running container
    ///
    /// # Errors
    ///
    /// Returns an error if the exec command fails
    pub fn exec(&self, container_id: &str, exec_args: &[String]) -> Result<String> {
        let mut args = vec!["exec".to_string(), container_id.to_string()];
        args.extend_from_slice(exec_args);

        let cmd = DockerCommand::new("exec", &args);
        self.execute(&cmd)
    }

    /// Get the host port mapping for a container port
    ///
    /// # Errors
    ///
    /// Returns an error if port mapping cannot be determined
    pub fn port(&self, container_id: &str, container_port: &str) -> Result<u16> {
        let cmd = DockerCommand::new("port", &[container_id.to_string(), container_port.to_string()]);
        let output = self.execute(&cmd)?;

        // Parse output like "0.0.0.0:32768"
        let port_str = output
            .split(':')
            .last()
            .ok_or_else(|| ggen_utils::error::Error::new("Failed to parse port mapping"))?;

        port_str
            .parse::<u16>()
            .map_err(|e| ggen_utils::error::Error::new(&format!("Invalid port number: {}", e)))
    }

    /// Pause a running container
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to pause
    pub fn pause(&self, container_id: &str) -> Result<()> {
        let cmd = DockerCommand::new("pause", &[container_id.to_string()]);
        self.execute(&cmd)?;
        Ok(())
    }

    /// Unpause a paused container
    ///
    /// # Errors
    ///
    /// Returns an error if the container fails to unpause
    pub fn unpause(&self, container_id: &str) -> Result<()> {
        let cmd = DockerCommand::new("unpause", &[container_id.to_string()]);
        self.execute(&cmd)?;
        Ok(())
    }

    /// Inspect a container and return JSON output
    ///
    /// # Errors
    ///
    /// Returns an error if the container inspection fails
    pub fn inspect(&self, container_id: &str) -> Result<String> {
        let cmd = DockerCommand::new("inspect", &[container_id.to_string()]);
        self.execute(&cmd)
    }

    /// Get container logs
    ///
    /// # Errors
    ///
    /// Returns an error if logs cannot be retrieved
    pub fn logs(&self, container_id: &str, tail: Option<usize>) -> Result<String> {
        let mut args = vec!["logs".to_string()];
        if let Some(n) = tail {
            args.push("--tail".to_string());
            args.push(n.to_string());
        }
        args.push(container_id.to_string());

        let cmd = DockerCommand::new("logs", &args);
        self.execute(&cmd)
    }
}

/// Docker command builder
///
/// Provides a type-safe way to construct Docker CLI commands.
#[derive(Debug, Clone)]
pub struct DockerCommand {
    /// Command name (e.g., "run", "stop", "rm")
    command: String,
    /// Command arguments
    args: Vec<String>,
}

impl DockerCommand {
    /// Create a new Docker command
    #[must_use]
    pub fn new(command: &str, args: &[String]) -> Self {
        Self {
            command: command.to_string(),
            args: args.to_vec(),
        }
    }

    /// Get the command arguments including the command name
    #[must_use]
    pub fn args(&self) -> Vec<String> {
        let mut all_args = vec![self.command.clone()];
        all_args.extend(self.args.clone());
        all_args
    }
}

/// Parse environment variables from Docker command args
#[must_use]
pub fn parse_env_vars(args: &[String]) -> HashMap<String, String> {
    let mut env_vars = HashMap::new();
    let mut iter = args.iter();

    while let Some(arg) = iter.next() {
        if arg == "-e" || arg == "--env" {
            if let Some(env_value) = iter.next() {
                if let Some((key, value)) = env_value.split_once('=') {
                    env_vars.insert(key.to_string(), value.to_string());
                }
            }
        }
    }

    env_vars
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_docker_client_creation() {
        let client = DockerClient::new();
        assert_eq!(client.binary, "docker");
        assert_eq!(client.timeout, Duration::from_secs(30));
    }

    #[test]
    fn test_docker_command_creation() {
        let cmd = DockerCommand::new("run", &["-d".to_string(), "redis".to_string()]);
        assert_eq!(cmd.command, "run");
        assert_eq!(cmd.args, vec!["-d", "redis"]);
    }

    #[test]
    fn test_parse_env_vars() {
        let args = vec![
            "-d".to_string(),
            "-e".to_string(),
            "KEY1=value1".to_string(),
            "--env".to_string(),
            "KEY2=value2".to_string(),
        ];

        let env_vars = parse_env_vars(&args);
        assert_eq!(env_vars.len(), 2);
        assert_eq!(env_vars.get("KEY1"), Some(&"value1".to_string()));
        assert_eq!(env_vars.get("KEY2"), Some(&"value2".to_string()));
    }
}
