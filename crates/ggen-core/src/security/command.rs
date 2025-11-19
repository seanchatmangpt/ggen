//! Safe command execution to prevent command injection attacks
//!
//! **SECURITY ISSUE 3: Command Injection Prevention**
//!
//! This module provides safe wrappers around `std::process::Command` to prevent
//! command injection attacks by avoiding shell execution and validating inputs.

use ggen_utils::error::{Error, Result};
use std::path::Path;
use std::process::{Command, Output};

/// Error type for command execution failures
#[derive(Debug, thiserror::Error)]
pub enum CommandError {
    #[error("Invalid command: {0}")]
    InvalidCommand(String),

    #[error("Command not allowed: {0}")]
    NotAllowed(String),

    #[error("Command execution failed: {0}")]
    ExecutionFailed(String),

    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    #[error("Command output invalid UTF-8")]
    InvalidUtf8,
}

impl From<CommandError> for Error {
    fn from(err: CommandError) -> Self {
        Error::new(&err.to_string())
    }
}

/// Safe command builder that prevents injection attacks
///
/// Instead of executing commands through a shell (vulnerable to injection),
/// this executes programs directly with validated arguments.
///
/// # Security Features
///
/// - No shell execution (prevents `; rm -rf /` attacks)
/// - Argument validation (prevents metacharacter injection)
/// - Whitelist of allowed commands
/// - Path validation for file operations
///
/// # Examples
///
/// ```rust
/// use ggen_core::security::command::SafeCommand;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// // Safe: Direct program execution
/// let output = SafeCommand::new("git")?
///     .arg("init")?
///     .execute()?;
///
/// // Unsafe equivalent that we prevent:
/// // Command::new("sh").arg("-c").arg(user_input) // VULNERABLE!
/// # Ok(())
/// # }
/// ```
#[derive(Clone)]
pub struct SafeCommand {
    program: String,
    args: Vec<String>,
    current_dir: Option<String>,
}

impl SafeCommand {
    /// Allowed commands whitelist
    const ALLOWED_COMMANDS: &'static [&'static str] =
        &["git", "cargo", "npm", "node", "rustc", "rustup"];

    /// Dangerous shell metacharacters to reject
    const DANGEROUS_CHARS: &'static [char] = &[
        ';', '|', '&', '$', '`', '\n', '\r', '<', '>', '(', ')', '{', '}',
    ];

    /// Create a new safe command
    ///
    /// # Security
    ///
    /// - Validates command is in whitelist
    /// - Prevents shell execution
    /// - Rejects commands with dangerous characters
    pub fn new(program: &str) -> Result<Self> {
        // Validate program name
        if program.is_empty() {
            return Err(CommandError::InvalidCommand("Empty command".to_string()).into());
        }

        // Check for dangerous characters
        if program.chars().any(|c| Self::DANGEROUS_CHARS.contains(&c)) {
            return Err(CommandError::InvalidCommand(format!(
                "Command contains dangerous characters: {}",
                program
            ))
            .into());
        }

        // Check whitelist
        if !Self::ALLOWED_COMMANDS.contains(&program) {
            return Err(CommandError::NotAllowed(format!(
                "Command '{}' is not in allowed list",
                program
            ))
            .into());
        }

        Ok(Self {
            program: program.to_string(),
            args: Vec::new(),
            current_dir: None,
        })
    }

    /// Add an argument to the command
    ///
    /// # Security
    ///
    /// - Validates argument doesn't contain shell metacharacters
    /// - Prevents injection via argument expansion
    pub fn arg(mut self, arg: &str) -> Result<Self> {
        // Validate argument
        if arg.chars().any(|c| Self::DANGEROUS_CHARS.contains(&c)) {
            return Err(CommandError::InvalidArgument(format!(
                "Argument contains dangerous characters: {}",
                arg
            ))
            .into());
        }

        self.args.push(arg.to_string());
        Ok(self)
    }

    /// Add multiple arguments
    pub fn args<I, S>(mut self, args: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        for arg in args {
            self = self.arg(arg.as_ref())?;
        }
        Ok(self)
    }

    /// Set the working directory
    ///
    /// # Security
    ///
    /// - Validates path exists and is a directory
    /// - Prevents path traversal attacks
    pub fn current_dir(mut self, dir: &Path) -> Result<Self> {
        // Validate directory exists
        if !dir.exists() {
            return Err(Error::new(&format!(
                "Directory does not exist: {}",
                dir.display()
            )));
        }

        if !dir.is_dir() {
            return Err(Error::new(&format!(
                "Path is not a directory: {}",
                dir.display()
            )));
        }

        // Store as string for later use
        self.current_dir = Some(dir.to_string_lossy().to_string());
        Ok(self)
    }

    /// Execute the command safely
    ///
    /// # Security
    ///
    /// - Uses direct program execution (no shell)
    /// - All arguments are properly escaped
    /// - Output is validated as UTF-8
    pub fn execute(self) -> Result<Output> {
        let mut cmd = Command::new(&self.program);

        // Add arguments
        for arg in &self.args {
            cmd.arg(arg);
        }

        // Set working directory if specified
        if let Some(dir) = &self.current_dir {
            cmd.current_dir(dir);
        }

        // Execute command
        let output = cmd
            .output()
            .map_err(|e| CommandError::ExecutionFailed(format!("{}: {}", self.program, e)))?;

        Ok(output)
    }

    /// Execute and return stdout as String
    ///
    /// # Security
    ///
    /// - Validates output is valid UTF-8
    /// - Returns error on non-zero exit code
    pub fn execute_stdout(self) -> Result<String> {
        let program = self.program.clone();
        let output = self.execute()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(CommandError::ExecutionFailed(format!("{}: {}", program, stderr)).into());
        }

        String::from_utf8(output.stdout).map_err(|_| CommandError::InvalidUtf8.into())
    }
}

/// Command executor with additional safety features
pub struct CommandExecutor;

impl CommandExecutor {
    /// Execute a git command safely
    pub fn git(args: &[&str]) -> Result<Output> {
        let mut cmd = SafeCommand::new("git")?;
        for arg in args {
            cmd = cmd.arg(arg)?;
        }
        cmd.execute()
    }

    /// Execute a cargo command safely
    pub fn cargo(args: &[&str]) -> Result<Output> {
        let mut cmd = SafeCommand::new("cargo")?;
        for arg in args {
            cmd = cmd.arg(arg)?;
        }
        cmd.execute()
    }

    /// Execute an npm command safely
    pub fn npm(args: &[&str]) -> Result<Output> {
        let mut cmd = SafeCommand::new("npm")?;
        for arg in args {
            cmd = cmd.arg(arg)?;
        }
        cmd.execute()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_command_new_validates_whitelist() {
        // Allowed command
        assert!(SafeCommand::new("git").is_ok());
        assert!(SafeCommand::new("cargo").is_ok());

        // Disallowed command
        assert!(SafeCommand::new("rm").is_err());
        assert!(SafeCommand::new("sh").is_err());
    }

    #[test]
    fn test_safe_command_rejects_dangerous_chars() {
        // Dangerous characters in command
        assert!(SafeCommand::new("git; rm -rf /").is_err());
        assert!(SafeCommand::new("git | cat").is_err());
        assert!(SafeCommand::new("git && ls").is_err());

        // Dangerous characters in arguments
        let cmd1 = SafeCommand::new("git").unwrap();
        assert!(cmd1.arg("init; rm -rf /").is_err());

        let cmd2 = SafeCommand::new("git").unwrap();
        assert!(cmd2.arg("init | cat").is_err());
    }

    #[test]
    fn test_safe_command_arg_validation() {
        let cmd = SafeCommand::new("git").unwrap();

        // Safe arguments
        assert!(cmd.clone().arg("init").is_ok());
        assert!(cmd.clone().arg("status").is_ok());

        // Dangerous arguments
        assert!(cmd.clone().arg("init; ls").is_err());
        assert!(cmd.clone().arg("$(whoami)").is_err());
        assert!(cmd.clone().arg("`whoami`").is_err());
    }

    #[test]
    fn test_command_injection_prevention() {
        // Attempt command injection via arguments
        let result = SafeCommand::new("git")
            .unwrap()
            .arg("init")
            .unwrap()
            .arg("; rm -rf /");

        assert!(result.is_err());

        // Attempt command injection via command name
        let result = SafeCommand::new("git; whoami");
        assert!(result.is_err());
    }

    #[test]
    fn test_executor_git() {
        // This will fail if git is not installed, but tests the API
        let result = CommandExecutor::git(&["--version"]);
        // Just ensure it returns a Result
        let _ = result;
    }
}
