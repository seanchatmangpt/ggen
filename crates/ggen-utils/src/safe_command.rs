//! Safe command execution with validation to prevent command injection attacks
//!
//! This module provides the `SafeCommand` type that wraps shell commands with validation
//! to prevent common command injection vulnerabilities. All commands are validated on
//! construction to ensure they:
//!
//! - Are in the whitelist of allowed commands
//! - Do not contain shell metacharacters in arguments (;|&><$`\n)
//! - Do not exceed maximum command length of 4096 characters
//! - Use validated paths for path arguments
//!
//! ## Security Model
//!
//! This module uses a multi-layered defense approach:
//!
//! 1. **Command Whitelist**: Only explicitly allowed commands can be executed
//! 2. **Argument Sanitization**: Shell metacharacters are blocked in arguments
//! 3. **Type-State Pattern**: Commands must be validated before execution
//! 4. **Path Integration**: Path arguments use SafePath validation
//! 5. **Length Limits**: Maximum 4096 characters to prevent buffer overflow
//!
//! ## Examples
//!
//! ```rust
//! use ggen_utils::safe_command::{SafeCommand, CommandName, CommandArg};
//!
//! // Valid command
//! let cmd = SafeCommand::new("cargo")
//!     .unwrap()
//!     .arg("build")
//!     .unwrap()
//!     .arg("--release")
//!     .unwrap()
//!     .validate()
//!     .unwrap();
//!
//! // Invalid command (not in whitelist)
//! assert!(SafeCommand::new("rm").is_err());
//!
//! // Invalid argument (shell metacharacter)
//! let result = SafeCommand::new("cargo")
//!     .unwrap()
//!     .arg("build; rm -rf /")
//!     .unwrap_or_else(|e| panic!("Should fail: {}", e));
//! ```

use crate::error::{Error, Result};
use crate::safe_path::SafePath;
use std::convert::TryFrom;
use std::fmt;
use std::process::Command;

/// Maximum allowed command length (including all arguments)
const MAX_COMMAND_LENGTH: usize = 4096;

/// Shell metacharacters that are blocked in command arguments
const SHELL_METACHARACTERS: &[char] = &[';', '|', '&', '>', '<', '$', '`', '\n', '\r'];

/// Whitelist of allowed commands
const ALLOWED_COMMANDS: &[&str] = &[
    "cargo",
    "git",
    "npm",
    "rustc",
    "rustfmt",
    "clippy-driver",
    "timeout",
    "make",
    "cmake",
    "sh",   // Only when explicitly needed with validated scripts
    "bash", // Only when explicitly needed with validated scripts
    "ggen", // Our own CLI
];

/// A validated command name from the whitelist
///
/// This newtype ensures that only whitelisted commands can be constructed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CommandName {
    /// Inner command name - private to prevent direct mutation
    inner: String,
}

impl CommandName {
    /// Create a new CommandName from a string
    ///
    /// # Validation Rules
    ///
    /// - Command must be in the whitelist
    /// - Command name cannot be empty
    /// - Command name cannot contain whitespace
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::CommandName;
    ///
    /// // Valid commands
    /// assert!(CommandName::new("cargo").is_ok());
    /// assert!(CommandName::new("git").is_ok());
    ///
    /// // Invalid commands
    /// assert!(CommandName::new("rm").is_err());
    /// assert!(CommandName::new("").is_err());
    /// ```
    pub fn new<S: AsRef<str>>(name: S) -> Result<Self> {
        let name = name.as_ref();

        // Check for empty
        if name.is_empty() {
            return Err(Error::invalid_input("Command name cannot be empty"));
        }

        // Check for whitespace
        if name.contains(char::is_whitespace) {
            return Err(Error::invalid_input(
                "Command name cannot contain whitespace",
            ));
        }

        // Check whitelist
        if !ALLOWED_COMMANDS.contains(&name) {
            return Err(Error::invalid_input(format!(
                "Command '{}' is not in whitelist. Allowed commands: {:?}",
                name, ALLOWED_COMMANDS
            )));
        }

        Ok(Self {
            inner: name.to_string(),
        })
    }

    /// Get the command name as a string slice
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.inner
    }

    /// Convert into the inner String
    #[must_use]
    pub fn into_string(self) -> String {
        self.inner
    }
}

impl TryFrom<&str> for CommandName {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        Self::new(value)
    }
}

impl TryFrom<String> for CommandName {
    type Error = Error;

    fn try_from(value: String) -> Result<Self> {
        Self::new(value)
    }
}

impl fmt::Display for CommandName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

/// A validated command argument with sanitization
///
/// This newtype ensures that command arguments do not contain shell metacharacters
/// that could lead to command injection attacks.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CommandArg {
    /// Inner argument - private to prevent direct mutation
    inner: String,
}

impl CommandArg {
    /// Create a new CommandArg from a string
    ///
    /// # Validation Rules
    ///
    /// - Argument cannot contain shell metacharacters (;|&><$`\n\r)
    /// - Argument can be empty (for flags like --release)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::CommandArg;
    ///
    /// // Valid arguments
    /// assert!(CommandArg::new("build").is_ok());
    /// assert!(CommandArg::new("--release").is_ok());
    /// assert!(CommandArg::new("").is_ok());
    ///
    /// // Invalid arguments (shell metacharacters)
    /// assert!(CommandArg::new("build; rm -rf /").is_err());
    /// assert!(CommandArg::new("build | tee").is_err());
    /// assert!(CommandArg::new("$(whoami)").is_err());
    /// ```
    pub fn new<S: AsRef<str>>(arg: S) -> Result<Self> {
        let arg = arg.as_ref();

        // Check for shell metacharacters
        for ch in SHELL_METACHARACTERS {
            if arg.contains(*ch) {
                return Err(Error::invalid_input(format!(
                    "Argument contains shell metacharacter '{}': {}",
                    ch, arg
                )));
            }
        }

        Ok(Self {
            inner: arg.to_string(),
        })
    }

    /// Create a CommandArg from a SafePath
    ///
    /// This ensures that path arguments are validated through SafePath.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::CommandArg;
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let path = SafePath::new("src/generated").unwrap();
    /// let arg = CommandArg::from_path(&path);
    /// assert_eq!(arg.as_str(), "src/generated");
    /// ```
    #[must_use]
    pub fn from_path(path: &SafePath) -> Self {
        // SafePath already validated, so we can safely convert
        // We know it doesn't contain shell metacharacters
        Self {
            inner: path.as_path().display().to_string(),
        }
    }

    /// Get the argument as a string slice
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.inner
    }

    /// Convert into the inner String
    #[must_use]
    pub fn into_string(self) -> String {
        self.inner
    }
}

impl TryFrom<&str> for CommandArg {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        Self::new(value)
    }
}

impl TryFrom<String> for CommandArg {
    type Error = Error;

    fn try_from(value: String) -> Result<Self> {
        Self::new(value)
    }
}

impl fmt::Display for CommandArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

/// Type-state marker for unvalidated commands
#[derive(Debug)]
pub struct Unvalidated;

/// Type-state marker for validated commands
#[derive(Debug, Clone)]
pub struct Validated;

/// A safe command builder with type-state pattern
///
/// Commands must be validated before they can be executed. The type-state pattern
/// ensures this at compile time.
///
/// # Type States
///
/// - `SafeCommand<Unvalidated>`: Command is being built, not yet validated
/// - `SafeCommand<Validated>`: Command has been validated and can be executed
///
/// # Examples
///
/// ```rust
/// use ggen_utils::safe_command::SafeCommand;
///
/// // Build and validate command
/// let cmd = SafeCommand::new("cargo")
///     .unwrap()
///     .arg("build")
///     .unwrap()
///     .arg("--release")
///     .unwrap()
///     .validate()
///     .unwrap();
///
/// // Now cmd can be converted to std::process::Command
/// let process_cmd = cmd.into_command();
/// ```
#[derive(Debug, Clone)]
pub struct SafeCommand<State = Unvalidated> {
    /// Validated command name
    command: CommandName,
    /// List of validated arguments
    args: Vec<CommandArg>,
    /// Type-state marker
    _state: std::marker::PhantomData<State>,
}

impl SafeCommand<Unvalidated> {
    /// Create a new SafeCommand with a validated command name
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::SafeCommand;
    ///
    /// let cmd = SafeCommand::new("cargo").unwrap();
    /// assert!(SafeCommand::new("rm").is_err());
    /// ```
    pub fn new<S: AsRef<str>>(command: S) -> Result<Self> {
        let command = CommandName::new(command)?;
        Ok(Self {
            command,
            args: Vec::new(),
            _state: std::marker::PhantomData,
        })
    }

    /// Add an argument to the command
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::SafeCommand;
    ///
    /// let cmd = SafeCommand::new("cargo")
    ///     .unwrap()
    ///     .arg("build")
    ///     .unwrap()
    ///     .arg("--release")
    ///     .unwrap();
    /// ```
    pub fn arg<S: AsRef<str>>(mut self, arg: S) -> Result<Self> {
        let arg = CommandArg::new(arg)?;
        self.args.push(arg);
        Ok(self)
    }

    /// Add a path argument to the command
    ///
    /// This ensures path arguments are validated through SafePath.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::SafeCommand;
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let path = SafePath::new("src/generated").unwrap();
    /// let cmd = SafeCommand::new("cargo")
    ///     .unwrap()
    ///     .arg("build")
    ///     .unwrap()
    ///     .arg_path(&path);
    /// ```
    #[must_use]
    pub fn arg_path(mut self, path: &SafePath) -> Self {
        let arg = CommandArg::from_path(path);
        self.args.push(arg);
        self
    }

    /// Add multiple arguments at once
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::SafeCommand;
    ///
    /// let cmd = SafeCommand::new("cargo")
    ///     .unwrap()
    ///     .args(&["build", "--release"])
    ///     .unwrap();
    /// ```
    pub fn args<I, S>(mut self, args: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        for arg in args {
            let validated_arg = CommandArg::new(arg)?;
            self.args.push(validated_arg);
        }
        Ok(self)
    }

    /// Validate the command and transition to Validated state
    ///
    /// This performs final validation including total command length check.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::SafeCommand;
    ///
    /// let cmd = SafeCommand::new("cargo")
    ///     .unwrap()
    ///     .arg("build")
    ///     .unwrap()
    ///     .validate()
    ///     .unwrap();
    /// ```
    pub fn validate(self) -> Result<SafeCommand<Validated>> {
        // Calculate total command length
        let total_length = self.total_length();

        if total_length > MAX_COMMAND_LENGTH {
            return Err(Error::invalid_input(format!(
                "Command length {} exceeds maximum allowed length of {}",
                total_length, MAX_COMMAND_LENGTH
            )));
        }

        Ok(SafeCommand {
            command: self.command,
            args: self.args,
            _state: std::marker::PhantomData,
        })
    }

    /// Calculate total command length (command + args + spaces)
    fn total_length(&self) -> usize {
        let mut length = self.command.as_str().len();

        for arg in &self.args {
            length += 1; // space
            length += arg.as_str().len();
        }

        length
    }
}

impl SafeCommand<Validated> {
    /// Convert this SafeCommand into a std::process::Command
    ///
    /// This is only available for validated commands.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_command::SafeCommand;
    ///
    /// let safe_cmd = SafeCommand::new("cargo")
    ///     .unwrap()
    ///     .arg("--version")
    ///     .unwrap()
    ///     .validate()
    ///     .unwrap();
    ///
    /// let mut cmd = safe_cmd.into_command();
    /// // Now you can execute cmd with std::process::Command
    /// ```
    #[must_use]
    pub fn into_command(self) -> Command {
        let mut cmd = Command::new(self.command.as_str());

        for arg in self.args {
            cmd.arg(arg.as_str());
        }

        cmd
    }

    /// Get a reference to the command name
    #[must_use]
    pub fn command(&self) -> &CommandName {
        &self.command
    }

    /// Get a reference to the arguments
    #[must_use]
    pub fn args(&self) -> &[CommandArg] {
        &self.args
    }

    /// Get the command as a string for display purposes
    ///
    /// This does NOT execute the command, it just formats it as a string.
    #[must_use]
    pub fn to_string_debug(&self) -> String {
        let mut result = self.command.as_str().to_string();

        for arg in &self.args {
            result.push(' ');
            result.push_str(arg.as_str());
        }

        result
    }
}

impl fmt::Display for SafeCommand<Validated> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string_debug())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ============================================================================
    // CommandName Tests
    // ============================================================================

    #[test]
    fn test_command_name_valid() {
        // Arrange
        let name = "cargo";

        // Act
        let result = CommandName::new(name);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "cargo");
    }

    #[test]
    fn test_command_name_all_whitelisted() {
        // Arrange & Act & Assert
        for &cmd in ALLOWED_COMMANDS {
            let result = CommandName::new(cmd);
            assert!(result.is_ok(), "Should allow whitelisted command: {}", cmd);
        }
    }

    #[test]
    fn test_command_name_not_whitelisted() {
        // Arrange
        let dangerous_commands = vec!["rm", "mv", "dd", "mkfs", "kill"];

        // Act & Assert
        for cmd in dangerous_commands {
            let result = CommandName::new(cmd);
            assert!(
                result.is_err(),
                "Should block non-whitelisted command: {}",
                cmd
            );
            assert!(
                result.unwrap_err().to_string().contains("not in whitelist"),
                "Error should mention whitelist"
            );
        }
    }

    #[test]
    fn test_command_name_empty() {
        // Arrange
        let name = "";

        // Act
        let result = CommandName::new(name);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_command_name_with_whitespace() {
        // Arrange
        let name = "cargo build";

        // Act
        let result = CommandName::new(name);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("whitespace"));
    }

    #[test]
    fn test_command_name_try_from_str() {
        // Arrange
        let name = "git";

        // Act
        let result = CommandName::try_from(name);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "git");
    }

    #[test]
    fn test_command_name_try_from_string() {
        // Arrange
        let name = String::from("npm");

        // Act
        let result = CommandName::try_from(name);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "npm");
    }

    #[test]
    fn test_command_name_display() {
        // Arrange
        let name = CommandName::new("cargo").unwrap();

        // Act
        let display = format!("{}", name);

        // Assert
        assert_eq!(display, "cargo");
    }

    // ============================================================================
    // CommandArg Tests
    // ============================================================================

    #[test]
    fn test_command_arg_valid() {
        // Arrange
        let arg = "build";

        // Act
        let result = CommandArg::new(arg);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "build");
    }

    #[test]
    fn test_command_arg_with_dashes() {
        // Arrange
        let args = vec!["--release", "-v", "--all-features"];

        // Act & Assert
        for arg in args {
            let result = CommandArg::new(arg);
            assert!(result.is_ok(), "Should allow arg with dashes: {}", arg);
        }
    }

    #[test]
    fn test_command_arg_empty() {
        // Arrange
        let arg = "";

        // Act
        let result = CommandArg::new(arg);

        // Assert - empty args are allowed (for compatibility)
        assert!(result.is_ok());
    }

    #[test]
    fn test_command_arg_shell_metacharacters() {
        // Arrange
        let attacks = vec![
            ("build; rm -rf /", ';'),
            ("build | tee output", '|'),
            ("build && rm -rf /", '&'),
            ("build > /dev/null", '>'),
            ("build < input", '<'),
            ("$(whoami)", '$'),
            ("`whoami`", '`'),
            ("build\nrm -rf /", '\n'),
            ("build\rrm -rf /", '\r'),
        ];

        // Act & Assert
        for (attack, metachar) in attacks {
            let result = CommandArg::new(attack);
            assert!(
                result.is_err(),
                "Should block shell metacharacter: {}",
                metachar
            );
            assert!(
                result.unwrap_err().to_string().contains("metacharacter"),
                "Error should mention metacharacter"
            );
        }
    }

    #[test]
    fn test_command_arg_from_path() {
        // Arrange
        let path = SafePath::new("src/generated").unwrap();

        // Act
        let arg = CommandArg::from_path(&path);

        // Assert
        assert_eq!(arg.as_str(), "src/generated");
    }

    #[test]
    fn test_command_arg_try_from() {
        // Arrange
        let arg_str = "--release";

        // Act
        let result = CommandArg::try_from(arg_str);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), "--release");
    }

    // ============================================================================
    // SafeCommand Basic Tests
    // ============================================================================

    #[test]
    fn test_safe_command_new() {
        // Arrange
        let cmd = "cargo";

        // Act
        let result = SafeCommand::new(cmd);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_command_new_invalid() {
        // Arrange
        let cmd = "rm";

        // Act
        let result = SafeCommand::new(cmd);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not in whitelist"));
    }

    #[test]
    fn test_safe_command_single_arg() {
        // Arrange & Act
        let result = SafeCommand::new("cargo").unwrap().arg("build");

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_command_multiple_args() {
        // Arrange & Act
        let result = SafeCommand::new("cargo")
            .unwrap()
            .arg("build")
            .unwrap()
            .arg("--release")
            .unwrap()
            .arg("--all-features");

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_command_args_bulk() {
        // Arrange & Act
        let result =
            SafeCommand::new("cargo")
                .unwrap()
                .args(&["build", "--release", "--all-features"]);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_command_arg_path() {
        // Arrange
        let path = SafePath::new("src/generated").unwrap();

        // Act
        let cmd = SafeCommand::new("cargo")
            .unwrap()
            .arg("build")
            .unwrap()
            .arg_path(&path);

        // Assert - arg_path is infallible (uses already-validated SafePath)
        let validated = cmd.validate();
        assert!(validated.is_ok());
    }

    #[test]
    fn test_safe_command_validate_success() {
        // Arrange
        let cmd = SafeCommand::new("cargo")
            .unwrap()
            .arg("build")
            .unwrap()
            .arg("--release")
            .unwrap();

        // Act
        let result = cmd.validate();

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_command_into_command() {
        // Arrange
        let safe_cmd = SafeCommand::new("cargo")
            .unwrap()
            .arg("--version")
            .unwrap()
            .validate()
            .unwrap();

        // Act
        let process_cmd = safe_cmd.into_command();

        // Assert - verify it creates a valid Command
        // We can't easily test execution here, but we can verify it compiles
        let _ = process_cmd;
    }

    #[test]
    fn test_safe_command_to_string_debug() {
        // Arrange
        let cmd = SafeCommand::new("cargo")
            .unwrap()
            .arg("build")
            .unwrap()
            .arg("--release")
            .unwrap()
            .validate()
            .unwrap();

        // Act
        let debug_string = cmd.to_string_debug();

        // Assert
        assert_eq!(debug_string, "cargo build --release");
    }

    #[test]
    fn test_safe_command_display() {
        // Arrange
        let cmd = SafeCommand::new("git")
            .unwrap()
            .arg("status")
            .unwrap()
            .validate()
            .unwrap();

        // Act
        let display = format!("{}", cmd);

        // Assert
        assert_eq!(display, "git status");
    }

    // ============================================================================
    // SafeCommand Security Tests
    // ============================================================================

    #[test]
    fn test_safe_command_injection_in_arg() {
        // Arrange & Act
        let result = SafeCommand::new("cargo").unwrap().arg("build; rm -rf /");

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("metacharacter"));
    }

    #[test]
    fn test_safe_command_injection_in_multiple_args() {
        // Arrange & Act
        let result = SafeCommand::new("cargo")
            .unwrap()
            .arg("build")
            .unwrap()
            .arg("--release && rm -rf /");

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_safe_command_max_length() {
        // Arrange - create a command that exceeds MAX_COMMAND_LENGTH
        let long_arg = "a".repeat(MAX_COMMAND_LENGTH);

        // Act
        let result = SafeCommand::new("cargo")
            .unwrap()
            .arg(&long_arg)
            .unwrap()
            .validate();

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("exceeds maximum"));
    }

    #[test]
    fn test_safe_command_max_length_boundary() {
        // Arrange - create a command at exactly MAX_COMMAND_LENGTH
        // "cargo" = 5 chars
        // " " = 1 char
        // So we need MAX_COMMAND_LENGTH - 6 chars for the arg
        let arg_length = MAX_COMMAND_LENGTH - 6;
        let exact_arg = "a".repeat(arg_length);

        // Act
        let result = SafeCommand::new("cargo")
            .unwrap()
            .arg(&exact_arg)
            .unwrap()
            .validate();

        // Assert - should succeed at exact boundary
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_command_command_and_args_accessors() {
        // Arrange
        let cmd = SafeCommand::new("cargo")
            .unwrap()
            .arg("build")
            .unwrap()
            .arg("--release")
            .unwrap()
            .validate()
            .unwrap();

        // Act
        let command = cmd.command();
        let args = cmd.args();

        // Assert
        assert_eq!(command.as_str(), "cargo");
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].as_str(), "build");
        assert_eq!(args[1].as_str(), "--release");
    }

    // ============================================================================
    // Integration with SafePath Tests
    // ============================================================================

    #[test]
    fn test_safe_command_with_safe_path() {
        // Arrange
        let path = SafePath::new("src/generated/output.rs").unwrap();

        // Act
        let cmd = SafeCommand::new("rustfmt")
            .unwrap()
            .arg_path(&path)
            .validate()
            .unwrap();

        // Assert
        let debug_string = cmd.to_string_debug();
        assert!(debug_string.contains("src/generated/output.rs"));
    }

    #[test]
    fn test_safe_command_multiple_paths() {
        // Arrange
        let path1 = SafePath::new("src/main.rs").unwrap();
        let path2 = SafePath::new("src/lib.rs").unwrap();

        // Act
        let cmd = SafeCommand::new("rustfmt")
            .unwrap()
            .arg_path(&path1)
            .arg_path(&path2)
            .validate()
            .unwrap();

        // Assert
        let debug_string = cmd.to_string_debug();
        assert!(debug_string.contains("src/main.rs"));
        assert!(debug_string.contains("src/lib.rs"));
    }

    // ============================================================================
    // Edge Cases
    // ============================================================================

    #[test]
    fn test_safe_command_no_args() {
        // Arrange & Act
        let result = SafeCommand::new("git").unwrap().validate();

        // Assert - command with no args should be valid
        assert!(result.is_ok());
    }

    #[test]
    fn test_safe_command_many_small_args() {
        // Arrange - create many small args
        let mut cmd = SafeCommand::new("cargo").unwrap();

        for i in 0..100 {
            cmd = cmd.arg(&format!("arg{}", i)).unwrap();
        }

        // Act
        let result = cmd.validate();

        // Assert - should succeed if under max length
        assert!(result.is_ok());
    }

    #[test]
    fn test_command_name_clone() {
        // Arrange
        let name = CommandName::new("cargo").unwrap();

        // Act
        let cloned = name.clone();

        // Assert
        assert_eq!(name, cloned);
    }

    #[test]
    fn test_command_arg_clone() {
        // Arrange
        let arg = CommandArg::new("build").unwrap();

        // Act
        let cloned = arg.clone();

        // Assert
        assert_eq!(arg, cloned);
    }

    #[test]
    fn test_safe_command_clone() {
        // Arrange
        let cmd = SafeCommand::new("cargo")
            .unwrap()
            .arg("build")
            .unwrap()
            .validate()
            .unwrap();

        // Act
        let cloned = cmd.clone();

        // Assert
        assert_eq!(cmd.to_string_debug(), cloned.to_string_debug());
    }
}
