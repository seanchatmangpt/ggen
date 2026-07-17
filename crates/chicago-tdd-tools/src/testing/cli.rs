//! CLI Testing Framework
//!
//! Provides comprehensive CLI testing capabilities using trycmd for Chicago TDD.
//! Trycmd enables golden file testing for CLI tools, ensuring command output
//! stability and correctness.
//!
//! # Chicago TDD Alignment
//!
//! CLI testing aligns with Chicago TDD principles:
//! - **State-Based Testing**: Verifies CLI command outputs
//! - **Behavior Verification**: Tests what CLI commands produce, not how
//! - **AAA Pattern**: Arrange (setup), Act (run command), Assert (verify output)
//!
//! # New in v1.2.0: Enhanced CLI Testing Helpers
//!
//! - `CliCommandBuilder`: Fluent API for building CLI commands
//! - `CliAssertions`: Output verification helpers
//! - `CliEnvironment`: Environment setup for tests
//! - `CliTestScenario`: Complete test scenario builder

#[cfg(feature = "cli-testing")]
use std::collections::HashMap;
#[cfg(feature = "cli-testing")]
use trycmd::TestCases;

/// CLI test helper for Chicago TDD
///
/// Provides a Chicago TDD-friendly wrapper around trycmd's CLI testing.
/// This makes CLI testing consistent with other testing utilities in the framework.
#[cfg(feature = "cli-testing")]
pub struct CliTest;

/// Builder for CLI commands with fluent API
///
/// Enables clean, readable CLI command construction with validation.
///
/// # Example
///
/// ```rust
/// # #[cfg(feature = "cli-testing")]
/// use chicago_tdd_tools::cli::CliCommandBuilder;
///
/// # #[cfg(feature = "cli-testing")]
/// let cmd = CliCommandBuilder::new("my-cli")
///     .arg("init")
///     .arg("--config=app.toml")
///     .env("RUST_LOG", "debug")
///     .build();
/// ```
#[cfg(feature = "cli-testing")]
#[derive(Debug, Clone)]
pub struct CliCommandBuilder {
    binary: String,
    args: Vec<String>,
    env: HashMap<String, String>,
}

#[cfg(feature = "cli-testing")]
impl CliCommandBuilder {
    /// Create a new CLI command builder
    ///
    /// # Arguments
    ///
    /// * `binary` - Name or path of the binary to run
    #[must_use]
    pub fn new(binary: &str) -> Self {
        Self { binary: binary.to_string(), args: Vec::new(), env: HashMap::new() }
    }

    /// Add an argument to the command
    #[must_use]
    pub fn arg(mut self, arg: &str) -> Self {
        self.args.push(arg.to_string());
        self
    }

    /// Add multiple arguments
    #[must_use]
    pub fn args(mut self, args: &[&str]) -> Self {
        self.args.extend(args.iter().map(ToString::to_string));
        self
    }

    /// Set an environment variable
    #[must_use]
    pub fn env(mut self, key: &str, value: &str) -> Self {
        self.env.insert(key.to_string(), value.to_string());
        self
    }

    /// Get command string representation
    #[must_use]
    pub fn build(&self) -> String {
        let mut cmd = self.binary.clone();
        for arg in &self.args {
            cmd.push(' ');
            cmd.push_str(arg);
        }
        cmd
    }

    /// Get environment variables
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // HashMap is not const-constructible
    pub fn env_vars(&self) -> &HashMap<String, String> {
        &self.env
    }
}

/// CLI assertion helpers for output verification
///
/// Provides common assertions for CLI testing.
#[cfg(feature = "cli-testing")]
pub struct CliAssertions;

#[cfg(feature = "cli-testing")]
impl CliAssertions {
    /// Assert output contains substring
    ///
    /// # Panics
    ///
    /// Panics if output does not contain the expected string
    pub fn assert_output_contains(output: &str, expected: &str) {
        assert!(
            output.contains(expected),
            "Output does not contain '{expected}'. Output: {output}"
        );
    }

    /// Assert output does not contain substring
    ///
    /// # Panics
    ///
    /// Panics if output contains the unexpected string
    pub fn assert_output_not_contains(output: &str, unexpected: &str) {
        assert!(
            !output.contains(unexpected),
            "Output contains unexpected '{unexpected}'.  Output: {output}"
        );
    }

    /// Assert output starts with prefix
    ///
    /// # Panics
    ///
    /// Panics if output does not start with the expected prefix
    pub fn assert_output_starts_with(output: &str, prefix: &str) {
        assert!(
            output.starts_with(prefix),
            "Output does not start with '{prefix}'. Output: {output}"
        );
    }

    /// Assert output ends with suffix
    ///
    /// # Panics
    ///
    /// Panics if output does not end with the expected suffix
    pub fn assert_output_ends_with(output: &str, suffix: &str) {
        assert!(output.ends_with(suffix), "Output does not end with '{suffix}'. Output: {output}");
    }

    /// Assert output lines contain all of the given strings
    ///
    /// # Panics
    ///
    /// Panics if any expected string is not found in output lines
    pub fn assert_output_contains_all(output: &str, expected_lines: &[&str]) {
        for expected in expected_lines {
            assert!(
                output.contains(expected),
                "Output does not contain line '{expected}'. Output: {output}"
            );
        }
    }

    /// Assert exit code is success (v1.3.0)
    ///
    /// # Panics
    ///
    /// Panics if exit code is not 0
    pub fn assert_success(exit_code: i32) {
        assert_eq!(exit_code, 0, "Expected success (exit code 0), got {exit_code}");
    }

    /// Assert exit code is failure (v1.3.0)
    ///
    /// # Panics
    ///
    /// Panics if exit code is 0
    pub fn assert_failure(exit_code: i32) {
        assert_ne!(exit_code, 0, "Expected failure (non-zero exit code), got {exit_code}");
    }

    /// Assert specific exit code (v1.3.0)
    ///
    /// # Panics
    ///
    /// Panics if exit code doesn't match expected
    pub fn assert_exit_code(actual: i32, expected: i32) {
        assert_eq!(actual, expected, "Expected exit code {expected}, got {actual}");
    }

    /// Assert output matches help pattern (v1.3.0)
    ///
    /// Commonly requested for CLI help validation.
    ///
    /// # Panics
    ///
    /// Panics if output doesn't look like help text
    pub fn assert_is_help(output: &str) {
        let help_indicators =
            ["Usage:", "USAGE:", "Options:", "OPTIONS:", "Commands:", "COMMANDS:"];
        let contains_help = help_indicators.iter().any(|indicator| output.contains(indicator));
        assert!(contains_help, "Output does not appear to be help text. Output: {output}");
    }

    /// Assert output matches version pattern (v1.3.0)
    ///
    /// Commonly requested for CLI version validation.
    ///
    /// # Panics
    ///
    /// Panics if output doesn't contain version information
    pub fn assert_is_version(output: &str) {
        // Check for common version patterns: "1.0.0", "v1.0.0", "version 1.0.0"
        let has_version = output.contains(char::is_numeric)
            && (output.contains('.') || output.to_lowercase().contains("version"));
        assert!(has_version, "Output does not appear to contain version info. Output: {output}");
    }

    /// Assert output is empty (v1.3.0)
    ///
    /// # Panics
    ///
    /// Panics if output is not empty
    pub fn assert_empty(output: &str) {
        assert!(output.trim().is_empty(), "Expected empty output, got: {output}");
    }

    /// Assert stderr is empty (v1.3.0)
    ///
    /// Commonly used to ensure no errors/warnings.
    ///
    /// # Panics
    ///
    /// Panics if stderr is not empty
    pub fn assert_stderr_empty(stderr: &str) {
        assert!(
            stderr.trim().is_empty(),
            "Expected empty stderr (no errors/warnings), got: {stderr}"
        );
    }
}

/// Environment setup for CLI tests
///
/// Manages environment variables for isolated test runs.
#[cfg(feature = "cli-testing")]
pub struct CliEnvironment {
    vars: HashMap<String, String>,
    original_vars: HashMap<String, Option<String>>,
}

#[cfg(feature = "cli-testing")]
impl CliEnvironment {
    /// Create a new CLI environment
    #[must_use]
    pub fn new() -> Self {
        Self { vars: HashMap::new(), original_vars: HashMap::new() }
    }

    /// Set an environment variable
    #[must_use]
    pub fn set(mut self, key: &str, value: &str) -> Self {
        self.vars.insert(key.to_string(), value.to_string());
        self
    }

    /// Create environment for CI testing (v1.3.0)
    ///
    /// Sets common CI environment variables.
    #[must_use]
    pub fn ci() -> Self {
        Self::new().set("CI", "true").set("TERM", "dumb").set("NO_COLOR", "1")
    }

    /// Create environment for development (v1.3.0)
    ///
    /// Sets common development environment variables.
    #[must_use]
    pub fn development() -> Self {
        Self::new().set("RUST_LOG", "debug").set("RUST_BACKTRACE", "1")
    }

    /// Create environment for production (v1.3.0)
    ///
    /// Sets common production environment variables.
    #[must_use]
    pub fn production() -> Self {
        Self::new().set("RUST_LOG", "info").set("RUST_BACKTRACE", "0")
    }

    /// Create clean environment (v1.3.0)
    ///
    /// Removes common variables that might affect tests.
    #[must_use]
    pub fn clean() -> Self {
        let mut env = Self::new();
        // Clear common variables that might interfere with tests
        env.vars.insert("HOME".to_string(), "/tmp/test-home".to_string());
        env.vars.insert("USER".to_string(), "test-user".to_string());
        env.vars.insert("PATH".to_string(), "/usr/bin:/bin".to_string());
        env
    }

    /// Set multiple environment variables at once (v1.3.0)
    #[must_use]
    pub fn with_vars(mut self, vars: &[(&str, &str)]) -> Self {
        for (key, value) in vars {
            self.vars.insert((*key).to_string(), (*value).to_string());
        }
        self
    }

    /// Apply environment variables
    pub fn apply(&mut self) {
        for (key, value) in &self.vars {
            self.original_vars.insert(key.clone(), std::env::var(key).ok());
            std::env::set_var(key, value);
        }
    }

    /// Restore original environment
    pub fn restore(&self) {
        for (key, original) in &self.original_vars {
            match original {
                Some(value) => std::env::set_var(key, value),
                None => std::env::remove_var(key),
            }
        }
    }
}

#[cfg(feature = "cli-testing")]
impl Default for CliEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "cli-testing")]
impl Drop for CliEnvironment {
    fn drop(&mut self) {
        self.restore();
    }
}

#[cfg(feature = "cli-testing")]
impl CliTest {
    /// Run CLI tests from a directory
    ///
    /// # Arguments
    ///
    /// * `test_dir` - Directory containing test cases (`.trycmd` files)
    ///
    /// # Panics
    ///
    /// Panics if any CLI test fails.
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "cli-testing")]
    /// use chicago_tdd_tools::cli::CliTest;
    ///
    /// # #[cfg(feature = "cli-testing")]
    /// # // CliTest::run_tests("tests/cli");
    /// ```
    pub fn run_tests(test_dir: &str) {
        TestCases::new().case(test_dir).run();
    }

    /// Run a single CLI test case
    ///
    /// # Arguments
    ///
    /// * `test_file` - Path to a `.trycmd` test file
    ///
    /// # Panics
    ///
    /// Panics if the CLI test fails.
    pub fn run_test(test_file: &str) {
        TestCases::new().case(test_file).run();
    }

    /// Run CLI tests with custom binary path
    ///
    /// # Arguments
    ///
    /// * `test_dir` - Directory containing test cases
    /// * `bin_path` - Path to the binary to test
    ///
    /// # Panics
    ///
    /// Panics if any CLI test fails.
    ///
    /// Note: Set the binary path via environment variable BIN or in .trycmd files
    pub fn run_tests_with_bin(test_dir: &str, bin_path: &str) {
        std::env::set_var("BIN", bin_path);
        TestCases::new().case(test_dir).run();
    }

    /// Create a new CLI command builder
    ///
    /// # Example
    ///
    /// ```rust
    /// # #[cfg(feature = "cli-testing")]
    /// use chicago_tdd_tools::cli::CliTest;
    ///
    /// # #[cfg(feature = "cli-testing")]
    /// let cmd = CliTest::command("my-cli")
    ///     .arg("--help")
    ///     .build();
    /// ```
    #[must_use]
    pub fn command(binary: &str) -> CliCommandBuilder {
        CliCommandBuilder::new(binary)
    }
}

#[cfg(feature = "cli-testing")]
#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::{CliAssertions, CliCommandBuilder, CliEnvironment, CliTest};

    #[test]
    fn test_cli_test_struct_available() {
        // Arrange: Verify CliTest struct is available
        // Act: Create a reference (compile-time check)
        let _test = CliTest;
        // Assert: Struct compiles and is available
        assert!(true);
    }

    #[test]
    fn test_cli_command_builder_creation() {
        // Arrange: Create a command builder
        let cmd = CliCommandBuilder::new("test-cli");
        // Act: Verify builder was created
        let command_str = cmd.build();
        // Assert: Command string is correct
        assert_eq!(command_str, "test-cli");
    }

    #[test]
    fn test_cli_command_builder_with_args() {
        // Arrange: Create builder and add arguments
        let cmd = CliCommandBuilder::new("test-cli").arg("init").arg("--config=app.toml");
        // Act: Build command
        let command_str = cmd.build();
        // Assert: Command contains all arguments
        assert!(command_str.contains("init"));
        assert!(command_str.contains("--config=app.toml"));
    }

    #[test]
    fn test_cli_command_builder_with_multiple_args() {
        // Arrange: Create builder with multiple args
        let cmd = CliCommandBuilder::new("test-cli").args(&["--verbose", "--debug", "--help"]);
        // Act: Build command
        let command_str = cmd.build();
        // Assert: All arguments are present
        assert!(command_str.contains("--verbose"));
        assert!(command_str.contains("--debug"));
        assert!(command_str.contains("--help"));
    }

    #[test]
    fn test_cli_command_builder_with_env() {
        // Arrange: Create builder with environment variables
        let cmd = CliCommandBuilder::new("test-cli").env("RUST_LOG", "debug").env("MODE", "test");
        // Act: Get environment variables
        let env_vars = cmd.env_vars();
        // Assert: Environment variables are stored
        assert_eq!(env_vars.get("RUST_LOG"), Some(&"debug".to_string()));
        assert_eq!(env_vars.get("MODE"), Some(&"test".to_string()));
    }

    #[test]
    fn test_cli_assertions_contains() {
        // Arrange: Create output
        let output = "Hello, World!\nTests passed!";
        // Act & Assert: Test contains assertion
        CliAssertions::assert_output_contains(output, "Hello");
        CliAssertions::assert_output_contains(output, "Tests passed");
    }

    #[test]
    #[should_panic(expected = "does not contain")]
    fn test_cli_assertions_contains_fails() {
        // Arrange: Create output
        let output = "Hello, World!";
        // Act & Assert: Test that missing content triggers panic
        CliAssertions::assert_output_contains(output, "Goodbye");
    }

    #[test]
    fn test_cli_assertions_not_contains() {
        // Arrange: Create output
        let output = "Hello, World!";
        // Act & Assert: Test not contains assertion
        CliAssertions::assert_output_not_contains(output, "Goodbye");
    }

    #[test]
    #[should_panic(expected = "contains unexpected")]
    fn test_cli_assertions_not_contains_fails() {
        // Arrange: Create output
        let output = "Hello, World!";
        // Act & Assert: Test that present content triggers panic
        CliAssertions::assert_output_not_contains(output, "Hello");
    }

    #[test]
    fn test_cli_assertions_starts_with() {
        // Arrange: Create output
        let output = "SUCCESS: All tests passed";
        // Act & Assert: Test starts with assertion
        CliAssertions::assert_output_starts_with(output, "SUCCESS");
    }

    #[test]
    #[should_panic(expected = "does not start with")]
    fn test_cli_assertions_starts_with_fails() {
        // Arrange: Create output
        let output = "SUCCESS: All tests passed";
        // Act & Assert: Test that different prefix triggers panic
        CliAssertions::assert_output_starts_with(output, "FAILURE");
    }

    #[test]
    fn test_cli_assertions_ends_with() {
        // Arrange: Create output
        let output = "All tests passed successfully";
        // Act & Assert: Test ends with assertion
        CliAssertions::assert_output_ends_with(output, "successfully");
    }

    #[test]
    #[should_panic(expected = "does not end with")]
    fn test_cli_assertions_ends_with_fails() {
        // Arrange: Create output
        let output = "All tests passed successfully";
        // Act & Assert: Test that different suffix triggers panic
        CliAssertions::assert_output_ends_with(output, "failed");
    }

    #[test]
    fn test_cli_assertions_contains_all() {
        // Arrange: Create output with multiple lines
        let output = "Test 1: PASS\nTest 2: PASS\nTest 3: PASS";
        // Act & Assert: Test contains all assertion
        CliAssertions::assert_output_contains_all(output, &["Test 1", "Test 2", "Test 3", "PASS"]);
    }

    #[test]
    #[should_panic(expected = "does not contain")]
    fn test_cli_assertions_contains_all_fails() {
        // Arrange: Create output
        let output = "Test 1: PASS\nTest 2: PASS";
        // Act & Assert: Test that missing line triggers panic
        CliAssertions::assert_output_contains_all(output, &["Test 1", "Test 3"]);
    }

    #[test]
    fn test_cli_environment_creation() {
        // Arrange: Create environment
        let env = CliEnvironment::new();
        // Act: Verify creation
        let env_default = CliEnvironment::default();
        // Assert: Both methods work
        assert_eq!(std::mem::size_of_val(&env), std::mem::size_of_val(&env_default));
    }

    #[test]
    fn test_cli_environment_set_and_restore() {
        // Arrange: Create environment and set variables
        let mut env = CliEnvironment::new().set("TEST_VAR", "test_value");
        // Act: Apply environment
        env.apply();
        // Assert: Variable is set
        assert_eq!(std::env::var("TEST_VAR").ok(), Some("test_value".to_string()));
        // Cleanup: Restore happens in drop
        drop(env);
    }

    #[test]
    fn test_cli_test_command_method() {
        // Arrange: Use CliTest::command method
        let cmd = CliTest::command("my-tool").arg("--version").build();
        // Act & Assert: Command is built correctly
        assert!(cmd.contains("my-tool"));
        assert!(cmd.contains("--version"));
    }

    // === v1.3.0 Phase 5: CLI Environment Helpers Tests ===

    #[test]
    fn test_assert_success_with_zero_exit_code() {
        // Arrange: Exit code 0 (success)
        let exit_code = 0;
        // Act & Assert: Should not panic
        CliAssertions::assert_success(exit_code);
    }

    #[test]
    #[should_panic(expected = "Expected success")]
    fn test_assert_success_fails_with_nonzero() {
        // Arrange: Exit code 1 (failure)
        let exit_code = 1;
        // Act & Assert: Should panic
        CliAssertions::assert_success(exit_code);
    }

    #[test]
    fn test_assert_failure_with_nonzero_exit_code() {
        // Arrange: Non-zero exit codes
        // Act & Assert: Should not panic
        CliAssertions::assert_failure(1);
        CliAssertions::assert_failure(127);
        CliAssertions::assert_failure(-1);
    }

    #[test]
    #[should_panic(expected = "Expected failure")]
    fn test_assert_failure_fails_with_zero() {
        // Arrange: Exit code 0
        let exit_code = 0;
        // Act & Assert: Should panic
        CliAssertions::assert_failure(exit_code);
    }

    #[test]
    fn test_assert_exit_code_matches() {
        // Arrange: Specific exit codes
        // Act & Assert: Should not panic
        CliAssertions::assert_exit_code(0, 0);
        CliAssertions::assert_exit_code(1, 1);
        CliAssertions::assert_exit_code(127, 127);
    }

    #[test]
    #[should_panic(expected = "Expected exit code")]
    fn test_assert_exit_code_mismatch() {
        // Arrange: Mismatched exit codes
        // Act & Assert: Should panic
        CliAssertions::assert_exit_code(0, 1);
    }

    #[test]
    fn test_assert_is_help_with_usage() {
        // Arrange: Help text with "Usage:"
        let output = "Usage: my-tool [OPTIONS]\n\nOptions:\n  --help    Display help";
        // Act & Assert: Should recognize as help text
        CliAssertions::assert_is_help(output);
    }

    #[test]
    fn test_assert_is_help_with_options() {
        // Arrange: Help text with "OPTIONS:"
        let output = "my-tool\n\nOPTIONS:\n  -v, --verbose    Verbose output";
        // Act & Assert: Should recognize as help text
        CliAssertions::assert_is_help(output);
    }

    #[test]
    fn test_assert_is_help_with_commands() {
        // Arrange: Help text with "Commands:"
        let output = "Commands:\n  init    Initialize project\n  build   Build project";
        // Act & Assert: Should recognize as help text
        CliAssertions::assert_is_help(output);
    }

    #[test]
    #[should_panic(expected = "does not appear to be help text")]
    fn test_assert_is_help_fails_without_indicators() {
        // Arrange: Regular output without help indicators
        let output = "Hello, world!";
        // Act & Assert: Should panic
        CliAssertions::assert_is_help(output);
    }

    #[test]
    fn test_assert_is_version_with_semantic_version() {
        // Arrange: Version output with semantic version
        let output = "my-tool 1.2.3";
        // Act & Assert: Should recognize as version
        CliAssertions::assert_is_version(output);
    }

    #[test]
    fn test_assert_is_version_with_version_keyword() {
        // Arrange: Version output with "version" keyword
        let output = "Version: 2.0.0-beta";
        // Act & Assert: Should recognize as version
        CliAssertions::assert_is_version(output);
    }

    #[test]
    fn test_assert_is_version_with_numbers() {
        // Arrange: Version with just numbers
        let output = "1.0.0";
        // Act & Assert: Should recognize as version
        CliAssertions::assert_is_version(output);
    }

    #[test]
    #[should_panic(expected = "does not appear to contain version info")]
    fn test_assert_is_version_fails_without_version() {
        // Arrange: Output without version info
        let output = "Hello, world!";
        // Act & Assert: Should panic
        CliAssertions::assert_is_version(output);
    }

    #[test]
    fn test_assert_empty_with_empty_string() {
        // Arrange: Empty output
        let output = "";
        // Act & Assert: Should not panic
        CliAssertions::assert_empty(output);
    }

    #[test]
    fn test_assert_empty_with_whitespace_only() {
        // Arrange: Whitespace-only output
        let output = "   \n\t  ";
        // Act & Assert: Should not panic (whitespace trimmed)
        CliAssertions::assert_empty(output);
    }

    #[test]
    #[should_panic(expected = "Expected empty output")]
    fn test_assert_empty_fails_with_content() {
        // Arrange: Non-empty output
        let output = "Some content";
        // Act & Assert: Should panic
        CliAssertions::assert_empty(output);
    }

    #[test]
    fn test_assert_stderr_empty_with_empty_stderr() {
        // Arrange: Empty stderr
        let stderr = "";
        // Act & Assert: Should not panic
        CliAssertions::assert_stderr_empty(stderr);
    }

    #[test]
    #[should_panic(expected = "Expected empty stderr")]
    fn test_assert_stderr_empty_fails_with_errors() {
        // Arrange: Stderr with error message
        let stderr = "ERROR: Something went wrong";
        // Act & Assert: Should panic
        CliAssertions::assert_stderr_empty(stderr);
    }

    #[test]
    fn test_environment_preset_ci() {
        // Arrange: Create CI environment
        let env = CliEnvironment::ci();
        // Act: Get environment variables
        let vars = &env.vars;
        // Assert: CI-specific variables are set
        assert_eq!(vars.get("CI"), Some(&"true".to_string()));
        assert_eq!(vars.get("TERM"), Some(&"dumb".to_string()));
        assert_eq!(vars.get("NO_COLOR"), Some(&"1".to_string()));
    }

    #[test]
    fn test_environment_preset_development() {
        // Arrange: Create development environment
        let env = CliEnvironment::development();
        // Act: Get environment variables
        let vars = &env.vars;
        // Assert: Development-specific variables are set
        assert_eq!(vars.get("RUST_LOG"), Some(&"debug".to_string()));
        assert_eq!(vars.get("RUST_BACKTRACE"), Some(&"1".to_string()));
    }

    #[test]
    fn test_environment_preset_production() {
        // Arrange: Create production environment
        let env = CliEnvironment::production();
        // Act: Get environment variables
        let vars = &env.vars;
        // Assert: Production-specific variables are set
        assert_eq!(vars.get("RUST_LOG"), Some(&"info".to_string()));
        assert_eq!(vars.get("RUST_BACKTRACE"), Some(&"0".to_string()));
    }

    #[test]
    fn test_environment_preset_clean() {
        // Arrange: Create clean environment
        let env = CliEnvironment::clean();
        // Act: Get environment variables
        let vars = &env.vars;
        // Assert: Clean environment has isolated variables
        assert_eq!(vars.get("HOME"), Some(&"/tmp/test-home".to_string()));
        assert_eq!(vars.get("USER"), Some(&"test-user".to_string()));
        assert_eq!(vars.get("PATH"), Some(&"/usr/bin:/bin".to_string()));
    }

    #[test]
    fn test_environment_with_vars_bulk_set() {
        // Arrange: Create environment and set multiple vars at once
        let vars_to_set = [("VAR1", "value1"), ("VAR2", "value2"), ("VAR3", "value3")];
        let env = CliEnvironment::new().with_vars(&vars_to_set);
        // Act: Get environment variables
        let vars = &env.vars;
        // Assert: All variables are set
        assert_eq!(vars.get("VAR1"), Some(&"value1".to_string()));
        assert_eq!(vars.get("VAR2"), Some(&"value2".to_string()));
        assert_eq!(vars.get("VAR3"), Some(&"value3".to_string()));
    }

    #[test]
    fn test_environment_preset_chaining() {
        // Arrange: Chain preset with additional vars
        let env = CliEnvironment::ci().set("CUSTOM_VAR", "custom_value");
        // Act: Get environment variables
        let vars = &env.vars;
        // Assert: Both preset and custom variables are set
        assert_eq!(vars.get("CI"), Some(&"true".to_string()));
        assert_eq!(vars.get("CUSTOM_VAR"), Some(&"custom_value".to_string()));
    }

    #[test]
    fn test_environment_with_vars_chaining() {
        // Arrange: Chain with_vars with additional set calls
        let env = CliEnvironment::new().with_vars(&[("A", "1"), ("B", "2")]).set("C", "3");
        // Act: Get environment variables
        let vars = &env.vars;
        // Assert: All variables are set
        assert_eq!(vars.get("A"), Some(&"1".to_string()));
        assert_eq!(vars.get("B"), Some(&"2".to_string()));
        assert_eq!(vars.get("C"), Some(&"3".to_string()));
    }
}
