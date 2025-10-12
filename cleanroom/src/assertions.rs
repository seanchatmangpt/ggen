//! Fluent assertions for scenario results
//!
//! Provides a composable assertion API for validating command outputs,
//! exit codes, and execution behavior.

use crate::scenario::RunResult;

/// Fluent assertion trait for RunResult
pub trait Assert {
    /// Assert that the command succeeded (exit code 0)
    fn success(&self) -> &Self;

    /// Assert that the command failed (non-zero exit code)
    fn failure(&self) -> &Self;

    /// Assert a specific exit code
    fn exit(&self, code: i32) -> &Self;

    /// Assert that stdout contains a substring
    fn stdout(&self, expected: &str) -> &Self;

    /// Assert that stderr contains a substring
    fn stderr(&self, expected: &str) -> &Self;

    /// Assert that stdout matches a regex pattern
    fn stdout_regex(&self, pattern: &str) -> &Self;

    /// Assert that stderr matches a regex pattern
    fn stderr_regex(&self, pattern: &str) -> &Self;

    /// Assert that stdout is valid JSON
    fn stdout_json(&self) -> &Self;

    /// Assert that stderr is valid JSON
    fn stderr_json(&self) -> &Self;

    /// Assert execution duration is within limit (milliseconds)
    fn duration_le(&self, max_ms: u64) -> &Self;

    /// Assert that output size doesn't exceed limit (bytes)
    fn no_output_over(&self, max_bytes: usize) -> &Self;
}

impl Assert for RunResult {
    fn success(&self) -> &Self {
        assert_eq!(self.exit_code, 0, "Expected success (exit code 0), got {}", self.exit_code);
        self
    }

    fn failure(&self) -> &Self {
        assert_ne!(self.exit_code, 0, "Expected failure (non-zero exit code), got {}", self.exit_code);
        self
    }

    fn exit(&self, code: i32) -> &Self {
        assert_eq!(self.exit_code, code, "Expected exit code {}, got {}", code, self.exit_code);
        self
    }

    fn stdout(&self, expected: &str) -> &Self {
        assert!(self.stdout.contains(expected), "Expected stdout to contain '{}', got: {}", expected, self.stdout);
        self
    }

    fn stderr(&self, expected: &str) -> &Self {
        assert!(self.stderr.contains(expected), "Expected stderr to contain '{}', got: {}", expected, self.stderr);
        self
    }

    fn stdout_regex(&self, pattern: &str) -> &Self {
        let regex = regex::Regex::new(pattern).expect("Invalid regex pattern");
        assert!(regex.is_match(&self.stdout), "Expected stdout to match pattern '{}', got: {}", pattern, self.stdout);
        self
    }

    fn stderr_regex(&self, pattern: &str) -> &Self {
        let regex = regex::Regex::new(pattern).expect("Invalid regex pattern");
        assert!(regex.is_match(&self.stderr), "Expected stderr to match pattern '{}', got: {}", pattern, self.stderr);
        self
    }

    fn stdout_json(&self) -> &Self {
        serde_json::from_str::<serde_json::Value>(&self.stdout)
            .expect("Expected stdout to be valid JSON");
        self
    }

    fn stderr_json(&self) -> &Self {
        serde_json::from_str::<serde_json::Value>(&self.stderr)
            .expect("Expected stderr to be valid JSON");
        self
    }

    fn duration_le(&self, max_ms: u64) -> &Self {
        assert!(self.duration_ms <= max_ms, "Expected duration <= {}ms, got {}ms", max_ms, self.duration_ms);
        self
    }

    fn no_output_over(&self, max_bytes: usize) -> &Self {
        assert!(self.stdout.len() + self.stderr.len() <= max_bytes,
               "Expected total output <= {} bytes, got {} bytes", max_bytes, self.stdout.len() + self.stderr.len());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scenario::RunResult;

    fn mock_success_result() -> RunResult {
        RunResult {
            exit_code: 0,
            stdout: "hello world".to_string(),
            stderr: "".to_string(),
            duration_ms: 100,
            steps: vec![],
            redacted_env: vec![],
            backend: "test".to_string(),
        }
    }

    fn mock_failure_result() -> RunResult {
        RunResult {
            exit_code: 1,
            stdout: "".to_string(),
            stderr: "error occurred".to_string(),
            duration_ms: 50,
            steps: vec![],
            redacted_env: vec![],
            backend: "test".to_string(),
        }
    }

    #[test]
    fn test_assert_success() {
        let result = mock_success_result();
        result.success(); // Should not panic
    }

    #[test]
    #[should_panic]
    fn test_assert_success_fails() {
        let result = mock_failure_result();
        result.success(); // Should panic
    }

    #[test]
    fn test_assert_failure() {
        let result = mock_failure_result();
        result.failure(); // Should not panic
    }

    #[test]
    #[should_panic]
    fn test_assert_failure_fails() {
        let result = mock_success_result();
        result.failure(); // Should panic
    }

    #[test]
    fn test_assert_exit() {
        let result = mock_success_result();
        result.exit(0); // Should not panic
    }

    #[test]
    fn test_assert_stdout() {
        let result = mock_success_result();
        result.stdout("hello"); // Should not panic
    }

    #[test]
    fn test_assert_stderr() {
        let result = mock_failure_result();
        result.stderr("error"); // Should not panic
    }

    #[test]
    fn test_assert_duration() {
        let result = mock_success_result();
        result.duration_le(200); // Should not panic
    }
}
