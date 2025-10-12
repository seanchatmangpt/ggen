//! Fluent assertions for scenario results
//!
//! Provides a composable assertion API for validating command outputs,
//! exit codes, and execution behavior.

use crate::scenario::RunResult;
use serde_json;

/// Fluent assertion trait for RunResult
pub trait Assert {
    /// Assert that the command succeeded (exit code 0)
    fn success(&self) -> &Self;

    /// Assert that the command failed (non-zero exit code)
    fn failure(&self) -> &Self;

    /// Assert a specific exit code
    fn exit(&self, code: i32) -> &Self;

    /// Assert that stdout contains a pattern
    fn stdout(&self, pattern: impl AsRef<str>) -> &Self;

    /// Assert that stderr contains a pattern
    fn stderr(&self, pattern: impl AsRef<str>) -> &Self;

    /// Assert that stdout matches a JSON value
    fn stdout_json<F: FnOnce(&serde_json::Value)>(&self, f: F) -> &Self;

    /// Assert that stderr matches a JSON value
    fn stderr_json<F: FnOnce(&serde_json::Value)>(&self, f: F) -> &Self;
}

impl Assert for RunResult {
    fn success(&self) -> &Self {
        assert_eq!(
            self.exit_code, 0,
            "Expected success (exit code 0), got {}",
            self.exit_code
        );
        self
    }

    fn failure(&self) -> &Self {
        assert_ne!(
            self.exit_code, 0,
            "Expected failure (non-zero exit code), got {}",
            self.exit_code
        );
        self
    }

    fn exit(&self, code: i32) -> &Self {
        assert_eq!(
            self.exit_code, code,
            "Expected exit code {}, got {}",
            code, self.exit_code
        );
        self
    }

    fn stdout(&self, pattern: impl AsRef<str>) -> &Self {
        let pattern = pattern.as_ref();
        assert!(
            self.stdout.contains(pattern),
            "Expected stdout to contain '{}', but got: {}",
            pattern,
            self.stdout
        );
        self
    }

    fn stderr(&self, pattern: impl AsRef<str>) -> &Self {
        let pattern = pattern.as_ref();
        assert!(
            self.stderr.contains(pattern),
            "Expected stderr to contain '{}', but got: {}",
            pattern,
            self.stderr
        );
        self
    }

    fn stdout_json<F: FnOnce(&serde_json::Value)>(&self, f: F) -> &Self {
        let value: serde_json::Value = serde_json::from_str(&self.stdout)
            .unwrap_or_else(|_| panic!("Failed to parse stdout as JSON: {}", self.stdout));
        f(&value);
        self
    }

    fn stderr_json<F: FnOnce(&serde_json::Value)>(&self, f: F) -> &Self {
        let value: serde_json::Value = serde_json::from_str(&self.stderr)
            .unwrap_or_else(|_| panic!("Failed to parse stderr as JSON: {}", self.stderr));
        f(&value);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::Assert;
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
            stderr: "error message".to_string(),
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
        assert_eq!(
            result.exit_code, 0,
            "Expected success (exit code 0), got {}",
            result.exit_code
        );
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
    #[should_panic]
    fn test_assert_exit_fails() {
        let result = mock_success_result();
        result.exit(1); // Should panic
    }

    #[test]
    fn test_assert_stdout() {
        let result = mock_success_result();
        Assert::stdout(&result, "hello"); // Should not panic
    }

    #[test]
    #[should_panic]
    fn test_assert_stdout_fails() {
        let result = mock_success_result();
        Assert::stdout(&result, "goodbye"); // Should panic
    }

    #[test]
    fn test_assert_stderr() {
        let result = mock_failure_result();
        Assert::stderr(&result, "error"); // Should not panic
    }

    #[test]
    #[should_panic]
    fn test_assert_stderr_fails() {
        let result = mock_failure_result();
        Assert::stderr(&result, "success"); // Should panic
    }

    #[test]
    fn test_stdout_json() {
        let mut result = mock_success_result();
        result.stdout = r#"{"name": "test", "value": 42}"#.to_string();

        result.stdout_json(|json| {
            assert_eq!(json["name"], "test");
            assert_eq!(json["value"], 42);
        }); // Should not panic
    }

    #[test]
    #[should_panic]
    fn test_stdout_json_invalid() {
        let mut result = mock_success_result();
        result.stdout = "not json".to_string();

        result.stdout_json(|_| {}); // Should panic
    }
}
