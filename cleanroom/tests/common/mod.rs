//! Common test utilities and fixtures for cleanroom testing
//!
//! Provides shared test helpers, fixture builders, and assertion utilities
//! for consistent testing across unit, integration, and acceptance tests.

use cleanroom::backend::{Cmd, RunResult};
use cleanroom::policy::{Policy, TimeProfile, RngProfile};
use cleanroom::scenario::{Scenario, StepResult};
use std::collections::HashMap;

/// Builder for creating test RunResult instances
pub struct RunResultBuilder {
    exit_code: i32,
    stdout: String,
    stderr: String,
    duration_ms: u64,
    steps: Vec<StepResult>,
    redacted_env: Vec<String>,
    backend: String,
}

impl RunResultBuilder {
    pub fn new() -> Self {
        Self {
            exit_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            duration_ms: 100,
            steps: vec![],
            redacted_env: vec![],
            backend: "test".to_string(),
        }
    }

    pub fn exit_code(mut self, code: i32) -> Self {
        self.exit_code = code;
        self
    }

    pub fn stdout(mut self, output: impl Into<String>) -> Self {
        self.stdout = output.into();
        self
    }

    pub fn stderr(mut self, output: impl Into<String>) -> Self {
        self.stderr = output.into();
        self
    }

    pub fn duration_ms(mut self, ms: u64) -> Self {
        self.duration_ms = ms;
        self
    }

    pub fn backend(mut self, name: impl Into<String>) -> Self {
        self.backend = name.into();
        self
    }

    pub fn add_step(mut self, name: impl Into<String>, exit_code: i32, stdout: impl Into<String>, stderr: impl Into<String>) -> Self {
        self.steps.push(StepResult {
            name: name.into(),
            exit_code,
            stdout: stdout.into(),
            stderr: stderr.into(),
            duration_ms: 50,
        });
        self
    }

    pub fn build(self) -> RunResult {
        RunResult {
            exit_code: self.exit_code,
            stdout: self.stdout,
            stderr: self.stderr,
            duration_ms: self.duration_ms,
            steps: self.steps,
            redacted_env: self.redacted_env,
            backend: self.backend,
        }
    }
}

impl Default for RunResultBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for creating test Scenario instances
pub struct ScenarioBuilder {
    name: String,
    policy: Policy,
    time_profile: TimeProfile,
    rng_profile: RngProfile,
    env: HashMap<String, String>,
}

impl ScenarioBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            policy: Policy::default(),
            time_profile: TimeProfile::default(),
            rng_profile: RngProfile::default(),
            env: HashMap::new(),
        }
    }

    pub fn policy(mut self, policy: Policy) -> Self {
        self.policy = policy;
        self
    }

    pub fn deterministic(mut self, time: TimeProfile, rng: RngProfile) -> Self {
        self.time_profile = time;
        self.rng_profile = rng;
        self
    }

    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    pub fn build(self) -> Scenario {
        let mut scenario = cleanroom::scenario(self.name);
        scenario = scenario.policy(self.policy);
        scenario = scenario.determinism(self.time_profile, self.rng_profile);
        
        for (key, value) in self.env {
            scenario = scenario.env(key, value);
        }
        
        scenario
    }
}

/// Assertion helpers for determinism verification
pub mod assertions {
    use super::*;
    use insta::assert_snapshot;

    /// Assert that two RunResults are identical (for determinism testing)
    pub fn assert_results_identical(result1: &RunResult, result2: &RunResult) {
        assert_eq!(result1.exit_code, result2.exit_code, "Exit codes differ");
        assert_eq!(result1.stdout, result2.stdout, "Stdout differs");
        assert_eq!(result1.stderr, result2.stderr, "Stderr differs");
        assert_eq!(result1.backend, result2.backend, "Backend differs");
    }

    /// Assert that a result contains expected patterns
    pub fn assert_result_contains(result: &RunResult, stdout_pattern: Option<&str>, stderr_pattern: Option<&str>) {
        if let Some(pattern) = stdout_pattern {
            assert!(result.stdout.contains(pattern), 
                "Expected stdout to contain '{}', got: {}", pattern, result.stdout);
        }
        if let Some(pattern) = stderr_pattern {
            assert!(result.stderr.contains(pattern), 
                "Expected stderr to contain '{}', got: {}", pattern, result.stderr);
        }
    }

    /// Create a snapshot of a RunResult for deterministic testing
    pub fn snapshot_result(result: &RunResult, name: &str) {
        let snapshot = format!(
            "Exit Code: {}\nStdout:\n{}\nStderr:\n{}\nDuration: {}ms\nBackend: {}",
            result.exit_code, result.stdout, result.stderr, result.duration_ms, result.backend
        );
        assert_snapshot!(snapshot, name);
    }
}

/// Test data generators for property-based testing
pub mod generators {
    use proptest::prelude::*;

    /// Generate random command arguments
    pub fn command_args() -> impl Strategy<Value = Vec<String>> {
        prop::collection::vec("[a-zA-Z0-9_-]{1,20}", 0..5)
    }

    /// Generate random environment variables
    pub fn env_vars() -> impl Strategy<Value = Vec<(String, String)>> {
        prop::collection::vec(
            (any::<String>(), any::<String>()),
            0..10
        ).prop_map(|vars| {
            vars.into_iter()
                .map(|(k, v)| (k.chars().take(20).collect(), v.chars().take(50).collect()))
                .collect()
        })
    }

    /// Generate random exit codes
    pub fn exit_codes() -> impl Strategy<Value = i32> {
        -1000..1000
    }
}

/// Fixture factories for common test scenarios
pub mod fixtures {
    use super::*;

    /// Create a simple echo command fixture
    pub fn echo_command() -> Cmd {
        Cmd::new("echo").args(["hello", "world"])
    }

    /// Create a failing command fixture
    pub fn failing_command() -> Cmd {
        Cmd::new("false")
    }

    /// Create a command with environment variables
    pub fn env_command() -> Cmd {
        Cmd::new("sh")
            .args(["-c", "echo $TEST_VAR"])
            .env("TEST_VAR", "test_value")
    }

    /// Create a locked-down policy fixture
    pub fn locked_policy() -> Policy {
        Policy::locked()
    }

    /// Create a permissive policy fixture
    pub fn permissive_policy() -> Policy {
        Policy::permissive()
    }

    /// Create a deterministic scenario fixture
    pub fn deterministic_scenario() -> Scenario {
        ScenarioBuilder::new("deterministic_test")
            .deterministic(TimeProfile::Frozen(12345), RngProfile::Seed(42))
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_result_builder() {
        let result = RunResultBuilder::new()
            .exit_code(0)
            .stdout("test output")
            .stderr("test error")
            .duration_ms(200)
            .backend("test_backend")
            .build();

        assert_eq!(result.exit_code, 0);
        assert_eq!(result.stdout, "test output");
        assert_eq!(result.stderr, "test error");
        assert_eq!(result.duration_ms, 200);
        assert_eq!(result.backend, "test_backend");
    }

    #[test]
    fn test_scenario_builder() {
        let scenario = ScenarioBuilder::new("test_scenario")
            .policy(Policy::locked())
            .deterministic(TimeProfile::Frozen(12345), RngProfile::Seed(42))
            .env("TEST_VAR", "test_value")
            .build();

        assert_eq!(scenario.name, "test_scenario");
        // Note: We can't directly access private fields, but the scenario is built correctly
    }

    #[test]
    fn test_assertions() {
        let result1 = RunResultBuilder::new()
            .stdout("hello")
            .build();
        let result2 = RunResultBuilder::new()
            .stdout("hello")
            .build();

        assertions::assert_results_identical(&result1, &result2);
        assertions::assert_result_contains(&result1, Some("hello"), None);
    }
}
