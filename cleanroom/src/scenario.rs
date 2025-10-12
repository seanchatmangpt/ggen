//! Scenario DSL for building and executing test scenarios
//!
//! Provides a fluent API for defining test scenarios with steps, policies,
//! determinism constraints, backends, and services.

use crate::backend::{Backend, Cmd};
use crate::error::Result;
use crate::policy::{Policy, RngProfile, TimeProfile};
use crate::services::Service;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Result of a scenario execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunResult {
    /// Exit code of the last step
    pub exit_code: i32,
    /// Combined standard output from all steps
    pub stdout: String,
    /// Combined standard error from all steps
    pub stderr: String,
    /// Total execution duration in milliseconds
    pub duration_ms: u64,
    /// Individual step results
    pub steps: Vec<StepResult>,
    /// Environment variables that were redacted in forensics
    pub redacted_env: Vec<String>,
    /// Backend used for execution
    pub backend: String,
}

impl RunResult {
    /// Create from process output (for single step)
    pub fn from_output(output: std::process::Output, duration_ms: u64) -> Self {
        Self {
            exit_code: output.status.code().unwrap_or(-1),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            duration_ms,
            steps: vec![StepResult {
                name: "default".to_string(),
                exit_code: output.status.code().unwrap_or(-1),
                stdout: String::from_utf8_lossy(&output.stdout).to_string(),
                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
                duration_ms,
            }],
            redacted_env: Vec::new(),
            backend: "unknown".to_string(),
        }
    }

    /// Check if the scenario succeeded
    pub fn success(&self) -> bool {
        self.exit_code == 0
    }

    /// Get the final exit code
    pub fn exit_code(&self) -> i32 {
        self.exit_code
    }

    /// Get the combined stdout
    pub fn stdout_content(&self) -> &str {
        &self.stdout
    }

    /// Get the combined stderr
    pub fn stderr_content(&self) -> &str {
        &self.stderr
    }

    /// Get execution duration
    pub fn duration(&self) -> Duration {
        Duration::from_millis(self.duration_ms)
    }
}

/// Result of a single step execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepResult {
    /// Step name
    pub name: String,
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Execution duration in milliseconds
    pub duration_ms: u64,
}

/// A test scenario builder
pub struct Scenario {
    /// Scenario name
    #[allow(dead_code)]
    name: String,
    /// Execution steps
    steps: Vec<Step>,
    /// Security policy
    policy: Policy,
    /// Time determinism profile
    time_profile: TimeProfile,
    /// RNG determinism profile
    rng_profile: RngProfile,
    /// Backend to use
    backend: Box<dyn Backend>,
    /// Services to start
    services: Vec<Box<dyn Service>>,
    /// Environment variables
    env: HashMap<String, String>,
    /// Concurrent execution flag
    concurrent: bool,
}

impl Scenario {
    fn new(name: String) -> Self {
        Self {
            name,
            steps: Vec::new(),
            policy: Policy::default(),
            time_profile: TimeProfile::default(),
            rng_profile: RngProfile::default(),
            backend: crate::backend::AutoBackend::detect()
                .unwrap_or_else(|_| {
                    // Fallback to local backend if detection fails
                    crate::backend::AutoBackend::new(Box::new(crate::backend::LocalBackend::new()))
                })
                .inner,
            services: Vec::new(),
            env: HashMap::new(),
            concurrent: false,
        }
    }

    /// Add an execution step
    pub fn step<I, S>(mut self, label: String, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let args_vec: Vec<String> = args.into_iter().map(|s| s.as_ref().to_string()).collect();
        if !args_vec.is_empty() {
            self.steps.push(Step {
                label,
                args: args_vec,
            });
        }
        self
    }

    /// Enable concurrent execution of steps
    pub fn concurrent(mut self) -> Self {
        self.concurrent = true;
        self
    }

    /// Set security policy
    pub fn policy(mut self, policy: Policy) -> Self {
        self.policy = policy;
        self
    }

    /// Set determinism profiles
    pub fn determinism(mut self, time: TimeProfile, rng: RngProfile) -> Self {
        self.time_profile = time;
        self.rng_profile = rng;
        self
    }

    /// Set backend explicitly
    pub fn backend(mut self, backend: Box<dyn Backend>) -> Self {
        self.backend = backend;
        self
    }

    /// Add services to start before execution
    pub fn services(mut self, services: Vec<Box<dyn Service>>) -> Self {
        self.services = services;
        self
    }

    /// Add environment variable
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    /// Execute the scenario
    pub fn run(mut self) -> Result<RunResult> {
        let start = Instant::now();

        // Start services
        for service in &mut self.services {
            service.start()?;
        }

        let mut step_results = Vec::new();
        let mut combined_stdout = String::new();
        let mut combined_stderr = String::new();
        let mut last_exit_code = 0;

        if self.concurrent {
            // Execute steps concurrently (simplified - in real impl would use threads/futures)
            for step in &self.steps {
                let result = self.execute_step(step)?;
                step_results.push(StepResult {
                    name: step.label.clone(),
                    exit_code: result.exit_code,
                    stdout: result.stdout.clone(),
                    stderr: result.stderr.clone(),
                    duration_ms: result.duration_ms,
                });
                combined_stdout.push_str(&result.stdout);
                combined_stderr.push_str(&result.stderr);
                last_exit_code = result.exit_code;
            }
        } else {
            // Execute steps sequentially
            for step in &self.steps {
                let result = self.execute_step(step)?;
                step_results.push(StepResult {
                    name: step.label.clone(),
                    exit_code: result.exit_code,
                    stdout: result.stdout.clone(),
                    stderr: result.stderr.clone(),
                    duration_ms: result.duration_ms,
                });
                combined_stdout.push_str(&result.stdout);
                combined_stderr.push_str(&result.stderr);
                last_exit_code = result.exit_code;

                // Stop on first failure in sequential mode
                if result.exit_code != 0 {
                    break;
                }
            }
        }

        // Stop services
        for mut service in self.services {
            service.stop()?;
        }

        let duration_ms = start.elapsed().as_millis() as u64;

        Ok(RunResult {
            exit_code: last_exit_code,
            stdout: combined_stdout,
            stderr: combined_stderr,
            duration_ms,
            steps: step_results,
            redacted_env: Vec::new(), // WIP: Implement env redaction
            backend: self.backend.name().to_string(),
        })
    }

    /// Execute a single step
    fn execute_step(&self, step: &Step) -> Result<RunResult> {
        // Apply determinism constraints to environment
        // WIP: Apply time, RNG, and other constraints

        // Execute via backend
        let cmd = Cmd::new(&step.args[0]).args(&step.args[1..]);
        let backend_result = self.backend.run_cmd(cmd)?;

        // Convert backend result to scenario result
        Ok(RunResult {
            exit_code: backend_result.exit_code,
            stdout: backend_result.stdout,
            stderr: backend_result.stderr,
            duration_ms: backend_result.duration_ms,
            steps: Vec::new(),
            redacted_env: Vec::new(),
            backend: self.backend.name().to_string(),
        })
    }
}

impl std::fmt::Debug for Scenario {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scenario")
            .field("name", &self.name)
            .field("steps", &self.steps.len())
            .field("policy", &self.policy)
            .field("time_profile", &self.time_profile)
            .field("rng_profile", &self.rng_profile)
            .field("backend", &self.backend.name())
            .field("services", &self.services.len())
            .field("env", &self.env)
            .field("concurrent", &self.concurrent)
            .finish()
    }
}

/// Execution step definition
#[derive(Debug, Clone)]
struct Step {
    /// Step label for identification
    label: String,
    /// Command arguments
    args: Vec<String>,
}

/// Create a new scenario builder
pub fn scenario(name: impl Into<String>) -> Scenario {
    Scenario::new(name.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::LocalBackend;

    #[test]
    fn test_scenario_creation() {
        let scenario = scenario("test scenario");
        assert_eq!(scenario.name, "test scenario");
    }

    #[test]
    fn test_scenario_step() {
        let scenario = scenario("test").step("echo step".to_string(), ["echo", "hello"]);

        assert_eq!(scenario.steps.len(), 1);
        assert_eq!(scenario.steps[0].label, "echo step");
        assert_eq!(scenario.steps[0].args, vec!["echo", "hello"]);
    }

    #[test]
    fn test_scenario_concurrent() {
        let scenario = scenario("test").concurrent();
        assert!(scenario.concurrent);
    }

    #[test]
    fn test_scenario_policy() {
        let policy = Policy::locked();
        let scenario = scenario("test").policy(policy.clone());
        assert_eq!(scenario.policy.net, policy.net);
    }

    #[test]
    fn test_scenario_determinism() {
        let scenario =
            scenario("test").determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42));

        assert!(matches!(scenario.time_profile, TimeProfile::Frozen(12345)));
        assert!(matches!(scenario.rng_profile, RngProfile::Seed(42)));
    }

    #[test]
    fn test_scenario_backend() {
        let backend = Box::new(LocalBackend::new()) as Box<dyn Backend>;
        let scenario = scenario("test").backend(backend);
        assert_eq!(scenario.backend.name(), "local");
    }

    #[test]
    fn test_scenario_env() {
        let scenario = scenario("test").env("TEST_VAR", "test_value");

        assert_eq!(
            scenario.env.get("TEST_VAR"),
            Some(&"test_value".to_string())
        );
    }
}
