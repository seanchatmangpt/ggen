//! Scenario DSL for multi-step test orchestration
//!
//! Provides a fluent API for defining complex test scenarios with
//! deterministic execution, step aggregation, and concurrent execution.

use crate::error::{Result, ScenarioError};
use crate::backend::{Backend, Cmd};
use crate::policy::{Policy, TimeProfile, RngProfile};
use crate::services::Service;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

/// Scenario execution result
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

/// Individual step execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepResult {
    /// Step name/label
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

/// A single execution step in a scenario
#[derive(Debug, Clone)]
struct Step {
    /// Step name/label
    name: String,
    /// Command binary
    cmd: Cmd,
}

/// Scenario builder for multi-step test orchestration
pub struct Scenario {
    /// Scenario name
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
    /// Create a new scenario with the given name
    pub fn new(name: String) -> Self {
        Self {
            name,
            steps: Vec::new(),
            policy: Policy::default(),
            time_profile: TimeProfile::default(),
            rng_profile: RngProfile::default(),
            backend: Box::new(crate::backend::AutoBackend::detect().unwrap_or_else(|_| {
                // Fallback to local backend if detection fails
                Box::new(crate::backend::LocalBackend::new())
            })),
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
        if args_vec.is_empty() {
            return self;
        }

        let cmd = Cmd::new(&args_vec[0]).args(&args_vec[1..]);
        self.steps.push(Step { name: label, cmd });
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

    /// Set time determinism profile
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

    /// Set environment variables
    pub fn env(mut self, env: HashMap<String, String>) -> Self {
        self.env = env;
        self
    }

    /// Execute the scenario
    pub fn run(self) -> Result<RunResult> {
        // Start services
        for mut service in self.services {
            service.start()?;
        }

        let start_time = std::time::Instant::now();
        let mut step_results = Vec::new();

        if self.concurrent {
            // Execute steps concurrently
            let handles: Vec<_> = self.steps.into_iter().enumerate().map(|(i, step)| {
                let backend = self.backend.as_ref();
                std::thread::spawn(move || {
                    execute_step(backend, &step, i)
                })
            }).collect();

            for handle in handles {
                match handle.join() {
                    Ok(result) => step_results.push(result),
                    Err(_) => return Err(ScenarioError::StepFailed("Thread panicked".to_string()).into()),
                }
            }
        } else {
            // Execute steps sequentially
            for (i, step) in self.steps.into_iter().enumerate() {
                let result = execute_step(self.backend.as_ref(), &step, i)?;
                step_results.push(result);
            }
        }

        // Stop services
        for mut service in self.services {
            service.stop()?;
        }

        let total_duration = start_time.elapsed().as_millis() as u64;

        // Aggregate results
        let final_exit_code = step_results.last().map(|s| s.exit_code).unwrap_or(0);
        let combined_stdout = step_results.iter().map(|s| s.stdout.as_str()).collect::<Vec<_>>().join("\n");
        let combined_stderr = step_results.iter().map(|s| s.stderr.as_str()).collect::<Vec<_>>().join("\n");

        Ok(RunResult {
            exit_code: final_exit_code,
            stdout: combined_stdout,
            stderr: combined_stderr,
            duration_ms: total_duration,
            steps: step_results,
            redacted_env: Vec::new(), // WIP: Implement env redaction
            backend: self.backend.name().to_string(),
        })
    }
}

/// Execute a single step
fn execute_step(backend: &dyn Backend, step: &Step, index: usize) -> Result<StepResult> {
    // Apply determinism constraints to environment
    // WIP: Apply time, RNG, and other constraints

    // Execute via backend
    let backend_result = backend.run_cmd(step.cmd.clone())?;

    Ok(StepResult {
        name: step.name.clone(),
        exit_code: backend_result.exit_code,
        stdout: backend_result.stdout,
        stderr: backend_result.stderr,
        duration_ms: backend_result.duration_ms,
    })
}

/// Create a new scenario
pub fn scenario(name: &str) -> Scenario {
    Scenario::new(name.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scenario_creation() {
        let scenario = scenario("test scenario");
        assert_eq!(scenario.name, "test scenario");
    }

    #[test]
    fn test_scenario_step() {
        let scenario = scenario("test")
            .step("echo hello", ["echo", "hello"]);

        assert_eq!(scenario.steps.len(), 1);
        assert_eq!(scenario.steps[0].name, "echo hello");
        assert_eq!(scenario.steps[0].cmd.bin, "echo");
        assert_eq!(scenario.steps[0].cmd.args, vec!["hello".to_string()]);
    }

    #[test]
    fn test_scenario_concurrent() {
        let scenario = scenario("concurrent test")
            .concurrent();

        assert!(scenario.concurrent);
    }
}
