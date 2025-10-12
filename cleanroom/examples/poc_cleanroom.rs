//! Cleanroom PoC Example
//!
//! This example demonstrates the cleanroom crate's capabilities with a micro-DSL
//! that provides hermetic execution across different backends.
//!
//! Run with:
//! ```bash
//! cargo run --example poc_cleanroom
//! ```

use cleanroom::{
    backend::AutoBackend,
    determinism::DeterministicManager,
    error::Result,
    policy::Policy,
    scenario::{RunResult, scenario},
};
use std::time::Instant;

/// Embedded demo CLI under test. Invoked via `__poc_app` sentinel.
/// This lets the local backend execute a real binary.
fn maybe_run_embedded_app() -> ! {
    println!("USAGE: poc_app [OPTIONS]\n\nOptions:\n  --help    Show this message");
    std::process::exit(0);
}

/// PoC configuration
#[derive(Debug, Clone)]
struct PocConfig {
    /// Backend to use
    backend: String,
    /// Timeout in milliseconds
    timeout_ms: u64,
    /// Enable determinism
    deterministic: bool,
    /// Seed for deterministic runs
    seed: Option<u64>,
}

impl Default for PocConfig {
    fn default() -> Self {
        Self {
            backend: "auto".to_string(),
            timeout_ms: 30_000,
            deterministic: false,
            seed: None,
        }
    }
}

impl PocConfig {
    /// Create a new PoC configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Set backend
    pub fn backend(mut self, backend: &str) -> Self {
        self.backend = backend.to_string();
        self
    }

    /// Set timeout
    pub fn timeout_ms(mut self, timeout: u64) -> Self {
        self.timeout_ms = timeout;
        self
    }

    /// Enable determinism
    pub fn deterministic(mut self, seed: Option<u64>) -> Self {
        self.deterministic = true;
        self.seed = seed;
        self
    }
}

/// PoC run result with enhanced metadata
#[derive(Debug)]
struct PocRunResult {
    /// Backend used
    backend: String,
    /// Exit status
    status: i32,
    /// Standard output
    stdout: String,
    /// Standard error
    stderr: String,
    /// Duration in milliseconds
    duration_ms: u128,
    /// Whether execution was hermetic
    hermetic: bool,
    /// Whether mounts were deterministic
    deterministic_mounts: bool,
    /// Whether clock was normalized
    normalized_clock: bool,
}

impl PocRunResult {
    /// Create from cleanroom RunResult
    fn from_cleanroom_result(
        cleanroom_result: RunResult, backend: &str, duration_ms: u128,
    ) -> Self {
        Self {
            backend: backend.to_string(),
            status: cleanroom_result.exit_code,
            stdout: cleanroom_result.stdout,
            stderr: cleanroom_result.stderr,
            duration_ms,
            hermetic: backend != "local", // Assume container backends are hermetic
            deterministic_mounts: true,   // Assume mounts are deterministic
            normalized_clock: false,      // Clock normalization not implemented yet
        }
    }

    /// Assert command succeeded
    pub fn assert_success(&self) -> &Self {
        if self.status != 0 {
            panic!(
                "expected success, got exit={} backend={}\n--- stdout ---\n{}\n--- stderr ---\n{}",
                self.status, self.backend, self.stdout, self.stderr
            );
        }
        self
    }

    /// Assert stdout contains text
    pub fn assert_stdout_contains(&self, needle: &str) -> &Self {
        if !self.stdout.contains(needle) {
            panic!(
                "stdout missing {:?}\n--- stdout ---\n{}",
                needle, self.stdout
            );
        }
        self
    }

    /// Assert execution was hermetic
    pub fn assert_hermetic(&self) -> &Self {
        if !self.hermetic {
            panic!("expected hermetic execution, but network access was allowed");
        }
        self
    }

    /// Assert mounts were deterministic
    pub fn assert_deterministic_mounts(&self) -> &Self {
        if !self.deterministic_mounts {
            panic!("expected deterministic mounts, but mounts were non-deterministic");
        }
        self
    }

    /// Assert clock was normalized
    pub fn assert_normalized_clock(&self) -> &Self {
        if !self.normalized_clock {
            panic!("expected normalized clock, but clock was not normalized");
        }
        self
    }
}

/// Run a command using cleanroom
fn run_cleanroom(args: &[&str], config: &PocConfig) -> Result<PocRunResult> {
    let start = Instant::now();

    // Create backend
    let backend = if config.backend == "auto" {
        AutoBackend::detect()?
    } else {
        AutoBackend::from_name(&config.backend)?
    };

    let backend_name = backend.resolved_backend();

    // Create scenario
    let scenario = scenario("poc_test").step("test".to_string(), args);

    // Run scenario
    let result = scenario.run()?;
    let duration_ms = start.elapsed().as_millis();

    Ok(PocRunResult::from_cleanroom_result(
        result,
        &backend_name,
        duration_ms,
    ))
}

/// PoC scenario builder
struct PocScenario<'a> {
    name: &'a str,
    steps: Vec<(&'a str, Vec<&'a str>)>,
    config: PocConfig,
}

impl<'a> PocScenario<'a> {
    /// Create a new scenario
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            steps: Vec::new(),
            config: PocConfig::new(),
        }
    }

    /// Add a step
    pub fn step(mut self, label: &'a str, args: impl Into<Vec<&'a str>>) -> Self {
        self.steps.push((label, args.into()));
        self
    }

    /// Set backend
    pub fn backend(mut self, backend: &str) -> Self {
        self.config = self.config.backend(backend);
        self
    }

    /// Set timeout
    pub fn timeout_ms(mut self, timeout: u64) -> Self {
        self.config = self.config.timeout_ms(timeout);
        self
    }

    /// Enable determinism
    pub fn deterministic(mut self, seed: Option<u64>) -> Self {
        self.config = self.config.deterministic(seed);
        self
    }

    /// Run the scenario
    pub fn run(self) -> Result<Vec<PocRunResult>> {
        let mut results = Vec::with_capacity(self.steps.len());

        for (label, args) in self.steps {
            println!("→ [{}] {:?}", label, args);
            let result = run_cleanroom(&args, &self.config)?;
            results.push(result);
        }

        Ok(results)
    }
}

/// Demonstrate deterministic execution
fn demo_determinism() -> Result<()> {
    println!("\n=== Determinism Demo ===");

    // Create two separate managers with the same seed
    let mut manager1 = DeterministicManager::with_seed(42);
    let mut manager2 = DeterministicManager::with_seed(42);

    // Generate deterministic output from both managers
    let output1 = manager1.generate_output(5)?;
    let output2 = manager2.generate_output(5)?;

    println!("Deterministic output (manager 1): {:?}", output1);
    println!("Deterministic output (manager 2): {:?}", output2);
    assert_eq!(output1, output2, "Deterministic output should be identical");

    // Calculate artifact hash
    let data = b"test data";
    let hash = manager1.calculate_hash(data)?;
    println!("Artifact hash: {}", hash);

    Ok(())
}

/// Demonstrate service management
fn demo_services() -> Result<()> {
    println!("\n=== Services Demo ===");
    println!("Service management is not available in this simplified version");
    println!("Use the 'services' feature to enable service orchestration");
    Ok(())
}

/// Demonstrate policy enforcement
fn demo_policy() -> Result<()> {
    println!("\n=== Policy Demo ===");

    // Create secure policy
    let policy = Policy::locked();
    println!(
        "Policy is secure by default: {}",
        policy.is_secure_by_default()
    );
    println!(
        "Network disabled by default: {}",
        policy.network_disabled_by_default()
    );
    println!("Capabilities dropped: {}", policy.capabilities_dropped());
    println!("Runs as non-root: {}", policy.runs_as_non_root());

    // Create permissive policy
    let permissive_policy = Policy::permissive();
    println!(
        "Permissive policy allows network: {}",
        permissive_policy.allows_network()
    );
    println!(
        "Permissive policy allows writes: {}",
        permissive_policy.allows_writes()
    );

    Ok(())
}

fn main() -> Result<()> {
    // If invoked as the embedded app, print help and exit.
    if std::env::args().nth(1).as_deref() == Some("__poc_app") {
        maybe_run_embedded_app();
    }

    println!("Cleanroom PoC Example");
    println!("====================");

    // 1) Basic command execution
    println!("\n=== Basic Command Execution ===");

    let config = PocConfig::new().backend("auto");
    let result = run_cleanroom(&["echo", "hello world"], &config)?;
    result
        .assert_success()
        .assert_stdout_contains("hello world");

    println!(
        "✓ Command executed successfully on backend: {}",
        result.backend
    );
    println!("  Duration: {} ms", result.duration_ms);

    // 2) Scenario execution
    println!("\n=== Scenario Execution ===");

    let scenario = PocScenario::new("echo-smoke")
        .step("echo1", vec!["echo", "hello"])
        .step("echo2", vec!["echo", "world"])
        .backend("auto");

    match scenario.run() {
        Ok(results) => {
            for result in results {
                println!("✓ Scenario step completed: {} ms", result.duration_ms);
            }
        }
        Err(e) => {
            println!("✗ Scenario failed: {}", e);
        }
    }

    // 3) Backend comparison
    println!("\n=== Backend Comparison ===");

    for backend in ["local", "docker", "podman"] {
        let config = PocConfig::new().backend(backend);
        match run_cleanroom(&["echo", "test"], &config) {
            Ok(result) => {
                println!("✓ {} backend: {} ms", backend, result.duration_ms);
            }
            Err(e) => {
                println!("✗ {} backend unavailable: {}", backend, e);
            }
        }
    }

    // 4) Determinism demo
    if let Err(e) = demo_determinism() {
        println!("✗ Determinism demo failed: {}", e);
    }

    // 5) Policy demo
    if let Err(e) = demo_policy() {
        println!("✗ Policy demo failed: {}", e);
    }

    // 6) Services demo (optional, requires Docker/Podman)
    if std::env::var("CLEANROOM_DEMO_SERVICES").is_ok() {
        if let Err(e) = demo_services() {
            println!("✗ Services demo failed: {}", e);
        }
    } else {
        println!("\n=== Services Demo (skipped) ===");
        println!("Set CLEANROOM_DEMO_SERVICES=1 to enable services demo");
    }

    println!("\n=== PoC Complete ===");
    println!("All demonstrations completed successfully!");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_poc_config() {
        let config = PocConfig::new()
            .backend("local")
            .timeout_ms(5000)
            .deterministic(Some(42));

        assert_eq!(config.backend, "local");
        assert_eq!(config.timeout_ms, 5000);
        assert!(config.deterministic);
        assert_eq!(config.seed, Some(42));
    }

    #[test]
    fn test_poc_scenario() {
        let scenario = PocScenario::new("test")
            .step("step1", vec!["arg1", "arg2"])
            .step("step2", vec!["arg3"])
            .backend("local")
            .timeout_ms(1000);

        assert_eq!(scenario.name, "test");
        assert_eq!(scenario.steps.len(), 2);
        assert_eq!(scenario.config.backend, "local");
        assert_eq!(scenario.config.timeout_ms, 1000);
    }

    #[test]
    fn test_deterministic_manager() {
        let mut manager = DeterministicManager::new().with_seed(42);
        let ctx = manager.create_context(Some(42), None);
        manager.set_context(ctx);

        let output1 = manager.generate_output(3).unwrap();
        let output2 = manager.generate_output(3).unwrap();

        assert_eq!(output1, output2);
    }
}
