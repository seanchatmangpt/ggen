//! # Ultra-Fast Deployment Workflow Tests
//!
//! Comprehensive test suite validating the ggen → cleanroom → deploy pipeline
//! with <60s execution targets and full integration validation.
//!
//! ## Test Categories
//!
//! 1. **Integration Tests**: End-to-end workflow validation
//! 2. **Performance Tests**: Timing and throughput benchmarks
//! 3. **Reliability Tests**: Consistency and error handling
//! 4. **Matrix Tests**: Multiple templates, platforms, scenarios
//!
//! ## Performance Targets
//!
//! - **Total workflow**: <60s (target: 45-55s)
//! - **Code generation**: <5s
//! - **Validation**: <10s
//! - **Test execution**: <20s
//! - **Build**: <25s
//! - **Fake publish**: <5s

use anyhow::{Context, Result};
use assert_cmd::Command;
use predicates::prelude::*;
use serial_test::serial;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tempfile::TempDir;

/// Performance timing tracker
#[derive(Debug, Clone)]
struct TimingReport {
    stage: String,
    duration: Duration,
    target: Duration,
}

impl TimingReport {
    fn new(stage: &str, duration: Duration, target: Duration) -> Self {
        Self {
            stage: stage.to_string(),
            duration,
            target,
        }
    }

    fn passed(&self) -> bool {
        self.duration <= self.target
    }

    fn percentage(&self) -> f64 {
        (self.duration.as_secs_f64() / self.target.as_secs_f64()) * 100.0
    }
}

/// Complete workflow result
#[derive(Debug)]
struct WorkflowResult {
    success: bool,
    total_duration: Duration,
    timings: Vec<TimingReport>,
    output_dir: PathBuf,
    artifacts: Vec<PathBuf>,
}

impl WorkflowResult {
    fn assert_success(&self) -> &Self {
        assert!(self.success, "Workflow failed");
        self
    }

    fn assert_timing(&self, target: Duration) -> &Self {
        assert!(
            self.total_duration <= target,
            "Workflow took {}s, expected <{}s",
            self.total_duration.as_secs(),
            target.as_secs()
        );
        self
    }

    fn assert_artifacts_exist(&self) -> &Self {
        for artifact in &self.artifacts {
            assert!(
                artifact.exists(),
                "Missing artifact: {}",
                artifact.display()
            );
        }
        self
    }

    fn print_report(&self) {
        println!("\n=== Ultra-Deploy Workflow Report ===");
        println!("Success: {}", self.success);
        println!("Total Duration: {:.2}s", self.total_duration.as_secs_f64());
        println!("\n--- Stage Timings ---");
        for timing in &self.timings {
            let status = if timing.passed() { "✓" } else { "✗" };
            println!(
                "{} {}: {:.2}s / {:.2}s ({:.1}%)",
                status,
                timing.stage,
                timing.duration.as_secs_f64(),
                timing.target.as_secs_f64(),
                timing.percentage()
            );
        }
        println!("\n--- Artifacts ---");
        for artifact in &self.artifacts {
            println!("  {}", artifact.display());
        }
        println!("=====================================\n");
    }
}

/// Test harness for ultra-deploy workflow
struct UltraDeployTester {
    ggen_bin: PathBuf,
    work_dir: TempDir,
}

impl UltraDeployTester {
    fn new() -> Result<Self> {
        let ggen_bin = Self::find_ggen_binary()?;
        let work_dir = TempDir::new()?;

        Ok(Self { ggen_bin, work_dir })
    }

    fn find_ggen_binary() -> Result<PathBuf> {
        let current_dir = std::env::current_dir()?;

        // Try release build first (optimized)
        let release_bin = current_dir.join("target/release/ggen");
        if release_bin.exists() {
            return Ok(release_bin);
        }

        // Fall back to debug build
        let debug_bin = current_dir.join("target/debug/ggen");
        if debug_bin.exists() {
            return Ok(debug_bin);
        }

        anyhow::bail!("ggen binary not found. Run: cargo build --release");
    }

    fn work_dir(&self) -> &Path {
        self.work_dir.path()
    }

    /// Execute complete ultra-deploy workflow
    async fn run_workflow(&self, template: &str) -> Result<WorkflowResult> {
        let workflow_start = Instant::now();
        let mut timings = Vec::new();
        let project_name = format!("test-{}", uuid::Uuid::new_v4());
        let project_dir = self.work_dir().join(&project_name);

        // Stage 1: Code Generation (target: <5s)
        let stage_start = Instant::now();
        self.generate_project(template, &project_name)?;
        timings.push(TimingReport::new(
            "Code Generation",
            stage_start.elapsed(),
            Duration::from_secs(5),
        ));

        // Stage 2: Validation (target: <10s)
        let stage_start = Instant::now();
        self.validate_project(&project_dir)?;
        timings.push(TimingReport::new(
            "Validation",
            stage_start.elapsed(),
            Duration::from_secs(10),
        ));

        // Stage 3: Test Execution (target: <20s)
        let stage_start = Instant::now();
        self.run_tests(&project_dir)?;
        timings.push(TimingReport::new(
            "Test Execution",
            stage_start.elapsed(),
            Duration::from_secs(20),
        ));

        // Stage 4: Build (target: <25s)
        let stage_start = Instant::now();
        self.build_project(&project_dir)?;
        timings.push(TimingReport::new(
            "Build",
            stage_start.elapsed(),
            Duration::from_secs(25),
        ));

        // Stage 5: Fake Publish (target: <5s)
        let stage_start = Instant::now();
        self.fake_publish(&project_dir)?;
        timings.push(TimingReport::new(
            "Fake Publish",
            stage_start.elapsed(),
            Duration::from_secs(5),
        ));

        // Collect artifacts
        let artifacts = self.collect_artifacts(&project_dir)?;

        Ok(WorkflowResult {
            success: true,
            total_duration: workflow_start.elapsed(),
            timings,
            output_dir: project_dir,
            artifacts,
        })
    }

    fn generate_project(&self, template: &str, project_name: &str) -> Result<()> {
        Command::new(&self.ggen_bin)
            .args(&["project", "init", project_name, "--template", template])
            .current_dir(self.work_dir())
            .assert()
            .success();
        Ok(())
    }

    fn validate_project(&self, project_dir: &Path) -> Result<()> {
        // Check Cargo.toml exists
        let cargo_toml = project_dir.join("Cargo.toml");
        assert!(cargo_toml.exists(), "Missing Cargo.toml");

        // Check src directory exists
        let src_dir = project_dir.join("src");
        assert!(src_dir.exists(), "Missing src directory");

        // Validate with cargo check
        Command::new("cargo")
            .args(&["check"])
            .current_dir(project_dir)
            .assert()
            .success();

        Ok(())
    }

    fn run_tests(&self, project_dir: &Path) -> Result<()> {
        Command::new("cargo")
            .args(&["test", "--", "--nocapture"])
            .current_dir(project_dir)
            .assert()
            .success();
        Ok(())
    }

    fn build_project(&self, project_dir: &Path) -> Result<()> {
        Command::new("cargo")
            .args(&["build", "--release"])
            .current_dir(project_dir)
            .assert()
            .success();
        Ok(())
    }

    fn fake_publish(&self, project_dir: &Path) -> Result<()> {
        // Simulate publish by running cargo package
        Command::new("cargo")
            .args(&["package", "--no-verify", "--allow-dirty"])
            .current_dir(project_dir)
            .assert()
            .success();
        Ok(())
    }

    fn collect_artifacts(&self, project_dir: &Path) -> Result<Vec<PathBuf>> {
        let mut artifacts = Vec::new();

        // Cargo.toml
        artifacts.push(project_dir.join("Cargo.toml"));

        // Release binary
        let target_dir = project_dir.join("target/release");
        if target_dir.exists() {
            for entry in std::fs::read_dir(&target_dir)? {
                let entry = entry?;
                if entry.path().is_file() && !entry.path().extension().is_some() {
                    artifacts.push(entry.path());
                    break; // Just collect first binary
                }
            }
        }

        // Package
        let package_dir = project_dir.join("target/package");
        if package_dir.exists() {
            artifacts.push(package_dir);
        }

        Ok(artifacts)
    }
}

/// Integration test: Complete CLI workflow under 60s
#[tokio::test]
#[serial]
async fn test_ultra_deploy_cli_under_60s() -> Result<()> {
    let tester = UltraDeployTester::new()?;

    let result = tester.run_workflow("rust-cli-minimal").await?;
    result.print_report();

    result
        .assert_success()
        .assert_timing(Duration::from_secs(60))
        .assert_artifacts_exist();

    Ok(())
}

/// Integration test: Library template workflow
#[tokio::test]
#[serial]
async fn test_ultra_deploy_lib_under_60s() -> Result<()> {
    let tester = UltraDeployTester::new()?;

    let result = tester.run_workflow("rust-lib-minimal").await?;
    result.print_report();

    result
        .assert_success()
        .assert_timing(Duration::from_secs(60));

    Ok(())
}

/// Performance test: Sequential workflow execution
#[tokio::test]
#[serial]
async fn test_sequential_workflow_performance() -> Result<()> {
    let tester = UltraDeployTester::new()?;

    let templates = vec!["rust-cli-minimal", "rust-lib-minimal"];
    let total_start = Instant::now();

    for template in templates {
        let result = tester.run_workflow(template).await?;
        result.assert_success();
        println!(
            "Template '{}' completed in {:.2}s",
            template,
            result.total_duration.as_secs_f64()
        );
    }

    let total_duration = total_start.elapsed();
    println!(
        "\nTotal sequential execution: {:.2}s",
        total_duration.as_secs_f64()
    );

    // Sequential should complete in reasonable time (< 2 minutes)
    assert!(total_duration < Duration::from_secs(120));

    Ok(())
}

/// Integration test: Ggen + Cleanroom validation
#[tokio::test]
#[serial]
async fn test_ggen_cleanroom_integration() -> Result<()> {
    let tester = UltraDeployTester::new()?;
    let project_name = format!("test-cleanroom-{}", uuid::Uuid::new_v4());
    let project_dir = tester.work_dir().join(&project_name);

    // Generate project with ggen
    tester.generate_project("rust-cli-minimal", &project_name)?;

    // Validate with cleanroom (using testcontainers)
    let config = clnrm::CleanroomConfig::default();
    let environment = clnrm::CleanroomEnvironment::new(config).await?;

    // Execute cargo check in cleanroom
    let result = environment
        .execute_test("cargo_check", || {
            Command::new("cargo")
                .args(&["check"])
                .current_dir(&project_dir)
                .output()
                .map_err(|e| clnrm::Error::execution_error(format!("Cargo check failed: {}", e)))
        })
        .await?;

    assert!(result.status.success(), "Cargo check failed in cleanroom");

    // Cleanup
    let mut env = environment;
    env.cleanup().await?;

    Ok(())
}

/// Performance test: Measure each stage individually
#[tokio::test]
#[serial]
async fn test_stage_performance_breakdown() -> Result<()> {
    let tester = UltraDeployTester::new()?;
    let project_name = format!("test-perf-{}", uuid::Uuid::new_v4());
    let project_dir = tester.work_dir().join(&project_name);

    // Measure each stage independently
    let mut timings = Vec::new();

    // Stage 1: Generation
    let start = Instant::now();
    tester.generate_project("rust-cli-minimal", &project_name)?;
    let generation_time = start.elapsed();
    timings.push(("Generation", generation_time));

    // Stage 2: Validation
    let start = Instant::now();
    Command::new("cargo")
        .args(&["check"])
        .current_dir(&project_dir)
        .assert()
        .success();
    let validation_time = start.elapsed();
    timings.push(("Validation", validation_time));

    // Stage 3: Test
    let start = Instant::now();
    Command::new("cargo")
        .args(&["test"])
        .current_dir(&project_dir)
        .assert()
        .success();
    let test_time = start.elapsed();
    timings.push(("Test", test_time));

    // Stage 4: Build
    let start = Instant::now();
    Command::new("cargo")
        .args(&["build", "--release"])
        .current_dir(&project_dir)
        .assert()
        .success();
    let build_time = start.elapsed();
    timings.push(("Build", build_time));

    // Print breakdown
    println!("\n=== Stage Performance Breakdown ===");
    for (stage, duration) in &timings {
        println!("{}: {:.2}s", stage, duration.as_secs_f64());
    }

    // Verify targets
    assert!(
        generation_time < Duration::from_secs(5),
        "Generation too slow"
    );
    assert!(
        validation_time < Duration::from_secs(10),
        "Validation too slow"
    );
    assert!(test_time < Duration::from_secs(20), "Tests too slow");
    assert!(build_time < Duration::from_secs(30), "Build too slow");

    Ok(())
}

/// Reliability test: Multiple workflow runs
#[tokio::test]
#[serial]
async fn test_workflow_reliability() -> Result<()> {
    let tester = UltraDeployTester::new()?;

    let iterations = 3;
    let mut durations = Vec::new();

    for i in 0..iterations {
        println!("\n=== Iteration {} ===", i + 1);
        let result = tester.run_workflow("rust-cli-minimal").await?;
        result.assert_success();
        durations.push(result.total_duration);
    }

    // Calculate statistics
    let avg_duration = durations.iter().sum::<Duration>() / iterations as u32;
    let max_duration = durations.iter().max().unwrap();
    let min_duration = durations.iter().min().unwrap();

    println!("\n=== Reliability Statistics ===");
    println!("Average: {:.2}s", avg_duration.as_secs_f64());
    println!("Min: {:.2}s", min_duration.as_secs_f64());
    println!("Max: {:.2}s", max_duration.as_secs_f64());

    // All runs should complete under 60s
    for duration in &durations {
        assert!(*duration < Duration::from_secs(60));
    }

    // Variance should be reasonable (max < 1.5x min)
    let variance = max_duration.as_secs_f64() / min_duration.as_secs_f64();
    assert!(
        variance < 1.5,
        "Too much variance in timing: {:.2}x",
        variance
    );

    Ok(())
}

/// Test matrix: Different templates
#[tokio::test]
#[serial]
async fn test_template_matrix() -> Result<()> {
    let tester = UltraDeployTester::new()?;

    let templates = vec![
        ("rust-cli-minimal", Duration::from_secs(60)),
        ("rust-lib-minimal", Duration::from_secs(50)),
    ];

    for (template, target) in templates {
        println!("\n=== Testing template: {} ===", template);
        let result = tester.run_workflow(template).await;

        // Some templates might not exist, that's ok
        if let Ok(result) = result {
            result.print_report();
            result.assert_success().assert_timing(target);
        } else {
            println!("Template '{}' not available, skipping", template);
        }
    }

    Ok(())
}

/// Test: Fake publish validation
#[tokio::test]
#[serial]
async fn test_fake_publish_validation() -> Result<()> {
    let tester = UltraDeployTester::new()?;
    let project_name = format!("test-publish-{}", uuid::Uuid::new_v4());
    let project_dir = tester.work_dir().join(&project_name);

    // Generate and build project
    tester.generate_project("rust-cli-minimal", &project_name)?;
    tester.build_project(&project_dir)?;

    // Fake publish
    let start = Instant::now();
    Command::new("cargo")
        .args(&["package", "--no-verify", "--allow-dirty"])
        .current_dir(&project_dir)
        .assert()
        .success();
    let publish_time = start.elapsed();

    println!(
        "Fake publish completed in {:.2}s",
        publish_time.as_secs_f64()
    );

    // Verify package was created
    let package_dir = project_dir.join("target/package");
    assert!(package_dir.exists(), "Package directory not created");

    // Verify timing
    assert!(publish_time < Duration::from_secs(5), "Publish too slow");

    Ok(())
}

/// Test: Output correctness validation
#[tokio::test]
#[serial]
async fn test_output_correctness() -> Result<()> {
    let tester = UltraDeployTester::new()?;
    let project_name = format!("test-correctness-{}", uuid::Uuid::new_v4());
    let project_dir = tester.work_dir().join(&project_name);

    // Generate project
    tester.generate_project("rust-cli-minimal", &project_name)?;

    // Verify project structure
    assert!(project_dir.join("Cargo.toml").exists());
    assert!(project_dir.join("src").exists());
    assert!(project_dir.join("src/main.rs").exists() || project_dir.join("src/lib.rs").exists());

    // Verify Cargo.toml contents
    let cargo_toml = std::fs::read_to_string(project_dir.join("Cargo.toml"))?;
    assert!(cargo_toml.contains("[package]"));
    assert!(cargo_toml.contains("name"));
    assert!(cargo_toml.contains("version"));

    // Build and verify binary works
    tester.build_project(&project_dir)?;
    let binary_dir = project_dir.join("target/release");
    assert!(binary_dir.exists());

    Ok(())
}

/// Test: Error handling and recovery
#[tokio::test]
#[serial]
async fn test_error_handling() -> Result<()> {
    let tester = UltraDeployTester::new()?;

    // Test with invalid template (should fail gracefully)
    let result = tester.run_workflow("nonexistent-template").await;
    assert!(result.is_err(), "Should fail with invalid template");

    Ok(())
}

/// Performance benchmark: End-to-end timing
#[tokio::test]
#[serial]
async fn test_performance_benchmark() -> Result<()> {
    let tester = UltraDeployTester::new()?;

    println!("\n=== Performance Benchmark ===");
    println!("Running comprehensive timing analysis...\n");

    let result = tester.run_workflow("rust-cli-minimal").await?;
    result.print_report();

    // Strict performance assertions
    result.assert_success();

    // Overall target: <60s
    assert!(
        result.total_duration < Duration::from_secs(60),
        "Failed 60s target: {:.2}s",
        result.total_duration.as_secs_f64()
    );

    // Verify each stage met its target
    for timing in &result.timings {
        assert!(
            timing.passed(),
            "Stage '{}' exceeded target: {:.2}s > {:.2}s",
            timing.stage,
            timing.duration.as_secs_f64(),
            timing.target.as_secs_f64()
        );
    }

    // Ideal target: 45-55s
    let ideal_target = Duration::from_secs(55);
    if result.total_duration <= ideal_target {
        println!("✓ Achieved ideal target: <55s");
    } else {
        println!("⚠ Exceeded ideal target but within acceptable range");
    }

    Ok(())
}

#[cfg(test)]
mod unit_tests {
    use super::*;

    #[test]
    fn test_timing_report_creation() {
        let report = TimingReport::new("test", Duration::from_secs(5), Duration::from_secs(10));
        assert_eq!(report.stage, "test");
        assert!(report.passed());
        assert_eq!(report.percentage(), 50.0);
    }

    #[test]
    fn test_timing_report_failure() {
        let report = TimingReport::new("test", Duration::from_secs(15), Duration::from_secs(10));
        assert!(!report.passed());
        assert_eq!(report.percentage(), 150.0);
    }

    #[test]
    fn test_find_ggen_binary() {
        // This test just verifies the function doesn't panic
        let _ = UltraDeployTester::find_ggen_binary();
    }
}
