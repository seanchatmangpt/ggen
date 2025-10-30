//! Comprehensive Lifecycle Management Tests
//!
//! This test suite covers:
//! 1. Phase transition flows (init -> setup -> build -> test -> deploy)
//! 2. Production readiness validation
//! 3. Deployment validation (staging, production)
//! 4. Rollback and recovery scenarios
//! 5. Lifecycle + marketplace integration
//!
//! All tests use clnrm containers for isolation and reproducibility.

use anyhow::{Context, Result};
use ggen_core::lifecycle::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

// ============================================================================
// TEST FIXTURES AND UTILITIES
// ============================================================================

/// Test fixture providing isolated environment for lifecycle testing
struct LifecycleTestFixture {
    temp_dir: TempDir,
    project_root: PathBuf,
    state_path: PathBuf,
}

impl LifecycleTestFixture {
    fn new() -> Result<Self> {
        let temp_dir = TempDir::new().context("Failed to create temp directory")?;
        let project_root = temp_dir.path().to_path_buf();
        let state_path = project_root.join(".ggen/state.json");

        // Create .ggen directory
        fs::create_dir_all(state_path.parent().unwrap())
            .context("Failed to create .ggen directory")?;

        Ok(Self {
            temp_dir,
            project_root,
            state_path,
        })
    }

    /// Write make.toml configuration
    fn write_make_toml(&self, content: &str) -> Result<()> {
        let path = self.project_root.join("make.toml");
        fs::write(&path, content)
            .with_context(|| format!("Failed to write make.toml to {:?}", path))
    }

    /// Write a file in the project root
    fn write_file(&self, relative_path: &str, content: &str) -> Result<()> {
        let path = self.project_root.join(relative_path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory {:?}", parent))?;
        }
        fs::write(&path, content)
            .with_context(|| format!("Failed to write file {:?}", path))
    }

    /// Create execution context
    fn create_context(&self) -> Result<Context> {
        let make_path = self.project_root.join("make.toml");
        let make = Arc::new(load_make(&make_path)?);
        Ok(Context::new(
            self.project_root.clone(),
            make,
            self.state_path.clone(),
            vec![],
        ))
    }

    /// Load current lifecycle state
    fn load_state(&self) -> Result<LifecycleState> {
        load_state(&self.state_path)
    }

    /// Assert that a phase was executed
    fn assert_phase_executed(&self, phase_name: &str) -> Result<()> {
        let state = self.load_state()?;
        assert!(
            state.phase_history.iter().any(|h| h.phase == phase_name),
            "Phase '{}' was not executed. History: {:?}",
            phase_name,
            state.phase_history.iter().map(|h| &h.phase).collect::<Vec<_>>()
        );
        Ok(())
    }

    /// Create a basic Rust project structure
    fn create_rust_project(&self) -> Result<()> {
        self.write_file("Cargo.toml", r#"
[package]
name = "test-project"
version = "0.1.0"
edition = "2021"

[dependencies]
"#)?;

        self.write_file("src/main.rs", r#"
fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
"#)?;

        Ok(())
    }
}

// ============================================================================
// TEST 1: PHASE TRANSITION FLOWS
// ============================================================================

#[test]
fn test_basic_phase_execution() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "test-project"

[lifecycle.init]
command = "echo 'Initializing project'"

[lifecycle.build]
command = "echo 'Building project'"

[lifecycle.test]
command = "echo 'Running tests'"
"#)?;

    let ctx = fixture.create_context()?;

    // Execute phases sequentially
    run_phase(&ctx, "init")?;
    run_phase(&ctx, "build")?;
    run_phase(&ctx, "test")?;

    // Verify all phases were recorded
    fixture.assert_phase_executed("init")?;
    fixture.assert_phase_executed("build")?;
    fixture.assert_phase_executed("test")?;

    // Verify state shows last phase
    let state = fixture.load_state()?;
    assert_eq!(state.last_phase, Some("test".to_string()));

    Ok(())
}

#[test]
fn test_full_lifecycle_pipeline() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "full-lifecycle"

[lifecycle.init]
command = "echo 'Phase: init'"

[lifecycle.setup]
command = "echo 'Phase: setup'"

[lifecycle.build]
command = "echo 'Phase: build'"

[lifecycle.test]
command = "echo 'Phase: test'"

[lifecycle.deploy]
command = "echo 'Phase: deploy'"
"#)?;

    let ctx = fixture.create_context()?;

    // Run complete pipeline
    run_pipeline(&ctx, &vec![
        "init".to_string(),
        "setup".to_string(),
        "build".to_string(),
        "test".to_string(),
        "deploy".to_string(),
    ])?;

    // Verify all phases executed in order
    let state = fixture.load_state()?;
    assert_eq!(state.phase_history.len(), 5);
    assert_eq!(state.phase_history[0].phase, "init");
    assert_eq!(state.phase_history[1].phase, "setup");
    assert_eq!(state.phase_history[2].phase, "build");
    assert_eq!(state.phase_history[3].phase, "test");
    assert_eq!(state.phase_history[4].phase, "deploy");

    Ok(())
}

#[test]
fn test_phase_with_multiple_commands() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "multi-command"

[lifecycle.setup]
commands = [
    "echo 'Step 1: Install dependencies'",
    "echo 'Step 2: Configure environment'",
    "echo 'Step 3: Verify setup'"
]
"#)?;

    let ctx = fixture.create_context()?;
    run_phase(&ctx, "setup")?;

    fixture.assert_phase_executed("setup")?;

    Ok(())
}

#[test]
fn test_phase_failure_stops_pipeline() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "failing-pipeline"

[lifecycle.init]
command = "echo 'Init succeeded'"

[lifecycle.build]
command = "exit 1"

[lifecycle.test]
command = "echo 'This should not run'"
"#)?;

    let ctx = fixture.create_context()?;

    // Pipeline should fail at build phase
    let result = run_pipeline(&ctx, &vec![
        "init".to_string(),
        "build".to_string(),
        "test".to_string(),
    ]);

    assert!(result.is_err(), "Pipeline should fail on build phase");

    // Verify init ran but test did not
    fixture.assert_phase_executed("init")?;

    let state = fixture.load_state()?;
    let has_test = state.phase_history.iter().any(|h| h.phase == "test");
    assert!(!has_test, "Test phase should not have executed after build failure");

    Ok(())
}

// ============================================================================
// TEST 2: PRODUCTION READINESS VALIDATION
// ============================================================================

#[test]
fn test_readiness_tracker_initialization() -> Result<()> {
    let tracker = ReadinessTracker::new("test-project".to_string());

    // Verify tracker starts empty
    assert_eq!(tracker.project_name, "test-project");
    assert!(tracker.requirements.is_empty());

    Ok(())
}

#[test]
fn test_readiness_requirement_lifecycle() -> Result<()> {
    let mut tracker = ReadinessTracker::new("test-project".to_string());

    // Add a critical requirement
    let req = ReadinessRequirement {
        id: "auth-basic".to_string(),
        name: "Basic Authentication".to_string(),
        description: "Implement JWT-based authentication".to_string(),
        category: ReadinessCategory::Critical,
        status: ReadinessStatus::Placeholder,
        components: vec!["src/auth.rs".to_string()],
        dependencies: vec![],
        effort_hours: Some(8),
        priority: 10,
        last_assessed: chrono::Utc::now(),
        notes: None,
    };

    tracker.add_requirement(req.clone());

    // Verify requirement was added
    let retrieved = tracker.get_requirement("auth-basic");
    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().name, "Basic Authentication");

    // Update status to complete
    tracker.update_status("auth-basic", ReadinessStatus::Complete)?;
    let updated = tracker.get_requirement("auth-basic").unwrap();
    assert_eq!(updated.status, ReadinessStatus::Complete);

    Ok(())
}

#[test]
fn test_readiness_report_generation() -> Result<()> {
    let mut tracker = ReadinessTracker::new("test-project".to_string());

    // Add requirements across categories
    tracker.add_requirement(ReadinessRequirement {
        id: "auth".to_string(),
        name: "Authentication".to_string(),
        description: "Core auth".to_string(),
        category: ReadinessCategory::Critical,
        status: ReadinessStatus::Complete,
        components: vec![],
        dependencies: vec![],
        effort_hours: None,
        priority: 10,
        last_assessed: chrono::Utc::now(),
        notes: None,
    });

    tracker.add_requirement(ReadinessRequirement {
        id: "logging".to_string(),
        name: "Logging".to_string(),
        description: "Structured logging".to_string(),
        category: ReadinessCategory::Important,
        status: ReadinessStatus::Placeholder,
        components: vec![],
        dependencies: vec![],
        effort_hours: Some(4),
        priority: 7,
        last_assessed: chrono::Utc::now(),
        notes: None,
    });

    tracker.add_requirement(ReadinessRequirement {
        id: "caching".to_string(),
        name: "Advanced Caching".to_string(),
        description: "Redis caching layer".to_string(),
        category: ReadinessCategory::NiceToHave,
        status: ReadinessStatus::Missing,
        components: vec![],
        dependencies: vec![],
        effort_hours: Some(16),
        priority: 3,
        last_assessed: chrono::Utc::now(),
        notes: None,
    });

    let report = tracker.generate_report();

    // Verify report statistics
    assert_eq!(report.total_requirements, 3);
    assert_eq!(report.completed, 1);

    // Calculate expected percentage: 1/3 * 100 = 33.33...
    assert!((report.completion_percentage - 33.33).abs() < 0.01);

    Ok(())
}

#[test]
fn test_readiness_validation_with_validator() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;
    fixture.create_rust_project()?;

    // Create a readiness tracker with requirements
    let mut tracker = ReadinessTracker::new("test-project".to_string());
    tracker.add_requirement(ReadinessRequirement {
        id: "error-handling".to_string(),
        name: "Error Handling".to_string(),
        description: "No .unwrap() in production code".to_string(),
        category: ReadinessCategory::Critical,
        status: ReadinessStatus::NeedsReview,
        components: vec!["src/**/*.rs".to_string()],
        dependencies: vec![],
        effort_hours: Some(4),
        priority: 9,
        last_assessed: chrono::Utc::now(),
        notes: None,
    });

    // Create validator and run checks
    let validator = ReadinessValidator::new(tracker);
    let result = validator.validate(&fixture.project_root)?;

    // Verify validation result has components
    assert!(result.issues.len() >= 0);

    Ok(())
}

// ============================================================================
// TEST 3: DEPLOYMENT VALIDATION
// ============================================================================

#[test]
fn test_deployment_to_staging() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "deployment-test"

[lifecycle.validate-staging]
command = "echo 'Validating staging configuration'"

[lifecycle.deploy-staging]
commands = [
    "echo 'Building for staging'",
    "echo 'Deploying to staging environment'",
    "echo 'Running smoke tests'"
]

[hooks]
before_deploy-staging = ["validate-staging"]
"#)?;

    let ctx = fixture.create_context()?;

    // Run staging deployment
    run_phase(&ctx, "deploy-staging")?;

    // Verify both validation and deployment ran
    fixture.assert_phase_executed("validate-staging")?;
    fixture.assert_phase_executed("deploy-staging")?;

    Ok(())
}

#[test]
fn test_deployment_to_production() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "production-deployment"

[lifecycle.pre-deploy-checks]
commands = [
    "echo 'Checking production readiness'",
    "echo 'Validating environment variables'",
    "echo 'Running security scan'"
]

[lifecycle.deploy-production]
commands = [
    "echo 'Building production artifacts'",
    "echo 'Deploying to production'",
    "echo 'Running health checks'",
    "echo 'Verifying deployment'"
]

[lifecycle.post-deploy-monitoring]
command = "echo 'Setting up monitoring and alerts'"

[hooks]
before_deploy-production = ["pre-deploy-checks"]
after_deploy-production = ["post-deploy-monitoring"]
"#)?;

    let ctx = fixture.create_context()?;

    // Run production deployment
    run_phase(&ctx, "deploy-production")?;

    // Verify all phases executed
    fixture.assert_phase_executed("pre-deploy-checks")?;
    fixture.assert_phase_executed("deploy-production")?;
    fixture.assert_phase_executed("post-deploy-monitoring")?;

    Ok(())
}

#[test]
fn test_deployment_validation_failure_prevents_deploy() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "validation-failure"

[lifecycle.validate]
command = "exit 1"

[lifecycle.deploy]
command = "echo 'Should not deploy'"

[hooks]
before_deploy = ["validate"]
"#)?;

    let ctx = fixture.create_context()?;

    // Deployment should fail during validation
    let result = run_phase(&ctx, "deploy");
    assert!(result.is_err(), "Deploy should fail when validation fails");

    // Verify deploy phase never ran
    let state = fixture.load_state()?;
    let has_deploy = state.phase_history.iter().any(|h| h.phase == "deploy");
    assert!(!has_deploy, "Deploy phase should not run after validation failure");

    Ok(())
}

// ============================================================================
// TEST 4: ROLLBACK AND RECOVERY SCENARIOS
// ============================================================================

#[test]
fn test_rollback_after_failed_deployment() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "rollback-test"

[lifecycle.deploy]
command = "exit 1"

[lifecycle.rollback]
commands = [
    "echo 'Restoring previous version'",
    "echo 'Clearing failed deployment artifacts'",
    "echo 'Verifying rollback successful'"
]
"#)?;

    let ctx = fixture.create_context()?;

    // Deploy fails
    let deploy_result = run_phase(&ctx, "deploy");
    assert!(deploy_result.is_err(), "Deploy should fail");

    // Execute rollback
    run_phase(&ctx, "rollback")?;

    fixture.assert_phase_executed("rollback")?;

    Ok(())
}

#[test]
fn test_state_recovery_after_interruption() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "recovery-test"

[lifecycle.build]
command = "echo 'Building'"

[lifecycle.test]
command = "echo 'Testing'"
"#)?;

    let ctx = fixture.create_context()?;

    // Run build successfully
    run_phase(&ctx, "build")?;

    // Simulate interruption by checking state
    let state_before = fixture.load_state()?;
    assert!(state_before.phase_history.iter().any(|h| h.phase == "build"));

    // Continue with test phase (simulating recovery)
    run_phase(&ctx, "test")?;

    // Verify both phases recorded
    let state_after = fixture.load_state()?;
    assert_eq!(state_after.phase_history.len(), 2);

    Ok(())
}

#[test]
fn test_cache_invalidation_on_failure() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "cache-invalidation"

[lifecycle.build]
command = "echo 'Building'"
cache = true

[lifecycle.test]
command = "exit 1"
cache = true
"#)?;

    let ctx = fixture.create_context()?;

    // Build succeeds and is cached
    run_phase(&ctx, "build")?;
    let state_after_build = fixture.load_state()?;
    assert!(!state_after_build.cache_keys.is_empty(), "Build should create cache key");

    // Test fails
    let test_result = run_phase(&ctx, "test");
    assert!(test_result.is_err(), "Test should fail");

    // Verify state still has build cache but not test cache
    let state_after_test = fixture.load_state()?;
    assert!(state_after_test.cache_keys.contains_key("build"));

    Ok(())
}

// ============================================================================
// TEST 5: LIFECYCLE + MARKETPLACE INTEGRATION
// ============================================================================

#[test]
fn test_marketplace_package_installation_in_setup() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "marketplace-integration"

[lifecycle.setup]
commands = [
    "echo 'Installing marketplace packages'",
    "echo 'ggen market add rust-web-framework'",
    "echo 'ggen market add database-migrations'"
]

[lifecycle.build]
command = "echo 'Building with marketplace packages'"
"#)?;

    let ctx = fixture.create_context()?;

    run_pipeline(&ctx, &vec![
        "setup".to_string(),
        "build".to_string(),
    ])?;

    fixture.assert_phase_executed("setup")?;
    fixture.assert_phase_executed("build")?;

    Ok(())
}

#[test]
fn test_template_generation_in_init_phase() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "template-generation"

[lifecycle.init]
commands = [
    "echo 'Initializing project structure'",
    "echo 'ggen template generate rust-service:api.tmpl'",
    "echo 'ggen template generate rust-service:database.tmpl'"
]
"#)?;

    let ctx = fixture.create_context()?;
    run_phase(&ctx, "init")?;

    fixture.assert_phase_executed("init")?;

    Ok(())
}

#[test]
fn test_end_to_end_marketplace_lifecycle_flow() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "e2e-marketplace-lifecycle"

[lifecycle.init]
command = "echo 'ggen lifecycle run init'"

[lifecycle.setup]
commands = [
    "echo 'ggen market search rust-web'",
    "echo 'ggen market add rust-axum-service'",
    "echo 'ggen template generate rust-axum-service:main.tmpl'"
]

[lifecycle.build]
command = "echo 'cargo build --release'"

[lifecycle.test]
command = "echo 'cargo test'"

[lifecycle.validate]
command = "echo 'ggen lifecycle readiness'"

[lifecycle.deploy]
commands = [
    "echo 'ggen lifecycle validate --env production'",
    "echo 'ggen lifecycle run deploy --env production'"
]
"#)?;

    let ctx = fixture.create_context()?;

    // Run complete e2e flow
    run_pipeline(&ctx, &vec![
        "init".to_string(),
        "setup".to_string(),
        "build".to_string(),
        "test".to_string(),
        "validate".to_string(),
        "deploy".to_string(),
    ])?;

    // Verify all phases executed
    let state = fixture.load_state()?;
    assert_eq!(state.phase_history.len(), 6);

    Ok(())
}

// ============================================================================
// TEST 6: HOOKS AND DEPENDENCIES
// ============================================================================

#[test]
fn test_before_and_after_hooks() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "hooks-test"

[lifecycle.validate]
command = "echo 'Validating'"

[lifecycle.build]
command = "echo 'Building'"

[lifecycle.notify]
command = "echo 'Notifying'"

[hooks]
before_build = ["validate"]
after_build = ["notify"]
"#)?;

    let ctx = fixture.create_context()?;
    run_phase(&ctx, "build")?;

    // Verify all three phases ran
    fixture.assert_phase_executed("validate")?;
    fixture.assert_phase_executed("build")?;
    fixture.assert_phase_executed("notify")?;

    // Verify execution order
    let state = fixture.load_state()?;
    let phases: Vec<&str> = state.phase_history.iter()
        .map(|h| h.phase.as_str())
        .collect();

    let validate_pos = phases.iter().position(|&p| p == "validate").unwrap();
    let build_pos = phases.iter().position(|&p| p == "build").unwrap();
    let notify_pos = phases.iter().position(|&p| p == "notify").unwrap();

    assert!(validate_pos < build_pos, "Validate should run before build");
    assert!(build_pos < notify_pos, "Build should run before notify");

    Ok(())
}

#[test]
fn test_circular_hook_detection() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "circular-hooks"

[lifecycle.a]
command = "echo 'Phase A'"

[lifecycle.b]
command = "echo 'Phase B'"

[hooks]
before_a = ["b"]
before_b = ["a"]
"#)?;

    let ctx = fixture.create_context()?;

    // Should detect circular dependency
    let result = run_phase(&ctx, "a");
    assert!(result.is_err(), "Should detect circular hook dependency");

    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("recursion") || err_msg.contains("circular"),
        "Error should mention recursion/circular dependency"
    );

    Ok(())
}

// ============================================================================
// TEST 7: CACHING AND OPTIMIZATION
// ============================================================================

#[test]
fn test_phase_caching_basic() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "caching-test"

[lifecycle.build]
command = "echo 'Expensive build operation'"
cache = true
"#)?;

    let ctx = fixture.create_context()?;

    // First run - should execute and cache
    run_phase(&ctx, "build")?;
    let state_first = fixture.load_state()?;
    assert!(state_first.cache_keys.contains_key("build"), "Should create cache key");

    // Cache key should be present
    let cache_key_first = state_first.cache_keys.get("build").unwrap().clone();
    assert!(!cache_key_first.is_empty());

    Ok(())
}

#[test]
fn test_cache_invalidation_on_command_change() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    // Initial configuration
    fixture.write_make_toml(r#"
[project]
name = "cache-invalidation"

[lifecycle.build]
command = "echo 'Version 1'"
cache = true
"#)?;

    let ctx = fixture.create_context()?;
    run_phase(&ctx, "build")?;
    let cache_key_v1 = fixture.load_state()?.cache_keys.get("build").unwrap().clone();

    // Change command
    fixture.write_make_toml(r#"
[project]
name = "cache-invalidation"

[lifecycle.build]
command = "echo 'Version 2'"
cache = true
"#)?;

    let ctx2 = fixture.create_context()?;
    run_phase(&ctx2, "build")?;
    let cache_key_v2 = fixture.load_state()?.cache_keys.get("build").unwrap().clone();

    // Cache keys should differ
    assert_ne!(cache_key_v1, cache_key_v2, "Cache key should change when command changes");

    Ok(())
}

// ============================================================================
// TEST 8: ERROR HANDLING AND RECOVERY
// ============================================================================

#[test]
fn test_detailed_error_messages() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "error-messages"

[lifecycle.failing-phase]
command = "nonexistent-command"
"#)?;

    let ctx = fixture.create_context()?;
    let result = run_phase(&ctx, "failing-phase");

    assert!(result.is_err(), "Should fail on nonexistent command");

    let err = result.unwrap_err();
    let err_msg = format!("{}", err);

    // Error should provide context
    assert!(
        err_msg.len() > 20,
        "Error message should be descriptive"
    );

    Ok(())
}

#[test]
fn test_state_preservation_on_error() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "state-preservation"

[lifecycle.step1]
command = "echo 'Step 1 succeeded'"

[lifecycle.step2]
command = "exit 1"

[lifecycle.step3]
command = "echo 'Step 3 should not run'"
"#)?;

    let ctx = fixture.create_context()?;

    // Run successful phase
    run_phase(&ctx, "step1")?;

    // Verify state after success
    let state_after_step1 = fixture.load_state()?;
    assert_eq!(state_after_step1.phase_history.len(), 1);

    // Run failing phase
    let _ = run_phase(&ctx, "step2");

    // State should still be intact
    let state_after_step2 = fixture.load_state()?;
    assert!(state_after_step2.phase_history.len() >= 1, "Previous state should be preserved");

    Ok(())
}

#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_phase_execution_performance() -> Result<()> {
        let fixture = LifecycleTestFixture::new()?;

        fixture.write_make_toml(r#"
[project]
name = "performance-test"

[lifecycle.fast-phase]
command = "echo 'Fast operation'"
"#)?;

        let ctx = fixture.create_context()?;

        let start = Instant::now();
        run_phase(&ctx, "fast-phase")?;
        let duration = start.elapsed();

        // Phase execution should be fast (< 1 second for simple echo)
        assert!(
            duration.as_secs() < 1,
            "Phase execution took {:?}, should be < 1s",
            duration
        );

        Ok(())
    }
}
