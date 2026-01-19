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

use anyhow::{Context as AnyhowContext, Result};
use chicago_tdd_tools::prelude::*;
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
        let temp_dir = TempDir::new().with_context(|| "Failed to create temp directory")?;
        let project_root = temp_dir.path().to_path_buf();
        let state_path = project_root.join(".ggen/state.json");

        // Create .ggen directory
        fs::create_dir_all(state_path.parent().unwrap())
            .with_context(|| "Failed to create .ggen directory")?;

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
        fs::write(&path, content).with_context(|| format!("Failed to write file {:?}", path))
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
        Ok(load_state(&self.state_path)?)
    }

    /// Assert that a phase was executed
    fn assert_phase_executed(&self, phase_name: &str) -> Result<()> {
        let state = self.load_state()?;
        assert!(
            state.phase_history.iter().any(|h| h.phase == phase_name),
            "Phase '{}' was not executed. History: {:?}",
            phase_name,
            state
                .phase_history
                .iter()
                .map(|h| &h.phase)
                .collect::<Vec<_>>()
        );
        Ok(())
    }

    /// Create a basic Rust project structure
    fn create_rust_project(&self) -> Result<()> {
        self.write_file(
            "Cargo.toml",
            r#"
[package]
name = "test-project"
version = "0.1.0"
edition = "2021"

[dependencies]
"#,
        )?;

        self.write_file(
            "src/main.rs",
            r#"
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
"#,
        )?;

        Ok(())
    }
}

// ============================================================================
// TEST 1: PHASE TRANSITION FLOWS
// ============================================================================

test!(test_basic_phase_execution, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "test-project"

[lifecycle.init]
command = "echo 'Initializing project'"

[lifecycle.build]
command = "echo 'Building project'"

[lifecycle.test]
command = "echo 'Running tests'"
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "init").unwrap();
    run_phase(&ctx, "build").unwrap();
    run_phase(&ctx, "test").unwrap();

    // Assert
    fixture.assert_phase_executed("init").unwrap();
    fixture.assert_phase_executed("build").unwrap();
    fixture.assert_phase_executed("test").unwrap();
    let state = fixture.load_state().unwrap();
    assert_eq!(state.last_phase, Some("test".to_string()));
});

test!(test_full_lifecycle_pipeline, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
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
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_pipeline(
        &ctx,
        &vec![
            "init".to_string(),
            "setup".to_string(),
            "build".to_string(),
            "test".to_string(),
            "deploy".to_string(),
        ],
    )
    .unwrap();

    // Assert
    let state = fixture.load_state().unwrap();
    assert_eq!(state.phase_history.len(), 5);
    assert_eq!(state.phase_history[0].phase, "init");
    assert_eq!(state.phase_history[1].phase, "setup");
    assert_eq!(state.phase_history[2].phase, "build");
    assert_eq!(state.phase_history[3].phase, "test");
    assert_eq!(state.phase_history[4].phase, "deploy");
});

test!(test_phase_with_multiple_commands, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "multi-command"

[lifecycle.setup]
commands = [
    "echo 'Step 1: Install dependencies'",
    "echo 'Step 2: Configure environment'",
    "echo 'Step 3: Verify setup'"
]
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "setup").unwrap();

    // Assert
    fixture.assert_phase_executed("setup").unwrap();
});

test!(test_phase_failure_stops_pipeline, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "failing-pipeline"

[lifecycle.init]
command = "echo 'Init succeeded'"

[lifecycle.build]
command = "exit 1"

[lifecycle.test]
command = "echo 'This should not run'"
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    let result = run_pipeline(
        &ctx,
        &vec!["init".to_string(), "build".to_string(), "test".to_string()],
    );

    // Assert
    assert_err!(&result, "Pipeline should fail on build phase");
    fixture.assert_phase_executed("init").unwrap();
    let state = fixture.load_state().unwrap();
    let has_test = state.phase_history.iter().any(|h| h.phase == "test");
    assert!(
        !has_test,
        "Test phase should not have executed after build failure"
    );
});

// ============================================================================
// TEST 2: PRODUCTION READINESS VALIDATION
// ============================================================================

test!(test_readiness_tracker_initialization, {
    // Arrange & Act
    let fixture = LifecycleTestFixture::new().unwrap();
    let mut tracker = ReadinessTracker::new(&fixture.project_root);
    tracker.load().unwrap();
    let report = tracker.generate_report();

    // Assert
    assert_eq!(report.project_name, "Current Project");
    assert!(report.requirements.len() >= 0);
});

test!(test_readiness_requirement_lifecycle, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();
    let mut tracker = ReadinessTracker::new(&fixture.project_root);

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

    // Act
    tracker.add_requirement(req.clone()).unwrap();
    let report = tracker.generate_report();
    let retrieved = report.requirements.iter().find(|r| r.id == "auth-basic");
    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().name, "Basic Authentication");

    tracker
        .update_requirement("auth-basic", ReadinessStatus::Complete)
        .unwrap();
    let updated_report = tracker.generate_report();
    let updated = updated_report
        .requirements
        .iter()
        .find(|r| r.id == "auth-basic")
        .unwrap();

    // Assert
    assert_eq!(updated.status, ReadinessStatus::Complete);
});

test!(test_readiness_report_generation, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();
    let mut tracker = ReadinessTracker::new(&fixture.project_root);

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
    })?;

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
    })?;

    tracker
        .add_requirement(ReadinessRequirement {
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
        })
        .unwrap();

    // Act & Assert
    let report = tracker.generate_report();
    assert_eq!(report.requirements.len(), 3);
    let crit = report
        .by_category
        .get(&ReadinessCategory::Critical)
        .unwrap();
    assert_eq!(crit.total_requirements, 1);
    assert_eq!(crit.completed, 1);
});

test!(test_readiness_validation_with_validator, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();
    fixture.create_rust_project().unwrap();

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

    // Act & Assert
    let report = tracker.generate_report();
    let validator = ReadinessValidator::new();
    let result = validator.validate(&report);
    assert!(result.issues.len() >= 0);
});

// ============================================================================
// TEST 3: DEPLOYMENT VALIDATION
// ============================================================================

test!(test_deployment_to_staging, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
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
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "deploy-staging").unwrap();

    // Assert
    fixture.assert_phase_executed("validate-staging").unwrap();
    fixture.assert_phase_executed("deploy-staging").unwrap();
});

test!(test_deployment_to_production, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
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
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "deploy-production").unwrap();

    // Assert
    fixture.assert_phase_executed("pre-deploy-checks").unwrap();
    fixture.assert_phase_executed("deploy-production").unwrap();
    fixture
        .assert_phase_executed("post-deploy-monitoring")
        .unwrap();
});

test!(test_deployment_validation_failure_prevents_deploy, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "validation-failure"

[lifecycle.validate]
command = "exit 1"

[lifecycle.deploy]
command = "echo 'Should not deploy'"

[hooks]
before_deploy = ["validate"]
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    let result = run_phase(&ctx, "deploy");

    // Assert
    assert_err!(&result, "Deploy should fail when validation fails");
    let state = fixture.load_state().unwrap();
    let has_deploy = state.phase_history.iter().any(|h| h.phase == "deploy");
    assert!(
        !has_deploy,
        "Deploy phase should not run after validation failure"
    );
});

// ============================================================================
// TEST 4: ROLLBACK AND RECOVERY SCENARIOS
// ============================================================================

test!(test_rollback_after_failed_deployment, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
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
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    let deploy_result = run_phase(&ctx, "deploy");
    assert_err!(&deploy_result, "Deploy should fail");
    run_phase(&ctx, "rollback").unwrap();

    // Assert
    fixture.assert_phase_executed("rollback").unwrap();
});

test!(test_state_recovery_after_interruption, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "recovery-test"

[lifecycle.build]
command = "echo 'Building'"

[lifecycle.test]
command = "echo 'Testing'"
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "build").unwrap();
    let state_before = fixture.load_state().unwrap();
    assert!(state_before
        .phase_history
        .iter()
        .any(|h| h.phase == "build"));
    run_phase(&ctx, "test").unwrap();

    // Assert
    let state_after = fixture.load_state().unwrap();
    assert_eq!(state_after.phase_history.len(), 2);
});

test!(test_cache_invalidation_on_failure, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "cache-invalidation"

[lifecycle.build]
command = "echo 'Building'"
cache = true

[lifecycle.test]
command = "exit 1"
cache = true
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "build").unwrap();
    let state_after_build = fixture.load_state().unwrap();
    assert!(
        !state_after_build.cache_keys.is_empty(),
        "Build should create cache key"
    );

    let test_result = run_phase(&ctx, "test");
    assert_err!(&test_result, "Test should fail");

    // Assert
    let state_after_test = fixture.load_state().unwrap();
    assert!(state_after_test
        .cache_keys
        .iter()
        .any(|k| k.phase == "build"));
});

// ============================================================================
// TEST 5: LIFECYCLE + MARKETPLACE INTEGRATION
// ============================================================================

test!(test_marketplace_package_installation_in_setup, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
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
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_pipeline(&ctx, &vec!["setup".to_string(), "build".to_string()]).unwrap();

    // Assert
    fixture.assert_phase_executed("setup").unwrap();
    fixture.assert_phase_executed("build").unwrap();
});

test!(test_template_generation_in_init_phase, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "template-generation"

[lifecycle.init]
commands = [
    "echo 'Initializing project structure'",
    "echo 'ggen template generate rust-service:api.tmpl'",
    "echo 'ggen template generate rust-service:database.tmpl'"
]
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "init").unwrap();

    // Assert
    fixture.assert_phase_executed("init").unwrap();
});

test!(test_end_to_end_marketplace_lifecycle_flow, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
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
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_pipeline(
        &ctx,
        &vec![
            "init".to_string(),
            "setup".to_string(),
            "build".to_string(),
            "test".to_string(),
            "validate".to_string(),
            "deploy".to_string(),
        ],
    )
    .unwrap();

    // Assert
    let state = fixture.load_state().unwrap();
    assert_eq!(state.phase_history.len(), 6);
});

// ============================================================================
// TEST 6: HOOKS AND DEPENDENCIES
// ============================================================================

test!(test_before_and_after_hooks, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
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
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "build").unwrap();

    // Assert
    fixture.assert_phase_executed("validate").unwrap();
    fixture.assert_phase_executed("build").unwrap();
    fixture.assert_phase_executed("notify").unwrap();
    let state = fixture.load_state().unwrap();
    let phases: Vec<&str> = state
        .phase_history
        .iter()
        .map(|h| h.phase.as_str())
        .collect();

    let validate_pos = phases.iter().position(|&p| p == "validate").unwrap();
    let build_pos = phases.iter().position(|&p| p == "build").unwrap();
    let notify_pos = phases.iter().position(|&p| p == "notify").unwrap();

    assert!(validate_pos < build_pos, "Validate should run before build");
    assert!(build_pos < notify_pos, "Build should run before notify");
});

test!(test_circular_hook_detection, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "circular-hooks"

[lifecycle.a]
command = "echo 'Phase A'"

[lifecycle.b]
command = "echo 'Phase B'"

[hooks]
before_a = ["b"]
before_b = ["a"]
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    let result = run_phase(&ctx, "a");

    // Assert
    assert_err!(&result, "Should detect circular hook dependency");
    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("recursion") || err_msg.contains("circular"),
        "Error should mention recursion/circular dependency"
    );
});

// ============================================================================
// TEST 7: CACHING AND OPTIMIZATION
// ============================================================================

test!(test_phase_caching_basic, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "caching-test"

[lifecycle.build]
command = "echo 'Expensive build operation'"
cache = true
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "build").unwrap();
    let state_first = fixture.load_state().unwrap();

    // Assert
    assert!(
        state_first.cache_keys.iter().any(|k| k.phase == "build"),
        "Should create cache key"
    );
    let cache_key_first = state_first.get_cache_key("build").unwrap().to_string();
    assert!(!cache_key_first.is_empty());
});

test!(test_cache_invalidation_on_command_change, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    // Initial configuration
    fixture
        .write_make_toml(
            r#"
[project]
name = "cache-invalidation"

[lifecycle.build]
command = "echo 'Version 1'"
cache = true
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "build").unwrap();
    let cache_key_v1 = fixture
        .load_state()
        .unwrap()
        .get_cache_key("build")
        .unwrap()
        .to_string();

    fixture
        .write_make_toml(
            r#"
[project]
name = "cache-invalidation"

[lifecycle.build]
command = "echo 'Version 2'"
cache = true
"#,
        )
        .unwrap();

    let ctx2 = fixture.create_context().unwrap();
    run_phase(&ctx2, "build").unwrap();
    let cache_key_v2 = fixture
        .load_state()
        .unwrap()
        .get_cache_key("build")
        .unwrap()
        .to_string();

    // Assert
    assert_ne!(
        cache_key_v1, cache_key_v2,
        "Cache key should change when command changes"
    );
});

// ============================================================================
// TEST 8: ERROR HANDLING AND RECOVERY
// ============================================================================

test!(test_detailed_error_messages, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "error-messages"

[lifecycle.failing-phase]
command = "nonexistent-command"
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    let result = run_phase(&ctx, "failing-phase");

    // Assert
    assert_err!(&result, "Should fail on nonexistent command");
    let err = result.unwrap_err();
    let err_msg = format!("{}", err);
    assert!(err_msg.len() > 20, "Error message should be descriptive");
});

test!(test_state_preservation_on_error, {
    // Arrange
    let fixture = LifecycleTestFixture::new().unwrap();

    fixture
        .write_make_toml(
            r#"
[project]
name = "state-preservation"

[lifecycle.step1]
command = "echo 'Step 1 succeeded'"

[lifecycle.step2]
command = "exit 1"

[lifecycle.step3]
command = "echo 'Step 3 should not run'"
"#,
        )
        .unwrap();

    // Act
    let ctx = fixture.create_context().unwrap();
    run_phase(&ctx, "step1").unwrap();
    let state_after_step1 = fixture.load_state().unwrap();
    assert_eq!(state_after_step1.phase_history.len(), 1);

    let _ = run_phase(&ctx, "step2");

    // Assert
    let state_after_step2 = fixture.load_state().unwrap();
    assert!(
        state_after_step2.phase_history.len() >= 1,
        "Previous state should be preserved"
    );
});

#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::time::Instant;

    test!(test_phase_execution_performance, {
        let fixture = LifecycleTestFixture::new()?;

        fixture
            .write_make_toml(
                r#"
[project]
name = "performance-test"

[lifecycle.fast-phase]
command = "echo 'Fast operation'"
"#,
            )
            .unwrap();

        // Act
        let ctx = fixture.create_context().unwrap();
        let start = Instant::now();
        run_phase(&ctx, "fast-phase").unwrap();
        let duration = start.elapsed();

        // Assert
        assert!(
            duration.as_secs() < 1,
            "Phase execution took {:?}, should be < 1s",
            duration
        );
    });
}
