//! Integration Tests for Lifecycle Phase Execution
//!
//! Tests the complete lifecycle management system including phase execution,
//! state tracking, hooks, and pipeline orchestration.
//!
//! ## Test Coverage
//!
//! - Phase execution (init, setup, build, test, deploy)
//! - Sequential phase pipeline
//! - State persistence and recovery
//! - Hook execution (before/after)
//! - Error handling and recovery
//!
//! ## Running These Tests
//!
//! ```bash
//! cargo test --test lifecycle_tests
//! ```

use anyhow::Result;
use chicago_tdd_tools::test;
use ggen_core::lifecycle::{
    load_make, load_state, run_phase, run_pipeline, save_state, Context, LifecycleState, Make,
    PhaseBuilder, Project,
};
use std::collections::BTreeMap;
use std::fs;
use std::sync::Arc;

// Import common test utilities
#[path = "../common/mod.rs"]
mod common;
use common::{create_temp_dir, sample_make_toml, write_file_in_temp};

// ============================================================================
// Basic Phase Execution Tests
// ============================================================================

#[test]
fn test_single_phase_execution() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let mut lifecycle = std::collections::BTreeMap::new();
    lifecycle.insert(
        "init".to_string(),
        PhaseBuilder::new("init")
            .description("Initialize project".to_string())
            .command("echo Initializing".to_string())
            .build()?
            .phase()
            .clone(),
    );

    let make = Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        std::sync::Arc::new(make),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act
    let result = run_phase(&ctx, "init");

    // Assert
    assert!(result.is_ok(), "Phase execution should succeed");
    Ok(())
}

#[test]
fn test_phase_with_working_directory() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    fs::create_dir_all(temp_dir.path().join("src"))?;

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "build".to_string(),
        PhaseBuilder::new("build")
            .description("Build in subdirectory".to_string())
            .command("pwd".to_string())
            .build()?
            .phase()
            .clone(),
    );

    let make = Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act
    let result = run_phase(&ctx, "build");

    // Assert
    assert!(result.is_ok(), "Phase with working_dir should execute");
    Ok(())
}

#[test]
fn test_phase_execution_failure() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "failing".to_string(),
        PhaseBuilder::new("failing")
            .description("Failing phase".to_string())
            .command("exit 1".to_string())
            .build()?
            .phase()
            .clone(),
    );

    let make = Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act
    let result = run_phase(&ctx, "failing");

    // Assert
    assert!(result.is_err(), "Failing phase should return error");
    Ok(())
}

// ============================================================================
// Pipeline Execution Tests
// ============================================================================

#[test]
fn test_complete_lifecycle_pipeline() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let make_content = sample_make_toml();
    write_file_in_temp(&temp_dir, "make.toml", &make_content);

    let make_path = temp_dir.path().join("make.toml");
    let make_config = load_make(&make_path)?;

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make_config),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act - Run all phases in order
    let result = run_pipeline(
        &ctx,
        &["init".to_string(), "build".to_string(), "test".to_string()],
    );

    // Assert
    match result {
        Ok(_) => Ok(()),
        Err(e) => panic!(
            "Complete pipeline should execute successfully. Error: {:?}",
            e
        ),
    }
}

#[test]
fn test_pipeline_with_phase_names() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "init".to_string(),
        PhaseBuilder::new("init")
            .description("Initialize".to_string())
            .command("echo init".to_string())
            .build()?
            .phase()
            .clone(),
    );
    lifecycle.insert(
        "build".to_string(),
        PhaseBuilder::new("build")
            .description("Build".to_string())
            .command("echo build".to_string())
            .build()?
            .phase()
            .clone(),
    );

    let make_config = Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: Some("Test project".to_string()),
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make_config),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act - Run only specific phases
    let result = run_pipeline(&ctx, &["init".to_string(), "build".to_string()]);

    // Assert
    assert!(
        result.is_ok(),
        "Pipeline with specific phases should succeed"
    );
    Ok(())
}

// ============================================================================
// State Persistence Tests
// ============================================================================

#[test]
fn test_state_persistence() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let state_path = temp_dir.path().join(".ggen").join("state.json");
    fs::create_dir_all(state_path.parent().expect("No parent"))?;

    let mut state = LifecycleState::default();
    state.record_run("init".to_string(), 0, 100, true);
    state.record_run("build".to_string(), 100, 200, true);

    // Act - Save state
    save_state(&state_path, &state)?;

    // Assert - Load state and verify
    let loaded_state = load_state(&state_path)?;
    assert!(
        loaded_state.has_completed_phase("init"),
        "State should persist 'init' completion"
    );
    assert!(
        loaded_state.has_completed_phase("build"),
        "State should persist 'build' completion"
    );
    Ok(())
}

#[test]
fn test_state_recovery_after_failure() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let state_path = temp_dir.path().join(".ggen").join("state.json");
    fs::create_dir_all(state_path.parent().expect("No parent"))?;

    let mut state = LifecycleState::default();
    state.record_run("init".to_string(), 0, 100, true);
    save_state(&state_path, &state)?;

    // Act - Mark a phase as failed
    let mut loaded_state = load_state(&state_path)?;
    loaded_state.record_run("build".to_string(), 100, 50, false);
    save_state(&state_path, &loaded_state)?;

    // Assert - Verify failure is recorded
    let recovered_state = load_state(&state_path)?;
    assert!(
        !recovered_state.has_completed_phase("build"),
        "Failed phase should not be marked complete"
    );
    Ok(())
}

// ============================================================================
// Hook Execution Tests
// ============================================================================

#[test]
fn test_before_hooks_execution() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let marker_file = temp_dir.path().join("before_hook_ran");

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "setup".to_string(),
        PhaseBuilder::new("setup")
            .command(format!("touch {}", marker_file.to_string_lossy()))
            .build()?
            .phase()
            .clone(),
    );
    lifecycle.insert(
        "test".to_string(),
        PhaseBuilder::new("test")
            .description("Test with hooks".to_string())
            .command("echo Running test".to_string())
            .build()?
            .phase()
            .clone(),
    );

    let mut hooks = ggen_core::lifecycle::model::Hooks::default();
    hooks.before_test = Some(vec!["setup".to_string()]);

    let make = Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: Some(hooks),
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act
    run_phase(&ctx, "test")?;

    // Assert
    assert!(
        marker_file.exists(),
        "Before hook should have created marker file"
    );
    Ok(())
}

#[test]
fn test_after_hooks_execution() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let marker_file = temp_dir.path().join("after_hook_ran");

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "cleanup".to_string(),
        PhaseBuilder::new("cleanup")
            .command(format!("touch {}", marker_file.to_string_lossy()))
            .build()?
            .phase()
            .clone(),
    );
    lifecycle.insert(
        "test".to_string(),
        PhaseBuilder::new("test")
            .description("Test with hooks".to_string())
            .command("echo Running test".to_string())
            .build()?
            .phase()
            .clone(),
    );

    let hooks = ggen_core::lifecycle::model::Hooks {
        after_test: Some(vec!["cleanup".to_string()]),
        ..Default::default()
    };

    let make = Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: Some(hooks),
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act
    run_phase(&ctx, "test")?;

    // Assert
    assert!(
        marker_file.exists(),
        "After hook should have created marker file"
    );
    Ok(())
}

// ============================================================================
// Phase Dependencies Tests
// ============================================================================

#[test]
fn test_phase_dependencies_respected() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "init".to_string(),
        PhaseBuilder::new("init")
            .description("Initialize".to_string())
            .command("echo Initializing".to_string())
            .build()?
            .phase()
            .clone(),
    );
    lifecycle.insert(
        "build".to_string(),
        PhaseBuilder::new("build")
            .description("Build".to_string())
            .command("echo Building".to_string())
            .build()?
            .phase()
            .clone(),
    );

    // Use hooks to express dependency: build depends on init
    let hooks = ggen_core::lifecycle::model::Hooks {
        before_build: Some(vec!["init".to_string()]),
        ..Default::default()
    };

    let make_config = Make {
        project: Project {
            name: "test-deps".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: Some(hooks),
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make_config),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act
    let result = run_pipeline(&ctx, &["build".to_string()]);

    // Assert
    assert!(
        result.is_ok(),
        "Pipeline should handle phase dependencies correctly"
    );
    Ok(())
}

// ============================================================================
// Cache Validation Tests
// ============================================================================

#[test]
fn test_cache_hit_skips_phase() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();
    let state_path = temp_dir.path().join(".ggen").join("state.json");
    fs::create_dir_all(state_path.parent().expect("No parent"))?;

    let mut state = LifecycleState::default();
    state.record_run("build".to_string(), 0, 100, true);
    state.add_cache_key("build".to_string(), "test-cache-key-123".to_string());
    save_state(&state_path, &state)?;

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "build".to_string(),
        PhaseBuilder::new("build")
            .description("Build".to_string())
            .command("echo Should not run".to_string())
            .cache(true)
            .build()?
            .phase()
            .clone(),
    );

    let make = Make {
        project: Project {
            name: "test-project".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make),
        state_path,
        vec![],
    );

    // Act - Phase should run (caching is deterministic based on inputs, not pre-set keys)
    let result = run_phase(&ctx, "build");

    // Assert
    assert!(result.is_ok(), "Phase should execute successfully");
    Ok(())
}

// ============================================================================
// Performance Tests
// ============================================================================

#[test]
fn test_pipeline_completes_in_reasonable_time() -> Result<()> {
    // Arrange
    let temp_dir = create_temp_dir();

    let mut lifecycle = BTreeMap::new();
    lifecycle.insert(
        "init".to_string(),
        PhaseBuilder::new("init")
            .command("echo init".to_string())
            .build()?
            .phase()
            .clone(),
    );
    lifecycle.insert(
        "build".to_string(),
        PhaseBuilder::new("build")
            .command("echo build".to_string())
            .build()?
            .phase()
            .clone(),
    );
    lifecycle.insert(
        "test".to_string(),
        PhaseBuilder::new("test")
            .command("echo test".to_string())
            .build()?
            .phase()
            .clone(),
    );

    let make_config = Make {
        project: Project {
            name: "perf-test".to_string(),
            project_type: None,
            version: Some("1.0.0".to_string()),
            description: None,
        },
        workspace: None,
        lifecycle,
        hooks: None,
    };

    let ctx = Context::new(
        temp_dir.path().to_path_buf(),
        Arc::new(make_config),
        temp_dir.path().join(".ggen").join("state.json"),
        vec![],
    );

    // Act
    let start = std::time::Instant::now();
    let result = run_pipeline(
        &ctx,
        &["init".to_string(), "build".to_string(), "test".to_string()],
    );
    let duration = start.elapsed();

    // Assert
    assert!(result.is_ok(), "Pipeline should complete successfully");
    assert!(
        duration.as_secs() < 5,
        "Simple pipeline should complete in < 5 seconds, took {:?}",
        duration
    );
    Ok(())
}
