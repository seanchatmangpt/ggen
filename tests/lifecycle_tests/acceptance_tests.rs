//! Outside-In Acceptance Tests (London School TDD)
//!
//! These tests represent the highest level of the system - testing from the public API
//! down through all collaborators using mocks. We start with user-facing behavior and
//! work our way inward, defining interfaces through test expectations.
//!
//! London School Principle: Test the conversation between objects, not their internals.

use super::mocks::*;
use super::mocks::assertions::*;
use std::path::PathBuf;

// ============================================================================
// HIGH-LEVEL ACCEPTANCE TESTS
// ============================================================================

/// ACCEPTANCE TEST 1: Complete Phase Execution with Hooks
///
/// This test represents the full user story:
/// "As a developer, I want to run a phase that executes commands,
///  runs before/after hooks, and updates state - all automatically"
///
/// London School Approach:
/// - Mock all collaborators (executor, state, hooks, observer)
/// - Verify the correct conversation happens between components
/// - Assert on interactions, not state
#[test]
fn acceptance_run_phase_executes_full_workflow() {
    // ARRANGE: Set up the world
    let mocks = MockSetupBuilder::new()
        .with_command_success("npm run build")
        .with_command_success("npm run test")  // before_build hook
        .build();

    // Configure phase with hooks
    let phase_config = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["npm run build".to_string()],
        before_hooks: vec!["test".to_string()],
        after_hooks: vec![],
    };

    // ACT: Execute the phase
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase_config);

    // ASSERT: Verify the conversation between objects
    assert!(result.is_ok(), "Phase execution should succeed");

    // Verify observer notified about phase lifecycle
    assert_phase_lifecycle(&mocks.observer, "build");

    // Verify hooks executed in correct order
    mocks.hook_registry.verify_hook_executed("test", HookStage::Before);

    // Verify commands executed
    assert_commands_in_order(&mocks.executor, &["npm run test", "npm run build"]);

    // Verify state was updated
    assert_state_saved(&mocks.state_repo);
    assert!(mocks.state_repo.verify_phase_recorded("build"));
    assert!(mocks.state_repo.verify_cache_key_stored("build"));
}

/// ACCEPTANCE TEST 2: Pipeline Execution
///
/// User Story: "Run multiple phases in sequence, stopping on first failure"
#[test]
fn acceptance_pipeline_executes_phases_sequentially() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_command_success("npm install")
        .with_command_success("npm run build")
        .with_command_success("npm test")
        .build();

    let phases = vec![
        PhaseConfig {
            name: "setup".to_string(),
            commands: vec!["npm install".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        },
        PhaseConfig {
            name: "build".to_string(),
            commands: vec!["npm run build".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        },
        PhaseConfig {
            name: "test".to_string(),
            commands: vec!["npm test".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        },
    ];

    // ACT
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_pipeline(&phases);

    // ASSERT
    assert!(result.is_ok(), "Pipeline should complete successfully");

    // Verify phases ran in order
    assert_commands_in_order(&mocks.executor, &[
        "npm install",
        "npm run build",
        "npm test",
    ]);

    // Verify each phase was tracked in state
    assert!(mocks.state_repo.verify_phase_recorded("setup"));
    assert!(mocks.state_repo.verify_phase_recorded("build"));
    assert!(mocks.state_repo.verify_phase_recorded("test"));

    // Verify observer notified for each phase
    assert!(mocks.observer.verify_phase_started("setup"));
    assert!(mocks.observer.verify_phase_started("build"));
    assert!(mocks.observer.verify_phase_started("test"));
}

/// ACCEPTANCE TEST 3: Error Handling - Command Failure
///
/// User Story: "When a command fails, stop execution and report the error"
#[test]
fn acceptance_command_failure_stops_execution() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_command_failure("npm run build", "Build failed: missing dependency")
        .build();

    let phase_config = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["npm run build".to_string()],
        before_hooks: vec![],
        after_hooks: vec![],
    };

    // ACT
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase_config);

    // ASSERT
    assert!(result.is_err(), "Phase should fail when command fails");

    // Verify error was reported to observer
    let events = mocks.observer.get_events();
    assert!(events.iter().any(|e| matches!(e, ObserverEvent::Error { phase, .. } if phase == "build")));

    // Verify state was NOT saved (because phase failed)
    assert!(!mocks.state_repo.verify_phase_recorded("build"));
}

/// ACCEPTANCE TEST 4: Hook Failure Propagation
///
/// User Story: "When a before hook fails, don't run the phase"
#[test]
fn acceptance_before_hook_failure_prevents_phase_execution() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_hook_failure("test", "Tests failed")
        .with_command_success("npm run build")  // Should never be called
        .build();

    let phase_config = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["npm run build".to_string()],
        before_hooks: vec!["test".to_string()],
        after_hooks: vec![],
    };

    // ACT
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase_config);

    // ASSERT
    assert!(result.is_err(), "Phase should fail when before hook fails");

    // Verify main command was NEVER executed
    assert!(!mocks.executor.verify_called("npm run build"),
           "Main command should not execute when before hook fails");

    // Verify after hooks were NEVER executed
    assert!(mocks.hook_registry.get_executed_hooks().iter()
           .all(|h| h.stage == HookStage::Before),
           "After hooks should not run when phase fails");
}

/// ACCEPTANCE TEST 5: After Hooks Run Even on Failure
///
/// User Story: "After hooks (like cleanup) should run even if the phase fails"
#[test]
fn acceptance_after_hooks_run_even_on_command_failure() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_command_failure("npm run build", "Build failed")
        .with_command_success("cleanup")  // after_build hook
        .build();

    let phase_config = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["npm run build".to_string()],
        before_hooks: vec![],
        after_hooks: vec!["cleanup".to_string()],
    };

    // ACT
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase_config);

    // ASSERT
    assert!(result.is_err(), "Phase should fail");

    // Verify cleanup hook WAS executed
    assert!(mocks.hook_registry.verify_hook_executed("cleanup", HookStage::After),
           "After hooks should run even on failure");
}

/// ACCEPTANCE TEST 6: Hook Recursion Prevention
///
/// User Story: "Prevent infinite loops when hooks reference each other"
#[test]
fn acceptance_prevents_hook_recursion() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_command_success("echo build")
        .build();

    // Phase 'build' has before_hook 'test', and 'test' has before_hook 'build' (cycle!)
    let phase_config = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["echo build".to_string()],
        before_hooks: vec!["build".to_string()],  // Recursive!
        after_hooks: vec![],
    };

    // ACT
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase_config);

    // ASSERT
    assert!(result.is_err(), "Should detect recursion");
    assert!(result.unwrap_err().contains("recursion"),
           "Error should mention recursion");
}

/// ACCEPTANCE TEST 7: State Persistence
///
/// User Story: "Track execution history and cache keys for each phase"
#[test]
fn acceptance_state_tracks_execution_history() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_command_success("npm run build")
        .build();

    let phase_config = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["npm run build".to_string()],
        before_hooks: vec![],
        after_hooks: vec![],
    };

    // ACT
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    executor.run_phase(&phase_config).unwrap();

    // ASSERT
    let state = mocks.state_repo.get_state();

    // Verify phase history
    assert_eq!(state.last_phase, Some("build".to_string()));
    assert!(state.phase_history.iter().any(|r| r.phase == "build" && r.success));

    // Verify timing information is recorded
    let build_record = state.phase_history.iter().find(|r| r.phase == "build").unwrap();
    assert!(build_record.duration_ms > 0, "Should record execution time");

    // Verify cache key
    assert!(state.cache_keys.contains_key("build"), "Should store cache key");
}

// ============================================================================
// PHASE EXECUTOR (System Under Test - Designed Through Tests)
// ============================================================================

/// PhaseExecutor coordinates the execution of lifecycle phases
///
/// This design emerged from our acceptance tests. The London School approach
/// means we designed the interface by writing tests first, then implemented
/// the minimum code to make tests pass.
pub struct PhaseExecutor {
    command_executor: MockCommandExecutor,
    state_repo: MockStateRepository,
    hook_registry: MockHookRegistry,
    observer: MockObserver,
}

impl PhaseExecutor {
    pub fn new(
        command_executor: MockCommandExecutor,
        state_repo: MockStateRepository,
        hook_registry: MockHookRegistry,
        observer: MockObserver,
    ) -> Self {
        Self {
            command_executor,
            state_repo,
            hook_registry,
            observer,
        }
    }

    /// Run a single phase with hooks
    pub fn run_phase(&self, phase: &PhaseConfig) -> Result<(), String> {
        // Notify phase started
        self.observer.on_phase_start(&phase.name);

        let start_time = current_time_ms();

        // Execute before hooks
        if let Err(e) = self.hook_registry.execute_before_hooks(&phase.name, &phase.before_hooks) {
            self.observer.on_error(&phase.name, &e);
            return Err(e);
        }

        // Execute main commands
        let mut command_failed = false;
        for cmd in &phase.commands {
            match self.command_executor.execute(cmd, &PathBuf::from("."), &[]) {
                Ok(response) => {
                    self.observer.on_command_executed(cmd, response.success);
                }
                Err(e) => {
                    command_failed = true;
                    self.observer.on_error(&phase.name, &e);

                    // Still run after hooks for cleanup
                    let _ = self.hook_registry.execute_after_hooks(&phase.name, &phase.after_hooks);

                    return Err(e);
                }
            }
        }

        // Execute after hooks
        if let Err(e) = self.hook_registry.execute_after_hooks(&phase.name, &phase.after_hooks) {
            self.observer.on_error(&phase.name, &e);
            return Err(e);
        }

        // Calculate duration
        let duration = current_time_ms() - start_time;

        // Update state
        let mut state = self.state_repo.load().unwrap_or_default();
        state.last_phase = Some(phase.name.clone());
        state.phase_history.push(RunRecordData {
            phase: phase.name.clone(),
            started_ms: start_time,
            duration_ms: duration,
            success: !command_failed,
        });

        // Generate and store cache key
        let cache_key = generate_cache_key(&phase.name, &phase.commands);
        state.cache_keys.insert(phase.name.clone(), cache_key);

        self.state_repo.save(&state).map_err(|e| format!("Failed to save state: {}", e))?;

        // Notify completion
        self.observer.on_phase_complete(&phase.name, true, duration);

        Ok(())
    }

    /// Run multiple phases in sequence
    pub fn run_pipeline(&self, phases: &[PhaseConfig]) -> Result<(), String> {
        for phase in phases {
            self.run_phase(phase)?;
        }
        Ok(())
    }
}

// ============================================================================
// TEST DATA STRUCTURES
// ============================================================================

#[derive(Debug, Clone)]
pub struct PhaseConfig {
    pub name: String,
    pub commands: Vec<String>,
    pub before_hooks: Vec<String>,
    pub after_hooks: Vec<String>,
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

fn current_time_ms() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis()
}

fn generate_cache_key(phase: &str, commands: &[String]) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    phase.hash(&mut hasher);
    for cmd in commands {
        cmd.hash(&mut hasher);
    }
    format!("{:x}", hasher.finish())
}
