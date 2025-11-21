//! Test Helper Utilities for London School TDD
//!
//! This module provides convenience functions and builders to make tests
//! more readable and maintainable.

use super::mocks::*;
use super::acceptance_tests::PhaseConfig;
use std::path::PathBuf;

// ============================================================================
// FLUENT TEST BUILDERS
// ============================================================================

/// Builder for creating PhaseConfig in tests
pub struct PhaseConfigBuilder {
    name: String,
    commands: Vec<String>,
    before_hooks: Vec<String>,
    after_hooks: Vec<String>,
}

impl PhaseConfigBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            commands: vec![],
            before_hooks: vec![],
            after_hooks: vec![],
        }
    }

    pub fn with_command(mut self, command: impl Into<String>) -> Self {
        self.commands.push(command.into());
        self
    }

    pub fn with_commands(mut self, commands: Vec<String>) -> Self {
        self.commands = commands;
        self
    }

    pub fn with_before_hook(mut self, hook: impl Into<String>) -> Self {
        self.before_hooks.push(hook.into());
        self
    }

    pub fn with_after_hook(mut self, hook: impl Into<String>) -> Self {
        self.after_hooks.push(hook.into());
        self
    }

    pub fn build(self) -> PhaseConfig {
        PhaseConfig {
            name: self.name,
            commands: self.commands,
            before_hooks: self.before_hooks,
            after_hooks: self.after_hooks,
        }
    }
}

// ============================================================================
// COMMON TEST SCENARIOS
// ============================================================================

/// Create a simple successful phase for testing
pub fn simple_phase(name: &str, command: &str) -> PhaseConfig {
    PhaseConfigBuilder::new(name)
        .with_command(command)
        .build()
}

/// Create a phase with hooks for testing
pub fn phase_with_hooks(name: &str, command: &str, before: Vec<&str>, after: Vec<&str>) -> PhaseConfig {
    let mut builder = PhaseConfigBuilder::new(name).with_command(command);

    for hook in before {
        builder = builder.with_before_hook(hook);
    }

    for hook in after {
        builder = builder.with_after_hook(hook);
    }

    builder.build()
}

/// Create a multi-command phase for testing
pub fn multi_command_phase(name: &str, commands: Vec<&str>) -> PhaseConfig {
    PhaseConfigBuilder::new(name)
        .with_commands(commands.iter().map(|s| s.to_string()).collect())
        .build()
}

// ============================================================================
// ASSERTION HELPERS
// ============================================================================

/// Assert that execution was successful with expected side effects
pub fn assert_successful_execution(
    mocks: &MockSetup,
    phase_name: &str,
    expected_commands: &[&str],
) {
    // Verify commands executed
    for cmd in expected_commands {
        assert!(mocks.executor.verify_called(cmd),
               "Expected command '{}' to be executed", cmd);
    }

    // Verify state saved
    assert!(mocks.state_repo.verify_save_called(),
           "State should be saved after successful execution");

    // Verify observer notified
    assert!(mocks.observer.verify_phase_started(phase_name),
           "Observer should be notified of phase start");
    assert!(mocks.observer.verify_phase_completed_successfully(phase_name),
           "Observer should be notified of phase completion");
}

/// Assert that execution failed with proper error handling
pub fn assert_failed_execution(
    mocks: &MockSetup,
    phase_name: &str,
) {
    // Verify error was reported
    let events = mocks.observer.get_events();
    assert!(events.iter().any(|e| matches!(e, ObserverEvent::Error { phase, .. } if phase == phase_name)),
           "Observer should be notified of error");

    // Verify state was not corrupted (no partial save)
    assert!(!mocks.state_repo.verify_phase_recorded(phase_name),
           "Failed phase should not be recorded in state");
}

// ============================================================================
// MOCK SETUP PRESETS
// ============================================================================

/// Standard mock setup for happy path testing
pub fn standard_mocks() -> MockSetup {
    MockSetupBuilder::new().build()
}

/// Mock setup with all commands pre-configured for success
pub fn mocks_with_successful_commands(commands: &[&str]) -> MockSetup {
    let mut builder = MockSetupBuilder::new();
    for cmd in commands {
        builder = builder.with_command_success(cmd);
    }
    builder.build()
}

/// Mock setup with specific command configured to fail
pub fn mocks_with_failing_command(failing_command: &str, error: &str) -> MockSetup {
    MockSetupBuilder::new()
        .with_command_failure(failing_command, error)
        .build()
}

/// Mock setup with hook failure
pub fn mocks_with_failing_hook(hook_name: &str, error: &str) -> MockSetup {
    MockSetupBuilder::new()
        .with_hook_failure(hook_name, error)
        .build()
}

// ============================================================================
// TEST DATA FACTORIES
// ============================================================================

/// Create sample state data for testing
pub fn sample_state() -> LifecycleStateData {
    let mut state = LifecycleStateData::default();
    state.last_phase = Some("build".to_string());
    state.phase_history.push(RunRecordData {
        phase: "build".to_string(),
        started_ms: 1000,
        duration_ms: 500,
        success: true,
    });
    state.cache_keys.insert("build".to_string(), "abc123".to_string());
    state
}

/// Create empty state for testing
pub fn empty_state() -> LifecycleStateData {
    LifecycleStateData::default()
}

// ============================================================================
// VERIFICATION HELPERS
// ============================================================================

/// Verify complete phase lifecycle was executed correctly
pub fn verify_complete_lifecycle(
    mocks: &MockSetup,
    phase_name: &str,
    commands: &[&str],
) {
    // Verify observer lifecycle
    assert!(mocks.observer.verify_event_order(phase_name),
           "Phase lifecycle events should be in correct order");

    // Verify commands
    assert!(mocks.executor.verify_call_order(commands),
           "Commands should execute in correct order");

    // Verify state persistence
    assert!(mocks.state_repo.verify_load_called(),
           "Should load state before execution");
    assert!(mocks.state_repo.verify_save_called(),
           "Should save state after execution");
    assert!(mocks.state_repo.verify_phase_recorded(phase_name),
           "Should record phase in state");
}

/// Verify hook execution order
pub fn verify_hook_order(
    hook_registry: &MockHookRegistry,
    expected: &[(&str, HookStage)],
) {
    let expected_owned: Vec<(String, HookStage)> = expected
        .iter()
        .map(|(name, stage)| (name.to_string(), *stage))
        .collect();

    assert!(hook_registry.verify_execution_order(&expected_owned),
           "Hooks should execute in correct order: {:?}", expected);
}

// ============================================================================
// COMMAND CALL VERIFICATION
// ============================================================================

/// Verify a command was called with specific working directory
pub fn verify_command_called_in_directory(
    executor: &MockCommandExecutor,
    command: &str,
    expected_dir: &str,
) {
    let calls = executor.get_calls();
    let matching_call = calls.iter().find(|c| {
        c.command == command && c.working_dir == PathBuf::from(expected_dir)
    });

    assert!(matching_call.is_some(),
           "Command '{}' should be called in directory '{}'", command, expected_dir);
}

/// Verify a command was called with all expected environment variables
pub fn verify_command_env(
    executor: &MockCommandExecutor,
    command: &str,
    expected_env: &[(&str, &str)],
) {
    for (key, value) in expected_env {
        assert!(executor.verify_called_with_env(command, key, value),
               "Command '{}' should be called with {}={}", command, key, value);
    }
}

// ============================================================================
// TIME TESTING HELPERS
// ============================================================================

/// Assert that duration was recorded and is reasonable
pub fn assert_reasonable_duration(state: &LifecycleStateData, phase: &str) {
    let record = state.phase_history.iter()
        .find(|r| r.phase == phase)
        #[allow(clippy::expect_used)]
        .expect(&format!("Phase '{}' should be in history", phase));

    assert!(record.duration_ms > 0, "Duration should be positive");
    assert!(record.duration_ms < 60_000, "Duration should be less than 60 seconds for tests");
}

// ============================================================================
// PATTERN MATCHING HELPERS
// ============================================================================

/// Check if observer received specific event
pub fn observer_received_event<F>(observer: &MockObserver, predicate: F) -> bool
where
    F: Fn(&ObserverEvent) -> bool,
{
    observer.get_events().iter().any(predicate)
}

/// Check if state contains specific phase record
pub fn state_contains_phase(state: &LifecycleStateData, phase: &str, success: bool) -> bool {
    state.phase_history.iter().any(|r| r.phase == phase && r.success == success)
}

// ============================================================================
// TEST ISOLATION HELPERS
// ============================================================================

/// Reset all mocks for clean test isolation
pub fn reset_all_mocks(mocks: &MockSetup) {
    mocks.reset_all();
}

/// Create isolated test environment
pub struct TestEnvironment {
    pub mocks: MockSetup,
}

impl TestEnvironment {
    pub fn new() -> Self {
        Self {
            mocks: standard_mocks(),
        }
    }

    pub fn with_successful_commands(commands: &[&str]) -> Self {
        Self {
            mocks: mocks_with_successful_commands(commands),
        }
    }

    /// Execute test in isolated environment
    pub fn run_test<F>(f: F)
    where
        F: FnOnce(&MockSetup),
    {
        let env = TestEnvironment::new();
        f(&env.mocks);
        env.mocks.reset_all();
    }
}

// ============================================================================
// EXPECTATION BUILDERS
// ============================================================================

/// Builder for setting up mock expectations fluently
pub struct ExpectationBuilder {
    executor_expectations: Vec<(String, bool)>,
    hook_expectations: Vec<(String, String)>,
}

impl ExpectationBuilder {
    pub fn new() -> Self {
        Self {
            executor_expectations: vec![],
            hook_expectations: vec![],
        }
    }

    pub fn expect_command_success(mut self, command: impl Into<String>) -> Self {
        self.executor_expectations.push((command.into(), true));
        self
    }

    pub fn expect_command_failure(mut self, command: impl Into<String>) -> Self {
        self.executor_expectations.push((command.into(), false));
        self
    }

    pub fn expect_hook_failure(mut self, hook: impl Into<String>, error: impl Into<String>) -> Self {
        self.hook_expectations.push((hook.into(), error.into()));
        self
    }

    pub fn setup_mocks(self) -> MockSetup {
        let mut builder = MockSetupBuilder::new();

        for (cmd, success) in self.executor_expectations {
            builder = if success {
                builder.with_command_success(&cmd)
            } else {
                builder.with_command_failure(&cmd, "Command failed")
            };
        }

        for (hook, error) in self.hook_expectations {
            builder = builder.with_hook_failure(&hook, &error);
        }

        builder.build()
    }
}

// ============================================================================
// EXAMPLE USAGE IN TESTS
// ============================================================================

#[cfg(test)]
mod helper_tests {
    use super::*;
    use crate::lifecycle_tests::acceptance_tests::PhaseExecutor;

    #[test]
    fn example_using_phase_builder() {
        let phase = PhaseConfigBuilder::new("build")
            .with_command("npm run build")
            .with_before_hook("test")
            .with_after_hook("cleanup")
            .build();

        assert_eq!(phase.name, "build");
        assert_eq!(phase.commands.len(), 1);
        assert_eq!(phase.before_hooks.len(), 1);
        assert_eq!(phase.after_hooks.len(), 1);
    }

    #[test]
    fn example_using_expectation_builder() {
        let mocks = ExpectationBuilder::new()
            .expect_command_success("npm run build")
            .expect_command_success("npm test")
            .expect_hook_failure("lint", "Linting failed")
            .setup_mocks();

        // Mocks are now configured
        assert!(mocks.executor.execute("npm run build", &PathBuf::from("."), &[]).is_ok());
    }

    #[test]
    fn example_using_test_environment() {
        TestEnvironment::run_test(|mocks| {
            let phase = simple_phase("test", "echo hello");
            let executor = PhaseExecutor::new(
                mocks.executor.clone(),
                mocks.state_repo.clone(),
                mocks.hook_registry.clone(),
                mocks.observer.clone(),
            );

            let result = executor.run_phase(&phase);
            assert!(result.is_ok());

            assert_successful_execution(mocks, "test", &["echo hello"]);
        });
    }

    #[test]
    fn example_using_verification_helpers() {
        let mocks = mocks_with_successful_commands(&["cmd1", "cmd2"]);

        let executor = mocks.executor.clone();
        executor.execute("cmd1", &PathBuf::from("."), &[]).unwrap();
        executor.execute("cmd2", &PathBuf::from("."), &[]).unwrap();

        verify_command_called_in_directory(&executor, "cmd1", ".");
        assert!(executor.verify_call_order(&["cmd1", "cmd2"]));
    }
}
