//! London School TDD Examples for ggen Lifecycle System
//!
//! This file demonstrates London School (mockist, outside-in) TDD patterns
//! using mockall for the lifecycle system. These are executable examples
//! showing best practices.

use mockall::predicate::*;
use mockall::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Duration;

// ============================================================================
// STEP 1: Define Traits (Discovered Through Testing)
// ============================================================================

/// Command executor abstraction (discovered by needing to mock shell commands)
#[automock]
pub trait CommandExecutor: Send + Sync {
    fn execute(&self, spec: &CommandSpec) -> Result<CommandOutput, ExecutionError>;
}

#[derive(Debug, Clone)]
pub struct CommandSpec {
    pub command: String,
    pub working_dir: PathBuf,
    pub env: HashMap<String, String>,
}

#[derive(Debug)]
pub struct CommandOutput {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
    pub duration: Duration,
}

impl CommandOutput {
    pub fn success() -> Self {
        Self {
            exit_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            duration: Duration::from_secs(1),
        }
    }
}

/// State repository abstraction (discovered by needing to mock filesystem)
#[automock]
pub trait StateRepository: Send + Sync {
    fn load(&self) -> Result<LifecycleState, StateError>;
    fn save(&self, state: &LifecycleState) -> Result<(), StateError>;
}

#[derive(Debug, Clone, Default)]
pub struct LifecycleState {
    pub last_phase: Option<String>,
    pub phase_history: Vec<PhaseRecord>,
}

#[derive(Debug, Clone)]
pub struct PhaseRecord {
    pub phase: String,
    pub timestamp: u128,
    pub duration_ms: u128,
    pub success: bool,
}

/// Hook registry abstraction (discovered by needing to test hook execution)
#[automock]
pub trait HookRegistry: Send + Sync {
    fn execute_before(&self, phase: &str) -> Result<(), HookError>;
    fn execute_after(&self, phase: &str) -> Result<(), HookError>;
}

/// Observer abstraction (discovered by needing to test progress reporting)
#[automock]
pub trait LifecycleObserver: Send + Sync {
    fn on_phase_start(&self, phase: &str);
    fn on_phase_complete(&self, phase: &str, duration: Duration);
    fn on_error(&self, phase: &str, error: &str);
}

// ============================================================================
// Error Types
// ============================================================================

#[derive(Debug, thiserror::Error)]
pub enum ExecutionError {
    #[error("Command failed: {0}")]
    CommandFailed(String),
}

#[derive(Debug, thiserror::Error)]
pub enum StateError {
    #[error("State error: {0}")]
    Io(String),
}

#[derive(Debug, thiserror::Error)]
pub enum HookError {
    #[error("Hook error: {0}")]
    HookFailed(String),

    #[error("Hook recursion detected: {0}")]
    Recursion(String),
}

#[derive(Debug, thiserror::Error)]
pub enum LifecycleError {
    #[error("Execution error: {0}")]
    Execution(#[from] ExecutionError),

    #[error("State error: {0}")]
    State(#[from] StateError),

    #[error("Hook error: {0}")]
    Hook(#[from] HookError),
}

// ============================================================================
// STEP 2: System Under Test (Designed Through Tests)
// ============================================================================

/// Phase executor with dependency injection (designed through TDD)
pub struct PhaseExecutor {
    cmd_executor: Box<dyn CommandExecutor>,
    state_repo: Box<dyn StateRepository>,
    hook_registry: Box<dyn HookRegistry>,
    observer: Box<dyn LifecycleObserver>,
}

impl PhaseExecutor {
    pub fn new(
        cmd_executor: Box<dyn CommandExecutor>,
        state_repo: Box<dyn StateRepository>,
        hook_registry: Box<dyn HookRegistry>,
        observer: Box<dyn LifecycleObserver>,
    ) -> Self {
        Self {
            cmd_executor,
            state_repo,
            hook_registry,
            observer,
        }
    }

    pub fn run_phase(&self, phase: &str) -> Result<(), LifecycleError> {
        // Notify start
        self.observer.on_phase_start(phase);

        // Execute before hooks
        if let Err(e) = self.hook_registry.execute_before(phase) {
            self.observer.on_error(phase, &e.to_string());
            return Err(e.into());
        }

        // Execute command
        let spec = CommandSpec {
            command: format!("echo 'Running {}'", phase),
            working_dir: PathBuf::from("."),
            env: HashMap::new(),
        };

        let start = std::time::Instant::now();
        let output = self.cmd_executor.execute(&spec)?;
        let duration = start.elapsed();

        // Update state
        let mut state = self.state_repo.load()?;
        state.last_phase = Some(phase.to_string());
        state.phase_history.push(PhaseRecord {
            phase: phase.to_string(),
            timestamp: 0, // Would be real timestamp
            duration_ms: duration.as_millis(),
            success: output.exit_code == 0,
        });
        self.state_repo.save(&state)?;

        // Execute after hooks
        self.hook_registry.execute_after(phase)?;

        // Notify completion
        self.observer.on_phase_complete(phase, duration);

        Ok(())
    }
}

// ============================================================================
// LONDON SCHOOL TDD TESTS
// ============================================================================

#[cfg(test)]
mod london_school_tests {
    use super::*;

    // ========================================================================
    // Example 1: Outside-In Acceptance Test
    // ========================================================================
    // Start with high-level test, use mocks for everything

    #[test]
    fn acceptance_test_run_phase_complete_workflow() {
        // GIVEN: All mocked dependencies with expectations
        let mut mock_cmd = MockCommandExecutor::new();
        let mut mock_state = MockStateRepository::new();
        let mut mock_hooks = MockHookRegistry::new();
        let mut mock_observer = MockLifecycleObserver::new();

        // EXPECT: Observer notified of start
        mock_observer
            .expect_on_phase_start()
            .with(eq("build"))
            .times(1)
            .return_const(());

        // EXPECT: Before hooks executed
        mock_hooks
            .expect_execute_before()
            .with(eq("build"))
            .times(1)
            .returning(|_| Ok(()));

        // EXPECT: Command executed successfully
        mock_cmd
            .expect_execute()
            .withf(|spec| spec.command.contains("build"))
            .times(1)
            .returning(|_| Ok(CommandOutput::success()));

        // EXPECT: State loaded, updated, and saved
        mock_state
            .expect_load()
            .times(1)
            .returning(|| Ok(LifecycleState::default()));

        mock_state
            .expect_save()
            .withf(|state| state.last_phase == Some("build".into()))
            .times(1)
            .returning(|_| Ok(()));

        // EXPECT: After hooks executed
        mock_hooks
            .expect_execute_after()
            .with(eq("build"))
            .times(1)
            .returning(|_| Ok(()));

        // EXPECT: Observer notified of completion
        mock_observer
            .expect_on_phase_complete()
            .with(eq("build"), always())
            .times(1)
            .return_const(());

        // WHEN: Run phase
        let executor = PhaseExecutor::new(
            Box::new(mock_cmd),
            Box::new(mock_state),
            Box::new(mock_hooks),
            Box::new(mock_observer),
        );

        let result = executor.run_phase("build");

        // THEN: Execution succeeds
        assert!(result.is_ok());

        // All mock expectations verified automatically on drop
    }

    // ========================================================================
    // Example 2: Testing Execution Order with Sequence
    // ========================================================================

    #[test]
    fn test_hooks_execute_in_correct_order() {
        use mockall::Sequence;

        let mut seq = Sequence::new();
        let mut mock_hooks = MockHookRegistry::new();

        // EXPECT: before hook runs first
        mock_hooks
            .expect_execute_before()
            .with(eq("test"))
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_| Ok(()));

        // EXPECT: after hook runs second
        mock_hooks
            .expect_execute_after()
            .with(eq("test"))
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_| Ok(()));

        // Execute in order
        mock_hooks.execute_before("test").unwrap();
        mock_hooks.execute_after("test").unwrap();

        // Sequence automatically verified
    }

    // ========================================================================
    // Example 3: Testing Error Propagation
    // ========================================================================

    #[test]
    fn test_command_failure_propagates_and_notifies_observer() {
        let mut mock_cmd = MockCommandExecutor::new();
        let mut mock_state = MockStateRepository::new();
        let mut mock_hooks = MockHookRegistry::new();
        let mut mock_observer = MockLifecycleObserver::new();

        // EXPECT: Normal flow until command
        mock_observer.expect_on_phase_start().times(1).return_const(());
        mock_hooks.expect_execute_before().times(1).returning(|_| Ok(()));

        // EXPECT: Command fails
        mock_cmd
            .expect_execute()
            .times(1)
            .returning(|_| Err(ExecutionError::CommandFailed("Build failed".into())));

        // EXPECT: Observer notified of error (NOT called because we return early)
        mock_observer.expect_on_error().times(0);

        // State and after hooks should NOT be called
        mock_state.expect_load().times(0);
        mock_hooks.expect_execute_after().times(0);

        // WHEN: Run phase
        let executor = PhaseExecutor::new(
            Box::new(mock_cmd),
            Box::new(mock_state),
            Box::new(mock_hooks),
            Box::new(mock_observer),
        );

        let result = executor.run_phase("build");

        // THEN: Error propagated
        assert!(result.is_err());
    }

    // ========================================================================
    // Example 4: Testing State Updates
    // ========================================================================

    #[test]
    fn test_state_updated_with_phase_record() {
        let mut mock_cmd = MockCommandExecutor::new();
        let mut mock_state = MockStateRepository::new();
        let mut mock_hooks = MockHookRegistry::new();
        let mut mock_observer = MockLifecycleObserver::new();

        // Setup minimal expectations
        mock_observer.expect_on_phase_start().return_const(());
        mock_observer.expect_on_phase_complete().return_const(());
        mock_hooks.expect_execute_before().returning(|_| Ok(()));
        mock_hooks.expect_execute_after().returning(|_| Ok(()));
        mock_cmd.expect_execute().returning(|_| Ok(CommandOutput::success()));

        // EXPECT: State loaded
        mock_state
            .expect_load()
            .times(1)
            .returning(|| Ok(LifecycleState::default()));

        // EXPECT: State saved with correct updates
        mock_state
            .expect_save()
            .withf(|state| {
                state.last_phase == Some("test".into()) &&
                state.phase_history.len() == 1 &&
                state.phase_history[0].phase == "test"
            })
            .times(1)
            .returning(|_| Ok(()));

        // WHEN: Run phase
        let executor = PhaseExecutor::new(
            Box::new(mock_cmd),
            Box::new(mock_state),
            Box::new(mock_hooks),
            Box::new(mock_observer),
        );

        executor.run_phase("test").unwrap();
    }

    // ========================================================================
    // Example 5: Testing Hook Recursion Prevention
    // ========================================================================

    #[test]
    fn test_hook_recursion_detected() {
        let mut mock_hooks = MockHookRegistry::new();

        // EXPECT: Hook fails with recursion error
        mock_hooks
            .expect_execute_before()
            .with(eq("build"))
            .times(1)
            .returning(|phase| Err(HookError::Recursion(phase.into())));

        let result = mock_hooks.execute_before("build");

        assert!(matches!(result, Err(HookError::Recursion(_))));
    }

    // ========================================================================
    // Example 6: Testing with Custom Matchers
    // ========================================================================

    #[test]
    fn test_command_spec_validation() {
        let mut mock_cmd = MockCommandExecutor::new();

        // EXPECT: Command with specific properties
        mock_cmd
            .expect_execute()
            .withf(|spec| {
                spec.command.starts_with("cargo") &&
                spec.working_dir == PathBuf::from(".") &&
                spec.env.is_empty()
            })
            .times(1)
            .returning(|_| Ok(CommandOutput::success()));

        let spec = CommandSpec {
            command: "cargo test".into(),
            working_dir: PathBuf::from("."),
            env: HashMap::new(),
        };

        mock_cmd.execute(&spec).unwrap();
    }

    // ========================================================================
    // Example 7: Testing Multiple Calls with Different Args
    // ========================================================================

    #[test]
    fn test_multiple_phases_different_behaviors() {
        let mut mock_observer = MockLifecycleObserver::new();

        // EXPECT: Different behavior for each phase
        mock_observer
            .expect_on_phase_start()
            .with(eq("init"))
            .times(1)
            .return_const(());

        mock_observer
            .expect_on_phase_start()
            .with(eq("build"))
            .times(1)
            .return_const(());

        mock_observer.on_phase_start("init");
        mock_observer.on_phase_start("build");
    }
}

// ============================================================================
// Test Helpers and Builders (London School Pattern)
// ============================================================================

#[cfg(test)]
mod test_helpers {
    use super::*;

    /// Builder for PhaseExecutor in tests
    pub struct PhaseExecutorBuilder {
        cmd_executor: Option<Box<dyn CommandExecutor>>,
        state_repo: Option<Box<dyn StateRepository>>,
        hook_registry: Option<Box<dyn HookRegistry>>,
        observer: Option<Box<dyn LifecycleObserver>>,
    }

    impl PhaseExecutorBuilder {
        pub fn new() -> Self {
            Self {
                cmd_executor: None,
                state_repo: None,
                hook_registry: None,
                observer: None,
            }
        }

        pub fn with_successful_command(mut self) -> Self {
            let mut mock = MockCommandExecutor::new();
            mock.expect_execute()
                .returning(|_| Ok(CommandOutput::success()));
            self.cmd_executor = Some(Box::new(mock));
            self
        }

        pub fn with_empty_state(mut self) -> Self {
            let mut mock = MockStateRepository::new();
            mock.expect_load()
                .returning(|| Ok(LifecycleState::default()));
            mock.expect_save()
                .returning(|_| Ok(()));
            self.state_repo = Some(Box::new(mock));
            self
        }

        pub fn with_no_hooks(mut self) -> Self {
            let mut mock = MockHookRegistry::new();
            mock.expect_execute_before().returning(|_| Ok(()));
            mock.expect_execute_after().returning(|_| Ok(()));
            self.hook_registry = Some(Box::new(mock));
            self
        }

        pub fn with_silent_observer(mut self) -> Self {
            let mut mock = MockLifecycleObserver::new();
            mock.expect_on_phase_start().return_const(());
            mock.expect_on_phase_complete().return_const(());
            mock.expect_on_error().return_const(());
            self.observer = Some(Box::new(mock));
            self
        }

        pub fn build(self) -> PhaseExecutor {
            PhaseExecutor::new(
                self.cmd_executor.expect("cmd_executor not set"),
                self.state_repo.expect("state_repo not set"),
                self.hook_registry.expect("hook_registry not set"),
                self.observer.expect("observer not set"),
            )
        }
    }

    #[test]
    fn test_using_builder() {
        let executor = PhaseExecutorBuilder::new()
            .with_successful_command()
            .with_empty_state()
            .with_no_hooks()
            .with_silent_observer()
            .build();

        assert!(executor.run_phase("test").is_ok());
    }
}
