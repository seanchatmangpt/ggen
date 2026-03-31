//! Interaction-Based Unit Tests (London School TDD)
//!
//! These tests focus on testing individual components in complete isolation,
//! verifying that they collaborate correctly with their dependencies through mocks.
//!
//! London School Principle: Test the messages sent between objects, not the
//! object's internal state.

use super::mocks::*;
use super::acceptance_tests::{PhaseExecutor, PhaseConfig};
use std::path::PathBuf;

// ============================================================================
// UNIT TESTS: PhaseExecutor
// ============================================================================

mod phase_executor_tests {
    use super::*;

    /// UNIT TEST: Verify executor delegates command execution correctly
    ///
    /// London School: We're testing the CONVERSATION between PhaseExecutor
    /// and CommandExecutor, not the internal logic of either.
    #[test]
    fn should_delegate_command_execution_to_executor() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_success("echo hello")
            .build();

        let phase = PhaseConfig {
            name: "test".to_string(),
            commands: vec!["echo hello".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            mocks.state_repo.clone(),
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        executor.run_phase(&phase).unwrap();

        // ASSERT - Verify the interaction
        assert!(mocks.executor.verify_called("echo hello"),
               "PhaseExecutor should delegate to CommandExecutor");

        // Verify it was called exactly once
        assert_eq!(mocks.executor.call_count("echo hello"), 1,
                  "Command should be executed exactly once");
    }

    /// UNIT TEST: Verify multiple commands are executed in order
    #[test]
    fn should_execute_multiple_commands_in_sequence() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_success("command1")
            .with_command_success("command2")
            .with_command_success("command3")
            .build();

        let phase = PhaseConfig {
            name: "test".to_string(),
            commands: vec![
                "command1".to_string(),
                "command2".to_string(),
                "command3".to_string(),
            ],
            before_hooks: vec![],
            after_hooks: vec![],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            mocks.state_repo.clone(),
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        executor.run_phase(&phase).unwrap();

        // ASSERT - Verify execution order
        assert!(mocks.executor.verify_call_order(&["command1", "command2", "command3"]),
               "Commands should execute in defined order");
    }

    /// UNIT TEST: Verify state repository is called to persist results
    #[test]
    fn should_save_state_after_successful_execution() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_success("build")
            .build();

        let phase = PhaseConfig {
            name: "build".to_string(),
            commands: vec!["build".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            mocks.state_repo.clone(),
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        executor.run_phase(&phase).unwrap();

        // ASSERT - Verify state collaboration
        assert!(mocks.state_repo.verify_load_called(),
               "Should load existing state before updating");

        assert!(mocks.state_repo.verify_save_called(),
               "Should save state after execution");

        // Verify correct data was saved
        assert!(mocks.state_repo.verify_phase_recorded("build"),
               "Should record phase execution in state");
    }

    /// UNIT TEST: Verify observer is notified of phase lifecycle
    #[test]
    fn should_notify_observer_of_phase_lifecycle() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_success("test")
            .build();

        let phase = PhaseConfig {
            name: "test".to_string(),
            commands: vec!["test".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            mocks.state_repo.clone(),
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        executor.run_phase(&phase).unwrap();

        // ASSERT - Verify observer interaction
        assert!(mocks.observer.verify_phase_started("test"),
               "Should notify observer when phase starts");

        assert!(mocks.observer.verify_phase_completed_successfully("test"),
               "Should notify observer when phase completes");

        // Verify correct order of notifications
        assert!(mocks.observer.verify_event_order("test"),
               "Start notification should come before complete notification");
    }

    /// UNIT TEST: Verify hooks are executed through registry
    #[test]
    fn should_execute_hooks_through_registry() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_success("main command")
            .build();

        let phase = PhaseConfig {
            name: "build".to_string(),
            commands: vec!["main command".to_string()],
            before_hooks: vec!["test".to_string(), "lint".to_string()],
            after_hooks: vec!["cleanup".to_string()],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            mocks.state_repo.clone(),
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        executor.run_phase(&phase).unwrap();

        // ASSERT - Verify hook registry interactions
        assert!(mocks.hook_registry.verify_hook_executed("test", HookStage::Before),
               "Should execute before hooks through registry");

        assert!(mocks.hook_registry.verify_hook_executed("lint", HookStage::Before),
               "Should execute all before hooks");

        assert!(mocks.hook_registry.verify_hook_executed("cleanup", HookStage::After),
               "Should execute after hooks through registry");

        // Verify execution order
        let expected_order = vec![
            ("test".to_string(), HookStage::Before),
            ("lint".to_string(), HookStage::Before),
            ("cleanup".to_string(), HookStage::After),
        ];
        assert!(mocks.hook_registry.verify_execution_order(&expected_order),
               "Hooks should execute in correct order");
    }

    /// UNIT TEST: Verify error handling - command failure
    #[test]
    fn should_propagate_command_failure() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_failure("failing command", "Command error")
            .build();

        let phase = PhaseConfig {
            name: "build".to_string(),
            commands: vec!["failing command".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            mocks.state_repo.clone(),
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        let result = executor.run_phase(&phase);

        // ASSERT - Verify error propagation
        assert!(result.is_err(), "Should return error when command fails");

        // Verify observer was notified of error
        let events = mocks.observer.get_events();
        assert!(events.iter().any(|e| matches!(e, ObserverEvent::Error { .. })),
               "Should notify observer of error");

        // Verify state was NOT saved (transaction rollback)
        assert!(!mocks.state_repo.verify_phase_recorded("build"),
               "Should not record failed phase in state");
    }

    /// UNIT TEST: Verify error handling - state save failure
    #[test]
    fn should_handle_state_save_failure() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_success("build")
            .build();

        // Configure state repo to fail on save
        let state_repo = mocks.state_repo.clone().with_save_failure();

        let phase = PhaseConfig {
            name: "build".to_string(),
            commands: vec!["build".to_string()],
            before_hooks: vec![],
            after_hooks: vec![],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            state_repo,
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        let result = executor.run_phase(&phase);

        // ASSERT
        assert!(result.is_err(), "Should propagate state save error");

        // Verify command WAS executed (failure happened after)
        assert!(mocks.executor.verify_called("build"),
               "Command execution should proceed even if state save fails");
    }

    /// UNIT TEST: Verify stopping on first command failure
    #[test]
    fn should_stop_on_first_command_failure() {
        // ARRANGE
        let mocks = MockSetupBuilder::new()
            .with_command_success("command1")
            .with_command_failure("command2", "Failed")
            .with_command_success("command3")  // Should never execute
            .build();

        let phase = PhaseConfig {
            name: "test".to_string(),
            commands: vec![
                "command1".to_string(),
                "command2".to_string(),
                "command3".to_string(),
            ],
            before_hooks: vec![],
            after_hooks: vec![],
        };

        let executor = PhaseExecutor::new(
            mocks.executor.clone(),
            mocks.state_repo.clone(),
            mocks.hook_registry.clone(),
            mocks.observer.clone(),
        );

        // ACT
        let result = executor.run_phase(&phase);

        // ASSERT
        assert!(result.is_err(), "Should fail on command2");

        // Verify command3 was NEVER executed
        assert!(!mocks.executor.verify_called("command3"),
               "Should not execute subsequent commands after failure");

        // Verify only first two commands were attempted
        assert_eq!(mocks.executor.call_count("command1"), 1);
        assert_eq!(mocks.executor.call_count("command2"), 1);
        assert_eq!(mocks.executor.call_count("command3"), 0);
    }
}

// ============================================================================
// UNIT TESTS: CommandExecutor Mock Itself
// ============================================================================

mod command_executor_mock_tests {
    use super::*;

    /// Verify mock correctly records calls
    #[test]
    fn mock_should_record_command_calls() {
        let executor = MockCommandExecutor::new();

        executor.execute("cmd1", &PathBuf::from("."), &[]).unwrap();
        executor.execute("cmd2", &PathBuf::from("/tmp"), &[]).unwrap();

        assert_eq!(executor.call_count("cmd1"), 1);
        assert_eq!(executor.call_count("cmd2"), 1);

        let calls = executor.get_calls();
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].command, "cmd1");
        assert_eq!(calls[1].command, "cmd2");
    }

    /// Verify mock correctly returns configured responses
    #[test]
    fn mock_should_return_configured_responses() {
        let executor = MockCommandExecutor::new()
            .with_success("good command")
            .with_failure("bad command", "Error message");

        let good_result = executor.execute("good command", &PathBuf::from("."), &[]);
        assert!(good_result.is_ok());

        let bad_result = executor.execute("bad command", &PathBuf::from("."), &[]);
        assert!(bad_result.is_err());
        assert_eq!(bad_result.unwrap_err(), "Error message");
    }

    /// Verify mock tracks environment variables
    #[test]
    fn mock_should_track_environment_variables() {
        let executor = MockCommandExecutor::new();

        let env = vec![
            ("NODE_ENV".to_string(), "production".to_string()),
            ("DEBUG".to_string(), "true".to_string()),
        ];

        executor.execute("npm run build", &PathBuf::from("."), &env).unwrap();

        assert!(executor.verify_called_with_env("npm run build", "NODE_ENV", "production"));
        assert!(executor.verify_called_with_env("npm run build", "DEBUG", "true"));
    }

    /// Verify mock can be reset for test isolation
    #[test]
    fn mock_should_reset_call_history() {
        let executor = MockCommandExecutor::new();

        executor.execute("cmd1", &PathBuf::from("."), &[]).unwrap();
        assert_eq!(executor.call_count("cmd1"), 1);

        executor.reset();
        assert_eq!(executor.call_count("cmd1"), 0);
        assert!(executor.verify_no_calls());
    }
}

// ============================================================================
// UNIT TESTS: StateRepository Mock
// ============================================================================

mod state_repository_mock_tests {
    use super::*;

    /// Verify mock tracks load/save calls
    #[test]
    fn mock_should_track_load_and_save_calls() {
        let repo = MockStateRepository::new();

        assert_eq!(repo.load_call_count(), 0);
        assert_eq!(repo.save_call_count(), 0);

        repo.load().unwrap();
        assert_eq!(repo.load_call_count(), 1);

        let state = LifecycleStateData::default();
        repo.save(&state).unwrap();
        assert_eq!(repo.save_call_count(), 1);
    }

    /// Verify mock persists state between save/load
    #[test]
    fn mock_should_persist_state_between_calls() {
        let repo = MockStateRepository::new();

        let mut state = LifecycleStateData::default();
        state.last_phase = Some("build".to_string());
        state.phase_history.push(RunRecordData {
            phase: "build".to_string(),
            started_ms: 1000,
            duration_ms: 500,
            success: true,
        });

        repo.save(&state).unwrap();

        let loaded = repo.load().unwrap();
        assert_eq!(loaded.last_phase, Some("build".to_string()));
        assert_eq!(loaded.phase_history.len(), 1);
    }

    /// Verify mock can simulate load failures
    #[test]
    fn mock_should_simulate_load_failure() {
        let repo = MockStateRepository::new().with_load_failure();

        let result = repo.load();
        assert!(result.is_err());
    }

    /// Verify mock can simulate save failures
    #[test]
    fn mock_should_simulate_save_failure() {
        let repo = MockStateRepository::new().with_save_failure();

        let state = LifecycleStateData::default();
        let result = repo.save(&state);
        assert!(result.is_err());
    }
}

// ============================================================================
// UNIT TESTS: HookRegistry Mock
// ============================================================================

mod hook_registry_mock_tests {
    use super::*;

    /// Verify mock tracks hook executions
    #[test]
    fn mock_should_track_hook_executions() {
        let registry = MockHookRegistry::new();

        registry.execute_before_hooks("build", &vec!["test".to_string()]).unwrap();
        registry.execute_after_hooks("build", &vec!["cleanup".to_string()]).unwrap();

        assert!(registry.verify_hook_executed("test", HookStage::Before));
        assert!(registry.verify_hook_executed("cleanup", HookStage::After));
    }

    /// Verify mock tracks execution order
    #[test]
    fn mock_should_track_hook_execution_order() {
        let registry = MockHookRegistry::new();

        let before_hooks = vec!["hook1".to_string(), "hook2".to_string()];
        let after_hooks = vec!["hook3".to_string()];

        registry.execute_before_hooks("phase", &before_hooks).unwrap();
        registry.execute_after_hooks("phase", &after_hooks).unwrap();

        let expected = vec![
            ("hook1".to_string(), HookStage::Before),
            ("hook2".to_string(), HookStage::Before),
            ("hook3".to_string(), HookStage::After),
        ];

        assert!(registry.verify_execution_order(&expected));
    }

    /// Verify mock can simulate hook failures
    #[test]
    fn mock_should_simulate_hook_failure() {
        let registry = MockHookRegistry::new()
            .with_hook_failure("failing_hook", "Hook error");

        let result = registry.execute_before_hooks(
            "build",
            &vec!["failing_hook".to_string()]
        );

        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Hook error");
    }

    /// Verify mock prevents recursion
    #[test]
    fn mock_should_prevent_recursion() {
        let registry = MockHookRegistry::new();

        // Start executing phase "build"
        let _ = registry.execute_before_hooks("build", &vec![]);

        // Try to execute "build" again (recursion)
        let result = registry.execute_before_hooks("build", &vec![]);

        assert!(result.is_err());
        assert!(result.unwrap_err().contains("recursion"));

        // Clean up
        let _ = registry.execute_after_hooks("build", &vec![]);
    }
}

// ============================================================================
// UNIT TESTS: Observer Mock
// ============================================================================

mod observer_mock_tests {
    use super::*;

    /// Verify mock tracks all events
    #[test]
    fn mock_should_track_all_events() {
        let observer = MockObserver::new();

        observer.on_phase_start("build");
        observer.on_command_executed("npm run build", true);
        observer.on_phase_complete("build", true, 1000);

        let events = observer.get_events();
        assert_eq!(events.len(), 3);

        assert!(matches!(events[0], ObserverEvent::PhaseStarted { .. }));
        assert!(matches!(events[1], ObserverEvent::CommandExecuted { .. }));
        assert!(matches!(events[2], ObserverEvent::PhaseCompleted { .. }));
    }

    /// Verify mock can verify specific events
    #[test]
    fn mock_should_verify_specific_events() {
        let observer = MockObserver::new();

        observer.on_phase_start("test");
        observer.on_phase_complete("test", true, 500);

        assert!(observer.verify_phase_started("test"));
        assert!(observer.verify_phase_completed_successfully("test"));
    }

    /// Verify mock tracks errors
    #[test]
    fn mock_should_track_errors() {
        let observer = MockObserver::new();

        observer.on_error("build", "Build failed");

        let events = observer.get_events();
        assert!(events.iter().any(|e| matches!(e, ObserverEvent::Error { .. })));
    }
}
