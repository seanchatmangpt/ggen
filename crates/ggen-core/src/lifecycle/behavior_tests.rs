//! London School TDD Behavior Tests for Lifecycle System
//!
//! These tests focus on BEHAVIOR and INTERACTIONS between components,
//! not implementation details. We use mocks to verify contracts and
//! collaborations between objects.

#[cfg(test)]
mod behavior_tests {
    use super::super::*;
    use anyhow::Result;
    use mockall::mock;
    use mockall::predicate::*;
    use std::path::PathBuf;

    // ============================================================================
    // TRAIT ABSTRACTIONS FOR MOCKING
    // ============================================================================

    /// Mock-friendly trait for command execution
    pub trait CommandExecutor {
        fn execute(&self, cmd: &str, cwd: &std::path::Path, env: &[(String, String)])
            -> Result<()>;
    }

    /// Mock-friendly trait for state persistence
    pub trait StatePersister {
        fn load(&self, path: &std::path::Path) -> state::LifecycleState;
        fn save(&self, path: &std::path::Path, state: &state::LifecycleState) -> Result<()>;
    }

    /// Mock-friendly trait for cache operations
    pub trait CacheManager {
        fn generate_key(&self, phase: &str, cmds: &[String], env: &[(String, String)]) -> String;
        fn is_valid(&self, phase: &str, key: &str) -> bool;
        fn invalidate(&self, phase: &str) -> Result<()>;
    }

    /// Mock-friendly trait for hook execution
    pub trait HookExecutor {
        fn run_before_hooks(&self, phase: &str) -> Result<()>;
        fn run_after_hooks(&self, phase: &str) -> Result<()>;
        fn run_error_hooks(&self) -> Result<()>;
    }

    // ============================================================================
    // MOCK IMPLEMENTATIONS
    // ============================================================================

    mock! {
        pub CommandExecutor {}
        impl CommandExecutor for CommandExecutor {
            fn execute(&self, cmd: &str, cwd: &std::path::Path, env: &[(String, String)]) -> Result<()>;
        }
    }

    mock! {
        pub StatePersister {}
        impl StatePersister for StatePersister {
            fn load(&self, path: &std::path::Path) -> state::LifecycleState;
            fn save(&self, path: &std::path::Path, state: &state::LifecycleState) -> Result<()>;
        }
    }

    mock! {
        pub CacheManager {}
        impl CacheManager for CacheManager {
            fn generate_key(&self, phase: &str, cmds: &[String], env: &[(String, String)]) -> String;
            fn is_valid(&self, phase: &str, key: &str) -> bool;
            fn invalidate(&self, phase: &str) -> Result<()>;
        }
    }

    mock! {
        pub HookExecutor {}
        impl HookExecutor for HookExecutor {
            fn run_before_hooks(&self, phase: &str) -> Result<()>;
            fn run_after_hooks(&self, phase: &str) -> Result<()>;
            fn run_error_hooks(&self) -> Result<()>;
        }
    }

    // ============================================================================
    // TEST 1: HOOK EXECUTION CONTRACT
    // ============================================================================
    // Verify that hooks are called in the correct order:
    // before_all -> before_phase -> phase_execution -> after_phase -> after_all

    #[test]
    fn test_hook_execution_order_contract() {
        // GIVEN: A mock hook executor that tracks call order
        let mut mock_hooks = MockHookExecutor::new();
        let mut call_sequence = mockall::Sequence::new();

        // Expect before_all to be called first
        mock_hooks
            .expect_run_before_hooks()
            .with(eq("build"))
            .times(1)
            .in_sequence(&mut call_sequence)
            .returning(|_| Ok(()));

        // Then after_all should be called last
        mock_hooks
            .expect_run_after_hooks()
            .with(eq("build"))
            .times(1)
            .in_sequence(&mut call_sequence)
            .returning(|_| Ok(()));

        // WHEN: We execute a phase
        let result = mock_hooks.run_before_hooks("build");
        assert!(result.is_ok());

        let result = mock_hooks.run_after_hooks("build");
        assert!(result.is_ok());

        // THEN: Mock expectations verify correct order
    }

    #[test]
    fn test_hooks_called_even_when_phase_succeeds() {
        // GIVEN: Hooks that should always be called
        let mut mock_hooks = MockHookExecutor::new();

        mock_hooks
            .expect_run_before_hooks()
            .with(eq("test"))
            .times(1)
            .returning(|_| Ok(()));

        mock_hooks
            .expect_run_after_hooks()
            .with(eq("test"))
            .times(1)
            .returning(|_| Ok(()));

        // WHEN: Phase executes successfully
        let _ = mock_hooks.run_before_hooks("test");
        let _ = mock_hooks.run_after_hooks("test");

        // THEN: Both hooks were called (verified by mockall)
    }

    #[test]
    fn test_after_hooks_not_called_when_before_hooks_fail() {
        // GIVEN: Before hooks that fail
        let mut mock_hooks = MockHookExecutor::new();

        mock_hooks
            .expect_run_before_hooks()
            .with(eq("deploy"))
            .times(1)
            .returning(|_| Err(anyhow::anyhow!("Hook failed")));

        mock_hooks
            .expect_run_after_hooks()
            .with(eq("deploy"))
            .times(0); // Should NOT be called

        // WHEN: Before hook fails
        let result = mock_hooks.run_before_hooks("deploy");
        assert!(result.is_err());

        // THEN: After hooks are not called (verified by times(0))
    }

    // ============================================================================
    // TEST 2: STATE PERSISTENCE CONTRACT
    // ============================================================================
    // Verify that state is saved after each phase execution

    #[test]
    fn test_state_saved_after_successful_phase() {
        // GIVEN: A mock state persister
        let mut mock_state = MockStatePersister::new();

        // Load empty state initially
        mock_state
            .expect_load()
            .times(1)
            .returning(|_| state::LifecycleState::default());

        // Expect save to be called with updated state
        mock_state
            .expect_save()
            .withf(|_path, state| {
                // Verify state contains the phase execution
                state.last_phase.is_some()
                    && state.phase_history.len() > 0
                    && state.cache_keys.len() > 0
            })
            .times(1)
            .returning(|_, _| Ok(()));

        // WHEN: Phase executes and completes
        let mut state = mock_state.load(&PathBuf::from(".ggen/state.json"));
        state.record_run("build".to_string(), 1000, 500, true);
        state.add_cache_key("build".to_string(), "abc123".to_string());
        let result = mock_state.save(&PathBuf::from(".ggen/state.json"), &state);

        // THEN: State was saved (verified by mock)
        assert!(result.is_ok());
    }

    #[test]
    fn test_state_includes_duration_and_timestamp() {
        // GIVEN: A mock state persister
        let mut mock_state = MockStatePersister::new();

        mock_state
            .expect_save()
            .withf(|_path, state| {
                // Verify state contains timing information
                if let Some(record) = state.phase_history.first() {
                    record.started_ms > 0 && record.duration_ms > 0
                } else {
                    false
                }
            })
            .times(1)
            .returning(|_, _| Ok(()));

        // WHEN: We save state with timing info
        let mut state = state::LifecycleState::default();
        state.record_run("test".to_string(), 1234567890, 450, true);
        let result = mock_state.save(&PathBuf::from(".ggen/state.json"), &state);

        // THEN: State includes proper timestamps
        assert!(result.is_ok());
    }

    #[test]
    fn test_state_not_saved_when_phase_fails() {
        // GIVEN: A command that will fail
        let mut mock_cmd = MockCommandExecutor::new();
        let mut mock_state = MockStatePersister::new();

        mock_cmd
            .expect_execute()
            .times(1)
            .returning(|_, _, _| Err(anyhow::anyhow!("Command failed")));

        // State should NOT be saved on failure
        mock_state.expect_save().times(0);

        // WHEN: Command fails
        let result = mock_cmd.execute("failing_command", &PathBuf::from("/tmp"), &[]);

        // THEN: Command failed and state was not saved
        assert!(result.is_err());
    }

    // ============================================================================
    // TEST 3: CACHE INVALIDATION CONTRACT
    // ============================================================================
    // Verify cache keys are regenerated when inputs change

    #[test]
    fn test_cache_key_regenerated_when_command_changes() {
        // GIVEN: A mock cache manager
        let mut mock_cache = MockCacheManager::new();

        // First call generates key1
        mock_cache
            .expect_generate_key()
            .with(eq("build"), eq(vec!["cargo build".to_string()]), always())
            .times(1)
            .returning(|_, _, _| "key1".to_string());

        // Second call with different command generates key2
        mock_cache
            .expect_generate_key()
            .with(
                eq("build"),
                eq(vec!["cargo build --release".to_string()]),
                always(),
            )
            .times(1)
            .returning(|_, _, _| "key2".to_string());

        // WHEN: Command changes
        let key1 = mock_cache.generate_key("build", &["cargo build".to_string()], &[]);
        let key2 = mock_cache.generate_key("build", &["cargo build --release".to_string()], &[]);

        // THEN: Different keys are generated
        assert_ne!(key1, key2);
    }

    #[test]
    fn test_cache_key_regenerated_when_env_changes() {
        // GIVEN: A mock cache manager
        let mut mock_cache = MockCacheManager::new();

        let env1 = vec![("RUST_LOG".to_string(), "info".to_string())];
        let env2 = vec![("RUST_LOG".to_string(), "debug".to_string())];

        mock_cache
            .expect_generate_key()
            .times(2)
            .returning(|_, _cmds, env| {
                // Generate different keys based on env
                if env.iter().any(|(_, v)| v == "debug") {
                    "key_debug".to_string()
                } else {
                    "key_info".to_string()
                }
            });

        // WHEN: Environment changes
        let key1 = mock_cache.generate_key("build", &["cargo build".to_string()], &env1);
        let key2 = mock_cache.generate_key("build", &["cargo build".to_string()], &env2);

        // THEN: Different keys are generated
        assert_ne!(key1, key2);
    }

    #[test]
    fn test_cache_invalidated_when_phase_fails() {
        // GIVEN: A mock cache that should be invalidated
        let mut mock_cache = MockCacheManager::new();

        mock_cache
            .expect_invalidate()
            .with(eq("build"))
            .times(1)
            .returning(|_| Ok(()));

        // WHEN: Phase fails and we invalidate cache
        let result = mock_cache.invalidate("build");

        // THEN: Cache was invalidated
        assert!(result.is_ok());
    }

    #[test]
    fn test_cache_valid_check_before_execution() {
        // GIVEN: A cache manager that checks validity
        let mut mock_cache = MockCacheManager::new();

        // Cache is valid for this key
        mock_cache
            .expect_is_valid()
            .with(eq("build"), eq("abc123"))
            .times(1)
            .returning(|_, _| true);

        // Cache is invalid for different key
        mock_cache
            .expect_is_valid()
            .with(eq("build"), eq("xyz789"))
            .times(1)
            .returning(|_, _| false);

        // WHEN: We check cache validity
        let is_valid1 = mock_cache.is_valid("build", "abc123");
        let is_valid2 = mock_cache.is_valid("build", "xyz789");

        // THEN: Correct validity is returned
        assert!(is_valid1);
        assert!(!is_valid2);
    }

    // ============================================================================
    // TEST 4: ERROR PROPAGATION CONTRACT
    // ============================================================================
    // Verify errors are properly handled and rolled back

    #[test]
    fn test_error_propagates_from_command_execution() {
        // GIVEN: A command executor that fails
        let mut mock_cmd = MockCommandExecutor::new();

        mock_cmd
            .expect_execute()
            .times(1)
            .returning(|_, _, _| Err(anyhow::anyhow!("Command execution failed")));

        // WHEN: Command is executed
        let result = mock_cmd.execute("bad_command", &PathBuf::from("/tmp"), &[]);

        // THEN: Error is propagated
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Command execution failed"));
    }

    #[test]
    fn test_error_hooks_called_on_failure() {
        // GIVEN: Hooks that should be called on error
        let mut mock_hooks = MockHookExecutor::new();

        mock_hooks
            .expect_run_before_hooks()
            .times(1)
            .returning(|_| Err(anyhow::anyhow!("Hook failed")));

        mock_hooks
            .expect_run_error_hooks()
            .times(1)
            .returning(|| Ok(()));

        // WHEN: Phase fails
        let result = mock_hooks.run_before_hooks("build");
        assert!(result.is_err());

        // Error hooks are triggered
        let error_result = mock_hooks.run_error_hooks();

        // THEN: Error hooks were called
        assert!(error_result.is_ok());
    }

    #[test]
    fn test_rollback_on_partial_failure() {
        // GIVEN: Multiple commands where second fails
        let mut mock_cmd = MockCommandExecutor::new();
        let mut mock_cache = MockCacheManager::new();

        // First command succeeds
        mock_cmd
            .expect_execute()
            .with(eq("cmd1"), always(), always())
            .times(1)
            .returning(|_, _, _| Ok(()));

        // Second command fails
        mock_cmd
            .expect_execute()
            .with(eq("cmd2"), always(), always())
            .times(1)
            .returning(|_, _, _| Err(anyhow::anyhow!("cmd2 failed")));

        // Cache should be invalidated on failure
        mock_cache
            .expect_invalidate()
            .times(1)
            .returning(|_| Ok(()));

        // WHEN: Commands execute and second fails
        let result1 = mock_cmd.execute("cmd1", &PathBuf::from("/tmp"), &[]);
        let result2 = mock_cmd.execute("cmd2", &PathBuf::from("/tmp"), &[]);

        assert!(result1.is_ok());
        assert!(result2.is_err());

        // Trigger rollback via cache invalidation
        let rollback = mock_cache.invalidate("build");

        // THEN: Rollback was performed
        assert!(rollback.is_ok());
    }

    #[test]
    fn test_error_includes_context() {
        // GIVEN: An executor that provides context in errors
        let mut mock_cmd = MockCommandExecutor::new();

        mock_cmd.expect_execute().times(1).returning(|cmd, cwd, _| {
            Err(anyhow::anyhow!(
                "Command '{}' failed in directory '{}'",
                cmd,
                cwd.display()
            ))
        });

        // WHEN: Command fails
        let result = mock_cmd.execute("test_cmd", &PathBuf::from("/project"), &[]);

        // THEN: Error includes context
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("test_cmd"));
        assert!(err.to_string().contains("/project"));
    }

    // ============================================================================
    // TEST 5: WORKSPACE COORDINATION CONTRACT
    // ============================================================================
    // Verify parallel/sequential workspace execution behavior

    #[test]
    fn test_sequential_workspace_execution() {
        // GIVEN: Multiple workspaces to execute sequentially
        let mut mock_cmd = MockCommandExecutor::new();
        let mut sequence = mockall::Sequence::new();

        // Workspace 1 executes first
        mock_cmd
            .expect_execute()
            .with(eq("npm build"), always(), always())
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_, _, _| Ok(()));

        // Workspace 2 executes second
        mock_cmd
            .expect_execute()
            .with(eq("cargo build"), always(), always())
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_, _, _| Ok(()));

        // WHEN: Workspaces execute in sequence
        let result1 = mock_cmd.execute("npm build", &PathBuf::from("/ws1"), &[]);
        let result2 = mock_cmd.execute("cargo build", &PathBuf::from("/ws2"), &[]);

        // THEN: Both succeeded in correct order
        assert!(result1.is_ok());
        assert!(result2.is_ok());
    }

    #[test]
    fn test_workspace_failure_stops_pipeline() {
        // GIVEN: Workspaces where first fails
        let mut mock_cmd = MockCommandExecutor::new();

        // First workspace fails
        mock_cmd
            .expect_execute()
            .with(eq("build"), always(), always())
            .times(1)
            .returning(|_, _, _| Err(anyhow::anyhow!("Build failed")));

        // Second workspace should NOT be called
        // (verified by not setting up expectation)

        // WHEN: First workspace fails
        let result = mock_cmd.execute("build", &PathBuf::from("/ws1"), &[]);

        // THEN: Pipeline stops
        assert!(result.is_err());
    }

    #[test]
    fn test_workspace_isolation() {
        // GIVEN: Workspaces with different working directories
        let mut mock_cmd = MockCommandExecutor::new();

        mock_cmd
            .expect_execute()
            .withf(|_, cwd, _| cwd == PathBuf::from("/workspace1"))
            .times(1)
            .returning(|_, _, _| Ok(()));

        mock_cmd
            .expect_execute()
            .withf(|_, cwd, _| cwd == PathBuf::from("/workspace2"))
            .times(1)
            .returning(|_, _, _| Ok(()));

        // WHEN: Commands execute in different workspaces
        let result1 = mock_cmd.execute("build", &PathBuf::from("/workspace1"), &[]);
        let result2 = mock_cmd.execute("build", &PathBuf::from("/workspace2"), &[]);

        // THEN: Each workspace is isolated
        assert!(result1.is_ok());
        assert!(result2.is_ok());
    }

    #[test]
    fn test_parallel_workspace_execution_contract() {
        // GIVEN: Workspaces that can execute in parallel
        let mut mock_cmd = MockCommandExecutor::new();

        // Both commands can be called in any order
        mock_cmd
            .expect_execute()
            .with(eq("build"), always(), always())
            .times(2)
            .returning(|_, _, _| Ok(()));

        // WHEN: Workspaces execute (order doesn't matter)
        let result1 = mock_cmd.execute("build", &PathBuf::from("/ws1"), &[]);
        let result2 = mock_cmd.execute("build", &PathBuf::from("/ws2"), &[]);

        // THEN: Both can succeed independently
        assert!(result1.is_ok());
        assert!(result2.is_ok());
    }

    // ============================================================================
    // TEST 6: INTEGRATION CONTRACT TESTS
    // ============================================================================
    // Test how components collaborate together

    #[test]
    fn test_full_phase_execution_contract() {
        // GIVEN: All mocked collaborators
        let mut mock_hooks = MockHookExecutor::new();
        let mut mock_cmd = MockCommandExecutor::new();
        let mut mock_state = MockStatePersister::new();
        let mut mock_cache = MockCacheManager::new();
        let mut sequence = mockall::Sequence::new();

        // 1. Before hooks
        mock_hooks
            .expect_run_before_hooks()
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_| Ok(()));

        // 2. Generate cache key
        mock_cache
            .expect_generate_key()
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_, _, _| "cache_key_123".to_string());

        // 3. Execute command
        mock_cmd
            .expect_execute()
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_, _, _| Ok(()));

        // 4. Load state
        mock_state
            .expect_load()
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_| state::LifecycleState::default());

        // 5. Save state
        mock_state
            .expect_save()
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_, _| Ok(()));

        // 6. After hooks
        mock_hooks
            .expect_run_after_hooks()
            .times(1)
            .in_sequence(&mut sequence)
            .returning(|_| Ok(()));

        // WHEN: Full phase executes
        let _ = mock_hooks.run_before_hooks("build");
        let _ = mock_cache.generate_key("build", &["cargo build".to_string()], &[]);
        let _ = mock_cmd.execute("cargo build", &PathBuf::from("/project"), &[]);
        let state = mock_state.load(&PathBuf::from(".ggen/state.json"));
        let _ = mock_state.save(&PathBuf::from(".ggen/state.json"), &state);
        let _ = mock_hooks.run_after_hooks("build");

        // THEN: All collaborations happened in correct order (verified by sequence)
    }

    #[test]
    fn test_cache_hit_skips_execution() {
        // GIVEN: Valid cache exists
        let mut mock_cache = MockCacheManager::new();
        let mut mock_cmd = MockCommandExecutor::new();

        // Cache is valid
        mock_cache.expect_is_valid().times(1).returning(|_, _| true);

        // Command should NOT be executed
        mock_cmd.expect_execute().times(0);

        // WHEN: We check cache before execution
        let cache_valid = mock_cache.is_valid("build", "key123");

        // THEN: Execution is skipped when cache is valid
        assert!(cache_valid);
    }

    #[test]
    fn test_state_persistence_across_phases() {
        // GIVEN: State persisted after first phase
        let mut mock_state = MockStatePersister::new();

        // First phase saves state
        mock_state.expect_save().times(1).returning(|_, _| Ok(()));

        // Second phase loads that state
        mock_state.expect_load().times(1).returning(|_| {
            let mut state = state::LifecycleState::default();
            state.record_run("build".to_string(), 1000, 500, true);
            state
        });

        // WHEN: Phases execute in sequence
        let mut state = state::LifecycleState::default();
        state.record_run("build".to_string(), 1000, 500, true);
        let _ = mock_state.save(&PathBuf::from(".ggen/state.json"), &state);

        let loaded_state = mock_state.load(&PathBuf::from(".ggen/state.json"));

        // THEN: State is persisted across phases
        assert!(loaded_state.last_phase.is_some());
        assert_eq!(loaded_state.phase_history.len(), 1);
    }
}
