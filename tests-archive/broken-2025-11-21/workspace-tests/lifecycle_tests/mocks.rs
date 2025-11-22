//! Mock implementations for London School TDD
//!
//! This module provides test doubles for all external dependencies in the lifecycle system.
//! Following the London School approach, we mock all collaborators to test interactions
//! and design interfaces through testing.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

// ============================================================================
// MOCK COMMAND EXECUTOR
// ============================================================================

/// Mock implementation of command execution for testing
///
/// This allows us to verify:
/// - Which commands were executed
/// - In what order
/// - With what environment variables
/// - Without actually executing shell commands
#[derive(Clone)]
pub struct MockCommandExecutor {
    calls: Arc<Mutex<Vec<CommandCall>>>,
    responses: Arc<Mutex<HashMap<String, CommandResponse>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CommandCall {
    pub command: String,
    pub working_dir: PathBuf,
    pub env: Vec<(String, String)>,
}

#[derive(Debug, Clone)]
pub struct CommandResponse {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: Option<i32>,
}

impl MockCommandExecutor {
    pub fn new() -> Self {
        Self {
            calls: Arc::new(Mutex::new(Vec::new())),
            responses: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Configure a response for a specific command
    pub fn with_response(mut self, command: impl Into<String>, response: CommandResponse) -> Self {
        self.responses.lock().unwrap().insert(command.into(), response);
        self
    }

    /// Configure success response for a command
    pub fn with_success(self, command: impl Into<String>) -> Self {
        self.with_response(command, CommandResponse {
            success: true,
            stdout: String::new(),
            stderr: String::new(),
            exit_code: Some(0),
        })
    }

    /// Configure failure response for a command
    pub fn with_failure(self, command: impl Into<String>, stderr: impl Into<String>) -> Self {
        self.with_response(command, CommandResponse {
            success: false,
            stdout: String::new(),
            stderr: stderr.into(),
            exit_code: Some(1),
        })
    }

    /// Execute a command (records call and returns configured response)
    pub fn execute(&self, command: &str, working_dir: &Path, env: &[(String, String)]) -> Result<CommandResponse, String> {
        // Record the call
        self.calls.lock().unwrap().push(CommandCall {
            command: command.to_string(),
            working_dir: working_dir.to_path_buf(),
            env: env.to_vec(),
        });

        // Return configured response
        let responses = self.responses.lock().unwrap();
        if let Some(response) = responses.get(command) {
            if response.success {
                Ok(response.clone())
            } else {
                Err(response.stderr.clone())
            }
        } else {
            // Default success if not configured
            Ok(CommandResponse {
                success: true,
                stdout: String::new(),
                stderr: String::new(),
                exit_code: Some(0),
            })
        }
    }

    // ========== VERIFICATION METHODS ==========

    /// Verify that a specific command was called
    pub fn verify_called(&self, command: &str) -> bool {
        self.calls.lock().unwrap().iter().any(|c| c.command == command)
    }

    /// Verify that commands were called in a specific order
    pub fn verify_call_order(&self, commands: &[&str]) -> bool {
        let calls = self.calls.lock().unwrap();
        let call_commands: Vec<&str> = calls.iter().map(|c| c.command.as_str()).collect();

        let mut last_index = 0;
        for cmd in commands {
            if let Some(pos) = call_commands[last_index..].iter().position(|c| c == cmd) {
                last_index += pos + 1;
            } else {
                return false;
            }
        }
        true
    }

    /// Get the number of times a command was called
    pub fn call_count(&self, command: &str) -> usize {
        self.calls.lock().unwrap().iter().filter(|c| c.command == command).count()
    }

    /// Get all recorded calls
    pub fn get_calls(&self) -> Vec<CommandCall> {
        self.calls.lock().unwrap().clone()
    }

    /// Verify that a command was called with specific environment variables
    pub fn verify_called_with_env(&self, command: &str, env_key: &str, env_value: &str) -> bool {
        self.calls.lock().unwrap().iter().any(|c| {
            c.command == command && c.env.iter().any(|(k, v)| k == env_key && v == env_value)
        })
    }

    /// Verify that no commands were executed
    pub fn verify_no_calls(&self) -> bool {
        self.calls.lock().unwrap().is_empty()
    }

    /// Reset all recorded calls (useful for test isolation)
    pub fn reset(&self) {
        self.calls.lock().unwrap().clear();
    }
}

// ============================================================================
// MOCK STATE REPOSITORY
// ============================================================================

/// Mock implementation of state persistence for testing
///
/// This allows us to verify:
/// - When state is loaded/saved
/// - What state changes were made
/// - Without touching the filesystem
#[derive(Clone)]
pub struct MockStateRepository {
    state: Arc<Mutex<LifecycleStateData>>,
    load_calls: Arc<Mutex<usize>>,
    save_calls: Arc<Mutex<usize>>,
    should_fail_load: Arc<Mutex<bool>>,
    should_fail_save: Arc<Mutex<bool>>,
}

#[derive(Debug, Clone, Default)]
pub struct LifecycleStateData {
    pub last_phase: Option<String>,
    pub phase_history: Vec<RunRecordData>,
    pub cache_keys: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct RunRecordData {
    pub phase: String,
    pub started_ms: u128,
    pub duration_ms: u128,
    pub success: bool,
}

impl MockStateRepository {
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(LifecycleStateData::default())),
            load_calls: Arc::new(Mutex::new(0)),
            save_calls: Arc::new(Mutex::new(0)),
            should_fail_load: Arc::new(Mutex::new(false)),
            should_fail_save: Arc::new(Mutex::new(false)),
        }
    }

    /// Configure initial state
    pub fn with_initial_state(self, state: LifecycleStateData) -> Self {
        *self.state.lock().unwrap() = state;
        self
    }

    /// Configure load to fail
    pub fn with_load_failure(self) -> Self {
        *self.should_fail_load.lock().unwrap() = true;
        self
    }

    /// Configure save to fail
    pub fn with_save_failure(self) -> Self {
        *self.should_fail_save.lock().unwrap() = true;
        self
    }

    /// Load state (records call)
    pub fn load(&self) -> Result<LifecycleStateData, String> {
        *self.load_calls.lock().unwrap() += 1;

        if *self.should_fail_load.lock().unwrap() {
            Err("Mock load failure".to_string())
        } else {
            Ok(self.state.lock().unwrap().clone())
        }
    }

    /// Save state (records call)
    pub fn save(&self, state: &LifecycleStateData) -> Result<(), String> {
        *self.save_calls.lock().unwrap() += 1;

        if *self.should_fail_save.lock().unwrap() {
            Err("Mock save failure".to_string())
        } else {
            *self.state.lock().unwrap() = state.clone();
            Ok(())
        }
    }

    // ========== VERIFICATION METHODS ==========

    /// Get current state
    pub fn get_state(&self) -> LifecycleStateData {
        self.state.lock().unwrap().clone()
    }

    /// Verify load was called
    pub fn verify_load_called(&self) -> bool {
        *self.load_calls.lock().unwrap() > 0
    }

    /// Verify save was called
    pub fn verify_save_called(&self) -> bool {
        *self.save_calls.lock().unwrap() > 0
    }

    /// Get number of load calls
    pub fn load_call_count(&self) -> usize {
        *self.load_calls.lock().unwrap()
    }

    /// Get number of save calls
    pub fn save_call_count(&self) -> usize {
        *self.save_calls.lock().unwrap()
    }

    /// Verify that a phase record was added
    pub fn verify_phase_recorded(&self, phase: &str) -> bool {
        self.state.lock().unwrap().phase_history.iter().any(|r| r.phase == phase)
    }

    /// Verify that a cache key was stored
    pub fn verify_cache_key_stored(&self, phase: &str) -> bool {
        self.state.lock().unwrap().cache_keys.contains_key(phase)
    }

    /// Reset call counters
    pub fn reset(&self) {
        *self.load_calls.lock().unwrap() = 0;
        *self.save_calls.lock().unwrap() = 0;
    }
}

// ============================================================================
// MOCK HOOK REGISTRY
// ============================================================================

/// Mock implementation of hook execution for testing
///
/// This allows us to verify:
/// - Which hooks were executed
/// - In what order (before/after)
/// - Prevent infinite recursion
#[derive(Clone)]
pub struct MockHookRegistry {
    executed_hooks: Arc<Mutex<Vec<HookExecution>>>,
    should_fail: Arc<Mutex<HashMap<String, String>>>,
    recursion_guard: Arc<Mutex<Vec<String>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HookExecution {
    pub hook_name: String,
    pub phase_name: String,
    pub stage: HookStage,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HookStage {
    Before,
    After,
}

impl MockHookRegistry {
    pub fn new() -> Self {
        Self {
            executed_hooks: Arc::new(Mutex::new(Vec::new())),
            should_fail: Arc::new(Mutex::new(HashMap::new())),
            recursion_guard: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Configure a hook to fail
    pub fn with_hook_failure(self, hook_name: impl Into<String>, error: impl Into<String>) -> Self {
        self.should_fail.lock().unwrap().insert(hook_name.into(), error.into());
        self
    }

    /// Execute before hooks
    pub fn execute_before_hooks(&self, phase: &str, hooks: &[String]) -> Result<(), String> {
        // Check recursion
        let mut guard = self.recursion_guard.lock().unwrap();
        if guard.contains(&phase.to_string()) {
            return Err(format!("Hook recursion detected: phase '{}' already executing", phase));
        }
        guard.push(phase.to_string());
        drop(guard);

        // Execute hooks
        for hook in hooks {
            self.executed_hooks.lock().unwrap().push(HookExecution {
                hook_name: hook.clone(),
                phase_name: phase.to_string(),
                stage: HookStage::Before,
            });

            // Check if this hook should fail
            if let Some(error) = self.should_fail.lock().unwrap().get(hook) {
                return Err(error.clone());
            }
        }

        Ok(())
    }

    /// Execute after hooks
    pub fn execute_after_hooks(&self, phase: &str, hooks: &[String]) -> Result<(), String> {
        for hook in hooks {
            self.executed_hooks.lock().unwrap().push(HookExecution {
                hook_name: hook.clone(),
                phase_name: phase.to_string(),
                stage: HookStage::After,
            });

            // Check if this hook should fail
            if let Some(error) = self.should_fail.lock().unwrap().get(hook) {
                return Err(error.clone());
            }
        }

        // Release recursion guard
        let mut guard = self.recursion_guard.lock().unwrap();
        guard.retain(|p| p != phase);

        Ok(())
    }

    // ========== VERIFICATION METHODS ==========

    /// Verify that a hook was executed
    pub fn verify_hook_executed(&self, hook_name: &str, stage: HookStage) -> bool {
        self.executed_hooks.lock().unwrap().iter()
            .any(|h| h.hook_name == hook_name && h.stage == stage)
    }

    /// Verify hook execution order
    pub fn verify_execution_order(&self, expected_order: &[(String, HookStage)]) -> bool {
        let executed = self.executed_hooks.lock().unwrap();
        if executed.len() != expected_order.len() {
            return false;
        }

        executed.iter().zip(expected_order.iter()).all(|(exec, (name, stage))| {
            exec.hook_name == *name && exec.stage == *stage
        })
    }

    /// Get all executed hooks
    pub fn get_executed_hooks(&self) -> Vec<HookExecution> {
        self.executed_hooks.lock().unwrap().clone()
    }

    /// Verify no hooks were executed
    pub fn verify_no_hooks_executed(&self) -> bool {
        self.executed_hooks.lock().unwrap().is_empty()
    }

    /// Reset
    pub fn reset(&self) {
        self.executed_hooks.lock().unwrap().clear();
        self.recursion_guard.lock().unwrap().clear();
    }
}

// ============================================================================
// MOCK OBSERVER
// ============================================================================

/// Mock implementation of progress observer for testing
///
/// This allows us to verify:
/// - Progress notifications
/// - Event ordering
/// - Output capture
#[derive(Clone)]
pub struct MockObserver {
    events: Arc<Mutex<Vec<ObserverEvent>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObserverEvent {
    PhaseStarted { phase: String },
    PhaseCompleted { phase: String, success: bool, duration_ms: u128 },
    CommandExecuted { command: String, success: bool },
    HookExecuted { hook: String, stage: HookStage },
    Error { phase: String, error: String },
}

impl MockObserver {
    pub fn new() -> Self {
        Self {
            events: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn on_phase_start(&self, phase: &str) {
        self.events.lock().unwrap().push(ObserverEvent::PhaseStarted {
            phase: phase.to_string(),
        });
    }

    pub fn on_phase_complete(&self, phase: &str, success: bool, duration_ms: u128) {
        self.events.lock().unwrap().push(ObserverEvent::PhaseCompleted {
            phase: phase.to_string(),
            success,
            duration_ms,
        });
    }

    pub fn on_command_executed(&self, command: &str, success: bool) {
        self.events.lock().unwrap().push(ObserverEvent::CommandExecuted {
            command: command.to_string(),
            success,
        });
    }

    pub fn on_hook_executed(&self, hook: &str, stage: HookStage) {
        self.events.lock().unwrap().push(ObserverEvent::HookExecuted {
            hook: hook.to_string(),
            stage,
        });
    }

    pub fn on_error(&self, phase: &str, error: &str) {
        self.events.lock().unwrap().push(ObserverEvent::Error {
            phase: phase.to_string(),
            error: error.to_string(),
        });
    }

    // ========== VERIFICATION METHODS ==========

    /// Verify that a phase start event was emitted
    pub fn verify_phase_started(&self, phase: &str) -> bool {
        self.events.lock().unwrap().iter().any(|e| {
            matches!(e, ObserverEvent::PhaseStarted { phase: p } if p == phase)
        })
    }

    /// Verify that a phase completed successfully
    pub fn verify_phase_completed_successfully(&self, phase: &str) -> bool {
        self.events.lock().unwrap().iter().any(|e| {
            matches!(e, ObserverEvent::PhaseCompleted { phase: p, success: true, .. } if p == phase)
        })
    }

    /// Verify event order
    pub fn verify_event_order(&self, phase: &str) -> bool {
        let events = self.events.lock().unwrap();
        let phase_events: Vec<_> = events.iter()
            .filter(|e| match e {
                ObserverEvent::PhaseStarted { phase: p } => p == phase,
                ObserverEvent::PhaseCompleted { phase: p, .. } => p == phase,
                _ => false,
            })
            .collect();

        if phase_events.len() != 2 {
            return false;
        }

        matches!(phase_events[0], ObserverEvent::PhaseStarted { .. }) &&
        matches!(phase_events[1], ObserverEvent::PhaseCompleted { .. })
    }

    /// Get all events
    pub fn get_events(&self) -> Vec<ObserverEvent> {
        self.events.lock().unwrap().clone()
    }

    /// Reset
    pub fn reset(&self) {
        self.events.lock().unwrap().clear();
    }
}

// ============================================================================
// MOCK BUILDERS (Fluent API for test setup)
// ============================================================================

/// Builder for creating mock configurations
pub struct MockSetupBuilder {
    executor: MockCommandExecutor,
    state_repo: MockStateRepository,
    hook_registry: MockHookRegistry,
    observer: MockObserver,
}

impl MockSetupBuilder {
    pub fn new() -> Self {
        Self {
            executor: MockCommandExecutor::new(),
            state_repo: MockStateRepository::new(),
            hook_registry: MockHookRegistry::new(),
            observer: MockObserver::new(),
        }
    }

    pub fn with_command_success(mut self, command: &str) -> Self {
        self.executor = self.executor.with_success(command);
        self
    }

    pub fn with_command_failure(mut self, command: &str, error: &str) -> Self {
        self.executor = self.executor.with_failure(command, error);
        self
    }

    pub fn with_initial_state(mut self, state: LifecycleStateData) -> Self {
        self.state_repo = self.state_repo.with_initial_state(state);
        self
    }

    pub fn with_hook_failure(mut self, hook: &str, error: &str) -> Self {
        self.hook_registry = self.hook_registry.with_hook_failure(hook, error);
        self
    }

    pub fn build(self) -> MockSetup {
        MockSetup {
            executor: self.executor,
            state_repo: self.state_repo,
            hook_registry: self.hook_registry,
            observer: self.observer,
        }
    }
}

/// Complete mock setup for testing
pub struct MockSetup {
    pub executor: MockCommandExecutor,
    pub state_repo: MockStateRepository,
    pub hook_registry: MockHookRegistry,
    pub observer: MockObserver,
}

impl MockSetup {
    /// Reset all mocks (useful for test isolation)
    pub fn reset_all(&self) {
        self.executor.reset();
        self.state_repo.reset();
        self.hook_registry.reset();
        self.observer.reset();
    }
}

// ============================================================================
// MOCK VERIFICATION ASSERTIONS
// ============================================================================

/// Assertion helpers for London School TDD
pub mod assertions {
    use super::*;

    /// Assert that commands were executed in order
    pub fn assert_commands_in_order(executor: &MockCommandExecutor, commands: &[&str]) {
        assert!(
            executor.verify_call_order(commands),
            "Commands not executed in expected order. Expected: {:?}, Got: {:?}",
            commands,
            executor.get_calls().iter().map(|c| c.command.as_str()).collect::<Vec<_>>()
        );
    }

    /// Assert that state was saved
    pub fn assert_state_saved(state_repo: &MockStateRepository) {
        assert!(
            state_repo.verify_save_called(),
            "Expected state to be saved, but save was never called"
        );
    }

    /// Assert that hooks were executed in order
    pub fn assert_hooks_in_order(
        hook_registry: &MockHookRegistry,
        expected: &[(String, HookStage)]
    ) {
        assert!(
            hook_registry.verify_execution_order(expected),
            "Hooks not executed in expected order. Expected: {:?}, Got: {:?}",
            expected,
            hook_registry.get_executed_hooks()
        );
    }

    /// Assert that observer events occurred in order
    pub fn assert_phase_lifecycle(observer: &MockObserver, phase: &str) {
        assert!(
            observer.verify_event_order(phase),
            "Phase lifecycle events not in correct order for phase '{}'",
            phase
        );
    }
}
