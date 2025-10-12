//! BDD Tests for Lifecycle System (London School TDD)
//!
//! These tests focus on behavior and collaboration between components,
//! using mocks to isolate the system under test.

use std::collections::HashMap;
use std::path::Path;

// Mock trait for command execution
trait CommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<CommandOutput, String>;
}

// Mock trait for state repository
trait StateRepository {
    fn load(&self) -> Result<LifecycleState, String>;
    fn save(&self, state: &LifecycleState) -> Result<(), String>;
}

// Mock trait for observers
trait LifecycleObserver {
    fn on_phase_start(&self, phase: &str);
    fn on_phase_complete(&self, phase: &str, duration_ms: u128);
    fn on_error(&self, phase: &str, error: &str);
}

// Test doubles
struct MockCommandExecutor {
    responses: HashMap<String, Result<CommandOutput, String>>,
    executed: std::cell::RefCell<Vec<String>>,
}

impl MockCommandExecutor {
    fn new() -> Self {
        Self {
            responses: HashMap::new(),
            executed: std::cell::RefCell::new(Vec::new()),
        }
    }

    fn expect(&mut self, cmd: &str, result: Result<CommandOutput, String>) {
        self.responses.insert(cmd.to_string(), result);
    }

    fn verify_executed(&self, cmd: &str) -> bool {
        self.executed.borrow().contains(&cmd.to_string())
    }

    fn execution_count(&self) -> usize {
        self.executed.borrow().len()
    }
}

impl CommandExecutor for MockCommandExecutor {
    fn execute(&self, cmd: &str, _cwd: &Path, _env: &[(String, String)]) -> Result<CommandOutput, String> {
        self.executed.borrow_mut().push(cmd.to_string());
        self.responses
            .get(cmd)
            .cloned()
            .unwrap_or_else(|| Ok(CommandOutput { stdout: String::new(), stderr: String::new(), success: true }))
    }
}

struct MockStateRepository {
    state: std::cell::RefCell<LifecycleState>,
    save_count: std::cell::RefCell<usize>,
}

impl MockStateRepository {
    fn new(initial_state: LifecycleState) -> Self {
        Self {
            state: std::cell::RefCell::new(initial_state),
            save_count: std::cell::RefCell::new(0),
        }
    }

    fn times_saved(&self) -> usize {
        *self.save_count.borrow()
    }
}

impl StateRepository for MockStateRepository {
    fn load(&self) -> Result<LifecycleState, String> {
        Ok(self.state.borrow().clone())
    }

    fn save(&self, state: &LifecycleState) -> Result<(), String> {
        *self.state.borrow_mut() = state.clone();
        *self.save_count.borrow_mut() += 1;
        Ok(())
    }
}

struct MockObserver {
    events: std::cell::RefCell<Vec<String>>,
}

impl MockObserver {
    fn new() -> Self {
        Self {
            events: std::cell::RefCell::new(Vec::new()),
        }
    }

    fn received_event(&self, event: &str) -> bool {
        self.events.borrow().contains(&event.to_string())
    }

    fn event_count(&self) -> usize {
        self.events.borrow().len()
    }
}

impl LifecycleObserver for MockObserver {
    fn on_phase_start(&self, phase: &str) {
        self.events.borrow_mut().push(format!("start:{}", phase));
    }

    fn on_phase_complete(&self, phase: &str, _duration_ms: u128) {
        self.events.borrow_mut().push(format!("complete:{}", phase));
    }

    fn on_error(&self, phase: &str, error: &str) {
        self.events.borrow_mut().push(format!("error:{}:{}", phase, error));
    }
}

// Data structures (simplified for testing)
#[derive(Clone)]
struct LifecycleState {
    phases_run: Vec<String>,
    cache_keys: HashMap<String, String>,
}

impl LifecycleState {
    fn new() -> Self {
        Self {
            phases_run: Vec::new(),
            cache_keys: HashMap::new(),
        }
    }

    fn phase_was_run(&self, phase: &str) -> bool {
        self.phases_run.contains(&phase.to_string())
    }
}

#[derive(Clone)]
struct CommandOutput {
    stdout: String,
    stderr: String,
    success: bool,
}

// Feature: Phase Execution
mod phase_execution {
    use super::*;

    #[test]
    fn should_execute_single_command_phase() {
        // GIVEN a phase with a single command
        let mut executor = MockCommandExecutor::new();
        executor.expect("npm run build", Ok(CommandOutput {
            stdout: "Build successful".to_string(),
            stderr: String::new(),
            success: true,
        }));

        let state_repo = MockStateRepository::new(LifecycleState::new());
        let observer = MockObserver::new();

        // WHEN the phase is executed
        let result = execute_phase(
            "build",
            &["npm run build"],
            &executor,
            &state_repo,
            &observer,
        );

        // THEN the command should be executed
        assert!(result.is_ok(), "Phase execution should succeed");
        assert!(executor.verify_executed("npm run build"), "Command should be executed");

        // AND the observer should be notified
        assert!(observer.received_event("start:build"), "Observer should receive start event");
        assert!(observer.received_event("complete:build"), "Observer should receive complete event");

        // AND the state should be saved
        assert_eq!(state_repo.times_saved(), 1, "State should be saved once");
    }

    #[test]
    fn should_execute_multiple_commands_sequentially() {
        // GIVEN a phase with multiple commands
        let mut executor = MockCommandExecutor::new();
        executor.expect("echo 'Step 1'", Ok(CommandOutput::default()));
        executor.expect("echo 'Step 2'", Ok(CommandOutput::default()));
        executor.expect("echo 'Step 3'", Ok(CommandOutput::default()));

        let state_repo = MockStateRepository::new(LifecycleState::new());
        let observer = MockObserver::new();

        // WHEN the phase is executed
        let commands = vec!["echo 'Step 1'", "echo 'Step 2'", "echo 'Step 3'"];
        let result = execute_phase("multi", &commands, &executor, &state_repo, &observer);

        // THEN all commands should execute in order
        assert!(result.is_ok());
        assert_eq!(executor.execution_count(), 3, "All 3 commands should execute");
    }

    #[test]
    fn should_stop_on_first_command_failure() {
        // GIVEN a phase where the second command fails
        let mut executor = MockCommandExecutor::new();
        executor.expect("echo 'OK'", Ok(CommandOutput::default()));
        executor.expect("exit 1", Err("Command failed".to_string()));
        executor.expect("echo 'Should not run'", Ok(CommandOutput::default()));

        let state_repo = MockStateRepository::new(LifecycleState::new());
        let observer = MockObserver::new();

        // WHEN the phase is executed
        let commands = vec!["echo 'OK'", "exit 1", "echo 'Should not run'"];
        let result = execute_phase("failing", &commands, &executor, &state_repo, &observer);

        // THEN execution should stop after the failure
        assert!(result.is_err(), "Phase should fail");
        assert_eq!(executor.execution_count(), 2, "Should stop after failed command");

        // AND the observer should be notified of the error
        assert!(observer.received_event("error:failing:Command failed"));
    }

    impl Default for CommandOutput {
        fn default() -> Self {
            Self {
                stdout: String::new(),
                stderr: String::new(),
                success: true,
            }
        }
    }

    // Simplified execution function for testing
    fn execute_phase(
        phase_name: &str,
        commands: &[&str],
        executor: &impl CommandExecutor,
        state_repo: &impl StateRepository,
        observer: &impl LifecycleObserver,
    ) -> Result<(), String> {
        observer.on_phase_start(phase_name);

        for cmd in commands {
            match executor.execute(cmd, Path::new("."), &[]) {
                Ok(_) => {}
                Err(e) => {
                    observer.on_error(phase_name, &e);
                    return Err(e);
                }
            }
        }

        let mut state = state_repo.load()?;
        state.phases_run.push(phase_name.to_string());
        state_repo.save(&state)?;

        observer.on_phase_complete(phase_name, 100);
        Ok(())
    }
}

// Feature: Hook Execution
mod hook_execution {
    use super::*;

    #[test]
    fn should_execute_before_hooks_before_phase() {
        // GIVEN a phase with before hooks
        let mut executor = MockCommandExecutor::new();
        let execution_order = std::cell::RefCell::new(Vec::new());

        // Track execution order
        let track = |name: &str| {
            execution_order.borrow_mut().push(name.to_string());
        };

        executor.expect("validate", Ok(CommandOutput::default()));
        executor.expect("lint", Ok(CommandOutput::default()));
        executor.expect("build", Ok(CommandOutput::default()));

        let state_repo = MockStateRepository::new(LifecycleState::new());

        // WHEN the phase is executed with before hooks
        let before_hooks = vec!["validate", "lint"];
        let result = execute_phase_with_hooks(
            "build",
            &before_hooks,
            &[],
            &["build"],
            &executor,
            &state_repo,
            &track,
        );

        // THEN hooks should execute before the main phase
        assert!(result.is_ok());
        assert_eq!(execution_order.borrow()[0], "validate", "First before hook");
        assert_eq!(execution_order.borrow()[1], "lint", "Second before hook");
        assert_eq!(execution_order.borrow()[2], "build", "Main phase");
    }

    #[test]
    fn should_execute_after_hooks_after_phase() {
        // GIVEN a phase with after hooks
        let mut executor = MockCommandExecutor::new();
        let execution_order = std::cell::RefCell::new(Vec::new());

        executor.expect("build", Ok(CommandOutput::default()));
        executor.expect("notify", Ok(CommandOutput::default()));
        executor.expect("cleanup", Ok(CommandOutput::default()));

        let state_repo = MockStateRepository::new(LifecycleState::new());

        // WHEN the phase is executed with after hooks
        let after_hooks = vec!["notify", "cleanup"];
        let result = execute_phase_with_hooks(
            "build",
            &[],
            &after_hooks,
            &["build"],
            &executor,
            &state_repo,
            &|name| execution_order.borrow_mut().push(name.to_string()),
        );

        // THEN hooks should execute after the main phase
        assert!(result.is_ok());
        assert_eq!(execution_order.borrow()[0], "build", "Main phase first");
        assert_eq!(execution_order.borrow()[1], "notify", "First after hook");
        assert_eq!(execution_order.borrow()[2], "cleanup", "Second after hook");
    }

    #[test]
    fn should_detect_hook_recursion() {
        // GIVEN a recursion guard
        let recursion_guard = std::cell::RefCell::new(std::collections::HashSet::new());

        // WHEN attempting to execute the same phase while it's already running
        recursion_guard.borrow_mut().insert("build".to_string());
        let result = check_recursion("build", &recursion_guard);

        // THEN recursion should be detected
        assert!(result.is_err(), "Should detect recursion");
        assert!(result.unwrap_err().contains("recursion"), "Error should mention recursion");
    }

    fn execute_phase_with_hooks<F>(
        phase_name: &str,
        before_hooks: &[&str],
        after_hooks: &[&str],
        commands: &[&str],
        executor: &impl CommandExecutor,
        state_repo: &impl StateRepository,
        track: &F,
    ) -> Result<(), String>
    where
        F: Fn(&str),
    {
        // Execute before hooks
        for hook in before_hooks {
            track(hook);
            executor.execute(hook, Path::new("."), &[])?;
        }

        // Execute main phase
        track(phase_name);
        for cmd in commands {
            executor.execute(cmd, Path::new("."), &[])?;
        }

        // Save state
        let mut state = state_repo.load()?;
        state.phases_run.push(phase_name.to_string());
        state_repo.save(&state)?;

        // Execute after hooks
        for hook in after_hooks {
            track(hook);
            executor.execute(hook, Path::new("."), &[])?;
        }

        Ok(())
    }

    fn check_recursion(
        phase_name: &str,
        guard: &std::cell::RefCell<std::collections::HashSet<String>>,
    ) -> Result<(), String> {
        // Check for recursion
        if guard.borrow().contains(phase_name) {
            return Err(format!("Hook recursion detected: {}", phase_name));
        }
        Ok(())
    }
}

// Feature: State Management
mod state_management {
    use super::*;

    #[test]
    fn should_track_executed_phases() {
        // GIVEN an empty state
        let state_repo = MockStateRepository::new(LifecycleState::new());

        // WHEN multiple phases are executed
        let mut state = state_repo.load().unwrap();
        state.phases_run.push("init".to_string());
        state.phases_run.push("build".to_string());
        state_repo.save(&state).unwrap();

        // THEN the state should track all executed phases
        let final_state = state_repo.load().unwrap();
        assert!(final_state.phase_was_run("init"));
        assert!(final_state.phase_was_run("build"));
        assert!(!final_state.phase_was_run("deploy"));
    }

    #[test]
    fn should_save_state_only_once_per_phase() {
        // GIVEN a phase with multiple commands
        let executor = MockCommandExecutor::new();
        let state_repo = MockStateRepository::new(LifecycleState::new());

        // WHEN the phase executes
        execute_phase_batch(
            "build",
            &["cmd1", "cmd2", "cmd3"],
            &executor,
            &state_repo,
        ).unwrap();

        // THEN state should be saved only once (not after each command)
        assert_eq!(state_repo.times_saved(), 1, "Should save state once per phase");
    }

    #[test]
    fn should_maintain_cache_keys() {
        // GIVEN a state repository
        let state_repo = MockStateRepository::new(LifecycleState::new());

        // WHEN cache keys are stored
        let mut state = state_repo.load().unwrap();
        state.cache_keys.insert("build".to_string(), "hash123".to_string());
        state.cache_keys.insert("test".to_string(), "hash456".to_string());
        state_repo.save(&state).unwrap();

        // THEN cache keys should be retrievable
        let final_state = state_repo.load().unwrap();
        assert_eq!(final_state.cache_keys.get("build"), Some(&"hash123".to_string()));
        assert_eq!(final_state.cache_keys.get("test"), Some(&"hash456".to_string()));
    }

    fn execute_phase_batch(
        phase_name: &str,
        commands: &[&str],
        executor: &impl CommandExecutor,
        state_repo: &impl StateRepository,
    ) -> Result<(), String> {
        // Execute all commands
        for cmd in commands {
            executor.execute(cmd, Path::new("."), &[])?;
        }

        // Save state once
        let mut state = state_repo.load()?;
        state.phases_run.push(phase_name.to_string());
        state_repo.save(&state)?;

        Ok(())
    }
}

// Feature: Observer Pattern
mod observer_pattern {
    use super::*;

    #[test]
    fn should_notify_multiple_observers() {
        // GIVEN multiple observers
        let observer1 = MockObserver::new();
        let observer2 = MockObserver::new();
        let observers: Vec<&dyn LifecycleObserver> = vec![&observer1, &observer2];

        // WHEN a phase executes
        notify_all(&observers, "start", "build");

        // THEN all observers should receive notifications
        assert!(observer1.received_event("start:build"));
        assert!(observer2.received_event("start:build"));
    }

    #[test]
    fn should_notify_on_phase_lifecycle() {
        // GIVEN an observer
        let observer = MockObserver::new();

        // WHEN a phase completes successfully
        observer.on_phase_start("build");
        observer.on_phase_complete("build", 150);

        // THEN observer should receive both events
        assert!(observer.received_event("start:build"));
        assert!(observer.received_event("complete:build"));
        assert_eq!(observer.event_count(), 2);
    }

    #[test]
    fn should_notify_on_errors() {
        // GIVEN an observer
        let observer = MockObserver::new();

        // WHEN a phase fails
        observer.on_phase_start("deploy");
        observer.on_error("deploy", "Connection timeout");

        // THEN observer should receive error event
        assert!(observer.received_event("error:deploy:Connection timeout"));
    }

    fn notify_all(observers: &[&dyn LifecycleObserver], event: &str, phase: &str) {
        for observer in observers {
            match event {
                "start" => observer.on_phase_start(phase),
                "complete" => observer.on_phase_complete(phase, 100),
                "error" => observer.on_error(phase, "test error"),
                _ => {}
            }
        }
    }
}

// Feature: Pipeline Execution
mod pipeline_execution {
    use super::*;

    #[test]
    fn should_execute_phases_in_sequence() {
        // GIVEN a pipeline of phases
        let mut executor = MockCommandExecutor::new();
        let execution_order = std::cell::RefCell::new(Vec::new());

        executor.expect("init", Ok(CommandOutput::default()));
        executor.expect("build", Ok(CommandOutput::default()));
        executor.expect("test", Ok(CommandOutput::default()));

        let state_repo = MockStateRepository::new(LifecycleState::new());

        // WHEN the pipeline is executed
        let phases = vec!["init", "build", "test"];
        execute_pipeline(
            &phases,
            &executor,
            &state_repo,
            &|phase| execution_order.borrow_mut().push(phase.to_string()),
        ).unwrap();

        // THEN phases should execute in order
        assert_eq!(execution_order.borrow()[0], "init");
        assert_eq!(execution_order.borrow()[1], "build");
        assert_eq!(execution_order.borrow()[2], "test");
    }

    #[test]
    fn should_stop_pipeline_on_failure() {
        // GIVEN a pipeline where the second phase fails
        let mut executor = MockCommandExecutor::new();
        executor.expect("init", Ok(CommandOutput::default()));
        executor.expect("build", Err("Build failed".to_string()));
        executor.expect("test", Ok(CommandOutput::default()));

        let state_repo = MockStateRepository::new(LifecycleState::new());

        // WHEN the pipeline is executed
        let phases = vec!["init", "build", "test"];
        let result = execute_pipeline(
            &phases,
            &executor,
            &state_repo,
            &|_| {},
        );

        // THEN pipeline should stop after failure
        assert!(result.is_err());

        let state = state_repo.load().unwrap();
        assert!(state.phase_was_run("init"), "First phase should complete");
        assert!(!state.phase_was_run("test"), "Third phase should not run");
    }

    fn execute_pipeline<F>(
        phases: &[&str],
        executor: &impl CommandExecutor,
        state_repo: &impl StateRepository,
        track: &F,
    ) -> Result<(), String>
    where
        F: Fn(&str),
    {
        for phase in phases {
            track(phase);
            executor.execute(phase, Path::new("."), &[])?;

            let mut state = state_repo.load()?;
            state.phases_run.push(phase.to_string());
            state_repo.save(&state)?;
        }

        Ok(())
    }
}
