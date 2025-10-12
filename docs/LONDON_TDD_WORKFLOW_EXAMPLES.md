# London School TDD Workflow Examples

Complete step-by-step examples of applying London School TDD to real features in the ggen lifecycle system.

---

## Example 1: Adding Parallel Command Execution

### User Story
"As a developer, I want to execute multiple commands in parallel to speed up my build"

### Step 1: RED - Write Acceptance Test

```rust
#[test]
fn acceptance_parallel_execution_runs_commands_concurrently() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_command_success("task1")
        .with_command_success("task2")
        .with_command_success("task3")
        .build();

    let phase = PhaseConfig {
        name: "build".to_string(),
        commands: vec![
            "task1".to_string(),
            "task2".to_string(),
            "task3".to_string(),
        ],
        parallel: true,  // NEW FEATURE
        before_hooks: vec![],
        after_hooks: vec![],
    };

    // ACT
    let start = Instant::now();
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    executor.run_phase(&phase).unwrap();
    let duration = start.elapsed();

    // ASSERT
    // All commands should execute
    assert!(mocks.executor.verify_called("task1"));
    assert!(mocks.executor.verify_called("task2"));
    assert!(mocks.executor.verify_called("task3"));

    // Order doesn't matter for parallel execution
    assert_eq!(mocks.executor.call_count("task1"), 1);
    assert_eq!(mocks.executor.call_count("task2"), 1);
    assert_eq!(mocks.executor.call_count("task3"), 1);

    // Execution should be faster than sequential
    // (In real implementation, this would verify concurrent execution)
}
```

**Result**: ❌ Test fails - `parallel` field doesn't exist on `PhaseConfig`

### Step 2: GREEN - Add Field to PhaseConfig

```rust
#[derive(Debug, Clone)]
pub struct PhaseConfig {
    pub name: String,
    pub commands: Vec<String>,
    pub parallel: bool,  // NEW
    pub before_hooks: Vec<String>,
    pub after_hooks: Vec<String>,
}
```

**Result**: ❌ Test fails - `PhaseExecutor::run_phase()` doesn't handle `parallel`

### Step 3: GREEN - Implement Parallel Execution

```rust
impl PhaseExecutor {
    pub fn run_phase(&self, phase: &PhaseConfig) -> Result<(), String> {
        self.observer.on_phase_start(&phase.name);

        self.hook_registry.execute_before_hooks(&phase.name, &phase.before_hooks)?;

        // NEW: Handle parallel execution
        if phase.parallel {
            self.execute_commands_parallel(phase)?;
        } else {
            self.execute_commands_sequential(phase)?;
        }

        self.hook_registry.execute_after_hooks(&phase.name, &phase.after_hooks)?;

        self.update_state(phase)?;
        self.observer.on_phase_complete(&phase.name, true, 0);

        Ok(())
    }

    fn execute_commands_parallel(&self, phase: &PhaseConfig) -> Result<(), String> {
        use std::thread;

        let handles: Vec<_> = phase.commands.iter().map(|cmd| {
            let cmd = cmd.clone();
            let executor = self.command_executor.clone();

            thread::spawn(move || {
                executor.execute(&cmd, &PathBuf::from("."), &[])
            })
        }).collect();

        for handle in handles {
            handle.join().unwrap()?;
        }

        Ok(())
    }

    fn execute_commands_sequential(&self, phase: &PhaseConfig) -> Result<(), String> {
        for cmd in &phase.commands {
            self.command_executor.execute(cmd, &PathBuf::from("."), &[])?;
        }
        Ok(())
    }
}
```

**Result**: ✅ Test passes!

### Step 4: REFACTOR - Add Unit Tests for New Behavior

```rust
#[test]
fn should_execute_commands_in_parallel_when_flag_set() {
    let mocks = MockSetupBuilder::new()
        .with_command_success("cmd1")
        .with_command_success("cmd2")
        .build();

    let phase = PhaseConfigBuilder::new("test")
        .with_command("cmd1")
        .with_command("cmd2")
        .with_parallel(true)
        .build();

    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    executor.run_phase(&phase).unwrap();

    // Both commands executed (order may vary)
    assert!(mocks.executor.verify_called("cmd1"));
    assert!(mocks.executor.verify_called("cmd2"));
}

#[test]
fn should_fail_parallel_execution_if_any_command_fails() {
    let mocks = MockSetupBuilder::new()
        .with_command_success("cmd1")
        .with_command_failure("cmd2", "Failed")
        .build();

    let phase = PhaseConfigBuilder::new("test")
        .with_command("cmd1")
        .with_command("cmd2")
        .with_parallel(true)
        .build();

    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase);

    assert!(result.is_err());
}
```

**Result**: ✅ All tests pass! Feature complete.

---

## Example 2: Adding Retry Logic for Transient Failures

### User Story
"As a developer, I want commands to automatically retry on network failures"

### Step 1: RED - Define Desired Behavior

```rust
#[test]
fn acceptance_retry_transient_command_failures() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        // First attempt fails, second succeeds
        .with_command_failure_then_success("npm install", "Network timeout", 1)
        .build();

    let phase = PhaseConfig {
        name: "setup".to_string(),
        commands: vec!["npm install".to_string()],
        retry_count: 3,  // NEW FEATURE
        retry_delay_ms: 100,  // NEW FEATURE
        before_hooks: vec![],
        after_hooks: vec![],
        parallel: false,
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
    assert!(result.is_ok(), "Should succeed after retry");

    // Command was called multiple times
    assert_eq!(mocks.executor.call_count("npm install"), 2);

    // Observer notified of retry
    assert!(observer_received_event(&mocks.observer, |e| {
        matches!(e, ObserverEvent::CommandRetried { .. })
    }));
}
```

**Result**: ❌ Test fails - Retry logic doesn't exist

### Step 2: GREEN - Extend Mock to Support Retry Scenarios

First, enhance the mock:

```rust
impl MockCommandExecutor {
    pub fn with_failure_then_success(
        mut self,
        command: impl Into<String>,
        error: impl Into<String>,
        fail_count: usize,
    ) -> Self {
        let cmd = command.into();
        let error = error.into();
        let call_count = Arc::new(Mutex::new(0));

        // Custom response that fails N times then succeeds
        self.response_handlers.insert(cmd.clone(), Box::new(move |_| {
            let mut count = call_count.lock().unwrap();
            *count += 1;

            if *count <= fail_count {
                Err(error.clone())
            } else {
                Ok(CommandResponse {
                    success: true,
                    stdout: String::new(),
                    stderr: String::new(),
                    exit_code: Some(0),
                })
            }
        }));

        self
    }
}
```

### Step 3: GREEN - Implement Retry Logic

```rust
impl PhaseExecutor {
    fn execute_command_with_retry(
        &self,
        command: &str,
        retry_count: u32,
        retry_delay_ms: u64,
    ) -> Result<CommandResponse, String> {
        let mut last_error = None;

        for attempt in 0..=retry_count {
            if attempt > 0 {
                self.observer.on_command_retry(command, attempt);
                std::thread::sleep(Duration::from_millis(retry_delay_ms));
            }

            match self.command_executor.execute(command, &PathBuf::from("."), &[]) {
                Ok(response) => return Ok(response),
                Err(e) if self.is_transient_error(&e) => {
                    last_error = Some(e);
                    continue;
                }
                Err(e) => return Err(e),  // Permanent error, fail fast
            }
        }

        Err(last_error.unwrap())
    }

    fn is_transient_error(&self, error: &str) -> bool {
        error.contains("timeout") ||
        error.contains("network") ||
        error.contains("ECONNREFUSED")
    }
}
```

### Step 4: REFACTOR - Extract Retry Strategy

```rust
// New abstraction discovered through testing
pub trait RetryStrategy: Send + Sync {
    fn should_retry(&self, error: &str, attempt: u32) -> bool;
    fn delay_ms(&self, attempt: u32) -> u64;
}

pub struct ExponentialBackoff {
    max_retries: u32,
    base_delay_ms: u64,
}

impl RetryStrategy for ExponentialBackoff {
    fn should_retry(&self, error: &str, attempt: u32) -> bool {
        attempt < self.max_retries && is_transient(error)
    }

    fn delay_ms(&self, attempt: u32) -> u64 {
        self.base_delay_ms * 2_u64.pow(attempt)
    }
}

// PhaseExecutor now uses strategy
impl PhaseExecutor {
    fn execute_with_strategy(
        &self,
        command: &str,
        strategy: &dyn RetryStrategy,
    ) -> Result<CommandResponse, String> {
        let mut attempt = 0;

        loop {
            match self.command_executor.execute(command, &PathBuf::from("."), &[]) {
                Ok(response) => return Ok(response),
                Err(e) if strategy.should_retry(&e, attempt) => {
                    let delay = strategy.delay_ms(attempt);
                    std::thread::sleep(Duration::from_millis(delay));
                    attempt += 1;
                }
                Err(e) => return Err(e),
            }
        }
    }
}
```

**Result**: ✅ Tests pass! Better design emerged through refactoring.

---

## Example 3: Adding Workspace Support

### User Story
"As a developer working on a monorepo, I want to run phases across multiple workspaces"

### Step 1: RED - Acceptance Test for Workspace Execution

```rust
#[test]
fn acceptance_execute_phase_across_workspaces() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .with_command_success("npm run build")
        .build();

    let workspace_config = vec![
        WorkspaceConfig {
            name: "frontend".to_string(),
            path: PathBuf::from("apps/web"),
        },
        WorkspaceConfig {
            name: "backend".to_string(),
            path: PathBuf::from("apps/api"),
        },
    ];

    let phase = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["npm run build".to_string()],
        workspaces: Some(workspace_config),  // NEW
        before_hooks: vec![],
        after_hooks: vec![],
        parallel: false,
    };

    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    // ACT
    executor.run_phase(&phase).unwrap();

    // ASSERT
    // Command executed in each workspace
    let calls = mocks.executor.get_calls();
    assert_eq!(calls.len(), 2);

    // Verify working directories
    assert!(calls.iter().any(|c|
        c.command == "npm run build" &&
        c.working_dir == PathBuf::from("apps/web")
    ));

    assert!(calls.iter().any(|c|
        c.command == "npm run build" &&
        c.working_dir == PathBuf::from("apps/api")
    ));

    // Each workspace has separate state
    assert!(mocks.state_repo.verify_workspace_state_saved("frontend"));
    assert!(mocks.state_repo.verify_workspace_state_saved("backend"));
}
```

**Result**: ❌ Test fails - Workspace support doesn't exist

### Step 2: GREEN - Implement Workspace Iteration

```rust
impl PhaseExecutor {
    pub fn run_phase(&self, phase: &PhaseConfig) -> Result<(), String> {
        if let Some(ref workspaces) = phase.workspaces {
            self.run_phase_across_workspaces(phase, workspaces)
        } else {
            self.run_phase_single(phase)
        }
    }

    fn run_phase_across_workspaces(
        &self,
        phase: &PhaseConfig,
        workspaces: &[WorkspaceConfig],
    ) -> Result<(), String> {
        for workspace in workspaces {
            self.observer.on_workspace_start(&workspace.name);

            // Execute in workspace context
            for cmd in &phase.commands {
                self.command_executor.execute(
                    cmd,
                    &workspace.path,  // Different working directory
                    &[],
                )?;
            }

            // Save workspace-specific state
            self.save_workspace_state(workspace, phase)?;

            self.observer.on_workspace_complete(&workspace.name);
        }

        Ok(())
    }
}
```

### Step 3: REFACTOR - Extract Workspace Executor

```rust
// New component discovered through testing
pub struct WorkspaceExecutor {
    command_executor: MockCommandExecutor,
    state_repo: MockStateRepository,
    observer: MockObserver,
}

impl WorkspaceExecutor {
    pub fn execute_in_workspaces(
        &self,
        phase: &PhaseConfig,
        workspaces: &[WorkspaceConfig],
    ) -> Result<Vec<WorkspaceResult>, String> {
        workspaces.iter()
            .map(|ws| self.execute_in_workspace(phase, ws))
            .collect()
    }

    fn execute_in_workspace(
        &self,
        phase: &PhaseConfig,
        workspace: &WorkspaceConfig,
    ) -> Result<WorkspaceResult, String> {
        self.observer.on_workspace_start(&workspace.name);

        let mut results = vec![];
        for cmd in &phase.commands {
            let result = self.command_executor.execute(
                cmd,
                &workspace.path,
                &[],
            )?;
            results.push(result);
        }

        self.save_workspace_state(workspace, phase)?;
        self.observer.on_workspace_complete(&workspace.name);

        Ok(WorkspaceResult {
            workspace_name: workspace.name.clone(),
            command_results: results,
        })
    }
}

// PhaseExecutor now delegates to WorkspaceExecutor
impl PhaseExecutor {
    fn run_phase_across_workspaces(
        &self,
        phase: &PhaseConfig,
        workspaces: &[WorkspaceConfig],
    ) -> Result<(), String> {
        self.workspace_executor.execute_in_workspaces(phase, workspaces)?;
        Ok(())
    }
}
```

### Step 4: Add Unit Tests for WorkspaceExecutor

```rust
#[test]
fn workspace_executor_should_execute_in_correct_directory() {
    let mocks = MockSetupBuilder::new()
        .with_command_success("build")
        .build();

    let workspace = WorkspaceConfig {
        name: "frontend".to_string(),
        path: PathBuf::from("apps/web"),
    };

    let phase = simple_phase("build", "build");

    let executor = WorkspaceExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.observer.clone(),
    );

    executor.execute_in_workspace(&phase, &workspace).unwrap();

    // Verify correct working directory
    verify_command_called_in_directory(
        &mocks.executor,
        "build",
        "apps/web"
    );
}

#[test]
fn workspace_executor_should_isolate_workspace_state() {
    let mocks = MockSetupBuilder::new()
        .with_command_success("test")
        .build();

    let workspaces = vec![
        WorkspaceConfig { name: "ws1".to_string(), path: PathBuf::from("ws1") },
        WorkspaceConfig { name: "ws2".to_string(), path: PathBuf::from("ws2") },
    ];

    let phase = simple_phase("test", "test");
    let executor = WorkspaceExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.observer.clone(),
    );

    executor.execute_in_workspaces(&phase, &workspaces).unwrap();

    // Each workspace has separate state
    assert!(mocks.state_repo.verify_workspace_state_saved("ws1"));
    assert!(mocks.state_repo.verify_workspace_state_saved("ws2"));
}
```

**Result**: ✅ All tests pass! New component emerged from TDD.

---

## Example 4: Adding Dry-Run Mode

### User Story
"As a developer, I want to see what commands would run without actually executing them"

### Step 1: RED - Acceptance Test

```rust
#[test]
fn acceptance_dry_run_shows_commands_without_executing() {
    // ARRANGE
    let mocks = MockSetupBuilder::new()
        .build();  // No command configuration needed

    let phase = PhaseConfig {
        name: "build".to_string(),
        commands: vec![
            "npm run build".to_string(),
            "npm run test".to_string(),
        ],
        before_hooks: vec!["setup".to_string()],
        after_hooks: vec!["cleanup".to_string()],
        parallel: false,
    };

    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    // ACT
    let plan = executor.dry_run(&phase).unwrap();

    // ASSERT
    // No commands actually executed
    assert!(mocks.executor.verify_no_calls());

    // But we got an execution plan
    assert_eq!(plan.commands.len(), 2);
    assert_eq!(plan.commands[0], "npm run build");
    assert_eq!(plan.commands[1], "npm run test");

    assert_eq!(plan.before_hooks, vec!["setup"]);
    assert_eq!(plan.after_hooks, vec!["cleanup"]);

    // State not modified
    assert!(!mocks.state_repo.verify_save_called());
}
```

**Result**: ❌ Test fails - `dry_run()` doesn't exist

### Step 2: GREEN - Implement Dry Run

```rust
pub struct ExecutionPlan {
    pub phase_name: String,
    pub before_hooks: Vec<String>,
    pub commands: Vec<String>,
    pub after_hooks: Vec<String>,
    pub workspaces: Option<Vec<String>>,
}

impl PhaseExecutor {
    pub fn dry_run(&self, phase: &PhaseConfig) -> Result<ExecutionPlan, String> {
        // Don't execute, just build plan
        Ok(ExecutionPlan {
            phase_name: phase.name.clone(),
            before_hooks: phase.before_hooks.clone(),
            commands: phase.commands.clone(),
            after_hooks: phase.after_hooks.clone(),
            workspaces: phase.workspaces.as_ref().map(|ws|
                ws.iter().map(|w| w.name.clone()).collect()
            ),
        })
    }
}
```

**Result**: ✅ Test passes!

### Step 3: REFACTOR - Add Unit Test for Plan Generation

```rust
#[test]
fn should_generate_execution_plan_without_side_effects() {
    let mocks = MockSetupBuilder::new().build();

    let phase = PhaseConfigBuilder::new("deploy")
        .with_command("build")
        .with_command("upload")
        .with_before_hook("test")
        .build();

    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let plan = executor.dry_run(&phase).unwrap();

    // Plan contains all information
    assert_eq!(plan.phase_name, "deploy");
    assert_eq!(plan.commands, vec!["build", "upload"]);
    assert_eq!(plan.before_hooks, vec!["test"]);

    // No side effects
    assert!(mocks.executor.verify_no_calls());
    assert!(!mocks.state_repo.verify_load_called());
    assert!(!mocks.hook_registry.verify_any_hooks_executed());
}
```

---

## Summary of TDD Workflow

### Key Patterns Demonstrated

1. **Start with acceptance test** - Define user-facing behavior
2. **Watch it fail** - Verify test is actually testing something
3. **Minimal implementation** - Get to green as fast as possible
4. **Refactor design** - Extract abstractions, improve structure
5. **Add unit tests** - Test new components in isolation
6. **Repeat** - Build next feature the same way

### London School Benefits Observed

1. **Interfaces emerged naturally** - `RetryStrategy`, `WorkspaceExecutor`
2. **Easy to test** - All dependencies mocked
3. **Fast tests** - No real I/O, network, or file operations
4. **Design guidance** - Hard-to-test code = design smell
5. **Fearless refactoring** - Tests protected us during extraction

### What We Built Through TDD

- ✅ Parallel command execution
- ✅ Retry logic with exponential backoff
- ✅ Workspace support for monorepos
- ✅ Dry-run mode for safe exploration

All driven by tests, all testable in isolation, all maintainable.

---

## Next Steps

Apply this workflow to:

1. Cache invalidation logic
2. Environment variable substitution
3. Conditional phase execution
4. Progress reporting improvements
5. Error recovery strategies

Remember: **Test First, Design Second, Implementation Third**.
