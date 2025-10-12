# London School TDD Examples for ggen Lifecycle System

Practical test examples using mockall for the lifecycle system.

## Example 1: Simple Phase Execution

### Test (Write First)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[test]
    fn test_run_phase_executes_single_command() {
        // ARRANGE
        let mut executor = MockCommandExecutor::new();
        let state_manager = MockStateManager::new();
        let time_provider = MockTimeProvider::new();

        // Create phase with single command
        let phase = Phase {
            command: Some("npm test".to_string()),
            commands: None,
            description: None,
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: None,
        };

        // EXPECT: Command executed once with correct arguments
        executor.expect_execute()
            .with(
                eq("npm test"),
                eq(Path::new("/project")),
                eq(vec![])
            )
            .times(1)
            .returning(|_, _, _| Ok(()));

        // ACT
        let result = execute_phase(&executor, &phase, Path::new("/project"));

        // ASSERT
        assert!(result.is_ok());
        // Mock verifies expectations on drop
    }
}
```

### Implementation

```rust
fn execute_phase<E: CommandExecutor>(
    executor: &E,
    phase: &Phase,
    cwd: &Path,
) -> Result<()> {
    if let Some(cmd) = &phase.command {
        executor.execute(cmd, cwd, &[])?;
    }
    Ok(())
}
```

---

## Example 2: Multiple Commands in Sequence

### Test

```rust
#[test]
fn test_run_phase_executes_multiple_commands_in_order() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();
    let mut seq = Sequence::new();

    let phase = Phase {
        command: None,
        commands: Some(vec![
            "npm install".to_string(),
            "npm run build".to_string(),
            "npm test".to_string(),
        ]),
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Commands executed in exact order
    executor.expect_execute()
        .with(eq("npm install"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    executor.expect_execute()
        .with(eq("npm run build"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    executor.expect_execute()
        .with(eq("npm test"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    // ACT
    let result = execute_phase(&executor, &phase, Path::new("/project"));

    // ASSERT
    assert!(result.is_ok());
}
```

### Implementation

```rust
fn execute_phase<E: CommandExecutor>(
    executor: &E,
    phase: &Phase,
    cwd: &Path,
) -> Result<()> {
    // Get all commands
    let commands = if let Some(cmd) = &phase.command {
        vec![cmd.clone()]
    } else if let Some(cmds) = &phase.commands {
        cmds.clone()
    } else {
        vec![]
    };

    // Execute in order
    for cmd in commands {
        executor.execute(&cmd, cwd, &[])?;
    }

    Ok(())
}
```

---

## Example 3: Before Hooks Execute First

### Test

```rust
#[test]
fn test_before_hooks_execute_before_main_phase() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();
    let mut seq = Sequence::new();

    let hooks = Hooks {
        before_build: Some(vec!["lint".to_string(), "test".to_string()]),
        ..Default::default()
    };

    let phase = Phase {
        command: Some("npm run build".to_string()),
        commands: None,
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Hook commands run first
    executor.expect_execute()
        .with(eq("npm run lint"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    executor.expect_execute()
        .with(eq("npm test"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    // EXPECT: Main phase command runs last
    executor.expect_execute()
        .with(eq("npm run build"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    // ACT
    let result = run_phase_with_hooks(&executor, &phase, &hooks, Path::new("/project"));

    // ASSERT
    assert!(result.is_ok());
}
```

### Implementation

```rust
fn run_phase_with_hooks<E: CommandExecutor>(
    executor: &E,
    phase: &Phase,
    hooks: &Hooks,
    cwd: &Path,
) -> Result<()> {
    // Execute before hooks
    if let Some(before_hooks) = &hooks.before_build {
        for hook_cmd in before_hooks {
            let cmd = format!("npm run {}", hook_cmd);
            executor.execute(&cmd, cwd, &[])?;
        }
    }

    // Execute main phase
    if let Some(cmd) = &phase.command {
        executor.execute(cmd, cwd, &[])?;
    }

    Ok(())
}
```

---

## Example 4: State Persistence After Success

### Test

```rust
#[test]
fn test_state_saved_after_successful_phase_execution() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();
    let mut state_manager = MockStateManager::new();
    let mut time_provider = MockTimeProvider::new();

    let phase = Phase {
        command: Some("npm test".to_string()),
        commands: None,
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Command executes successfully
    executor.expect_execute()
        .times(1)
        .returning(|_, _, _| Ok(()));

    // EXPECT: Time is recorded
    time_provider.expect_current_time_ms()
        .times(1)
        .returning(|| 1000000);

    // EXPECT: State is loaded
    state_manager.expect_load()
        .times(1)
        .returning(|_| LifecycleState::default());

    // EXPECT: State is saved with correct data
    state_manager.expect_save()
        .times(1)
        .withf(|_, state| {
            state.last_phase == Some("test".to_string()) &&
            state.phase_history.len() == 1 &&
            state.phase_history[0].phase == "test" &&
            state.phase_history[0].success == true &&
            state.phase_history[0].started_ms == 1000000
        })
        .returning(|_, _| Ok(()));

    // ACT
    let result = run_phase_with_state(
        &executor,
        &state_manager,
        &time_provider,
        &phase,
        "test",
        Path::new("/test/.ggen/state.json"),
        Path::new("/test"),
    );

    // ASSERT
    assert!(result.is_ok());
}
```

### Implementation

```rust
fn run_phase_with_state<E, S, T>(
    executor: &E,
    state_manager: &S,
    time_provider: &T,
    phase: &Phase,
    phase_name: &str,
    state_path: &Path,
    cwd: &Path,
) -> Result<()>
where
    E: CommandExecutor,
    S: StateManager,
    T: TimeProvider,
{
    // Record start time
    let started = time_provider.current_time_ms();
    let timer = std::time::Instant::now();

    // Execute phase
    if let Some(cmd) = &phase.command {
        executor.execute(cmd, cwd, &[])?;
    }

    // Calculate duration
    let duration = timer.elapsed().as_millis();

    // Update state
    let mut state = state_manager.load(state_path);
    state.record_run(phase_name.to_string(), started, duration, true);
    state_manager.save(state_path, &state)?;

    Ok(())
}
```

---

## Example 5: State NOT Saved on Failure

### Test

```rust
#[test]
fn test_state_not_saved_when_command_fails() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();
    let mut state_manager = MockStateManager::new();
    let time_provider = MockTimeProvider::new();

    let phase = Phase {
        command: Some("failing-command".to_string()),
        commands: None,
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Command fails
    executor.expect_execute()
        .times(1)
        .returning(|_, _, _| Err(anyhow::anyhow!("Command failed")));

    // EXPECT: State is NEVER loaded or saved
    state_manager.expect_load()
        .times(0);

    state_manager.expect_save()
        .times(0);

    // ACT
    let result = run_phase_with_state(
        &executor,
        &state_manager,
        &time_provider,
        &phase,
        "test",
        Path::new("/test/.ggen/state.json"),
        Path::new("/test"),
    );

    // ASSERT
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Command failed"));
}
```

### Implementation

```rust
fn run_phase_with_state<E, S, T>(
    executor: &E,
    state_manager: &S,
    time_provider: &T,
    phase: &Phase,
    phase_name: &str,
    state_path: &Path,
    cwd: &Path,
) -> Result<()>
where
    E: CommandExecutor,
    S: StateManager,
    T: TimeProvider,
{
    let started = time_provider.current_time_ms();
    let timer = std::time::Instant::now();

    // Execute phase - return early on error (no state update)
    if let Some(cmd) = &phase.command {
        executor.execute(cmd, cwd, &[])?;  // Early return on error!
    }

    // Only reach here if successful
    let duration = timer.elapsed().as_millis();

    let mut state = state_manager.load(state_path);
    state.record_run(phase_name.to_string(), started, duration, true);
    state_manager.save(state_path, &state)?;

    Ok(())
}
```

---

## Example 6: Environment Variables Passed Through

### Test

```rust
#[test]
fn test_environment_variables_passed_to_executor() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();

    let env = vec![
        ("NODE_ENV".to_string(), "production".to_string()),
        ("API_KEY".to_string(), "secret".to_string()),
    ];

    let phase = Phase {
        command: Some("npm start".to_string()),
        commands: None,
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Command executed with exact environment
    executor.expect_execute()
        .with(
            eq("npm start"),
            always(),
            eq(vec![
                ("NODE_ENV".to_string(), "production".to_string()),
                ("API_KEY".to_string(), "secret".to_string()),
            ])
        )
        .times(1)
        .returning(|_, _, _| Ok(()));

    // ACT
    let result = execute_phase_with_env(&executor, &phase, Path::new("/test"), &env);

    // ASSERT
    assert!(result.is_ok());
}
```

### Implementation

```rust
fn execute_phase_with_env<E: CommandExecutor>(
    executor: &E,
    phase: &Phase,
    cwd: &Path,
    env: &[(String, String)],
) -> Result<()> {
    if let Some(cmd) = &phase.command {
        executor.execute(cmd, cwd, env)?;
    }
    Ok(())
}
```

---

## Example 7: Empty Phase Does Nothing

### Test

```rust
#[test]
fn test_empty_phase_does_not_call_executor() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();

    let phase = Phase {
        command: None,
        commands: None,
        description: Some("Empty phase".to_string()),
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Executor NEVER called
    executor.expect_execute()
        .times(0);  // or .never()

    // ACT
    let result = execute_phase(&executor, &phase, Path::new("/test"));

    // ASSERT
    assert!(result.is_ok());
}
```

### Implementation

```rust
fn execute_phase<E: CommandExecutor>(
    executor: &E,
    phase: &Phase,
    cwd: &Path,
) -> Result<()> {
    // Get commands
    let commands = if let Some(cmd) = &phase.command {
        vec![cmd.clone()]
    } else if let Some(cmds) = &phase.commands {
        cmds.clone()
    } else {
        vec![]  // Empty - executor won't be called
    };

    // Execute (nothing happens if empty)
    for cmd in commands {
        executor.execute(&cmd, cwd, &[])?;
    }

    Ok(())
}
```

---

## Example 8: Cache Key Generation

### Test

```rust
#[test]
fn test_cache_key_recorded_after_successful_execution() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();
    let mut state_manager = MockStateManager::new();
    let time_provider = MockTimeProvider::new();

    let phase = Phase {
        command: Some("npm build".to_string()),
        commands: None,
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: Some(true),
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Command executes
    executor.expect_execute()
        .times(1)
        .returning(|_, _, _| Ok(()));

    // EXPECT: State loaded
    state_manager.expect_load()
        .times(1)
        .returning(|_| LifecycleState::default());

    // EXPECT: State saved with cache key
    state_manager.expect_save()
        .times(1)
        .withf(|_, state| {
            // Verify cache key was added
            state.cache_keys.len() == 1 &&
            state.cache_keys[0].phase == "build" &&
            state.cache_keys[0].key.len() == 64  // SHA256 hex
        })
        .returning(|_, _| Ok(()));

    // ACT
    let result = run_phase_with_cache(
        &executor,
        &state_manager,
        &time_provider,
        &phase,
        "build",
        Path::new("/test/.ggen/state.json"),
        Path::new("/test"),
    );

    // ASSERT
    assert!(result.is_ok());
}
```

### Implementation

```rust
fn run_phase_with_cache<E, S, T>(
    executor: &E,
    state_manager: &S,
    time_provider: &T,
    phase: &Phase,
    phase_name: &str,
    state_path: &Path,
    cwd: &Path,
) -> Result<()>
where
    E: CommandExecutor,
    S: StateManager,
    T: TimeProvider,
{
    let started = time_provider.current_time_ms();
    let timer = std::time::Instant::now();

    // Execute phase
    if let Some(cmd) = &phase.command {
        executor.execute(cmd, cwd, &[])?;
    }

    let duration = timer.elapsed().as_millis();

    // Generate cache key
    let cache_key = if phase.cache == Some(true) {
        let cmds = vec![phase.command.clone().unwrap_or_default()];
        Some(cache::cache_key(phase_name, &cmds, &[], &[]))
    } else {
        None
    };

    // Update state
    let mut state = state_manager.load(state_path);
    state.record_run(phase_name.to_string(), started, duration, true);

    if let Some(key) = cache_key {
        state.add_cache_key(phase_name.to_string(), key);
    }

    state_manager.save(state_path, &state)?;

    Ok(())
}
```

---

## Example 9: Error Propagation

### Test

```rust
#[test]
fn test_first_command_failure_stops_subsequent_commands() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();
    let mut seq = Sequence::new();

    let phase = Phase {
        command: None,
        commands: Some(vec![
            "cmd1".to_string(),
            "cmd2".to_string(),
            "cmd3".to_string(),
        ]),
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: First command fails
    executor.expect_execute()
        .with(eq("cmd1"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Err(anyhow::anyhow!("cmd1 failed")));

    // EXPECT: Subsequent commands NEVER called
    executor.expect_execute()
        .with(eq("cmd2"), always(), always())
        .times(0);

    executor.expect_execute()
        .with(eq("cmd3"), always(), always())
        .times(0);

    // ACT
    let result = execute_phase(&executor, &phase, Path::new("/test"));

    // ASSERT
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("cmd1 failed"));
}
```

### Implementation

```rust
fn execute_phase<E: CommandExecutor>(
    executor: &E,
    phase: &Phase,
    cwd: &Path,
) -> Result<()> {
    let commands = if let Some(cmd) = &phase.command {
        vec![cmd.clone()]
    } else if let Some(cmds) = &phase.commands {
        cmds.clone()
    } else {
        vec![]
    };

    // Execute in order, stop on first error
    for cmd in commands {
        executor.execute(&cmd, cwd, &[])?;  // ? operator propagates error
    }

    Ok(())
}
```

---

## Example 10: Custom Argument Matching

### Test

```rust
#[test]
fn test_commands_matching_with_custom_predicates() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();

    let phase = Phase {
        command: Some("npm run test:unit".to_string()),
        commands: None,
        description: None,
        watch: None,
        port: None,
        outputs: None,
        cache: None,
        workspaces: None,
        parallel: None,
    };

    // EXPECT: Command matches custom predicate
    executor.expect_execute()
        .withf(|cmd, cwd, env| {
            // Custom matching logic
            cmd.starts_with("npm") &&
            cmd.contains("test") &&
            cwd.to_str().unwrap().ends_with("project") &&
            env.is_empty()
        })
        .times(1)
        .returning(|_, _, _| Ok(()));

    // ACT
    let result = execute_phase(&executor, &phase, Path::new("/test/project"));

    // ASSERT
    assert!(result.is_ok());
}
```

---

## Testing Best Practices

### 1. AAA Pattern (Arrange, Act, Assert)

```rust
#[test]
fn test_example() {
    // ARRANGE - Set up mocks and expectations
    let mut mock = MockCommandExecutor::new();
    mock.expect_execute()
        .times(1)
        .returning(|_, _, _| Ok(()));

    // ACT - Execute the code under test
    let result = execute_phase(&mock, &phase, path);

    // ASSERT - Verify the result
    assert!(result.is_ok());
    // Mock verifies expectations on drop
}
```

### 2. Clear Test Names

```rust
// ✅ Good
#[test]
fn test_before_hooks_execute_before_main_phase() { }

#[test]
fn test_command_failure_stops_execution() { }

// ❌ Bad
#[test]
fn test1() { }

#[test]
fn test_hooks() { }
```

### 3. One Concept Per Test

```rust
// ✅ Good - Tests one thing
#[test]
fn test_state_saved_after_success() { }

#[test]
fn test_state_not_saved_after_failure() { }

// ❌ Bad - Tests too many things
#[test]
fn test_state_management() {
    // Tests success AND failure AND cache
}
```

### 4. Explicit Expectations

```rust
// ✅ Good - Explicit call count
mock.expect_execute()
    .times(1)
    .returning(|_, _, _| Ok(()));

// ⚠️ Risky - Implicitly allows any number
mock.expect_execute()
    .returning(|_, _, _| Ok(()));
```

---

## Next Steps

1. ✅ Review these examples
2. 💻 Create `ggen-core/src/lifecycle/traits.rs`
3. 💻 Add `mockall = "0.13.1"` to dev-dependencies
4. 💻 Write first test (Example 1)
5. 💻 Run test (RED)
6. 💻 Implement minimal code (GREEN)
7. 💻 Refactor
8. 💻 Repeat with more tests

---

**Document:** Practical TDD Examples
**Date:** 2025-10-11
**Status:** Ready for implementation
