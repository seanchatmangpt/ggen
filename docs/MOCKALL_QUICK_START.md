# mockall Quick Start Guide for ggen Lifecycle System

## Installation

```toml
# ggen-core/Cargo.toml
[dev-dependencies]
mockall = "0.13.1"
```

## Basic Pattern

### 1. Define Trait with Automock

```rust
// ggen-core/src/lifecycle/traits.rs

use mockall::automock;

#[cfg_attr(test, automock)]
pub trait CommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()>;
}
```

### 2. Implement Real Version

```rust
pub struct RealCommandExecutor;

impl CommandExecutor for RealCommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
        // Real implementation using std::process::Command
        let mut command = if cfg!(target_os = "windows") {
            let mut c = std::process::Command::new("cmd");
            c.arg("/C");
            c
        } else {
            let mut c = std::process::Command::new("sh");
            c.arg("-lc");
            c
        };

        command.current_dir(cwd).arg(cmd);
        for (key, value) in env {
            command.env(key, value);
        }

        let status = command.status()?;
        if !status.success() {
            return Err(anyhow::anyhow!("Command failed: {}", cmd));
        }
        Ok(())
    }
}
```

### 3. Use in Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[test]
    fn test_command_execution() {
        // Create mock
        let mut mock = MockCommandExecutor::new();

        // Set expectations
        mock.expect_execute()
            .with(eq("npm test"), always(), always())
            .times(1)
            .returning(|_, _, _| Ok(()));

        // Use mock
        let result = mock.execute("npm test", Path::new("/tmp"), &[]);
        assert!(result.is_ok());
    }
}
```

## Common Patterns

### Verify Exact Call Count

```rust
mock.expect_execute()
    .times(3)  // Must be called exactly 3 times
    .returning(|_, _, _| Ok(()));
```

### Verify Never Called

```rust
mock.expect_execute()
    .times(0)  // or .never()
    .returning(|_, _, _| Ok(()));
```

### Match Arguments

```rust
// Exact match
mock.expect_execute()
    .with(eq("build"), eq(Path::new("/tmp")), always())
    .returning(|_, _, _| Ok(()));

// Custom predicate
mock.expect_execute()
    .withf(|cmd, _, _| cmd.starts_with("npm"))
    .returning(|_, _, _| Ok(()));
```

### Return Different Values

```rust
// Return specific value
mock.expect_execute()
    .returning(|_, _, _| Ok(()));

// Return error
mock.expect_execute()
    .returning(|_, _, _| Err(anyhow::anyhow!("Failed")));

// Return based on arguments
mock.expect_execute()
    .returning(|cmd, _, _| {
        if cmd == "fail" {
            Err(anyhow::anyhow!("Failed"))
        } else {
            Ok(())
        }
    });
```

### Enforce Call Order

```rust
use mockall::Sequence;

let mut seq = Sequence::new();

mock.expect_execute()
    .with(eq("install"), always(), always())
    .times(1)
    .in_sequence(&mut seq)
    .returning(|_, _, _| Ok(()));

mock.expect_execute()
    .with(eq("build"), always(), always())
    .times(1)
    .in_sequence(&mut seq)
    .returning(|_, _, _| Ok(()));

// Must call in this order: install, then build
```

### Stateful Returns

```rust
let mut call_count = 0;

mock.expect_load()
    .returning(move |_| {
        call_count += 1;
        LifecycleState {
            last_phase: Some(format!("phase_{}", call_count)),
            ..Default::default()
        }
    });

// Each call returns different state
```

## London School TDD Workflow

### 1. Write Test First

```rust
#[test]
fn test_phase_runs_before_hooks_first() {
    // ARRANGE
    let mut executor = MockCommandExecutor::new();
    let mut seq = Sequence::new();

    // Set expectations
    executor.expect_execute()
        .with(eq("lint"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    executor.expect_execute()
        .with(eq("build"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    // ACT
    let result = run_phase(&executor, "build");

    // ASSERT
    assert!(result.is_ok());
    // Mock verifies expectations on drop
}
```

### 2. Run Test (RED)

```bash
cargo test test_phase_runs_before_hooks_first
# Test fails - function doesn't exist or doesn't call hooks
```

### 3. Write Minimal Implementation (GREEN)

```rust
fn run_phase<E: CommandExecutor>(executor: &E, phase: &str) -> Result<()> {
    // Execute hooks
    executor.execute("lint", Path::new("."), &[])?;

    // Execute phase
    executor.execute("build", Path::new("."), &[])?;

    Ok(())
}
```

### 4. Refactor

```rust
fn run_phase<E: CommandExecutor>(executor: &E, phase: &str) -> Result<()> {
    // Load hooks from config
    let hooks = get_before_hooks(phase);
    for hook in hooks {
        executor.execute(&hook, Path::new("."), &[])?;
    }

    // Execute phase commands
    let commands = get_phase_commands(phase);
    for cmd in commands {
        executor.execute(&cmd, Path::new("."), &[])?;
    }

    Ok(())
}
```

## Tips & Tricks

### 1. One Mock Per Test

```rust
#[test]
fn test_success() {
    let mut mock = MockCommandExecutor::new();  // Fresh mock
    // ...
}

#[test]
fn test_failure() {
    let mut mock = MockCommandExecutor::new();  // Fresh mock
    // ...
}
```

### 2. Use Helper Functions

```rust
fn create_mock_executor() -> MockCommandExecutor {
    let mut mock = MockCommandExecutor::new();

    mock.expect_execute()
        .returning(|_, _, _| Ok(()));

    mock
}

#[test]
fn test_with_helper() {
    let executor = create_mock_executor();
    // Use executor
}
```

### 3. Test Error Cases

```rust
#[test]
fn test_command_failure_propagates() {
    let mut executor = MockCommandExecutor::new();

    executor.expect_execute()
        .times(1)
        .returning(|_, _, _| Err(anyhow::anyhow!("Command failed")));

    let result = run_phase(&executor, "build");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Command failed"));
}
```

### 4. Debug Mock Failures

When test fails with "Unexpected call to execute", check:

1. **Missing expectation** - Did you forget to set up an expected call?
2. **Wrong arguments** - Are the arguments what you expected?
3. **Call count mismatch** - Called more times than `.times(n)`?
4. **Wrong order** - Using `Sequence` but called in wrong order?

Enable debug output:
```rust
mock.expect_execute()
    .times(1)
    .returning(|cmd, cwd, env| {
        eprintln!("Called with: cmd={}, cwd={:?}, env={:?}", cmd, cwd, env);
        Ok(())
    });
```

## Integration with ggen Lifecycle

### Key Traits to Mock

```rust
// Command execution
#[cfg_attr(test, automock)]
pub trait CommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()>;
}

// State management
#[cfg_attr(test, automock)]
pub trait StateManager {
    fn load(&self, path: &Path) -> LifecycleState;
    fn save(&self, path: &Path, state: &LifecycleState) -> Result<()>;
}

// Time provider (for deterministic tests)
#[cfg_attr(test, automock)]
pub trait TimeProvider {
    fn current_time_ms(&self) -> u128;
}
```

### Test Structure

```rust
#[cfg(test)]
mod london_school_tests {
    use super::*;
    use mockall::predicate::*;
    use mockall::Sequence;

    fn create_test_context<'a>(
        make: &'a Make,
        executor: MockCommandExecutor,
        state_manager: MockStateManager,
        time_provider: MockTimeProvider,
    ) -> Context<'a, MockCommandExecutor, MockStateManager, MockTimeProvider> {
        Context {
            root: Path::new("/test"),
            make,
            state_path: Path::new("/test/.ggen/state.json"),
            env: vec![],
            executor,
            state_manager,
            time_provider,
        }
    }

    #[test]
    fn test_example() {
        let mut executor = MockCommandExecutor::new();
        let mut state_manager = MockStateManager::new();
        let mut time_provider = MockTimeProvider::new();

        // Set up expectations...

        let ctx = create_test_context(&make, executor, state_manager, time_provider);
        let result = run_phase(&ctx, "build");
        assert!(result.is_ok());
    }
}
```

## Common Errors

### Error: "No expectation matched"

```
thread 'tests::test_example' panicked at:
  MockCommandExecutor::execute: No expectation matched
```

**Fix:** Add expectation for the method call:
```rust
mock.expect_execute()
    .returning(|_, _, _| Ok(()));
```

### Error: "Expected n calls but got m"

```
thread 'tests::test_example' panicked at:
  MockCommandExecutor::execute: Expectation(<function>):
    expected to be called 1 times but actually called 0 times
```

**Fix:** Either:
1. Add the missing call in your code
2. Change `.times(1)` to `.times(0)` if it shouldn't be called
3. Use `.times(..n)` for flexible call counts

### Error: "Sequence violation"

```
thread 'tests::test_example' panicked at:
  MockCommandExecutor::execute: Sequence violation
```

**Fix:** Calls must be in the order specified in `Sequence`:
```rust
let mut seq = Sequence::new();

mock.expect_first()
    .in_sequence(&mut seq);

mock.expect_second()
    .in_sequence(&mut seq);

// Must call: first(), then second()
```

## Cheat Sheet

| Operation | Code |
|-----------|------|
| Expect call | `mock.expect_method()` |
| Exact call count | `.times(n)` |
| Never called | `.times(0)` or `.never()` |
| At least n | `.times(n..)` |
| At most n | `.times(..=n)` |
| Range | `.times(min..=max)` |
| Match argument | `.with(eq(value))` |
| Match any | `.with(always())` |
| Custom match | `.withf(\|arg\| predicate)` |
| Return value | `.returning(\|\| value)` |
| Return once | `.return_once(\|\| value)` |
| Return error | `.returning(\|\| Err(...))` |
| Enforce order | `.in_sequence(&mut seq)` |

## Resources

- Full documentation: https://docs.rs/mockall/latest/mockall/
- Examples: https://github.com/asomers/mockall/tree/master/mockall/examples
- User guide: https://docs.rs/mockall/latest/mockall/#user-guide

## Next Steps

1. ✅ Read this guide
2. ✅ See RUST_MOCKING_LIBRARIES_RESEARCH.md for detailed analysis
3. ✅ Create traits.rs with testable abstractions
4. ✅ Refactor exec.rs for dependency injection
5. ✅ Write first London School TDD test
6. ✅ Run test (RED)
7. ✅ Implement minimal code (GREEN)
8. ✅ Refactor and repeat
