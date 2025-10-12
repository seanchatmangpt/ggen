# Rust Mocking Libraries Research for London School TDD

**Date:** 2025-10-11
**Project:** ggen lifecycle system
**Context:** Evaluating mocking libraries for London School TDD (outside-in, interaction-based testing)

## Executive Summary

After comprehensive analysis of the ggen-core lifecycle system and evaluation of available Rust mocking libraries, **mockall** is the recommended choice for implementing London School TDD patterns. It provides the strongest support for interaction verification, expectation setting, and trait-based mocking required for the lifecycle system's architecture.

## Current State Analysis

### Existing Test Infrastructure

**Location:** `/Users/sac/ggen/ggen-core/src/lifecycle/`

**Current Testing Approach:**
- Integration tests only (`integration_test.rs` - 607 lines)
- No unit tests for individual components
- Manual mocks (e.g., `MockAgent`, `MockMcpServer`)
- Classical TDD style (state verification)
- No interaction-based testing
- No command execution mocking

**Key Traits Requiring Mocking:**
1. **Command Executor** - For `execute_command()` in `exec.rs`
2. **State Manager** - For `load_state()` and `save_state()` operations
3. **File System Operations** - For cache, state file I/O
4. **Time Provider** - For `current_time_ms()` determinism

**Dependencies:** (from `ggen-core/Cargo.toml`)
```toml
[dev-dependencies]
tempfile = "3"
serde_json = "1"
# No mocking libraries currently
```

## Library Comparison Matrix

| Feature | mockall | mockito | faux | double |
|---------|---------|---------|------|--------|
| **Trait Mocking** | ✅ Excellent | ❌ HTTP only | ❌ No | ✅ Limited |
| **Struct Mocking** | ✅ Yes | ❌ HTTP only | ✅ Primary focus | ❌ No |
| **Call Verification** | ✅ Full support | ⚠️ HTTP only | ✅ Yes | ⚠️ Basic |
| **Return Values** | ✅ Flexible | ✅ HTTP bodies | ✅ Yes | ✅ Yes |
| **Argument Matchers** | ✅ Rich (eq, any, custom) | ✅ HTTP matchers | ⚠️ Limited | ❌ No |
| **Expectation Ordering** | ✅ Sequences | ❌ N/A | ⚠️ Limited | ❌ No |
| **Call Counting** | ✅ times(n) | ✅ HTTP calls | ✅ Yes | ⚠️ Basic |
| **Proc Macros** | ✅ #[automock] | ❌ N/A | ✅ #[create] | ❌ N/A |
| **Async Support** | ✅ Full | ✅ Full | ✅ Yes | ⚠️ Limited |
| **Error Messages** | ✅ Excellent | ⚠️ HTTP focused | ⚠️ Basic | ⚠️ Basic |
| **Maintenance** | ✅ Active (v0.13.1) | ⚠️ Legacy (v1.7.0) | ⚠️ Inactive (v0.1.13) | ⚠️ Archived |
| **Documentation** | ✅ Comprehensive | ✅ Good | ⚠️ Limited | ⚠️ Minimal |
| **Learning Curve** | ⚠️ Moderate | ✅ Easy | ⚠️ Moderate | ⚠️ Steep |
| **London TDD Fit** | ✅ Excellent | ❌ Wrong domain | ⚠️ Partial | ⚠️ Limited |

## Detailed Library Analysis

### 1. mockall (✅ RECOMMENDED)

**Version:** 0.13.1 (Active development)
**GitHub:** https://github.com/asomers/mockall
**Downloads:** ~15M total, ~500K/month
**License:** MIT/Apache-2.0

#### Strengths for London School TDD

1. **Full Interaction Verification:**
   ```rust
   mock.expect_execute()
       .times(1)
       .with(eq("build"))
       .return_once(|| Ok(()));
   ```

2. **Trait Mocking with Automock:**
   ```rust
   #[automock]
   trait CommandExecutor {
       fn execute(&self, cmd: &str, cwd: &Path) -> Result<()>;
   }
   ```

3. **Rich Expectation DSL:**
   - `times(n)` - exact call count
   - `times(min..max)` - range
   - `never()` - must not be called
   - `with()` - argument matching
   - `withf()` - custom predicates
   - `returning()` - return values
   - `returning_st()` - stateful returns

4. **Argument Matchers:**
   - `eq(value)` - equality
   - `ne(value)` - not equal
   - `gt(value)` - greater than
   - `always()` - any value
   - Custom functions

5. **Sequence Control:**
   ```rust
   let mut seq = Sequence::new();
   mock1.expect_foo().times(1).in_sequence(&mut seq);
   mock2.expect_bar().times(1).in_sequence(&mut seq);
   ```

#### Limitations

- Learning curve for complex scenarios
- Proc macros can increase compile times
- Some edge cases with generic traits
- Mock objects are not `Clone` by default

#### Best Practices

```rust
use mockall::predicate::*;
use mockall::{automock, Sequence};

#[automock]
trait CommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_phase_execution_calls_commands_in_order() {
        let mut executor = MockCommandExecutor::new();
        let mut seq = Sequence::new();

        // Expect commands in specific order
        executor.expect_execute()
            .with(eq("npm install"), always(), always())
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_, _, _| Ok(()));

        executor.expect_execute()
            .with(eq("npm test"), always(), always())
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_, _, _| Ok(()));

        // Exercise
        let phase = Phase::new("test");
        phase.run(&executor).unwrap();

        // Verify (automatic on drop)
    }
}
```

### 2. mockito (❌ NOT APPLICABLE)

**Version:** 1.7.0
**Purpose:** HTTP server mocking
**Verdict:** Wrong domain - designed for HTTP integration tests, not unit testing

**Why Not:**
- HTTP-specific (mock servers, request matching)
- No support for arbitrary trait/function mocking
- Doesn't address command execution or state management
- Use case: Testing REST clients, not lifecycle logic

### 3. faux (⚠️ PARTIAL FIT)

**Version:** 0.1.13 (Low activity since 2021)
**GitHub:** https://github.com/nrxus/faux
**Focus:** Struct mocking (not traits)

#### Strengths

1. **Struct Mocking:**
   ```rust
   #[create]
   impl MyStruct {
       fn method(&self) -> i32 { 42 }
   }

   #[test]
   fn test() {
       let mut mock = MyStruct::faux();
       faux::when!(mock.method).then_return(100);
   }
   ```

2. **Simple API**
3. **Good for concrete implementations**

#### Weaknesses for ggen

1. **No trait support** - lifecycle system heavily trait-based
2. **Limited interaction verification** - basic call counting only
3. **No expectation ordering** - can't verify command sequences
4. **Inactive development** - last update 2021
5. **Smaller ecosystem** - fewer examples and community support

**Verdict:** Not suitable for trait-heavy, interaction-focused TDD

### 4. double (❌ NOT RECOMMENDED)

**Version:** 0.2.4 (Archived project)
**Status:** No longer maintained
**Last Update:** 2018

**Why Not:**
- Project archived and unmaintained
- Minimal documentation
- No trait mocking support
- Limited feature set
- Security concerns with unmaintained dependencies

## Lifecycle System Integration Plan

### Phase 1: Trait Extraction

**Create testable traits in:** `ggen-core/src/lifecycle/traits.rs`

```rust
use std::path::Path;
use anyhow::Result;

/// Command execution abstraction for testing
#[cfg_attr(test, mockall::automock)]
pub trait CommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()>;
}

/// Real implementation using std::process::Command
pub struct RealCommandExecutor;

impl CommandExecutor for RealCommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
        // Existing execute_command logic from exec.rs
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

        let status = command.status()
            .map_err(|e| anyhow::anyhow!("Failed to execute: {}", e))?;

        if !status.success() {
            return Err(anyhow::anyhow!("Command failed: {}", cmd));
        }

        Ok(())
    }
}

/// State persistence abstraction
#[cfg_attr(test, mockall::automock)]
pub trait StateManager {
    fn load(&self, path: &Path) -> LifecycleState;
    fn save(&self, path: &Path, state: &LifecycleState) -> Result<()>;
}

/// Time provider for deterministic testing
#[cfg_attr(test, mockall::automock)]
pub trait TimeProvider {
    fn current_time_ms(&self) -> u128;
}

/// Real time implementation
pub struct SystemTime;

impl TimeProvider for SystemTime {
    fn current_time_ms(&self) -> u128 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis()
    }
}
```

### Phase 2: Refactor exec.rs for Dependency Injection

```rust
// ggen-core/src/lifecycle/exec.rs

use super::traits::{CommandExecutor, StateManager, TimeProvider};

/// Execution context with injectable dependencies
pub struct Context<'a, E, S, T>
where
    E: CommandExecutor,
    S: StateManager,
    T: TimeProvider,
{
    pub root: &'a Path,
    pub make: &'a Make,
    pub state_path: &'a Path,
    pub env: Vec<(String, String)>,
    pub executor: E,
    pub state_manager: S,
    pub time_provider: T,
}

/// Production context with real implementations
impl<'a> Context<'a, RealCommandExecutor, RealStateManager, SystemTime> {
    pub fn new(root: &'a Path, make: &'a Make, state_path: &'a Path) -> Self {
        Self {
            root,
            make,
            state_path,
            env: vec![],
            executor: RealCommandExecutor,
            state_manager: RealStateManager,
            time_provider: SystemTime,
        }
    }
}

/// Run phase with dependency injection
pub fn run_phase<E, S, T>(ctx: &Context<E, S, T>, phase_name: &str) -> Result<()>
where
    E: CommandExecutor,
    S: StateManager,
    T: TimeProvider,
{
    let phase = ctx.make.lifecycle.get(phase_name)
        .ok_or_else(|| anyhow::anyhow!("Phase '{}' not found", phase_name))?;

    let cmds = get_phase_commands(phase);
    if cmds.is_empty() {
        println!("⚠️  Phase '{}' has no commands", phase_name);
        return Ok(());
    }

    run_before_hooks(ctx, phase_name)?;

    let key = cache_key(phase_name, &cmds, &ctx.env, &[]);
    let started = ctx.time_provider.current_time_ms();
    let timer = Instant::now();

    println!("▶️  Running phase: {}", phase_name);

    // Use injected executor
    for cmd in &cmds {
        ctx.executor.execute(cmd, ctx.root, &ctx.env)?;
    }

    let duration = timer.elapsed().as_millis();
    println!("✅ Phase '{}' completed in {}ms", phase_name, duration);

    // Use injected state manager
    let mut state = ctx.state_manager.load(ctx.state_path);
    state.record_run(phase_name.to_string(), started, duration, true);
    state.add_cache_key(phase_name.to_string(), key);
    ctx.state_manager.save(ctx.state_path, &state)?;

    run_after_hooks(ctx, phase_name)?;

    Ok(())
}
```

### Phase 3: Example London School TDD Tests

**File:** `ggen-core/src/lifecycle/exec_test.rs`

```rust
#[cfg(test)]
mod london_school_tests {
    use super::*;
    use mockall::predicate::*;
    use mockall::Sequence;
    use crate::lifecycle::traits::{MockCommandExecutor, MockStateManager, MockTimeProvider};
    use std::path::PathBuf;

    /// Helper to create test context with mocks
    fn create_test_context<'a>(
        make: &'a Make,
        executor: MockCommandExecutor,
        state_manager: MockStateManager,
        time_provider: MockTimeProvider,
    ) -> Context<'a, MockCommandExecutor, MockStateManager, MockTimeProvider> {
        let root = PathBuf::from("/test");
        let state_path = PathBuf::from("/test/.ggen/state.json");

        Context {
            root: Box::leak(Box::new(root)),
            make,
            state_path: Box::leak(Box::new(state_path)),
            env: vec![],
            executor,
            state_manager,
            time_provider,
        }
    }

    #[test]
    fn test_run_phase_executes_commands_in_correct_order() {
        // ARRANGE
        let mut executor = MockCommandExecutor::new();
        let mut state_manager = MockStateManager::new();
        let mut time_provider = MockTimeProvider::new();
        let mut seq = Sequence::new();

        // Create make.toml with multiple commands
        let make = Make {
            project: Project {
                name: "test".to_string(),
                project_type: None,
                version: None,
                description: None,
            },
            workspace: None,
            lifecycle: {
                let mut map = BTreeMap::new();
                map.insert("build".to_string(), Phase {
                    commands: Some(vec![
                        "npm install".to_string(),
                        "npm run build".to_string(),
                        "npm test".to_string(),
                    ]),
                    command: None,
                    description: None,
                    watch: None,
                    port: None,
                    outputs: None,
                    cache: None,
                    workspaces: None,
                    parallel: None,
                });
                map
            },
            hooks: None,
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

        // EXPECT: Time provider called once for timestamp
        time_provider.expect_current_time_ms()
            .times(1)
            .returning(|| 1000000);

        // EXPECT: State loaded, updated, and saved
        state_manager.expect_load()
            .times(1)
            .returning(|_| LifecycleState::default());

        state_manager.expect_save()
            .times(1)
            .withf(|_, state| {
                // Verify state was updated correctly
                state.last_phase == Some("build".to_string()) &&
                state.phase_history.len() == 1 &&
                state.phase_history[0].phase == "build"
            })
            .returning(|_, _| Ok(()));

        let ctx = create_test_context(&make, executor, state_manager, time_provider);

        // ACT
        let result = run_phase(&ctx, "build");

        // ASSERT
        assert!(result.is_ok());
        // Mock expectations verified on drop
    }

    #[test]
    fn test_run_phase_propagates_command_failure() {
        // ARRANGE
        let mut executor = MockCommandExecutor::new();
        let mut state_manager = MockStateManager::new();
        let time_provider = MockTimeProvider::new();

        let make = Make {
            project: Project {
                name: "test".to_string(),
                project_type: None,
                version: None,
                description: None,
            },
            workspace: None,
            lifecycle: {
                let mut map = BTreeMap::new();
                map.insert("build".to_string(), Phase {
                    command: Some("failing-command".to_string()),
                    commands: None,
                    description: None,
                    watch: None,
                    port: None,
                    outputs: None,
                    cache: None,
                    workspaces: None,
                    parallel: None,
                });
                map
            },
            hooks: None,
        };

        // EXPECT: Command fails
        executor.expect_execute()
            .times(1)
            .returning(|_, _, _| Err(anyhow::anyhow!("Command failed")));

        // EXPECT: State NOT saved when command fails
        state_manager.expect_load()
            .times(0);
        state_manager.expect_save()
            .times(0);

        let ctx = create_test_context(&make, executor, state_manager, time_provider);

        // ACT
        let result = run_phase(&ctx, "build");

        // ASSERT
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Command failed"));
    }

    #[test]
    fn test_run_phase_with_hooks_executes_in_correct_order() {
        // ARRANGE
        let mut executor = MockCommandExecutor::new();
        let mut state_manager = MockStateManager::new();
        let mut time_provider = MockTimeProvider::new();
        let mut seq = Sequence::new();

        let make = Make {
            project: Project {
                name: "test".to_string(),
                project_type: None,
                version: None,
                description: None,
            },
            workspace: None,
            lifecycle: {
                let mut map = BTreeMap::new();
                map.insert("lint".to_string(), Phase {
                    command: Some("npm run lint".to_string()),
                    commands: None,
                    description: None,
                    watch: None,
                    port: None,
                    outputs: None,
                    cache: None,
                    workspaces: None,
                    parallel: None,
                });
                map.insert("build".to_string(), Phase {
                    command: Some("npm run build".to_string()),
                    commands: None,
                    description: None,
                    watch: None,
                    port: None,
                    outputs: None,
                    cache: None,
                    workspaces: None,
                    parallel: None,
                });
                map
            },
            hooks: Some(Hooks {
                before_build: Some(vec!["lint".to_string()]),
                ..Default::default()
            }),
        };

        // EXPECT: Hook phase runs first, then main phase
        // Hook: lint
        executor.expect_execute()
            .with(eq("npm run lint"), always(), always())
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_, _, _| Ok(()));

        time_provider.expect_current_time_ms()
            .times(1)
            .in_sequence(&mut seq)
            .returning(|| 1000000);

        state_manager.expect_load()
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_| LifecycleState::default());

        state_manager.expect_save()
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_, _| Ok(()));

        // Main phase: build
        executor.expect_execute()
            .with(eq("npm run build"), always(), always())
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_, _, _| Ok(()));

        time_provider.expect_current_time_ms()
            .times(1)
            .in_sequence(&mut seq)
            .returning(|| 1001000);

        state_manager.expect_load()
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_| {
                let mut state = LifecycleState::default();
                state.record_run("lint".to_string(), 1000000, 50, true);
                state
            });

        state_manager.expect_save()
            .times(1)
            .in_sequence(&mut seq)
            .returning(|_, _| Ok(()));

        let ctx = create_test_context(&make, executor, state_manager, time_provider);

        // ACT
        let result = run_phase(&ctx, "build");

        // ASSERT
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_phase_never_calls_executor_for_empty_commands() {
        // ARRANGE
        let mut executor = MockCommandExecutor::new();
        let state_manager = MockStateManager::new();
        let time_provider = MockTimeProvider::new();

        let make = Make {
            project: Project {
                name: "test".to_string(),
                project_type: None,
                version: None,
                description: None,
            },
            workspace: None,
            lifecycle: {
                let mut map = BTreeMap::new();
                map.insert("empty".to_string(), Phase {
                    command: None,
                    commands: None,
                    description: Some("Empty phase".to_string()),
                    watch: None,
                    port: None,
                    outputs: None,
                    cache: None,
                    workspaces: None,
                    parallel: None,
                });
                map
            },
            hooks: None,
        };

        // EXPECT: Executor never called
        executor.expect_execute()
            .never();

        let ctx = create_test_context(&make, executor, state_manager, time_provider);

        // ACT
        let result = run_phase(&ctx, "empty");

        // ASSERT
        assert!(result.is_ok());
    }

    #[test]
    fn test_cache_key_generation_consistency() {
        // Test that cache keys are deterministic
        let cmds = vec!["echo test".to_string()];
        let env = vec![("FOO".to_string(), "bar".to_string())];
        let inputs = vec![];

        let key1 = cache::cache_key("build", &cmds, &env, &inputs);
        let key2 = cache::cache_key("build", &cmds, &env, &inputs);

        assert_eq!(key1, key2);
        assert_eq!(key1.len(), 64); // SHA256 hex
    }
}
```

### Phase 4: Project Integration

**Update `ggen-core/Cargo.toml`:**

```toml
[dependencies]
# ... existing dependencies ...

[dev-dependencies]
tempfile = "3"
serde_json = "1"
mockall = "0.13.1"  # Add for London School TDD
```

**File Structure:**

```
ggen-core/src/lifecycle/
├── mod.rs                  # Public API
├── model.rs                # Data structures
├── traits.rs               # NEW: Testable trait abstractions
├── exec.rs                 # Refactored with DI
├── exec_test.rs            # NEW: Unit tests (London School)
├── loader.rs               # Config loading
├── state.rs                # State management
├── cache.rs                # Cache operations
├── dag.rs                  # Dependency resolution
└── integration_test.rs     # Existing integration tests
```

## Usage Examples

### Basic Trait Mocking

```rust
use mockall::predicate::*;

#[test]
fn test_basic_mock() {
    let mut mock = MockCommandExecutor::new();

    mock.expect_execute()
        .with(eq("npm test"), always(), always())
        .times(1)
        .returning(|_, _, _| Ok(()));

    // Use mock
    mock.execute("npm test", Path::new("/tmp"), &[]).unwrap();
}
```

### Argument Matching

```rust
#[test]
fn test_argument_matching() {
    let mut mock = MockCommandExecutor::new();

    // Match specific command
    mock.expect_execute()
        .with(eq("build"), always(), always())
        .returning(|_, _, _| Ok(()));

    // Match any command starting with "npm"
    mock.expect_execute()
        .withf(|cmd, _, _| cmd.starts_with("npm"))
        .returning(|_, _, _| Ok(()));

    // Custom predicate
    mock.expect_execute()
        .withf(|cmd, cwd, env| {
            cmd.contains("test") &&
            cwd.to_str().unwrap().contains("/tmp") &&
            env.iter().any(|(k, _)| k == "NODE_ENV")
        })
        .returning(|_, _, _| Ok(()));
}
```

### Stateful Mocking

```rust
#[test]
fn test_stateful_behavior() {
    let mut mock = MockStateManager::new();
    let mut call_count = 0;

    mock.expect_load()
        .returning(move |_| {
            call_count += 1;
            let mut state = LifecycleState::default();
            state.record_run(
                format!("phase_{}", call_count),
                1000 * call_count as u128,
                100,
                true
            );
            state
        });

    // Each call returns different state
    let state1 = mock.load(Path::new("/tmp"));
    let state2 = mock.load(Path::new("/tmp"));

    assert_eq!(state1.phase_history.len(), 1);
    assert_eq!(state2.phase_history.len(), 1);
    assert_ne!(state1.phase_history[0].phase, state2.phase_history[0].phase);
}
```

### Sequence Verification

```rust
#[test]
fn test_execution_sequence() {
    let mut executor = MockCommandExecutor::new();
    let mut seq = Sequence::new();

    // Must execute in this exact order
    executor.expect_execute()
        .with(eq("npm install"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    executor.expect_execute()
        .with(eq("npm build"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    executor.expect_execute()
        .with(eq("npm test"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    // Execute in order
    executor.execute("npm install", Path::new("/tmp"), &[]).unwrap();
    executor.execute("npm build", Path::new("/tmp"), &[]).unwrap();
    executor.execute("npm test", Path::new("/tmp"), &[]).unwrap();

    // Wrong order would panic
}
```

## Common Patterns for Lifecycle System

### 1. Testing Phase Execution

```rust
#[test]
fn test_phase_executes_all_commands() {
    let mut executor = MockCommandExecutor::new();

    // Expect all commands from phase definition
    for cmd in ["cmd1", "cmd2", "cmd3"] {
        executor.expect_execute()
            .with(eq(cmd), always(), always())
            .times(1)
            .returning(|_, _, _| Ok(()));
    }

    // Test phase with 3 commands
    let phase = create_test_phase(vec!["cmd1", "cmd2", "cmd3"]);
    execute_phase(&executor, &phase).unwrap();
}
```

### 2. Testing Hook Execution

```rust
#[test]
fn test_before_hooks_run_first() {
    let mut executor = MockCommandExecutor::new();
    let mut seq = Sequence::new();

    // Before hooks must run first
    executor.expect_execute()
        .with(eq("lint"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    // Then main phase
    executor.expect_execute()
        .with(eq("build"), always(), always())
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_, _, _| Ok(()));

    run_phase_with_hooks(&executor, "build").unwrap();
}
```

### 3. Testing Error Handling

```rust
#[test]
fn test_command_failure_stops_execution() {
    let mut executor = MockCommandExecutor::new();

    // First command fails
    executor.expect_execute()
        .with(eq("cmd1"), always(), always())
        .times(1)
        .returning(|_, _, _| Err(anyhow::anyhow!("Failed")));

    // Second command should never be called
    executor.expect_execute()
        .with(eq("cmd2"), always(), always())
        .times(0);

    let result = execute_phase(&executor, &phase);
    assert!(result.is_err());
}
```

### 4. Testing State Updates

```rust
#[test]
fn test_state_updated_after_successful_execution() {
    let mut state_manager = MockStateManager::new();

    state_manager.expect_load()
        .times(1)
        .returning(|_| LifecycleState::default());

    state_manager.expect_save()
        .times(1)
        .withf(|_, state| {
            // Verify state was updated correctly
            state.last_phase == Some("build".to_string()) &&
            state.phase_history.len() == 1 &&
            state.phase_history[0].success == true
        })
        .returning(|_, _| Ok(()));

    run_phase_with_state(&state_manager, "build").unwrap();
}
```

## Migration Strategy

### Step 1: Add mockall dependency
```bash
cd ggen-core
cargo add --dev mockall
```

### Step 2: Create traits.rs
- Extract trait abstractions
- Add `#[cfg_attr(test, automock)]` attributes
- Implement real versions

### Step 3: Refactor exec.rs incrementally
- Add generic parameters for dependencies
- Start with CommandExecutor
- Then StateManager
- Finally TimeProvider

### Step 4: Write unit tests
- Start with simple phases (no hooks)
- Add hook tests
- Add error handling tests
- Add sequence verification tests

### Step 5: Keep integration tests
- Integration tests validate end-to-end behavior
- Unit tests validate interactions and edge cases
- Both are valuable!

## Best Practices for ggen Project

1. **Use `#[cfg_attr(test, automock)]`** - Only generate mocks in test builds
2. **One mock per test** - Create fresh mocks for each test to avoid state leakage
3. **Explicit expectations** - Always set `.times()` for clarity
4. **Sequence when order matters** - Use `Sequence` for hooks and multi-command phases
5. **Test failure paths** - Mock errors to test error handling
6. **Keep integration tests** - Don't replace all integration tests with unit tests
7. **Document mock setup** - Complex mocks need comments explaining expectations

## Learning Resources

### mockall Documentation
- Official docs: https://docs.rs/mockall/latest/mockall/
- User guide: https://docs.rs/mockall/latest/mockall/#user-guide
- Examples: https://github.com/asomers/mockall/tree/master/mockall/examples

### London School TDD
- "Growing Object-Oriented Software, Guided by Tests" by Freeman & Pryce
- Focus on behavior, not state
- Mock collaborators, test interactions
- Design emerges from tests

## Conclusion

**Recommendation: Use mockall**

**Rationale:**
1. ✅ Best fit for London School TDD (interaction verification)
2. ✅ Strong trait support (lifecycle system is trait-heavy)
3. ✅ Rich expectation DSL (times, with, sequences)
4. ✅ Active maintenance and community
5. ✅ Excellent documentation and examples
6. ✅ Integrates well with standard Rust testing

**Next Steps:**
1. Add mockall to dev-dependencies
2. Create traits.rs with testable abstractions
3. Refactor exec.rs for dependency injection
4. Write unit tests for phase execution
5. Write unit tests for hook execution
6. Write unit tests for error handling
7. Keep existing integration tests for end-to-end validation

**Metrics for Success:**
- 80%+ test coverage for exec.rs
- Fast unit tests (<100ms each)
- Clear test names describing behavior
- Minimal test maintenance burden
- Easy to add new phase tests

---

**Research conducted by:** Research Agent
**Review status:** Ready for architect and coder agents
**Implementation priority:** High (foundation for TDD workflow)
