# London School TDD for ggen Lifecycle System

> **Methodology**: London School (Mockist, Outside-In) Test-Driven Development
> **Status**: Implementation Guide
> **Prerequisites**: Basic TDD knowledge, familiarity with lifecycle system

---

## Executive Summary

This document provides a complete guide to applying **London School TDD** (mockist, outside-in) to the ggen lifecycle system. London School TDD emphasizes:

1. **Outside-In Development** - Start with acceptance tests, work inward
2. **Interaction Testing** - Verify object collaborations, not state
3. **Mock-Driven Design** - Discover interfaces through testing needs
4. **Behavior Focus** - Test what objects do, not what they contain

**Key Benefits**:
- 🎯 Better design through collaboration focus
- 🧪 Fast, isolated tests (no I/O)
- 🔍 Clear interface discovery
- 🛡️ Strong refactoring safety net

---

## Table of Contents

1. [London School vs Classical TDD](#1-london-school-vs-classical-tdd)
2. [Mock Infrastructure with mockall](#2-mock-infrastructure-with-mockall)
3. [Outside-In Test Development](#3-outside-in-test-development)
4. [Interaction-Based Unit Tests](#4-interaction-based-unit-tests)
5. [Red-Green-Refactor Workflow](#5-red-green-refactor-workflow)
6. [Refactoring for Testability](#6-refactoring-for-testability)
7. [Best Practices](#7-best-practices)

---

## 1. London School vs Classical TDD

### Classical (Detroit) School

```rust
// Classical: Test state changes
#[test]
fn test_phase_updates_state() {
    let mut state = LifecycleState::default();

    // Exercise
    state.record_run("build".into(), 1000, 500, true);

    // Verify state
    assert_eq!(state.last_phase, Some("build".into()));
    assert_eq!(state.phase_history.len(), 1);
}
```

**Characteristics**:
- Tests verify final state
- Uses real objects when possible
- Integration-heavy
- Slower but comprehensive

### London (Mockist) School

```rust
// London: Test interactions
#[test]
fn test_phase_executor_records_run() {
    let mut mock_repo = MockStateRepository::new();

    // Expectation: Should call save with correct data
    mock_repo.expect_save()
        .with(predicate::function(|state: &LifecycleState| {
            state.last_phase == Some("build".into())
        }))
        .times(1)
        .returning(|_| Ok(()));

    let executor = PhaseExecutor::new(mock_repo);
    executor.run_phase("build").unwrap();

    // Verification happens automatically via mock
}
```

**Characteristics**:
- Tests verify collaborations
- Mocks all dependencies
- Fast, isolated unit tests
- Discovers interfaces naturally

---

## 2. Mock Infrastructure with mockall

### Installation

```toml
# Cargo.toml
[dev-dependencies]
mockall = "0.13"
```

### Core Traits to Mock

#### CommandExecutor Trait

```rust
// lifecycle/traits.rs - NEW
use mockall::automock;

#[automock]
pub trait CommandExecutor: Send + Sync {
    fn execute(&self, spec: &CommandSpec) -> Result<CommandOutput>;
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
```

**Usage in Tests**:
```rust
#[test]
fn test_executor_runs_command() {
    let mut mock = MockCommandExecutor::new();

    mock.expect_execute()
        .withf(|spec| spec.command == "cargo build")
        .times(1)
        .returning(|_| Ok(CommandOutput {
            exit_code: 0,
            stdout: "Compiling...".into(),
            stderr: "".into(),
            duration: Duration::from_secs(2),
        }));

    let result = mock.execute(&CommandSpec {
        command: "cargo build".into(),
        working_dir: PathBuf::from("."),
        env: HashMap::new(),
    });

    assert!(result.is_ok());
}
```

#### StateRepository Trait

```rust
#[automock]
pub trait StateRepository: Send + Sync {
    fn load(&self) -> Result<LifecycleState>;
    fn save(&self, state: &LifecycleState) -> Result<()>;
}
```

**Usage in Tests**:
```rust
#[test]
fn test_phase_executor_saves_state() {
    let mut mock_repo = MockStateRepository::new();

    // Expect load first
    mock_repo.expect_load()
        .times(1)
        .returning(|| Ok(LifecycleState::default()));

    // Then expect save with updated state
    mock_repo.expect_save()
        .times(1)
        .returning(|_| Ok(()));

    let executor = PhaseExecutor::new(
        Box::new(MockCommandExecutor::new()),
        Box::new(mock_repo),
    );

    executor.run_phase("build").unwrap();
}
```

#### HookRegistry Trait

```rust
#[automock]
pub trait HookRegistry: Send + Sync {
    fn execute_before(&self, phase: &str) -> Result<()>;
    fn execute_after(&self, phase: &str) -> Result<()>;
}
```

#### Observer Trait

```rust
#[automock]
pub trait LifecycleObserver: Send + Sync {
    fn on_phase_start(&self, phase: &str);
    fn on_phase_complete(&self, phase: &str, duration: Duration);
    fn on_error(&self, phase: &str, error: &Error);
}
```

---

## 3. Outside-In Test Development

### Step 1: Write Acceptance Test First

**Scenario**: User runs `ggen lifecycle run build`

```rust
// tests/acceptance/lifecycle_acceptance_tests.rs
use mockall::predicate::*;

#[test]
fn acceptance_run_phase_with_hooks() {
    // GIVEN: A lifecycle with mocked dependencies
    let mut mock_executor = MockCommandExecutor::new();
    let mut mock_repo = MockStateRepository::new();
    let mut mock_hooks = MockHookRegistry::new();
    let mut mock_observer = MockObserver::new();

    // EXPECT: Observer notified of start
    mock_observer.expect_on_phase_start()
        .with(eq("build"))
        .times(1)
        .returning(|_| ());

    // EXPECT: Before hooks executed
    mock_hooks.expect_execute_before()
        .with(eq("build"))
        .times(1)
        .returning(|_| Ok(()));

    // EXPECT: Command executed
    mock_executor.expect_execute()
        .withf(|spec| spec.command == "cargo build")
        .times(1)
        .returning(|_| Ok(CommandOutput::success()));

    // EXPECT: State saved
    mock_repo.expect_save()
        .times(1)
        .returning(|_| Ok(()));

    // EXPECT: After hooks executed
    mock_hooks.expect_execute_after()
        .with(eq("build"))
        .times(1)
        .returning(|_| Ok(()));

    // EXPECT: Observer notified of completion
    mock_observer.expect_on_phase_complete()
        .with(eq("build"), always())
        .times(1)
        .returning(|_, _| ());

    // WHEN: User runs phase
    let lifecycle = Lifecycle::builder()
        .executor(Box::new(mock_executor))
        .state_repo(Box::new(mock_repo))
        .hook_registry(Box::new(mock_hooks))
        .observer(Box::new(mock_observer))
        .build();

    let result = lifecycle.run("build");

    // THEN: Phase completes successfully
    assert!(result.is_ok());
}
```

This test drives the design of:
- `Lifecycle::builder()` API
- Trait interfaces for all collaborators
- Expected execution flow

### Step 2: Implement Minimal Code to Pass

```rust
// lifecycle/facade.rs
pub struct Lifecycle {
    executor: Box<dyn CommandExecutor>,
    state_repo: Box<dyn StateRepository>,
    hook_registry: Box<dyn HookRegistry>,
    observer: Box<dyn LifecycleObserver>,
}

impl Lifecycle {
    pub fn builder() -> LifecycleBuilder {
        LifecycleBuilder::new()
    }

    pub fn run(&self, phase: &str) -> Result<()> {
        // Implementation driven by test expectations
        self.observer.on_phase_start(phase);
        self.hook_registry.execute_before(phase)?;

        let spec = self.build_command_spec(phase)?;
        let output = self.executor.execute(&spec)?;

        let state = self.state_repo.load()?;
        // Update state...
        self.state_repo.save(&state)?;

        self.hook_registry.execute_after(phase)?;
        self.observer.on_phase_complete(phase, output.duration);

        Ok(())
    }
}
```

### Step 3: Write Next Level of Tests

Now test each collaborator in isolation with its own mocks.

---

## 4. Interaction-Based Unit Tests

### Testing PhaseExecutor

```rust
// tests/unit/phase_executor_tests.rs

#[test]
fn executes_command_with_correct_spec() {
    let mut mock = MockCommandExecutor::new();

    // Verify exact command specification
    mock.expect_execute()
        .withf(|spec| {
            spec.command == "npm test" &&
            spec.working_dir == PathBuf::from("/project") &&
            spec.env.get("NODE_ENV") == Some(&"test".to_string())
        })
        .times(1)
        .returning(|_| Ok(CommandOutput::success()));

    let executor = PhaseExecutor::new(Box::new(mock));
    let result = executor.execute_phase("test");

    assert!(result.is_ok());
}
```

### Testing Hook Execution Order

```rust
use mockall::Sequence;

#[test]
fn executes_hooks_in_correct_order() {
    let mut seq = Sequence::new();
    let mut mock = MockHookRegistry::new();

    // Verify order: before_all -> before_build -> after_build -> after_all
    mock.expect_execute_global_before()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|| Ok(()));

    mock.expect_execute_before()
        .with(eq("build"))
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_| Ok(()));

    mock.expect_execute_after()
        .with(eq("build"))
        .times(1)
        .in_sequence(&mut seq)
        .returning(|_| Ok(()));

    mock.expect_execute_global_after()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|| Ok(()));

    let registry = mock;
    registry.execute_hooks_for_phase("build").unwrap();
}
```

### Testing Error Propagation

```rust
#[test]
fn propagates_command_failure() {
    let mut mock = MockCommandExecutor::new();

    mock.expect_execute()
        .times(1)
        .returning(|_| Err(Error::CommandFailed {
            command: "npm test".into(),
            exit_code: 1,
        }));

    let executor = PhaseExecutor::new(Box::new(mock));
    let result = executor.execute_phase("test");

    assert!(matches!(result, Err(Error::CommandFailed { .. })));
}
```

### Testing State Updates

```rust
#[test]
fn updates_state_with_execution_record() {
    let mut mock_repo = MockStateRepository::new();

    // Expect state loaded
    mock_repo.expect_load()
        .times(1)
        .returning(|| Ok(LifecycleState::default()));

    // Expect state saved with new record
    mock_repo.expect_save()
        .withf(|state| {
            state.last_phase == Some("build".into()) &&
            state.phase_history.len() == 1
        })
        .times(1)
        .returning(|_| Ok(()));

    let manager = StateManager::new(Box::new(mock_repo));
    manager.record_phase_execution("build", Duration::from_secs(5)).unwrap();
}
```

---

## 5. Red-Green-Refactor Workflow

### Example: Adding Hook Recursion Prevention

#### RED: Write Failing Test

```rust
#[test]
#[should_panic(expected = "Hook recursion detected")]
fn prevents_hook_recursion() {
    let mut mock = MockHookRegistry::new();

    // Simulate recursive hook call
    mock.expect_execute_before()
        .with(eq("build"))
        .times(1)
        .returning(|phase| {
            // This would cause recursion in real code
            Err(Error::HookRecursion { phase: phase.into() })
        });

    let registry = HookRegistry::new_with_mock(mock);
    registry.execute_before("build").unwrap();
}
```

Test **FAILS** - recursion prevention not implemented.

#### GREEN: Minimal Implementation

```rust
// lifecycle/hooks.rs
use std::cell::RefCell;
use std::collections::HashSet;

pub struct HookRegistry {
    executing: RefCell<HashSet<String>>,
    inner: Box<dyn HookRegistryTrait>,
}

impl HookRegistry {
    pub fn execute_before(&self, phase: &str) -> Result<()> {
        // Guard against recursion
        let mut executing = self.executing.borrow_mut();
        if executing.contains(phase) {
            return Err(Error::HookRecursion { phase: phase.into() });
        }
        executing.insert(phase.into());

        let result = self.inner.execute_before(phase);

        self.executing.borrow_mut().remove(phase);
        result
    }
}
```

Test **PASSES** - minimal implementation added.

#### REFACTOR: Improve Design

```rust
pub struct HookGuard<'a> {
    registry: &'a HookRegistry,
    phase: String,
}

impl Drop for HookGuard<'_> {
    fn drop(&mut self) {
        self.registry.executing.borrow_mut().remove(&self.phase);
    }
}

impl HookRegistry {
    fn enter_guard(&self, phase: &str) -> Result<HookGuard> {
        let mut executing = self.executing.borrow_mut();
        if executing.contains(phase) {
            return Err(Error::HookRecursion { phase: phase.into() });
        }
        executing.insert(phase.into());

        Ok(HookGuard {
            registry: self,
            phase: phase.into(),
        })
    }

    pub fn execute_before(&self, phase: &str) -> Result<()> {
        let _guard = self.enter_guard(phase)?;
        self.inner.execute_before(phase)
    }
}
```

Tests still **PASS** - design improved with RAII guard.

---

## 6. Refactoring for Testability

### Current Code (Hard to Test)

```rust
// exec.rs - Direct dependencies
pub fn run_phase(ctx: &Context, phase_name: &str) -> Result<()> {
    // Direct shell execution - cannot mock
    let status = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .status()?;

    // Direct filesystem - cannot mock
    let mut state = load_state(ctx.state_path);
    state.record_run(phase_name.into(), started, duration, true);
    save_state(ctx.state_path, &state)?;

    Ok(())
}
```

### Refactored (Testable)

```rust
// lifecycle/executor.rs - Dependency injection
pub struct PhaseExecutor {
    cmd_executor: Box<dyn CommandExecutor>,
    state_repo: Box<dyn StateRepository>,
    hook_registry: Box<dyn HookRegistry>,
}

impl PhaseExecutor {
    pub fn new(
        cmd_executor: Box<dyn CommandExecutor>,
        state_repo: Box<dyn StateRepository>,
        hook_registry: Box<dyn HookRegistry>,
    ) -> Self {
        Self { cmd_executor, state_repo, hook_registry }
    }

    pub fn run_phase(&self, phase_name: &str) -> Result<()> {
        // All dependencies injected - fully mockable
        self.hook_registry.execute_before(phase_name)?;

        let spec = self.build_spec(phase_name)?;
        self.cmd_executor.execute(&spec)?;

        let mut state = self.state_repo.load()?;
        state.record_run(phase_name.into(), started, duration, true);
        self.state_repo.save(&state)?;

        self.hook_registry.execute_after(phase_name)?;
        Ok(())
    }
}
```

### Test with All Mocks

```rust
#[test]
fn test_refactored_executor() {
    let mut mock_cmd = MockCommandExecutor::new();
    let mut mock_state = MockStateRepository::new();
    let mut mock_hooks = MockHookRegistry::new();

    // Setup all expectations...

    let executor = PhaseExecutor::new(
        Box::new(mock_cmd),
        Box::new(mock_state),
        Box::new(mock_hooks),
    );

    executor.run_phase("build").unwrap();
    // All interactions verified by mocks
}
```

---

## 7. Best Practices

### When to Use Mocks

✅ **Use mocks for**:
- External dependencies (filesystem, network, shell)
- Cross-boundary collaborators
- Expensive operations (database, compilation)
- Non-deterministic behavior (time, random)

❌ **Don't mock**:
- Value objects (simple data structures)
- Internal data structures
- Pure functions without side effects

### Mock Verification Patterns

```rust
// Verify call count
mock.expect_method()
    .times(1)  // Exactly once
    .times(2..5)  // Between 2 and 5
    .times(mockall::predicate::ge(1))  // At least once

// Verify arguments
mock.expect_method()
    .with(eq("exact_value"))
    .withf(|arg| arg.len() > 5)
    .with(predicate::function(|x: &str| x.starts_with("prefix")))

// Verify order
let mut seq = Sequence::new();
mock.expect_method1().in_sequence(&mut seq);
mock.expect_method2().in_sequence(&mut seq);

// Return values
mock.expect_method()
    .returning(|| Ok(42))
    .returning(|arg| Ok(arg * 2))
```

### Test Organization

```
tests/
├── acceptance/
│   └── lifecycle_acceptance_tests.rs  # Outside-in, high-level
├── unit/
│   ├── phase_executor_tests.rs        # Interaction tests
│   ├── hook_registry_tests.rs
│   └── state_manager_tests.rs
├── helpers/
│   ├── mod.rs
│   ├── builders.rs                    # Test data builders
│   └── mocks.rs                       # Mock presets
└── fixtures/
    └── sample_make.toml
```

### Common Pitfalls

❌ **Over-mocking**:
```rust
// Bad: Mocking simple value object
let mut mock_phase = MockPhase::new();
mock_phase.expect_get_commands()...
```

✅ **Use real value objects**:
```rust
// Good: Use real Phase instance
let phase = Phase {
    command: Some("npm test".into()),
    ..Default::default()
};
```

❌ **Testing implementation details**:
```rust
// Bad: Testing private method calls
mock.expect_internal_helper().times(1);
```

✅ **Test public behavior**:
```rust
// Good: Test observable behavior
mock.expect_execute().times(1);
assert!(result.is_ok());
```

### Builder Pattern for Test Setup

```rust
// tests/helpers/builders.rs
pub struct PhaseExecutorBuilder {
    cmd_executor: Option<Box<dyn CommandExecutor>>,
    state_repo: Option<Box<dyn StateRepository>>,
    hook_registry: Option<Box<dyn HookRegistry>>,
}

impl PhaseExecutorBuilder {
    pub fn new() -> Self {
        Self {
            cmd_executor: None,
            state_repo: None,
            hook_registry: None,
        }
    }

    pub fn with_cmd_executor(mut self, exec: Box<dyn CommandExecutor>) -> Self {
        self.cmd_executor = Some(exec);
        self
    }

    pub fn with_successful_command(mut self) -> Self {
        let mut mock = MockCommandExecutor::new();
        mock.expect_execute()
            .returning(|_| Ok(CommandOutput::success()));
        self.cmd_executor = Some(Box::new(mock));
        self
    }

    pub fn build(self) -> PhaseExecutor {
        PhaseExecutor::new(
            self.cmd_executor.unwrap_or_else(|| Box::new(MockCommandExecutor::new())),
            self.state_repo.unwrap_or_else(|| Box::new(MockStateRepository::new())),
            self.hook_registry.unwrap_or_else(|| Box::new(MockHookRegistry::new())),
        )
    }
}

// Usage in tests:
#[test]
fn test_with_builder() {
    let executor = PhaseExecutorBuilder::new()
        .with_successful_command()
        .with_empty_state()
        .build();

    assert!(executor.run_phase("build").is_ok());
}
```

---

## Quick Reference

### mockall Cheat Sheet

```rust
use mockall::prelude::*;

// Define mockable trait
#[automock]
trait MyTrait {
    fn method(&self, arg: &str) -> Result<i32>;
}

// Create mock
let mut mock = MockMyTrait::new();

// Set expectations
mock.expect_method()
    .with(eq("test"))           // Exact match
    .times(1)                    // Called once
    .returning(|_| Ok(42));     // Return value

// Use mock
assert_eq!(mock.method("test"), Ok(42));

// Verify (automatic on drop)
```

### Test Template

```rust
#[test]
fn test_feature_does_something() {
    // GIVEN: Setup mocks with expectations
    let mut mock = MockDependency::new();
    mock.expect_method()
        .with(eq("input"))
        .times(1)
        .returning(|_| Ok(()));

    // WHEN: Execute the code under test
    let sut = SystemUnderTest::new(Box::new(mock));
    let result = sut.do_something("input");

    // THEN: Verify behavior
    assert!(result.is_ok());
    // Mock verifies interactions on drop
}
```

---

## Summary

London School TDD transforms how we design and test the lifecycle system:

**Before** (Classical):
- Slow tests with real filesystem
- Hard to isolate failures
- Unclear interface boundaries
- Integration-heavy testing

**After** (London):
- ✅ Fast, isolated unit tests (<1ms)
- ✅ Clear interface contracts
- ✅ Easy to verify collaborations
- ✅ Design emerges from tests

**Next Steps**:
1. Add mockall to dependencies
2. Extract trait interfaces
3. Refactor Context → PhaseExecutor with DI
4. Write acceptance tests first
5. Implement with TDD workflow

**Success Metrics**:
- All tests run in <100ms total
- Zero filesystem/shell dependencies in tests
- 100% interaction coverage for collaborations
- Design driven by test needs

---

**Document Version**: 1.0
**Last Updated**: 2025-01-11
**Methodology**: London School TDD
**Tool**: mockall v0.13

**Next**: See `/docs/LIFECYCLE_BEST_PRACTICES.md` for implementation patterns
