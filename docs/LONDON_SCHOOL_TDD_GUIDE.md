# London School TDD Guide for ggen Lifecycle System

## Table of Contents

1. [Introduction to London School TDD](#introduction)
2. [Core Principles](#core-principles)
3. [Mock Infrastructure](#mock-infrastructure)
4. [Outside-In Acceptance Tests](#outside-in-acceptance-tests)
5. [Interaction-Based Unit Tests](#interaction-based-unit-tests)
6. [TDD Workflow: Red-Green-Refactor](#tdd-workflow)
7. [Best Practices](#best-practices)
8. [Common Patterns](#common-patterns)
9. [Troubleshooting](#troubleshooting)

---

## Introduction

The London School of TDD (also called "mockist" TDD) emphasizes:

- **Outside-In Development**: Start with high-level tests and work inward
- **Interaction Testing**: Verify how objects collaborate, not their internal state
- **Mock-Driven Design**: Use mocks to discover and define interfaces
- **Behavior Verification**: Focus on the conversation between objects

This guide applies London School TDD to the ggen lifecycle system using the complete mock infrastructure in `/tests/lifecycle_tests/`.

---

## Core Principles

### 1. Test the Conversation, Not the Implementation

**❌ Classical TDD (State-Based)**:
```rust
#[test]
fn test_phase_execution() {
    let executor = PhaseExecutor::new();
    let phase = Phase { name: "build", command: "make" };

    executor.run_phase(&phase);

    // Check internal state
    assert_eq!(executor.executed_phases.len(), 1);
    assert_eq!(executor.executed_phases[0], "build");
}
```

**✅ London School (Interaction-Based)**:
```rust
#[test]
fn test_phase_execution() {
    let mock_executor = MockCommandExecutor::new()
        .with_success("make");
    let mock_state = MockStateRepository::new();
    let mock_observer = MockObserver::new();

    let phase_executor = PhaseExecutor::new(
        mock_executor.clone(),
        mock_state.clone(),
        mock_observer.clone(),
    );

    phase_executor.run_phase(&phase);

    // Verify interactions
    assert!(mock_executor.verify_called("make"));
    assert!(mock_state.verify_save_called());
    assert!(mock_observer.verify_phase_completed_successfully("build"));
}
```

**Key Difference**: We verify that PhaseExecutor **told** its collaborators to do the right things, not that it changed its own internal state.

### 2. Outside-In Design Flow

```
User Story/Acceptance Test
         ↓
    Define Public API
         ↓
    Mock Collaborators
         ↓
  Write Failing Test
         ↓
   Minimal Implementation
         ↓
    Refactor Design
         ↓
   Unit Tests for New Components (repeat)
```

### 3. Mock Everything Except the System Under Test

When testing `PhaseExecutor`:
- ✅ Mock: `CommandExecutor`, `StateRepository`, `HookRegistry`, `Observer`
- ❌ Don't Mock: `PhaseExecutor` itself

### 4. Design Emerges from Tests

The interfaces of your mocks **are** the interfaces of your real components. If a mock is hard to use, the real interface will be hard to use.

---

## Mock Infrastructure

### Available Mocks

Located in `/tests/lifecycle_tests/mocks.rs`:

1. **`MockCommandExecutor`** - Shell command execution
2. **`MockStateRepository`** - State persistence
3. **`MockHookRegistry`** - Hook lifecycle management
4. **`MockObserver`** - Progress notifications

### Mock Features

All mocks support:
- **Call Recording**: Track all method calls
- **Call Verification**: Assert methods were called correctly
- **Order Verification**: Assert calls happened in specific order
- **Response Configuration**: Set up success/failure scenarios
- **Reset**: Clean state between tests

### Example: Setting Up Mocks

```rust
use lifecycle_tests::mocks::*;

let mocks = MockSetupBuilder::new()
    .with_command_success("npm run build")
    .with_command_failure("npm test", "Tests failed")
    .with_hook_failure("lint", "Linting errors")
    .build();

// Now use mocks in your test
let executor = PhaseExecutor::new(
    mocks.executor.clone(),
    mocks.state_repo.clone(),
    mocks.hook_registry.clone(),
    mocks.observer.clone(),
);
```

---

## Outside-In Acceptance Tests

### What Are Acceptance Tests?

Acceptance tests represent complete user stories, testing the system from the public API through all collaborators.

### Example: Complete Phase Execution

**User Story**: "As a developer, I want to run a phase that executes commands, runs hooks, and updates state"

```rust
#[test]
fn acceptance_run_phase_executes_full_workflow() {
    // ARRANGE: Set up the world
    let mocks = MockSetupBuilder::new()
        .with_command_success("npm run build")
        .with_command_success("npm run test")  // before_build hook
        .build();

    let phase_config = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["npm run build".to_string()],
        before_hooks: vec!["test".to_string()],
        after_hooks: vec![],
    };

    // ACT: Execute the phase
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase_config);

    // ASSERT: Verify the conversation between objects
    assert!(result.is_ok());

    // Verify observer lifecycle
    assert_phase_lifecycle(&mocks.observer, "build");

    // Verify hooks executed
    mocks.hook_registry.verify_hook_executed("test", HookStage::Before);

    // Verify command order
    assert_commands_in_order(&mocks.executor, &["npm run test", "npm run build"]);

    // Verify state updated
    assert_state_saved(&mocks.state_repo);
    assert!(mocks.state_repo.verify_phase_recorded("build"));
}
```

### Key Characteristics of Acceptance Tests

1. **Test complete workflows** - Multiple collaborators involved
2. **Use public API only** - No testing of internals
3. **Mock all external dependencies** - Commands, file I/O, etc.
4. **Verify interactions** - Not just return values

### Common Acceptance Test Scenarios

```rust
// 1. Happy path - everything succeeds
#[test]
fn acceptance_successful_pipeline_execution() { }

// 2. Error handling - command fails
#[test]
fn acceptance_command_failure_stops_execution() { }

// 3. Hook execution order
#[test]
fn acceptance_hooks_execute_in_correct_order() { }

// 4. State persistence
#[test]
fn acceptance_state_tracks_execution_history() { }

// 5. Recursion prevention
#[test]
fn acceptance_prevents_hook_recursion() { }
```

---

## Interaction-Based Unit Tests

### What Are Unit Tests in London School?

Unit tests verify that a component correctly **collaborates** with its direct dependencies, using mocks for all collaborators.

### Example: Testing Command Delegation

```rust
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
    assert!(mocks.executor.verify_called("echo hello"));
    assert_eq!(mocks.executor.call_count("echo hello"), 1);
}
```

### Testing Execution Order

```rust
#[test]
fn should_execute_commands_in_sequence() {
    let mocks = MockSetupBuilder::new()
        .with_command_success("command1")
        .with_command_success("command2")
        .with_command_success("command3")
        .build();

    let phase = PhaseConfig {
        name: "test".to_string(),
        commands: vec!["command1".to_string(), "command2".to_string(), "command3".to_string()],
        before_hooks: vec![],
        after_hooks: vec![],
    };

    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    executor.run_phase(&phase).unwrap();

    // Verify order
    assert!(mocks.executor.verify_call_order(&["command1", "command2", "command3"]));
}
```

### Testing Error Propagation

```rust
#[test]
fn should_propagate_command_failure() {
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

    let result = executor.run_phase(&phase);

    // Verify error handling
    assert!(result.is_err());

    // Verify observer was notified
    let events = mocks.observer.get_events();
    assert!(events.iter().any(|e| matches!(e, ObserverEvent::Error { .. })));

    // Verify state was NOT corrupted
    assert!(!mocks.state_repo.verify_phase_recorded("build"));
}
```

---

## TDD Workflow: Red-Green-Refactor

### The London School Red-Green-Refactor Cycle

#### Step 1: RED - Write a Failing Test with Mocks

Start by writing a test that defines the **conversation** you want to happen.

```rust
#[test]
fn should_execute_hooks_through_registry() {
    // Define expected behavior with mocks
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
        mocks.hook_registry.clone(),  // Not implemented yet!
        mocks.observer.clone(),
    );

    executor.run_phase(&phase).unwrap();

    // Define expected interactions
    assert!(mocks.hook_registry.verify_hook_executed("test", HookStage::Before));
    assert!(mocks.hook_registry.verify_hook_executed("lint", HookStage::Before));
    assert!(mocks.hook_registry.verify_hook_executed("cleanup", HookStage::After));
}
```

**This test will fail** because `PhaseExecutor` doesn't call the hook registry yet.

#### Step 2: GREEN - Minimal Implementation

Write just enough code to make the test pass.

```rust
impl PhaseExecutor {
    pub fn run_phase(&self, phase: &PhaseConfig) -> Result<(), String> {
        // Execute before hooks
        self.hook_registry.execute_before_hooks(&phase.name, &phase.before_hooks)?;

        // Execute commands
        for cmd in &phase.commands {
            self.command_executor.execute(cmd, &PathBuf::from("."), &[])?;
        }

        // Execute after hooks
        self.hook_registry.execute_after_hooks(&phase.name, &phase.after_hooks)?;

        Ok(())
    }
}
```

**Test should now pass!**

#### Step 3: REFACTOR - Improve Design

Now that the test is passing, improve the design while keeping tests green.

```rust
impl PhaseExecutor {
    pub fn run_phase(&self, phase: &PhaseConfig) -> Result<(), String> {
        self.notify_phase_start(&phase.name);

        self.execute_before_hooks(phase)?;
        self.execute_commands(phase)?;
        self.execute_after_hooks(phase)?;
        self.update_state(phase)?;

        self.notify_phase_complete(&phase.name);

        Ok(())
    }

    fn execute_before_hooks(&self, phase: &PhaseConfig) -> Result<(), String> {
        self.hook_registry.execute_before_hooks(&phase.name, &phase.before_hooks)
    }

    fn execute_commands(&self, phase: &PhaseConfig) -> Result<(), String> {
        for cmd in &phase.commands {
            self.command_executor.execute(cmd, &PathBuf::from("."), &[])?;
        }
        Ok(())
    }

    // ... other methods
}
```

**Tests still pass!** The refactoring improved code organization without breaking behavior.

### Complete TDD Example: Adding Hook Recursion Prevention

#### RED: Define Expected Behavior

```rust
#[test]
fn should_prevent_hook_recursion() {
    let mocks = MockSetupBuilder::new()
        .with_command_success("echo build")
        .build();

    let phase = PhaseConfig {
        name: "build".to_string(),
        commands: vec!["echo build".to_string()],
        before_hooks: vec!["build".to_string()],  // Recursive!
        after_hooks: vec![],
    };

    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase);

    // Should detect and prevent recursion
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("recursion"));
}
```

**Test fails** - No recursion detection yet.

#### GREEN: Implement Recursion Guard

```rust
// In mock_hook_registry.rs
pub struct MockHookRegistry {
    executed_hooks: Arc<Mutex<Vec<HookExecution>>>,
    recursion_guard: Arc<Mutex<Vec<String>>>,  // Track executing phases
}

impl MockHookRegistry {
    pub fn execute_before_hooks(&self, phase: &str, hooks: &[String]) -> Result<(), String> {
        // Check for recursion
        let mut guard = self.recursion_guard.lock().unwrap();
        if guard.contains(&phase.to_string()) {
            return Err(format!("Hook recursion detected: phase '{}' already executing", phase));
        }
        guard.push(phase.to_string());
        drop(guard);

        // Execute hooks...
        for hook in hooks {
            self.executed_hooks.lock().unwrap().push(HookExecution {
                hook_name: hook.clone(),
                phase_name: phase.to_string(),
                stage: HookStage::Before,
            });
        }

        Ok(())
    }

    pub fn execute_after_hooks(&self, phase: &str, hooks: &[String]) -> Result<(), String> {
        // Execute hooks...

        // Release recursion guard
        let mut guard = self.recursion_guard.lock().unwrap();
        guard.retain(|p| p != phase);

        Ok(())
    }
}
```

**Test passes!**

#### REFACTOR: Improve Recursion Guard

```rust
// Create dedicated guard type
pub struct RecursionGuard {
    executing_phases: RefCell<HashSet<String>>,
}

impl RecursionGuard {
    pub fn new() -> Self {
        Self {
            executing_phases: RefCell::new(HashSet::new()),
        }
    }

    pub fn enter(&self, phase: &str) -> Result<GuardToken, String> {
        let mut executing = self.executing_phases.borrow_mut();
        if executing.contains(phase) {
            return Err(format!("Recursion detected: {}", phase));
        }
        executing.insert(phase.to_string());
        Ok(GuardToken {
            guard: self,
            phase: phase.to_string(),
        })
    }
}

pub struct GuardToken<'a> {
    guard: &'a RecursionGuard,
    phase: String,
}

impl Drop for GuardToken<'_> {
    fn drop(&mut self) {
        self.guard.executing_phases.borrow_mut().remove(&self.phase);
    }
}
```

**Tests still pass!** Better design with RAII pattern.

---

## Best Practices

### 1. When to Use Mocks vs Real Implementations

**✅ Always Mock**:
- External services (databases, APIs, file systems)
- Slow operations (network calls, compilation)
- Non-deterministic behavior (random, time)
- Side effects (emails, notifications)

**✅ Use Real Implementations**:
- Value objects (data structures, DTOs)
- Pure functions (calculations, transformations)
- In-memory data structures

**Example**:
```rust
// ✅ Mock external command execution
let mock_executor = MockCommandExecutor::new();

// ✅ Use real PhaseConfig (value object)
let phase = PhaseConfig {
    name: "build".to_string(),
    commands: vec!["make".to_string()],
};
```

### 2. Mock Verification Patterns

#### Pattern 1: Verify Method Called

```rust
assert!(mock.verify_called("command"));
```

#### Pattern 2: Verify Call Count

```rust
assert_eq!(mock.call_count("command"), 1);
```

#### Pattern 3: Verify Call Order

```rust
assert!(mock.verify_call_order(&["cmd1", "cmd2", "cmd3"]));
```

#### Pattern 4: Verify Method Called With Arguments

```rust
assert!(mock.verify_called_with_env("npm build", "NODE_ENV", "production"));
```

#### Pattern 5: Verify No Unexpected Calls

```rust
assert!(mock.verify_no_calls());
```

### 3. Test Organization

```
tests/lifecycle_tests/
├── mod.rs                    # Module declarations
├── mocks.rs                  # Mock implementations
├── acceptance_tests.rs       # High-level user story tests
├── unit_tests.rs            # Isolated component tests
└── test_helpers.rs          # Builders and utilities
```

### 4. Naming Conventions

```rust
// Acceptance tests: User story perspective
#[test]
fn acceptance_user_can_run_pipeline_with_hooks() { }

// Unit tests: Component behavior
#[test]
fn should_delegate_command_execution() { }
fn should_execute_hooks_in_order() { }
fn should_propagate_errors() { }
```

### 5. Test Isolation

**Always reset mocks between tests**:

```rust
#[test]
fn test_1() {
    let mocks = setup_mocks();
    // ... test logic ...
    mocks.reset_all();
}

// Or use helper
TestEnvironment::run_test(|mocks| {
    // Test runs in isolated environment
    // Automatic cleanup
});
```

---

## Common Patterns

### Pattern 1: Testing Error Paths

```rust
#[test]
fn should_handle_command_failure_gracefully() {
    let mocks = MockSetupBuilder::new()
        .with_command_failure("npm build", "Build failed")
        .build();

    let result = executor.run_phase(&phase);

    assert!(result.is_err());
    assert!(mocks.observer.verify_error_reported("build"));
    assert!(!mocks.state_repo.verify_phase_recorded("build"));
}
```

### Pattern 2: Testing State Transitions

```rust
#[test]
fn should_update_state_after_successful_execution() {
    let initial_state = empty_state();
    let mocks = MockSetupBuilder::new()
        .with_initial_state(initial_state)
        .with_command_success("build")
        .build();

    executor.run_phase(&phase).unwrap();

    let final_state = mocks.state_repo.get_state();
    assert_eq!(final_state.last_phase, Some("build".to_string()));
    assert_eq!(final_state.phase_history.len(), 1);
}
```

### Pattern 3: Testing Collaborator Coordination

```rust
#[test]
fn should_coordinate_all_collaborators() {
    let mocks = setup_mocks();

    executor.run_phase(&phase).unwrap();

    // Verify coordination
    verify_complete_lifecycle(&mocks, "build", &["npm run build"]);
}
```

### Pattern 4: Testing Async Behavior (Future Enhancement)

```rust
#[tokio::test]
async fn should_execute_parallel_commands() {
    let mocks = setup_mocks();

    executor.run_phase_parallel(&phase).await.unwrap();

    // All commands should be called (order may vary)
    assert!(mocks.executor.verify_called("cmd1"));
    assert!(mocks.executor.verify_called("cmd2"));
}
```

---

## Troubleshooting

### Problem: Mock Not Recording Calls

**Symptom**: `verify_called()` returns false even though method was called.

**Solution**: Ensure you're using the same mock instance:

```rust
// ❌ Wrong - different instances
let executor = PhaseExecutor::new(MockCommandExecutor::new(), ...);
let mock = MockCommandExecutor::new();  // Different instance!
assert!(mock.verify_called("cmd"));  // Will fail

// ✅ Correct - same instance
let mock = MockCommandExecutor::new();
let executor = PhaseExecutor::new(mock.clone(), ...);
assert!(mock.verify_called("cmd"));  // Works
```

### Problem: Test Passes in Isolation, Fails in Suite

**Symptom**: Test works when run alone but fails when run with other tests.

**Solution**: Reset mocks between tests or use `TestEnvironment`:

```rust
TestEnvironment::run_test(|mocks| {
    // Automatic isolation and cleanup
});
```

### Problem: Mock Order Verification Fails

**Symptom**: `verify_call_order()` fails even though calls look correct.

**Solution**: Check for typos and ensure exact command strings:

```rust
// ❌ Will fail - whitespace difference
mock.with_command_success("npm  run build");  // Double space
assert!(mock.verify_call_order(&["npm run build"]));  // Single space

// ✅ Use constants
const BUILD_CMD: &str = "npm run build";
mock.with_command_success(BUILD_CMD);
assert!(mock.verify_call_order(&[BUILD_CMD]));
```

### Problem: Hard to Test Complex Interactions

**Symptom**: Test becomes too complex with many mock setups.

**Solution**: Break into smaller tests or use test helpers:

```rust
// Instead of one large test
#[test]
fn test_everything() { /* 100 lines */ }

// Multiple focused tests
#[test]
fn should_execute_commands() { }

#[test]
fn should_execute_hooks() { }

#[test]
fn should_update_state() { }
```

---

## Summary

### London School TDD Key Takeaways

1. **Test interactions, not state** - Verify what objects tell each other
2. **Mock all collaborators** - Test components in complete isolation
3. **Outside-in design** - Start with acceptance tests, work inward
4. **Let tests drive design** - Mock interfaces become real interfaces
5. **Verify behavior** - Assert on method calls, not internal state

### Workflow Summary

```
1. Write acceptance test (defines user story)
2. Mock all collaborators
3. Write failing test (RED)
4. Minimal implementation (GREEN)
5. Refactor design (REFACTOR)
6. Repeat for next component
```

### Next Steps

1. Run existing tests: `cargo test --test lifecycle_tests`
2. Add new acceptance tests for your user stories
3. Drive new component designs through mocking
4. Refactor existing code using TDD approach
5. Achieve >80% test coverage through interaction testing

### Resources

- Test files: `/tests/lifecycle_tests/`
- Mocks: `/tests/lifecycle_tests/mocks.rs`
- Examples: `/tests/lifecycle_tests/acceptance_tests.rs`
- Helpers: `/tests/lifecycle_tests/test_helpers.rs`

---

**Remember**: In London School TDD, tests are not just verification - they're a design tool. Let the tests guide you to better interfaces and clearer responsibilities.
