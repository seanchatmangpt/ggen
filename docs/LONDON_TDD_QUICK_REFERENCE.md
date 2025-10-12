# London School TDD Quick Reference

One-page cheat sheet for the ggen lifecycle London School TDD implementation.

---

## 📁 Files at a Glance

```
/Users/sac/ggen/
├── tests/lifecycle_tests/
│   ├── mocks.rs                 647 lines | Mock infrastructure
│   ├── acceptance_tests.rs      462 lines | Outside-in tests
│   ├── unit_tests.rs            583 lines | Interaction tests
│   ├── test_helpers.rs          449 lines | Utilities & builders
│   ├── mod.rs                     5 lines | Module exports
│   └── README.md               300+ lines | Test documentation
│
└── docs/
    ├── LONDON_SCHOOL_TDD_GUIDE.md              855 lines | Complete guide
    ├── LONDON_TDD_WORKFLOW_EXAMPLES.md         766 lines | Step-by-step
    ├── LONDON_TDD_IMPLEMENTATION_SUMMARY.md    600+ lines | Overview
    └── LONDON_TDD_QUICK_REFERENCE.md             (this file)

TOTAL: 2,146 lines of test code + 2,210+ lines of documentation
```

---

## 🚀 Quick Start

### Run Tests
```bash
# All tests
cargo test --test lifecycle_tests

# Single test
cargo test --test lifecycle_tests acceptance_run_phase

# With output
cargo test --test lifecycle_tests -- --nocapture
```

### Write a Test
```rust
use lifecycle_tests::mocks::*;
use lifecycle_tests::test_helpers::*;

#[test]
fn my_test() {
    // 1. Setup
    let mocks = MockSetupBuilder::new()
        .with_command_success("build")
        .build();

    // 2. Execute
    let phase = simple_phase("build", "build");
    let executor = create_executor(&mocks);
    executor.run_phase(&phase).unwrap();

    // 3. Verify
    assert!(mocks.executor.verify_called("build"));
}
```

---

## 🎯 Core Principles

### 1. Test Interactions, Not State
```rust
// ❌ State-based
assert_eq!(executor.phases_run, 1);

// ✅ Interaction-based
assert!(mock.verify_called("command"));
```

### 2. Mock All Collaborators
```rust
PhaseExecutor::new(
    mock_executor,      // ✅ Mock
    mock_state,         // ✅ Mock
    mock_hooks,         // ✅ Mock
    mock_observer,      // ✅ Mock
)
```

### 3. Outside-In Development
```
User Story → Acceptance Test → Unit Tests → Implementation
```

### 4. Red-Green-Refactor
```
1. RED:     Write failing test
2. GREEN:   Minimal implementation
3. REFACTOR: Improve design
```

---

## 🔧 Mock Reference

### MockCommandExecutor
```rust
let mock = MockCommandExecutor::new()
    .with_success("cmd")
    .with_failure("bad", "error");

// Verify
assert!(mock.verify_called("cmd"));
assert_eq!(mock.call_count("cmd"), 1);
assert!(mock.verify_call_order(&["cmd1", "cmd2"]));
```

### MockStateRepository
```rust
let mock = MockStateRepository::new()
    .with_initial_state(state);

// Verify
assert!(mock.verify_save_called());
assert!(mock.verify_phase_recorded("build"));
assert!(mock.verify_cache_key_stored("build"));
```

### MockHookRegistry
```rust
let mock = MockHookRegistry::new()
    .with_hook_failure("lint", "error");

// Verify
assert!(mock.verify_hook_executed("test", HookStage::Before));
assert!(mock.verify_execution_order(&[...]));
```

### MockObserver
```rust
let mock = MockObserver::new();

// Verify
assert!(mock.verify_phase_started("build"));
assert!(mock.verify_phase_completed_successfully("build"));
assert!(mock.verify_event_order("build"));
```

---

## 🧪 Test Patterns

### Acceptance Test
```rust
#[test]
fn acceptance_user_story() {
    // Setup world
    let mocks = MockSetupBuilder::new()...;
    let phase = phase_with_hooks(...);
    let executor = create_executor(&mocks);

    // User action
    executor.run_phase(&phase).unwrap();

    // Verify complete workflow
    verify_complete_lifecycle(&mocks, "phase", &["cmds"]);
}
```

### Unit Test
```rust
#[test]
fn should_delegate_to_collaborator() {
    // Setup
    let mock = Mock::new();
    let sut = Component::new(mock.clone());

    // Act
    sut.method();

    // Verify interaction
    assert!(mock.verify_called("method"));
}
```

### Error Test
```rust
#[test]
fn should_handle_failure() {
    let mocks = mocks_with_failing_command("cmd", "error");
    let result = executor.run_phase(&phase);

    assert!(result.is_err());
    assert_failed_execution(&mocks, "phase");
}
```

---

## 🛠️ Helper Utilities

### Builders
```rust
// Phase
PhaseConfigBuilder::new("build")
    .with_command("npm build")
    .with_before_hook("test")
    .build()

// Mocks
MockSetupBuilder::new()
    .with_command_success("cmd")
    .with_hook_failure("hook", "error")
    .build()

// Expectations
ExpectationBuilder::new()
    .expect_command_success("cmd")
    .setup_mocks()
```

### Factories
```rust
simple_phase("name", "command")
phase_with_hooks("name", "cmd", before, after)
multi_command_phase("name", vec!["cmd1", "cmd2"])
```

### Assertions
```rust
assert_successful_execution(&mocks, "phase", &["cmds"])
assert_failed_execution(&mocks, "phase")
verify_complete_lifecycle(&mocks, "phase", &["cmds"])
verify_hook_order(&hooks, &[("hook", Stage)])
```

---

## 📋 Checklist

### Writing New Test

- [ ] Name describes behavior (`should_...`, `acceptance_...`)
- [ ] Mocks all external dependencies
- [ ] Tests single collaboration
- [ ] Verifies interactions, not state
- [ ] Uses helper utilities
- [ ] Follows Arrange-Act-Assert
- [ ] Cleans up (reset mocks)

### Reviewing Test

- [ ] Clear and readable
- [ ] Focused on one thing
- [ ] No implementation details
- [ ] Proper error handling
- [ ] Good failure messages
- [ ] Follows project patterns

---

## 🐛 Common Issues

### Mock Not Recording
```rust
// ❌ Different instances
let mock = Mock::new();
let sut = Component::new(Mock::new());
assert!(mock.verify_called("x")); // Fails!

// ✅ Same instance
let mock = Mock::new();
let sut = Component::new(mock.clone());
assert!(mock.verify_called("x")); // Works!
```

### Test Interference
```rust
// ✅ Use TestEnvironment
TestEnvironment::run_test(|mocks| {
    // Automatic isolation
});
```

### Order Verification Fails
```rust
// ✅ Use constants
const CMD: &str = "npm build";
mock.with_success(CMD);
assert!(mock.verify_call_order(&[CMD]));
```

---

## 📖 Documentation Map

| Document | Purpose | When to Read |
|----------|---------|--------------|
| **Quick Reference** (this) | Cheat sheet | Daily use |
| **Test Suite README** | Test organization | Getting started |
| **TDD Guide** | Complete principles | Learning phase |
| **Workflow Examples** | Step-by-step | Implementing features |
| **Implementation Summary** | Overview | Understanding scope |

---

## 🎓 Key Concepts

| Concept | Description | Example |
|---------|-------------|---------|
| **SUT** | System Under Test | PhaseExecutor |
| **Collaborator** | Dependency | CommandExecutor |
| **Mock** | Test double | MockCommandExecutor |
| **Verification** | Assert interaction | verify_called() |
| **Acceptance Test** | User story | acceptance_run_phase |
| **Unit Test** | Component test | should_delegate |

---

## ⚡ TDD Workflow

```
┌─────────────────────────────────────────────────┐
│  1. RED: Write Failing Test                    │
│     - Define expected behavior                  │
│     - Mock collaborators                        │
│     - Watch test fail                          │
└─────────────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│  2. GREEN: Minimal Implementation               │
│     - Write just enough code                   │
│     - Make test pass                           │
│     - Don't optimize yet                       │
└─────────────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│  3. REFACTOR: Improve Design                    │
│     - Extract patterns                         │
│     - Improve names                            │
│     - Keep tests green                         │
└─────────────────────────────────────────────────┘
                     ↓
                  REPEAT
```

---

## 💡 Best Practices

### DO ✅
- Mock all external dependencies
- Verify interactions
- Test one thing at a time
- Use descriptive names
- Reset between tests
- Leverage helpers

### DON'T ❌
- Mock the SUT
- Test implementation
- Create complex setups
- Share mocks
- Verify unused methods
- Mix test types

---

## 📊 Test Statistics

| Metric | Value |
|--------|-------|
| Mock Types | 4 |
| Acceptance Tests | 7 |
| Unit Tests | 20+ |
| Test Helpers | 10+ |
| Test Code Lines | 2,146 |
| Documentation Lines | 2,210+ |
| Total Lines | 4,356+ |

---

## 🔗 Quick Links

```bash
# View mocks
cat tests/lifecycle_tests/mocks.rs

# View acceptance tests
cat tests/lifecycle_tests/acceptance_tests.rs

# View helpers
cat tests/lifecycle_tests/test_helpers.rs

# Read guide
cat docs/LONDON_SCHOOL_TDD_GUIDE.md

# See examples
cat docs/LONDON_TDD_WORKFLOW_EXAMPLES.md
```

---

## 🎯 Remember

> "In London School TDD, mocks aren't just test utilities - they're design tools that help discover better interfaces and clearer responsibilities."

**The Three Laws of TDD:**
1. Write no production code except to pass a failing test
2. Write only enough of a test to demonstrate a failure
3. Write only enough production code to pass the test

**London School Addition:**
- Use mocks to define interfaces
- Test object conversations
- Let tests drive design

---

**Quick Reference Version**: 1.0
**Last Updated**: October 2025
**Status**: ✅ Complete Implementation
