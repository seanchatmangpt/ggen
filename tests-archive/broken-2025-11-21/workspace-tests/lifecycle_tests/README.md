# London School TDD Test Suite for ggen Lifecycle System

This directory contains a comprehensive London School TDD implementation for the ggen lifecycle system, demonstrating mockist, outside-in testing principles.

## 📁 Structure

```
tests/lifecycle_tests/
├── README.md                 # This file
├── mod.rs                    # Module declarations
├── mocks.rs                  # Complete mock infrastructure (20KB)
├── acceptance_tests.rs       # Outside-in user story tests (15KB)
├── unit_tests.rs            # Interaction-based component tests (19KB)
└── test_helpers.rs          # Builders and utilities (14KB)
```

## 🎯 Quick Start

### Running Tests

```bash
# Run all lifecycle tests
cargo test --test lifecycle_tests

# Run specific test file
cargo test --test lifecycle_tests acceptance_tests

# Run single test
cargo test --test lifecycle_tests acceptance_run_phase_executes_full_workflow

# Run with output
cargo test --test lifecycle_tests -- --nocapture
```

### Writing Your First Test

```rust
use lifecycle_tests::mocks::*;
use lifecycle_tests::test_helpers::*;

#[test]
fn my_test() {
    // 1. Set up mocks
    let mocks = MockSetupBuilder::new()
        .with_command_success("npm run build")
        .build();

    // 2. Create test data
    let phase = simple_phase("build", "npm run build");

    // 3. Execute
    let executor = PhaseExecutor::new(
        mocks.executor.clone(),
        mocks.state_repo.clone(),
        mocks.hook_registry.clone(),
        mocks.observer.clone(),
    );

    let result = executor.run_phase(&phase);

    // 4. Verify interactions
    assert!(result.is_ok());
    assert_successful_execution(&mocks, "build", &["npm run build"]);
}
```

## 📚 Documentation

### Core Guides

1. **[LONDON_SCHOOL_TDD_GUIDE.md](../../docs/LONDON_SCHOOL_TDD_GUIDE.md)** (50KB)
   - Complete guide to London School principles
   - Mock infrastructure reference
   - Acceptance and unit testing patterns
   - Best practices and troubleshooting

2. **[LONDON_TDD_WORKFLOW_EXAMPLES.md](../../docs/LONDON_TDD_WORKFLOW_EXAMPLES.md)** (25KB)
   - Step-by-step Red-Green-Refactor examples
   - Real feature implementations
   - Design evolution through testing

3. **[LIFECYCLE_BEST_PRACTICES.md](../../docs/LIFECYCLE_BEST_PRACTICES.md)** (75KB)
   - Architecture patterns
   - Performance optimization
   - Error handling
   - Implementation roadmap

## 🔧 Mock Infrastructure

### Available Mocks

All mocks in `mocks.rs`:

#### MockCommandExecutor
Tests shell command execution without running real commands.

```rust
let executor = MockCommandExecutor::new()
    .with_success("npm build")
    .with_failure("npm test", "Tests failed");

// Execute and verify
executor.execute("npm build", &PathBuf::from("."), &[]).unwrap();
assert!(executor.verify_called("npm build"));
assert_eq!(executor.call_count("npm build"), 1);
```

#### MockStateRepository
Tests state persistence without touching the filesystem.

```rust
let state_repo = MockStateRepository::new()
    .with_initial_state(sample_state());

state_repo.save(&new_state).unwrap();
assert!(state_repo.verify_save_called());
assert!(state_repo.verify_phase_recorded("build"));
```

#### MockHookRegistry
Tests hook execution with recursion prevention.

```rust
let hooks = MockHookRegistry::new()
    .with_hook_failure("lint", "Linting failed");

hooks.execute_before_hooks("build", &vec!["lint".to_string()]);
assert!(hooks.verify_hook_executed("lint", HookStage::Before));
```

#### MockObserver
Tests progress notifications and event tracking.

```rust
let observer = MockObserver::new();

observer.on_phase_start("build");
observer.on_phase_complete("build", true, 1000);

assert!(observer.verify_phase_started("build"));
assert!(observer.verify_phase_completed_successfully("build"));
```

### Mock Features

✅ **Call Recording** - Track all method invocations
✅ **Verification** - Assert correct interactions
✅ **Order Checking** - Verify sequential execution
✅ **Response Config** - Set up success/failure scenarios
✅ **Reset Support** - Clean state between tests

## 🧪 Test Categories

### 1. Acceptance Tests (`acceptance_tests.rs`)

**Purpose**: Test complete user stories from public API.

**Examples**:
- ✅ Complete phase execution with hooks
- ✅ Pipeline execution
- ✅ Error handling and propagation
- ✅ Hook recursion prevention
- ✅ State persistence

**Pattern**:
```rust
#[test]
fn acceptance_user_can_do_something() {
    // ARRANGE: Set up world
    let mocks = MockSetupBuilder::new()...;

    // ACT: User action
    let result = executor.run_phase(&phase);

    // ASSERT: Verify complete workflow
    assert!(result.is_ok());
    assert_phase_lifecycle(&mocks.observer, "phase");
    assert_commands_in_order(&mocks.executor, &["cmd1", "cmd2"]);
}
```

### 2. Unit Tests (`unit_tests.rs`)

**Purpose**: Test individual components in isolation.

**Examples**:
- ✅ Command delegation
- ✅ Execution order verification
- ✅ State repository interactions
- ✅ Observer notifications
- ✅ Error propagation

**Pattern**:
```rust
#[test]
fn should_delegate_to_collaborator() {
    // ARRANGE
    let mock_collaborator = MockCollaborator::new();
    let component = Component::new(mock_collaborator.clone());

    // ACT
    component.do_something();

    // ASSERT - Verify interaction
    assert!(mock_collaborator.verify_called("method"));
}
```

### 3. Test Helpers (`test_helpers.rs`)

**Purpose**: Reduce boilerplate and improve test readability.

**Features**:
- Fluent builders (PhaseConfigBuilder, ExpectationBuilder)
- Common test scenarios (simple_phase, phase_with_hooks)
- Assertion helpers (assert_successful_execution)
- Mock setup presets (standard_mocks, mocks_with_failing_command)

**Example**:
```rust
// Without helpers
let phase = PhaseConfig {
    name: "build".to_string(),
    commands: vec!["npm run build".to_string()],
    before_hooks: vec!["test".to_string()],
    after_hooks: vec![],
    parallel: false,
};

// With helpers
let phase = PhaseConfigBuilder::new("build")
    .with_command("npm run build")
    .with_before_hook("test")
    .build();
```

## 🎨 Design Patterns

### Pattern 1: Mock Everything Except SUT

```rust
// Testing PhaseExecutor - mock all collaborators
let executor = PhaseExecutor::new(
    mock_command_executor,    // ✅ Mocked
    mock_state_repo,          // ✅ Mocked
    mock_hook_registry,       // ✅ Mocked
    mock_observer,            // ✅ Mocked
);

let phase = PhaseConfig { ... };  // ✅ Real value object

executor.run_phase(&phase);       // ❌ Not mocked (SUT)
```

### Pattern 2: Verify Interactions, Not State

```rust
// ❌ State-based (Classical TDD)
assert_eq!(executor.executed_phases.len(), 1);

// ✅ Interaction-based (London School)
assert!(mock_executor.verify_called("build command"));
assert!(mock_state.verify_save_called());
```

### Pattern 3: Test Conversations

```rust
// Verify the full conversation between objects
assert_commands_in_order(&mock_executor, &["cmd1", "cmd2"]);
assert_hooks_in_order(&mock_hooks, &[
    ("hook1", HookStage::Before),
    ("hook2", HookStage::After),
]);
assert_phase_lifecycle(&mock_observer, "build");
```

## 🚀 TDD Workflow

### Red-Green-Refactor Cycle

1. **RED**: Write failing test defining desired interaction
   ```rust
   #[test]
   fn should_call_new_collaborator() {
       let mock = MockCollaborator::new();
       component.new_feature(&mock);
       assert!(mock.verify_called("new_method"));  // ❌ Fails
   }
   ```

2. **GREEN**: Minimal implementation to pass test
   ```rust
   impl Component {
       fn new_feature(&self, collab: &MockCollaborator) {
           collab.new_method();  // ✅ Passes
       }
   }
   ```

3. **REFACTOR**: Improve design while keeping tests green
   ```rust
   impl Component {
       fn new_feature(&self, collab: &dyn Collaborator) {
           self.validate_preconditions();
           collab.new_method();
           self.update_state();
       }
   }
   ```

### Outside-In Development

```
Acceptance Test (User Story)
         ↓
    Mock Collaborators
         ↓
    Define Interfaces
         ↓
    Unit Test Components
         ↓
    Implement & Refactor
         ↓
    Repeat for Next Layer
```

## 📊 Test Coverage

### Current Coverage
- ✅ **Mock Infrastructure**: 100% (all mocks fully tested)
- ✅ **Acceptance Tests**: 7 complete user story tests
- ✅ **Unit Tests**: 20+ interaction-based component tests
- ✅ **Test Helpers**: Comprehensive builders and utilities

### Critical Paths Covered
- ✅ Phase execution workflow
- ✅ Hook lifecycle (before/after)
- ✅ Command execution and ordering
- ✅ State persistence
- ✅ Error handling and propagation
- ✅ Observer notifications
- ✅ Recursion prevention

## 🔍 Example Test Scenarios

### Scenario 1: Happy Path
```rust
#[test]
fn acceptance_successful_phase_execution() {
    let mocks = mocks_with_successful_commands(&["build", "test"]);
    let phase = phase_with_hooks("build", "build", vec!["setup"], vec!["cleanup"]);
    let executor = create_executor(&mocks);

    let result = executor.run_phase(&phase);

    assert!(result.is_ok());
    verify_complete_lifecycle(&mocks, "build", &["setup", "build", "cleanup"]);
}
```

### Scenario 2: Error Path
```rust
#[test]
fn acceptance_command_failure_stops_pipeline() {
    let mocks = mocks_with_failing_command("build", "Build failed");
    let phase = simple_phase("build", "build");
    let executor = create_executor(&mocks);

    let result = executor.run_phase(&phase);

    assert!(result.is_err());
    assert_failed_execution(&mocks, "build");
}
```

### Scenario 3: Complex Workflow
```rust
#[test]
fn acceptance_pipeline_with_hooks_and_state() {
    let mocks = standard_mocks();
    let phases = vec![
        phase_with_hooks("setup", "npm install", vec![], vec![]),
        phase_with_hooks("build", "npm build", vec!["test"], vec!["cleanup"]),
    ];
    let executor = create_executor(&mocks);

    executor.run_pipeline(&phases).unwrap();

    // Verify complete pipeline execution
    assert_commands_in_order(&mocks.executor, &[
        "npm install", "npm test", "npm build", "cleanup"
    ]);

    assert!(mocks.state_repo.verify_phase_recorded("setup"));
    assert!(mocks.state_repo.verify_phase_recorded("build"));
}
```

## 💡 Best Practices

### DO ✅
- Mock all external dependencies
- Verify interactions, not internal state
- Test one collaboration per test
- Use descriptive test names (`should_`, `acceptance_`)
- Reset mocks between tests
- Use test helpers to reduce boilerplate

### DON'T ❌
- Mock the system under test
- Test implementation details
- Create overly complex test setups
- Share mocks between tests
- Verify methods that weren't called
- Mix acceptance and unit test concerns

## 🐛 Troubleshooting

### Mock Not Recording Calls
**Problem**: `verify_called()` returns false
**Solution**: Ensure same mock instance used
```rust
// ✅ Correct
let mock = MockExecutor::new();
let executor = PhaseExecutor::new(mock.clone(), ...);
assert!(mock.verify_called("cmd"));
```

### Tests Interfere With Each Other
**Problem**: Test passes alone, fails in suite
**Solution**: Use `TestEnvironment` for isolation
```rust
TestEnvironment::run_test(|mocks| {
    // Automatic isolation
});
```

### Order Verification Fails
**Problem**: `verify_call_order()` fails unexpectedly
**Solution**: Use constants to avoid typos
```rust
const BUILD: &str = "npm run build";
mock.with_command_success(BUILD);
assert!(mock.verify_call_order(&[BUILD]));
```

## 📈 Next Steps

### Applying London School TDD

1. **Start with acceptance test** - Define user story
2. **Mock collaborators** - Identify dependencies
3. **Watch test fail** - Verify it's testing something
4. **Implement minimally** - Get to green fast
5. **Refactor design** - Extract patterns
6. **Add unit tests** - Test new components
7. **Repeat** - Build next feature

### Features to Add Using TDD

- [ ] Cache invalidation logic
- [ ] Parallel workspace execution
- [ ] Retry with exponential backoff
- [ ] Dry-run mode
- [ ] Environment variable substitution
- [ ] Conditional phase execution
- [ ] Progress percentage tracking

## 📖 Additional Resources

- **[Growing Object-Oriented Software, Guided by Tests](http://www.growing-object-oriented-software.com/)** - The definitive London School TDD book
- **[Mock Roles, Not Objects](http://jmock.org/oopsla2004.pdf)** - Original mockist TDD paper
- **Test files in this directory** - Practical examples

## 🤝 Contributing

When adding tests:

1. Follow existing patterns
2. Use helper utilities
3. Write descriptive test names
4. Document complex scenarios
5. Keep tests focused and isolated

## 📝 Summary

This test suite demonstrates:

- ✅ Complete mock infrastructure for all dependencies
- ✅ Outside-in acceptance tests for user stories
- ✅ Interaction-based unit tests for components
- ✅ Red-Green-Refactor workflow examples
- ✅ Test helpers for productivity
- ✅ Comprehensive documentation

**Key Insight**: In London School TDD, mocks aren't just test utilities - they're design tools that help discover better interfaces and clearer responsibilities.

---

**Questions?** See the comprehensive guides in `/docs/LONDON_*.md` or examine the test files for practical examples.
