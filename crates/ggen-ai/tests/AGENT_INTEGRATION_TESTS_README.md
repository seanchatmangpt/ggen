# Agent Infrastructure Integration Tests

Comprehensive integration test suite for ggen-ai agent infrastructure following Chicago TDD principles.

## Test Files

### 1. `agent_integration_tests.rs` (Full Suite - Requires Enabled Modules)

**Status**: Requires `hyper_concurrent`, `microframework`, and `swarm` modules to be enabled in `src/lib.rs`

Comprehensive end-to-end integration tests covering:

- **HyperConcurrent + DSPy Predictor Pipeline**
  - 10-agent parallel execution with DSPy predictors
  - ChainOfThought parallel reasoning
  - Deterministic execution with mock LLM

- **Microframework Task Graphs with DSPy Modules**
  - Task graph dependency resolution
  - 10-agent parallel task execution
  - Complex multi-stage pipelines
  - Topological sort validation

- **Swarm Collaborative Optimization**
  - Multi-agent swarm coordination
  - 10-agent parallel swarm execution
  - Work-stealing load balancing
  - Health monitoring

- **Error Propagation and Recovery**
  - Circuit breaker pattern
  - Timeout handling
  - Retry mechanisms
  - Partial failure recovery

- **Performance and Determinism**
  - Deterministic execution with mock LLM
  - Performance under max load (10 agents)
  - Backpressure under overload
  - SLO compliance (<30s for full suite)

**To Enable:**
```bash
# Edit src/lib.rs and uncomment:
pub mod hyper_concurrent;
pub mod microframework;
pub mod swarm;

# Then run:
cargo test --package ggen-ai --test agent_integration_tests
```

### 2. `agent_integration_tests_working.rs` (Current Working Suite)

**Status**: ✓ Works with currently enabled modules (DSPy only)

Tests that work right now without enabling additional modules:

- **DSPy Predictor with Mock LLM**
  - Predictor configuration and validation
  - ChainOfThought reasoning instructions
  - Temperature clamping (0.0-2.0)
  - Model configuration

- **Signature Validation**
  - Signature builder pattern
  - Field constraints (min, max, required, pattern)
  - Multiple input/output fields
  - Metadata and descriptions

- **Mock LLM Client**
  - Deterministic responses
  - Usage statistics
  - Multiple response scenarios

- **Performance**
  - Fast signature creation (1000 sigs <100ms)
  - Module trait compliance

**Run Tests:**
```bash
cargo test --package ggen-ai --test agent_integration_tests_working
```

## Testing Philosophy: Chicago TDD

All tests follow the **Chicago TDD** pattern:

### AAA Pattern (Arrange-Act-Assert)

```rust
#[tokio::test]
async fn test_example() {
    // Arrange: Set up real objects (no mocks except LLM)
    let signature = Signature::new("Test", "Description")
        .with_input(InputField::new("input", "desc", "String"))
        .with_output(OutputField::new("output", "desc", "String"));

    let predictor = Predictor::with_model(signature, "model");

    // Act: Call the public API
    let result = predictor.forward(inputs).await;

    // Assert: Verify observable state/behavior
    assert!(result.is_ok());
    assert_eq!(result.unwrap().len(), 1);
}
```

### Real Objects, Not Mocks

- Use **real agent implementations**
- Use **real task graphs** and **real executors**
- Only mock: **LLM responses** (for determinism and speed)

### State-Based Verification

Test observable state changes, not internal implementation:
- ✓ Task completed successfully
- ✓ All 10 agents executed
- ✓ Metrics show success rate
- ✗ Don't verify internal method calls

## Test Coverage

### Coverage Targets

| Component | Unit Tests | Integration Tests | Status |
|-----------|-----------|-------------------|--------|
| DSPy Signature | ✓ | ✓ | Complete |
| DSPy Predictor | ✓ | ✓ | Complete |
| HyperConcurrent Executor | ✓ | Pending module enable | Ready |
| Microframework Orchestrator | ✓ | Pending module enable | Ready |
| Swarm Coordinator | ✓ | Pending module enable | Ready |
| Circuit Breaker | ✓ | ✓ | Complete |
| Work Stealing | ✓ | ✓ | Complete |

### Test Scenarios

#### Parallel Execution (10-Agent Max)

```rust
// Test maximum parallelism
#[tokio::test]
async fn test_ten_agent_parallel_execution() {
    let executor = HyperConcurrentExecutor::max_performance();

    let tasks: Vec<_> = (0..10).map(|i| {
        let task = move || async move { Ok(compute(i)) };
        (format!("agent-{}", i), task)
    }).collect();

    let results = executor.execute_parallel(tasks).await;

    assert_eq!(results.len(), 10);
    assert!(results.iter().all(|r| r.is_success()));
}
```

#### Task Graph Dependencies

```rust
// Test dependency resolution
#[tokio::test]
async fn test_task_dependencies() {
    let mut graph = TaskGraph::new();

    let t1 = Task::code_gen("Generate base");
    let t2 = Task::test("Test base").with_dependencies(vec![t1.id.clone()]);
    let t3 = Task::validate("Validate").with_dependencies(vec![t2.id.clone()]);

    graph.add_task(t1); graph.add_task(t2); graph.add_task(t3);

    let sorted = graph.topological_sort().unwrap();

    // Verify correct execution order
    assert!(sorted[0].contains(&t1.id));
    // t2 must come after t1, t3 after t2
}
```

#### Error Recovery

```rust
// Test circuit breaker on failures
#[tokio::test]
async fn test_circuit_breaker() {
    let config = HyperConcurrentConfig {
        circuit_breaker_threshold: 3,
        ..Default::default()
    };
    let executor = HyperConcurrentExecutor::new(config);

    // Create failing tasks
    let tasks = vec![...failing tasks...];
    executor.execute_parallel(tasks).await;

    // Assert: Circuit breaker opens after threshold
    assert!(executor.is_circuit_open("failing-agent"));
}
```

## Determinism

### Mock LLM for Determinism

```rust
use ggen_ai::providers::MockClient;

let mock = MockClient::with_response("Fixed response");
let response = mock.complete("Any prompt").await.unwrap();

assert_eq!(response.content, "Fixed response");
```

### Deterministic Test Characteristics

- ✓ Same input → Same output (always)
- ✓ No random behavior
- ✓ No race conditions
- ✓ No flaky tests
- ✓ Fast execution (<30s for full suite)

## Performance Requirements

### SLO Targets

| Test Suite | Target | Actual |
|-----------|--------|--------|
| Working Suite | <5s | TBD |
| Full Integration Suite | <30s | TBD |
| 10-Agent Parallel | <5s | TBD |
| Single Task | <100ms | TBD |

### Measurement

```rust
#[tokio::test]
async fn test_performance_slo() {
    let start = std::time::Instant::now();

    let results = executor.execute_parallel(tasks).await;

    let duration = start.elapsed();
    assert!(duration < Duration::from_secs(5),
        "Should complete in <5s, took {:?}", duration);
}
```

## Running Tests

### Run Working Tests Only

```bash
# Fast - runs with currently enabled modules
cargo test --package ggen-ai --test agent_integration_tests_working
```

### Run Full Suite (After Enabling Modules)

```bash
# 1. Edit src/lib.rs - uncomment agent modules
# 2. Run full integration tests
cargo test --package ggen-ai --test agent_integration_tests

# Or use cargo make
cargo make test
```

### Run Specific Test

```bash
# Run single test by name
cargo test --package ggen-ai --test agent_integration_tests_working test_dspy_predictor_with_mock_llm

# Run tests matching pattern
cargo test --package ggen-ai --test agent_integration_tests_working dspy
```

### Run with Output

```bash
# Show println! output
cargo test --package ggen-ai --test agent_integration_tests_working -- --nocapture

# Show test names as they run
cargo test --package ggen-ai --test agent_integration_tests_working -- --test-threads=1 --nocapture
```

## Test Receipts

When tests complete, provide receipt with:

```
[Receipt] cargo test --package ggen-ai --test agent_integration_tests_working
✓ 10/10 tests passed
✓ Duration: 2.3s (<5s SLO)
✓ All deterministic (no flaky tests)
✓ Coverage: DSPy module 95%
```

Example format:

```
Test Suite: agent_integration_tests_working
Status: ✓ PASS
Tests: 10 passed, 0 failed
Duration: 2.34s
SLO: ✓ <5s (met)
Determinism: ✓ All tests deterministic
Coverage: DSPy Predictor 100%, Signature 100%
```

## Troubleshooting

### Module Not Found Errors

```
error[E0433]: failed to resolve: use of undeclared crate or module `hyper_concurrent`
```

**Solution**: These tests require agent modules to be enabled in `src/lib.rs`. Use `agent_integration_tests_working.rs` for tests that work now, or enable the modules.

### Compilation Timeout

If compilation takes too long:

```bash
# Build lib first
cargo build --package ggen-ai --lib

# Then run tests
cargo test --package ggen-ai --test agent_integration_tests_working --no-run
cargo test --package ggen-ai --test agent_integration_tests_working
```

### Test Failures

1. **Check module status**: Ensure required modules are enabled
2. **Check dependencies**: Run `cargo update`
3. **Clean build**: `cargo clean && cargo build`
4. **Check logs**: Use `RUST_LOG=debug cargo test`

## Contributing

When adding new integration tests:

1. **Follow Chicago TDD**: AAA pattern, real objects
2. **Test observable behavior**: Not internal implementation
3. **Use mock LLM only**: Keep tests deterministic
4. **Target <30s total**: Fast feedback
5. **Document test purpose**: Clear Arrange-Act-Assert sections
6. **Verify 10-agent parallelism**: If testing concurrency
7. **Check error paths**: Test success AND failure cases

### Test Template

```rust
#[tokio::test]
async fn test_descriptive_name() {
    // Arrange: Create real objects and setup
    let component = RealComponent::new(config);
    let input = create_test_input();

    // Act: Execute the operation under test
    let result = component.execute(input).await;

    // Assert: Verify observable outcomes
    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output.status, Expected::Status);
    assert!(output.metrics.success_rate > 0.9);
}
```

## References

- [Chicago TDD Pattern](../../../.claude/skills/chicago-tdd-pattern.md)
- [ggen Testing Strategy](../../../TESTING.md)
- [DSPy Documentation](../../ggen-ai/src/dspy/README.md)
- [HyperConcurrent Architecture](../../ggen-ai/src/hyper_concurrent/mod.rs)
