# Testing Guide: Agent Integration Patterns

**Chicago TDD & Integration Testing for Agent-Tool Systems**

## Table of Contents

1. [Overview](#overview)
2. [Chicago TDD Pattern](#chicago-tdd-pattern)
3. [Unit Tests](#unit-tests)
4. [Integration Tests](#integration-tests)
5. [Mock Agent Creation](#mock-agent-creation)
6. [Performance Testing](#performance-testing)
7. [Test Organization](#test-organization)
8. [Common Patterns](#common-patterns)

---

## Overview

This guide documents testing patterns for agent-tool integration using the **Chicago TDD** pattern (state-based testing with real collaborators, not mocks).

### Why Chicago TDD?

| Aspect | Chicago TDD | London TDD |
|--------|-----------|-----------|
| **Focus** | Behavior + state changes | Message passing |
| **Mocking** | Real collaborators | Mock everything |
| **Test Scope** | Integration | Isolation |
| **Complexity** | Lower (real objects) | Higher (many mocks) |
| **Refactoring** | Robust to changes | Brittle |
| **For Agent Systems** | ✓ Better | ✗ Over-mocks |

**Key Principle**: Test agents with **real registries and real tools**, not mocks.

---

## Chicago TDD Pattern

### AAA Pattern: Arrange-Act-Assert

```rust
#[tokio::test]
async fn test_agent_invokes_tool_successfully() {
    // ARRANGE: Set up registry and tools
    let registry = ToolRegistry::new();
    let tool = Arc::new(TestFinancialTool::new());
    registry.register(tool).await.expect("Register tool");

    let mut agent = FinancialAgent::new(registry.clone());
    agent.initialize().await.expect("Initialize agent");

    // ACT: Execute the behavior
    let input = InputData::from_json(serde_json::json!({
        "ticker_symbol": "AAPL",
        "period_days": 90,
    })).expect("Parse input");

    let result = agent.execute_task_with_tool(&input, "FinancialAnalyzer").await;

    // ASSERT: Verify state changes and outcomes
    assert!(result.is_ok(), "Execution should succeed");
    let output = result.unwrap();
    assert!(output.contains_key("recommendation"), "Output should have recommendation");
    assert_eq!(
        output.get("recommendation").unwrap().as_str().unwrap(),
        "BUY"
    );
}
```

### Key Characteristics

1. **Real Objects**: Use actual ToolRegistry, not mocks
2. **Focused Scope**: Test one agent behavior per test
3. **Clear Intent**: Test name describes expected behavior
4. **Fast Execution**: <100ms per test
5. **No Test Interdependencies**: Each test stands alone

---

## Unit Tests

### Testing Signatures

```rust
#[cfg(test)]
mod signature_tests {
    use super::*;
    use ggen_ai::dspy::{Signature, InputField, OutputField};
    use ggen_ai::generators::validator::FieldConstraint;

    #[test]
    fn test_signature_field_access() {
        // ARRANGE
        let sig = Signature::new("Calculator", "Simple math")
            .with_input(InputField::new("a", "f64"))
            .with_input(InputField::new("b", "f64"))
            .with_output(OutputField::new("result", "f64"));

        // ACT
        let input_names = sig.input_names();
        let output_names = sig.output_names();

        // ASSERT
        assert_eq!(input_names, vec!["a", "b"]);
        assert_eq!(output_names, vec!["result"]);
    }

    #[test]
    fn test_signature_field_retrieval() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("name", "String"));

        let field = sig.get_input("name");
        assert!(field.is_some());
        assert_eq!(field.unwrap().type_annotation(), "String");
    }

    #[test]
    fn test_signature_with_constraints() {
        let sig = Signature::new("Validator", "Test")
            .with_input(
                InputField::new("ticker", "String")
                    .with_constraint(FieldConstraint::pattern("^[A-Z]{1,5}$"))
                    .with_constraint(FieldConstraint::required())
            );

        let field = sig.get_input("ticker").unwrap();
        assert!(field.is_required());
    }

    #[test]
    fn test_signature_code_generation() {
        let sig = Signature::new("TestStruct", "A test structure")
            .with_input(InputField::new("field_a", "String"))
            .with_output(OutputField::new("result", "i32"));

        let code = sig.as_rust_struct();
        assert!(code.contains("TestStruct_Inputs"));
        assert!(code.contains("TestStruct_Outputs"));
        assert!(code.contains("field_a: String"));
        assert!(code.contains("result: i32"));
    }
}
```

### Testing Constraints

```rust
#[cfg(test)]
mod constraint_tests {
    use super::*;
    use ggen_ai::generators::validator::{FieldConstraint, ConstraintValidator, BasicConstraintValidator};

    #[test]
    fn test_required_constraint() {
        let validator = BasicConstraintValidator;
        let constraint = FieldConstraint::required();

        assert!(validator.validate("value", &constraint).passed);
        assert!(!validator.validate("", &constraint).passed);
    }

    #[test]
    fn test_pattern_constraint() {
        let validator = BasicConstraintValidator;
        let constraint = FieldConstraint::pattern("^[A-Z]{1,5}$");

        assert!(validator.validate("AAPL", &constraint).passed);
        assert!(validator.validate("MSFT", &constraint).passed);
        assert!(!validator.validate("invalid123", &constraint).passed);
        assert!(!validator.validate("toolong", &constraint).passed);
    }

    #[test]
    fn test_length_constraints() {
        let validator = BasicConstraintValidator;

        let min_constraint = FieldConstraint::min_length(3);
        assert!(!validator.validate("ab", &min_constraint).passed);
        assert!(validator.validate("abc", &min_constraint).passed);

        let max_constraint = FieldConstraint::max_length(5);
        assert!(validator.validate("ab", &max_constraint).passed);
        assert!(!validator.validate("toolong", &max_constraint).passed);
    }
}
```

---

## Integration Tests

### Testing Tool Registry

```rust
#[cfg(test)]
mod registry_integration_tests {
    use super::*;
    use ggen_ai::tools::{ToolRegistry, Tool, InputData, OutputData};
    use ggen_ai::dspy::Signature;
    use async_trait::async_trait;
    use std::sync::Arc;

    // Real test tool implementation
    #[derive(Debug)]
    struct TestTool {
        name: String,
        signature: Signature,
        call_count: Arc<tokio::sync::Mutex<u32>>,
    }

    #[async_trait]
    impl Tool for TestTool {
        fn name(&self) -> &str { &self.name }
        fn signature(&self) -> &Signature { &self.signature }

        async fn execute(&self, input: InputData) -> Result<OutputData> {
            let mut count = self.call_count.lock().await;
            *count += 1;

            let mut output = OutputData::new();
            output.insert("echo".to_string(), serde_json::json!("ok"));
            Ok(output)
        }
    }

    #[tokio::test]
    async fn test_registry_register_and_retrieve() {
        // ARRANGE
        let registry = ToolRegistry::new();
        let sig = Signature::new("TestTool", "Test tool");
        let tool = Arc::new(TestTool {
            name: "TestTool".to_string(),
            signature: sig,
            call_count: Arc::new(tokio::sync::Mutex::new(0)),
        });

        // ACT
        registry.register(tool.clone()).await.expect("Register");

        // ASSERT
        let retrieved = registry.get("TestTool").await;
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().name(), "TestTool");
    }

    #[tokio::test]
    async fn test_registry_invoke_with_validation() {
        // ARRANGE
        let registry = ToolRegistry::new();
        let sig = Signature::new("Calculator", "Add numbers")
            .with_input(InputField::new("a", "f64"))
            .with_input(InputField::new("b", "f64"))
            .with_output(OutputField::new("sum", "f64"));

        let tool = Arc::new(CalculatorTool { signature: sig });
        registry.register(tool).await.expect("Register");

        let input = InputData::from_json(serde_json::json!({
            "a": 5.0,
            "b": 3.0,
        })).expect("Parse input");

        // ACT
        let result = registry.invoke("Calculator", &input).await;

        // ASSERT
        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(output.get("sum").unwrap().as_f64().unwrap(), 8.0);
    }

    #[tokio::test]
    async fn test_registry_tool_not_found() {
        let registry = ToolRegistry::new();
        let input = InputData::new();

        let result = registry.invoke("NonExistent", &input).await;

        assert!(result.is_err());
        match result {
            Err(GgenAiError::ToolNotFound(name)) => assert_eq!(name, "NonExistent"),
            _ => panic!("Expected ToolNotFound error"),
        }
    }

    #[tokio::test]
    async fn test_registry_input_validation_failure() {
        // ARRANGE
        let registry = ToolRegistry::new();
        let sig = Signature::new("Ticker", "Stock ticker analyzer")
            .with_input(
                InputField::new("ticker", "String")
                    .with_constraint(FieldConstraint::pattern("^[A-Z]{1,5}$"))
            );

        let tool = Arc::new(TickerTool { signature: sig });
        registry.register(tool).await.expect("Register");

        // Invalid ticker (contains numbers)
        let input = InputData::from_json(serde_json::json!({
            "ticker": "invalid123",
        })).expect("Parse input");

        // ACT & ASSERT: Should fail validation
        let result = registry.invoke("Ticker", &input).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_registry_list_and_discovery() {
        let registry = ToolRegistry::new();

        // Register multiple tools
        for i in 0..3 {
            let tool = Arc::new(TestTool {
                name: format!("Tool{}", i),
                signature: Signature::new(format!("Tool{}", i), "Test"),
                call_count: Arc::new(tokio::sync::Mutex::new(0)),
            });
            registry.register(tool).await.expect("Register");
        }

        // ACT
        let tools = registry.list_all().await;
        let count = registry.count().await;

        // ASSERT
        assert_eq!(count, 3);
        assert_eq!(tools.len(), 3);
    }
}
```

### Testing Agent-Tool Interaction

```rust
#[cfg(test)]
mod agent_integration_tests {
    use super::*;
    use ggen_ai::agents::{Agent, AgentConfig, AgentMessage, TaskDefinition, TaskType, TaskPriority};
    use ggen_ai::tools::{ToolRegistry, Tool};
    use uuid::Uuid;

    #[tokio::test]
    async fn test_agent_initializes_with_registry() {
        // ARRANGE
        let registry = ToolRegistry::new();
        let config = AgentConfig {
            id: Uuid::new_v4(),
            name: "TestAgent".to_string(),
            role: AgentRole::Validator,
            capabilities: vec!["test".to_string()],
            max_concurrent_tasks: 5,
        };

        let mut agent = TestAgent::new(config.clone(), registry.clone());

        // ACT
        let result = agent.initialize().await;

        // ASSERT
        assert!(result.is_ok());
        assert_eq!(agent.config().name, "TestAgent");
    }

    #[tokio::test]
    async fn test_agent_executes_tool_through_registry() {
        // ARRANGE
        let registry = ToolRegistry::new();

        // Register a real tool
        let sig = Signature::new("TestTool", "Test")
            .with_input(InputField::new("input", "String"))
            .with_output(OutputField::new("output", "String"));

        let tool = Arc::new(TestTool { signature: sig, ... });
        registry.register(tool).await.expect("Register");

        let mut agent = TestAgent::new(config, registry.clone());
        agent.initialize().await.expect("Initialize");

        // ACT: Send task to agent
        let task = TaskDefinition {
            id: Uuid::new_v4(),
            task_type: TaskType::Analysis,
            parameters: serde_json::json!({
                "tool_name": "TestTool",
                "input": "test_data",
            }).into(),
            priority: TaskPriority::Normal,
        };

        let message = AgentMessage::TaskAssignment {
            task_id: task.id,
            task: task.clone(),
        };

        let result = agent.handle_message(message).await;

        // ASSERT: Agent should invoke tool and return success
        assert!(result.is_ok());
        match result.unwrap() {
            AgentMessage::TaskCompletion { success, result, .. } => {
                assert!(success);
                assert!(result.is_some());
            }
            _ => panic!("Expected TaskCompletion message"),
        }
    }
}
```

---

## Mock Agent Creation

### Minimal Test Agent

```rust
use async_trait::async_trait;

#[derive(Debug)]
struct TestAgent {
    config: AgentConfig,
    registry: Arc<ToolRegistry>,
    status: AgentStatus,
}

impl TestAgent {
    fn new(config: AgentConfig, registry: Arc<ToolRegistry>) -> Self {
        Self {
            config,
            registry,
            status: AgentStatus::Offline,
        }
    }
}

#[async_trait]
impl Agent for TestAgent {
    async fn initialize(
        &mut self,
    ) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
        self.status = AgentStatus::Healthy;
        Ok(())
    }

    async fn start(
        &mut self,
    ) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
        Ok(())
    }

    async fn stop(
        &mut self,
    ) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
        self.status = AgentStatus::Offline;
        Ok(())
    }

    async fn status(&self) -> AgentStatus {
        self.status.clone()
    }

    fn config(&self) -> &AgentConfig {
        &self.config
    }

    async fn handle_message(
        &mut self,
        message: AgentMessage,
    ) -> std::result::Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        match message {
            AgentMessage::TaskAssignment { task_id, task } => {
                let output = self.registry
                    .invoke("TestTool", &InputData::new())
                    .await
                    .map_err(|e| format!("Tool invocation failed: {}", e).into())?;

                Ok(AgentMessage::TaskCompletion {
                    task_id,
                    result: TaskResult {
                        task_id,
                        success: true,
                        result: Some(serde_json::to_value(output)?),
                        error: None,
                        duration_ms: 0,
                        metrics: None,
                    },
                })
            }
            _ => Ok(message),
        }
    }
}
```

### Spy Tool for Verification

```rust
#[derive(Debug)]
struct SpyTool {
    signature: Signature,
    executions: Arc<tokio::sync::Mutex<Vec<InputData>>>,
}

#[async_trait]
impl Tool for SpyTool {
    fn name(&self) -> &str { "SpyTool" }
    fn signature(&self) -> &Signature { &self.signature }

    async fn execute(&self, input: InputData) -> Result<OutputData> {
        // Record call
        let mut execs = self.executions.lock().await;
        execs.push(input.clone());

        // Return valid output
        let mut output = OutputData::new();
        output.insert("result".to_string(), serde_json::json!("ok"));
        Ok(output)
    }
}

// Usage in test
#[tokio::test]
async fn test_agent_calls_tool_with_correct_params() {
    let spy = Arc::new(SpyTool {
        signature: Signature::new("SpyTool", "Spy"),
        executions: Arc::new(tokio::sync::Mutex::new(Vec::new())),
    });

    // ... run agent code ...

    // Verify tool was called with expected data
    let execs = spy.executions.lock().await;
    assert_eq!(execs.len(), 1);
    assert_eq!(execs[0].get("expected_field"), Some(expected_value));
}
```

---

## Performance Testing

### Benchmarking Tool Invocation

```rust
#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::time::Instant;

    #[tokio::test]
    async fn bench_tool_lookup() {
        let registry = ToolRegistry::new();

        // Register 100 tools
        for i in 0..100 {
            let tool = Arc::new(TestTool {
                name: format!("Tool{:03}", i),
                ...
            });
            registry.register(tool).await.ok();
        }

        let start = Instant::now();
        for _ in 0..1000 {
            let _ = registry.get("Tool050").await;
        }
        let elapsed = start.elapsed();

        println!("1000 lookups: {}ms", elapsed.as_millis());
        assert!(elapsed.as_millis() < 100, "Lookup should be <100ms");
    }

    #[tokio::test]
    async fn bench_validation() {
        let sig = Signature::new("Test", "Test")
            .with_input(
                InputField::new("ticker", "String")
                    .with_constraint(FieldConstraint::pattern("^[A-Z]{1,5}$"))
                    .with_constraint(FieldConstraint::min_length(1))
                    .with_constraint(FieldConstraint::max_length(5))
            );

        let validator = SignatureValidator::new(sig);
        let input = InputData::from_json(serde_json::json!({
            "ticker": "AAPL",
        })).expect("Parse");

        let start = Instant::now();
        for _ in 0..1000 {
            let _ = validator.validate_input(&input);
        }
        let elapsed = start.elapsed();

        println!("1000 validations: {}ms", elapsed.as_millis());
        assert!(elapsed.as_millis() < 100, "Validation should be <100ms");
    }
}
```

### SLO Verification

```rust
#[tokio::test]
async fn verify_slos() {
    let registry = ToolRegistry::new();
    let tool = Arc::new(TestTool { ... });
    registry.register(tool).await.ok();

    // Tool lookup SLO: <1ms
    let start = Instant::now();
    let _ = registry.get("TestTool").await;
    assert!(start.elapsed().as_micros() < 1000);

    // Input validation SLO: <10ms
    let validator = SignatureValidator::new(sig.clone());
    let start = Instant::now();
    let _ = validator.validate_input(&input);
    assert!(start.elapsed().as_millis() < 10);

    // Complete invoke cycle SLO: <150ms
    let start = Instant::now();
    let _ = registry.invoke("TestTool", &input).await;
    assert!(start.elapsed().as_millis() < 150);
}
```

---

## Test Organization

### File Structure

```
crates/ggen-ai/tests/
├── integration/
│   ├── agent_registry_tests.rs      # Agent + Registry integration
│   ├── tool_registry_tests.rs       # Tool Registry tests
│   ├── validation_tests.rs          # Validation + constraints
│   └── workflow_tests.rs            # End-to-end workflows
├── units/
│   ├── signature_tests.rs           # Signature API tests
│   ├── constraint_tests.rs          # Constraint tests
│   ├── field_tests.rs               # Input/Output field tests
│   └── error_tests.rs               # Error handling tests
└── fixtures/
    ├── tools.rs                     # Test tool implementations
    ├── agents.rs                    # Test agent implementations
    └── data.rs                      # Test data builders
```

### Test Helpers Module

```rust
// tests/common/mod.rs
pub mod fixtures {
    use ggen_ai::*;
    use std::sync::Arc;

    pub fn create_test_registry() -> Arc<ToolRegistry> {
        Arc::new(ToolRegistry::new())
    }

    pub fn create_test_agent(registry: Arc<ToolRegistry>) -> TestAgent {
        let config = AgentConfig {
            id: Uuid::new_v4(),
            name: "TestAgent".to_string(),
            role: AgentRole::Validator,
            capabilities: vec![],
            max_concurrent_tasks: 5,
        };
        TestAgent::new(config, registry)
    }

    pub fn create_financial_signature() -> Signature {
        Signature::new("FinancialAnalyzer", "Analyze financial data")
            .with_input(InputField::new("ticker", "String")
                .with_constraint(FieldConstraint::pattern("^[A-Z]{1,5}$")))
            .with_output(OutputField::new("recommendation", "String"))
    }
}
```

---

## Common Patterns

### Testing Input Validation Errors

```rust
#[tokio::test]
async fn test_invalid_ticker_symbol() {
    let registry = ToolRegistry::new();
    let tool = Arc::new(TickerTool { ... });
    registry.register(tool).await.ok();

    let invalid_input = InputData::from_json(serde_json::json!({
        "ticker": "INVALID123",  // Invalid format
    })).expect("Parse");

    let result = registry.invoke("TickerTool", &invalid_input).await;

    assert!(result.is_err());
    match result {
        Err(GgenAiError::Validation(ValidationError::PatternMismatch { field, .. })) => {
            assert_eq!(field, "ticker");
        }
        _ => panic!("Expected PatternMismatch error"),
    }
}
```

### Testing Concurrent Invocations

```rust
#[tokio::test]
async fn test_concurrent_tool_invocations() {
    let registry = Arc::new(ToolRegistry::new());
    let tool = Arc::new(ConcurrentTestTool { ... });
    registry.register(tool).await.ok();

    let mut handles = Vec::new();

    // Spawn 10 concurrent invocations
    for i in 0..10 {
        let reg = registry.clone();
        let handle = tokio::spawn(async move {
            let input = InputData::from_json(serde_json::json!({
                "id": i,
            })).expect("Parse");

            reg.invoke("Tool", &input).await
        });
        handles.push(handle);
    }

    // Wait for all to complete
    for handle in handles {
        let result = handle.await;
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }
}
```

### Testing Tool Registry Lifecycle

```rust
#[tokio::test]
async fn test_registry_lifecycle() {
    let registry = ToolRegistry::new();

    // Register
    let tool = Arc::new(TestTool { ... });
    registry.register(tool.clone()).await.ok();
    assert_eq!(registry.count().await, 1);

    // Use
    let input = InputData::new();
    let result = registry.invoke("TestTool", &input).await;
    assert!(result.is_ok());

    // Unregister
    assert!(registry.unregister("TestTool").await);
    assert_eq!(registry.count().await, 0);

    // Verify removed
    let result = registry.invoke("TestTool", &input).await;
    assert!(result.is_err());
}
```

---

## Running Tests

### Run all tests
```bash
cargo test --package ggen-ai
```

### Run specific test file
```bash
cargo test --package ggen-ai --test integration_tests
```

### Run with output
```bash
cargo test --package ggen-ai -- --nocapture
```

### Run benchmarks
```bash
cargo test --package ggen-ai --test perf_tests -- --nocapture --test-threads=1
```

---

## Related Documentation

- **Agent Integration Guide**: [AGENT_INTEGRATION.md](AGENT_INTEGRATION.md)
- **API Reference**: [API_REFERENCE.md](API_REFERENCE.md)
- **Chicago TDD Pattern**: Chicago TDD principles applied to Rust testing
