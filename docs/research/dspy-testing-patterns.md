<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DSPy Testing Patterns Research & Recommendations for Rust Implementation](#dspy-testing-patterns-research--recommendations-for-rust-implementation)
  - [Executive Summary](#executive-summary)
    - [Python DSPy Testing Architecture](#python-dspy-testing-architecture)
    - [Current Rust Implementation Status](#current-rust-implementation-status)
    - [Key Recommendations](#key-recommendations)
  - [1. Python DSPy Test Suite Structure](#1-python-dspy-test-suite-structure)
    - [1.1 Directory Organization](#11-directory-organization)
    - [1.2 pytest Configuration](#12-pytest-configuration)
  - [2. DSPy Mocking Strategies](#2-dspy-mocking-strategies)
    - [2.1 DummyLM Implementation](#21-dummylm-implementation)
      - [Mode 1: Sequential List Mode](#mode-1-sequential-list-mode)
      - [Mode 2: Query-Based Dictionary Mode](#mode-2-query-based-dictionary-mode)
      - [Mode 3: Example Following Mode](#mode-3-example-following-mode)
    - [2.2 Rust Equivalent Implementation Strategy](#22-rust-equivalent-implementation-strategy)
  - [3. Test Data and Fixture Patterns](#3-test-data-and-fixture-patterns)
    - [3.1 Python DSPy Example Objects](#31-python-dspy-example-objects)
    - [3.2 Rust Implementation - Current vs Recommended](#32-rust-implementation---current-vs-recommended)
  - [4. Testing Modules, Optimizers, and Pipelines](#4-testing-modules-optimizers-and-pipelines)
    - [4.1 Module Testing Pattern (Python DSPy)](#41-module-testing-pattern-python-dspy)
    - [4.2 Optimizer Testing Pattern (Python DSPy)](#42-optimizer-testing-pattern-python-dspy)
    - [4.3 Rust Implementation Recommendations](#43-rust-implementation-recommendations)
  - [5. Integration vs Unit Testing Approaches](#5-integration-vs-unit-testing-approaches)
    - [5.1 Python DSPy Approach](#51-python-dspy-approach)
    - [5.2 Rust Implementation - Current vs Recommended](#52-rust-implementation---current-vs-recommended)
  - [6. Error Handling and Retry Testing Patterns](#6-error-handling-and-retry-testing-patterns)
    - [6.1 Python DSPy Assertions Framework](#61-python-dspy-assertions-framework)
    - [6.2 Rust Implementation Recommendation](#62-rust-implementation-recommendation)
  - [7. Performance Testing and Benchmarking](#7-performance-testing-and-benchmarking)
    - [7.1 Python DSPy Evaluation Patterns](#71-python-dspy-evaluation-patterns)
    - [7.2 Rust Benchmarking Recommendations](#72-rust-benchmarking-recommendations)
  - [8. Chicago TDD Integration](#8-chicago-tdd-integration)
    - [8.1 Chicago vs London Style Review](#81-chicago-vs-london-style-review)
    - [8.2 Hybrid Strategy for DSPy Testing](#82-hybrid-strategy-for-dspy-testing)
    - [8.3 Current Implementation Alignment](#83-current-implementation-alignment)
  - [9. Quality Metrics and Coverage Targets](#9-quality-metrics-and-coverage-targets)
    - [9.1 Python DSPy Metrics](#91-python-dspy-metrics)
    - [9.2 Rust Implementation Metrics](#92-rust-implementation-metrics)
  - [10. Testing Improvements Roadmap](#10-testing-improvements-roadmap)
    - [Phase 1: Foundation (Immediate - 1-2 weeks)](#phase-1-foundation-immediate---1-2-weeks)
    - [Phase 2: Optimizer Testing (2-4 weeks)](#phase-2-optimizer-testing-2-4-weeks)
    - [Phase 3: Advanced Testing (4-6 weeks)](#phase-3-advanced-testing-4-6-weeks)
    - [Phase 4: Quality & Polish (Ongoing)](#phase-4-quality--polish-ongoing)
  - [11. Key Takeaways](#11-key-takeaways)
    - [What to Adopt from DSPy](#what-to-adopt-from-dspy)
    - [What to Keep from Current Rust Implementation](#what-to-keep-from-current-rust-implementation)
    - [What Makes Rust Different](#what-makes-rust-different)
  - [12. Conclusion](#12-conclusion)
  - [Sources](#sources)
    - [DSPy Testing Documentation](#dspy-testing-documentation)
    - [Chicago TDD and Testing Best Practices](#chicago-tdd-and-testing-best-practices)
    - [Python Testing Frameworks](#python-testing-frameworks)
    - [Golden Tests and Regression Testing](#golden-tests-and-regression-testing)
    - [DSPy GitHub Test Files Referenced](#dspy-github-test-files-referenced)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DSPy Testing Patterns Research & Recommendations for Rust Implementation

**Date**: 2026-01-11
**Author**: Claude Code Research Agent
**Purpose**: Document Python DSPy testing patterns and provide actionable recommendations for ggen Rust implementation

---

## Executive Summary

This research analyzes Python DSPy's testing architecture and provides recommendations for enhancing the Rust implementation in ggen. Key findings:

### Python DSPy Testing Architecture
- **Test Organization**: Mirror-structured tests organized by functional component (17 subdirectories)
- **Mocking Strategy**: `DummyLM` with 3 operating modes (sequential, query-based, example-following)
- **Testing Framework**: pytest with async support, custom markers, and conditional execution
- **Quality Focus**: Built-in signature test suites, evaluation metrics, and validation utilities

### Current Rust Implementation Status
- ✅ **Strong Foundation**: Property-based testing with proptest, Chicago TDD patterns, mock generators
- ✅ **Good Practices**: Factory functions for test helpers, AAA pattern, integration test structure
- 🟡 **Gaps**: Limited optimizer testing, missing assertion/retry patterns, no golden tests

### Key Recommendations
1. Implement comprehensive `DummyLM` equivalent with multiple response modes
2. Expand optimizer testing with metrics validation
3. Add assertion/retry mechanism testing patterns
4. Implement golden test/baseline comparison framework
5. Enhance test data fixtures with example builder patterns
6. Add performance benchmarking for optimizers
7. Improve test categorization and conditional execution

---

## 1. Python DSPy Test Suite Structure

### 1.1 Directory Organization

DSPy organizes tests to mirror the main module structure, facilitating focused component testing:

```
tests/
├── adapters/          # Adapter functionality tests
├── callback/          # Callback mechanism tests
├── clients/           # LM client tests (including DummyLM usage)
├── datasets/          # Dataset handling tests
├── docs/              # Documentation tests
├── evaluate/          # Evaluation utility tests
├── examples/          # Example code validation
├── metadata/          # Metadata handling tests
├── predict/           # Prediction module tests (core functionality)
├── primitives/        # Core primitive tests (Example, Signature)
├── propose/           # Proposal generation tests
├── reliability/       # Adapter reliability and stability tests
├── retrievers/        # Retrieval system tests
├── signatures/        # Signature definition tests
├── streaming/         # Streaming functionality tests
├── teleprompt/        # Optimizer tests (BootstrapFewShot, MIPROv2, etc.)
└── test_utils/        # Testing utility functions
```

**Pattern**: Each subdirectory corresponds to a major feature or subsystem, enabling isolated testing.

### 1.2 pytest Configuration

**conftest.py fixtures**:

```python
@pytest.fixture(autouse=True)
def clear_settings():
    """Ensures settings are cleared after each test for isolation."""
    yield
    dspy.configure(DEFAULT_CONFIG)

@pytest.fixture
def anyio_backend():
    """Configures async test backend."""
    return "asyncio"

@pytest.fixture
def lm_for_test():
    """Retrieves LM_FOR_TEST environment variable or skips test."""
    model = os.getenv("LM_FOR_TEST")
    if not model:
        pytest.skip("LM_FOR_TEST not set")
    return model
```

**Custom markers for conditional execution**:
- `@pytest.mark.reliability` - Adapter reliability tests
- `@pytest.mark.extra` - Additional comprehensive tests
- `@pytest.mark.llm_call` - Tests requiring actual LM calls

**Command-line flags**:
```bash
pytest --reliability  # Run reliability tests
pytest --llm_call    # Run tests with actual LM calls
```

**Dependencies**:
- `pytest` (>6.2.5): Core framework
- `pytest-mock` (>3.12.0): Mocking support
- `pytest-asyncio` (>0.26.0): Async test support

---

## 2. DSPy Mocking Strategies

### 2.1 DummyLM Implementation

DSPy uses `DummyLM` for test-focused language model simulation with three operating modes:

#### Mode 1: Sequential List Mode
Returns predefined responses sequentially:

```python
# Python DSPy pattern
lm = DummyLM([
    {"answer": "Paris"},
    {"answer": "London"},
    {"answer": "Berlin"}
])
dspy.configure(lm=lm)

# Each call returns next response in sequence
predictor = dspy.Predict("question -> answer")
result1 = predictor(question="Capital of France?")  # "Paris"
result2 = predictor(question="Capital of UK?")      # "London"
```

#### Mode 2: Query-Based Dictionary Mode
Matches prompt content to return appropriate response:

```python
lm = DummyLM({
    "What is 1+1?": {"answer": "2"},
    "What is 2+2?": {"answer": "4"}
})
dspy.configure(lm=lm)

# Response selected based on query match
```

#### Mode 3: Example Following Mode
Examines prompt history to find matching examples:

```python
lm = DummyLM([], follow_examples=True)
# Returns outputs from matching examples in prompt
```

### 2.2 Rust Equivalent Implementation Strategy

**Current Rust Implementation** (`ggen-ai/src/test_helpers.rs`):

```rust
// Single-response mock (basic)
pub fn create_ontology_test_generator() -> OntologyGenerator {
    let response = r#"```turtle
@prefix ex: <http://example.org/> .
ex:Person a rdf:Class .
```"#;
    let client = MockClient::with_response(response);
    OntologyGenerator::new(Arc::new(client))
}
```

**Recommended Enhancement** - Multi-mode `DummyLM` equivalent:

```rust
/// Test-focused LM with multiple response modes (DSPy DummyLM equivalent)
pub enum DummyLMMode {
    /// Sequential: Return responses in order
    Sequential(Vec<String>),

    /// QueryBased: Match input to response
    QueryBased(HashMap<String, String>),

    /// ExampleFollowing: Extract from demonstration history
    ExampleFollowing {
        demonstrations: Vec<Demonstration>,
    },
}

pub struct DummyLM {
    mode: DummyLMMode,
    call_count: Arc<Mutex<usize>>,
    history: Arc<Mutex<Vec<(String, String)>>>,
}

impl DummyLM {
    /// Create sequential mode DummyLM
    pub fn sequential(responses: Vec<String>) -> Self {
        Self {
            mode: DummyLMMode::Sequential(responses),
            call_count: Arc::new(Mutex::new(0)),
            history: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Create query-based mode DummyLM
    pub fn query_based(responses: HashMap<String, String>) -> Self {
        Self {
            mode: DummyLMMode::QueryBased(responses),
            call_count: Arc::new(Mutex::new(0)),
            history: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Create example-following mode DummyLM
    pub fn example_following(demonstrations: Vec<Demonstration>) -> Self {
        Self {
            mode: DummyLMMode::ExampleFollowing { demonstrations },
            call_count: Arc::new(Mutex::new(0)),
            history: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Get call count for test assertions
    pub fn call_count(&self) -> usize {
        *self.call_count.lock().unwrap()
    }

    /// Get interaction history for verification
    pub fn history(&self) -> Vec<(String, String)> {
        self.history.lock().unwrap().clone()
    }
}

#[async_trait]
impl LlmClient for DummyLM {
    async fn complete(&self, prompt: &str) -> Result<String, GgenAiError> {
        let mut count = self.call_count.lock().unwrap();
        *count += 1;

        let response = match &self.mode {
            DummyLMMode::Sequential(responses) => {
                let idx = (*count - 1) % responses.len();
                responses[idx].clone()
            }
            DummyLMMode::QueryBased(map) => {
                // Find best match in map
                map.iter()
                    .find(|(key, _)| prompt.contains(key.as_str()))
                    .map(|(_, val)| val.clone())
                    .unwrap_or_else(|| "default response".to_string())
            }
            DummyLMMode::ExampleFollowing { demonstrations } => {
                // Extract matching demonstration output
                demonstrations
                    .iter()
                    .find(|demo| prompt.contains(&demo.input))
                    .map(|demo| demo.output.clone())
                    .unwrap_or_else(|| "no match".to_string())
            }
        };

        self.history.lock().unwrap().push((prompt.to_string(), response.clone()));
        Ok(response)
    }
}
```

**Usage in tests**:

```rust
#[tokio::test]
async fn test_sequential_responses() {
    let dummy = DummyLM::sequential(vec![
        "response 1".to_string(),
        "response 2".to_string(),
    ]);

    let client = Arc::new(dummy);
    let generator = OntologyGenerator::new(client.clone());

    // First call gets "response 1"
    let result1 = generator.generate_ontology("test", vec![]).await.unwrap();

    // Second call gets "response 2"
    let result2 = generator.generate_ontology("test", vec![]).await.unwrap();

    assert_eq!(client.call_count(), 2);
}

#[tokio::test]
async fn test_query_based_responses() {
    let mut responses = HashMap::new();
    responses.insert("rust".to_string(), "Rust ontology".to_string());
    responses.insert("python".to_string(), "Python ontology".to_string());

    let dummy = DummyLM::query_based(responses);
    let client = Arc::new(dummy);
    let generator = OntologyGenerator::new(client.clone());

    let result = generator.generate_ontology("rust project", vec![]).await.unwrap();
    assert!(result.contains("Rust ontology"));
}
```

---

## 3. Test Data and Fixture Patterns

### 3.1 Python DSPy Example Objects

DSPy uses `Example` objects as the core data structure for test datasets:

```python
# Creating examples with inputs/outputs
example = dspy.Example(
    question="What is the capital of France?",
    answer="Paris"
).with_inputs("question")

# Building datasets
trainset = [
    dspy.Example(question="Q1", answer="A1").with_inputs("question"),
    dspy.Example(question="Q2", answer="A2").with_inputs("question"),
]

# Helper function pattern
def new_example(question, answer):
    """Factory function for consistent example creation."""
    return dspy.Example(
        question=question,
        answer=answer,
    ).with_inputs("question")
```

### 3.2 Rust Implementation - Current vs Recommended

**Current** (`ggen-ai/tests/dspy_optimizer_test.rs`):

```rust
let mut ex1_inputs = HashMap::new();
ex1_inputs.insert("question".to_string(), json!("What is Rust?"));
let mut ex1_outputs = HashMap::new();
ex1_outputs.insert("answer".to_string(), json!("A programming language"));

let example = Example::new(ex1_inputs.clone(), ex1_outputs.clone());
```

**Recommended Enhancement** - Example Builder Pattern:

```rust
/// Builder for creating test examples with fluent API
pub struct ExampleBuilder {
    inputs: HashMap<String, Value>,
    outputs: HashMap<String, Value>,
}

impl ExampleBuilder {
    pub fn new() -> Self {
        Self {
            inputs: HashMap::new(),
            outputs: HashMap::new(),
        }
    }

    /// Add input field
    pub fn input(mut self, key: &str, value: impl Into<Value>) -> Self {
        self.inputs.insert(key.to_string(), value.into());
        self
    }

    /// Add output field
    pub fn output(mut self, key: &str, value: impl Into<Value>) -> Self {
        self.outputs.insert(key.to_string(), value.into());
        self
    }

    /// Build the example
    pub fn build(self) -> Example {
        Example::new(self.inputs, self.outputs)
    }
}

/// QA example factory (common pattern)
pub fn qa_example(question: &str, answer: &str) -> Example {
    ExampleBuilder::new()
        .input("question", question)
        .output("answer", answer)
        .build()
}

/// Create training dataset with factory
pub fn create_qa_trainset() -> Vec<Example> {
    vec![
        qa_example("What is Rust?", "A systems programming language"),
        qa_example("What is DSPy?", "A framework for programming LMs"),
        qa_example("What is ggen?", "An ontology-driven code generator"),
    ]
}
```

**Usage in tests**:

```rust
#[tokio::test]
async fn test_optimizer_with_builder_pattern() {
    // Arrange: Create examples using builder
    let trainset = vec![
        ExampleBuilder::new()
            .input("question", "What is 2+2?")
            .output("answer", "4")
            .build(),
        ExampleBuilder::new()
            .input("question", "What is 3+3?")
            .output("answer", "6")
            .build(),
    ];

    // Or use factory for common patterns
    let trainset = create_qa_trainset();

    // Act & Assert
    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(2);
    let result = optimizer.compile(&student, &trainset).await;
    assert!(result.is_ok());
}
```

---

## 4. Testing Modules, Optimizers, and Pipelines

### 4.1 Module Testing Pattern (Python DSPy)

From `tests/predict/test_predict.py`:

```python
def test_call_method():
    # Arrange
    predict_instance = Predict("input -> output")
    lm = DummyLM([{"output": "test output"}])
    dspy.configure(lm=lm)

    # Act
    result = predict_instance(input="test input")

    # Assert
    assert result.output == "test output"
```

**Key patterns**:
1. **Arrange**: Configure DummyLM, create module instance
2. **Act**: Call module with test inputs
3. **Assert**: Verify outputs match expectations

### 4.2 Optimizer Testing Pattern (Python DSPy)

From `tests/teleprompt/test_bootstrap.py`:

```python
async def test_bootstrap_fewshot_basic():
    # Arrange: Metric that accepts all
    metric = lambda example, output: example.output == output.output

    optimizer = BootstrapFewShot(
        metric=metric,
        max_bootstrapped_demos=2
    )

    trainset = [
        Example(input='Question 1', output='Answer 1').with_inputs('input'),
        Example(input='Question 2', output='Answer 2').with_inputs('input'),
    ]

    student = Predict("input -> output")

    # Act: Compile optimizer
    optimized = optimizer.compile(student, trainset)

    # Assert: Verify demonstrations were generated
    assert optimized.demonstration_count() == 2
```

**Key patterns**:
1. Define evaluation metric (lambda or function)
2. Create optimizer with configuration
3. Compile with student module and training set
4. Verify demonstration count and quality

### 4.3 Rust Implementation Recommendations

**Current optimizer test** (`ggen-ai/tests/dspy_optimizer_test.rs`):

```rust
#[tokio::test]
async fn test_bootstrap_fewshot_basic() {
    let metric = Arc::new(|_example: &Example, _output: &HashMap<String, Value>| {
        Ok(true) // Accept all
    });

    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(2);

    let trainset = vec![
        Example::new(ex1_inputs, ex1_outputs),
        Example::new(ex2_inputs, ex2_outputs),
    ];

    let student = MockPredictor::new(sig, responses);
    let optimized = optimizer.compile(&student, &trainset).await.unwrap();

    assert_eq!(optimized.demonstration_count(), 2);
}
```

**Recommended Enhancements**:

```rust
// 1. Add metric validation tests
#[tokio::test]
async fn test_metric_filters_demonstrations() {
    // Arrange: Metric that only accepts exact matches
    let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        let expected = example.outputs.get("answer");
        let actual = output.get("answer");
        Ok(expected == actual)
    });

    let optimizer = BootstrapFewShot::new(metric);

    // Create trainset with mixed quality examples
    let trainset = vec![
        qa_example("Good Q", "Good A"),
        qa_example("Bad Q", "Wrong A"),  // Will be filtered
    ];

    // Student that returns correct for first, wrong for second
    let student = DummyLM::sequential(vec![
        json!({"answer": "Good A"}).to_string(),
        json!({"answer": "Not Wrong A"}).to_string(),
    ]);

    // Act
    let optimized = optimizer.compile(&student, &trainset).await.unwrap();

    // Assert: Only 1 demonstration passed metric
    assert_eq!(optimized.demonstration_count(), 1);
}

// 2. Add demonstration quality tests
#[tokio::test]
async fn test_demonstrations_contain_valid_examples() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, Value>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(3);

    let trainset = create_qa_trainset();
    let student = create_test_predictor();

    let optimized = optimizer.compile(&student, &trainset).await.unwrap();

    // Verify each demonstration is well-formed
    for demo in optimized.demonstrations() {
        assert!(!demo.input.is_empty(), "Demo input should not be empty");
        assert!(!demo.output.is_empty(), "Demo output should not be empty");
        assert!(demo.formatted_for_signature(&optimized.signature()).is_ok());
    }
}

// 3. Add optimizer parameter tests
#[tokio::test]
async fn test_max_bootstrapped_demos_enforced() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, Value>| Ok(true));
    let max_demos = 2;
    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(max_demos);

    // Provide more training examples than max_demos
    let trainset = create_large_trainset(10);
    let student = create_test_predictor();

    let optimized = optimizer.compile(&student, &trainset).await.unwrap();

    // Should not exceed max_bootstrapped_demos
    assert!(optimized.demonstration_count() <= max_demos);
}

// 4. Add error handling tests
#[tokio::test]
async fn test_empty_trainset_returns_error() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, Value>| Ok(true));
    let optimizer = BootstrapFewShot::new(metric);
    let student = create_test_predictor();

    let result = optimizer.compile(&student, &[]).await;

    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("empty"));
}

// 5. Add metric error propagation tests
#[tokio::test]
async fn test_metric_errors_propagate() {
    let metric = Arc::new(|_: &Example, _: &HashMap<String, Value>| {
        Err(ModuleError::ValidationError("Metric failed".to_string()))
    });

    let optimizer = BootstrapFewShot::new(metric);
    let trainset = create_qa_trainset();
    let student = create_test_predictor();

    let result = optimizer.compile(&student, &trainset).await;

    assert!(result.is_err());
}
```

---

## 5. Integration vs Unit Testing Approaches

### 5.1 Python DSPy Approach

**Unit Tests** (`tests/predict/test_predict.py`):
- Test individual modules in isolation
- Use DummyLM for predictable responses
- Focus on single functionality
- Fast execution (<1s per test)

**Integration Tests** (`tests/evaluate/test_evaluate.py`):
- Test multiple components together
- May use real LM clients (conditional)
- Test end-to-end workflows
- Slower execution (may require timeouts)

**Example Integration Test Pattern**:

```python
def test_evaluate_call():
    # Integration: Evaluate + DummyLM + Predictor
    dspy.configure(lm=DummyLM({
        "What is 1+1?": {"answer": "2"},
        "What is 2+2?": {"answer": "4"}
    }))

    program = Predict("question -> answer")
    devset = [
        Example(question="What is 1+1?", answer="2").with_inputs("question"),
        Example(question="What is 2+2?", answer="4").with_inputs("question"),
    ]

    evaluator = Evaluate(
        devset=devset,
        metric=answer_exact_match,
        display_progress=False,
        num_threads=2
    )

    score = evaluator(program)
    assert score.score == 100.0
```

### 5.2 Rust Implementation - Current vs Recommended

**Current Structure**:
- Unit tests in `crates/ggen-ai/tests/dspy_*_tests.rs`
- Integration tests in `crates/ggen-ai/tests/agent_integration_tests.rs`
- Mix of property tests and example-based tests

**Recommended Test Organization**:

```rust
// crates/ggen-ai/tests/unit/mod.rs
mod signature;
mod field;
mod module;
mod optimizer;

// crates/ggen-ai/tests/integration/mod.rs
mod optimizer_pipeline;
mod evaluation_workflow;
mod multi_module_system;

// Example: Unit test for optimizer
// tests/unit/optimizer.rs
#[cfg(test)]
mod bootstrap_fewshot {
    use super::*;

    #[tokio::test]
    async fn accepts_valid_configuration() {
        let metric = Arc::new(|_: &Example, _: &Output| Ok(true));
        let optimizer = BootstrapFewShot::new(metric)
            .with_max_bootstrapped_demos(5)
            .with_max_labeled_demos(10);

        // Just verify construction succeeds
        assert!(true);
    }

    #[tokio::test]
    async fn rejects_invalid_configuration() {
        // Test error conditions
    }
}

// Example: Integration test for complete workflow
// tests/integration/optimizer_pipeline.rs
#[tokio::test]
async fn test_complete_optimization_workflow() {
    // Arrange: Full pipeline
    let dataset = load_test_dataset();
    let (trainset, devset) = split_dataset(&dataset, 0.8);

    let metric = create_exact_match_metric();
    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(5);

    let student = Predict::new(create_qa_signature());

    // Act: Compile and evaluate
    let optimized = optimizer.compile(&student, &trainset).await.unwrap();

    let evaluator = Evaluate::new(devset, metric);
    let score = evaluator.evaluate(&optimized).await.unwrap();

    // Assert: Optimization improved performance
    assert!(score > 0.5, "Optimized module should perform reasonably");
}
```

---

## 6. Error Handling and Retry Testing Patterns

### 6.1 Python DSPy Assertions Framework

DSPy provides sophisticated error handling with `dspy.Assert` and `dspy.Suggest`:

```python
# Hard assertion with retry
dspy.Assert(
    len(output.answer) > 10,
    "Answer must be at least 10 characters",
    backtrack=predictor
)

# Soft suggestion with logging
dspy.Suggest(
    output.confidence > 0.8,
    "Low confidence in answer"
)
```

**Retry Mechanism**:
- When assertion fails, DSPy backtracks to specified module
- Retry includes error message in prompt
- Continues until assertion passes or max retries reached

### 6.2 Rust Implementation Recommendation

```rust
/// Assertion configuration for DSPy modules
pub struct AssertionConfig {
    pub max_retries: usize,
    pub backtrack_module: Option<String>,
    pub error_message: String,
}

/// Assertion result
pub enum AssertionResult {
    Pass,
    FailRetry(String),  // Retry with error message
    FailHard(String),   // Stop execution
}

/// Trait for modules with assertions
#[async_trait]
pub trait AssertableModule: Module {
    /// Validate output and determine retry strategy
    async fn assert_output(
        &self,
        output: &HashMap<String, Value>,
    ) -> Result<AssertionResult, ModuleError>;

    /// Forward with retry logic
    async fn forward_with_retry(
        &self,
        inputs: HashMap<String, Value>,
        config: AssertionConfig,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        let mut attempts = 0;
        let mut last_error = None;

        while attempts < config.max_retries {
            match self.forward(inputs.clone()).await {
                Ok(output) => {
                    match self.assert_output(&output).await? {
                        AssertionResult::Pass => return Ok(output),
                        AssertionResult::FailRetry(msg) => {
                            attempts += 1;
                            last_error = Some(msg);
                            // Modify inputs to include error feedback
                            continue;
                        }
                        AssertionResult::FailHard(msg) => {
                            return Err(ModuleError::AssertionError(msg));
                        }
                    }
                }
                Err(e) => {
                    attempts += 1;
                    last_error = Some(e.to_string());
                }
            }
        }

        Err(ModuleError::MaxRetriesExceeded(
            last_error.unwrap_or_else(|| "Unknown error".to_string())
        ))
    }
}

// Test pattern for assertions
#[tokio::test]
async fn test_assertion_retry_mechanism() {
    // Arrange: Module that fails first attempt, succeeds second
    let responses = vec![
        json!({"answer": "short"}),  // Will fail min_length assertion
        json!({"answer": "This is a longer answer"}),  // Will pass
    ];

    let module = MockAssertableModule::new(responses);

    let config = AssertionConfig {
        max_retries: 3,
        backtrack_module: None,
        error_message: "Answer too short".to_string(),
    };

    // Act
    let result = module.forward_with_retry(
        HashMap::from([("question".to_string(), json!("Test?"))]),
        config
    ).await;

    // Assert
    assert!(result.is_ok());
    assert_eq!(module.call_count(), 2);  // Took 2 attempts
}

#[tokio::test]
async fn test_assertion_max_retries_exceeded() {
    // Arrange: Module that always fails assertion
    let module = MockAssertableModule::always_fails();

    let config = AssertionConfig {
        max_retries: 3,
        backtrack_module: None,
        error_message: "Always fails".to_string(),
    };

    // Act
    let result = module.forward_with_retry(inputs, config).await;

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        ModuleError::MaxRetriesExceeded(msg) => {
            assert!(msg.contains("Always fails"));
        }
        _ => panic!("Expected MaxRetriesExceeded error"),
    }
}
```

---

## 7. Performance Testing and Benchmarking

### 7.1 Python DSPy Evaluation Patterns

**Metrics System**:

```python
# Simple metric
def exact_match(example, output):
    return example.answer == output.answer

# Complex metric with scoring
def semantic_f1(example, output):
    recall = compute_recall(example.answer, output.answer)
    precision = compute_precision(example.answer, output.answer)
    f1 = 2 * (precision * recall) / (precision + recall + 1e-10)
    return f1

# Evaluation utility
evaluator = Evaluate(
    devset=devset,
    metric=semantic_f1,
    display_progress=True,
    num_threads=4  # Parallel evaluation
)

score = evaluator(program)
```

### 7.2 Rust Benchmarking Recommendations

**Add optimizer benchmarks**:

```rust
// benches/optimizer_performance.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use ggen_ai::dspy::optimizer::BootstrapFewShot;

fn benchmark_bootstrap_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("optimizer_compilation");

    for size in [10, 50, 100, 500].iter() {
        group.bench_with_input(
            BenchmarkId::new("BootstrapFewShot", size),
            size,
            |b, &size| {
                let trainset = create_trainset(size);
                let metric = create_test_metric();
                let optimizer = BootstrapFewShot::new(metric);
                let student = create_test_predictor();

                b.iter(|| {
                    let rt = tokio::runtime::Runtime::new().unwrap();
                    rt.block_on(async {
                        black_box(optimizer.compile(&student, &trainset).await)
                    })
                });
            },
        );
    }

    group.finish();
}

fn benchmark_metric_evaluation(c: &mut Criterion) {
    let mut group = c.benchmark_group("metric_evaluation");

    let example = create_test_example();
    let output = create_test_output();

    group.bench_function("exact_match", |b| {
        b.iter(|| {
            black_box(exact_match_metric(&example, &output))
        })
    });

    group.bench_function("semantic_similarity", |b| {
        b.iter(|| {
            black_box(semantic_similarity_metric(&example, &output))
        })
    });

    group.finish();
}

criterion_group!(benches, benchmark_bootstrap_compilation, benchmark_metric_evaluation);
criterion_main!(benches);
```

**SLO Targets** (from CLAUDE.md):
- Optimizer compilation: <30s for 100 examples
- Metric evaluation: <100ms per example
- Full pipeline (train + eval): <2min for moderate datasets

---

## 8. Chicago TDD Integration

### 8.1 Chicago vs London Style Review

**Chicago School (Classicist)**:
- Inside-out development
- Use real objects, avoid mocks
- State-based verification
- Higher integration confidence
- **Perfect for**: DSPy modules where behavior emerges from real interactions

**London School (Mockist)**:
- Outside-in development
- Heavy use of mocks
- Behavior-based verification
- Fast, isolated tests
- **Perfect for**: LM client interfaces, external dependencies

### 8.2 Hybrid Strategy for DSPy Testing

```rust
// Chicago style: Use real DSPy components
#[cfg(test)]
mod chicago_style_tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;

    #[test]
    fn test_signature_validation_with_real_objects() {
        // Arrange: Real Signature, real InputField, real Validator
        let signature = Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String")
                .with_min_length(5))
            .with_output(OutputField::new("answer", "The answer", "String"));

        let validator = SignatureValidator::new(signature);

        // Act: Validate with real JSON
        let valid_input = json!({"question": "What is Rust?"});
        let invalid_input = json!({"question": "Hi"});  // Too short

        // Assert: State-based verification
        assert!(validator.validate(&valid_input).is_ok());
        assert!(validator.validate(&invalid_input).is_err());
    }

    #[tokio::test]
    async fn test_optimizer_with_real_modules() {
        // Arrange: Real optimizer, real examples, real metric
        let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
            Ok(example.outputs.get("answer") == output.get("answer"))
        });

        let optimizer = BootstrapFewShot::new(metric)
            .with_max_bootstrapped_demos(3);

        let trainset = vec![
            qa_example("What is 2+2?", "4"),
            qa_example("What is 3+3?", "6"),
        ];

        // Mock only the LM client (external dependency)
        let lm = DummyLM::query_based(hashmap! {
            "2+2" => json!({"answer": "4"}).to_string(),
            "3+3" => json!({"answer": "6"}).to_string(),
        });

        let student = Predict::new_with_lm(create_qa_signature(), Arc::new(lm));

        // Act: Real compilation process
        let optimized = optimizer.compile(&student, &trainset).await.unwrap();

        // Assert: State verification
        assert_eq!(optimized.demonstration_count(), 2);
        assert!(optimized.demonstrations().iter().all(|d| !d.input.is_empty()));
    }
}

// London style: Mock external dependencies
#[cfg(test)]
mod london_style_tests {
    use super::*;
    use mockall::predicate::*;
    use mockall::*;

    #[tokio::test]
    async fn test_lm_client_interaction() {
        // Arrange: Mock LM client
        let mut mock_lm = MockLlmClient::new();
        mock_lm
            .expect_complete()
            .with(predicate::str::contains("question"))
            .times(1)
            .returning(|_| Ok("mocked answer".to_string()));

        // Act
        let result = mock_lm.complete("Test question").await;

        // Assert: Behavior verification
        assert_eq!(result.unwrap(), "mocked answer");
        // mockall automatically verifies expect_complete was called once
    }
}
```

### 8.3 Current Implementation Alignment

**Good Chicago TDD practices already in place**:

✅ `dspy_property_tests.rs`: Uses real Signature, Field, Validator objects
✅ `dspy_optimizer_test.rs`: Uses real Example, Demonstration objects
✅ `test_helpers.rs`: Factory functions for real generators with mocked LM only

**Opportunities for improvement**:

🟡 Add more integration tests with real module interactions
🟡 Reduce mocking of internal DSPy components
🟡 Expand state-based assertions over behavior verification

---

## 9. Quality Metrics and Coverage Targets

### 9.1 Python DSPy Metrics

**Built-in Metrics**:
- `answer_exact_match`: Binary correctness
- `SemanticF1`: Recall, precision, F1 score
- `CompleteAndGrounded`: Multi-aspect scoring
- Custom metrics: Small DSPy programs that check properties

**Coverage Approach**:
- Signature test suites assess adapter reliability
- Property-based testing for edge cases
- Integration tests for workflows
- Coverage excludes `conftest.py`, virtual envs

### 9.2 Rust Implementation Metrics

**Recommended Coverage Targets**:

```toml
# Add to Cargo.toml
[package.metadata.coverage]
exclude = [
    "tests/*",
    "benches/*",
    "examples/*",
]

# Target: 80% line coverage for core modules
# Target: 90% line coverage for optimizers
# Target: 70% line coverage for generators (due to LM variability)
```

**Metric Implementation**:

```rust
/// Metric trait for evaluating DSPy outputs
pub trait Metric: Send + Sync {
    fn evaluate(
        &self,
        example: &Example,
        output: &HashMap<String, Value>,
    ) -> Result<f64, MetricError>;

    fn name(&self) -> &str;
}

/// Exact match metric
pub struct ExactMatchMetric {
    field: String,
}

impl Metric for ExactMatchMetric {
    fn evaluate(
        &self,
        example: &Example,
        output: &HashMap<String, Value>,
    ) -> Result<f64, MetricError> {
        match (example.outputs.get(&self.field), output.get(&self.field)) {
            (Some(expected), Some(actual)) if expected == actual => Ok(1.0),
            (Some(_), Some(_)) => Ok(0.0),
            _ => Err(MetricError::MissingField(self.field.clone())),
        }
    }

    fn name(&self) -> &str {
        "exact_match"
    }
}

/// F1 metric (semantic similarity)
pub struct F1Metric {
    field: String,
}

impl Metric for F1Metric {
    fn evaluate(
        &self,
        example: &Example,
        output: &HashMap<String, Value>,
    ) -> Result<f64, MetricError> {
        let expected = example.outputs.get(&self.field)
            .and_then(|v| v.as_str())
            .ok_or_else(|| MetricError::MissingField(self.field.clone()))?;

        let actual = output.get(&self.field)
            .and_then(|v| v.as_str())
            .ok_or_else(|| MetricError::MissingField(self.field.clone()))?;

        // Compute token-level F1
        let expected_tokens: HashSet<&str> = expected.split_whitespace().collect();
        let actual_tokens: HashSet<&str> = actual.split_whitespace().collect();

        let intersection = expected_tokens.intersection(&actual_tokens).count() as f64;
        let precision = intersection / actual_tokens.len() as f64;
        let recall = intersection / expected_tokens.len() as f64;

        if precision + recall == 0.0 {
            Ok(0.0)
        } else {
            Ok(2.0 * precision * recall / (precision + recall))
        }
    }

    fn name(&self) -> &str {
        "f1"
    }
}

// Test metrics
#[cfg(test)]
mod metric_tests {
    use super::*;

    #[test]
    fn exact_match_returns_1_for_identical() {
        let metric = ExactMatchMetric { field: "answer".to_string() };
        let example = qa_example("Q", "Same answer");
        let output = hashmap! { "answer" => json!("Same answer") };

        assert_eq!(metric.evaluate(&example, &output).unwrap(), 1.0);
    }

    #[test]
    fn exact_match_returns_0_for_different() {
        let metric = ExactMatchMetric { field: "answer".to_string() };
        let example = qa_example("Q", "Expected");
        let output = hashmap! { "answer" => json!("Actual") };

        assert_eq!(metric.evaluate(&example, &output).unwrap(), 0.0);
    }

    #[test]
    fn f1_computes_token_overlap() {
        let metric = F1Metric { field: "answer".to_string() };
        let example = qa_example("Q", "the quick brown fox");
        let output = hashmap! { "answer" => json!("the brown dog") };

        let score = metric.evaluate(&example, &output).unwrap();
        // Expected: 2 tokens overlap (the, brown) out of 4 expected, 3 actual
        // Precision: 2/3, Recall: 2/4, F1: 2 * (2/3 * 2/4) / (2/3 + 2/4) = 0.571
        assert!((score - 0.571).abs() < 0.01);
    }
}
```

---

## 10. Testing Improvements Roadmap

### Phase 1: Foundation (Immediate - 1-2 weeks)

**Priority 1: Enhanced DummyLM**
- [ ] Implement `DummyLM` with 3 modes (sequential, query-based, example-following)
- [ ] Add call count tracking
- [ ] Add interaction history
- [ ] Write unit tests for each mode

**Priority 2: Test Data Builders**
- [ ] Implement `ExampleBuilder` with fluent API
- [ ] Add factory functions for common example patterns (QA, classification, etc.)
- [ ] Create `create_trainset()` helpers for standard datasets

**Priority 3: Test Organization**
- [ ] Split tests into `unit/` and `integration/` directories
- [ ] Add `conftest`-equivalent with common fixtures
- [ ] Implement test categorization (reliability, llm_call, etc.)

### Phase 2: Optimizer Testing (2-4 weeks)

**Priority 1: Comprehensive Optimizer Tests**
- [ ] Add metric validation tests
- [ ] Add demonstration quality tests
- [ ] Add parameter enforcement tests
- [ ] Add error handling tests

**Priority 2: Metrics System**
- [ ] Implement `Metric` trait
- [ ] Add `ExactMatchMetric`
- [ ] Add `F1Metric`
- [ ] Add `SemanticSimilarityMetric` (if needed)

**Priority 3: Evaluation Utilities**
- [ ] Implement `Evaluate` struct
- [ ] Add parallel evaluation support
- [ ] Add progress reporting
- [ ] Add result aggregation

### Phase 3: Advanced Testing (4-6 weeks)

**Priority 1: Assertion/Retry Mechanism**
- [ ] Implement `AssertableModule` trait
- [ ] Add `forward_with_retry` method
- [ ] Add assertion configuration
- [ ] Write retry behavior tests

**Priority 2: Golden Tests**
- [ ] Implement golden test framework
- [ ] Add baseline comparison utilities
- [ ] Create regression test suite
- [ ] Add baseline update tooling

**Priority 3: Performance Benchmarks**
- [ ] Add optimizer compilation benchmarks
- [ ] Add metric evaluation benchmarks
- [ ] Add end-to-end pipeline benchmarks
- [ ] Document SLO targets

### Phase 4: Quality & Polish (Ongoing)

**Priority 1: Coverage Improvement**
- [ ] Achieve 80% line coverage for core modules
- [ ] Achieve 90% line coverage for optimizers
- [ ] Add missing edge case tests
- [ ] Add property-based tests for complex logic

**Priority 2: Documentation**
- [ ] Document testing patterns in module docs
- [ ] Add test examples to API documentation
- [ ] Create testing guide in docs/
- [ ] Document metric implementations

**Priority 3: CI/CD Integration**
- [ ] Add test categorization to CI
- [ ] Add coverage reporting
- [ ] Add benchmark regression detection
- [ ] Add integration test gating

---

## 11. Key Takeaways

### What to Adopt from DSPy

1. **Multi-mode DummyLM**: Essential for realistic testing without LM calls
2. **Test Organization**: Mirror structure aids navigation and maintenance
3. **Example Builder Pattern**: Reduces boilerplate, improves readability
4. **Evaluation Utilities**: Standardized evaluation across projects
5. **Assertion/Retry Pattern**: Sophisticated error handling worth implementing
6. **Metrics System**: Flexible, composable evaluation framework

### What to Keep from Current Rust Implementation

1. **Property-based testing**: Excellent edge case coverage with proptest
2. **Chicago TDD patterns**: Real objects, state-based verification
3. **Test helpers**: Factory functions reduce duplication
4. **AAA pattern**: Clear, consistent test structure
5. **Integration tests**: Good coverage of realistic workflows

### What Makes Rust Different

1. **Type Safety**: Compiler catches many issues Python tests would catch
2. **No Mocks Needed**: Use real objects more often (Chicago TDD advantage)
3. **Async Complexity**: Requires careful handling of async test execution
4. **Performance**: Can run property tests with thousands of cases efficiently
5. **Compile-Time Guarantees**: Fewer runtime error tests needed

---

## 12. Conclusion

The Python DSPy testing architecture provides excellent patterns for comprehensive LLM framework testing. The current Rust implementation has a solid foundation with property-based testing and Chicago TDD patterns. Key improvements should focus on:

1. **Enhanced mocking** with multi-mode `DummyLM`
2. **Comprehensive optimizer testing** with metrics validation
3. **Test data builders** for reduced boilerplate
4. **Assertion/retry mechanisms** for sophisticated error handling
5. **Performance benchmarking** for SLO validation

By adopting these patterns while preserving Rust's type safety and the Chicago TDD philosophy, ggen can achieve robust, maintainable testing for its DSPy implementation.

---

## Sources

### DSPy Testing Documentation
- [DSPy Official Documentation](https://dspy.ai/)
- [DSPy GitHub Repository](https://github.com/stanfordnlp/dspy)
- [DSPy Testing with Autoblocks](https://www.autoblocks.ai/blog/collaboratively-test-evaluate-your-dspy-app)
- [DSPy Metrics Documentation](https://dspy.ai/learn/evaluation/metrics/)
- [DSPy Evaluation Overview](https://dspy.ai/learn/evaluation/overview/)
- [DSPy Assertions Paper](https://arxiv.org/html/2312.13382v2)

### Chicago TDD and Testing Best Practices
- [Chicago vs London TDD](https://medium.com/geekculture/london-vs-chicago-in-tdd-77067077d0cc)
- [AAA Pattern in Unit Testing](https://medium.com/@pjbgf/title-testing-code-ocd-and-the-aaa-pattern-df453975ab80)
- [When to Mock](https://enterprisecraftsmanship.com/posts/when-to-mock/)
- [Chicago and London Schools of TDD](https://chuniversiteit.nl/programming/chicago-and-london-schools-of-tdd)

### Python Testing Frameworks
- [pytest Documentation](https://docs.pytest.org/)
- [Hypothesis Property Testing](https://www.python4data.science/en/latest/clean-prep/hypothesis.html)
- [pytest with Hypothesis](https://pytest-with-eric.com/pytest-advanced/hypothesis-testing-python/)

### Golden Tests and Regression Testing
- [Golden Tests in AI](https://www.shaped.ai/blog/golden-tests-in-ai)
- [Snapshot Testing](https://www.checklyhq.com/docs/browser-checks/visual-regression-snapshot-testing/)

### DSPy GitHub Test Files Referenced
- [test_predict.py](https://github.com/stanfordnlp/dspy/blob/main/tests/predict/test_predict.py)
- [test_bootstrap.py](https://github.com/stanfordnlp/dspy/blob/main/tests/teleprompt/test_bootstrap.py)
- [test_evaluate.py](https://github.com/stanfordnlp/dspy/blob/main/tests/evaluate/test_evaluate.py)
- [test_example.py](https://github.com/stanfordnlp/dspy/blob/main/tests/primitives/test_example.py)
- [dummies.py](https://github.com/stanfordnlp/dspy/blob/main/dspy/utils/dummies.py)
- [conftest.py](https://github.com/stanfordnlp/dspy/blob/main/tests/conftest.py)
- [pyproject.toml](https://github.com/stanfordnlp/dspy/blob/main/pyproject.toml)
