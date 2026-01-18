# DSPy Assertion System - Rust Implementation Plan for ggen

**Date:** 2026-01-11
**Status:** Design Phase
**Epic:** EPIC 9 Candidate (Parallel Agent Infrastructure)

---

## Executive Summary

This document outlines the implementation plan for a DSPy-style assertion system in Rust for the ggen project. The system will enable LLM agents to self-correct through backtracking, retry mechanisms, and dynamic prompt modification based on validation failures.

**Key Innovation:** Transform validation failures into learning opportunities through introspective feedback loops.

**Strategic Alignment:**
- Leverages ggen's RDF-based specification system
- Integrates with existing DSPy optimizer infrastructure
- Follows constitutional rules (Result<T,E>, no unwrap, type-first)
- Supports EPIC 9 parallel agent execution patterns

---

## Current State Analysis

### Existing Infrastructure

The ggen-ai crate already has robust validation infrastructure:

**File:** `/home/user/ggen/crates/ggen-ai/src/dspy/validation_error.rs`
- `ValidationError` and `ValidationErrorDetail` types
- `ValidationErrorType` enum (11 error types)
- Comprehensive error aggregation and reporting
- JSON serialization support

**File:** `/home/user/ggen/crates/ggen-ai/src/dspy/signature_validator.rs`
- `SignatureValidator` for runtime constraint checking
- Supports: required fields, type checking, string/array constraints, enum values, regex patterns
- Comprehensive test coverage (>90%)

**File:** `/home/user/ggen/crates/ggen-ai/src/dspy/optimizer.rs`
- `BootstrapFewShot` optimizer implementation
- Example-based learning infrastructure
- Metric-driven optimization

**File:** `/home/user/ggen/crates/ggen-ai/src/microframework/agents.rs`
- Agent trait system (`MicroAgent`)
- Pre-built agents (CodeGen, Tester, Reviewer, Validator, etc.)
- Task-based execution model

### Gap Analysis

**What We Have:**
✓ Validation error types and reporting
✓ Input constraint validation
✓ DSPy optimizer infrastructure
✓ Agent execution framework
✓ Result<T,E> error handling throughout

**What We Need:**
✗ Backtracking/retry mechanism
✗ Dynamic prompt modification with feedback
✗ Assertion severity levels (Assert vs Suggest)
✗ Output validation (currently only input validation)
✗ Integration with optimizer for assertion-driven bootstrapping
✗ Telemetry for retry attempts
✗ TTL specification for assertions

---

## Design Overview

### Architecture Components

```
┌─────────────────────────────────────────────────────────────┐
│                     Assertion System                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────┐      ┌──────────────┐     ┌───────────┐  │
│  │  Validator   │──────│  Assertion   │─────│  Retry    │  │
│  │    Trait     │      │   Builder    │     │  Strategy │  │
│  └──────────────┘      └──────────────┘     └───────────┘  │
│         │                      │                    │       │
│         │                      │                    │       │
│  ┌──────▼──────────────────────▼────────────────────▼────┐ │
│  │           BacktrackExecutor<M: Module>               │ │
│  │  • Executes module with retry logic                  │ │
│  │  • Manages attempt state                             │ │
│  │  • Modifies prompts with feedback                    │ │
│  │  • Handles Assert vs Suggest termination             │ │
│  └──────────────────────────────────┬───────────────────┘ │
│                                     │                      │
│                      ┌──────────────▼──────────────┐       │
│                      │     Telemetry Observer      │       │
│                      │  • Logs retry attempts      │       │
│                      │  • Tracks validation stats  │       │
│                      └─────────────────────────────┘       │
└─────────────────────────────────────────────────────────────┘
                               │
                               │ Integration Points
                               │
    ┌──────────────────────────┼──────────────────────────┐
    │                          │                          │
┌───▼────────┐      ┌──────────▼──────┐      ┌──────────▼─────┐
│  DSPy      │      │   Microframework │      │   RDF Specs    │
│ Optimizer  │      │     Agents       │      │  (.specify/)   │
└────────────┘      └──────────────────┘      └────────────────┘
```

### Type System

```rust
// Core validation types (extend existing)
pub trait Validator<T>: Send + Sync {
    fn validate(&self, output: &T) -> ValidationResult;
}

pub enum ValidationResult {
    Valid,
    Invalid { feedback: String },
}

// Assertion configuration
pub struct Assertion<T> {
    validator: Box<dyn Validator<T>>,
    level: AssertionLevel,
    config: BacktrackConfig,
    feedback: Option<String>,
}

pub enum AssertionLevel {
    Assert,  // Hard constraint
    Suggest, // Soft constraint
}

pub struct BacktrackConfig {
    max_attempts: usize,
    strategy: RetryStrategy,
}

// Integration with Module trait
pub trait Module {
    type Input;
    type Output;
    type Error;

    fn execute(&mut self, input: Self::Input) -> Result<Self::Output, Self::Error>;
    fn id(&self) -> ModuleId;
}

pub struct AssertedModule<M: Module> {
    inner: M,
    assertions: Vec<Assertion<M::Output>>,
}

// Retry execution
pub struct BacktrackExecutor<M: Module> {
    module: AssertedModule<M>,
    telemetry: Arc<dyn AssertionObserver>,
}

impl<M: Module> BacktrackExecutor<M> {
    pub async fn execute(&mut self, input: M::Input)
        -> Result<M::Output, AssertionError>;
}
```

---

## Implementation Phases

### Phase 1: Core Assertion Infrastructure (Week 1-2)

**Goal:** Implement basic assertion types and validation framework

**Tasks:**
1. Create `crates/ggen-ai/src/dspy/assertion.rs`
   - `Validator<T>` trait
   - `ValidationResult` enum
   - `Assertion<T>` builder
   - `AssertionLevel` enum

2. Create `crates/ggen-ai/src/dspy/backtrack.rs`
   - `BacktrackConfig` struct
   - `RetryStrategy` enum
   - `RetryContext` for state management

3. Extend existing `ValidationError` types
   - Add `AssertionError` variant
   - Add `SuggestionWarning` type
   - Support retry attempt metadata

4. Create `crates/ggen-ai/src/dspy/validators/mod.rs`
   - Standard validators (length, pattern, format)
   - Combinator validators (AND, OR, NOT)
   - Custom validator support

**Deliverables:**
- [ ] Core types implemented
- [ ] Unit tests (chicago-tdd pattern)
- [ ] Documentation with examples
- [ ] Integration with existing `ValidationError`

**Receipt Target:**
```
[Receipt] Phase 1 Complete
  Files: 4 new, 2 modified
  Tests: 47 passing, 0 failing, <16s
  Coverage: >85%
  Clippy: 0 warnings
  cargo make check: ✓ <5s
```

---

### Phase 2: Module Integration & Retry Logic (Week 3-4)

**Goal:** Implement backtracking executor and module wrapper

**Tasks:**
1. Create `crates/ggen-ai/src/dspy/asserted_module.rs`
   - `AssertedModule<M>` wrapper
   - Assertion registration API
   - Output validation pipeline

2. Create `crates/ggen-ai/src/dspy/executor.rs`
   - `BacktrackExecutor<M>` implementation
   - Retry loop logic
   - State management across attempts
   - Termination condition handling

3. Implement prompt modification
   - `RetryPrompt<T>` type
   - Dynamic signature extension
   - Feedback injection mechanism

4. Add to existing Module trait
   - Extend `dspy::module::Module` trait
   - Add `execute_with_retry` method
   - Support for retry context

**Deliverables:**
- [ ] Module wrapper complete
- [ ] Retry logic functional
- [ ] Prompt modification working
- [ ] Integration tests passing

**Receipt Target:**
```
[Receipt] Phase 2 Complete
  Integration tests: 23 passing
  Retry scenarios: 6/6 working
  Assert/Suggest: both modes functional
  cargo make test: ✓ <30s
```

---

### Phase 3: Optimizer Integration (Week 5-6)

**Goal:** Enable assertion-driven example bootstrapping

**Tasks:**
1. Extend `crates/ggen-ai/src/dspy/optimizer.rs`
   - Add assertion support to `BootstrapFewShot`
   - Implement assertion-driven bootstrapping
   - Counterexample collection

2. Create `crates/ggen-ai/src/dspy/counterexample.rs`
   - `CounterExample` type
   - Failure trace capture
   - Integration with few-shot prompts

3. Add assertion metrics
   - `AssertionMetric` for scoring
   - Integration with existing metric system
   - Performance tracking

4. Update examples
   - Extend `dspy_bootstrap_fewshot.rs` example
   - Add assertion-driven optimization example

**Deliverables:**
- [ ] Optimizer supports assertions
- [ ] Counterexample bootstrapping working
- [ ] Examples demonstrate feature
- [ ] Benchmarks show improvement

**Receipt Target:**
```
[Receipt] Phase 3 Complete
  Optimizer tests: 15 passing
  Counterexample capture: ✓
  Assertion metrics: integrated
  Example runtime: ✓ <2min
  Performance: +25% reliability (benchmark)
```

---

### Phase 4: Telemetry & Observability (Week 7)

**Goal:** Add comprehensive logging and debugging support

**Tasks:**
1. Create `crates/ggen-ai/src/dspy/telemetry.rs`
   - `AssertionObserver` trait
   - `AssertionTelemetry` type
   - `AttemptRecord` for each retry

2. Implement observers
   - `LoggingObserver` (tracing integration)
   - `MetricsObserver` (Prometheus/OpenTelemetry)
   - `DebugObserver` (detailed retry traces)

3. Add structured logging
   - Log all retry attempts
   - Track validation failures
   - Performance metrics

4. Create debugging tools
   - Replay retry sequences
   - Visualization of backtracking
   - Analysis tools

**Deliverables:**
- [ ] Telemetry system complete
- [ ] Multiple observers implemented
- [ ] Debugging tools functional
- [ ] Documentation updated

**Receipt Target:**
```
[Receipt] Phase 4 Complete
  Observers: 3 implementations
  Log coverage: 100% of retry paths
  Debug tools: 4 utilities
  Overhead: <5% (benchmark)
```

---

### Phase 5: TTL Specification Support (Week 8-9)

**Goal:** Enable RDF-based assertion specification

**Tasks:**
1. Design RDF vocabulary
   - Create `.specify/assertions.ttl` ontology
   - Define assertion properties
   - Validator type registry

2. Extend `crates/ggen-ai/src/codegen/` parsers
   - Parse assertion specs from TTL
   - Generate validator code
   - Generate assertion builders

3. Create `crates/ggen-ai/src/dspy/assertion_codegen.rs`
   - TTL → Rust assertion translation
   - Template-based code generation
   - Type-safe builder generation

4. Update `ggen sync` command
   - Include assertion generation
   - Validate assertion specs
   - Generate documentation

**Deliverables:**
- [ ] TTL vocabulary defined
- [ ] Code generation working
- [ ] `ggen sync` integration complete
- [ ] Examples use TTL specs

**Receipt Target:**
```
[Receipt] Phase 5 Complete
  TTL ontology: validated via SHACL
  Codegen tests: 12 passing
  ggen sync: includes assertions
  Generated code: compiles ✓
```

---

### Phase 6: Performance & Polish (Week 10)

**Goal:** Optimize, document, and prepare for production

**Tasks:**
1. Performance optimization
   - Parallel validation (Rayon)
   - Cached validators
   - Async retry execution

2. Documentation
   - API documentation (rustdoc)
   - Tutorial: "Assertions in ggen"
   - Migration guide for existing code

3. Examples and templates
   - Real-world assertion patterns
   - Best practices guide
   - Common validator library

4. Production readiness
   - Error message quality review
   - Edge case handling
   - SLO compliance verification

**Deliverables:**
- [ ] Performance benchmarks pass
- [ ] Documentation complete
- [ ] Examples comprehensive
- [ ] Production checklist satisfied

**Receipt Target:**
```
[Receipt] Phase 6 Complete
  Benchmarks: validation <5ms p95
  Documentation: 100% API coverage
  Examples: 8 comprehensive demos
  cargo make pre-commit: ✓ <2min
  SLO compliance: check <5s, test <30s, lint <60s
```

---

## Integration with Existing Systems

### 1. DSPy Optimizer Integration

**Current State:**
```rust
// crates/ggen-ai/examples/dspy_bootstrap_fewshot.rs
let optimizer = BootstrapFewShot::new(metric)
    .with_max_bootstrapped_demos(3)
    .with_max_labeled_demos(10);

let optimized = optimizer.compile(&student, &trainset).await?;
```

**With Assertions:**
```rust
use ggen_ai::dspy::assertion::{Assertion, validators};

// Define assertions for teacher model
let teacher = student.clone()
    .with_assertion(
        Assertion::suggest(validators::length_between::<10, 200>())
            .with_feedback("Answer should be detailed (10-200 chars)")
            .max_retries(3)
    );

// Optimizer uses assertions during bootstrapping
let optimizer = BootstrapFewShot::new(metric)
    .with_max_bootstrapped_demos(3)
    .with_max_labeled_demos(10)
    .with_assertion_driven_bootstrapping(true); // NEW

// Compiled student includes counterexamples
let optimized = optimizer.compile_with_assertions(&teacher, &trainset).await?;
```

### 2. Microframework Agent Integration

**Current State:**
```rust
// crates/ggen-ai/src/microframework/agents.rs
#[async_trait]
impl MicroAgent for CodeGenAgent {
    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        // Generate code
        Ok(TaskResult::success(task.id.clone(), output, duration))
    }
}
```

**With Assertions:**
```rust
use ggen_ai::dspy::assertion::{Assertion, validators};

impl CodeGenAgent {
    pub fn with_code_validation(mut self) -> Self {
        // Add assertion to validate generated code
        self.assertions.push(
            Assertion::assert(validators::rust_syntax_valid())
                .with_feedback("Generated code must be valid Rust syntax")
                .max_retries(5)
        );
        self
    }
}

#[async_trait]
impl MicroAgent for CodeGenAgent {
    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let executor = BacktrackExecutor::new(self.code_module.clone());

        match executor.execute_with_assertions(task).await {
            Ok(output) => Ok(TaskResult::success(task.id.clone(), output, duration)),
            Err(e) => Ok(TaskResult::failure(task.id.clone(), e.to_string()))
        }
    }
}
```

### 3. RDF Specification Integration

**TTL Specification:**
```turtle
# .specify/assertions/code_generation_assertions.ttl
@prefix ggen: <https://ggen.tech/ontology#> .
@prefix assert: <https://ggen.tech/ontology/assertions#> .

:CodeGenModule a ggen:Module ;
    ggen:name "code_generator" ;
    assert:hasAssertion :SyntaxValidation, :LengthConstraint .

:SyntaxValidation a assert:Assertion ;
    assert:level assert:Assert ;
    assert:validator :RustSyntaxValidator ;
    assert:maxRetries 5 ;
    assert:feedback "Generated code must be valid Rust syntax" .

:RustSyntaxValidator a assert:Validator ;
    assert:validatorType "rust_syntax" ;
    assert:usesCompiler true .

:LengthConstraint a assert:Assertion ;
    assert:level assert:Suggest ;
    assert:validator :LengthValidator ;
    assert:maxRetries 3 ;
    assert:feedback "Code should be concise but complete" .

:LengthValidator a assert:Validator ;
    assert:validatorType "length_between" ;
    assert:minLength 10 ;
    assert:maxLength 1000 .
```

**Generated Rust Code:**
```rust
// Generated by `ggen sync` from .specify/assertions/code_generation_assertions.ttl

use ggen_ai::dspy::assertion::{Assertion, validators};

pub fn create_code_gen_assertions() -> Vec<Assertion<String>> {
    vec![
        // SyntaxValidation
        Assertion::assert(validators::rust_syntax())
            .with_feedback("Generated code must be valid Rust syntax")
            .max_retries(5),

        // LengthConstraint
        Assertion::suggest(validators::length_between::<10, 1000>())
            .with_feedback("Code should be concise but complete")
            .max_retries(3),
    ]
}
```

---

## Standard Validator Library

Create a comprehensive validator library in `crates/ggen-ai/src/dspy/validators/`:

```rust
pub mod validators {
    // Length validators
    pub fn length_between<const MIN: usize, const MAX: usize>()
        -> LengthValidator;
    pub fn min_length<const MIN: usize>() -> MinLengthValidator;
    pub fn max_length<const MAX: usize>() -> MaxLengthValidator;

    // Format validators
    pub fn matches_regex(pattern: &str) -> Result<RegexValidator, regex::Error>;
    pub fn json_valid() -> JsonValidator;
    pub fn yaml_valid() -> YamlValidator;
    pub fn toml_valid() -> TomlValidator;

    // Code validators
    pub fn rust_syntax() -> RustSyntaxValidator;
    pub fn rust_compiles() -> RustCompileValidator;
    pub fn python_syntax() -> PythonSyntaxValidator;

    // Content validators
    pub fn not_empty() -> NotEmptyValidator;
    pub fn contains_keywords(keywords: Vec<&str>) -> KeywordValidator;
    pub fn unique_items() -> UniquenessValidator;

    // Logical combinators
    pub fn all<T>(validators: Vec<Box<dyn Validator<T>>>) -> AllValidator<T>;
    pub fn any<T>(validators: Vec<Box<dyn Validator<T>>>) -> AnyValidator<T>;
    pub fn not<T>(validator: Box<dyn Validator<T>>) -> NotValidator<T>;

    // Custom validator wrapper
    pub fn custom<T, F>(f: F) -> CustomValidator<T, F>
    where
        F: Fn(&T) -> ValidationResult + Send + Sync;
}
```

---

## Testing Strategy

### Unit Tests (Chicago TDD Pattern)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;

    #[test]
    fn test_assertion_builder_pattern() {
        // Arrange
        let validator = validators::length_between::<10, 100>();

        // Act
        let assertion = Assertion::assert(validator)
            .with_feedback("Output must be 10-100 characters")
            .max_retries(3);

        // Assert
        assert_eq!(assertion.level, AssertionLevel::Assert);
        assert_eq!(assertion.config.max_attempts, 3);
    }

    #[test]
    fn test_backtrack_executor_success_first_try() {
        // Arrange
        let mut module = ValidOutputModule::new();
        let assertion = Assertion::assert(validators::not_empty())
            .max_retries(3);
        let mut executor = BacktrackExecutor::new(
            AssertedModule::new(module).with_assertion(assertion)
        );

        // Act
        let result = executor.execute("input".to_string());

        // Assert
        assert!(result.is_ok());
        assert_eq!(module.call_count(), 1); // Only called once
    }

    #[test]
    fn test_backtrack_executor_retries_then_succeeds() {
        // Arrange
        let mut module = FailThenSucceedModule::new(2); // Fail 2 times, then succeed
        let assertion = Assertion::assert(validators::not_empty())
            .max_retries(5);
        let mut executor = BacktrackExecutor::new(
            AssertedModule::new(module).with_assertion(assertion)
        );

        // Act
        let result = executor.execute("input".to_string());

        // Assert
        assert!(result.is_ok());
        assert_eq!(module.call_count(), 3); // Failed twice, succeeded third time
    }

    #[test]
    fn test_assert_fails_after_max_retries() {
        // Arrange
        let mut module = AlwaysInvalidModule::new();
        let assertion = Assertion::assert(validators::not_empty())
            .max_retries(3);
        let mut executor = BacktrackExecutor::new(
            AssertedModule::new(module).with_assertion(assertion)
        );

        // Act
        let result = executor.execute("input".to_string());

        // Assert
        assert!(result.is_err());
        assert_eq!(module.call_count(), 4); // Initial + 3 retries
        assert!(matches!(result.unwrap_err(), AssertionError::AssertionFailed { .. }));
    }

    #[test]
    fn test_suggest_logs_warning_after_max_retries() {
        // Arrange
        let mut module = AlwaysInvalidModule::new();
        let assertion = Assertion::suggest(validators::length_between::<10, 100>())
            .max_retries(2);
        let mut executor = BacktrackExecutor::new(
            AssertedModule::new(module).with_assertion(assertion)
        );

        // Act
        let result = executor.execute("input".to_string());

        // Assert
        assert!(result.is_ok()); // Suggest doesn't fail
        // Verify warning was logged (check telemetry)
    }
}
```

### Property Tests

```rust
#[cfg(test)]
mod proptests {
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn backtrack_always_terminates(max_retries in 1usize..10) {
            let module = AlwaysFailingModule::new();
            let assertion = Assertion::assert(validators::always_invalid())
                .max_retries(max_retries);
            let mut executor = BacktrackExecutor::new(
                AssertedModule::new(module).with_assertion(assertion)
            );

            let result = executor.execute("input".to_string());

            prop_assert!(result.is_err());
            prop_assert_eq!(module.call_count(), max_retries + 1);
        }

        #[test]
        fn validation_result_deterministic(input: String) {
            let validator = validators::length_between::<5, 50>();

            let result1 = validator.validate(&input);
            let result2 = validator.validate(&input);

            prop_assert_eq!(result1, result2);
        }
    }
}
```

### Integration Tests

```rust
// crates/ggen-ai/tests/assertion_integration_tests.rs

#[tokio::test]
async fn test_end_to_end_assertion_workflow() {
    // Create a predictor with assertions
    let signature = Signature::new("QA", "Question Answering")
        .with_input(InputField::new("question", "Question", "String"))
        .with_output(OutputField::new("answer", "Answer", "String"));

    let predictor = Predictor::new(signature)
        .with_assertion(
            Assertion::assert(validators::not_empty())
                .with_feedback("Answer cannot be empty")
                .max_retries(3)
        )
        .with_assertion(
            Assertion::suggest(validators::length_between::<20, 500>())
                .with_feedback("Answer should be detailed")
                .max_retries(2)
        );

    // Execute with mock LLM
    let result = predictor.forward(json!({
        "question": "What is Rust?"
    })).await;

    assert!(result.is_ok());
    // Verify assertions were checked
    // Verify telemetry was recorded
}

#[tokio::test]
async fn test_assertion_driven_bootstrapping() {
    // Create training data
    let trainset = vec![/* examples */];

    // Create teacher with assertions
    let teacher = create_predictor_with_assertions();

    // Optimize with assertion-driven bootstrapping
    let optimizer = BootstrapFewShot::new(metric)
        .with_assertion_driven_bootstrapping(true);

    let student = optimizer.compile_with_assertions(&teacher, &trainset).await?;

    // Verify counterexamples were collected
    assert!(student.has_counterexamples());

    // Verify student performs better with assertions
    let performance = evaluate(&student, &testset).await?;
    assert!(performance.accuracy > 0.8);
}
```

---

## Performance Targets

Based on ggen SLO requirements:

| Operation | Target | Measurement |
|-----------|--------|-------------|
| Validation (single) | <5ms | p95 latency |
| Backtrack retry | <500ms | per attempt (including LLM) |
| Assertion compile | <2s | TTL → Rust generation |
| Full suite tests | <30s | `cargo make test` |
| Pre-commit checks | <2min | `cargo make pre-commit` |

---

## Risk Mitigation

### Risk 1: Retry Explosion

**Risk:** Multiple assertions with high retry counts = exponential cost

**Mitigation:**
- Default max_retries=2 (aligned with DSPy)
- Warn on total retry budget >10
- Circuit breaker pattern for repeated failures
- Telemetry to identify problematic assertions

### Risk 2: Validation Overhead

**Risk:** Validation adds latency to every module execution

**Mitigation:**
- Parallel validation where possible (Rayon)
- Cached validators for deterministic checks
- Lightweight validators (regex, length) prioritized
- Heavy validators (compilation) only when needed

### Risk 3: Prompt Bloat

**Risk:** Adding past outputs + feedback inflates prompt tokens

**Mitigation:**
- Limit past_outputs history (default: last 2)
- Summarize feedback instead of full error messages
- Truncate long outputs in retry context
- Monitor token usage via telemetry

### Risk 4: Complexity Creep

**Risk:** Assertion system becomes too complex to use effectively

**Mitigation:**
- Start with simple validators (length, not_empty)
- Provide standard library of common validators
- Comprehensive documentation and examples
- TTL code generation reduces manual complexity

---

## Success Criteria

### Functional Requirements

- [ ] Assert and Suggest levels both functional
- [ ] Backtracking with max_retries enforcement
- [ ] Dynamic prompt modification with feedback
- [ ] Integration with BootstrapFewShot optimizer
- [ ] Counterexample collection working
- [ ] TTL specification → code generation
- [ ] Telemetry for all retry attempts

### Non-Functional Requirements

- [ ] Zero `unwrap/expect` in production code
- [ ] All APIs return `Result<T, E>`
- [ ] Test coverage >85%
- [ ] Clippy passes with `-D warnings`
- [ ] SLO compliance: check <5s, test <30s, lint <60s
- [ ] Documentation coverage 100% (public APIs)

### Performance Requirements

- [ ] Validation overhead <5% of total execution time
- [ ] Backtrack latency <500ms per retry
- [ ] Parallel validation speedup >2x for 4+ assertions
- [ ] Memory overhead <10MB per executor

### Quality Requirements

- [ ] Chicago TDD pattern for all tests
- [ ] Property tests for retry logic
- [ ] Integration tests cover end-to-end workflows
- [ ] Error messages actionable for LLM self-correction

---

## Next Steps

### Immediate Actions (Week 1)

1. **Review & Approve Design**
   - Stakeholder review of this document
   - Identify any missing requirements
   - Confirm EPIC 9 alignment

2. **Set Up Infrastructure**
   - Create feature branch: `feature/dspy-assertion-system`
   - Set up CI/CD for new modules
   - Create initial file structure

3. **Spike: Proof of Concept**
   - Implement minimal `Validator` trait
   - Create simple `BacktrackExecutor`
   - Demonstrate retry with feedback injection
   - Measure performance overhead

### Weekly Milestones

| Week | Phase | Key Deliverable | Receipt |
|------|-------|-----------------|---------|
| 1-2 | Phase 1 | Core types & validators | 47 tests passing |
| 3-4 | Phase 2 | Backtrack executor working | 23 integration tests |
| 5-6 | Phase 3 | Optimizer integration | +25% reliability |
| 7 | Phase 4 | Telemetry complete | 3 observers impl |
| 8-9 | Phase 5 | TTL codegen working | ggen sync ✓ |
| 10 | Phase 6 | Production ready | All SLOs pass |

---

## References

### Documentation
- **Full Analysis:** `/home/user/ggen/docs/dspy-assertion-system-analysis.md`
- **Quick Reference:** `/home/user/ggen/docs/dspy-assertion-quick-reference.md`
- **CLAUDE.md:** `/home/user/ggen/CLAUDE.md` (constitutional rules)

### Existing Code
- **Validation:** `/home/user/ggen/crates/ggen-ai/src/dspy/validation_error.rs`
- **Validators:** `/home/user/ggen/crates/ggen-ai/src/dspy/signature_validator.rs`
- **Optimizer:** `/home/user/ggen/crates/ggen-ai/src/dspy/optimizer.rs`
- **Agents:** `/home/user/ggen/crates/ggen-ai/src/microframework/agents.rs`

### External Resources
- **DSPy Assertions:** https://dspy.ai/learn/programming/7-assertions/
- **Research Paper:** https://arxiv.org/abs/2312.13382
- **Tutorial:** https://learnbybuilding.ai/tutorial/guiding-llm-output-with-dspy-assertions-and-suggestions/

---

## Approval Checklist

- [ ] Design reviewed by technical lead
- [ ] Alignment with ggen constitutional rules confirmed
- [ ] EPIC 9 integration points identified
- [ ] Resource allocation approved (10 weeks)
- [ ] Dependencies on other work streams resolved
- [ ] Risk mitigation strategies accepted
- [ ] Success criteria agreed upon
- [ ] Implementation timeline approved

**Approved by:** ________________
**Date:** ________________

---

**End of Implementation Plan**
