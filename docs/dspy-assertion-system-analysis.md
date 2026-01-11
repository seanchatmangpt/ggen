# DSPy Assertion System Research

**Research Date:** 2026-01-11
**Status:** Deprecated in DSPy 2.6 (replaced by `dspy.Refine` and `dspy.BestOfN`)

## Executive Summary

DSPy Assertions represent a computational constraints system for self-refining language model pipelines. The system enables automated enforcement of validation rules through backtracking, retry mechanisms, and dynamic prompt modification. While deprecated in DSPy 2.6, the design patterns offer valuable insights for Rust implementation of LLM agent reliability systems.

**Key Innovation:** Unlike traditional Python assertions that halt execution, DSPy assertions trigger sophisticated retry mechanisms with introspective feedback, enabling models to self-refine their outputs.

**Performance Impact:** Assertions improved rule compliance by up to 164% and response quality by up to 37% across four text generation benchmarks.

---

## 1. Assertion/Suggestion System Architecture

### 1.1 Core Constructs

DSPy provides two types of LM Assertions:

#### `dspy.Assert` (Hard Assertions)
- **Purpose:** Critical constraints that must be satisfied
- **Behavior:** Initiates retry with backtracking on failure
- **Failure Mode:** Raises `dspy.AssertionError` after max retries
- **Use Case:** Development-time "checkers" for non-negotiable requirements

```python
dspy.Assert(
    validation_fn(model_outputs),
    "feedback message explaining what went wrong",
    target_module=module_to_retry
)
```

#### `dspy.Suggest` (Soft Suggestions)
- **Purpose:** Desirable but non-essential constraints
- **Behavior:** Attempts retry with backtracking on failure
- **Failure Mode:** Logs `SuggestionError` warning and continues pipeline
- **Use Case:** Evaluation-time "helpers" for optimization guidance

```python
dspy.Suggest(
    validation_fn(model_outputs),
    "feedback message for improvement",
    target_module=module_to_retry
)
```

### 1.2 API Signature

```python
dspy.Assert(
    constraint: bool,           # Validation result
    msg: str,                   # Error/feedback message
    target_module: Module = None,  # Module to retry (default: previous)
    backtrack: Module = None    # Deprecated alias for target_module
)
```

### 1.3 Replacement in DSPy 2.6

**Deprecation Notice:** `dspy.Assert` and `dspy.Suggest` are replaced by:

- **`dspy.BestOfN`**: Runs module N times, returns best prediction based on reward function
- **`dspy.Refine`**: Extends BestOfN with automatic feedback loop for iterative improvement

```python
# Modern approach (DSPy 2.6+)
best_of_3 = dspy.BestOfN(
    module=qa_module,
    N=3,
    reward_fn=validation_function,
    threshold=1.0
)

# With refinement feedback
refined = dspy.Refine(
    module=qa_module,
    reward_fn=validation_function,
    threshold=0.8,
    max_iterations=3
)
```

---

## 2. How Assertions Guide Optimization

### 2.1 Three Core Optimization Mechanisms

#### A. Assertion-Driven Backtracking (Inference Time)
- **When:** During runtime when validation fails
- **How:** Model receives past failed output + error message, retries with modified prompt
- **Benefit:** Self-refinement without manual intervention

#### B. Assertion-Driven Example Bootstrapping (Compile Time)
- **When:** During prompt optimization with BootstrapFewShot
- **How:** Teacher model uses assertions to generate robust few-shot examples
- **Benefit:** Creates harder, more challenging training examples
- **Implementation:** Assertions applied to teacher model during bootstrapping

```python
optimizer = BootstrapFewShot(
    metric=validation_metric,
    max_bootstrapped_demos=8,
    max_labeled_demos=4
)
compiled_program = optimizer.compile(student=program, trainset=examples)
```

#### C. Counterexample Bootstrapping (Compile Time)
- **When:** During optimization when failures occur
- **How:** Captures failed examples with error traces and fixes
- **Benefit:** Models learn to avoid mistakes by seeing negative demonstrations
- **Result:** Reduces need for runtime backtracking when mixed with positive examples

### 2.2 Compilation + Inference Integration

**Dual-Stage Optimization:**
1. **Compile Time:** Teacher model with assertions generates high-quality bootstrapped examples
2. **Inference Time:** Student model uses its own assertions for runtime self-correction

**Impact:** Compound reliability improvement from both demonstration quality and runtime validation

---

## 3. Assert Transform Module Pattern

### 3.1 The `assert_transform_module` Function

Wraps a DSPy module with assertion handling and backtracking logic:

```python
from dspy.primitives.assertions import assert_transform_module, backtrack_handler

# Basic usage
program_with_assertions = assert_transform_module(
    MyDSPyProgram(),
    backtrack_handler
)

# Custom retry configuration
import functools
program_with_custom_retries = assert_transform_module(
    MyDSPyProgram(),
    functools.partial(backtrack_handler, max_backtracks=3)
)
```

### 3.2 Alternative: Direct Activation

```python
program = MyDSPyProgram()
program.activate_assertions()  # Uses default max_backtracks=2
```

### 3.3 How Transform Works

1. **Wrapping:** Intercepts module execution
2. **Validation:** Checks assertion conditions after each module call
3. **Backtracking:** On failure, modifies signature and retries
4. **Signature Modification:** Injects `Past Output` and `Instruction` fields
5. **Retry Logic:** Continues until success or max_backtracks exceeded

### 3.4 Backtrack Handler Internals

The backtrack handler implements:
- **State Management:** Tracks retry attempts and failed outputs
- **Prompt Engineering:** Dynamically adds error context to signatures
- **Retry Coordination:** Manages which module to retry (target_module)
- **Error Escalation:** Converts persistent failures to exceptions

---

## 4. Built-In Assertion Types & Validation Patterns

### 4.1 Common Validation Patterns

DSPy doesn't provide built-in assertion types; instead, it uses Python functions returning booleans.

#### Length Constraints
```python
dspy.Suggest(
    len(output.query) <= 100,
    "Query should be short and less than 100 characters",
    target_module=self.generate_query
)
```

#### Format Constraints
```python
dspy.Assert(
    output.answer.count(" ") == 0,
    "Answer must be a single word without spaces",
    target_module=self.answer_module
)
```

#### Content Constraints
```python
dspy.Assert(
    output.response.strip() != "",
    "Response cannot be empty",
    target_module=self.generate_response
)
```

#### Logical Constraints
```python
def validate_context_relevance(output):
    return any(keyword in output.context for keyword in required_keywords)

dspy.Suggest(
    validate_context_relevance(output),
    "Context must contain relevant keywords",
    target_module=self.retrieve_context
)
```

#### Uniqueness Constraints
```python
dspy.Suggest(
    len(set(output.queries)) == len(output.queries),
    "All generated queries must be unique",
    target_module=self.generate_queries
)
```

### 4.2 Complex Validation Functions

```python
def validate_citation_format(output):
    """Ensure all citations follow [1], [2] format"""
    import re
    citations = re.findall(r'\[\d+\]', output.text)
    # Check sequential numbering
    expected = [f"[{i}]" for i in range(1, len(citations) + 1)]
    return citations == expected

dspy.Assert(
    validate_citation_format(output),
    "Citations must be sequential: [1], [2], [3]...",
    target_module=self.generate_answer
)
```

### 4.3 Validation Function Requirements

- **Return Type:** Must return `bool`
- **Side Effects:** Should be pure functions (no state modification)
- **Error Handling:** Should not raise exceptions (return False instead)
- **Performance:** Should be fast (executed on every attempt)

---

## 5. Backtracking and Retry Mechanisms

### 5.1 Retry Flow Architecture

```
[Module Execution] → [Output Generated]
        ↓
[Assertion Check]
        ↓
    [Pass?] → YES → [Continue Pipeline]
        ↓ NO
[Modify Signature: Add Past Output + Instruction]
        ↓
[Retry Target Module]
        ↓
[Increment Backtrack Counter]
        ↓
[Counter < Max?] → YES → [Retry Execution]
        ↓ NO
[Assert: Raise Error | Suggest: Log Warning & Continue]
```

### 5.2 Dynamic Signature Modification

When assertion fails, DSPy modifies the module's signature:

**Original Signature:**
```python
class AnswerQuestion(dspy.Signature):
    question = dspy.InputField()
    answer = dspy.OutputField()
```

**Modified Signature (After Failure):**
```python
class AnswerQuestion(dspy.Signature):
    question = dspy.InputField()
    past_output = dspy.InputField(desc="Previous answer that failed validation")
    instruction = dspy.InputField(desc="Feedback: Answer must be one word without spaces")
    answer = dspy.OutputField()
```

### 5.3 Backtracking Configuration

#### Default Configuration
- **max_backtracks:** 2 (default)
- **target_module:** Previous module in pipeline
- **error_mode:** Assert raises error, Suggest logs warning

#### Custom Configuration
```python
# Option 1: Via assert_transform_module
import functools
program = assert_transform_module(
    MyProgram(),
    functools.partial(backtrack_handler, max_backtracks=5)
)

# Option 2: Per-assertion configuration (target_module)
dspy.Assert(
    validation_fn(output),
    "Error message",
    target_module=specific_module  # Retry this specific module
)
```

### 5.4 Retry State Management

**Per-Attempt State:**
- Failed output from previous attempt
- Feedback message (from assertion msg parameter)
- Retry attempt count
- Original input context

**Cross-Attempt State:**
- All previous failed outputs (potentially)
- Cumulative feedback messages
- Module execution history

### 5.5 Termination Conditions

**Success Termination:**
- Assertion validation passes
- Continue to next pipeline stage

**Failure Termination (Assert):**
- Max backtracks exceeded
- Raise `dspy.AssertionError`
- Pipeline halts

**Failure Termination (Suggest):**
- Max backtracks exceeded
- Log `SuggestionError` warning
- Pipeline continues

---

## 6. How Assertions Improve Reliability

### 6.1 Quantitative Improvements

From research benchmarks:
- **Rule Compliance:** Up to 164% improvement in constraint satisfaction
- **Output Quality:** Up to 37% improvement in response quality
- **Consistency:** Deterministic validation reduces output variance

### 6.2 Reliability Mechanisms

#### A. Self-Correction at Runtime
**Problem:** LLMs produce outputs violating constraints
**Solution:** Automatic retry with explicit feedback
**Benefit:** No manual intervention required

#### B. Explicit Constraint Declaration
**Problem:** Implicit expectations in prompts are unreliable
**Solution:** Programmatic validation functions
**Benefit:** Clear, testable requirements

#### C. Feedback Loop Integration
**Problem:** Models don't know why outputs failed
**Solution:** Past output + instruction injection
**Benefit:** Targeted self-refinement

#### D. Graceful Degradation
**Problem:** Hard failures crash pipelines
**Solution:** Suggest vs Assert distinction
**Benefit:** Configurable failure handling

#### E. Learning from Failures
**Problem:** Same mistakes repeated across runs
**Solution:** Counterexample bootstrapping
**Benefit:** Compile-time learning from errors

### 6.3 Development Workflow Benefits

#### Early Error Detection
- **Phase:** Development
- **Mechanism:** `dspy.Assert` as type checker
- **Benefit:** Catch bugs before deployment

#### Production Robustness
- **Phase:** Production
- **Mechanism:** `dspy.Suggest` for graceful handling
- **Benefit:** Continue operation despite constraint violations

#### Iterative Refinement
- **Phase:** Optimization
- **Mechanism:** Assertion-driven bootstrapping
- **Benefit:** Automatically generate hard test cases

### 6.4 Reliability Patterns

#### Pattern 1: Critical Path Protection
```python
# Critical outputs must pass strict validation
dspy.Assert(
    validate_safety(output),
    "Safety check failed - output contains prohibited content",
    target_module=self.generate_response
)
```

#### Pattern 2: Quality Optimization
```python
# Encourage quality without blocking
dspy.Suggest(
    len(output.answer) > 50,
    "Answer should be detailed (>50 chars)",
    target_module=self.generate_answer
)
```

#### Pattern 3: Format Enforcement
```python
# Ensure downstream compatibility
dspy.Assert(
    json.loads(output.structured_data),  # Validates JSON
    "Output must be valid JSON",
    target_module=self.format_output
)
```

### 6.5 Limitations and Trade-offs

#### Computational Cost
- **Issue:** Each retry requires additional LLM call
- **Impact:** Latency increases with retries
- **Mitigation:** Set reasonable max_backtracks

#### Non-Determinism
- **Issue:** Different retries may produce different outputs
- **Impact:** Harder to debug and reproduce
- **Mitigation:** Log all attempts for analysis

#### Validation Function Quality
- **Issue:** Poor validation logic undermines system
- **Impact:** False positives/negatives
- **Mitigation:** Test validation functions independently

#### Cascading Failures
- **Issue:** Multiple assertions can compound retry costs
- **Impact:** Exponential retry attempts
- **Mitigation:** Strategic assertion placement

---

## 7. Rust Assertion System Design Recommendations

### 7.1 Core Design Principles

Based on DSPy analysis, a Rust implementation should embody:

1. **Type-Safe Validation:** Leverage Rust's type system for compile-time guarantees
2. **Result<T, E> Integration:** Natural fit with Rust error handling
3. **Zero-Cost Abstractions:** Minimal runtime overhead
4. **Explicit State Management:** Clear ownership of retry state
5. **Trait-Based Extensibility:** Pluggable validation and retry strategies

### 7.2 Proposed Architecture

#### A. Core Types

```rust
/// Validation result with optional feedback
pub enum ValidationResult {
    Valid,
    Invalid { feedback: String },
}

/// Assertion severity level
pub enum AssertionLevel {
    /// Critical - halt on failure
    Assert,
    /// Advisory - log and continue
    Suggest,
}

/// Backtracking configuration
pub struct BacktrackConfig {
    pub max_attempts: usize,
    pub strategy: RetryStrategy,
}

/// Retry strategy
pub enum RetryStrategy {
    /// Retry same module with feedback
    SameModule,
    /// Retry specific target module
    TargetModule(ModuleId),
    /// Custom retry logic
    Custom(Box<dyn RetryHandler>),
}
```

#### B. Validation Trait

```rust
/// Trait for output validation
pub trait Validator<T>: Send + Sync {
    /// Validate output, return result with optional feedback
    fn validate(&self, output: &T) -> ValidationResult;

    /// Optional: Provide feedback message template
    fn feedback_template(&self) -> Option<&str> {
        None
    }
}

// Example: Length validator
pub struct LengthValidator {
    max_length: usize,
}

impl Validator<String> for LengthValidator {
    fn validate(&self, output: &String) -> ValidationResult {
        if output.len() <= self.max_length {
            ValidationResult::Valid
        } else {
            ValidationResult::Invalid {
                feedback: format!(
                    "Output too long: {} chars (max: {})",
                    output.len(),
                    self.max_length
                ),
            }
        }
    }
}
```

#### C. Assertion Builder Pattern

```rust
/// Fluent assertion builder
pub struct Assertion<T> {
    validator: Box<dyn Validator<T>>,
    level: AssertionLevel,
    backtrack_config: BacktrackConfig,
    custom_feedback: Option<String>,
}

impl<T> Assertion<T> {
    pub fn assert(validator: impl Validator<T> + 'static) -> Self {
        Self {
            validator: Box::new(validator),
            level: AssertionLevel::Assert,
            backtrack_config: BacktrackConfig::default(),
            custom_feedback: None,
        }
    }

    pub fn suggest(validator: impl Validator<T> + 'static) -> Self {
        Self {
            validator: Box::new(validator),
            level: AssertionLevel::Suggest,
            backtrack_config: BacktrackConfig::default(),
            custom_feedback: None,
        }
    }

    pub fn with_feedback(mut self, msg: impl Into<String>) -> Self {
        self.custom_feedback = Some(msg.into());
        self
    }

    pub fn max_retries(mut self, max: usize) -> Self {
        self.backtrack_config.max_attempts = max;
        self
    }

    pub fn target_module(mut self, module_id: ModuleId) -> Self {
        self.backtrack_config.strategy = RetryStrategy::TargetModule(module_id);
        self
    }
}
```

#### D. Module Integration

```rust
/// DSPy-style module with assertion support
pub trait Module {
    type Input;
    type Output;
    type Error;

    /// Execute module
    fn execute(&mut self, input: Self::Input) -> Result<Self::Output, Self::Error>;

    /// Get module ID for retry targeting
    fn id(&self) -> ModuleId;
}

/// Module with assertions
pub struct AssertedModule<M: Module> {
    inner: M,
    assertions: Vec<Assertion<M::Output>>,
}

impl<M: Module> AssertedModule<M> {
    pub fn new(module: M) -> Self {
        Self {
            inner: module,
            assertions: Vec::new(),
        }
    }

    pub fn with_assertion(mut self, assertion: Assertion<M::Output>) -> Self {
        self.assertions.push(assertion);
        self
    }
}
```

#### E. Backtracking Executor

```rust
/// Manages retry logic with backtracking
pub struct BacktrackExecutor<M: Module> {
    module: AssertedModule<M>,
}

impl<M: Module> BacktrackExecutor<M> {
    pub fn execute(
        &mut self,
        input: M::Input,
    ) -> Result<M::Output, ExecutionError<M::Error>> {
        let mut attempts = 0;
        let mut past_outputs = Vec::new();
        let mut feedback_history = Vec::new();

        loop {
            // Execute module with context
            let context = RetryContext {
                attempt: attempts,
                past_outputs: &past_outputs,
                feedback: &feedback_history,
            };

            let output = self.execute_with_context(input.clone(), context)?;

            // Validate against all assertions
            let validation = self.validate_output(&output);

            match validation {
                ValidationResult::Valid => return Ok(output),
                ValidationResult::Invalid { feedback } => {
                    attempts += 1;
                    past_outputs.push(output);
                    feedback_history.push(feedback.clone());

                    // Check termination conditions
                    if attempts >= self.max_attempts() {
                        return self.handle_max_retries(feedback);
                    }
                }
            }
        }
    }

    fn handle_max_retries(
        &self,
        feedback: String,
    ) -> Result<M::Output, ExecutionError<M::Error>> {
        match self.module.assertions[0].level {
            AssertionLevel::Assert => {
                Err(ExecutionError::AssertionFailed { feedback })
            }
            AssertionLevel::Suggest => {
                // Log warning and return last output
                log::warn!("Suggestion failed: {}", feedback);
                Ok(/* last output */)
            }
        }
    }
}
```

### 7.3 Signature Modification Strategy

DSPy modifies prompts dynamically. In Rust, we represent this explicitly:

```rust
/// Extended prompt with retry context
pub struct RetryPrompt<T> {
    pub original_input: T,
    pub past_outputs: Vec<String>,  // Serialized outputs
    pub feedback: Vec<String>,       // Error messages
    pub attempt: usize,
}

/// Trait for modules that support retry context
pub trait RetryableModule: Module {
    /// Execute with retry context (modified signature)
    fn execute_with_retry(
        &mut self,
        input: Self::Input,
        context: RetryContext,
    ) -> Result<Self::Output, Self::Error>;
}
```

### 7.4 Optimization Integration

#### Bootstrapping Support

```rust
/// Compiler/optimizer with assertion support
pub struct AssertionDrivenOptimizer<M: Module> {
    teacher: M,
    student: M,
    assertions: Vec<Assertion<M::Output>>,
}

impl<M: Module> AssertionDrivenOptimizer<M> {
    /// Generate bootstrapped examples using assertions
    pub fn bootstrap_examples(
        &mut self,
        train_data: Vec<M::Input>,
    ) -> Result<Vec<Example<M::Input, M::Output>>, OptimizationError> {
        let mut examples = Vec::new();
        let mut counterexamples = Vec::new();

        for input in train_data {
            // Execute teacher with assertions
            match self.execute_teacher_with_assertions(input.clone()) {
                Ok(output) => {
                    examples.push(Example { input, output });
                }
                Err(failure) => {
                    // Capture counterexample with trace
                    counterexamples.push(CounterExample {
                        input,
                        failed_outputs: failure.attempts,
                        feedback: failure.feedback,
                    });
                }
            }
        }

        // Compile student with examples + counterexamples
        self.compile_student(examples, counterexamples)
    }
}
```

#### Validation Metric Integration

```rust
/// Metric that incorporates assertions
pub struct AssertionMetric<T> {
    assertions: Vec<Assertion<T>>,
}

impl<T> Metric<T> for AssertionMetric<T> {
    fn score(&self, output: &T) -> f64 {
        let total = self.assertions.len() as f64;
        let passed = self.assertions
            .iter()
            .filter(|a| matches!(a.validator.validate(output), ValidationResult::Valid))
            .count() as f64;

        passed / total
    }
}
```

### 7.5 Error Handling Design

```rust
/// Assertion-specific error types
#[derive(Debug, thiserror::Error)]
pub enum AssertionError {
    #[error("Assertion failed after {attempts} attempts: {feedback}")]
    AssertionFailed {
        attempts: usize,
        feedback: String,
    },

    #[error("Validation function panicked: {0}")]
    ValidationPanic(String),

    #[error("Module execution failed: {0}")]
    ModuleError(Box<dyn std::error::Error + Send + Sync>),
}

/// Warning type for suggestions
#[derive(Debug)]
pub struct SuggestionWarning {
    pub attempts: usize,
    pub feedback: String,
    pub final_output: String,  // Output that was accepted despite failure
}
```

### 7.6 Type-Safe Validation Builders

Leverage Rust's type system for compile-time validation:

```rust
/// Type-safe validation builder
pub mod validators {
    use super::*;

    /// Length constraint (compile-time bounds)
    pub fn length_between<const MIN: usize, const MAX: usize>() -> LengthValidator {
        LengthValidator { min: MIN, max: MAX }
    }

    /// Regex pattern (validated at construction)
    pub fn matches_regex(pattern: &str) -> Result<RegexValidator, regex::Error> {
        Ok(RegexValidator {
            regex: regex::Regex::new(pattern)?,
        })
    }

    /// Combinator: AND
    pub fn all<T>(validators: Vec<Box<dyn Validator<T>>>) -> AllValidator<T> {
        AllValidator { validators }
    }

    /// Combinator: OR
    pub fn any<T>(validators: Vec<Box<dyn Validator<T>>>) -> AnyValidator<T> {
        AnyValidator { validators }
    }
}

// Usage
let assertion = Assertion::assert(
    validators::all(vec![
        Box::new(validators::length_between::<10, 100>()),
        Box::new(validators::matches_regex(r"^\w+$").unwrap()),
    ])
)
.with_feedback("Output must be 10-100 alphanumeric characters")
.max_retries(3);
```

### 7.7 Observability and Debugging

```rust
/// Telemetry for assertion execution
pub struct AssertionTelemetry {
    pub module_id: ModuleId,
    pub assertion_id: String,
    pub attempts: Vec<AttemptRecord>,
    pub final_result: ValidationResult,
    pub total_duration: Duration,
}

pub struct AttemptRecord {
    pub attempt_number: usize,
    pub output: String,  // Serialized
    pub validation_result: ValidationResult,
    pub duration: Duration,
}

/// Trait for assertion observers
pub trait AssertionObserver: Send + Sync {
    fn on_validation_attempt(&self, record: &AttemptRecord);
    fn on_backtrack(&self, attempt: usize, feedback: &str);
    fn on_final_result(&self, telemetry: AssertionTelemetry);
}
```

### 7.8 Performance Optimizations

#### Parallel Validation
```rust
/// Validate against multiple assertions in parallel
pub struct ParallelValidator<T> {
    validators: Vec<Box<dyn Validator<T>>>,
}

impl<T: Send + Sync> Validator<T> for ParallelValidator<T> {
    fn validate(&self, output: &T) -> ValidationResult {
        use rayon::prelude::*;

        let results: Vec<_> = self.validators
            .par_iter()
            .map(|v| v.validate(output))
            .collect();

        // Aggregate results
        results.into_iter()
            .find(|r| matches!(r, ValidationResult::Invalid { .. }))
            .unwrap_or(ValidationResult::Valid)
    }
}
```

#### Cached Validation
```rust
/// Cache validation results for deterministic outputs
pub struct CachedValidator<T: Hash + Eq> {
    inner: Box<dyn Validator<T>>,
    cache: Arc<Mutex<HashMap<T, ValidationResult>>>,
}

impl<T: Hash + Eq + Clone> Validator<T> for CachedValidator<T> {
    fn validate(&self, output: &T) -> ValidationResult {
        let mut cache = self.cache.lock().unwrap();

        if let Some(cached) = cache.get(output) {
            return cached.clone();
        }

        let result = self.inner.validate(output);
        cache.insert(output.clone(), result.clone());
        result
    }
}
```

### 7.9 Integration with ggen Architecture

Given ggen's RDF-based specification system:

#### TTL Specification for Assertions

```turtle
@prefix ggen: <https://ggen.tech/ontology#> .
@prefix assert: <https://ggen.tech/ontology/assertions#> .

:MyModule a ggen:Module ;
    assert:hasAssertion [
        a assert:Assertion ;
        assert:level assert:Assert ;
        assert:validator :LengthValidator ;
        assert:maxRetries 3 ;
        assert:feedback "Output must be 10-100 characters"
    ] .

:LengthValidator a assert:Validator ;
    assert:validatorType "length_between" ;
    assert:minLength 10 ;
    assert:maxLength 100 .
```

#### Code Generation from Specs

```rust
/// Generated assertion from TTL spec
pub fn generate_assertion_from_spec(
    spec: &AssertionSpec,
) -> Result<Assertion<String>, SpecError> {
    let validator = match &spec.validator_type {
        "length_between" => {
            Box::new(validators::length_between_dynamic(
                spec.min_length,
                spec.max_length,
            ))
        }
        "regex" => {
            Box::new(validators::matches_regex(&spec.pattern)?)
        }
        _ => return Err(SpecError::UnknownValidator),
    };

    Ok(Assertion::assert(validator)
        .with_feedback(spec.feedback.clone())
        .max_retries(spec.max_retries))
}
```

### 7.10 Testing Strategy

#### Unit Tests for Validators
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;

    #[test]
    fn test_length_validator_valid() {
        // Arrange
        let validator = validators::length_between::<5, 10>();
        let output = "hello".to_string();

        // Act
        let result = validator.validate(&output);

        // Assert
        assert!(matches!(result, ValidationResult::Valid));
    }

    #[test]
    fn test_assertion_backtracking() {
        // Arrange
        let mut module = MockModule::new();
        module.expect_execute()
            .times(3)
            .returning(|_| Ok("invalid".to_string()))
            .then()
            .returning(|_| Ok("valid output".to_string()));

        let assertion = Assertion::assert(validators::length_between::<10, 50>())
            .max_retries(5);

        let mut executor = BacktrackExecutor::new(
            AssertedModule::new(module).with_assertion(assertion)
        );

        // Act
        let result = executor.execute("input".to_string());

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "valid output");
    }
}
```

#### Property Tests for Retry Logic
```rust
#[cfg(test)]
mod proptests {
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn backtrack_terminates(max_retries in 1usize..10) {
            // Arrange
            let module = AlwaysFailingModule::new();
            let assertion = Assertion::assert(validators::always_invalid())
                .max_retries(max_retries);
            let mut executor = BacktrackExecutor::new(
                AssertedModule::new(module).with_assertion(assertion)
            );

            // Act
            let result = executor.execute("input".to_string());

            // Assert
            prop_assert!(result.is_err());
            prop_assert_eq!(
                result.unwrap_err().attempts(),
                max_retries
            );
        }
    }
}
```

---

## 8. Implementation Roadmap

### Phase 1: Core Infrastructure (EPIC 9 Candidate)
- [ ] Define core types (ValidationResult, AssertionLevel, BacktrackConfig)
- [ ] Implement Validator trait
- [ ] Build Assertion builder pattern
- [ ] Create BacktrackExecutor
- [ ] Write comprehensive tests

### Phase 2: Standard Validators
- [ ] Length validators
- [ ] Regex validators
- [ ] Format validators (JSON, YAML, etc.)
- [ ] Logical combinators (AND, OR, NOT)
- [ ] Custom validator support

### Phase 3: Module Integration
- [ ] Module trait with assertion support
- [ ] AssertedModule wrapper
- [ ] Retry context propagation
- [ ] Signature modification for prompts

### Phase 4: Optimization Integration
- [ ] Assertion-driven bootstrapping
- [ ] Counterexample collection
- [ ] Metric integration
- [ ] Compiler hooks

### Phase 5: Observability
- [ ] Telemetry collection
- [ ] Logging integration
- [ ] Debugging tools
- [ ] Performance profiling

### Phase 6: TTL Specification
- [ ] RDF vocabulary for assertions
- [ ] Code generation from specs
- [ ] Validation of specs
- [ ] Documentation generation

---

## 9. Key Takeaways for Rust Implementation

### Alignment with ggen Principles

1. **Type-First Design:** Assertions encoded in type system where possible
2. **Result<T, E> Throughout:** Natural mapping to Rust error handling
3. **Zero Unwrap/Expect:** Validators return Result, not panic
4. **RDF as Source of Truth:** Assertions specified in .specify/*.ttl
5. **Deterministic Receipts:** Telemetry provides evidence of validation

### Critical Success Factors

1. **Validator Trait Quality:** Core abstraction must be ergonomic and composable
2. **Performance:** Validation overhead must be minimal (<5% of execution time)
3. **Error Messages:** Feedback must be actionable for LLM self-correction
4. **Observability:** Full telemetry for debugging and optimization
5. **TTL Integration:** Seamless specification-to-code generation

### Anti-Patterns to Avoid

1. **Validation Side Effects:** Validators must be pure functions
2. **Implicit Retries:** All backtracking must be explicit and configurable
3. **Untyped Feedback:** Use structured error types, not just strings
4. **Blocking Validation:** Async validators where I/O is required
5. **Global State:** Each executor manages its own retry state

### Advantages Over Python DSPy

1. **Compile-Time Safety:** Many validation errors caught at compile time
2. **Zero-Cost Abstractions:** Trait-based design with minimal overhead
3. **Explicit Ownership:** Clear retry state management
4. **Type-Safe Builders:** Fluent APIs with compile-time validation
5. **Deterministic Execution:** No GIL, better concurrency

---

## Sources

### Official Documentation
- [DSPy Assertions Documentation](https://dspy.ai/learn/programming/7-assertions/)
- [DSPy FAQ](https://dspy.ai/faqs/)
- [DSPy Cheatsheet](https://dspy.ai/cheatsheet/)
- [DSPy Optimizers](https://dspy.ai/learn/optimization/optimizers/)
- [DSPy Output Refinement - BestOfN and Refine](https://dspy.ai/tutorials/output_refinement/best-of-n-and-refine/)

### Research Papers
- [DSPy Assertions: Computational Constraints for Self-Refining Language Model Pipelines (arXiv)](https://arxiv.org/html/2312.13382v2)
- [DSPy Assertions PDF](https://arxiv.org/pdf/2312.13382)

### Tutorials and Analysis
- [Guiding LLM Output with DSPy Assertions and Suggestions](https://learnbybuilding.ai/tutorial/guiding-llm-output-with-dspy-assertions-and-suggestions/)
- [DSPy & The Principle Of Assertions by Cobus Greyling](https://cobusgreyling.medium.com/dspy-the-principle-of-assertions-b2c3982d95a8)
- [Building Production-Ready AI Agents & LLM programs with DSPy](https://www.firebird-technologies.com/p/building-production-ready-ai-agents)
- [DSPy Assertions - Arize AI](https://arize.com/blog/dspy-assertions-computational-constraints/)

### GitHub Resources
- [DSPy GitHub Repository](https://github.com/stanfordnlp/dspy)
- [DSPy Documentation Migration Issue](https://github.com/stanfordnlp/dspy/issues/8668)
- [DSPy Concatenated Documentation](https://gist.github.com/damek/c5dcf37e5776128a7470c5708b5779f4)

---

**Research Conclusion:** The DSPy assertion system demonstrates how computational constraints can transform LLM reliability through backtracking, self-refinement, and optimization integration. The proposed Rust design leverages type safety, Result<T,E> error handling, and trait-based extensibility to create a more robust, performant, and specification-driven assertion system for the ggen project.
