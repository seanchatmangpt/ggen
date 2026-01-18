# DSPy Evaluation System Research

**Date**: 2026-01-11
**Status**: Complete
**Purpose**: Document Python DSPy evaluation framework and recommend Rust architecture

## Executive Summary

DSPy provides a comprehensive evaluation framework centered around the `Evaluate` class, which enables parallel, metrics-driven assessment of LLM programs. The system supports both simple accuracy metrics and complex AI-based evaluators, with deep integration into the optimization pipeline through trace collection and demonstration bootstrapping.

**Key Findings**:
- Evaluation is metric-driven with a simple function signature: `metric(example, pred, trace=None) -> float|bool`
- Parallel processing via `num_threads` parameter enables efficient large-scale evaluation
- The `trace` parameter enables dual-mode metrics: boolean for bootstrapping, float for evaluation
- Built-in metrics (SemanticF1, CompleteAndGrounded) use DSPy programs as evaluators
- Integration with optimizers allows intermediate step validation during compilation

**Rust Implementation Status**: We have `MetricFn` type and basic optimizer support, but lack a dedicated `Evaluate` framework, built-in metrics library, and trace-aware evaluation capabilities.

---

## 1. The Evaluate Class

### 1.1 Core Architecture

The `dspy.Evaluate` class provides systematic program evaluation with parallel processing and comprehensive reporting.

**Location**: `dspy.evaluate.evaluate.Evaluate`

**Constructor Signature**:
```python
class Evaluate:
    def __init__(
        self,
        devset: list[dspy.Example],        # Evaluation dataset (required)
        metric: Callable | None = None,    # Scoring function
        num_threads: int | None = None,    # Parallel threads
        display_progress: bool = False,    # Show progress bar
        display_table: bool | int = False, # Display results table
        max_errors: int | None = None,     # Stop after N errors
        provide_traceback: bool | None = None,
        failure_score: float = 0.0,        # Default score on exception
        save_as_csv: str | None = None,    # CSV output path
        save_as_json: str | None = None    # JSON output path
    )
```

### 1.2 Call Interface

The evaluator is invoked with optional parameter overrides:

```python
# Setup evaluator (reusable)
evaluator = Evaluate(
    devset=YOUR_DEVSET,
    num_threads=4,
    display_progress=True,
    display_table=5
)

# Run evaluation
result = evaluator(YOUR_PROGRAM, metric=YOUR_METRIC)
```

**Return Type**: `EvaluationResult` containing:
- `score`: Float percentage (0-100) representing overall performance
- `results`: List of `(example, prediction, score)` tuples for each dataset item

### 1.3 Key Features

**Parallel Evaluation**: The `num_threads` parameter enables concurrent evaluation across dataset examples, critical for large-scale benchmarking.

**Progressive Display**: The `display_table` parameter shows sample inputs/outputs with metric scores during evaluation, aiding in rapid iteration.

**Error Handling**: The `max_errors` and `failure_score` parameters provide graceful degradation and configurable failure behavior.

**Export Capabilities**: Results can be saved to CSV/JSON for analysis and reporting.

### 1.4 Usage Pattern

```python
from dspy.evaluate import Evaluate

# Define metric
def accuracy_metric(example, pred, trace=None):
    return example.answer == pred.answer

# Create evaluator
evaluator = Evaluate(
    devset=dev_examples,
    metric=accuracy_metric,
    num_threads=8,
    display_progress=True,
    display_table=10
)

# Evaluate program
result = evaluator(my_program)
print(f"Score: {result.score}%")
```

---

## 2. Metrics Framework

### 2.1 Metric Function Signature

All DSPy metrics follow a consistent pattern:

```python
def metric_name(
    example: dspy.Example,  # Ground truth from dataset
    pred: dspy.Prediction,  # Program output
    trace: Any | None = None  # Optional trace for optimization
) -> float | int | bool
```

**Parameters**:
- `example`: Contains input fields and expected outputs (ground truth)
- `pred`: The actual output from the DSPy program
- `trace`: Optional trace data collected during optimization (see §4)

**Return Value**:
- `bool`: Used during bootstrapping to accept/reject demonstrations
- `float`: Used during evaluation/optimization for scoring (typically 0.0-1.0)
- `int`: Also acceptable for scoring

### 2.2 Built-in Metrics

DSPy provides several sophisticated built-in metrics:

#### 2.2.1 Basic Metrics

**answer_exact_match**: Checks if answer exactly matches expected value.

**answer_passage_match**: Validates if answer appears in retrieved context passages.

#### 2.2.2 SemanticF1

Computes recall, precision, and F1 score using LLM-based semantic comparison.

**Implementation**:
```python
class SemanticF1(dspy.Module):
    def __init__(self, threshold=0.66, decompositional=False):
        self.threshold = threshold
        signature = (DecompositionalSemanticRecallPrecision
                    if decompositional
                    else SemanticRecallPrecision)
        self.evaluate = dspy.ChainOfThought(signature)
```

**Signatures**:
- `SemanticRecallPrecision`: "Compare a system's response to the ground truth to compute its recall and precision."
- `DecompositionalSemanticRecallPrecision`: Adds intermediate reasoning by enumerating key ideas first.

**Usage**:
```python
metric = SemanticF1(threshold=0.7)
score = metric(example, prediction)
```

**F1 Calculation**:
```python
def f1_score(precision, recall):
    precision = max(0.0, min(1.0, precision))
    recall = max(0.0, min(1.0, recall))
    if precision + recall == 0:
        return 0.0
    return 2 * (precision * recall) / (precision + recall)
```

#### 2.2.3 CompleteAndGrounded

Evaluates two dimensions: completeness against ground truth and groundedness against retrieved context.

**Architecture**: Runs two parallel evaluations using ChainOfThought and combines scores using F1.

### 2.3 Custom Metric Examples

#### Example 1: Simple Validation

```python
def validate_answer(example, pred, trace=None):
    """Exact match metric."""
    return example.answer.lower() == pred.answer.lower()
```

#### Example 2: Trace-Aware Dual-Mode Metric

```python
def validate_context_and_answer(example, pred, trace=None):
    """Validates both answer correctness and context grounding.

    Returns bool during bootstrapping, float during evaluation.
    """
    answer_match = example.answer.lower() == pred.answer.lower()
    context_match = any(
        (pred.answer.lower() in c)
        for c in pred.context
    )

    if trace is None:  # Evaluation or optimization
        return (answer_match + context_match) / 2.0
    else:  # Bootstrapping demonstrations
        return answer_match and context_match
```

#### Example 3: AI-Based Metric Using DSPy Program

```python
class FactJudge(dspy.Signature):
    """Judge if answer is factually correct based on context."""
    context = dspy.InputField(desc="Retrieved context")
    question = dspy.InputField()
    answer = dspy.InputField(desc="Answer to evaluate")
    factually_correct = dspy.OutputField(
        desc="Is answer factually correct?",
        prefix="Factuality[Yes/No]:"
    )

# Create metric using DSPy program
factuality_evaluator = dspy.ChainOfThought(FactJudge)

def factuality_metric(example, pred, trace=None):
    """Use LLM to judge factuality."""
    result = factuality_evaluator(
        context=pred.context,
        question=example.question,
        answer=pred.answer
    )
    is_factual = result.factually_correct.lower() == 'yes'

    if trace is None:
        return 1.0 if is_factual else 0.0
    else:
        return is_factual
```

#### Example 4: Multi-Property Complex Metric

```python
def comprehensive_metric(example, pred, trace=None):
    """Validates multiple properties with weighted scoring."""

    # Property 1: Answer correctness (50%)
    answer_correct = example.answer.lower() in pred.answer.lower()

    # Property 2: Context grounding (30%)
    grounded = any(pred.answer.lower() in c.lower()
                   for c in pred.context)

    # Property 3: Length appropriate (20%)
    length_ok = 10 <= len(pred.answer.split()) <= 100

    if trace is None:  # Evaluation mode
        score = (
            0.5 * float(answer_correct) +
            0.3 * float(grounded) +
            0.2 * float(length_ok)
        )
        return score
    else:  # Bootstrapping mode
        return answer_correct and grounded and length_ok
```

### 2.4 Metric Design Best Practices

1. **Start Simple**: Begin with exact match, iterate based on output analysis
2. **Return Type Discipline**: Use `bool` for bootstrapping, `float` for evaluation
3. **Use DSPy Programs as Metrics**: For long-form outputs, create evaluator signatures
4. **Leverage Trace Parameter**: Different behavior during optimization vs evaluation
5. **Validate Intermediate Steps**: Use trace to check reasoning paths (see §4.3)

---

## 3. Evaluation Loop and Parallel Processing

### 3.1 Simple Evaluation Loop

Manual evaluation without the `Evaluate` utility:

```python
# Simple loop evaluation
total_score = 0
results = []

for example in devset:
    pred = program(example.inputs)
    score = metric(example, pred)
    results.append((example, pred, score))
    total_score += score

avg_score = total_score / len(devset)
print(f"Average Score: {avg_score}")
```

### 3.2 Built-in Evaluate Utility

The `Evaluate` class handles parallelization, progress tracking, and reporting:

```python
from dspy.evaluate import Evaluate

evaluator = Evaluate(
    devset=devset,
    metric=my_metric,
    num_threads=NUM_THREADS,      # Parallel workers
    display_progress=True,         # tqdm progress bar
    display_table=num_rows         # Show N examples
)

result = evaluator(program)
```

**Benefits**:
- Automatic parallel execution across `num_threads` workers
- Progress bar for long-running evaluations
- Sample output display for rapid debugging
- Structured result aggregation

### 3.3 Parallel Processing Architecture

**Threading Model**: DSPy uses thread-based parallelism for evaluation:
- Each thread processes a subset of the devset
- Thread-safe metric invocation
- Results aggregated after completion

**Parallelization Points**:
1. **Evaluation**: Multiple examples evaluated concurrently
2. **Compilation/Optimization**: Bootstrapping demonstrations in parallel

**Configuration**:
```python
# Example: Evaluate 1000 examples with 16 threads
evaluator = Evaluate(
    devset=large_devset,  # 1000 examples
    num_threads=16,       # 16 parallel workers
    metric=accuracy
)

# Each thread processes ~62 examples
result = evaluator(program)
```

### 3.4 Error Handling in Parallel Evaluation

The `max_errors` parameter provides fail-fast behavior:

```python
evaluator = Evaluate(
    devset=devset,
    num_threads=8,
    max_errors=10,         # Stop after 10 errors
    failure_score=0.0,     # Score failed examples as 0.0
    provide_traceback=True # Include error details
)
```

---

## 4. Integration with Optimizers

### 4.1 The Trace Parameter

During optimization (compilation), DSPy traces LM calls to capture input/output behavior of each module.

**Trace Contents**:
- Inputs to each DSPy predictor
- Outputs from each DSPy predictor
- Intermediate reasoning steps
- Complete execution path

**Dual-Mode Metrics**: The trace parameter enables different behavior:

```python
def adaptive_metric(example, pred, trace=None):
    if trace is None:
        # Evaluation/optimization mode: return float score
        return compute_detailed_score(example, pred)
    else:
        # Bootstrapping mode: return bool (accept/reject)
        return meets_requirements(example, pred, trace)
```

### 4.2 BootstrapFewShot Integration

The `BootstrapFewShot` optimizer uses metrics to validate demonstrations:

**Algorithm**:
1. For each training example:
   - Run teacher/student predictor on inputs
   - Evaluate output using metric function
   - If metric returns `True`, save as demonstration
2. Create optimized predictor with validated demonstrations

**Code Example**:
```python
# Define metric for bootstrapping
def validation_metric(example, pred, trace=None):
    matches = example.answer == pred.answer

    if trace is None:
        return 1.0 if matches else 0.0  # Evaluation
    else:
        return matches  # Bootstrapping: bool

# Create optimizer
optimizer = dspy.BootstrapFewShot(
    metric=validation_metric,
    max_bootstrapped_demos=8
)

# Compile student predictor
compiled = optimizer.compile(student, trainset=trainset)
```

**Metric Behavior During Bootstrapping**:
- Called with `trace=None` (typically)
- Should return `bool` to accept/reject demonstrations
- Only successful demonstrations (metric returns `True`) are included in prompt

### 4.3 Validating Intermediate Steps with Trace

Advanced metrics can validate reasoning paths:

```python
def validate_multi_hop_reasoning(example, pred, trace=None):
    """Validate intermediate steps in multi-hop QA."""

    if trace is None:
        # Standard evaluation
        return example.answer == pred.answer

    # Optimization mode: validate intermediate steps

    # Check that each hop's query is relevant
    valid_hops = all(
        is_relevant_query(hop.query, example.question)
        for hop in trace.get('hops', [])
    )

    # Check that context was retrieved for each hop
    has_context = all(
        len(hop.context) > 0
        for hop in trace.get('hops', [])
    )

    # Check final answer
    correct_answer = example.answer == pred.answer

    return valid_hops and has_context and correct_answer
```

### 4.4 MIPROv2 Integration

The MIPROv2 optimizer uses evaluation metrics to guide Bayesian optimization:

**Process**:
1. Generate instruction/demonstration candidates
2. Evaluate each candidate using metric on validation set
3. Use Bayesian optimization to search configuration space
4. Select best configuration based on metric performance

**Metric Requirements**:
- Must return numeric score (float)
- Should be differentiable or provide clear gradients
- Fast evaluation preferred for large search spaces

### 4.5 Optimizer Evaluation Patterns

**Pattern 1: Bootstrap Validation**
```python
# Metric used during BootstrapFewShot.compile()
metric_bootstrap = lambda ex, pred, trace: ex.answer == pred.answer
```

**Pattern 2: Continuous Optimization**
```python
# Metric used during MIPROv2 search
metric_optimize = lambda ex, pred, trace: f1_score(ex, pred)
```

**Pattern 3: Dual-Mode Adaptive**
```python
def adaptive_metric(example, pred, trace=None):
    score = compute_score(example, pred)
    return score > 0.5 if trace else score  # bool vs float
```

---

## 5. Evaluation Best Practices and Patterns

### 5.1 Data Collection Best Practices

**Development Set Size**: Collect 20-200 input examples for initial evaluation.

**Data Sources**:
- HuggingFace datasets
- Production logs
- User queries (e.g., StackExchange, forums)
- Synthetic generation

**Labeling Requirements**:
- Inputs and final outputs required
- Intermediate step labels NOT required (DSPy learns from traces)

### 5.2 Metric Development Workflow

**Step 1: Start Simple**
```python
# Begin with exact match
def v1_metric(example, pred, trace=None):
    return example.answer == pred.answer
```

**Step 2: Analyze Failures**
```python
# Run evaluation and inspect results
evaluator = Evaluate(devset=devset, display_table=20)
result = evaluator(program, metric=v1_metric)

# Review displayed examples to understand failure modes
```

**Step 3: Refine Metric**
```python
# Add flexibility based on analysis
def v2_metric(example, pred, trace=None):
    # Allow case-insensitive match
    return example.answer.lower() == pred.answer.lower()
```

**Step 4: Add Complexity**
```python
# Handle partial matches and aliases
def v3_metric(example, pred, trace=None):
    expected = normalize(example.answer)
    actual = normalize(pred.answer)
    return expected in actual or actual in expected
```

### 5.3 Evaluation Patterns

#### Pattern 1: Baseline Establishment

```python
# Evaluate multiple program variants
programs = {
    'zero_shot': ZeroShotPredictor(),
    'few_shot': FewShotPredictor(demos=3),
    'optimized': OptimizedPredictor()
}

results = {}
for name, program in programs.items():
    score = evaluator(program, metric=accuracy)
    results[name] = score.score
    print(f"{name}: {score.score}%")
```

#### Pattern 2: Hyperparameter Search

```python
# Find best configuration
best_score = 0
best_config = None

for num_demos in [1, 2, 4, 8]:
    for temperature in [0.0, 0.3, 0.7, 1.0]:
        program = create_program(
            num_demos=num_demos,
            temperature=temperature
        )
        result = evaluator(program)

        if result.score > best_score:
            best_score = result.score
            best_config = (num_demos, temperature)
```

#### Pattern 3: Progressive Validation

```python
# Validate on increasingly difficult subsets
subsets = {
    'easy': easy_examples,
    'medium': medium_examples,
    'hard': hard_examples
}

for difficulty, subset in subsets.items():
    evaluator = Evaluate(devset=subset, metric=metric)
    score = evaluator(program)
    print(f"{difficulty}: {score.score}%")
```

#### Pattern 4: Multi-Metric Evaluation

```python
# Evaluate across multiple dimensions
metrics = {
    'accuracy': accuracy_metric,
    'f1': f1_metric,
    'semantic_similarity': semantic_metric
}

results = {}
for name, metric in metrics.items():
    evaluator = Evaluate(devset=devset, metric=metric)
    score = evaluator(program)
    results[name] = score.score

print(f"Accuracy: {results['accuracy']}%")
print(f"F1: {results['f1']}%")
print(f"Semantic: {results['semantic_similarity']}%")
```

### 5.4 Metric Optimization Pattern

**Meta-Evaluation**: Optimize the metric itself using a metric-for-the-metric:

```python
# Define metric signature
class MetricJudge(dspy.Signature):
    """Judge if a score accurately reflects quality."""
    question = dspy.InputField()
    answer = dspy.InputField()
    ground_truth = dspy.InputField()
    score = dspy.InputField(desc="Metric's score")
    accurate_score = dspy.OutputField(desc="Is score accurate?")

# Create metric as DSPy program
metric_program = dspy.ChainOfThought(MetricJudge)

# Meta-metric to evaluate the metric
def meta_metric(example, pred):
    judge_result = metric_program(
        question=example.question,
        answer=pred.answer,
        ground_truth=example.answer,
        score=pred.score
    )
    return judge_result.accurate_score == 'Yes'

# Optimize the metric program
optimizer = dspy.MIPROv2(metric=meta_metric)
optimized_metric = optimizer.compile(metric_program, trainset)
```

### 5.5 Debugging Evaluation

**Display Table for Inspection**:
```python
evaluator = Evaluate(
    devset=devset,
    metric=metric,
    display_table=10  # Show 10 examples
)
```

**Export for Analysis**:
```python
evaluator = Evaluate(
    devset=devset,
    metric=metric,
    save_as_csv="results.csv",
    save_as_json="results.json"
)
```

**Error Tracking**:
```python
evaluator = Evaluate(
    devset=devset,
    metric=metric,
    max_errors=5,
    provide_traceback=True
)
```

---

## 6. Comparison with Rust Implementation

### 6.1 Current Rust Implementation

**What We Have**:
- ✅ `MetricFn` type alias: `Arc<dyn Fn(&Example, &HashMap<String, Value>) -> Result<bool, ModuleError> + Send + Sync>`
- ✅ Basic optimizer integration in `BootstrapFewShot`
- ✅ Example and Demonstration types
- ✅ Async module execution

**What We're Missing**:
- ❌ Dedicated `Evaluate` struct with parallel processing
- ❌ Built-in metrics library (SemanticF1, CompleteAndGrounded, etc.)
- ❌ Trace-aware metrics (dual-mode bool/float)
- ❌ Result aggregation and reporting
- ❌ Progress tracking and display
- ❌ CSV/JSON export capabilities
- ❌ Error handling with `max_errors` and `failure_score`
- ❌ Validation of intermediate steps via trace

### 6.2 Current Rust Metric Signature

```rust
pub type MetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>)
        -> Result<bool, ModuleError>
    + Send + Sync
>;
```

**Limitations**:
1. Returns only `bool` (no `float` for scoring)
2. No `trace` parameter for dual-mode operation
3. No support for returning `Result<f64>` for evaluation scores

### 6.3 Usage Example (Current)

```rust
// Current Rust implementation
let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    let expected = example.outputs.get("answer");
    let actual = output.get("answer");

    match (expected, actual) {
        (Some(exp), Some(act)) => Ok(exp == act),
        _ => Ok(false),
    }
});

let optimizer = BootstrapFewShot::new(metric)
    .with_max_bootstrapped_demos(3);

let optimized = optimizer.compile(&student, &trainset).await?;
```

**Missing**: No `Evaluate` utility, no parallel evaluation, no result aggregation.

---

## 7. Recommended Rust Architecture

### 7.1 Core Types

```rust
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::task::JoinSet;

/// Evaluation result for a single example
#[derive(Debug, Clone)]
pub struct EvaluationPoint {
    pub example: Example,
    pub prediction: HashMap<String, Value>,
    pub score: f64,
    pub error: Option<String>,
}

/// Aggregated evaluation results
#[derive(Debug, Clone)]
pub struct EvaluationResult {
    /// Overall score (0.0 - 100.0 percentage)
    pub score: f64,
    /// Individual results for each example
    pub results: Vec<EvaluationPoint>,
    /// Number of successful evaluations
    pub successful: usize,
    /// Number of failed evaluations
    pub failed: usize,
    /// Total evaluation time
    pub elapsed: std::time::Duration,
}

impl EvaluationResult {
    /// Get average score
    pub fn average_score(&self) -> f64 {
        self.score
    }

    /// Get success rate
    pub fn success_rate(&self) -> f64 {
        self.successful as f64 / self.results.len() as f64
    }

    /// Export to CSV
    pub fn to_csv(&self, path: &str) -> Result<()> {
        // Implementation
    }

    /// Export to JSON
    pub fn to_json(&self, path: &str) -> Result<()> {
        // Implementation
    }
}
```

### 7.2 Enhanced Metric Types

```rust
/// Trace information collected during optimization
#[derive(Debug, Clone)]
pub struct Trace {
    /// Inputs/outputs for each module
    pub module_calls: Vec<ModuleCall>,
    /// Intermediate reasoning steps
    pub reasoning_steps: Vec<String>,
    /// Execution path
    pub path: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ModuleCall {
    pub module_name: String,
    pub inputs: HashMap<String, Value>,
    pub outputs: HashMap<String, Value>,
}

/// Metric result - either bool (bootstrapping) or float (evaluation)
#[derive(Debug, Clone)]
pub enum MetricResult {
    Boolean(bool),
    Score(f64),
}

impl MetricResult {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            MetricResult::Boolean(b) => Some(*b),
            MetricResult::Score(s) => Some(*s > 0.5),
        }
    }

    pub fn as_score(&self) -> f64 {
        match self {
            MetricResult::Boolean(b) => if *b { 1.0 } else { 0.0 },
            MetricResult::Score(s) => *s,
        }
    }
}

/// Enhanced metric function signature
pub type MetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>, Option<&Trace>)
        -> Result<MetricResult, MetricError>
    + Send + Sync
>;

/// Simple metric function (no trace)
pub type SimpleMetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>)
        -> Result<f64, MetricError>
    + Send + Sync
>;
```

### 7.3 Evaluate Struct

```rust
use indicatif::{ProgressBar, ProgressStyle};
use tokio::sync::Semaphore;

/// Evaluator for DSPy programs
pub struct Evaluate {
    /// Development/test dataset
    devset: Vec<Example>,
    /// Metric function (optional, can be provided at call time)
    metric: Option<MetricFn>,
    /// Number of parallel threads/tasks
    num_threads: Option<usize>,
    /// Show progress bar
    display_progress: bool,
    /// Number of examples to display in table
    display_table: Option<usize>,
    /// Maximum errors before stopping
    max_errors: Option<usize>,
    /// Score to assign on failure
    failure_score: f64,
    /// CSV output path
    save_as_csv: Option<String>,
    /// JSON output path
    save_as_json: Option<String>,
}

impl Evaluate {
    /// Create new evaluator
    pub fn new(devset: Vec<Example>) -> Self {
        Self {
            devset,
            metric: None,
            num_threads: None,
            display_progress: false,
            display_table: None,
            max_errors: None,
            failure_score: 0.0,
            save_as_csv: None,
            save_as_json: None,
        }
    }

    /// Builder pattern methods
    pub fn with_metric(mut self, metric: MetricFn) -> Self {
        self.metric = Some(metric);
        self
    }

    pub fn with_num_threads(mut self, n: usize) -> Self {
        self.num_threads = Some(n);
        self
    }

    pub fn with_display_progress(mut self, display: bool) -> Self {
        self.display_progress = display;
        self
    }

    pub fn with_display_table(mut self, n: usize) -> Self {
        self.display_table = Some(n);
        self
    }

    pub fn with_max_errors(mut self, max: usize) -> Self {
        self.max_errors = Some(max);
        self
    }

    pub fn with_failure_score(mut self, score: f64) -> Self {
        self.failure_score = score;
        self
    }

    pub fn save_as_csv(mut self, path: impl Into<String>) -> Self {
        self.save_as_csv = Some(path.into());
        self
    }

    pub fn save_as_json(mut self, path: impl Into<String>) -> Self {
        self.save_as_json = Some(path.into());
        self
    }

    /// Evaluate a program (call syntax)
    pub async fn evaluate(
        &self,
        program: &dyn Module,
        metric: Option<MetricFn>,
    ) -> Result<EvaluationResult, EvaluationError> {
        let metric = metric.or_else(|| self.metric.clone())
            .ok_or(EvaluationError::MissingMetric)?;

        let start = std::time::Instant::now();

        // Setup progress bar
        let pb = if self.display_progress {
            let pb = ProgressBar::new(self.devset.len() as u64);
            pb.set_style(
                ProgressStyle::default_bar()
                    .template("[{elapsed_precise}] {bar:40.cyan/blue} {pos}/{len} {msg}")
                    .unwrap()
            );
            Some(pb)
        } else {
            None
        };

        // Parallel evaluation
        let results = self.evaluate_parallel(program, metric, pb.as_ref()).await?;

        if let Some(pb) = pb {
            pb.finish_with_message("Evaluation complete");
        }

        // Calculate aggregate score
        let total_score: f64 = results.iter()
            .map(|r| r.score)
            .sum();
        let avg_score = (total_score / results.len() as f64) * 100.0;

        let successful = results.iter().filter(|r| r.error.is_none()).count();
        let failed = results.len() - successful;

        let result = EvaluationResult {
            score: avg_score,
            results,
            successful,
            failed,
            elapsed: start.elapsed(),
        };

        // Display table if requested
        if let Some(n) = self.display_table {
            self.display_results_table(&result, n);
        }

        // Save outputs if requested
        if let Some(path) = &self.save_as_csv {
            result.to_csv(path)?;
        }
        if let Some(path) = &self.save_as_json {
            result.to_json(path)?;
        }

        Ok(result)
    }

    /// Parallel evaluation implementation
    async fn evaluate_parallel(
        &self,
        program: &dyn Module,
        metric: MetricFn,
        pb: Option<&ProgressBar>,
    ) -> Result<Vec<EvaluationPoint>, EvaluationError> {
        let num_threads = self.num_threads.unwrap_or_else(|| {
            std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(4)
        });

        let semaphore = Arc::new(Semaphore::new(num_threads));
        let error_count = Arc::new(AtomicUsize::new(0));
        let program = Arc::new(program);
        let metric = Arc::new(metric);

        let mut join_set = JoinSet::new();

        for example in self.devset.clone() {
            let permit = semaphore.clone().acquire_owned().await?;
            let program = program.clone();
            let metric = metric.clone();
            let error_count = error_count.clone();
            let max_errors = self.max_errors;
            let failure_score = self.failure_score;
            let pb = pb.map(|p| p.clone());

            join_set.spawn(async move {
                let _permit = permit;

                // Check error threshold
                if let Some(max) = max_errors {
                    if error_count.load(Ordering::Relaxed) >= max {
                        return EvaluationPoint {
                            example: example.clone(),
                            prediction: HashMap::new(),
                            score: failure_score,
                            error: Some("Max errors reached".to_string()),
                        };
                    }
                }

                // Run program
                let prediction = match program.forward(example.inputs.clone()).await {
                    Ok(pred) => pred,
                    Err(e) => {
                        error_count.fetch_add(1, Ordering::Relaxed);
                        if let Some(pb) = &pb {
                            pb.inc(1);
                        }
                        return EvaluationPoint {
                            example: example.clone(),
                            prediction: HashMap::new(),
                            score: failure_score,
                            error: Some(e.to_string()),
                        };
                    }
                };

                // Evaluate with metric
                let score = match metric(&example, &prediction, None) {
                    Ok(result) => result.as_score(),
                    Err(e) => {
                        error_count.fetch_add(1, Ordering::Relaxed);
                        if let Some(pb) = &pb {
                            pb.inc(1);
                        }
                        return EvaluationPoint {
                            example: example.clone(),
                            prediction,
                            score: failure_score,
                            error: Some(e.to_string()),
                        };
                    }
                };

                if let Some(pb) = &pb {
                    pb.inc(1);
                }

                EvaluationPoint {
                    example,
                    prediction,
                    score,
                    error: None,
                }
            });
        }

        // Collect results
        let mut results = Vec::new();
        while let Some(result) = join_set.join_next().await {
            results.push(result?);
        }

        Ok(results)
    }

    /// Display results table
    fn display_results_table(&self, result: &EvaluationResult, num_rows: usize) {
        use prettytable::{Table, Row, Cell};

        let mut table = Table::new();
        table.add_row(Row::new(vec![
            Cell::new("Input"),
            Cell::new("Expected"),
            Cell::new("Predicted"),
            Cell::new("Score"),
        ]));

        for point in result.results.iter().take(num_rows) {
            // Extract first input/output fields for display
            let input = point.example.inputs.values().next()
                .and_then(|v| v.as_str())
                .unwrap_or("N/A");
            let expected = point.example.outputs.values().next()
                .and_then(|v| v.as_str())
                .unwrap_or("N/A");
            let predicted = point.prediction.values().next()
                .and_then(|v| v.as_str())
                .unwrap_or("N/A");

            table.add_row(Row::new(vec![
                Cell::new(&truncate(input, 30)),
                Cell::new(&truncate(expected, 20)),
                Cell::new(&truncate(predicted, 20)),
                Cell::new(&format!("{:.2}", point.score)),
            ]));
        }

        table.printstd();
        println!("\nOverall Score: {:.2}%", result.score);
        println!("Success Rate: {:.2}%", result.success_rate() * 100.0);
    }
}

fn truncate(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len - 3])
    }
}
```

### 7.4 Built-in Metrics

```rust
/// Built-in metrics module
pub mod metrics {
    use super::*;

    /// Exact match metric
    pub fn answer_exact_match(
        field: &str,
    ) -> SimpleMetricFn {
        let field = field.to_string();
        Arc::new(move |example, pred| {
            let expected = example.outputs.get(&field);
            let actual = pred.get(&field);

            match (expected, actual) {
                (Some(exp), Some(act)) => {
                    Ok(if exp == act { 1.0 } else { 0.0 })
                }
                _ => Ok(0.0),
            }
        })
    }

    /// Case-insensitive exact match
    pub fn answer_exact_match_ci(
        field: &str,
    ) -> SimpleMetricFn {
        let field = field.to_string();
        Arc::new(move |example, pred| {
            let expected = example.outputs.get(&field)
                .and_then(|v| v.as_str());
            let actual = pred.get(&field)
                .and_then(|v| v.as_str());

            match (expected, actual) {
                (Some(exp), Some(act)) => {
                    Ok(if exp.to_lowercase() == act.to_lowercase() {
                        1.0
                    } else {
                        0.0
                    })
                }
                _ => Ok(0.0),
            }
        })
    }

    /// Substring match metric
    pub fn answer_passage_match(
        answer_field: &str,
        passage_field: &str,
    ) -> SimpleMetricFn {
        let answer_field = answer_field.to_string();
        let passage_field = passage_field.to_string();

        Arc::new(move |example, pred| {
            let answer = pred.get(&answer_field)
                .and_then(|v| v.as_str());
            let passages = pred.get(&passage_field)
                .and_then(|v| v.as_array());

            match (answer, passages) {
                (Some(ans), Some(passages)) => {
                    let found = passages.iter().any(|p| {
                        p.as_str()
                            .map(|s| s.contains(ans))
                            .unwrap_or(false)
                    });
                    Ok(if found { 1.0 } else { 0.0 })
                }
                _ => Ok(0.0),
            }
        })
    }

    /// F1 score calculation
    pub fn f1_score(precision: f64, recall: f64) -> f64 {
        let precision = precision.clamp(0.0, 1.0);
        let recall = recall.clamp(0.0, 1.0);

        if precision + recall == 0.0 {
            0.0
        } else {
            2.0 * (precision * recall) / (precision + recall)
        }
    }

    /// Semantic F1 metric using LLM evaluation
    pub struct SemanticF1 {
        threshold: f64,
        evaluator: Arc<dyn Module>,
    }

    impl SemanticF1 {
        pub fn new(threshold: f64) -> Self {
            // Create evaluation signature
            let signature = Signature::new(
                "SemanticRecallPrecision",
                "Compare system response to ground truth"
            )
            .with_input(InputField::new("ground_truth", "Expected answer", "String"))
            .with_input(InputField::new("prediction", "System response", "String"))
            .with_output(OutputField::new("recall", "Recall score 0-1", "Float"))
            .with_output(OutputField::new("precision", "Precision score 0-1", "Float"));

            let evaluator = Arc::new(
                ChainOfThought::new(signature)
            );

            Self { threshold, evaluator }
        }

        pub fn as_metric(&self) -> SimpleMetricFn {
            let evaluator = self.evaluator.clone();
            let threshold = self.threshold;

            Arc::new(move |example, pred| {
                let ground_truth = example.outputs.values().next()
                    .and_then(|v| v.as_str())
                    .ok_or(MetricError::MissingField("ground_truth".into()))?;

                let prediction = pred.values().next()
                    .and_then(|v| v.as_str())
                    .ok_or(MetricError::MissingField("prediction".into()))?;

                // Call LLM evaluator
                let mut inputs = HashMap::new();
                inputs.insert("ground_truth".to_string(), json!(ground_truth));
                inputs.insert("prediction".to_string(), json!(prediction));

                let result = evaluator.forward(inputs).await?;

                let recall = result.get("recall")
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0);
                let precision = result.get("precision")
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0);

                let f1 = f1_score(precision, recall);

                Ok(if f1 >= threshold { 1.0 } else { f1 })
            })
        }
    }
}
```

### 7.5 Usage Examples

```rust
use ggen_ai::dspy::{
    evaluate::{Evaluate, EvaluationResult},
    metrics,
    Example,
    Module,
};

#[tokio::main]
async fn main() -> Result<()> {
    // Example 1: Simple evaluation with built-in metric
    let evaluator = Evaluate::new(devset)
        .with_metric(metrics::answer_exact_match("answer"))
        .with_num_threads(8)
        .with_display_progress(true)
        .with_display_table(10);

    let result = evaluator.evaluate(&program, None).await?;
    println!("Score: {:.2}%", result.score);

    // Example 2: Custom metric
    let custom_metric = Arc::new(
        |example: &Example, pred: &HashMap<String, Value>, trace: Option<&Trace>| {
            let expected = example.outputs.get("answer");
            let actual = pred.get("answer");

            let matches = expected == actual;

            if trace.is_some() {
                // Bootstrapping mode
                Ok(MetricResult::Boolean(matches))
            } else {
                // Evaluation mode
                Ok(MetricResult::Score(if matches { 1.0 } else { 0.0 }))
            }
        }
    );

    let result = evaluator.evaluate(&program, Some(custom_metric)).await?;

    // Example 3: Multi-metric evaluation
    let metrics_list = vec![
        ("exact_match", metrics::answer_exact_match("answer")),
        ("case_insensitive", metrics::answer_exact_match_ci("answer")),
    ];

    for (name, metric) in metrics_list {
        let evaluator = Evaluate::new(devset.clone())
            .with_num_threads(8);
        let result = evaluator.evaluate(&program, Some(metric)).await?;
        println!("{}: {:.2}%", name, result.score);
    }

    // Example 4: With export
    let evaluator = Evaluate::new(devset)
        .with_metric(metrics::answer_exact_match("answer"))
        .with_num_threads(16)
        .with_display_progress(true)
        .save_as_csv("results.csv")
        .save_as_json("results.json");

    let result = evaluator.evaluate(&program, None).await?;

    Ok(())
}
```

### 7.6 Integration with Optimizer

Update `BootstrapFewShot` to use enhanced metrics:

```rust
impl BootstrapFewShot {
    pub fn new(metric: MetricFn) -> Self {
        // MetricFn now supports trace parameter and MetricResult
        Self {
            metric,
            max_bootstrapped_demos: 4,
            max_labeled_demos: 16,
            teacher: None,
        }
    }

    pub async fn compile(
        &self,
        student: &dyn Module,
        trainset: &[Example],
    ) -> Result<OptimizedPredictor, ModuleError> {
        // ... existing code ...

        // Evaluate with trace for bootstrapping
        let trace = self.collect_trace(&predictor, &example).await?;

        let is_successful = match (self.metric)(
            example,
            &output,
            Some(&trace)  // Pass trace during bootstrapping
        ) {
            Ok(MetricResult::Boolean(b)) => b,
            Ok(MetricResult::Score(s)) => s > 0.5,  // Convert score to bool
            Err(e) => {
                warn!("Metric evaluation failed: {}", e);
                continue;
            }
        };

        // ... rest of compilation ...
    }

    async fn collect_trace(
        &self,
        predictor: &dyn Module,
        example: &Example,
    ) -> Result<Trace, ModuleError> {
        // Implementation to collect trace information
        Ok(Trace {
            module_calls: vec![],
            reasoning_steps: vec![],
            path: vec![],
        })
    }
}
```

---

## 8. Implementation Roadmap

### Phase 1: Core Evaluation (Week 1)

**Goals**:
- Implement `EvaluationPoint` and `EvaluationResult`
- Create basic `Evaluate` struct with builder pattern
- Support sequential evaluation (no parallelism yet)

**Deliverables**:
```rust
// Basic evaluator
let evaluator = Evaluate::new(devset)
    .with_metric(my_metric);
let result = evaluator.evaluate(&program, None).await?;
```

### Phase 2: Parallel Processing (Week 2)

**Goals**:
- Add `num_threads` support with `JoinSet`
- Implement semaphore-based concurrency control
- Add progress bar with `indicatif`

**Deliverables**:
```rust
let evaluator = Evaluate::new(devset)
    .with_num_threads(16)
    .with_display_progress(true);
```

### Phase 3: Enhanced Metrics (Week 3)

**Goals**:
- Implement `Trace` type and collection
- Update `MetricFn` to support `Option<&Trace>` and `MetricResult`
- Create built-in metrics: `answer_exact_match`, `answer_exact_match_ci`, `answer_passage_match`

**Deliverables**:
```rust
use ggen_ai::dspy::metrics;

let metric = metrics::answer_exact_match("answer");
```

### Phase 4: Display and Export (Week 4)

**Goals**:
- Implement `display_table` with `prettytable`
- Add CSV export with `csv` crate
- Add JSON export with `serde_json`

**Deliverables**:
```rust
let evaluator = Evaluate::new(devset)
    .with_display_table(10)
    .save_as_csv("results.csv")
    .save_as_json("results.json");
```

### Phase 5: LLM-Based Metrics (Week 5)

**Goals**:
- Implement `SemanticF1` metric using DSPy module
- Create evaluation signatures for recall/precision
- Support F1 score calculation

**Deliverables**:
```rust
let semantic_f1 = SemanticF1::new(0.7);
let metric = semantic_f1.as_metric();
```

### Phase 6: Optimizer Integration (Week 6)

**Goals**:
- Update `BootstrapFewShot` to collect and use traces
- Support trace-aware metric validation
- Enable intermediate step validation

**Deliverables**:
```rust
// Metric with trace support
let metric = Arc::new(|ex, pred, trace| {
    if let Some(t) = trace {
        // Validate intermediate steps
        validate_reasoning(ex, pred, t)
    } else {
        // Standard evaluation
        score_output(ex, pred)
    }
});
```

---

## 9. Testing Strategy

### 9.1 Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evaluation_result_creation() {
        let results = vec![
            EvaluationPoint {
                example: create_example(),
                prediction: create_prediction(),
                score: 1.0,
                error: None,
            },
        ];

        let result = EvaluationResult {
            score: 100.0,
            results,
            successful: 1,
            failed: 0,
            elapsed: Duration::from_secs(1),
        };

        assert_eq!(result.average_score(), 100.0);
        assert_eq!(result.success_rate(), 1.0);
    }

    #[tokio::test]
    async fn test_simple_evaluation() {
        let devset = vec![create_example()];
        let metric = metrics::answer_exact_match("answer");
        let program = MockModule::new();

        let evaluator = Evaluate::new(devset)
            .with_metric(metric);

        let result = evaluator.evaluate(&program, None).await.unwrap();
        assert!(result.score >= 0.0 && result.score <= 100.0);
    }

    #[tokio::test]
    async fn test_parallel_evaluation() {
        let devset = create_large_devset(100);
        let metric = metrics::answer_exact_match("answer");
        let program = MockModule::new();

        let evaluator = Evaluate::new(devset)
            .with_metric(metric)
            .with_num_threads(8);

        let result = evaluator.evaluate(&program, None).await.unwrap();
        assert_eq!(result.results.len(), 100);
    }

    #[test]
    fn test_metric_result_conversion() {
        let bool_result = MetricResult::Boolean(true);
        assert_eq!(bool_result.as_score(), 1.0);
        assert_eq!(bool_result.as_bool(), Some(true));

        let score_result = MetricResult::Score(0.75);
        assert_eq!(score_result.as_score(), 0.75);
        assert_eq!(score_result.as_bool(), Some(true));
    }
}
```

### 9.2 Integration Tests

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_evaluation_with_real_llm() {
        // Skip if no LLM configured
        if std::env::var("GGEN_LLM_MODEL").is_err() {
            return;
        }

        let signature = Signature::new("QA", "Answer questions")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        let program = Predictor::new(signature);

        let devset = vec![
            Example::new(
                hashmap!{"question" => json!("What is 2+2?")},
                hashmap!{"answer" => json!("4")},
            ),
        ];

        let metric = metrics::answer_exact_match("answer");
        let evaluator = Evaluate::new(devset)
            .with_metric(metric)
            .with_display_progress(true);

        let result = evaluator.evaluate(&program, None).await.unwrap();
        assert!(result.successful > 0 || result.failed > 0);
    }

    #[tokio::test]
    async fn test_export_functionality() {
        let devset = create_test_devset();
        let program = MockModule::new();
        let metric = metrics::answer_exact_match("answer");

        let temp_dir = tempfile::tempdir().unwrap();
        let csv_path = temp_dir.path().join("results.csv");
        let json_path = temp_dir.path().join("results.json");

        let evaluator = Evaluate::new(devset)
            .with_metric(metric)
            .save_as_csv(csv_path.to_str().unwrap())
            .save_as_json(json_path.to_str().unwrap());

        let result = evaluator.evaluate(&program, None).await.unwrap();

        assert!(csv_path.exists());
        assert!(json_path.exists());
    }
}
```

### 9.3 Benchmark Tests

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_evaluation(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("evaluate_100_examples_sequential", |b| {
        b.to_async(&rt).iter(|| async {
            let devset = create_devset(100);
            let program = MockModule::new();
            let metric = metrics::answer_exact_match("answer");

            let evaluator = Evaluate::new(devset)
                .with_metric(metric);

            black_box(evaluator.evaluate(&program, None).await.unwrap())
        });
    });

    c.bench_function("evaluate_100_examples_parallel_8", |b| {
        b.to_async(&rt).iter(|| async {
            let devset = create_devset(100);
            let program = MockModule::new();
            let metric = metrics::answer_exact_match("answer");

            let evaluator = Evaluate::new(devset)
                .with_metric(metric)
                .with_num_threads(8);

            black_box(evaluator.evaluate(&program, None).await.unwrap())
        });
    });
}

criterion_group!(benches, benchmark_evaluation);
criterion_main!(benches);
```

---

## 10. Key Takeaways

### 10.1 DSPy Evaluation Strengths

1. **Unified Interface**: The `Evaluate` class provides a consistent API for all evaluation scenarios
2. **Parallel-First**: Built-in threading support makes large-scale evaluation efficient
3. **Metric Flexibility**: Support for simple functions to complex DSPy programs as metrics
4. **Trace Integration**: Deep optimizer integration via trace-aware metrics
5. **Developer Experience**: Progress bars, table display, and export capabilities aid rapid iteration

### 10.2 Implementation Priorities for Rust

**Must-Have (Phase 1-2)**:
- ✅ `Evaluate` struct with builder pattern
- ✅ Parallel evaluation with `tokio::task::JoinSet`
- ✅ Progress tracking with `indicatif`
- ✅ Basic metric types and built-ins

**Should-Have (Phase 3-4)**:
- ✅ Enhanced `MetricFn` with `trace` and `MetricResult`
- ✅ Display table with `prettytable`
- ✅ CSV/JSON export
- ✅ Error handling with `max_errors`

**Nice-to-Have (Phase 5-6)**:
- ✅ LLM-based metrics (SemanticF1, CompleteAndGrounded)
- ✅ Trace collection infrastructure
- ✅ Intermediate step validation
- ✅ Metric composition utilities

### 10.3 Architectural Decisions

**1. Use `MetricResult` enum instead of complex type gymnastics**
- Allows both bool and float returns cleanly
- Provides conversion helpers for flexibility

**2. Separate `SimpleMetricFn` and `MetricFn` types**
- Simple metrics don't need trace complexity
- Advanced metrics can opt-in to trace awareness

**3. Builder pattern for `Evaluate`**
- Matches Rust idioms
- Allows flexible configuration
- Clear, readable code at call site

**4. `tokio::task::JoinSet` for parallelism**
- Natural fit for async DSPy modules
- Better than thread pools for I/O-bound LLM calls
- Clean error propagation

**5. Progress bar and display as opt-in features**
- Developer experience enhancement
- Easy to disable for production use
- No performance overhead when disabled

---

## Sources

### Primary Documentation
- [DSPy Evaluate API](https://dspy.ai/api/evaluation/Evaluate/)
- [DSPy Metrics Documentation](https://dspy.ai/learn/evaluation/metrics/)
- [DSPy Evaluation Overview](https://dspy.ai/learn/evaluation/overview/)
- [DSPy Optimizers](https://dspy.ai/learn/optimization/optimizers/)
- [DSPy Cheatsheet](https://dspy.ai/cheatsheet/)

### Source Code
- [DSPy GitHub Repository](https://github.com/stanfordnlp/dspy)
- [Auto-Evaluation Implementation](https://github.com/stanfordnlp/dspy/blob/main/dspy/evaluate/auto_evaluation.py)
- [Metrics Implementation (GitHub)](https://github.com/stanfordnlp/dspy/blob/main/docs/docs/learn/evaluation/metrics.md)
- [Evaluation Overview (GitHub)](https://github.com/stanfordnlp/dspy/blob/main/docs/docs/learn/evaluation/overview.md)

### Tutorials and Examples
- [Creating Metrics in DSPy (CodeSignal Learn)](https://codesignal.com/learn/courses/evaluation-in-dspy/lessons/creating-metrics-in-dspy-1)
- [DSPy Evaluator and Optimizer (Medium)](https://ritikjain51.medium.com/dspy-evaluator-and-optimizer-698e776f914a)
- [Understanding Optimizers in DSPy (Medium)](https://medium.com/the-modern-scientist/understanding-optimizers-in-dspy-1ea9451c128b)
- [Evaluating and Optimizing LLM Applications with DSPy (Pedram Navid)](https://pedramnavid.com/blog/dspy-evals/)
- [Prompt Optimization with DSPy and G-Eval Metrics (Medium)](https://medium.com/@a-romero/prompt-optimization-with-dspy-and-g-eval-metrics-e7d0bdd21b8b)
- [Prompt Optimization with DSPy (Haystack)](https://haystack.deepset.ai/cookbook/prompt_optimization_with_dspy)

---

**End of Document**
