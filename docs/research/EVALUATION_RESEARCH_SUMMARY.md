# DSPy Evaluation System Research - Executive Summary

**Date**: 2026-01-11
**Research Focus**: Python DSPy Evaluation Framework
**Outcome**: Comprehensive architecture recommendations for Rust implementation

---

## Research Deliverables

1. **[dspy-evaluation-system.md](./dspy-evaluation-system.md)** - Complete technical documentation (10 sections, 50+ pages)
2. **[dspy-evaluation-examples.md](./dspy-evaluation-examples.md)** - Implementation patterns and code examples
3. **This summary** - Executive overview and action items

---

## Key Findings

### 1. DSPy Evaluate Class Architecture

**Core Functionality**:
- Centralized evaluation interface with `Evaluate` class
- Parallel processing via `num_threads` parameter (critical for scale)
- Progress tracking and result display for developer experience
- Export to CSV/JSON for analysis and reporting

**Constructor Parameters**:
```python
Evaluate(
    devset,                    # Required: evaluation dataset
    metric=None,               # Optional: metric function
    num_threads=None,          # Parallel workers
    display_progress=False,    # Show progress bar
    display_table=False|int,   # Display N sample results
    max_errors=None,           # Fail-fast threshold
    failure_score=0.0,         # Default score on error
    save_as_csv=None,          # CSV export path
    save_as_json=None          # JSON export path
)
```

**Return Type**: `EvaluationResult` with:
- `score`: Overall percentage (0-100)
- `results`: List of `(example, prediction, score)` tuples

### 2. Metrics Framework

**Metric Signature**:
```python
def metric(example, pred, trace=None) -> float | bool
```

**Three Parameters**:
1. `example`: Ground truth from dataset (inputs + expected outputs)
2. `pred`: Actual program output
3. `trace`: Optional execution trace (enables dual-mode behavior)

**Dual-Mode Behavior**:
- `trace=None` → Return `float` for evaluation/optimization
- `trace=Some` → Return `bool` for bootstrapping demonstrations

**Built-in Metrics**:
- `answer_exact_match`: Simple equality check
- `answer_passage_match`: Substring presence in context
- `SemanticF1`: LLM-based recall/precision evaluation
- `CompleteAndGrounded`: Multi-dimensional completeness + grounding

**Custom Metric Examples**:
```python
# Simple validation
def exact_match(example, pred, trace=None):
    return example.answer == pred.answer

# Dual-mode adaptive
def adaptive(example, pred, trace=None):
    correct = example.answer == pred.answer
    if trace is None:
        return 1.0 if correct else 0.0  # Float for evaluation
    else:
        return correct  # Bool for bootstrapping

# AI-based evaluation
class Judge(dspy.Signature):
    answer = dspy.InputField()
    is_correct = dspy.OutputField()

judge = dspy.ChainOfThought(Judge)

def ai_metric(example, pred, trace=None):
    result = judge(answer=pred.answer)
    is_correct = result.is_correct.lower() == 'yes'
    return 1.0 if is_correct else 0.0
```

### 3. Parallel Processing

**Threading Model**:
- Thread-pool based parallelism
- Configurable via `num_threads` parameter
- Progress bar tracks completion across threads
- Automatic result aggregation

**Performance Benefits**:
- Linear speedup for I/O-bound LLM calls
- Typical: 8-16 threads for balanced throughput
- Large datasets: Can scale to 32+ threads

### 4. Optimizer Integration

**Trace Collection**:
- During compilation/optimization, DSPy traces all LM calls
- Trace contains: inputs, outputs, reasoning steps, execution path
- Enables validation of intermediate steps, not just final output

**BootstrapFewShot Integration**:
```python
def validation_metric(example, pred, trace=None):
    if trace:
        # Bootstrapping: validate intermediate steps
        return check_reasoning(trace) and check_answer(pred)
    else:
        # Evaluation: score final output
        return compute_score(example, pred)

optimizer = BootstrapFewShot(metric=validation_metric)
compiled = optimizer.compile(student, trainset)
```

**MIPROv2 Integration**:
- Uses metrics to guide Bayesian optimization
- Evaluates instruction/demonstration candidates
- Selects best configuration based on metric scores

### 5. Best Practices

**Data Collection**:
- 20-200 examples for initial development set
- Only inputs and final outputs needed (no intermediate labels)
- Sources: HuggingFace, production logs, synthetic generation

**Metric Development Workflow**:
1. Start simple (exact match)
2. Run evaluation, inspect failures via `display_table`
3. Iteratively refine metric based on observed failure modes
4. Add complexity only when justified

**Evaluation Patterns**:
- **Baseline Establishment**: Compare zero-shot, few-shot, optimized variants
- **Progressive Validation**: Test on easy → medium → hard subsets
- **Multi-Metric**: Evaluate across multiple dimensions simultaneously
- **Hyperparameter Search**: Grid search with evaluation loop

---

## Current Rust Implementation Status

### What We Have ✅

**File**: `/home/user/ggen/crates/ggen-ai/src/dspy/optimizer.rs`

```rust
// Metric type (simplified, bootstrap-only)
pub type MetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>)
        -> Result<bool, ModuleError>
    + Send + Sync
>;

// Example and Demonstration types
pub struct Example {
    pub inputs: HashMap<String, Value>,
    pub outputs: HashMap<String, Value>,
}

pub struct Demonstration {
    pub inputs: HashMap<String, Value>,
    pub outputs: HashMap<String, Value>,
}

// BootstrapFewShot optimizer with metric integration
pub struct BootstrapFewShot {
    metric: MetricFn,
    max_bootstrapped_demos: usize,
    max_labeled_demos: usize,
    teacher: Option<Arc<dyn Module>>,
}

impl BootstrapFewShot {
    pub async fn compile(
        &self,
        student: &dyn Module,
        trainset: &[Example],
    ) -> Result<OptimizedPredictor, ModuleError> {
        // Uses metric to validate demonstrations
        let is_successful = (self.metric)(example, &output)?;
        // ...
    }
}
```

**Strengths**:
- Working optimizer with metric validation
- Async module execution
- Type-safe Example/Demonstration structures

### What We're Missing ❌

**No Evaluation Framework**:
- ❌ No `Evaluate` struct
- ❌ No parallel evaluation
- ❌ No result aggregation
- ❌ No progress tracking
- ❌ No CSV/JSON export

**Limited Metric System**:
- ❌ Only `bool` return type (no `float` for scoring)
- ❌ No `trace` parameter (no dual-mode support)
- ❌ No built-in metrics library
- ❌ No LLM-based evaluators (SemanticF1, etc.)

**Missing Infrastructure**:
- ❌ No `Trace` type for execution tracking
- ❌ No `EvaluationResult` aggregation
- ❌ No error handling with `max_errors`
- ❌ No display utilities

---

## Recommended Rust Architecture

### Core Types

```rust
/// Trace collected during optimization
#[derive(Debug, Clone)]
pub struct Trace {
    pub module_calls: Vec<ModuleCall>,
    pub reasoning_steps: Vec<String>,
    pub path: Vec<String>,
}

/// Metric result - dual mode
#[derive(Debug, Clone)]
pub enum MetricResult {
    Boolean(bool),   // Bootstrapping
    Score(f64),      // Evaluation
}

impl MetricResult {
    pub fn as_bool(&self) -> Option<bool> { /* ... */ }
    pub fn as_score(&self) -> f64 { /* ... */ }
}

/// Enhanced metric with trace support
pub type MetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>, Option<&Trace>)
        -> Result<MetricResult, MetricError>
    + Send + Sync
>;

/// Simple metric (no trace)
pub type SimpleMetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>)
        -> Result<f64, MetricError>
    + Send + Sync
>;

/// Single evaluation point
#[derive(Debug, Clone)]
pub struct EvaluationPoint {
    pub example: Example,
    pub prediction: HashMap<String, Value>,
    pub score: f64,
    pub error: Option<String>,
}

/// Aggregated results
#[derive(Debug, Clone)]
pub struct EvaluationResult {
    pub score: f64,              // Overall percentage
    pub results: Vec<EvaluationPoint>,
    pub successful: usize,
    pub failed: usize,
    pub elapsed: Duration,
}

impl EvaluationResult {
    pub fn average_score(&self) -> f64 { /* ... */ }
    pub fn success_rate(&self) -> f64 { /* ... */ }
    pub fn to_csv(&self, path: &str) -> Result<()> { /* ... */ }
    pub fn to_json(&self, path: &str) -> Result<()> { /* ... */ }
}
```

### Evaluate Struct

```rust
pub struct Evaluate {
    devset: Vec<Example>,
    metric: Option<MetricFn>,
    num_threads: Option<usize>,
    display_progress: bool,
    display_table: Option<usize>,
    max_errors: Option<usize>,
    failure_score: f64,
    save_as_csv: Option<String>,
    save_as_json: Option<String>,
}

impl Evaluate {
    pub fn new(devset: Vec<Example>) -> Self { /* ... */ }

    // Builder methods
    pub fn with_metric(mut self, metric: MetricFn) -> Self { /* ... */ }
    pub fn with_num_threads(mut self, n: usize) -> Self { /* ... */ }
    pub fn with_display_progress(mut self, display: bool) -> Self { /* ... */ }
    pub fn with_display_table(mut self, n: usize) -> Self { /* ... */ }

    // Main evaluation method
    pub async fn evaluate(
        &self,
        program: &dyn Module,
        metric: Option<MetricFn>,
    ) -> Result<EvaluationResult, EvaluationError> {
        // Parallel evaluation with tokio::task::JoinSet
        // Progress bar with indicatif
        // Result aggregation
        // Optional export
    }
}
```

### Built-in Metrics

```rust
pub mod metrics {
    /// Exact match
    pub fn answer_exact_match(field: &str) -> SimpleMetricFn { /* ... */ }

    /// Case-insensitive match
    pub fn answer_exact_match_ci(field: &str) -> SimpleMetricFn { /* ... */ }

    /// Passage match
    pub fn answer_passage_match(
        answer_field: &str,
        passage_field: &str,
    ) -> SimpleMetricFn { /* ... */ }

    /// F1 calculation
    pub fn f1_score(precision: f64, recall: f64) -> f64 { /* ... */ }

    /// Semantic F1 (LLM-based)
    pub struct SemanticF1 {
        threshold: f64,
        evaluator: Arc<dyn Module>,
    }

    impl SemanticF1 {
        pub fn new(threshold: f64) -> Self { /* ... */ }
        pub fn as_metric(&self) -> SimpleMetricFn { /* ... */ }
    }
}
```

### Usage Example

```rust
use ggen_ai::dspy::{
    evaluate::Evaluate,
    metrics,
    Example,
};

#[tokio::main]
async fn main() -> Result<()> {
    // Create evaluator
    let evaluator = Evaluate::new(devset)
        .with_metric(metrics::answer_exact_match("answer"))
        .with_num_threads(16)
        .with_display_progress(true)
        .with_display_table(10)
        .save_as_csv("results.csv");

    // Evaluate
    let result = evaluator.evaluate(&program, None).await?;

    println!("Score: {:.2}%", result.score);
    println!("Success rate: {:.2}%", result.success_rate() * 100.0);

    Ok(())
}
```

---

## Implementation Roadmap

### Phase 1: Core Evaluation (Week 1)
**Goal**: Basic sequential evaluation

**Tasks**:
- [ ] Define `EvaluationPoint` and `EvaluationResult` types
- [ ] Create `Evaluate` struct with builder pattern
- [ ] Implement sequential evaluation loop
- [ ] Basic error handling

**Deliverable**: Working evaluator without parallelism

**SLO**: Single-threaded evaluation completes <5s for 100 examples

### Phase 2: Parallel Processing (Week 2)
**Goal**: Efficient multi-threaded evaluation

**Tasks**:
- [ ] Implement `evaluate_parallel()` with `tokio::task::JoinSet`
- [ ] Add semaphore-based concurrency control
- [ ] Integrate `indicatif` progress bar
- [ ] Test with 1000+ examples

**Deliverable**: Parallel evaluation with progress tracking

**SLO**: 16-thread evaluation 8x faster than sequential

### Phase 3: Enhanced Metrics (Week 3)
**Goal**: Dual-mode metrics with trace support

**Tasks**:
- [ ] Define `Trace` type and `ModuleCall` structure
- [ ] Create `MetricResult` enum (Boolean | Score)
- [ ] Update `MetricFn` type signature
- [ ] Implement built-in metrics module
  - [ ] `answer_exact_match`
  - [ ] `answer_exact_match_ci`
  - [ ] `answer_passage_match`
  - [ ] `f1_score` utility

**Deliverable**: Rich metric system with trace awareness

**Test**: Bootstrap validation using dual-mode metrics

### Phase 4: Display and Export (Week 4)
**Goal**: Developer experience and result persistence

**Tasks**:
- [ ] Implement `display_results_table()` with `prettytable`
- [ ] Add CSV export with `csv` crate
- [ ] Add JSON export with `serde_json`
- [ ] Format display with proper truncation

**Deliverable**: Results display and export capabilities

**Receipt**: Successfully export 1000 evaluations to CSV/JSON <1s

### Phase 5: LLM-Based Metrics (Week 5)
**Goal**: AI-powered evaluation

**Tasks**:
- [ ] Create evaluation signatures (SemanticRecallPrecision)
- [ ] Implement `SemanticF1` metric class
- [ ] Support ChainOfThought evaluator wrapping
- [ ] Add threshold-based pass/fail logic

**Deliverable**: LLM-based metrics matching Python DSPy

**Test**: SemanticF1 achieves >90% agreement with human judgment

### Phase 6: Optimizer Integration (Week 6)
**Goal**: Full trace-aware optimization

**Tasks**:
- [ ] Implement trace collection in `Module::forward()`
- [ ] Update `BootstrapFewShot` to use dual-mode metrics
- [ ] Support intermediate step validation
- [ ] Add trace-aware metric examples

**Deliverable**: Complete optimization pipeline with trace validation

**Receipt**: BootstrapFewShot with trace validation improves accuracy by 15%+

---

## Dependencies

### Required Crates

```toml
[dependencies]
# Existing
tokio = { version = "1.47", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
async-trait = "0.1"

# New for evaluation
indicatif = "0.17"        # Progress bars
prettytable-rs = "0.10"   # Table display
csv = "1.3"               # CSV export
dashmap = "6.0"           # Thread-safe maps (for metrics)
parking_lot = "0.12"      # Fast synchronization

[dev-dependencies]
criterion = "0.5"         # Benchmarking
tempfile = "3.0"          # Temporary files for tests
```

---

## Success Metrics

### Functional Requirements
- ✅ Sequential evaluation completes successfully
- ✅ Parallel evaluation with 16 threads
- ✅ Progress bar displays during evaluation
- ✅ Results table shows sample outputs
- ✅ CSV/JSON export works correctly
- ✅ Built-in metrics (exact match, passage match, f1)
- ✅ LLM-based metrics (SemanticF1)
- ✅ Trace collection and validation
- ✅ Dual-mode metrics (bool/float)

### Performance Requirements (SLOs)
- Evaluation overhead: <10ms per example
- 16-thread speedup: >12x vs sequential (75% efficiency)
- Progress bar update frequency: 10Hz minimum
- CSV export: <1s for 1000 results
- Memory overhead: <100MB for 10k examples

### Quality Requirements
- Zero `unwrap/expect` in production code
- All errors returned as `Result<T, E>`
- 100% test coverage for core evaluation logic
- Benchmark suite validates performance claims
- Documentation with examples for all public APIs

---

## Testing Strategy

### Unit Tests
```rust
#[tokio::test]
async fn test_basic_evaluation() { /* ... */ }

#[tokio::test]
async fn test_parallel_evaluation() { /* ... */ }

#[test]
fn test_metric_result_conversions() { /* ... */ }

#[test]
fn test_evaluation_result_aggregation() { /* ... */ }
```

### Integration Tests
```rust
#[tokio::test]
async fn test_evaluation_with_real_llm() { /* ... */ }

#[tokio::test]
async fn test_csv_json_export() { /* ... */ }

#[tokio::test]
async fn test_optimizer_integration() { /* ... */ }
```

### Benchmarks
```rust
fn benchmark_evaluation(c: &mut Criterion) {
    c.bench_function("evaluate_100_sequential", /* ... */);
    c.bench_function("evaluate_100_parallel_16", /* ... */);
    c.bench_function("custom_metric_overhead", /* ... */);
}
```

---

## Migration Path

### Phase 1: Non-Breaking Addition
Add new evaluation framework alongside existing optimizer:

```rust
// Existing code continues to work
let optimizer = BootstrapFewShot::new(old_metric);

// New evaluation framework available
let evaluator = Evaluate::new(devset)
    .with_metric(new_metric);
```

### Phase 2: Deprecation
Mark old `MetricFn` as deprecated, encourage migration:

```rust
#[deprecated(since = "0.7.0", note = "Use new MetricFn with trace support")]
pub type OldMetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>) -> Result<bool>>;
```

### Phase 3: Migration
Update examples and documentation to use new API:

```rust
// Old (deprecated)
let metric = Arc::new(|ex, out| Ok(ex.outputs.get("answer") == out.get("answer")));

// New (recommended)
let metric = metrics::answer_exact_match("answer");
```

### Phase 4: Removal
After 2-3 releases, remove deprecated types (breaking change).

---

## Related Work

### Existing Rust Implementations
- **Current ggen DSPy**: Basic optimizer, simple metrics
- **Metrics infrastructure**: Various `*Metrics` structs in hyper_concurrent, governance, codegen
- **No unified evaluation framework**

### Python DSPy Versions
- **v2.5+**: Current stable with Evaluate class
- **Auto-evaluation module**: `dspy/evaluate/auto_evaluation.py`
- **Metrics module**: `dspy/evaluate/metrics.py`

### Design Influences
- **Chicago TDD**: Real objects, no mocks in metrics
- **Poka-yoke**: Type safety prevents metric misuse (MetricResult enum)
- **EPIC 9**: Parallel evaluation aligns with parallel agent execution

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Parallel overhead exceeds benefits | Performance regression | Benchmark with varying thread counts, add adaptive threading |
| Trace collection too expensive | Slow optimization | Make trace optional, collect only when metric needs it |
| LLM-based metrics unreliable | Poor evaluation quality | Provide threshold tuning, allow metric chaining |
| Breaking changes to MetricFn | Migration burden | Provide compatibility layer, deprecation period |

---

## Next Steps

1. **Review this research** with team
2. **Approve architecture** and roadmap
3. **Create EPIC 9 specification** for parallel implementation
4. **Begin Phase 1** (Core Evaluation)
5. **Track progress** via receipts (test results, benchmarks)

---

## References

### Documentation
- [DSPy Evaluate API](https://dspy.ai/api/evaluation/Evaluate/)
- [DSPy Metrics](https://dspy.ai/learn/evaluation/metrics/)
- [DSPy Evaluation Overview](https://dspy.ai/learn/evaluation/overview/)
- [DSPy Optimizers](https://dspy.ai/learn/optimization/optimizers/)

### Source Code
- [DSPy GitHub](https://github.com/stanfordnlp/dspy)
- [Auto-Evaluation Implementation](https://github.com/stanfordnlp/dspy/blob/main/dspy/evaluate/auto_evaluation.py)

### Research Documents
- [dspy-evaluation-system.md](./dspy-evaluation-system.md) - Full technical spec
- [dspy-evaluation-examples.md](./dspy-evaluation-examples.md) - Code examples and patterns

---

**End of Summary**
