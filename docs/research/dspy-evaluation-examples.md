# DSPy Evaluation: Implementation Examples

**Quick Reference**: Common evaluation patterns and metric implementations for Rust DSPy

---

## Python DSPy Evaluation Patterns

### Basic Evaluation

```python
from dspy.evaluate import Evaluate

# Simple accuracy metric
def accuracy_metric(example, pred, trace=None):
    return example.answer == pred.answer

# Create evaluator
evaluator = Evaluate(
    devset=dev_examples,
    metric=accuracy_metric,
    num_threads=8,
    display_progress=True,
    display_table=5
)

# Run evaluation
result = evaluator(my_program)
print(f"Score: {result.score}%")
```

### Custom Metrics

```python
# Dual-mode metric (bootstrapping vs evaluation)
def smart_metric(example, pred, trace=None):
    correct = example.answer.lower() == pred.answer.lower()
    grounded = pred.answer in pred.context

    if trace is None:  # Evaluation mode
        return (correct + grounded) / 2.0
    else:  # Bootstrapping mode
        return correct and grounded

# AI-based metric using DSPy
class FactJudge(dspy.Signature):
    """Judge factual correctness."""
    context = dspy.InputField()
    answer = dspy.InputField()
    is_factual = dspy.OutputField(desc="Yes/No")

evaluator = dspy.ChainOfThought(FactJudge)

def factuality_metric(example, pred, trace=None):
    result = evaluator(context=pred.context, answer=pred.answer)
    is_correct = result.is_factual.lower() == 'yes'

    if trace is None:
        return 1.0 if is_correct else 0.0
    else:
        return is_correct
```

### Built-in Metrics

```python
from dspy.evaluate.metrics import answer_exact_match, SemanticF1

# Exact match
evaluator = Evaluate(devset=devset, metric=answer_exact_match)

# Semantic F1 (LLM-based)
semantic_f1 = SemanticF1(threshold=0.7)
evaluator = Evaluate(devset=devset, metric=semantic_f1)
```

### Optimizer Integration

```python
# BootstrapFewShot with evaluation metric
def validation_metric(example, pred, trace=None):
    if trace:
        # During bootstrapping: return bool
        return example.answer == pred.answer
    else:
        # During evaluation: return float
        return 1.0 if example.answer == pred.answer else 0.0

optimizer = dspy.BootstrapFewShot(
    metric=validation_metric,
    max_bootstrapped_demos=8
)

compiled = optimizer.compile(student, trainset=trainset)

# Evaluate optimized program
evaluator = Evaluate(devset=devset, metric=validation_metric)
result = evaluator(compiled)
```

---

## Proposed Rust Implementation

### Basic Evaluation

```rust
use ggen_ai::dspy::{
    evaluate::Evaluate,
    metrics,
    Example,
    Module,
};

#[tokio::main]
async fn main() -> Result<()> {
    // Create development set
    let devset = vec![
        Example::new(
            hashmap!{"question" => json!("What is 2+2?")},
            hashmap!{"answer" => json!("4")},
        ),
        Example::new(
            hashmap!{"question" => json!("What is 5*3?")},
            hashmap!{"answer" => json!("15")},
        ),
    ];

    // Create evaluator with built-in metric
    let evaluator = Evaluate::new(devset)
        .with_metric(metrics::answer_exact_match("answer"))
        .with_num_threads(8)
        .with_display_progress(true)
        .with_display_table(5);

    // Evaluate program
    let program = create_program();
    let result = evaluator.evaluate(&program, None).await?;

    println!("Score: {:.2}%", result.score);
    println!("Success rate: {:.2}%", result.success_rate() * 100.0);

    Ok(())
}
```

### Custom Metrics

```rust
use std::sync::Arc;
use ggen_ai::dspy::{
    optimizer::{Example, MetricFn, MetricResult},
    evaluate::Trace,
};

// Simple metric (evaluation only)
fn create_simple_metric() -> SimpleMetricFn {
    Arc::new(|example: &Example, pred: &HashMap<String, Value>| {
        let expected = example.outputs.get("answer");
        let actual = pred.get("answer");

        match (expected, actual) {
            (Some(exp), Some(act)) => {
                Ok(if exp == act { 1.0 } else { 0.0 })
            }
            _ => Ok(0.0),
        }
    })
}

// Dual-mode metric (bootstrapping + evaluation)
fn create_dual_mode_metric() -> MetricFn {
    Arc::new(|example: &Example,
              pred: &HashMap<String, Value>,
              trace: Option<&Trace>| {

        let expected = example.outputs.get("answer");
        let actual = pred.get("answer");
        let matches = expected == actual;

        if let Some(_trace) = trace {
            // Bootstrapping mode: return bool
            Ok(MetricResult::Boolean(matches))
        } else {
            // Evaluation mode: return float
            let score = if matches { 1.0 } else { 0.0 };
            Ok(MetricResult::Score(score))
        }
    })
}

// Multi-property metric
fn create_complex_metric() -> MetricFn {
    Arc::new(|example: &Example,
              pred: &HashMap<String, Value>,
              trace: Option<&Trace>| {

        // Check answer correctness
        let answer_correct = example.outputs.get("answer")
            .and_then(|exp| pred.get("answer").map(|act| exp == act))
            .unwrap_or(false);

        // Check context grounding
        let grounded = pred.get("context")
            .and_then(|ctx| ctx.as_array())
            .and_then(|arr| {
                pred.get("answer")
                    .and_then(|ans| ans.as_str())
                    .map(|ans_str| {
                        arr.iter().any(|c| {
                            c.as_str()
                                .map(|s| s.contains(ans_str))
                                .unwrap_or(false)
                        })
                    })
            })
            .unwrap_or(false);

        if trace.is_some() {
            // Bootstrapping: both must be true
            Ok(MetricResult::Boolean(answer_correct && grounded))
        } else {
            // Evaluation: weighted score
            let score = 0.7 * (answer_correct as u8 as f64)
                      + 0.3 * (grounded as u8 as f64);
            Ok(MetricResult::Score(score))
        }
    })
}

// Trace-aware metric (validates intermediate steps)
fn create_trace_aware_metric() -> MetricFn {
    Arc::new(|example: &Example,
              pred: &HashMap<String, Value>,
              trace: Option<&Trace>| {

        let answer_correct = example.outputs.get("answer") == pred.get("answer");

        if let Some(trace) = trace {
            // Validate intermediate reasoning steps
            let valid_steps = trace.reasoning_steps.iter().all(|step| {
                !step.is_empty() && step.len() > 10  // Non-trivial reasoning
            });

            // Validate module calls
            let has_context_retrieval = trace.module_calls.iter().any(|call| {
                call.module_name.contains("Retrieve")
                    && call.outputs.contains_key("context")
            });

            let all_valid = answer_correct && valid_steps && has_context_retrieval;
            Ok(MetricResult::Boolean(all_valid))
        } else {
            // Standard evaluation
            Ok(MetricResult::Score(if answer_correct { 1.0 } else { 0.0 }))
        }
    })
}
```

### Built-in Metrics Usage

```rust
use ggen_ai::dspy::metrics;

// Exact match
let metric1 = metrics::answer_exact_match("answer");

// Case-insensitive match
let metric2 = metrics::answer_exact_match_ci("answer");

// Passage match
let metric3 = metrics::answer_passage_match("answer", "context");

// F1 score calculation
let precision = 0.8;
let recall = 0.9;
let f1 = metrics::f1_score(precision, recall);

// Semantic F1 (LLM-based)
let semantic_f1 = metrics::SemanticF1::new(0.7);
let metric4 = semantic_f1.as_metric();
```

### Parallel Evaluation

```rust
// Evaluate with multiple threads
let evaluator = Evaluate::new(large_devset)  // 1000 examples
    .with_metric(metrics::answer_exact_match("answer"))
    .with_num_threads(16)  // 16 parallel workers
    .with_display_progress(true);

let result = evaluator.evaluate(&program, None).await?;

// Each thread processes ~62 examples concurrently
println!("Evaluated {} examples in {:?}",
         result.results.len(),
         result.elapsed);
```

### Error Handling

```rust
// Configure error handling
let evaluator = Evaluate::new(devset)
    .with_metric(my_metric)
    .with_max_errors(10)          // Stop after 10 errors
    .with_failure_score(0.0)      // Score failures as 0.0
    .with_num_threads(8);

let result = evaluator.evaluate(&program, None).await?;

println!("Successful: {}", result.successful);
println!("Failed: {}", result.failed);

// Inspect errors
for point in &result.results {
    if let Some(err) = &point.error {
        eprintln!("Error: {}", err);
    }
}
```

### Export Results

```rust
// Export to CSV and JSON
let evaluator = Evaluate::new(devset)
    .with_metric(my_metric)
    .with_num_threads(8)
    .save_as_csv("results.csv")
    .save_as_json("results.json");

let result = evaluator.evaluate(&program, None).await?;

// Files are automatically written
println!("Results saved to results.csv and results.json");
```

### Display Results

```rust
// Show sample results in table
let evaluator = Evaluate::new(devset)
    .with_metric(my_metric)
    .with_display_table(10)  // Show 10 examples
    .with_display_progress(true);

let result = evaluator.evaluate(&program, None).await?;

// Output:
// [00:00:05] ████████████████████ 100/100 Evaluation complete
//
// ┌──────────────────────────────┬──────────────────┬──────────────────┬───────┐
// │ Input                        │ Expected         │ Predicted        │ Score │
// ├──────────────────────────────┼──────────────────┼──────────────────┼───────┤
// │ What is 2+2?                 │ 4                │ 4                │ 1.00  │
// │ What is the capital of Fr... │ Paris            │ Paris            │ 1.00  │
// └──────────────────────────────┴──────────────────┴──────────────────┴───────┘
//
// Overall Score: 95.50%
// Success Rate: 98.00%
```

### Multi-Metric Evaluation

```rust
// Evaluate program across multiple metrics
let metrics_suite = vec![
    ("exact_match", metrics::answer_exact_match("answer")),
    ("case_insensitive", metrics::answer_exact_match_ci("answer")),
    ("passage_match", metrics::answer_passage_match("answer", "context")),
];

let mut results = HashMap::new();

for (name, metric) in metrics_suite {
    let evaluator = Evaluate::new(devset.clone())
        .with_num_threads(8);

    let result = evaluator.evaluate(&program, Some(metric)).await?;
    results.insert(name, result.score);

    println!("{}: {:.2}%", name, result.score);
}

// Output:
// exact_match: 85.50%
// case_insensitive: 92.30%
// passage_match: 78.20%
```

### Optimizer Integration

```rust
use ggen_ai::dspy::{
    optimizer::BootstrapFewShot,
    evaluate::Evaluate,
};

// Define dual-mode metric for bootstrapping + evaluation
let metric = Arc::new(
    |example: &Example,
     pred: &HashMap<String, Value>,
     trace: Option<&Trace>| {

        let matches = example.outputs.get("answer") == pred.get("answer");

        if trace.is_some() {
            Ok(MetricResult::Boolean(matches))  // Bootstrapping
        } else {
            Ok(MetricResult::Score(if matches { 1.0 } else { 0.0 }))  // Evaluation
        }
    }
);

// Create optimizer
let optimizer = BootstrapFewShot::new(metric.clone())
    .with_max_bootstrapped_demos(8);

// Compile student predictor
let student = Predictor::new(signature);
let optimized = optimizer.compile(&student, &trainset).await?;

println!("Bootstrapped {} demonstrations", optimized.demonstration_count());

// Evaluate optimized program
let evaluator = Evaluate::new(devset)
    .with_num_threads(8)
    .with_display_progress(true);

let result = evaluator.evaluate(&optimized, Some(metric)).await?;

println!("Baseline: {:.2}%", baseline_score);
println!("Optimized: {:.2}%", result.score);
println!("Improvement: {:.2}%", result.score - baseline_score);
```

### Progressive Validation

```rust
// Evaluate on increasingly difficult subsets
let test_suites = vec![
    ("easy", easy_examples),
    ("medium", medium_examples),
    ("hard", hard_examples),
];

for (difficulty, subset) in test_suites {
    let evaluator = Evaluate::new(subset)
        .with_metric(my_metric.clone())
        .with_num_threads(8);

    let result = evaluator.evaluate(&program, None).await?;

    println!("{} difficulty: {:.2}%", difficulty, result.score);
}

// Output:
// easy difficulty: 98.50%
// medium difficulty: 85.20%
// hard difficulty: 62.80%
```

### Hyperparameter Search

```rust
// Find best configuration
let mut best_score = 0.0;
let mut best_config = None;

for num_demos in [1, 2, 4, 8] {
    for temperature in [0.0, 0.3, 0.7, 1.0] {
        let program = create_program(num_demos, temperature);

        let evaluator = Evaluate::new(devset.clone())
            .with_metric(my_metric.clone())
            .with_num_threads(8);

        let result = evaluator.evaluate(&program, None).await?;

        if result.score > best_score {
            best_score = result.score;
            best_config = Some((num_demos, temperature));
        }

        println!("demos={}, temp={:.1}: {:.2}%",
                 num_demos, temperature, result.score);
    }
}

println!("\nBest config: {:?} with score {:.2}%",
         best_config, best_score);
```

### Comparison Evaluation

```rust
// Compare multiple program variants
let programs = vec![
    ("zero_shot", create_zero_shot_predictor()),
    ("few_shot_3", create_few_shot_predictor(3)),
    ("few_shot_8", create_few_shot_predictor(8)),
    ("optimized", create_optimized_predictor()),
];

let mut results = Vec::new();

for (name, program) in programs {
    let evaluator = Evaluate::new(devset.clone())
        .with_metric(my_metric.clone())
        .with_num_threads(8);

    let result = evaluator.evaluate(&program, None).await?;
    results.push((name, result.score));

    println!("{}: {:.2}%", name, result.score);
}

// Find best
results.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
println!("\nBest variant: {} ({:.2}%)", results[0].0, results[0].1);
```

---

## Testing Patterns

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_basic_evaluation() {
        let devset = vec![
            Example::new(
                hashmap!{"question" => json!("Test")},
                hashmap!{"answer" => json!("Response")},
            ),
        ];

        let metric = metrics::answer_exact_match("answer");
        let program = MockModule::new();

        let evaluator = Evaluate::new(devset)
            .with_metric(metric);

        let result = evaluator.evaluate(&program, None).await.unwrap();

        assert!(result.score >= 0.0 && result.score <= 100.0);
        assert_eq!(result.results.len(), 1);
    }

    #[tokio::test]
    async fn test_parallel_evaluation() {
        let devset = create_large_devset(100);
        let metric = metrics::answer_exact_match("answer");
        let program = MockModule::new();

        let start = Instant::now();

        let evaluator = Evaluate::new(devset)
            .with_metric(metric)
            .with_num_threads(8);

        let result = evaluator.evaluate(&program, None).await.unwrap();

        let elapsed = start.elapsed();

        assert_eq!(result.results.len(), 100);
        assert!(elapsed < Duration::from_secs(10));  // Should be fast
    }

    #[test]
    fn test_metric_result_types() {
        let bool_result = MetricResult::Boolean(true);
        assert_eq!(bool_result.as_score(), 1.0);
        assert_eq!(bool_result.as_bool(), Some(true));

        let score_result = MetricResult::Score(0.75);
        assert_eq!(score_result.as_score(), 0.75);
        assert_eq!(score_result.as_bool(), Some(true));
    }

    #[tokio::test]
    async fn test_error_handling() {
        let devset = create_failing_examples();
        let metric = metrics::answer_exact_match("answer");
        let program = FailingModule::new();

        let evaluator = Evaluate::new(devset)
            .with_metric(metric)
            .with_max_errors(5)
            .with_failure_score(0.0);

        let result = evaluator.evaluate(&program, None).await.unwrap();

        assert!(result.failed > 0);
        assert_eq!(result.failed, result.results.iter()
            .filter(|r| r.error.is_some())
            .count());
    }
}
```

### Benchmark Tests

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_evaluation(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("evaluate_100_sequential", |b| {
        b.to_async(&rt).iter(|| async {
            let devset = create_devset(100);
            let program = MockModule::new();
            let metric = metrics::answer_exact_match("answer");

            let evaluator = Evaluate::new(devset)
                .with_metric(metric);

            black_box(evaluator.evaluate(&program, None).await.unwrap())
        });
    });

    c.bench_function("evaluate_100_parallel_16", |b| {
        b.to_async(&rt).iter(|| async {
            let devset = create_devset(100);
            let program = MockModule::new();
            let metric = metrics::answer_exact_match("answer");

            let evaluator = Evaluate::new(devset)
                .with_metric(metric)
                .with_num_threads(16);

            black_box(evaluator.evaluate(&program, None).await.unwrap())
        });
    });

    c.bench_function("custom_metric_overhead", |b| {
        b.iter(|| {
            let example = create_example();
            let pred = create_prediction();

            let metric = create_complex_metric();
            black_box(metric(&example, &pred, None).unwrap())
        });
    });
}

criterion_group!(benches, benchmark_evaluation);
criterion_main!(benches);
```

---

## Quick Reference

### Type Signatures

```rust
// Simple metric (no trace support)
pub type SimpleMetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>)
        -> Result<f64, MetricError>
    + Send + Sync
>;

// Full metric (with trace support)
pub type MetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>, Option<&Trace>)
        -> Result<MetricResult, MetricError>
    + Send + Sync
>;

// Metric result
pub enum MetricResult {
    Boolean(bool),   // For bootstrapping
    Score(f64),      // For evaluation
}
```

### Common Patterns

```rust
// Pattern 1: Simple evaluation
let result = Evaluate::new(devset)
    .with_metric(metrics::answer_exact_match("answer"))
    .evaluate(&program, None).await?;

// Pattern 2: Parallel evaluation with progress
let result = Evaluate::new(devset)
    .with_metric(my_metric)
    .with_num_threads(16)
    .with_display_progress(true)
    .evaluate(&program, None).await?;

// Pattern 3: Export results
let result = Evaluate::new(devset)
    .with_metric(my_metric)
    .save_as_csv("results.csv")
    .save_as_json("results.json")
    .evaluate(&program, None).await?;

// Pattern 4: Multi-metric comparison
for (name, metric) in metrics {
    let result = Evaluate::new(devset.clone())
        .evaluate(&program, Some(metric)).await?;
    println!("{}: {:.2}%", name, result.score);
}

// Pattern 5: Optimizer integration
let optimized = BootstrapFewShot::new(metric.clone())
    .compile(&student, &trainset).await?;

let result = Evaluate::new(devset)
    .evaluate(&optimized, Some(metric)).await?;
```

---

**End of Document**
