<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DSPy BootstrapFewShot Optimizer](#dspy-bootstrapfewshot-optimizer)
  - [Overview](#overview)
  - [Algorithm](#algorithm)
  - [Core Components](#core-components)
    - [1. Example](#1-example)
    - [2. Demonstration](#2-demonstration)
    - [3. MetricFn](#3-metricfn)
    - [4. BootstrapFewShot](#4-bootstrapfewshot)
    - [5. OptimizedPredictor](#5-optimizedpredictor)
  - [Usage Example](#usage-example)
  - [Type Safety](#type-safety)
  - [Error Handling](#error-handling)
  - [Testing](#testing)
    - [Unit Tests](#unit-tests)
    - [Integration Tests](#integration-tests)
    - [Example](#example)
  - [Performance](#performance)
  - [Integration with DSPy Ecosystem](#integration-with-dspy-ecosystem)
  - [Advanced Usage](#advanced-usage)
    - [Custom Teacher Model](#custom-teacher-model)
    - [Fuzzy Metric](#fuzzy-metric)
    - [Iterative Optimization](#iterative-optimization)
  - [Metrics](#metrics)
  - [Design Principles](#design-principles)
  - [References](#references)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DSPy BootstrapFewShot Optimizer

## Overview

The `BootstrapFewShot` optimizer implements the Bootstrap Few-Shot learning algorithm from Stanford's DSPy framework in production-quality Rust. It improves LLM predictor performance by automatically collecting successful demonstrations and incorporating them into prompts.

## Algorithm

```
For each example in training set (up to max_bootstrapped_demos):
  1. Run teacher predictor (or student) on example inputs
  2. Evaluate output using metric function
  3. If metric passes → save as demonstration
  4. Continue until max demonstrations collected

Result: OptimizedPredictor with demonstrations in prompt
```

## Core Components

### 1. Example

Represents a training/validation example with inputs and expected outputs.

```rust
use std::collections::HashMap;
use serde_json::Value;

let mut inputs = HashMap::new();
inputs.insert("question".to_string(), Value::String("What is Rust?".into()));

let mut outputs = HashMap::new();
outputs.insert("answer".to_string(), Value::String("A programming language".into()));

let example = Example::new(inputs, outputs);
```

### 2. Demonstration

A successful example used in few-shot prompts.

```rust
let demo = Demonstration::new(inputs, outputs);
let formatted = demo.format(&signature); // "question: ...\nanswer: ..."
```

### 3. MetricFn

Function type for evaluating if outputs are acceptable:

```rust
use std::sync::Arc;

let metric: MetricFn = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    // Check if output matches expected
    Ok(example.outputs.get("answer") == output.get("answer"))
});
```

### 4. BootstrapFewShot

The optimizer that orchestrates the bootstrapping process:

```rust
let optimizer = BootstrapFewShot::new(metric)
    .with_max_bootstrapped_demos(4)
    .with_max_labeled_demos(16)
    .with_teacher(teacher_predictor);  // Optional
```

### 5. OptimizedPredictor

Result of compilation - a predictor with demonstrations:

```rust
let optimized = optimizer.compile(&student, &trainset).await?;

println!("Collected {} demonstrations", optimized.demonstration_count());

// Use like any Module
let output = optimized.forward(inputs).await?;
```

## Usage Example

```rust
use ggen_ai::dspy::{
    optimizer::{BootstrapFewShot, Example},
    field::{InputField, OutputField},
    module::Module,
    predictor::Predictor,
    signature::Signature,
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Define task signature
    let signature = Signature::new("QA", "Answer questions")
        .with_input(InputField::new("question", "Question", "String"))
        .with_output(OutputField::new("answer", "Answer", "String"));

    // 2. Create training examples
    let trainset = vec![
        Example::new(
            [("question".into(), json!("What is 2+2?"))].into(),
            [("answer".into(), json!("4"))].into(),
        ),
        Example::new(
            [("question".into(), json!("What is 5*3?"))].into(),
            [("answer".into(), json!("15"))].into(),
        ),
    ];

    // 3. Define metric (exact match)
    let metric = Arc::new(|example: &Example, output| {
        Ok(example.outputs.get("answer") == output.get("answer"))
    });

    // 4. Create optimizer
    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(2);

    // 5. Create student predictor
    let student = Predictor::new(signature)
        .with_model("gpt-4");

    // 6. Compile (bootstrap demonstrations)
    let optimized = optimizer.compile(&student, &trainset).await?;

    // 7. Use optimized predictor
    let output = optimized.forward(
        [("question".into(), json!("What is 6+7?"))].into()
    ).await?;

    println!("Answer: {}", output.get("answer").unwrap());

    Ok(())
}
```

## Type Safety

All operations use `Result<T, E>` for error handling:

```rust
pub async fn compile(
    &self,
    student: &dyn Module,
    trainset: &[Example],
) -> Result<OptimizedPredictor, ModuleError>;
```

No unwrap/expect in production code.

## Error Handling

```rust
match optimizer.compile(&student, &trainset).await {
    Ok(optimized) => {
        println!("Success: {} demos", optimized.demonstration_count());
    }
    Err(ModuleError::Other(msg)) if msg.contains("empty") => {
        eprintln!("Training set is empty");
    }
    Err(ModuleError::LlmError(msg)) => {
        eprintln!("LLM call failed: {}", msg);
    }
    Err(e) => {
        eprintln!("Optimization failed: {}", e);
    }
}
```

## Testing

### Unit Tests

```bash
cargo test -p ggen-ai --lib dspy::optimizer
```

### Integration Tests

```bash
cargo test -p ggen-ai --test dspy_optimizer_test
```

### Example

```bash
export GGEN_LLM_MODEL=gpt-4
cargo run --example dspy_bootstrap_fewshot
```

## Performance

- **Zero-Cost Abstractions**: Generic types eliminate runtime overhead
- **Memory Efficient**: Demonstrations stored in `Vec<Demonstration>`
- **Async Native**: Non-blocking LLM calls via tokio
- **Type-First Design**: Compiler enforces invariants

## Integration with DSPy Ecosystem

The optimizer integrates seamlessly with other DSPy components:

- **Predictor**: Standard predictor as student
- **ChainOfThought**: Can be used as teacher
- **Signature**: Defines task interface
- **Module**: Trait for composability

## Advanced Usage

### Custom Teacher Model

```rust
let teacher = Arc::new(Predictor::new(signature.clone())
    .with_model("gpt-4")
    .with_temperature(0.0));  // More deterministic

let optimizer = BootstrapFewShot::new(metric)
    .with_teacher(teacher);
```

### Fuzzy Metric

```rust
let fuzzy_metric = Arc::new(|example: &Example, output| {
    let expected = example.outputs.get("answer")?.as_str()?;
    let actual = output.get("answer")?.as_str()?;

    // Accept if similar (e.g., edit distance)
    let similarity = calculate_similarity(expected, actual);
    Ok(similarity > 0.8)
});
```

### Iterative Optimization

```rust
let mut optimized = optimizer.compile(&student, &trainset).await?;

// Further optimize with more examples
let extended_trainset = collect_more_examples();
optimized = optimizer.compile(&optimized, &extended_trainset).await?;
```

## Metrics

Track optimization effectiveness:

```rust
let optimized = optimizer.compile(&student, &trainset).await?;

println!("Demonstrations: {}/{}",
    optimized.demonstration_count(),
    trainset.len()
);

// Success rate = demos / attempts
let success_rate = optimized.demonstration_count() as f64 / trainset.len() as f64;
println!("Success rate: {:.2}%", success_rate * 100.0);
```

## Design Principles

1. **Type-First**: Constraints in types, not runtime
2. **Result<T, E>**: All fallible operations
3. **Zero Unwrap**: No panics in production
4. **Async Native**: Non-blocking throughout
5. **Composable**: Implements Module trait

## References

- Python DSPy: https://github.com/stanfordnlp/dspy
- Paper: "DSPy: Compiling Declarative Language Model Calls into Self-Improving Pipelines"
- ggen DSPy Module: `/home/user/ggen/crates/ggen-ai/src/dspy/`

## License

Part of ggen project - see LICENSE file.
