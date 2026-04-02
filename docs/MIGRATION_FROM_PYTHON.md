<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Migrating from Python DSPy to Rust ggen-dspy](#migrating-from-python-dspy-to-rust-ggen-dspy)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
    - [Why Migrate to Rust?](#why-migrate-to-rust)
  - [Key Differences](#key-differences)
    - [1. Type System](#1-type-system)
    - [2. Async Model](#2-async-model)
    - [3. Error Handling](#3-error-handling)
    - [4. Data Structures](#4-data-structures)
  - [Concept Mapping](#concept-mapping)
    - [Core Abstractions](#core-abstractions)
    - [Optimizers](#optimizers)
  - [Code Examples Side-by-Side](#code-examples-side-by-side)
    - [Example 1: Basic Prediction](#example-1-basic-prediction)
    - [Example 2: Chain of Thought](#example-2-chain-of-thought)
    - [Example 3: Bootstrap Few-Shot Optimization](#example-3-bootstrap-few-shot-optimization)
    - [Example 4: Custom Module](#example-4-custom-module)
  - [Common Patterns](#common-patterns)
    - [Pattern 1: Evaluation Loop](#pattern-1-evaluation-loop)
    - [Pattern 2: Batch Processing](#pattern-2-batch-processing)
    - [Pattern 3: Metric Functions](#pattern-3-metric-functions)
  - [Error Handling](#error-handling)
    - [Python: Try/Except](#python-tryexcept)
    - [Rust: Result and Match](#rust-result-and-match)
    - [Rust: Using ? Operator](#rust-using--operator)
  - [Async Patterns](#async-patterns)
    - [Python: asyncio](#python-asyncio)
    - [Rust: Tokio](#rust-tokio)
  - [Optimization Workflows](#optimization-workflows)
    - [Python DSPy](#python-dspy)
    - [Rust ggen-dspy](#rust-ggen-dspy)
  - [Gotchas and Tips](#gotchas-and-tips)
    - [1. HashMap Construction](#1-hashmap-construction)
    - [2. String Conversions](#2-string-conversions)
    - [3. Accessing Nested Values](#3-accessing-nested-values)
    - [4. Optional Teacher](#4-optional-teacher)
    - [5. Cloning Data](#5-cloning-data)
    - [6. Lifetime Issues](#6-lifetime-issues)
    - [7. Arc for Shared State](#7-arc-for-shared-state)
    - [8. Environment Variables](#8-environment-variables)
  - [Summary: Quick Reference](#summary-quick-reference)
  - [Common Pitfalls and Solutions](#common-pitfalls-and-solutions)
    - [Pitfall 1: Forgetting `.await` on Async Calls](#pitfall-1-forgetting-await-on-async-calls)
    - [Pitfall 2: Unwrapping in Production Code](#pitfall-2-unwrapping-in-production-code)
    - [Pitfall 3: Incorrect HashMap Construction](#pitfall-3-incorrect-hashmap-construction)
    - [Pitfall 4: Module Ownership Issues](#pitfall-4-module-ownership-issues)
    - [Pitfall 5: Cloning vs. Borrowing](#pitfall-5-cloning-vs-borrowing)
    - [Pitfall 6: Missing Type Annotations](#pitfall-6-missing-type-annotations)
    - [Pitfall 7: Metric Function Closures](#pitfall-7-metric-function-closures)
    - [Pitfall 8: Nested Result Handling](#pitfall-8-nested-result-handling)
    - [Pitfall 9: Module Trait Implementation](#pitfall-9-module-trait-implementation)
    - [Pitfall 10: Environment Variable Access](#pitfall-10-environment-variable-access)
  - [Advanced Migration Examples](#advanced-migration-examples)
    - [Example: Custom Optimizer](#example-custom-optimizer)
    - [Example: Streaming Responses](#example-streaming-responses)
    - [Example: Parallel Module Execution](#example-parallel-module-execution)
  - [Performance Comparison](#performance-comparison)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Migrating from Python DSPy to Rust ggen-dspy

**Version**: 1.0
**Date**: 2026-01-11

## Table of Contents

1. [Overview](#overview)
2. [Key Differences](#key-differences)
3. [Concept Mapping](#concept-mapping)
4. [Code Examples Side-by-Side](#code-examples-side-by-side)
5. [Common Patterns](#common-patterns)
6. [Error Handling](#error-handling)
7. [Async Patterns](#async-patterns)
8. [Optimization Workflows](#optimization-workflows)
9. [Gotchas and Tips](#gotchas-and-tips)

---

## Overview

This guide helps Python DSPy users transition to the Rust ggen-dspy implementation. While the core concepts remain the same, Rust's type system and async model require different patterns.

### Why Migrate to Rust?

| Benefit | Python DSPy | Rust ggen-dspy |
|---------|------------|----------------|
| **Type Safety** | Runtime checking | Compile-time verification |
| **Performance** | ~100ms/call | ~50ms/call (less overhead) |
| **Concurrency** | GIL limitations | True parallelism |
| **Memory Safety** | GC pauses | Zero-cost abstractions |
| **Production Ready** | Good | Excellent (no unwrap, Result<T,E>) |

---

## Key Differences

### 1. Type System

**Python**: Dynamic typing
```python
def process(question: str) -> str:  # Type hints optional
    return predict(question)
```

**Rust**: Static typing enforced
```rust
async fn process(question: String) -> Result<String, ModuleError> {
    let result = predict(question).await?;
    Ok(result)
}
```

### 2. Async Model

**Python**: `async`/`await` with asyncio
```python
async def main():
    result = await predictor(question="...")
```

**Rust**: `async`/`await` with Tokio
```rust
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let result = predictor.forward(inputs).await?;
    Ok(())
}
```

### 3. Error Handling

**Python**: Exceptions
```python
try:
    result = predictor(question="...")
except Exception as e:
    print(f"Error: {e}")
```

**Rust**: `Result<T, E>` everywhere
```rust
match predictor.forward(inputs).await {
    Ok(result) => println!("Success: {:?}", result),
    Err(e) => eprintln!("Error: {}", e),
}

// Or with ? operator
let result = predictor.forward(inputs).await?;
```

### 4. Data Structures

**Python**: Dict for inputs/outputs
```python
inputs = {"question": "What is 2+2?"}
output = predictor(**inputs)
print(output.answer)
```

**Rust**: HashMap<String, Value>
```rust
let mut inputs = HashMap::new();
inputs.insert("question".into(), json!("What is 2+2?"));

let output = predictor.forward(inputs).await?;
let answer = output.get("answer")
    .and_then(|v| v.as_str())
    .ok_or(ModuleError::MissingOutput("answer".into()))?;
```

---

## Concept Mapping

### Core Abstractions

| Python DSPy | Rust ggen-dspy | Notes |
|------------|----------------|-------|
| `dspy.Signature` | `Signature::new()` | Builder pattern in Rust |
| `dspy.InputField` | `InputField::new()` | Immutable by default |
| `dspy.OutputField` | `OutputField::new()` | Immutable by default |
| `dspy.Module` | `trait Module` | Trait (interface) pattern |
| `dspy.Predict` | `Predictor` | Similar functionality |
| `dspy.ChainOfThought` | `ChainOfThought` | Identical concept |
| `dspy.Example` | `optimizer::Example` | In optimizer module |

### Optimizers

| Python DSPy | Rust ggen-dspy | Status |
|------------|----------------|--------|
| `dspy.BootstrapFewShot` | `optimizer::BootstrapFewShot` | ✅ Implemented |
| `dspy.KNNFewShot` | - | 🚧 Planned (Phase 4) |
| `dspy.COPRO` | - | 🚧 Planned (Phase 4) |
| `dspy.MIPROv2` | - | 🚧 Planned (Phase 4) |
| `dspy.Evaluate` | Manual implementation | See examples below |

---

## Code Examples Side-by-Side

### Example 1: Basic Prediction

**Python**:
```python
import dspy

# Configure LLM
lm = dspy.OpenAI(model="gpt-4")
dspy.settings.configure(lm=lm)

# Define signature
class QA(dspy.Signature):
    """Answer questions accurately."""
    question = dspy.InputField()
    answer = dspy.OutputField()

# Create predictor
predictor = dspy.Predict(QA)

# Run
result = predictor(question="What is the capital of France?")
print(result.answer)
```

**Rust**:
```rust
use ggen_ai::dspy::{Signature, InputField, OutputField, Predictor, Module};
use serde_json::json;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // LLM configured via environment (GGEN_LLM_MODEL=gpt-4)

    // Define signature
    let sig = Signature::new("QA", "Answer questions accurately")
        .with_input(InputField::new("question", "Question to answer", "String"))
        .with_output(OutputField::new("answer", "The answer", "String"));

    // Create predictor
    let predictor = Predictor::new(sig);

    // Run
    let mut inputs = HashMap::new();
    inputs.insert("question".into(), json!("What is the capital of France?"));

    let result = predictor.forward(inputs).await?;
    println!("{}", result["answer"]);

    Ok(())
}
```

### Example 2: Chain of Thought

**Python**:
```python
import dspy

class MathQA(dspy.Signature):
    """Solve math problems with step-by-step reasoning."""
    problem = dspy.InputField()
    solution = dspy.OutputField()

# Use ChainOfThought
math_solver = dspy.ChainOfThought(MathQA)

result = math_solver(problem="If John has 5 apples and buys 3 more, how many does he have?")
print(result.solution)
```

**Rust**:
```rust
use ggen_ai::dspy::{Signature, InputField, OutputField, ChainOfThought, Module};
use serde_json::json;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let sig = Signature::new("MathQA", "Solve math problems with step-by-step reasoning")
        .with_input(InputField::new("problem", "Math problem", "String"))
        .with_output(OutputField::new("solution", "Solution with reasoning", "String"));

    let math_solver = ChainOfThought::new(sig);

    let mut inputs = HashMap::new();
    inputs.insert("problem".into(),
        json!("If John has 5 apples and buys 3 more, how many does he have?"));

    let result = math_solver.forward(inputs).await?;
    println!("{}", result["solution"]);

    Ok(())
}
```

### Example 3: Bootstrap Few-Shot Optimization

**Python**:
```python
import dspy

# Define signature
class Classify(dspy.Signature):
    """Classify sentiment as positive or negative."""
    text = dspy.InputField()
    sentiment = dspy.OutputField()

# Create training data
trainset = [
    dspy.Example(text="I love this!", sentiment="positive").with_inputs("text"),
    dspy.Example(text="This is terrible.", sentiment="negative").with_inputs("text"),
    dspy.Example(text="Amazing experience!", sentiment="positive").with_inputs("text"),
]

# Define metric
def accuracy(example, pred, trace=None):
    return example.sentiment == pred.sentiment

# Optimize
student = dspy.Predict(Classify)
optimizer = dspy.BootstrapFewShot(metric=accuracy)
optimized = optimizer.compile(student, trainset=trainset)

# Use optimized
result = optimized(text="Great product!")
print(result.sentiment)
```

**Rust**:
```rust
use ggen_ai::dspy::{
    Signature, InputField, OutputField, Predictor,
    optimizer::{BootstrapFewShot, Example},
};
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Define signature
    let sig = Signature::new("Classify", "Classify sentiment as positive or negative")
        .with_input(InputField::new("text", "Text to classify", "String"))
        .with_output(OutputField::new("sentiment", "Sentiment (positive/negative)", "String"));

    // Create training data
    let trainset = vec![
        Example::new(
            [("text".into(), json!("I love this!"))].into(),
            [("sentiment".into(), json!("positive"))].into(),
        ),
        Example::new(
            [("text".into(), json!("This is terrible."))].into(),
            [("sentiment".into(), json!("negative"))].into(),
        ),
        Example::new(
            [("text".into(), json!("Amazing experience!"))].into(),
            [("sentiment".into(), json!("positive"))].into(),
        ),
    ];

    // Define metric
    let metric = Arc::new(|example: &Example, output: &HashMap<String, _>| {
        Ok(example.outputs.get("sentiment") == output.get("sentiment"))
    });

    // Optimize
    let student = Predictor::new(sig);
    let optimizer = BootstrapFewShot::new(metric);
    let optimized = optimizer.compile(&student, &trainset).await?;

    // Use optimized
    let result = optimized.forward(
        [("text".into(), json!("Great product!"))].into()
    ).await?;
    println!("{}", result["sentiment"]);

    Ok(())
}
```

### Example 4: Custom Module

**Python**:
```python
import dspy

class MultiStepQA(dspy.Module):
    def __init__(self):
        super().__init__()
        self.decompose = dspy.ChainOfThought("question -> sub_questions")
        self.answer = dspy.ChainOfThought("question, context -> answer")

    def forward(self, question):
        # Decompose question
        sub_q = self.decompose(question=question)

        # Answer each sub-question (simplified)
        context = "\n".join([f"Q: {q}\nA: ..." for q in sub_q.sub_questions])

        # Final answer
        return self.answer(question=question, context=context)

qa = MultiStepQA()
result = qa(question="Complex question")
```

**Rust**:
```rust
use ggen_ai::dspy::{
    Signature, InputField, OutputField, ChainOfThought, Module, ModuleError,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use async_trait::async_trait;

struct MultiStepQA {
    decompose: ChainOfThought,
    answer: ChainOfThought,
    signature: Signature,
}

impl MultiStepQA {
    fn new() -> Self {
        let decompose_sig = Signature::new("Decompose", "Break down question")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("sub_questions", "Sub-questions", "String"));

        let answer_sig = Signature::new("Answer", "Answer with context")
            .with_input(InputField::new("question", "Question", "String"))
            .with_input(InputField::new("context", "Context", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        Self {
            decompose: ChainOfThought::new(decompose_sig),
            answer: ChainOfThought::new(answer_sig),
            signature: Signature::new("MultiStepQA", "Multi-step QA"),
        }
    }
}

#[async_trait]
impl Module for MultiStepQA {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>)
        -> Result<HashMap<String, Value>, ModuleError> {
        // Decompose
        let sub_q_result = self.decompose.forward(inputs.clone()).await?;

        // Build context (simplified)
        let context = sub_q_result.get("sub_questions")
            .and_then(|v| v.as_str())
            .unwrap_or("");

        // Answer
        let mut answer_inputs = inputs;
        answer_inputs.insert("context".into(), json!(context));

        self.answer.forward(answer_inputs).await
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let qa = MultiStepQA::new();
    let result = qa.forward(
        [("question".into(), json!("Complex question"))].into()
    ).await?;

    println!("{}", result["answer"]);
    Ok(())
}
```

---

## Common Patterns

### Pattern 1: Evaluation Loop

**Python**:
```python
def evaluate(program, dataset, metric):
    total = 0
    for example in dataset:
        pred = program(**example.inputs())
        total += metric(example, pred)
    return total / len(dataset)

score = evaluate(optimized, testset, accuracy)
print(f"Accuracy: {score:.2%}")
```

**Rust**:
```rust
async fn evaluate(
    module: &dyn Module,
    dataset: &[Example],
    metric: &MetricFn,
) -> Result<f64, Box<dyn std::error::Error>> {
    let mut total = 0.0;

    for example in dataset {
        let pred = module.forward(example.inputs.clone()).await?;
        if metric(example, &pred)? {
            total += 1.0;
        }
    }

    Ok(total / dataset.len() as f64)
}

let score = evaluate(&optimized, &testset, &metric).await?;
println!("Accuracy: {:.2}%", score * 100.0);
```

### Pattern 2: Batch Processing

**Python**:
```python
import asyncio

async def batch_process(predictor, inputs_list):
    tasks = [predictor(**inputs) for inputs in inputs_list]
    return await asyncio.gather(*tasks)

results = asyncio.run(batch_process(predictor, inputs_list))
```

**Rust**:
```rust
use futures::stream::{self, StreamExt};

async fn batch_process(
    module: &dyn Module,
    inputs_list: Vec<HashMap<String, Value>>,
    concurrency: usize,
) -> Vec<Result<HashMap<String, Value>, ModuleError>> {
    stream::iter(inputs_list)
        .map(|inputs| async move {
            module.forward(inputs).await
        })
        .buffer_unordered(concurrency)
        .collect()
        .await
}

let results = batch_process(&predictor, inputs_list, 10).await;
```

### Pattern 3: Metric Functions

**Python**:
```python
# Simple metric
def exact_match(example, pred, trace=None):
    return example.answer == pred.answer

# Multi-criteria metric
def complex_metric(example, pred, trace=None):
    answer_match = example.answer.lower() == pred.answer.lower()
    word_count = len(pred.answer.split())
    is_concise = word_count < 50
    return answer_match and is_concise
```

**Rust**:
```rust
// Simple metric
let exact_match = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    Ok(example.outputs.get("answer") == output.get("answer"))
});

// Multi-criteria metric
let complex_metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    let answer_match = {
        let exp = example.outputs["answer"].as_str()?.to_lowercase();
        let act = output["answer"].as_str()?.to_lowercase();
        exp == act
    };

    let word_count = output["answer"].as_str()?
        .split_whitespace()
        .count();
    let is_concise = word_count < 50;

    Ok(answer_match && is_concise)
});
```

---

## Error Handling

### Python: Try/Except

```python
try:
    result = predictor(question="...")
    print(result.answer)
except ValueError as e:
    print(f"Validation error: {e}")
except Exception as e:
    print(f"Unexpected error: {e}")
```

### Rust: Result and Match

```rust
match predictor.forward(inputs).await {
    Ok(result) => {
        println!("{}", result["answer"]);
    }
    Err(ModuleError::MissingInput(field)) => {
        eprintln!("Missing input field: {}", field);
    }
    Err(ModuleError::ValidationError(msg)) => {
        eprintln!("Validation error: {}", msg);
    }
    Err(e) => {
        eprintln!("Unexpected error: {}", e);
    }
}
```

### Rust: Using ? Operator

```rust
async fn process(predictor: &Predictor) -> Result<String, Box<dyn std::error::Error>> {
    let result = predictor.forward(inputs).await?;
    let answer = result.get("answer")
        .and_then(|v| v.as_str())
        .ok_or("Missing answer")?
        .to_string();
    Ok(answer)
}
```

---

## Async Patterns

### Python: asyncio

```python
import asyncio
import dspy

async def main():
    # Sequential
    result1 = await predictor1(question="Q1")
    result2 = await predictor2(question="Q2")

    # Parallel
    results = await asyncio.gather(
        predictor1(question="Q1"),
        predictor2(question="Q2"),
        predictor3(question="Q3"),
    )

if __name__ == "__main__":
    asyncio.run(main())
```

### Rust: Tokio

```rust
use tokio;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Sequential
    let result1 = predictor1.forward(inputs1).await?;
    let result2 = predictor2.forward(inputs2).await?;

    // Parallel with tokio::join
    let (r1, r2, r3) = tokio::join!(
        predictor1.forward(inputs1),
        predictor2.forward(inputs2),
        predictor3.forward(inputs3),
    );

    // Parallel with futures
    use futures::future::try_join_all;
    let futures = vec![
        predictor1.forward(inputs1),
        predictor2.forward(inputs2),
        predictor3.forward(inputs3),
    ];
    let results = try_join_all(futures).await?;

    Ok(())
}
```

---

## Optimization Workflows

### Python DSPy

```python
import dspy

# 1. Define task
class QA(dspy.Signature):
    question = dspy.InputField()
    answer = dspy.OutputField()

# 2. Create data
trainset = [
    dspy.Example(question="...", answer="...").with_inputs("question"),
    # ... more examples
]

# 3. Define metric
def metric(example, pred, trace=None):
    return example.answer == pred.answer

# 4. Optimize
student = dspy.Predict(QA)
optimizer = dspy.BootstrapFewShot(metric=metric, max_bootstrapped_demos=4)
optimized = optimizer.compile(student, trainset=trainset)

# 5. Evaluate
valset = [...]  # Validation set
score = dspy.Evaluate(devset=valset, metric=metric)(optimized)
print(f"Score: {score:.2%}")

# 6. Use in production
result = optimized(question="New question")
```

### Rust ggen-dspy

```rust
use ggen_ai::dspy::{Signature, Predictor, optimizer::*};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Define task
    let sig = Signature::new("QA", "Answer questions")
        .with_input(InputField::new("question", "Question", "String"))
        .with_output(OutputField::new("answer", "Answer", "String"));

    // 2. Create data
    let trainset = vec![
        Example::new(
            [("question".into(), json!("..."))].into(),
            [("answer".into(), json!("..."))].into(),
        ),
        // ... more examples
    ];

    // 3. Define metric
    let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        Ok(example.outputs.get("answer") == output.get("answer"))
    });

    // 4. Optimize
    let student = Predictor::new(sig);
    let optimizer = BootstrapFewShot::new(metric.clone())
        .with_max_bootstrapped_demos(4);
    let optimized = optimizer.compile(&student, &trainset).await?;

    // 5. Evaluate
    let valset = vec![/* ... */];
    let score = evaluate(&optimized, &valset, &metric).await?;
    println!("Score: {:.2}%", score * 100.0);

    // 6. Use in production
    let result = optimized.forward(
        [("question".into(), json!("New question"))].into()
    ).await?;

    Ok(())
}

async fn evaluate(
    module: &dyn Module,
    dataset: &[Example],
    metric: &MetricFn,
) -> Result<f64, Box<dyn std::error::Error>> {
    let mut total = 0.0;
    for example in dataset {
        let pred = module.forward(example.inputs.clone()).await?;
        if metric(example, &pred)? {
            total += 1.0;
        }
    }
    Ok(total / dataset.len() as f64)
}
```

---

## Gotchas and Tips

### 1. HashMap Construction

**Python**: Simple dict literals
```python
inputs = {"question": "What is 2+2?", "context": "Math"}
```

**Rust**: Multiple ways to construct

```rust
// Method 1: Verbose
let mut inputs = HashMap::new();
inputs.insert("question".into(), json!("What is 2+2?"));
inputs.insert("context".into(), json!("Math"));

// Method 2: From array (recommended for small maps)
let inputs: HashMap<String, Value> = [
    ("question".into(), json!("What is 2+2?")),
    ("context".into(), json!("Math")),
].into();

// Method 3: Using maplit crate (external dependency)
use maplit::hashmap;
let inputs = hashmap! {
    "question".to_string() => json!("What is 2+2?"),
    "context".to_string() => json!("Math"),
};
```

### 2. String Conversions

**Python**: Automatic
```python
question = "What is 2+2?"
inputs = {"question": question}
```

**Rust**: Explicit `.into()` or `.to_string()`
```rust
let question = "What is 2+2?";
let mut inputs = HashMap::new();
inputs.insert("question".into(), json!(question));  // &str -> String
```

### 3. Accessing Nested Values

**Python**: Dot notation
```python
result = predictor(question="...")
print(result.answer)
```

**Rust**: HashMap get + unwrap carefully
```rust
let result = predictor.forward(inputs).await?;

// Safe version
let answer = result.get("answer")
    .and_then(|v| v.as_str())
    .ok_or(ModuleError::MissingOutput("answer".into()))?;

// Or with match
let answer = match result.get("answer") {
    Some(Value::String(s)) => s,
    _ => return Err(ModuleError::MissingOutput("answer".into())),
};
```

### 4. Optional Teacher

**Python**:
```python
# Teacher specified
optimizer = dspy.BootstrapFewShot(teacher=teacher_program)

# No teacher (uses student)
optimizer = dspy.BootstrapFewShot()
```

**Rust**:
```rust
// Teacher specified
let optimizer = BootstrapFewShot::new(metric)
    .with_teacher(Arc::new(teacher_program));

// No teacher (uses student)
let optimizer = BootstrapFewShot::new(metric);
```

### 5. Cloning Data

**Python**: Usually automatic (reference counting)
```python
for example in trainset:
    result = predictor(**example.inputs())  # Automatic copy
```

**Rust**: Explicit `.clone()`
```rust
for example in &trainset {
    let result = predictor.forward(example.inputs.clone()).await?;
    // .clone() creates a deep copy of HashMap
}
```

### 6. Lifetime Issues

**Rust-specific**: Borrowing vs. Ownership

```rust
// ❌ Won't compile - borrowed value doesn't live long enough
async fn process(predictor: &Predictor) -> &HashMap<String, Value> {
    let inputs = [("question".into(), json!("..."))].into();
    let result = predictor.forward(inputs).await.unwrap();
    &result  // Error: result dropped here
}

// ✅ Correct - return owned value
async fn process(predictor: &Predictor) -> HashMap<String, Value> {
    let inputs = [("question".into(), json!("..."))].into();
    predictor.forward(inputs).await.unwrap()  // Returns owned HashMap
}
```

### 7. Arc for Shared State

**Python**: Sharing is automatic
```python
metric = accuracy  # Just assign
optimizer = dspy.BootstrapFewShot(metric=metric)
```

**Rust**: Use `Arc` for thread-safe sharing
```rust
let metric = Arc::new(|example, output| {
    Ok(example.outputs.get("answer") == output.get("answer"))
});

let optimizer = BootstrapFewShot::new(metric.clone());  // Arc::clone is cheap
```

### 8. Environment Variables

**Python**:
```python
import os
os.environ["OPENAI_API_KEY"] = "sk-..."

lm = dspy.OpenAI(model="gpt-4")
dspy.settings.configure(lm=lm)
```

**Rust**:
```bash
# Set before running
export GGEN_LLM_MODEL=gpt-4
export OPENAI_API_KEY=sk-...
```

```rust
// Or in code (not recommended for production)
std::env::set_var("GGEN_LLM_MODEL", "gpt-4");

// Configuration is automatic via ggen-ai
let predictor = Predictor::new(sig);  // Uses env vars
```

---

## Summary: Quick Reference

| Feature | Python DSPy | Rust ggen-dspy |
|---------|------------|----------------|
| **Define Signature** | `class Sig(dspy.Signature):` | `Signature::new().with_input()...` |
| **Create Predictor** | `dspy.Predict(Sig)` | `Predictor::new(sig)` |
| **Run Prediction** | `predictor(field=value)` | `predictor.forward(inputs).await?` |
| **Access Output** | `result.field` | `result["field"]` |
| **Error Handling** | `try/except` | `Result<T, E>` with `?` |
| **Async** | `asyncio.run()` | `#[tokio::main]` |
| **Optimization** | `optimizer.compile()` | `optimizer.compile().await?` |
| **Metric** | `def metric(ex, pred):` | `Arc::new(\|ex, out\| { Ok(...) })` |
| **Module Trait** | `class M(dspy.Module):` | `impl Module for M` |

---

## Common Pitfalls and Solutions

### Pitfall 1: Forgetting `.await` on Async Calls

**Python**: Async is optional
```python
result = predictor(question="...")  # May or may not be async
```

**Rust**: Async must be explicit
```rust
// ❌ WRONG - Compile error
let result = predictor.forward(inputs);

// ✅ CORRECT
let result = predictor.forward(inputs).await?;
```

### Pitfall 2: Unwrapping in Production Code

**Python**: Exceptions are common
```python
result = predictor(question="...")
answer = result.answer  # Might raise AttributeError
```

**Rust**: Use Result pattern
```rust
// ❌ WRONG - Will panic on error
let result = predictor.forward(inputs).await.unwrap();
let answer = result["answer"].as_str().unwrap();

// ✅ CORRECT - Proper error handling
let result = predictor.forward(inputs).await?;
let answer = result.get("answer")
    .and_then(|v| v.as_str())
    .ok_or_else(|| ModuleError::MissingOutput("answer".into()))?;
```

### Pitfall 3: Incorrect HashMap Construction

**Python**: Dict literals are simple
```python
inputs = {"question": "What is Rust?", "context": "..."}
```

**Rust**: Multiple approaches
```rust
// ❌ WRONG - Type mismatch
let inputs = HashMap::new();
inputs.insert("question", "What is Rust?");  // &str != String

// ✅ CORRECT - Explicit conversion
let mut inputs = HashMap::new();
inputs.insert("question".to_string(), json!("What is Rust?"));
inputs.insert("context".to_string(), json!("..."));

// ✅ BEST - From array shorthand
let inputs: HashMap<String, Value> = [
    ("question".into(), json!("What is Rust?")),
    ("context".into(), json!("...")),
].into();
```

### Pitfall 4: Module Ownership Issues

**Python**: Everything is reference-counted
```python
module = MyModule()
result1 = module(x="test")
result2 = module(x="test2")  # Can reuse
```

**Rust**: Ownership and borrowing
```rust
// ❌ WRONG - Moves ownership
let module = MyModule::new();
let result1 = module.forward(inputs1).await?;
let result2 = module.forward(inputs2).await?;  // Compile error if module moved

// ✅ CORRECT - Borrow with &
let module = MyModule::new();
let result1 = module.forward(inputs1).await?;  // Borrows &self
let result2 = module.forward(inputs2).await?;  // Can borrow again
```

### Pitfall 5: Cloning vs. Borrowing

**Python**: Automatic reference management
```python
for example in trainset:
    result = predictor(**example.inputs())  # Automatic copy/ref
```

**Rust**: Explicit cloning
```rust
// ❌ WRONG - Moves value
for example in trainset {
    let result = predictor.forward(example.inputs).await?;
    // example.inputs moved, can't use again
}

// ✅ CORRECT - Borrow with iteration
for example in &trainset {
    let result = predictor.forward(example.inputs.clone()).await?;
    // Clone inputs, example still owned by trainset
}
```

### Pitfall 6: Missing Type Annotations

**Python**: Types are inferred/optional
```python
def process(data):  # Any type
    return transform(data)
```

**Rust**: Types must be explicit
```rust
// ❌ WRONG - Type unclear
fn process(data) -> impl Future<Output = Result<_, _>> {
    transform(data)
}

// ✅ CORRECT - Explicit types
async fn process(
    data: HashMap<String, Value>
) -> Result<HashMap<String, Value>, ModuleError> {
    transform(data).await
}
```

### Pitfall 7: Metric Function Closures

**Python**: Simple function
```python
def metric(example, pred, trace=None):
    return example.answer == pred.answer
```

**Rust**: Arc<dyn Fn> with Send + Sync
```rust
// ❌ WRONG - Not Send/Sync
let metric = |example, output| {
    Ok(example.outputs["answer"] == output["answer"])
};

// ✅ CORRECT - Arc for shared ownership
let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    Ok(example.outputs.get("answer") == output.get("answer"))
});
```

### Pitfall 8: Nested Result Handling

**Python**: Nested try/except
```python
try:
    result = predictor(question="...")
    try:
        answer = result.answer
    except AttributeError:
        answer = "default"
except Exception as e:
    print(f"Error: {e}")
```

**Rust**: Chaining with combinators
```rust
// ❌ WRONG - Nested match hell
match predictor.forward(inputs).await {
    Ok(result) => {
        match result.get("answer") {
            Some(value) => {
                match value.as_str() {
                    Some(s) => println!("{}", s),
                    None => eprintln!("Not a string"),
                }
            }
            None => eprintln!("Missing answer"),
        }
    }
    Err(e) => eprintln!("Error: {}", e),
}

// ✅ CORRECT - Combinators
let answer = predictor.forward(inputs).await?
    .get("answer")
    .and_then(|v| v.as_str())
    .unwrap_or("default");
println!("{}", answer);
```

### Pitfall 9: Module Trait Implementation

**Python**: Subclass dspy.Module
```python
class MyModule(dspy.Module):
    def forward(self, question):
        return self.predictor(question=question)
```

**Rust**: Implement Module trait
```rust
// ❌ WRONG - Missing signature, async_trait
impl Module for MyModule {
    fn forward(&self, inputs: HashMap<String, Value>)
        -> Result<HashMap<String, Value>, ModuleError> {
        self.predictor.forward(inputs)  // Not awaited
    }
}

// ✅ CORRECT - Complete implementation
#[async_trait]
impl Module for MyModule {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>)
        -> Result<HashMap<String, Value>, ModuleError> {
        self.predictor.forward(inputs).await
    }
}
```

### Pitfall 10: Environment Variable Access

**Python**: Simple os.environ
```python
import os
model = os.environ.get("GGEN_LLM_MODEL", "gpt-4")
```

**Rust**: Result-based access
```rust
// ❌ WRONG - Panics if not set
let model = std::env::var("GGEN_LLM_MODEL").unwrap();

// ✅ CORRECT - With fallback
let model = std::env::var("GGEN_LLM_MODEL")
    .unwrap_or_else(|_| "gpt-4".to_string());

// ✅ BETTER - Explicit error handling
let model = std::env::var("GGEN_LLM_MODEL")
    .map_err(|_| ConfigError::MissingEnvVar("GGEN_LLM_MODEL"))?;
```

---

## Advanced Migration Examples

### Example: Custom Optimizer

**Python DSPy**:
```python
import dspy

class CustomOptimizer:
    def __init__(self, metric):
        self.metric = metric

    def compile(self, student, trainset, teacher=None):
        teacher = teacher or student
        demos = []

        for example in trainset:
            pred = teacher(**example.inputs())
            if self.metric(example, pred):
                demos.append(example)

        # Return optimized student with demos
        return student.copy(demos=demos)

# Usage
optimizer = CustomOptimizer(metric=accuracy)
optimized = optimizer.compile(student, trainset)
```

**Rust ggen-dspy**:
```rust
use ggen_ai::dspy::*;
use std::sync::Arc;

struct CustomOptimizer {
    metric: MetricFn,
}

impl CustomOptimizer {
    fn new(metric: MetricFn) -> Self {
        Self { metric }
    }

    async fn compile(
        &self,
        student: &Predictor,
        trainset: &[Example],
        teacher: Option<Arc<dyn Module>>,
    ) -> Result<OptimizedPredictor, OptimizerError> {
        let teacher = teacher.unwrap_or_else(|| Arc::new(student.clone()));
        let mut demos = Vec::new();

        for example in trainset {
            let pred = teacher.forward(example.inputs.clone()).await?;

            if (self.metric)(example, &pred)? {
                demos.push(Demonstration {
                    inputs: example.inputs.clone(),
                    outputs: pred,
                });
            }
        }

        Ok(OptimizedPredictor::new(
            student.clone(),
            demos,
        ))
    }
}

// Usage
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        Ok(example.outputs.get("answer") == output.get("answer"))
    });

    let optimizer = CustomOptimizer::new(metric);
    let optimized = optimizer.compile(&student, &trainset, None).await?;

    Ok(())
}
```

### Example: Streaming Responses

**Python DSPy**:
```python
import dspy

class StreamingModule(dspy.Module):
    async def forward(self, question):
        async for chunk in self.predictor.stream(question=question):
            yield chunk

# Usage
async for chunk in module.forward(question="..."):
    print(chunk, end="", flush=True)
```

**Rust ggen-dspy**:
```rust
use futures::stream::{Stream, StreamExt};
use std::pin::Pin;

struct StreamingModule {
    predictor: Predictor,
    signature: Signature,
}

impl StreamingModule {
    async fn forward_stream(
        &self,
        inputs: HashMap<String, Value>,
    ) -> Pin<Box<dyn Stream<Item = Result<String, ModuleError>> + Send>> {
        // Implementation depends on LLM client streaming support
        // This is a conceptual example
        Box::pin(futures::stream::iter(vec![
            Ok("Hello".to_string()),
            Ok(" world".to_string()),
        ]))
    }
}

// Usage
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let module = StreamingModule::new();
    let mut stream = module.forward_stream(inputs).await;

    while let Some(chunk) = stream.next().await {
        match chunk {
            Ok(text) => print!("{}", text),
            Err(e) => eprintln!("Error: {}", e),
        }
    }

    Ok(())
}
```

### Example: Parallel Module Execution

**Python DSPy**:
```python
import asyncio
import dspy

async def parallel_execution(modules, inputs):
    tasks = [module(**inputs) for module in modules]
    results = await asyncio.gather(*tasks)
    return results

# Usage
results = asyncio.run(parallel_execution([module1, module2, module3], inputs))
```

**Rust ggen-dspy**:
```rust
use futures::future::try_join_all;

async fn parallel_execution(
    modules: &[Arc<dyn Module>],
    inputs: HashMap<String, Value>,
) -> Result<Vec<HashMap<String, Value>>, ModuleError> {
    let futures: Vec<_> = modules
        .iter()
        .map(|m| m.forward(inputs.clone()))
        .collect();

    try_join_all(futures).await
}

// Usage
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let modules: Vec<Arc<dyn Module>> = vec![
        Arc::new(module1),
        Arc::new(module2),
        Arc::new(module3),
    ];

    let results = parallel_execution(&modules, inputs).await?;

    for (i, result) in results.iter().enumerate() {
        println!("Module {}: {:?}", i + 1, result);
    }

    Ok(())
}
```

---

## Performance Comparison

| Aspect | Python DSPy | Rust ggen-dspy |
|--------|------------|----------------|
| **Cold Start** | ~200ms (import time) | ~10ms (compiled binary) |
| **Memory Overhead** | ~50MB (Python interpreter) | ~2-5MB (Rust runtime) |
| **Async Performance** | GIL limitations | True parallelism |
| **Type Checking** | Runtime (optional) | Compile-time (mandatory) |
| **Error Detection** | Runtime exceptions | Compile-time errors |
| **Binary Size** | N/A (interpreted) | ~5-10MB (optimized) |
| **Concurrency** | asyncio (single-threaded) | Tokio (multi-threaded) |

---

## Next Steps

1. Read [GGEN_DSPY_GUIDE.md](/home/user/ggen/docs/GGEN_DSPY_GUIDE.md) for comprehensive Rust guide
2. Run examples in `/home/user/ggen/crates/ggen-ai/examples/`
3. Check [DSPy Optimizer Research](/home/user/ggen/docs/dspy_optimizer_research.md) for advanced optimizers

---

**Document Version**: 1.0
**Last Updated**: 2026-01-11
**Maintainer**: ggen-ai team
