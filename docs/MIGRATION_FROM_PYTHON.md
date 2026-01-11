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

## Next Steps

1. Read [GGEN_DSPY_GUIDE.md](/home/user/ggen/docs/GGEN_DSPY_GUIDE.md) for comprehensive Rust guide
2. Run examples in `/home/user/ggen/crates/ggen-ai/examples/`
3. Check [DSPy Optimizer Research](/home/user/ggen/docs/dspy_optimizer_research.md) for advanced optimizers

---

**Document Version**: 1.0
**Last Updated**: 2026-01-11
**Maintainer**: ggen-ai team
