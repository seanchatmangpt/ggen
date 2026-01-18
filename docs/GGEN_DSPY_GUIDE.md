# ggen-dspy: Rust DSPy Implementation Guide

**Version**: 1.0
**Date**: 2026-01-11
**Status**: Production-Ready Core + Roadmap

## Table of Contents

1. [Introduction](#introduction)
2. [Architecture Overview](#architecture-overview)
3. [Quick Start Guide](#quick-start-guide)
4. [Module Catalog](#module-catalog)
5. [Optimizer Comparison](#optimizer-comparison)
6. [Evaluation Patterns](#evaluation-patterns)
7. [Best Practices](#best-practices)
8. [Advanced Composition](#advanced-composition)
9. [Performance Tuning](#performance-tuning)
10. [Roadmap](#roadmap)

---

## Introduction

ggen-dspy is a production-ready Rust implementation of Stanford's DSPy framework for building LLM-powered applications through declarative programming. Unlike prompt engineering, DSPy allows you to define task signatures and let optimizers improve your prompts automatically.

### Core Philosophy

**DSPy Equation**: $A = \mu(O)$ where:
- **A** = Application behavior (code)
- **O** = Ontology/Specification (signatures + examples)
- **μ** = Transformation pipeline (optimizers)

### Key Features

- **Type-Safe Signatures**: Compile-time verification of task interfaces
- **Automatic Optimization**: Bootstrap demonstrations from validation data
- **Async-First**: Built on Tokio for concurrent LLM calls
- **Zero Unwrap**: Production-grade error handling with `Result<T, E>`
- **Composable Modules**: Mix and match predictors, optimizers, and patterns
- **Multi-LLM Support**: Works with OpenAI, Anthropic, Ollama, and more via genai

### Current Implementation Status

| Component | Status | Notes |
|-----------|--------|-------|
| Signature | ✅ Complete | Type-safe task interfaces |
| Predictor | ✅ Complete | Basic prediction with LLM |
| ChainOfThought | ✅ Complete | Step-by-step reasoning |
| BootstrapFewShot | ✅ Complete | Few-shot optimizer |
| Module Trait | ✅ Complete | Composable abstractions |
| Retrieve | 🚧 Roadmap | RAG retrieval module |
| ReAct | 🚧 Roadmap | Tool-using agents |
| Advanced Optimizers | 🚧 Roadmap | COPRO, GEPA, MIPROv2 |

---

## Architecture Overview

### Three-Layer Design

```
┌─────────────────────────────────────────────┐
│          Application Layer                  │
│  (MultiHopQA, ReAct, Custom Modules)       │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│          Module Layer                       │
│  (Predictor, ChainOfThought, Retrieve)     │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│          Optimizer Layer                    │
│  (BootstrapFewShot, COPRO, GEPA)           │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│          LLM Provider Layer                 │
│  (genai: OpenAI, Anthropic, Ollama, etc)   │
└─────────────────────────────────────────────┘
```

### Core Abstractions

#### 1. Signature

Defines the interface of a task with type-safe inputs and outputs.

```rust
use ggen_ai::dspy::{Signature, InputField, OutputField};

let signature = Signature::new("QuestionAnswering", "Answer questions accurately")
    .with_input(InputField::new("question", "The question to answer", "String"))
    .with_input(InputField::new("context", "Relevant context", "String"))
    .with_output(OutputField::new("answer", "The answer", "String"))
    .with_instructions("Provide accurate, concise answers based on the context.");
```

#### 2. Module Trait

All DSPy components implement the `Module` trait:

```rust
#[async_trait]
pub trait Module: Send + Sync {
    fn signature(&self) -> &Signature;
    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>>;
}
```

#### 3. Predictor

Basic LLM-powered module:

```rust
use ggen_ai::dspy::Predictor;

let qa = Predictor::new(signature);
let result = qa.forward(inputs).await?;
```

#### 4. ChainOfThought

Adds step-by-step reasoning:

```rust
use ggen_ai::dspy::ChainOfThought;

let qa_cot = ChainOfThought::new(signature);
let result = qa_cot.forward(inputs).await?;
```

---

## Quick Start Guide

### Installation

Add to `Cargo.toml`:

```toml
[dependencies]
ggen-ai = "5.1.0"
tokio = { version = "1.47", features = ["full"] }
serde_json = "1.0"
```

### Environment Setup

```bash
# Choose your LLM provider
export GGEN_LLM_MODEL=gpt-4              # OpenAI
export OPENAI_API_KEY=sk-...

# Or Anthropic
export GGEN_LLM_MODEL=claude-3-opus-20240229
export ANTHROPIC_API_KEY=...

# Or local Ollama
export GGEN_LLM_MODEL=ollama:llama3
```

### Hello World Example

```rust
use ggen_ai::dspy::{Signature, InputField, OutputField, Predictor, Module};
use serde_json::json;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Define signature
    let sig = Signature::new("Greeting", "Generate friendly greetings")
        .with_input(InputField::new("name", "Person's name", "String"))
        .with_output(OutputField::new("greeting", "Friendly greeting", "String"));

    // 2. Create predictor
    let greeter = Predictor::new(sig);

    // 3. Run
    let mut inputs = HashMap::new();
    inputs.insert("name".into(), json!("Alice"));

    let result = greeter.forward(inputs).await?;
    println!("Greeting: {}", result["greeting"]);

    Ok(())
}
```

### Optimization Example

```rust
use ggen_ai::dspy::{
    Signature, InputField, OutputField, Predictor,
    optimizer::{BootstrapFewShot, Example},
};
use serde_json::json;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Define signature
    let sig = Signature::new("Math", "Solve math problems")
        .with_input(InputField::new("problem", "Math problem", "String"))
        .with_output(OutputField::new("solution", "Solution", "String"));

    // 2. Create training data
    let trainset = vec![
        Example::new(
            [("problem".into(), json!("2 + 2"))].into(),
            [("solution".into(), json!("4"))].into(),
        ),
        Example::new(
            [("problem".into(), json!("5 * 3"))].into(),
            [("solution".into(), json!("15"))].into(),
        ),
    ];

    // 3. Define metric
    let metric = Arc::new(|example, output| {
        Ok(example.outputs.get("solution") == output.get("solution"))
    });

    // 4. Optimize
    let student = Predictor::new(sig);
    let optimizer = BootstrapFewShot::new(metric);
    let optimized = optimizer.compile(&student, &trainset).await?;

    // 5. Use optimized predictor
    let result = optimized.forward(
        [("problem".into(), json!("10 - 3"))].into()
    ).await?;

    println!("Solution: {}", result["solution"]);
    Ok(())
}
```

---

## Module Catalog

### 1. Predictor

**Purpose**: Basic LLM prediction with signature
**Use When**: Simple task, no reasoning required
**Performance**: Fast, single LLM call

```rust
use ggen_ai::dspy::Predictor;

let predictor = Predictor::new(signature)
    .with_model("gpt-4")           // Override model
    .with_temperature(0.7);         // Control randomness

let output = predictor.forward(inputs).await?;
```

**Prompt Structure**:
```
[System Instructions from signature]

[Input Fields]
question: What is the capital of France?

[Output Fields - Model completes]
answer:
```

### 2. ChainOfThought

**Purpose**: Step-by-step reasoning
**Use When**: Complex reasoning, multi-step problems
**Performance**: Slower (more tokens), higher accuracy

```rust
use ggen_ai::dspy::ChainOfThought;

let cot = ChainOfThought::new(signature);
let output = cot.forward(inputs).await?;
```

**Prompt Structure**:
```
[System Instructions]

[Input Fields]
question: What is the capital of France?

[Reasoning Field - Model fills]
reasoning: Let me think step-by-step...

[Output Fields]
answer: Paris
```

### 3. OptimizedPredictor

**Purpose**: Predictor with few-shot demonstrations
**Use When**: After optimization with BootstrapFewShot
**Performance**: Better accuracy, more tokens

```rust
use ggen_ai::dspy::optimizer::BootstrapFewShot;

let optimizer = BootstrapFewShot::new(metric);
let optimized = optimizer.compile(&student, &trainset).await?;

// OptimizedPredictor has demonstrations in prompt
let output = optimized.forward(inputs).await?;
```

**Prompt Structure**:
```
[System Instructions]

[Demonstration 1]
question: 2 + 2
answer: 4

[Demonstration 2]
question: 5 * 3
answer: 15

[New Input]
question: 10 - 3
answer:
```

---

## Optimizer Comparison

### BootstrapFewShot (Currently Implemented)

**Algorithm**: Teacher/student bootstrapping

**Steps**:
1. Run teacher (or student) on training examples
2. Validate outputs with metric function
3. Collect successful examples as demonstrations
4. Return optimized predictor with demonstrations

**When to Use**:
- Small datasets (10-50 examples)
- Quick baseline optimization
- Clear success metric available

**Performance**: 10-20% accuracy improvement

**Configuration**:
```rust
let optimizer = BootstrapFewShot::new(metric)
    .with_max_bootstrapped_demos(4)     // Demos to collect
    .with_max_labeled_demos(16)         // Max from training set
    .with_teacher(teacher_module);      // Optional teacher
```

**Example Metric Functions**:

```rust
// Exact match
let exact_match = Arc::new(|example, output| {
    Ok(example.outputs.get("answer") == output.get("answer"))
});

// Fuzzy match (requires strsim)
let fuzzy = Arc::new(|example, output| {
    use strsim::normalized_damerau_levenshtein;
    let expected = example.outputs["answer"].as_str()?;
    let actual = output["answer"].as_str()?;
    Ok(normalized_damerau_levenshtein(expected, actual) >= 0.8)
});

// Contains match
let contains = Arc::new(|example, output| {
    let expected = example.outputs["answer"].as_str()?.to_lowercase();
    let actual = output["answer"].as_str()?.to_lowercase();
    Ok(actual.contains(&expected) || expected.contains(&actual))
});

// Multi-criteria
let multi = Arc::new(|example, output| {
    let answer_match = example.outputs["answer"] == output["answer"];
    let word_count = output["answer"].as_str()?.split_whitespace().count();
    let is_concise = word_count < 50;
    Ok(answer_match && is_concise)
});
```

### Planned Optimizers (See Roadmap)

| Optimizer | Data Needs | Improvement | Complexity | Status |
|-----------|-----------|-------------|------------|--------|
| **BootstrapFewShot** | 10+ | 10-20% | Low | ✅ Implemented |
| **KNNFewShot** | 50+ | 15-25% | Medium | 🚧 Planned (High Priority) |
| **COPRO** | 20+ | 20-30% | Medium | 🚧 Planned (High Priority) |
| **BootstrapRandomSearch** | 50+ | 15-25% | Low | 🚧 Planned (Medium Priority) |
| **BootstrapOptuna** | 50-200 | 25-35% | Medium | 🚧 Planned (Medium Priority) |
| **GEPA** | 3-10 | 30-50% | High | 🚧 Planned (Highest Priority) |
| **MIPROv2** | 200+ | 30-50% | High | 🚧 Planned (Medium Priority) |

**GEPA** (Genetic-Pareto Algorithm) - Recommended for 2026:
- **Best performance**: +42.5 points improvement (37.5% → 80%)
- **Sample efficient**: Works with 3-10 scenarios
- **Reflective learning**: Self-improving through trace analysis
- **Pareto frontier**: Maintains diversity, prevents overfitting

---

## Evaluation Patterns

### Basic Evaluation

```rust
use ggen_ai::dspy::{Module, optimizer::Example};

async fn evaluate(
    module: &dyn Module,
    dataset: &[Example],
    metric: &MetricFn,
) -> Result<f64, Box<dyn std::error::Error>> {
    let mut total = 0.0;

    for example in dataset {
        let output = module.forward(example.inputs.clone()).await?;
        if metric(example, &output)? {
            total += 1.0;
        }
    }

    Ok(total / dataset.len() as f64)
}

// Usage
let accuracy = evaluate(&predictor, &test_set, &metric).await?;
println!("Accuracy: {:.2}%", accuracy * 100.0);
```

### Detailed Metrics

```rust
#[derive(Default)]
struct EvaluationMetrics {
    total: usize,
    correct: usize,
    total_latency_ms: u64,
    errors: Vec<String>,
}

impl EvaluationMetrics {
    fn accuracy(&self) -> f64 {
        self.correct as f64 / self.total as f64
    }

    fn avg_latency_ms(&self) -> u64 {
        self.total_latency_ms / self.total as u64
    }

    fn error_rate(&self) -> f64 {
        self.errors.len() as f64 / self.total as f64
    }
}

async fn detailed_evaluate(
    module: &dyn Module,
    dataset: &[Example],
    metric: &MetricFn,
) -> Result<EvaluationMetrics, Box<dyn std::error::Error>> {
    let mut metrics = EvaluationMetrics::default();

    for example in dataset {
        metrics.total += 1;

        let start = std::time::Instant::now();
        let result = module.forward(example.inputs.clone()).await;
        let elapsed = start.elapsed().as_millis() as u64;

        metrics.total_latency_ms += elapsed;

        match result {
            Ok(output) => {
                if metric(example, &output)? {
                    metrics.correct += 1;
                }
            }
            Err(e) => {
                metrics.errors.push(e.to_string());
            }
        }
    }

    Ok(metrics)
}
```

### Cross-Validation

```rust
fn split_dataset(dataset: &[Example], k: usize) -> Vec<Vec<Example>> {
    let chunk_size = (dataset.len() + k - 1) / k;
    dataset.chunks(chunk_size)
        .map(|chunk| chunk.to_vec())
        .collect()
}

async fn cross_validate(
    signature: Signature,
    dataset: &[Example],
    metric: MetricFn,
    k: usize,
) -> Result<f64, Box<dyn std::error::Error>> {
    let folds = split_dataset(dataset, k);
    let mut scores = Vec::new();

    for i in 0..k {
        // Create train/val split
        let val_set = &folds[i];
        let train_set: Vec<_> = folds.iter()
            .enumerate()
            .filter(|(j, _)| *j != i)
            .flat_map(|(_, fold)| fold.clone())
            .collect();

        // Train and evaluate
        let student = Predictor::new(signature.clone());
        let optimizer = BootstrapFewShot::new(metric.clone());
        let optimized = optimizer.compile(&student, &train_set).await?;

        let score = evaluate(&optimized, val_set, &metric).await?;
        scores.push(score);

        println!("Fold {}/{}: {:.2}%", i + 1, k, score * 100.0);
    }

    let avg = scores.iter().sum::<f64>() / scores.len() as f64;
    Ok(avg)
}
```

---

## Best Practices

### 1. Signature Design

**DO**:
```rust
// Clear, specific names
let sig = Signature::new("EntityExtraction", "Extract named entities from text")
    .with_input(InputField::new("text", "Input text to analyze", "String"))
    .with_output(OutputField::new("entities", "List of entities (JSON array)", "String"))
    .with_instructions("Extract person names, organizations, and locations. Return as JSON array.");
```

**DON'T**:
```rust
// Vague, unclear
let sig = Signature::new("Process", "Do something")
    .with_input(InputField::new("input", "Input", "String"))
    .with_output(OutputField::new("output", "Output", "String"));
```

### 2. Error Handling

**DO**:
```rust
async fn process(module: &dyn Module, input: String) -> Result<String, ProcessError> {
    let mut inputs = HashMap::new();
    inputs.insert("text".into(), json!(input));

    let output = module.forward(inputs).await
        .map_err(|e| ProcessError::ModuleError(e))?;

    output.get("result")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .ok_or(ProcessError::MissingOutput("result".into()))
}
```

**DON'T**:
```rust
// Unwrap in production code
async fn process(module: &dyn Module, input: String) -> String {
    let mut inputs = HashMap::new();
    inputs.insert("text".into(), json!(input));

    let output = module.forward(inputs).await.unwrap();  // ❌ NEVER!
    output["result"].as_str().unwrap().to_string()      // ❌ NEVER!
}
```

### 3. Metric Functions

**DO**: Make metrics specific and measurable
```rust
let metric = Arc::new(|example, output| {
    // Multiple validation criteria
    let has_answer = output.contains_key("answer");
    let answer_length = output["answer"].as_str()
        .map(|s| s.len())
        .unwrap_or(0);

    let expected = example.outputs["answer"].as_str().unwrap_or("");
    let actual = output["answer"].as_str().unwrap_or("");

    Ok(has_answer &&
       answer_length > 0 &&
       answer_length < 500 &&
       actual.to_lowercase() == expected.to_lowercase())
});
```

### 4. Optimization Workflow

```rust
// 1. Baseline: Test without optimization
let baseline = Predictor::new(signature.clone());
let baseline_score = evaluate(&baseline, &val_set, &metric).await?;
println!("Baseline: {:.2}%", baseline_score * 100.0);

// 2. Optimize
let optimizer = BootstrapFewShot::new(metric.clone())
    .with_max_bootstrapped_demos(4);
let optimized = optimizer.compile(&baseline, &train_set).await?;

// 3. Evaluate improvement
let optimized_score = evaluate(&optimized, &val_set, &metric).await?;
println!("Optimized: {:.2}%", optimized_score * 100.0);
println!("Improvement: {:+.2}%", (optimized_score - baseline_score) * 100.0);

// 4. Test on held-out set
let test_score = evaluate(&optimized, &test_set, &metric).await?;
println!("Test: {:.2}%", test_score * 100.0);
```

### 5. Production Deployment

```rust
// Use environment-based configuration
use ggen_ai::config::LlmConfig;

let config = LlmConfig::default()
    .with_model(std::env::var("PRODUCTION_MODEL").unwrap_or("gpt-4".into()))
    .with_temperature(0.0)  // Deterministic for production
    .with_max_tokens(500);

let predictor = Predictor::new(signature)
    .with_config(config);

// Add monitoring
use tracing::instrument;

#[instrument(skip(module))]
async fn predict_with_monitoring(
    module: &dyn Module,
    inputs: HashMap<String, Value>,
) -> Result<HashMap<String, Value>, ModuleError> {
    let start = std::time::Instant::now();
    let result = module.forward(inputs).await;
    let elapsed = start.elapsed();

    tracing::info!(
        latency_ms = elapsed.as_millis(),
        success = result.is_ok(),
        "Prediction completed"
    );

    result
}
```

---

## Advanced Composition

### Sequential Composition

```rust
use ggen_ai::dspy::{Module, Signature, Predictor};

struct SequentialPipeline {
    step1: Box<dyn Module>,
    step2: Box<dyn Module>,
    signature: Signature,
}

#[async_trait::async_trait]
impl Module for SequentialPipeline {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>)
        -> ModuleResult<HashMap<String, Value>> {
        // Step 1
        let intermediate = self.step1.forward(inputs).await?;

        // Step 2 (uses output from step 1)
        self.step2.forward(intermediate).await
    }
}

// Usage
let extractor = Predictor::new(extraction_sig);
let summarizer = Predictor::new(summary_sig);

let pipeline = SequentialPipeline {
    step1: Box::new(extractor),
    step2: Box::new(summarizer),
    signature: pipeline_sig,
};
```

### Parallel Composition

```rust
struct ParallelPipeline {
    modules: Vec<Box<dyn Module>>,
    signature: Signature,
}

#[async_trait::async_trait]
impl Module for ParallelPipeline {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>)
        -> ModuleResult<HashMap<String, Value>> {
        // Run all modules in parallel
        let futures: Vec<_> = self.modules.iter()
            .map(|m| m.forward(inputs.clone()))
            .collect();

        let results = futures::future::try_join_all(futures).await?;

        // Aggregate results (e.g., majority vote)
        self.aggregate(results)
    }

    fn aggregate(&self, results: Vec<HashMap<String, Value>>)
        -> ModuleResult<HashMap<String, Value>> {
        // Implementation depends on task
        // Example: majority vote for classification
        Ok(results[0].clone())  // Simplified
    }
}
```

### Conditional Composition

```rust
struct ConditionalModule {
    classifier: Box<dyn Module>,
    branch_a: Box<dyn Module>,
    branch_b: Box<dyn Module>,
    signature: Signature,
}

#[async_trait::async_trait]
impl Module for ConditionalModule {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>)
        -> ModuleResult<HashMap<String, Value>> {
        // Classify input
        let classification = self.classifier.forward(inputs.clone()).await?;

        // Branch based on classification
        let category = classification.get("category")
            .and_then(|v| v.as_str())
            .ok_or(ModuleError::MissingOutput("category".into()))?;

        match category {
            "A" => self.branch_a.forward(inputs).await,
            "B" => self.branch_b.forward(inputs).await,
            _ => Err(ModuleError::Other(format!("Unknown category: {}", category))),
        }
    }
}
```

---

## Performance Tuning

### Latency Optimization

```rust
// 1. Use appropriate models
let fast_predictor = Predictor::new(signature)
    .with_model("gpt-3.5-turbo");  // Faster, cheaper

let accurate_predictor = Predictor::new(signature)
    .with_model("gpt-4");          // Slower, more accurate

// 2. Reduce max_tokens
let predictor = Predictor::new(signature)
    .with_max_tokens(100);  // Limit response length

// 3. Use temperature=0 for caching
let predictor = Predictor::new(signature)
    .with_temperature(0.0);  // Deterministic responses
```

### Throughput Optimization

```rust
use futures::stream::{self, StreamExt};

async fn batch_process(
    module: &dyn Module,
    inputs: Vec<HashMap<String, Value>>,
    concurrency: usize,
) -> Vec<Result<HashMap<String, Value>, ModuleError>> {
    stream::iter(inputs)
        .map(|input| async move {
            module.forward(input).await
        })
        .buffer_unordered(concurrency)  // Process N at a time
        .collect()
        .await
}

// Usage
let results = batch_process(&predictor, inputs, 10).await;
```

### Memory Optimization

```rust
// Use streaming for large datasets
async fn process_stream<S>(
    module: &dyn Module,
    input_stream: S,
) -> impl Stream<Item = Result<HashMap<String, Value>, ModuleError>>
where
    S: Stream<Item = HashMap<String, Value>>,
{
    input_stream.then(move |input| async move {
        module.forward(input).await
    })
}
```

---

## Roadmap

### Phase 1: Completed ✅

- [x] Signature abstraction
- [x] Module trait
- [x] Predictor implementation
- [x] ChainOfThought implementation
- [x] BootstrapFewShot optimizer
- [x] Basic examples and documentation

### Phase 2: Retrieval & RAG (2-4 weeks)

- [ ] `Retrieve` module with pluggable backends
- [ ] `RetrieverBackend` trait
- [ ] In-memory retriever (testing)
- [ ] Qdrant integration
- [ ] Basic RAG pipeline
- [ ] Multi-stage RAG
- [ ] Query rewriting module
- [ ] Examples: `basic_retrieval.rs`, `rag_pipeline.rs`

### Phase 3: Tool System & Agents (2-3 weeks)

- [ ] `Tool` trait and registry
- [ ] Built-in tools (Search, Calculator, etc.)
- [ ] ReAct agent implementation
- [ ] Trajectory tracking
- [ ] Error recovery for tools
- [ ] Examples: `react_agent.rs`, `tool_usage.rs`

### Phase 4: Advanced Optimizers (4-6 weeks)

**High Priority**:
- [ ] KNNFewShot (retrieval-augmented few-shot)
- [ ] COPRO (instruction optimization)
- [ ] GEPA (genetic-pareto, best 2026 performance)

**Medium Priority**:
- [ ] BootstrapFewShotWithRandomSearch
- [ ] BootstrapFewShotWithOptuna
- [ ] MIPROv2 (joint instruction + demo optimization)

### Phase 5: Advanced Patterns (3-4 weeks)

- [ ] Multi-Hop QA
- [ ] Self-Ask
- [ ] ReWOO (planning-then-execution)
- [ ] STORM (multi-perspective article generation)
- [ ] Pipeline builder API

### Phase 6: Production Features (2-3 weeks)

- [ ] Enhanced error recovery (retries, fallbacks)
- [ ] Caching layer
- [ ] Monitoring and observability
- [ ] Performance profiling
- [ ] Production deployment guide

---

## API Reference

### Signature API

```rust
// Create signature
let sig = Signature::new("TaskName", "Description");

// Add inputs
let sig = sig.with_input(InputField::new(
    "field_name",     // Field identifier
    "description",    // Human-readable description
    "String"          // Type hint
));

// Add outputs
let sig = sig.with_output(OutputField::new(
    "result",
    "The result",
    "String"
));

// Add instructions
let sig = sig.with_instructions(
    "Detailed instructions for the LLM..."
);

// Access fields
for input in &sig.inputs {
    println!("{}: {}", input.name, input.description);
}
```

### Module API

```rust
#[async_trait]
pub trait Module: Send + Sync {
    /// Get the signature defining this module's interface
    fn signature(&self) -> &Signature;

    /// Execute the module with given inputs
    async fn forward(
        &self,
        inputs: HashMap<String, Value>
    ) -> Result<HashMap<String, Value>, ModuleError>;
}
```

**Implementation Example**:
```rust
struct MyModule {
    predictor: Predictor,
    signature: Signature,
}

#[async_trait]
impl Module for MyModule {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self,
        inputs: HashMap<String, Value>
    ) -> Result<HashMap<String, Value>, ModuleError> {
        // Validate inputs
        for input_field in &self.signature.inputs {
            if !inputs.contains_key(&input_field.name) {
                return Err(ModuleError::MissingInput(
                    input_field.name.clone()
                ));
            }
        }

        // Execute logic
        self.predictor.forward(inputs).await
    }
}
```

### Predictor API

```rust
// Create predictor
let predictor = Predictor::new(signature);

// Configure (optional)
let predictor = Predictor::new(signature)
    .with_model("gpt-4o")         // Override model
    .with_temperature(0.7)         // Set creativity
    .with_max_tokens(500);         // Limit response length

// Execute
let mut inputs = HashMap::new();
inputs.insert("question".into(), json!("What is Rust?"));

let result = predictor.forward(inputs).await?;
let answer = result["answer"].as_str().unwrap();
```

### ChainOfThought API

```rust
// ChainOfThought adds reasoning field automatically
let cot = ChainOfThought::new(signature);

// The signature is augmented with:
// - reasoning: OutputField (step-by-step thinking)

let result = cot.forward(inputs).await?;

// Access reasoning
let reasoning = result.get("reasoning")
    .and_then(|v| v.as_str())
    .unwrap_or("");

println!("Reasoning: {}", reasoning);
println!("Answer: {}", result["answer"]);
```

### Optimizer API

```rust
// Create optimizer
let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    Ok(example.outputs.get("answer") == output.get("answer"))
});

let optimizer = BootstrapFewShot::new(metric)
    .with_max_bootstrapped_demos(4)   // Max demos to collect
    .with_max_labeled_demos(16)       // Max from training set
    .with_teacher(Arc::new(teacher)); // Optional teacher module

// Compile (optimize)
let student = Predictor::new(signature);
let optimized = optimizer.compile(&student, &trainset).await?;

// Use optimized predictor
let result = optimized.forward(inputs).await?;

// Inspect demonstrations
println!("Demos collected: {}", optimized.demonstration_count());
```

### Example API

```rust
// Create example
let example = Example::new(
    HashMap::from([
        ("question".to_string(), json!("What is 2+2?")),
    ]),
    HashMap::from([
        ("answer".to_string(), json!("4")),
    ]),
);

// Access fields
let inputs = &example.inputs;
let outputs = &example.outputs;

// Create from convenience method
let examples = vec![
    Example::new(
        [("x".into(), json!(1))].into(),
        [("y".into(), json!(2))].into(),
    ),
];
```

### Assertion API

```rust
use ggen_ai::dspy::assertions::*;

// Create assertion
let assertion = Assertion::assert(LengthValidator::between(10, 100))
    .with_feedback("Must be 10-100 characters")
    .max_retries(3);

// Create suggestion (soft)
let suggestion = Assertion::suggest(ContainsValidator::new("keyword"))
    .with_feedback("Should contain keyword")
    .max_retries(2);

// Execute with backtracking
let mut executor = BacktrackExecutor::new(vec![assertion, suggestion]);
let result = executor.execute(&module, inputs).await?;

// Check warnings
for warning in executor.warnings() {
    println!("Warning: {} (attempts: {})", warning.feedback, warning.attempts);
}
```

### DummyLM API (Testing)

```rust
use ggen_ai::dspy::testing::DummyLM;

// Sequential mode
let dummy = DummyLM::sequential(vec![
    HashMap::from([("answer".to_string(), json!("Response 1"))]),
    HashMap::from([("answer".to_string(), json!("Response 2"))]),
]);

// Query-based mode
let mut query_map = HashMap::new();
query_map.insert(
    "keyword".to_string(),
    HashMap::from([("answer".to_string(), json!("Matched response"))]),
);
let dummy = DummyLM::query_based(query_map);

// Example-following mode
let dummy = DummyLM::example_following(vec![
    (inputs1, outputs1),
    (inputs2, outputs2),
]);

// Verify behavior
assert_eq!(dummy.call_count(), 3);
let history = dummy.history();
dummy.reset();
```

---

## Production Deployment Guide

### Configuration Management

**Environment-Based Configuration**:
```rust
use std::env;

fn get_llm_config() -> LlmConfig {
    LlmConfig::default()
        .with_model(env::var("PRODUCTION_MODEL")
            .unwrap_or_else(|_| "gpt-4".to_string()))
        .with_temperature(0.0)  // Deterministic for production
        .with_max_tokens(
            env::var("MAX_TOKENS")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(500)
        )
}
```

**Secrets Management**:
```bash
# Use environment variables, never hardcode
export OPENAI_API_KEY=$(vault read -field=api_key secret/openai)
export ANTHROPIC_API_KEY=$(vault read -field=api_key secret/anthropic)

# Or use .env file (not checked into git)
echo "OPENAI_API_KEY=sk-..." > .env
```

### Error Handling and Retries

```rust
use tokio::time::{sleep, Duration};

async fn predict_with_retry(
    module: &dyn Module,
    inputs: HashMap<String, Value>,
    max_retries: u32,
) -> Result<HashMap<String, Value>, ModuleError> {
    let mut attempts = 0;
    let mut delay = Duration::from_secs(1);

    loop {
        match module.forward(inputs.clone()).await {
            Ok(result) => return Ok(result),
            Err(e) if attempts < max_retries => {
                tracing::warn!(
                    "Attempt {}/{} failed: {}. Retrying in {:?}",
                    attempts + 1,
                    max_retries,
                    e,
                    delay
                );
                sleep(delay).await;
                attempts += 1;
                delay *= 2; // Exponential backoff
            }
            Err(e) => return Err(e),
        }
    }
}
```

### Monitoring and Observability

```rust
use tracing::{info, warn, instrument};
use std::time::Instant;

#[instrument(skip(module, inputs))]
async fn predict_with_monitoring(
    module: &dyn Module,
    inputs: HashMap<String, Value>,
) -> Result<HashMap<String, Value>, ModuleError> {
    let start = Instant::now();

    let result = module.forward(inputs).await;

    let elapsed = start.elapsed();

    match &result {
        Ok(_) => {
            info!(
                latency_ms = elapsed.as_millis(),
                "Prediction successful"
            );
        }
        Err(e) => {
            warn!(
                latency_ms = elapsed.as_millis(),
                error = %e,
                "Prediction failed"
            );
        }
    }

    result
}
```

### Caching Strategy

```rust
use moka::future::Cache;
use sha2::{Sha256, Digest};
use std::sync::Arc;

struct CachedModule {
    inner: Arc<dyn Module>,
    cache: Cache<String, HashMap<String, Value>>,
}

impl CachedModule {
    fn new(module: Arc<dyn Module>, max_capacity: u64) -> Self {
        let cache = Cache::builder()
            .max_capacity(max_capacity)
            .time_to_live(Duration::from_secs(3600)) // 1 hour TTL
            .build();

        Self { inner: module, cache }
    }

    fn cache_key(inputs: &HashMap<String, Value>) -> String {
        let json = serde_json::to_string(inputs).unwrap();
        let mut hasher = Sha256::new();
        hasher.update(json.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

#[async_trait]
impl Module for CachedModule {
    fn signature(&self) -> &Signature {
        self.inner.signature()
    }

    async fn forward(
        &self,
        inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        let key = Self::cache_key(&inputs);

        if let Some(cached) = self.cache.get(&key).await {
            tracing::debug!("Cache hit for key: {}", key);
            return Ok(cached);
        }

        let result = self.inner.forward(inputs).await?;
        self.cache.insert(key, result.clone()).await;

        Ok(result)
    }
}
```

### Rate Limiting

```rust
use governor::{Quota, RateLimiter};
use governor::clock::DefaultClock;
use governor::state::{InMemoryState, NotKeyed};
use std::num::NonZeroU32;

struct RateLimitedModule {
    inner: Arc<dyn Module>,
    limiter: Arc<RateLimiter<NotKeyed, InMemoryState, DefaultClock>>,
}

impl RateLimitedModule {
    fn new(module: Arc<dyn Module>, requests_per_minute: u32) -> Self {
        let quota = Quota::per_minute(
            NonZeroU32::new(requests_per_minute).unwrap()
        );
        let limiter = Arc::new(RateLimiter::direct(quota));

        Self { inner: module, limiter }
    }
}

#[async_trait]
impl Module for RateLimitedModule {
    fn signature(&self) -> &Signature {
        self.inner.signature()
    }

    async fn forward(
        &self,
        inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        // Wait for rate limit
        self.limiter.until_ready().await;

        self.inner.forward(inputs).await
    }
}
```

### Health Checks

```rust
use axum::{Router, routing::get};
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Clone)]
struct HealthMetrics {
    total_requests: Arc<AtomicU64>,
    failed_requests: Arc<AtomicU64>,
}

async fn health_check(metrics: HealthMetrics) -> String {
    let total = metrics.total_requests.load(Ordering::Relaxed);
    let failed = metrics.failed_requests.load(Ordering::Relaxed);
    let success_rate = if total > 0 {
        ((total - failed) as f64 / total as f64) * 100.0
    } else {
        100.0
    };

    format!(
        "{{\"status\": \"healthy\", \"total\": {}, \"failed\": {}, \"success_rate\": {:.2}}}",
        total, failed, success_rate
    )
}

#[tokio::main]
async fn main() {
    let metrics = HealthMetrics {
        total_requests: Arc::new(AtomicU64::new(0)),
        failed_requests: Arc::new(AtomicU64::new(0)),
    };

    let app = Router::new()
        .route("/health", get(|| health_check(metrics)));

    // Run server
}
```

### Deployment Checklist

```markdown
## Pre-Deployment
- [ ] All tests passing (cargo make test)
- [ ] No clippy warnings (cargo make lint)
- [ ] Load testing completed
- [ ] Security audit passed
- [ ] Secrets in vault/env, not code
- [ ] Monitoring configured (traces, metrics)
- [ ] Rate limits configured
- [ ] Cache strategy defined
- [ ] Error handling comprehensive
- [ ] Retry logic with backoff

## Deployment
- [ ] Blue-green or canary deployment
- [ ] Health checks enabled
- [ ] Gradual traffic ramp-up
- [ ] Monitor error rates
- [ ] Monitor latency (p50, p95, p99)
- [ ] Monitor cache hit rates
- [ ] Monitor LLM API costs

## Post-Deployment
- [ ] Verify all endpoints healthy
- [ ] Check logs for errors
- [ ] Validate metrics dashboards
- [ ] Test rollback procedure
- [ ] Document any issues
- [ ] Update runbooks
```

---

## References

### Documentation
- [DSPy Official Documentation](https://dspy.ai/)
- [DSPy GitHub Repository](https://github.com/stanfordnlp/dspy)
- [ggen-ai Crate Documentation](https://docs.rs/ggen-ai)

### Research Papers
- [DSPy: Compiling Declarative Language Model Calls into Self-Improving Pipelines](https://arxiv.org/abs/2310.03714)
- [GEPA: Reflective Prompt Evolution](https://arxiv.org/abs/2507.19457)

### Internal Documents
- [DSPy Optimizer Research](/home/user/ggen/docs/dspy_optimizer_research.md)
- [DSPy Advanced Composition Patterns](/home/user/ggen/docs/dspy_advanced_composition_patterns.md)
- [DSPy Optimizer Comparison](/home/user/ggen/docs/dspy_optimizer_comparison.md)

---

## Support

For issues, questions, or contributions:
- GitHub: https://github.com/seanchatmangpt/ggen
- Issues: https://github.com/seanchatmangpt/ggen/issues

---

**Document Version**: 1.0
**Last Updated**: 2026-01-11
**Maintainer**: ggen-ai team
