# Rust-GenAI Usage Guide for ggen-dspy

**Version**: 1.0.0
**Date**: 2026-01-11
**Crate**: ggen-dspy
**Dependencies**: rust-genai 0.1.x, ggen-ai 1.0+

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Client Initialization](#client-initialization)
4. [Model Configuration](#model-configuration)
5. [Request/Response Patterns](#requestresponse-patterns)
6. [Adapter System](#adapter-system)
7. [Error Handling](#error-handling)
8. [Async/Await Best Practices](#asyncawait-best-practices)
9. [Retry and Timeout Configuration](#retry-and-timeout-configuration)
10. [Token Counting and Cost Tracking](#token-counting-and-cost-tracking)
11. [Caching Strategies](#caching-strategies)
12. [Provider-Specific Guidance](#provider-specific-guidance)
13. [Testing with DummyLM](#testing-with-dummylm)
14. [Common Pitfalls](#common-pitfalls)
15. [Troubleshooting](#troubleshooting)
16. [Migration Guide](#migration-guide)
17. [Quick Reference](#quick-reference)

---

## Overview

The `ggen-dspy` crate integrates `rust-genai` for production-ready multi-provider LLM integration. This guide covers the complete usage patterns, best practices, and advanced features for working with `rust-genai` in the context of DSPy-style programming.

### Key Features

- **Multi-provider Support**: OpenAI, Anthropic, Ollama, and more via rust-genai
- **Adapter Pattern**: Flexible prompt formatting and response parsing
- **Automatic Retry**: Exponential backoff with configurable limits
- **Response Caching**: Memory-based caching with TTL
- **Token Tracking**: Comprehensive usage statistics and cost tracking
- **Type Safety**: `Result<T, E>` throughout, zero unwrap/expect in production
- **Testing Support**: DummyLM for deterministic testing without API calls

### Core Equation

```
DSPy Module = Signature + LLM Client + Adapter
```

Where:
- **Signature**: Input/output specification
- **LLM Client**: rust-genai client (GenAiClient)
- **Adapter**: Prompt formatting and response parsing logic

---

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────┐
│                     ggen-dspy Module                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │  Predictor   │  │ ChainOfThought│  │   ReAct      │     │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘     │
│         │                  │                  │              │
│         └──────────────────┴──────────────────┘              │
│                            │                                 │
│                  ┌─────────▼─────────┐                      │
│                  │  Adapter Layer     │                      │
│                  │  ┌──────────────┐ │                      │
│                  │  │ ChatAdapter  │ │                      │
│                  │  │ JSONAdapter  │ │                      │
│                  │  │ CompAdapter  │ │                      │
│                  │  └──────┬───────┘ │                      │
│                  └─────────┼─────────┘                      │
│                            │                                 │
│                  ┌─────────▼─────────┐                      │
│                  │ IntegratedAdapter │                      │
│                  │  - Retry Logic    │                      │
│                  │  - Caching        │                      │
│                  │  - Token Tracking │                      │
│                  └─────────┬─────────┘                      │
└────────────────────────────┼─────────────────────────────────┘
                             │
                  ┌──────────▼──────────┐
                  │   rust-genai        │
                  │  (GenAiClient)      │
                  └──────────┬──────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
   ┌────▼────┐         ┌────▼────┐         ┌────▼────┐
   │ OpenAI  │         │Anthropic│         │ Ollama  │
   └─────────┘         └─────────┘         └─────────┘
```

### Data Flow

```rust
// 1. User creates module with signature
let predictor = Predictor::new(signature)
    .with_temperature(0.7)
    .with_max_tokens(1024);

// 2. Module forwards inputs
let result = predictor.forward(&[("question", "What is Rust?")]).await?;

// 3. Internal flow:
// a. Build prompt from signature + inputs
// b. Call LLM via rust-genai client
// c. Parse response into structured output
// d. Return ModuleOutput with fields

// 4. User extracts results
let answer = result.get("answer")?;
```

---

## Client Initialization

### Basic Initialization

```rust
use ggen_ai::{GenAiClient, LlmConfig, LlmClient};
use ggen_dspy::adapters::GgenAiAdapter;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Create LLM configuration
    let config = LlmConfig {
        model: "gpt-4o".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    };

    // 2. Initialize client
    let client = GenAiClient::new(config)?;

    // 3. Create adapter
    let adapter = GgenAiAdapter::new(client);

    Ok(())
}
```

### Environment-Based Configuration

```rust
use ggen_ai::LlmConfig;
use std::env;

// Load from environment variables
fn create_config_from_env() -> LlmConfig {
    LlmConfig {
        model: env::var("GGEN_LLM_MODEL")
            .unwrap_or_else(|_| "gpt-4o".to_string()),
        max_tokens: env::var("LLM_MAX_TOKENS")
            .ok()
            .and_then(|v| v.parse().ok())
            .or(Some(4096)),
        temperature: env::var("LLM_TEMPERATURE")
            .ok()
            .and_then(|v| v.parse().ok())
            .or(Some(0.7)),
        top_p: env::var("LLM_TOP_P")
            .ok()
            .and_then(|v| v.parse().ok())
            .or(Some(0.9)),
        stop: None,
        extra: HashMap::new(),
    }
}
```

### Configuration Validation

```rust
use ggen_ai::LlmConfig;

impl LlmConfig {
    pub fn validate(&self) -> Result<()> {
        // Model name cannot be empty
        if self.model.is_empty() {
            return Err(GgenAiError::configuration("Model name cannot be empty"));
        }

        // Validate max_tokens
        if let Some(max_tokens) = self.max_tokens {
            if !(1..=128000).contains(&max_tokens) {
                return Err(GgenAiError::configuration(
                    "Max tokens must be between 1 and 128000"
                ));
            }
        }

        // Validate temperature (0.0 to 2.0)
        if let Some(temperature) = self.temperature {
            if !(0.0..=2.0).contains(&temperature) {
                return Err(GgenAiError::configuration(
                    "Temperature must be between 0.0 and 2.0"
                ));
            }
        }

        // Validate top_p (0.0 to 1.0)
        if let Some(top_p) = self.top_p {
            if !(0.0..=1.0).contains(&top_p) {
                return Err(GgenAiError::configuration(
                    "Top-p must be between 0.0 and 1.0"
                ));
            }
        }

        Ok(())
    }
}
```

### Default Configuration

```rust
use ggen_ai::LlmConfig;

// Use sensible defaults
let config = LlmConfig::default();

// Or customize with builder pattern
let config = LlmConfig::default()
    .with_model("gpt-4o")
    .with_max_tokens(4096)
    .with_temperature(0.7);
```

---

## Model Configuration

### Temperature Settings

Temperature controls randomness in LLM outputs (0.0 to 2.0):

```rust
use ggen_dspy::Predictor;
use ggen_ai::dspy::Signature;

// Conservative (factual, deterministic)
let predictor = Predictor::new(signature)
    .with_temperature(0.0);  // Most deterministic

// Balanced (recommended for most tasks)
let predictor = Predictor::new(signature)
    .with_temperature(0.7);  // Default, good balance

// Creative (diverse, varied outputs)
let predictor = Predictor::new(signature)
    .with_temperature(1.5);  // More creative

// Very creative (experimental)
let predictor = Predictor::new(signature)
    .with_temperature(2.0);  // Maximum creativity
```

**Use Cases**:
- **0.0 - 0.3**: Factual QA, code generation, classification
- **0.4 - 0.7**: General purpose, reasoning tasks
- **0.8 - 1.2**: Creative writing, brainstorming
- **1.3 - 2.0**: Experimental, highly diverse outputs

### Max Tokens Configuration

```rust
// Short responses (summaries, classifications)
let predictor = Predictor::new(signature)
    .with_max_tokens(256);

// Medium responses (paragraphs, explanations)
let predictor = Predictor::new(signature)
    .with_max_tokens(1024);  // Default

// Long responses (articles, detailed analysis)
let predictor = Predictor::new(signature)
    .with_max_tokens(4096);

// Maximum length (for very long documents)
let predictor = Predictor::new(signature)
    .with_max_tokens(16384);  // Model-dependent
```

### Model Selection

```rust
// OpenAI GPT-4 (best reasoning)
let config = LlmConfig {
    model: "gpt-4o".to_string(),
    ..Default::default()
};

// OpenAI GPT-4 Turbo (fast, cost-effective)
let config = LlmConfig {
    model: "gpt-4-turbo".to_string(),
    ..Default::default()
};

// Anthropic Claude (long context)
let config = LlmConfig {
    model: "claude-sonnet-4-5-20250929".to_string(),
    ..Default::default()
};

// Ollama (local, private)
let config = LlmConfig {
    model: "llama3:70b".to_string(),
    ..Default::default()
};
```

### Advanced Parameters

```rust
use std::collections::HashMap;

let mut extra = HashMap::new();

// Frequency penalty (reduce repetition)
extra.insert(
    "frequency_penalty".to_string(),
    serde_json::json!(0.5)
);

// Presence penalty (encourage new topics)
extra.insert(
    "presence_penalty".to_string(),
    serde_json::json!(0.3)
);

// Stop sequences
let config = LlmConfig {
    model: "gpt-4o".to_string(),
    stop: Some(vec![
        "\n\n".to_string(),
        "END".to_string()
    ]),
    extra,
    ..Default::default()
};
```

---

## Request/Response Patterns

### Basic Request Pattern

```rust
use ggen_dspy::{Predictor, Result};
use ggen_ai::dspy::{Signature, InputField, OutputField};

#[tokio::main]
async fn main() -> Result<()> {
    // 1. Define signature
    let signature = Signature::new("QA", "Answer questions accurately")
        .with_input(InputField::new("question", "The question", "String"))
        .with_output(OutputField::new("answer", "The answer", "String"));

    // 2. Create predictor
    let predictor = Predictor::new(signature)
        .with_temperature(0.7)
        .with_max_tokens(1024);

    // 3. Forward inputs
    let inputs = vec![("question", "What is Rust?")];
    let output = predictor.forward(&inputs).await?;

    // 4. Extract result
    let answer = output.get("answer")?;
    println!("Answer: {}", answer);

    Ok(())
}
```

### Multiple Output Fields

```rust
// Signature with multiple outputs
let signature = Signature::new("Analysis", "Analyze text")
    .with_input(InputField::new("text", "Text to analyze", "String"))
    .with_output(OutputField::new("sentiment", "Sentiment (positive/negative/neutral)", "String"))
    .with_output(OutputField::new("confidence", "Confidence score (0-1)", "Float"))
    .with_output(OutputField::new("summary", "Brief summary", "String"));

let predictor = Predictor::new(signature);

let inputs = vec![("text", "This product is amazing!")];
let output = predictor.forward(&inputs).await?;

let sentiment = output.get("sentiment")?;
let confidence = output.get("confidence")?;
let summary = output.get("summary")?;

println!("Sentiment: {} (confidence: {})", sentiment, confidence);
println!("Summary: {}", summary);
```

### Batch Processing Pattern

```rust
use futures::future::join_all;

async fn batch_process(
    predictor: &Predictor,
    questions: Vec<&str>
) -> Result<Vec<String>> {
    let futures: Vec<_> = questions
        .iter()
        .map(|question| async move {
            let inputs = vec![("question", *question)];
            let output = predictor.forward(&inputs).await?;
            output.get("answer").map(|s| s.to_string())
        })
        .collect();

    let results = join_all(futures).await;

    results.into_iter().collect()
}

// Usage
let questions = vec![
    "What is Rust?",
    "What is DSPy?",
    "What is rust-genai?"
];

let answers = batch_process(&predictor, questions).await?;
```

### Streaming Pattern

```rust
use ggen_ai::LlmClient;
use futures::StreamExt;

async fn stream_completion(client: &GenAiClient, prompt: &str) -> Result<String> {
    let mut stream = client.complete_stream(prompt).await?;
    let mut full_response = String::new();

    while let Some(chunk) = stream.next().await {
        print!("{}", chunk.content);  // Print as it arrives
        full_response.push_str(&chunk.content);
    }

    Ok(full_response)
}
```

### Error Recovery Pattern

```rust
async fn robust_completion(
    predictor: &Predictor,
    inputs: &[(&str, &str)],
    max_attempts: usize
) -> Result<ModuleOutput> {
    let mut last_error = None;

    for attempt in 1..=max_attempts {
        match predictor.forward(inputs).await {
            Ok(output) => return Ok(output),
            Err(e) => {
                warn!("Attempt {} failed: {}", attempt, e);
                last_error = Some(e);

                // Exponential backoff
                let delay = Duration::from_millis(100 * 2_u64.pow(attempt as u32));
                tokio::time::sleep(delay).await;
            }
        }
    }

    Err(last_error.unwrap())
}
```

---

## Adapter System

### Adapter Types Overview

The ggen-dspy adapter system provides three built-in adapters:

1. **ChatAdapter**: Natural language format with field markers `[[ ## field ## ]]`
2. **JSONAdapter**: Structured JSON output (model-dependent)
3. **CompletionAdapter**: Simple text completion

### ChatAdapter Usage

Best for models that don't support JSON mode reliably:

```rust
use ggen_dspy::adapters::{ChatAdapter, LlmAdapter};
use serde_json::Value;
use std::collections::HashMap;

// Create adapter
let adapter = ChatAdapter::new();

// Prepare inputs
let mut inputs = HashMap::new();
inputs.insert("question".to_string(), Value::String("What is Rust?".to_string()));

// Define output fields
let output_fields = vec!["answer".to_string()];

// Format prompt
let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;

// Prompt will look like:
// question: What is Rust?
//
// [[ ## answer ## ]]

// Parse LLM response
let llm_response = "[[ ## answer ## ]]\nRust is a systems programming language.";
let parsed = adapter.parse_response(llm_response, &output_fields)?;

assert_eq!(
    parsed.get("answer").unwrap(),
    &Value::String("Rust is a systems programming language.".to_string())
);
```

### JSONAdapter Usage

Best for models with native JSON support (GPT-4, Claude):

```rust
use ggen_dspy::adapters::{JSONAdapter, LlmAdapter};

// Create adapter
let adapter = JSONAdapter::new();

// Prepare inputs
let mut inputs = HashMap::new();
inputs.insert("text".to_string(), Value::String("Great product!".to_string()));

// Define output fields
let output_fields = vec!["sentiment".to_string(), "confidence".to_string()];

// Format prompt (includes JSON schema)
let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;

// Parse JSON response (handles markdown code blocks)
let llm_response = r#"```json
{
    "sentiment": "positive",
    "confidence": 0.95
}
```"#;

let parsed = adapter.parse_response(llm_response, &output_fields)?;

assert_eq!(parsed.get("sentiment").unwrap(), &Value::String("positive".to_string()));
```

### AdapterWithFallback

Automatically selects best adapter for model:

```rust
use ggen_dspy::adapters::AdapterWithFallback;

// For GPT-4 (supports JSON) → uses JSONAdapter
let adapter = AdapterWithFallback::new("gpt-4");
assert_eq!(adapter.name(), "JSONAdapter");

// For GPT-3.5 (less reliable JSON) → uses ChatAdapter
let adapter = AdapterWithFallback::new("gpt-3.5-turbo");
assert_eq!(adapter.name(), "ChatAdapter");

// Use it like any adapter
let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;
let parsed = adapter.parse_response(response, &output_fields)?;
```

### Custom Schema

```rust
use serde_json::json;

// Define custom JSON schema
let schema = json!({
    "type": "object",
    "properties": {
        "answer": {
            "type": "string",
            "description": "The detailed answer"
        },
        "sources": {
            "type": "array",
            "items": {"type": "string"},
            "description": "List of sources"
        }
    },
    "required": ["answer", "sources"]
});

// Use with JSONAdapter
let prompt = adapter.format_prompt(&inputs, &output_fields, Some(&schema), None)?;
```

### Few-Shot Demonstrations

```rust
use ggen_dspy::adapters::Demonstration;

// Create demonstrations (examples)
let mut demo1_in = HashMap::new();
demo1_in.insert("number".to_string(), Value::String("4".to_string()));
let mut demo1_out = HashMap::new();
demo1_out.insert("squared".to_string(), Value::String("16".to_string()));

let mut demo2_in = HashMap::new();
demo2_in.insert("number".to_string(), Value::String("7".to_string()));
let mut demo2_out = HashMap::new();
demo2_out.insert("squared".to_string(), Value::String("49".to_string()));

let demos = vec![
    Demonstration::new(demo1_in, demo1_out),
    Demonstration::new(demo2_in, demo2_out),
];

// Use with adapter
let prompt = adapter.format_prompt(
    &inputs,
    &output_fields,
    None,
    Some(&demos)  // Include demonstrations
)?;

// Prompt will include example input/output pairs
```

### IntegratedAdapter with Features

Complete adapter with retry, caching, and token tracking:

```rust
use ggen_dspy::adapters::{IntegratedAdapter, RetryConfig};
use std::time::Duration;
use std::sync::Arc;

// Create base client and adapter
let client = Arc::new(GenAiClient::new(config)?);
let adapter = Box::new(ChatAdapter::new());

// Create integrated adapter
let integrated = IntegratedAdapter::new(client, adapter)
    .with_cache(
        Duration::from_secs(3600),  // 1 hour TTL
        1000                         // Max 1000 entries
    )
    .with_retry_config(RetryConfig {
        max_retries: 3,
        initial_backoff: Duration::from_millis(100),
        max_backoff: Duration::from_secs(10),
        backoff_multiplier: 2.0,
    });

// Use it
let result = integrated.complete_with_retry(
    &inputs,
    &output_fields,
    None,
    None
).await?;

// Get token statistics
let stats = integrated.get_token_stats();
println!("Total tokens used: {}", stats.total_tokens);
```

---

## Error Handling

### Error Types

The `ggen-dspy` crate uses `Result<T, DspyError>` throughout:

```rust
use ggen_dspy::DspyError;

pub enum DspyError {
    /// LLM client error (from ggen-ai)
    LlmError(ggen_ai::GgenAiError),

    /// Module execution error
    ModuleError(String),

    /// Parsing error
    ParsingError(String),

    /// Missing field error
    FieldError { field: String },

    /// Missing input error
    MissingInput(String),

    /// Configuration error
    ConfigError(String),

    /// Timeout error
    Timeout(u64),

    /// Serialization error
    SerializationError(serde_json::Error),

    /// I/O error
    IoError(std::io::Error),

    // ... more variants
}
```

### Error Handling Patterns

#### Basic Pattern

```rust
use ggen_dspy::Result;

async fn process_question(question: &str) -> Result<String> {
    let signature = Signature::new("QA", "QA")
        .with_input(InputField::new("question", "Question", "String"))
        .with_output(OutputField::new("answer", "Answer", "String"));

    let predictor = Predictor::new(signature);

    let inputs = vec![("question", question)];
    let output = predictor.forward(&inputs).await?;

    output.get("answer").map(|s| s.to_string())
}

// Usage
match process_question("What is Rust?").await {
    Ok(answer) => println!("Answer: {}", answer),
    Err(e) => eprintln!("Error: {}", e),
}
```

#### Error Context Pattern

```rust
use anyhow::Context;

async fn process_with_context(question: &str) -> anyhow::Result<String> {
    let output = predictor.forward(&inputs).await
        .context("Failed to call LLM")?;

    let answer = output.get("answer")
        .context("Failed to extract answer field")?;

    Ok(answer.to_string())
}
```

#### Error Recovery Pattern

```rust
async fn process_with_fallback(question: &str) -> Result<String> {
    // Try primary model
    match try_gpt4(question).await {
        Ok(answer) => return Ok(answer),
        Err(e) => {
            warn!("GPT-4 failed: {}, trying fallback", e);
        }
    }

    // Fallback to GPT-3.5
    match try_gpt35(question).await {
        Ok(answer) => return Ok(answer),
        Err(e) => {
            warn!("GPT-3.5 failed: {}, trying local model", e);
        }
    }

    // Final fallback to local model
    try_ollama(question).await
}
```

#### Specific Error Handling

```rust
use ggen_dspy::DspyError;

async fn handle_specific_errors(question: &str) -> Result<String> {
    match predictor.forward(&inputs).await {
        Ok(output) => output.get("answer").map(|s| s.to_string()),

        Err(DspyError::MissingInput(field)) => {
            Err(DspyError::module(format!("Required input '{}' not provided", field)))
        },

        Err(DspyError::FieldError { field }) => {
            warn!("Could not extract field '{}', using default", field);
            Ok("Default response".to_string())
        },

        Err(DspyError::Timeout(ms)) => {
            Err(DspyError::module(format!("Request timed out after {}ms", ms)))
        },

        Err(e) => Err(e),
    }
}
```

### Custom Error Types

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AppError {
    #[error("DSPy error: {0}")]
    Dspy(#[from] DspyError),

    #[error("Database error: {0}")]
    Database(String),

    #[error("Validation error: {0}")]
    Validation(String),
}

pub type AppResult<T> = std::result::Result<T, AppError>;

async fn app_logic() -> AppResult<String> {
    // Automatic conversion from DspyError to AppError
    let output = predictor.forward(&inputs).await?;

    // Custom validation
    let answer = output.get("answer")?;
    if answer.is_empty() {
        return Err(AppError::Validation("Empty answer".to_string()));
    }

    Ok(answer.to_string())
}
```

---

## Async/Await Best Practices

### Concurrent Requests

```rust
use futures::future::join_all;
use tokio::spawn;

// Parallel processing of multiple questions
async fn process_questions_parallel(
    predictor: &Predictor,
    questions: Vec<&str>
) -> Vec<Result<String>> {
    let futures = questions.iter().map(|question| {
        let predictor = predictor.clone();  // Cheap clone
        async move {
            let inputs = vec![("question", *question)];
            let output = predictor.forward(&inputs).await?;
            output.get("answer").map(|s| s.to_string())
        }
    });

    join_all(futures).await
}
```

### Task Spawning

```rust
use tokio::task::JoinHandle;

// Spawn independent tasks
async fn process_with_spawns(questions: Vec<String>) -> Vec<Result<String>> {
    let handles: Vec<JoinHandle<Result<String>>> = questions
        .into_iter()
        .map(|question| {
            spawn(async move {
                let predictor = create_predictor();
                let inputs = vec![("question", &question)];
                let output = predictor.forward(&inputs).await?;
                output.get("answer").map(|s| s.to_string())
            })
        })
        .collect();

    let results = join_all(handles).await;

    results
        .into_iter()
        .map(|r| r.unwrap())  // Unwrap JoinHandle result
        .collect()
}
```

### Rate Limiting

```rust
use tokio::time::{interval, Duration};

async fn process_with_rate_limit(
    predictor: &Predictor,
    questions: Vec<&str>,
    requests_per_second: u32
) -> Vec<Result<String>> {
    let delay = Duration::from_secs(1) / requests_per_second;
    let mut interval = interval(delay);
    let mut results = Vec::new();

    for question in questions {
        interval.tick().await;

        let inputs = vec![("question", question)];
        let result = predictor.forward(&inputs).await;
        results.push(result.map(|o| o.get("answer").unwrap().to_string()));
    }

    results
}
```

### Timeout Handling

```rust
use tokio::time::{timeout, Duration};

async fn process_with_timeout(
    predictor: &Predictor,
    question: &str,
    timeout_duration: Duration
) -> Result<String> {
    let inputs = vec![("question", question)];

    match timeout(timeout_duration, predictor.forward(&inputs)).await {
        Ok(Ok(output)) => output.get("answer").map(|s| s.to_string()),
        Ok(Err(e)) => Err(e),
        Err(_) => Err(DspyError::Timeout(timeout_duration.as_millis() as u64)),
    }
}
```

### Select Pattern (First to Complete)

```rust
use tokio::select;

async fn process_with_multiple_models(question: &str) -> Result<String> {
    let gpt4_fut = try_gpt4(question);
    let claude_fut = try_claude(question);
    let ollama_fut = try_ollama(question);

    select! {
        result = gpt4_fut => result,
        result = claude_fut => result,
        result = ollama_fut => result,
    }
}
```

### Buffered Streams

```rust
use futures::stream::{self, StreamExt};

async fn process_with_buffer(questions: Vec<&str>, buffer_size: usize) -> Vec<Result<String>> {
    stream::iter(questions)
        .map(|question| async move {
            let predictor = create_predictor();
            let inputs = vec![("question", question)];
            let output = predictor.forward(&inputs).await?;
            output.get("answer").map(|s| s.to_string())
        })
        .buffer_unordered(buffer_size)
        .collect()
        .await
}
```

---

## Retry and Timeout Configuration

### Retry Configuration

```rust
use ggen_dspy::adapters::RetryConfig;
use std::time::Duration;

// Default configuration
let retry_config = RetryConfig::default();
// max_retries: 3
// initial_backoff: 100ms
// max_backoff: 10s
// backoff_multiplier: 2.0

// Custom configuration
let retry_config = RetryConfig {
    max_retries: 5,
    initial_backoff: Duration::from_millis(200),
    max_backoff: Duration::from_secs(30),
    backoff_multiplier: 2.5,
};

// Calculate backoff for attempt
let backoff = retry_config.backoff_duration(0);  // 200ms
let backoff = retry_config.backoff_duration(1);  // 500ms
let backoff = retry_config.backoff_duration(2);  // 1250ms
let backoff = retry_config.backoff_duration(3);  // 3125ms
```

### Exponential Backoff

```rust
async fn retry_with_backoff<F, Fut, T>(
    mut operation: F,
    config: RetryConfig
) -> Result<T>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<T>>,
{
    let mut attempt = 0;
    let mut last_error = None;

    while attempt < config.max_retries {
        match operation().await {
            Ok(value) => return Ok(value),
            Err(e) => {
                warn!("Attempt {} failed: {}", attempt + 1, e);
                last_error = Some(e);
                attempt += 1;

                if attempt < config.max_retries {
                    let delay = config.backoff_duration(attempt);
                    info!("Retrying after {:?}", delay);
                    tokio::time::sleep(delay).await;
                }
            }
        }
    }

    Err(last_error.unwrap())
}
```

### Timeout Configuration

```rust
use ggen_dspy::{DspySettings, configure_dspy};

// Configure global timeout
let settings = DspySettings::new()
    .with_timeout(120);  // 120 seconds

configure_dspy(settings)?;

// Or per-request timeout
use tokio::time::{timeout, Duration};

let result = timeout(
    Duration::from_secs(30),
    predictor.forward(&inputs)
).await??;
```

### Retry with Conditions

```rust
async fn retry_with_conditions<T>(
    operation: impl Fn() -> Pin<Box<dyn Future<Output = Result<T>>>>,
    should_retry: impl Fn(&DspyError) -> bool,
    config: RetryConfig
) -> Result<T> {
    let mut attempt = 0;

    loop {
        match operation().await {
            Ok(value) => return Ok(value),
            Err(e) => {
                if !should_retry(&e) || attempt >= config.max_retries {
                    return Err(e);
                }

                attempt += 1;
                let delay = config.backoff_duration(attempt);
                tokio::time::sleep(delay).await;
            }
        }
    }
}

// Usage
let result = retry_with_conditions(
    || Box::pin(predictor.forward(&inputs)),
    |e| matches!(e, DspyError::LlmError(_) | DspyError::Timeout(_)),
    RetryConfig::default()
).await?;
```

### Circuit Breaker Pattern

```rust
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};

struct CircuitBreaker {
    failure_count: Arc<AtomicU32>,
    threshold: u32,
    reset_timeout: Duration,
}

impl CircuitBreaker {
    fn new(threshold: u32, reset_timeout: Duration) -> Self {
        Self {
            failure_count: Arc::new(AtomicU32::new(0)),
            threshold,
            reset_timeout,
        }
    }

    async fn call<F, T>(&self, operation: F) -> Result<T>
    where
        F: Future<Output = Result<T>>,
    {
        let failures = self.failure_count.load(Ordering::Relaxed);

        if failures >= self.threshold {
            return Err(DspyError::module("Circuit breaker open"));
        }

        match operation.await {
            Ok(value) => {
                self.failure_count.store(0, Ordering::Relaxed);
                Ok(value)
            },
            Err(e) => {
                self.failure_count.fetch_add(1, Ordering::Relaxed);
                Err(e)
            }
        }
    }
}
```

---

## Token Counting and Cost Tracking

### Basic Token Tracking

```rust
use ggen_dspy::adapters::{TokenCounter, TokenStats};

// Create token counter
let counter = TokenCounter::new();

// Add usage
counter.add_usage(
    100,                      // prompt tokens
    50,                       // completion tokens
    "gpt-4o".to_string()     // model
);

counter.add_usage(200, 75, "gpt-4o".to_string());

// Get statistics
let stats = counter.get_stats();
println!("Total prompt tokens: {}", stats.prompt_tokens);
println!("Total completion tokens: {}", stats.completion_tokens);
println!("Total tokens: {}", stats.total_tokens);

// Per-model statistics
for (model, usage) in stats.model_usage {
    println!("Model: {}", model);
    println!("  Requests: {}", usage.request_count);
    println!("  Prompt tokens: {}", usage.prompt_tokens);
    println!("  Completion tokens: {}", usage.completion_tokens);
}
```

### Integrated Token Tracking

```rust
use ggen_dspy::adapters::GgenAiAdapter;

// Token tracking is automatic in GgenAiAdapter
let adapter = GgenAiAdapter::new(client);

// Make some requests
let result1 = adapter.complete(&inputs, &output_fields).await?;
let result2 = adapter.complete(&inputs2, &output_fields).await?;

// Get accumulated statistics
let stats = adapter.get_token_stats();
println!("Total API cost: ${:.4}", calculate_cost(&stats));

fn calculate_cost(stats: &TokenStats) -> f64 {
    let mut total_cost = 0.0;

    for (model, usage) in &stats.model_usage {
        let cost = match model.as_str() {
            "gpt-4o" => {
                (usage.prompt_tokens as f64 * 0.00003) +
                (usage.completion_tokens as f64 * 0.00006)
            },
            "gpt-4-turbo" => {
                (usage.prompt_tokens as f64 * 0.00001) +
                (usage.completion_tokens as f64 * 0.00003)
            },
            "claude-sonnet-4-5-20250929" => {
                (usage.prompt_tokens as f64 * 0.000003) +
                (usage.completion_tokens as f64 * 0.000015)
            },
            _ => 0.0,
        };

        total_cost += cost;
    }

    total_cost
}
```

### Cost Budget Enforcement

```rust
struct BudgetEnforcer {
    budget_usd: f64,
    spent_usd: Arc<Mutex<f64>>,
}

impl BudgetEnforcer {
    fn new(budget_usd: f64) -> Self {
        Self {
            budget_usd,
            spent_usd: Arc::new(Mutex::new(0.0)),
        }
    }

    async fn check_and_call<F, T>(&self, cost: f64, operation: F) -> Result<T>
    where
        F: Future<Output = Result<T>>,
    {
        let mut spent = self.spent_usd.lock().unwrap();

        if *spent + cost > self.budget_usd {
            return Err(DspyError::module(format!(
                "Budget exceeded: ${:.4} + ${:.4} > ${:.4}",
                *spent, cost, self.budget_usd
            )));
        }

        drop(spent);

        let result = operation.await?;

        let mut spent = self.spent_usd.lock().unwrap();
        *spent += cost;

        info!("Spent: ${:.4} / ${:.4}", *spent, self.budget_usd);

        Ok(result)
    }
}
```

### Per-Request Token Monitoring

```rust
async fn monitor_tokens(
    predictor: &Predictor,
    inputs: &[(&str, &str)]
) -> Result<(String, u32, u32)> {
    // Create monitoring wrapper
    let output = predictor.forward(inputs).await?;

    // Extract usage from response (if available)
    let prompt_tokens = 0;  // From LLM response metadata
    let completion_tokens = 0;  // From LLM response metadata

    let answer = output.get("answer")?.to_string();

    Ok((answer, prompt_tokens, completion_tokens))
}
```

---

## Caching Strategies

### Memory Cache

```rust
use ggen_dspy::adapters::GgenAiAdapter;
use std::time::Duration;

// Enable caching with TTL and max entries
let adapter = GgenAiAdapter::new(client)
    .with_cache(
        Duration::from_secs(3600),  // 1 hour TTL
        1000                         // Max 1000 entries
    );

// First call - hits LLM
let result1 = adapter.complete(&inputs, &output_fields).await?;

// Second call with same inputs - cache hit, no LLM call
let result2 = adapter.complete(&inputs, &output_fields).await?;

assert_eq!(result1, result2);
```

### Custom Cache Key

```rust
use moka::future::Cache;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

fn create_cache_key(
    inputs: &HashMap<String, Value>,
    output_fields: &[String]
) -> String {
    let mut hasher = DefaultHasher::new();

    // Hash inputs
    let mut sorted_inputs: Vec<_> = inputs.iter().collect();
    sorted_inputs.sort_by_key(|(k, _)| k.as_str());
    for (k, v) in sorted_inputs {
        k.hash(&mut hasher);
        v.to_string().hash(&mut hasher);
    }

    // Hash output fields
    output_fields.hash(&mut hasher);

    format!("{:x}", hasher.finish())
}
```

### Persistent Cache

```rust
use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(Serialize, Deserialize)]
struct CacheEntry {
    prompt: String,
    response: String,
    timestamp: u64,
}

async fn load_from_disk_cache(prompt: &str, cache_dir: &Path) -> Option<String> {
    let hash = create_cache_key_from_prompt(prompt);
    let cache_file = cache_dir.join(format!("{}.json", hash));

    if let Ok(content) = tokio::fs::read_to_string(cache_file).await {
        if let Ok(entry) = serde_json::from_str::<CacheEntry>(&content) {
            // Check if cache is still valid (e.g., < 24 hours old)
            let age = current_timestamp() - entry.timestamp;
            if age < 86400 {
                return Some(entry.response);
            }
        }
    }

    None
}

async fn save_to_disk_cache(prompt: &str, response: &str, cache_dir: &Path) -> Result<()> {
    let hash = create_cache_key_from_prompt(prompt);
    let cache_file = cache_dir.join(format!("{}.json", hash));

    let entry = CacheEntry {
        prompt: prompt.to_string(),
        response: response.to_string(),
        timestamp: current_timestamp(),
    };

    let content = serde_json::to_string_pretty(&entry)?;
    tokio::fs::write(cache_file, content).await?;

    Ok(())
}
```

### Cache Invalidation

```rust
use moka::future::Cache;

struct CacheManager {
    cache: Cache<String, String>,
}

impl CacheManager {
    fn new(ttl: Duration, max_entries: u64) -> Self {
        let cache = Cache::builder()
            .time_to_live(ttl)
            .max_capacity(max_entries)
            .build();

        Self { cache }
    }

    async fn get(&self, key: &str) -> Option<String> {
        self.cache.get(key).await
    }

    async fn set(&self, key: String, value: String) {
        self.cache.insert(key, value).await;
    }

    async fn invalidate(&self, key: &str) {
        self.cache.invalidate(key).await;
    }

    async fn invalidate_all(&self) {
        self.cache.invalidate_all();
    }

    async fn clear_expired(&self) {
        self.cache.run_pending_tasks().await;
    }
}
```

---

## Provider-Specific Guidance

### OpenAI (GPT-4, GPT-3.5)

```rust
// Best practices for OpenAI
let config = LlmConfig {
    model: "gpt-4o".to_string(),
    max_tokens: Some(4096),
    temperature: Some(0.7),
    top_p: Some(0.9),
    stop: None,
    extra: {
        let mut extra = HashMap::new();
        extra.insert("frequency_penalty".to_string(), json!(0.0));
        extra.insert("presence_penalty".to_string(), json!(0.0));
        extra
    },
};

// Use JSONAdapter for structured output
let adapter = JSONAdapter::new();

// Set OPENAI_API_KEY environment variable
std::env::set_var("OPENAI_API_KEY", "sk-...");
```

**Tips**:
- GPT-4o supports JSON mode natively
- Use `frequency_penalty` to reduce repetition
- `temperature=0` for deterministic outputs
- Max context: 128K tokens for GPT-4o

### Anthropic Claude

```rust
// Best practices for Claude
let config = LlmConfig {
    model: "claude-sonnet-4-5-20250929".to_string(),
    max_tokens: Some(4096),
    temperature: Some(1.0),  // Claude uses 0-1 range
    top_p: Some(0.999),
    stop: None,
    extra: HashMap::new(),
};

// Claude works well with both adapters
let adapter = AdapterWithFallback::new("claude-sonnet-4-5-20250929");

// Set ANTHROPIC_API_KEY environment variable
std::env::set_var("ANTHROPIC_API_KEY", "sk-ant-...");
```

**Tips**:
- Claude excels at long context (200K tokens)
- Temperature range is 0-1 (not 0-2)
- Very good at following structured output formats
- Lower latency than GPT-4 for most tasks

### Ollama (Local Models)

```rust
// Best practices for Ollama
let config = LlmConfig {
    model: "llama3:70b".to_string(),
    max_tokens: Some(2048),
    temperature: Some(0.7),
    top_p: Some(0.9),
    stop: None,
    extra: HashMap::new(),
};

// Use ChatAdapter for better compatibility
let adapter = ChatAdapter::new();

// Set OLLAMA_BASE_URL if not default
std::env::set_var("OLLAMA_BASE_URL", "http://localhost:11434");
```

**Tips**:
- Free and private (runs locally)
- Lower quality than GPT-4/Claude
- Good for development and testing
- Adjust `num_ctx` for context length
- Use smaller models for faster responses

### Provider Comparison Table

| Provider | Best For | Context Length | JSON Support | Cost (per 1M tokens) |
|----------|----------|----------------|--------------|---------------------|
| OpenAI GPT-4o | Reasoning, coding | 128K | Excellent | $15 (output) |
| OpenAI GPT-4-turbo | Fast, cost-effective | 128K | Excellent | $30 (output) |
| Anthropic Claude | Long context, writing | 200K | Very Good | $15 (output) |
| Ollama | Local, private | Varies | Fair | Free |

---

## Testing with DummyLM

### Overview

`DummyLM` provides deterministic LLM responses for testing without API calls. It supports three operating modes:

1. **Sequential**: Returns responses in order
2. **QueryBased**: Matches prompt content to responses
3. **ExampleFollowing**: Extracts from demonstration history

### Sequential Mode

```rust
use ggen_ai::dspy::testing::DummyLM;
use serde_json::json;
use std::collections::HashMap;

#[tokio::test]
async fn test_sequential_responses() {
    // Define responses
    let responses = vec![
        HashMap::from([("answer".to_string(), json!("Paris"))]),
        HashMap::from([("answer".to_string(), json!("London"))]),
        HashMap::from([("answer".to_string(), json!("Berlin"))]),
    ];

    // Create DummyLM
    let dummy = DummyLM::sequential(responses);

    // First call
    let inputs = HashMap::from([("prompt".to_string(), json!("Capital of France?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("Paris"));

    // Second call
    let inputs = HashMap::from([("prompt".to_string(), json!("Capital of UK?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("London"));

    // Third call
    let inputs = HashMap::from([("prompt".to_string(), json!("Capital of Germany?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("Berlin"));

    // Fourth call - cycles back to first
    let inputs = HashMap::from([("prompt".to_string(), json!("Another question?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("Paris"));
}
```

### QueryBased Mode

```rust
#[tokio::test]
async fn test_query_based_responses() {
    // Map prompts to responses
    let mut query_map = HashMap::new();
    query_map.insert(
        "France".to_string(),
        HashMap::from([("answer".to_string(), json!("Paris"))]),
    );
    query_map.insert(
        "UK".to_string(),
        HashMap::from([("answer".to_string(), json!("London"))]),
    );
    query_map.insert(
        "Germany".to_string(),
        HashMap::from([("answer".to_string(), json!("Berlin"))]),
    );

    let dummy = DummyLM::query_based(query_map);

    // Match "France" in prompt
    let inputs = HashMap::from([("prompt".to_string(), json!("What is the capital of France?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("Paris"));

    // Match "UK" in prompt
    let inputs = HashMap::from([("prompt".to_string(), json!("UK capital city"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("London"));

    // No match - returns default
    let inputs = HashMap::from([("prompt".to_string(), json!("Capital of Spain?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("response").unwrap(), &json!("default response"));
}
```

### ExampleFollowing Mode

```rust
#[tokio::test]
async fn test_example_following() {
    // Create demonstrations (input/output pairs)
    let demonstrations = vec![
        (
            HashMap::from([("question".to_string(), json!("What is 2+2?"))]),
            HashMap::from([("answer".to_string(), json!("4"))]),
        ),
        (
            HashMap::from([("question".to_string(), json!("What is 5+5?"))]),
            HashMap::from([("answer".to_string(), json!("10"))]),
        ),
    ];

    let dummy = DummyLM::example_following(demonstrations);

    // Match first example
    let inputs = HashMap::from([("prompt".to_string(), json!("question: What is 2+2?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("4"));

    // Match second example
    let inputs = HashMap::from([("prompt".to_string(), json!("question: What is 5+5?"))]);
    let result = dummy.forward(inputs).await.unwrap();
    assert_eq!(result.get("answer").unwrap(), &json!("10"));
}
```

### Call History Tracking

```rust
#[tokio::test]
async fn test_call_history() {
    let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
    let dummy = DummyLM::sequential(responses);

    // Make some calls
    for i in 1..=3 {
        let inputs = HashMap::from([("prompt".to_string(), json!(format!("Question {}", i)))]);
        dummy.forward(inputs).await.unwrap();
    }

    // Check call count
    assert_eq!(dummy.call_count(), 3);

    // Get history
    let history = dummy.history();
    assert_eq!(history.len(), 3);
    assert!(history[0].prompt.contains("Question 1"));
    assert!(history[1].prompt.contains("Question 2"));
    assert!(history[2].prompt.contains("Question 3"));

    // Get last N calls
    let last_2 = dummy.last_n_calls(2);
    assert_eq!(last_2.len(), 2);
    assert!(last_2[0].prompt.contains("Question 3"));  // Most recent first
    assert!(last_2[1].prompt.contains("Question 2"));

    // Reset
    dummy.reset();
    assert_eq!(dummy.call_count(), 0);
    assert_eq!(dummy.history().len(), 0);
}
```

### Integration with Modules

```rust
use ggen_dspy::{Predictor, testing::MockModule, ModuleOutput};

#[tokio::test]
async fn test_predictor_with_dummy_lm() {
    // Create mock predictor that returns fixed output
    let mut output = ModuleOutput::new();
    output.set("answer", "Rust is a systems programming language");

    let mock_predictor = MockModule::new("TestPredictor")
        .with_output(output);

    // Use in tests
    let inputs = vec![("question", "What is Rust?")];
    let result = mock_predictor.forward(&inputs).await.unwrap();

    assert_eq!(
        result.get("answer").unwrap(),
        "Rust is a systems programming language"
    );
}
```

### Property-Based Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_dummy_lm_always_responds(
        questions in prop::collection::vec(any::<String>(), 1..100)
    ) {
        tokio::runtime::Runtime::new().unwrap().block_on(async {
            let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
            let dummy = DummyLM::sequential(responses);

            for question in questions {
                let inputs = HashMap::from([("prompt".to_string(), json!(question))]);
                let result = dummy.forward(inputs).await;
                assert!(result.is_ok());
            }
        });
    }
}
```

---

## Common Pitfalls

### 1. Unwrap/Expect in Production

**❌ WRONG**:
```rust
// NEVER use unwrap/expect in production code
let answer = output.get("answer").unwrap();
let config = LlmConfig::default();
let client = GenAiClient::new(config).expect("Failed to create client");
```

**✅ CORRECT**:
```rust
// Always return Result
let answer = output.get("answer")?;
let config = LlmConfig::default();
let client = GenAiClient::new(config)?;
```

### 2. Missing Input Validation

**❌ WRONG**:
```rust
// No validation of inputs
async fn process(question: &str) -> Result<String> {
    let inputs = vec![("question", question)];
    let output = predictor.forward(&inputs).await?;
    output.get("answer").map(|s| s.to_string())
}
```

**✅ CORRECT**:
```rust
// Validate inputs before processing
async fn process(question: &str) -> Result<String> {
    if question.trim().is_empty() {
        return Err(DspyError::InvalidInput("Question cannot be empty".to_string()));
    }

    if question.len() > 10000 {
        return Err(DspyError::InvalidInput("Question too long".to_string()));
    }

    let inputs = vec![("question", question)];
    let output = predictor.forward(&inputs).await?;
    output.get("answer").map(|s| s.to_string())
}
```

### 3. Ignoring Model Capabilities

**❌ WRONG**:
```rust
// Using JSONAdapter with model that doesn't support JSON well
let adapter = JSONAdapter::new();
let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;
// Send to gpt-3.5-turbo → unreliable JSON parsing
```

**✅ CORRECT**:
```rust
// Use AdapterWithFallback for automatic selection
let adapter = AdapterWithFallback::new("gpt-3.5-turbo");
// Automatically uses ChatAdapter for reliability
```

### 4. No Retry Logic

**❌ WRONG**:
```rust
// Single attempt, fails on network issues
let output = predictor.forward(&inputs).await?;
```

**✅ CORRECT**:
```rust
// Use IntegratedAdapter with retry
let adapter = GgenAiAdapter::new(client)
    .with_retry_config(RetryConfig::default());

let output = adapter.complete(&inputs, &output_fields).await?;
```

### 5. Not Handling Rate Limits

**❌ WRONG**:
```rust
// Process all at once, hits rate limits
for question in questions {
    let result = process(question).await?;
}
```

**✅ CORRECT**:
```rust
// Rate limit requests
use tokio::time::{interval, Duration};

let mut interval = interval(Duration::from_millis(500));
for question in questions {
    interval.tick().await;
    let result = process(question).await?;
}
```

### 6. Inefficient Caching

**❌ WRONG**:
```rust
// No caching, repeated identical calls
for _ in 0..100 {
    let result = adapter.complete(&inputs, &output_fields).await?;
}
```

**✅ CORRECT**:
```rust
// Enable caching
let adapter = adapter.with_cache(
    Duration::from_secs(3600),
    1000
);

// First call hits LLM, rest are cached
for _ in 0..100 {
    let result = adapter.complete(&inputs, &output_fields).await?;
}
```

### 7. Missing Token Tracking

**❌ WRONG**:
```rust
// No visibility into costs
let result = predictor.forward(&inputs).await?;
```

**✅ CORRECT**:
```rust
// Track token usage
let adapter = GgenAiAdapter::new(client);
let result = adapter.complete(&inputs, &output_fields).await?;

let stats = adapter.get_token_stats();
println!("Total cost: ${:.4}", calculate_cost(&stats));
```

### 8. Blocking Async Code

**❌ WRONG**:
```rust
// Don't block async runtime
fn sync_process(question: &str) -> Result<String> {
    let output = predictor.forward(&inputs).await?;  // Won't compile
    output.get("answer").map(|s| s.to_string())
}
```

**✅ CORRECT**:
```rust
// Properly async
async fn async_process(question: &str) -> Result<String> {
    let output = predictor.forward(&inputs).await?;
    output.get("answer").map(|s| s.to_string())
}
```

### 9. Poor Error Messages

**❌ WRONG**:
```rust
// Generic error message
return Err(DspyError::module("Failed"));
```

**✅ CORRECT**:
```rust
// Descriptive error messages
return Err(DspyError::module(format!(
    "Failed to extract field '{}' from LLM response. Response: {:?}",
    field_name, response
)));
```

### 10. Not Testing with DummyLM

**❌ WRONG**:
```rust
// Tests require real API calls
#[tokio::test]
async fn test_predictor() {
    let predictor = create_real_predictor();  // Calls OpenAI
    let output = predictor.forward(&inputs).await?;
    assert!(!output.get("answer")?.is_empty());
}
```

**✅ CORRECT**:
```rust
// Use DummyLM for deterministic testing
#[tokio::test]
async fn test_predictor() {
    let responses = vec![HashMap::from([("answer".to_string(), json!("Test answer"))])];
    let dummy = DummyLM::sequential(responses);

    let output = dummy.forward(inputs).await.unwrap();
    assert_eq!(output.get("answer").unwrap(), &json!("Test answer"));
}
```

---

## Troubleshooting

### Issue: "No model specified" Error

**Symptom**:
```
Error: Module error: No model specified. Set GGEN_LLM_MODEL env var or use .with_model()
```

**Solution**:
```bash
# Set environment variable
export GGEN_LLM_MODEL="gpt-4o"
# Or export DEFAULT_MODEL="gpt-4o"
```

Or in code:
```rust
let predictor = Predictor::new(signature)
    .with_model("gpt-4o");
```

### Issue: API Authentication Failures

**Symptom**:
```
Error: LLM error: Request failed: Unauthorized
```

**Solution**:
```bash
# Set provider-specific API keys
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
export OLLAMA_BASE_URL="http://localhost:11434"
```

### Issue: Timeout Errors

**Symptom**:
```
Error: Operation timed out after 120000ms
```

**Solution**:
```rust
// Increase timeout
let settings = DspySettings::new()
    .with_timeout(300);  // 5 minutes

configure_dspy(settings)?;

// Or use per-request timeout
let result = timeout(
    Duration::from_secs(300),
    predictor.forward(&inputs)
).await??;
```

### Issue: Field Extraction Failures

**Symptom**:
```
Error: Failed to extract field: answer
```

**Solution**:
```rust
// Use more robust adapter
let adapter = AdapterWithFallback::new("gpt-4o");

// Or add fallback logic
let answer = match output.get("answer") {
    Ok(ans) => ans.to_string(),
    Err(_) => {
        warn!("Could not extract answer, using entire response");
        response.trim().to_string()
    }
};
```

### Issue: Rate Limit Errors

**Symptom**:
```
Error: LLM error: Rate limit exceeded
```

**Solution**:
```rust
// Add rate limiting
use tokio::time::{interval, Duration};

let mut rate_limiter = interval(Duration::from_millis(200));

for input in inputs {
    rate_limiter.tick().await;
    let result = predictor.forward(&input).await?;
}

// Or use retry with backoff
let adapter = adapter.with_retry_config(RetryConfig {
    max_retries: 5,
    initial_backoff: Duration::from_secs(1),
    max_backoff: Duration::from_secs(60),
    backoff_multiplier: 2.0,
});
```

### Issue: JSON Parsing Failures

**Symptom**:
```
Error: Parsing error: Invalid JSON: expected value at line 1 column 1
```

**Solution**:
```rust
// Use ChatAdapter instead of JSONAdapter
let adapter = ChatAdapter::new();

// Or use fallback adapter
let adapter = AdapterWithFallback::new(model_name);

// Or handle parsing errors gracefully
match adapter.parse_response(response, &output_fields) {
    Ok(parsed) => parsed,
    Err(e) => {
        warn!("JSON parsing failed: {}, trying alternative parsing", e);
        alternative_parse(response, &output_fields)?
    }
}
```

### Issue: Memory Leaks from Caching

**Symptom**: Memory usage grows over time

**Solution**:
```rust
// Set reasonable cache limits
let adapter = adapter.with_cache(
    Duration::from_secs(3600),  // TTL
    1000                         // Max entries (limit memory)
);

// Or clear cache periodically
cache_manager.invalidate_all().await;
```

### Issue: Slow Performance

**Symptom**: Requests take too long

**Solution**:
```rust
// 1. Use faster model
let config = LlmConfig {
    model: "gpt-4-turbo".to_string(),  // Faster than gpt-4o
    ..Default::default()
};

// 2. Reduce max_tokens
let predictor = predictor.with_max_tokens(512);

// 3. Enable caching
let adapter = adapter.with_cache(Duration::from_secs(3600), 1000);

// 4. Use parallel processing
let results = stream::iter(questions)
    .map(|q| process_question(q))
    .buffer_unordered(10)  // Process 10 at a time
    .collect()
    .await;
```

### Debugging Tips

```rust
// 1. Enable tracing
use tracing::{debug, info, warn};
use tracing_subscriber;

tracing_subscriber::fmt::init();

// 2. Log prompts and responses
debug!("Prompt: {}", prompt);
debug!("Response: {}", response);

// 3. Inspect call history (with DummyLM)
let history = dummy.history();
for call in history {
    println!("Prompt: {}", call.prompt);
    println!("Response: {}", call.response);
}

// 4. Check token usage
let stats = adapter.get_token_stats();
info!("Token usage: {:?}", stats);

// 5. Validate configuration
config.validate()?;
```

---

## Migration Guide

### From Custom LLM Clients to rust-genai

**Before (Custom Client)**:
```rust
// Old custom client approach
struct CustomLlmClient {
    api_key: String,
    base_url: String,
}

impl CustomLlmClient {
    async fn complete(&self, prompt: &str) -> Result<String> {
        // Custom HTTP request logic
        // ...
    }
}
```

**After (rust-genai)**:
```rust
// New unified approach
use ggen_ai::{GenAiClient, LlmConfig};

let config = LlmConfig {
    model: "gpt-4o".to_string(),
    max_tokens: Some(4096),
    temperature: Some(0.7),
    ..Default::default()
};

let client = GenAiClient::new(config)?;
```

### From Direct API Calls to Adapters

**Before**:
```rust
// Direct prompt construction
let prompt = format!("Question: {}\nAnswer:", question);
let response = client.complete(&prompt).await?;
let answer = response.trim();
```

**After**:
```rust
// Using adapter pattern
let adapter = GgenAiAdapter::new(client);

let mut inputs = HashMap::new();
inputs.insert("question".to_string(), Value::String(question.to_string()));

let output_fields = vec!["answer".to_string()];
let result = adapter.complete(&inputs, &output_fields).await?;

let answer = result.get("answer")?;
```

### From Manual Retry to Automatic

**Before**:
```rust
// Manual retry logic
let mut attempts = 0;
loop {
    match client.complete(&prompt).await {
        Ok(response) => break response,
        Err(e) if attempts < 3 => {
            attempts += 1;
            tokio::time::sleep(Duration::from_secs(2)).await;
        },
        Err(e) => return Err(e),
    }
}
```

**After**:
```rust
// Automatic retry with IntegratedAdapter
let adapter = GgenAiAdapter::new(client)
    .with_retry_config(RetryConfig::default());

let result = adapter.complete(&inputs, &output_fields).await?;
```

### From No Caching to Automatic Caching

**Before**:
```rust
// No caching - every call hits API
for question in questions {
    let response = client.complete(&question).await?;
}
```

**After**:
```rust
// Automatic caching
let adapter = adapter.with_cache(
    Duration::from_secs(3600),
    1000
);

// Duplicate questions are cached
for question in questions {
    let result = adapter.complete(&inputs, &output_fields).await?;
}
```

### Environment Variable Changes

**Before**:
```bash
export OPENAI_KEY="sk-..."
export MODEL="gpt-4"
```

**After**:
```bash
export OPENAI_API_KEY="sk-..."
export GGEN_LLM_MODEL="gpt-4o"
# or
export DEFAULT_MODEL="gpt-4o"
```

---

## Quick Reference

### Essential Imports

```rust
// Core DSPy
use ggen_dspy::{Predictor, Module, ModuleOutput, Result, DspyError};

// Signatures
use ggen_ai::dspy::{Signature, InputField, OutputField};

// Client and config
use ggen_ai::{GenAiClient, LlmConfig, LlmClient};

// Adapters
use ggen_dspy::adapters::{
    ChatAdapter, JSONAdapter, AdapterWithFallback,
    GgenAiAdapter, IntegratedAdapter, RetryConfig,
    Demonstration, TokenCounter, TokenStats,
};

// Testing
use ggen_ai::dspy::testing::DummyLM;
use ggen_dspy::testing::MockModule;

// Async
use tokio;
use futures::stream::{self, StreamExt};
```

### Configuration Cheat Sheet

```rust
// Basic config
let config = LlmConfig {
    model: "gpt-4o".to_string(),
    max_tokens: Some(4096),
    temperature: Some(0.7),
    top_p: Some(0.9),
    stop: None,
    extra: HashMap::new(),
};

// DSPy settings
let settings = DspySettings::new()
    .with_temperature(0.7)
    .with_max_tokens(1024)
    .with_timeout(120)
    .with_cache(true)
    .with_usage_tracking(true);

configure_dspy(settings)?;
```

### Predictor Template

```rust
// 1. Create signature
let signature = Signature::new("TaskName", "Description")
    .with_input(InputField::new("input1", "Desc", "Type"))
    .with_output(OutputField::new("output1", "Desc", "Type"));

// 2. Create predictor
let predictor = Predictor::new(signature)
    .with_temperature(0.7)
    .with_max_tokens(1024)
    .with_model("gpt-4o");

// 3. Forward inputs
let inputs = vec![("input1", "value")];
let output = predictor.forward(&inputs).await?;

// 4. Extract results
let result = output.get("output1")?;
```

### Adapter Usage Template

```rust
// Create client
let client = GenAiClient::new(config)?;

// Create adapter with features
let adapter = GgenAiAdapter::new(client)
    .with_cache(Duration::from_secs(3600), 1000)
    .with_retry_config(RetryConfig::default());

// Use adapter
let mut inputs = HashMap::new();
inputs.insert("field".to_string(), Value::String("value".to_string()));

let output_fields = vec!["result".to_string()];
let result = adapter.complete(&inputs, &output_fields).await?;
```

### Testing Template

```rust
#[tokio::test]
async fn test_with_dummy_lm() {
    // Setup
    let responses = vec![
        HashMap::from([("answer".to_string(), json!("Test answer"))]),
    ];
    let dummy = DummyLM::sequential(responses);

    // Execute
    let inputs = HashMap::from([("prompt".to_string(), json!("Test prompt"))]);
    let result = dummy.forward(inputs).await.unwrap();

    // Assert
    assert_eq!(result.get("answer").unwrap(), &json!("Test answer"));
    assert_eq!(dummy.call_count(), 1);
}
```

### Common Patterns

```rust
// Retry with backoff
let result = retry_with_backoff(
    || predictor.forward(&inputs),
    RetryConfig::default()
).await?;

// Batch processing
let results = stream::iter(inputs)
    .map(|input| predictor.forward(&input))
    .buffer_unordered(10)
    .collect()
    .await;

// Timeout
let result = timeout(
    Duration::from_secs(30),
    predictor.forward(&inputs)
).await??;

// Fallback
let result = try_primary(&inputs).await
    .or_else(|_| try_fallback(&inputs).await)?;
```

### Environment Variables

```bash
# LLM Configuration
export GGEN_LLM_MODEL="gpt-4o"
export DEFAULT_MODEL="gpt-4o"
export LLM_MAX_TOKENS="4096"
export LLM_TEMPERATURE="0.7"
export LLM_TOP_P="0.9"

# Provider API Keys
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
export OLLAMA_BASE_URL="http://localhost:11434"

# Logging
export RUST_LOG="ggen_dspy=debug,ggen_ai=debug"
```

---

## Conclusion

This guide covers the comprehensive usage of rust-genai in ggen-dspy, from basic initialization to advanced patterns like retry logic, caching, and testing. Key takeaways:

1. **Always use `Result<T, E>`** - No unwrap/expect in production
2. **Choose the right adapter** - JSONAdapter for capable models, ChatAdapter for others
3. **Enable retry and caching** - IntegratedAdapter provides both
4. **Test with DummyLM** - No API calls needed for deterministic tests
5. **Track token usage** - Monitor costs with TokenCounter
6. **Follow async best practices** - Concurrent requests, rate limiting, timeouts

For more information:
- [ggen-ai README](../crates/ggen-ai/README.md)
- [DSPy Documentation](./GGEN_DSPY_GUIDE.md)
- [rust-genai GitHub](https://github.com/jeremychone/rust-genai)

---

**End of Rust-GenAI Usage Guide**
