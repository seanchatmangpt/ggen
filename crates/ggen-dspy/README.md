# ggen-dspy

DSPy-inspired programming framework for LLM agents in Rust, integrated with ggen-ai.

## Overview

ggen-dspy provides a type-safe, declarative framework for building self-optimizing LLM agent pipelines. It brings DSPy concepts to Rust with zero-cost abstractions, compile-time guarantees, and idiomatic error handling.

## Core Concepts

- **Modules**: Composable building blocks (Predictor, ChainOfThought, ReAct, etc.)
- **Optimizers**: Self-improvement algorithms (BootstrapFewShot, MIPRO, etc.)
- **Evaluation**: Metrics and scoring for optimization loops
- **Assertions**: Runtime constraints and validation for LLM outputs
- **Adapters**: Integration with ggen-ai and other LLM providers

## Quick Start

```rust
use ggen_dspy::{Module, Predictor, Signature, InputField, OutputField};
use ggen_ai::{GenAiClient, LlmConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Define a signature (input/output schema)
    let signature = Signature::builder()
        .input(InputField::new("question", "A question to answer"))
        .output(OutputField::new("answer", "The answer"))
        .build()?;

    // Create a predictor module
    let llm_config = LlmConfig::default();
    let client = GenAiClient::new(llm_config)?;
    let predictor = Predictor::new(signature, client);

    // Execute the module
    let result = predictor.forward(&[("question", "What is Rust?")]).await?;
    println!("Answer: {}", result.get("answer")?);

    Ok(())
}
```

## Design Principles

Following CLAUDE.md conventions:

- **Type-First**: All constraints expressed in types, verified by compiler
- **Result<T, E>**: All fallible operations return Result, no unwrap/expect in production
- **Zero-Cost Abstractions**: Generics over trait objects for performance
- **Deterministic**: Same inputs produce same outputs (with seed control)
- **Testable**: Clear separation of pure logic and LLM interactions

## Module Organization

```
src/
├── lib.rs              # Public API and re-exports
├── error.rs            # Error types (DspyError, Result)
├── modules/            # Core module types
│   ├── predictor.rs    # Basic LLM call with signature
│   ├── chain_of_thought.rs  # CoT reasoning
│   └── react.rs        # Reasoning + Acting pattern
├── optimizers/         # Optimization algorithms
│   ├── bootstrap.rs    # BootstrapFewShot
│   └── mipro.rs        # MIPRO optimizer
├── evaluation/         # Metrics and scoring
├── assertions/         # Runtime constraints
├── config/             # Configuration types
├── adapters/           # LLM client adapters
├── patterns/           # Reusable agent patterns
└── testing/            # Test utilities
```

## Features

- **Module Composition**: Chain modules together for complex agent pipelines
- **Automatic Optimization**: BootstrapFewShot and MIPRO for self-improvement
- **Caching**: Automatic LLM response caching with TTL and size limits
- **Progress Tracking**: Built-in progress bars for optimization loops
- **Evaluation**: Comprehensive metrics for scoring agent performance
- **CSV Export**: Export evaluation results for analysis
- **Pretty Tables**: Display metrics in formatted tables

## Performance

- **Memory Budget**: Target ≤ 100MB for typical agent pipelines
- **Caching**: Reduces LLM API calls and latency
- **Concurrency**: Async/await with tokio for parallel agent execution
- **Zero-Copy**: Reference-based APIs where possible to minimize allocations

## Testing

```bash
# Run all tests
cargo make test-unit

# Run with live LLM tests (requires API keys)
cargo test --features live-llm-tests

# Run benchmarks
cargo bench
```

## Integration with ggen-ai

ggen-dspy builds on top of ggen-ai for LLM interactions:

- Supports all ggen-ai providers (OpenAI, Anthropic, Ollama, etc.)
- Uses ggen-ai's caching layer for efficiency
- Leverages ggen-ai's streaming support
- Integrates with ggen-ai's tool registry

## Examples

See the `examples/` directory for:

- Basic predictor usage
- Chain of Thought reasoning
- ReAct agents with tools
- Optimization with BootstrapFewShot
- Custom evaluation metrics
- Pattern composition

## License

MIT

## Contributing

Contributions welcome! Please follow CLAUDE.md conventions:

- All production code must use Result<T, E>
- No unwrap/expect in production code
- Type-first design with compiler verification
- Comprehensive tests with chicago-tdd-tools
- Zero warnings policy (cargo make lint)
