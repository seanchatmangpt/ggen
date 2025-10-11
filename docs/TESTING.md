<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Testing Guide](#testing-guide)
  - [Running Tests](#running-tests)
    - [Fast Tests (Default - No API Calls)](#fast-tests-default---no-api-calls)
    - [AI Integration Tests](#ai-integration-tests)
    - [Live LLM Integration Tests (Opt-In)](#live-llm-integration-tests-opt-in)
      - [Ollama Tests](#ollama-tests)
      - [OpenAI Tests](#openai-tests)
      - [Anthropic Tests](#anthropic-tests)
      - [All Live Tests](#all-live-tests)
  - [Feature Flags](#feature-flags)
    - [ggen-core](#ggen-core)
    - [ggen-ai](#ggen-ai)
  - [Test Environment Detection](#test-environment-detection)
  - [Examples with Live API Calls](#examples-with-live-api-calls)
  - [CI/CD Configuration](#cicd-configuration)
  - [Best Practices](#best-practices)
  - [Troubleshooting](#troubleshooting)
    - [Tests are slow](#tests-are-slow)
    - [API key errors](#api-key-errors)
    - [Ollama connection errors](#ollama-connection-errors)
  - [Performance Expectations](#performance-expectations)
  - [AI Implementation Testing](#ai-implementation-testing)
    - [Graph Reference File Generation](#graph-reference-file-generation)
    - [SPARQL Query Generation with Context](#sparql-query-generation-with-context)
    - [Iterative Template Validation](#iterative-template-validation)
    - [Enhanced Error Handling](#enhanced-error-handling)
  - [Security Considerations](#security-considerations)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Testing Guide

This guide explains how to run tests for ggen, including fast unit tests and opt-in live LLM integration tests.

## Running Tests

### Fast Tests (Default - No API Calls)
```bash
# Standard tests (recommended for development)
cargo make test

# Even faster with cargo-nextest (60% faster)
cargo nextest run --workspace
```

Runs all unit tests and integration tests with mocked LLM responses. This is the default test mode and should be fast.

### AI Integration Tests
```bash
# Test AI-powered template generation
cargo make test-ai-generate

# Test AI-powered SPARQL query generation
cargo make test-ai-sparql

# Test AI-powered RDF graph generation
cargo make test-ai-graph

# Test complete AI project generation
cargo make test-ai-project
```

### Live LLM Integration Tests (Opt-In)

#### Ollama Tests
Requires Ollama running with qwen3-coder:30b model:
```bash
# Start Ollama and pull the model
ollama pull qwen3-coder:30b

# Run Ollama integration tests
cargo make test-ollama
```

#### OpenAI Tests
Requires OPENAI_API_KEY environment variable:
```bash
export OPENAI_API_KEY=your-key
cargo make test-openai
```

#### Anthropic Tests
Requires ANTHROPIC_API_KEY environment variable:
```bash
export ANTHROPIC_API_KEY=your-key
cargo make test-anthropic
```

#### All Live Tests
```bash
cargo make test-live
```

## Feature Flags

The project uses feature flags to control which tests are compiled and run:

### ggen-core
- `live-llm-tests`: Enables OpenAI/Anthropic integration tests

### ggen-ai
- `ollama-integration`: Enables Ollama integration tests
- `openai-integration`: Enables OpenAI integration tests
- `anthropic-integration`: Enables Anthropic integration tests
- `all-integrations`: Enables all integration tests

## Test Environment Detection

The CLI commands automatically detect test environments and use mock clients unless explicitly testing live APIs:

```rust
#[cfg(all(test, not(feature = "live-llm-tests")))]
{
    // Use mock client during tests
    use ggen_ai::providers::adapter::MockClient;
    // ... mock test logic
}
```

## Examples with Live API Calls

Examples that make live API calls require explicit opt-in:

```bash
# Examples will warn and exit unless this is set
export GGEN_ALLOW_LIVE_CALLS=1
cargo run --example ggen-ai-demo
```

## CI/CD Configuration

In CI/CD pipelines, live tests should be run separately and conditionally:

```yaml
- name: Run fast tests
  run: cargo make test

- name: Run live LLM tests (optional)
  if: ${{ github.event_name == 'schedule' || github.event.inputs.run-live-tests }}
  env:
    OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}
  run: cargo make test-live
```

## Advanced Testing Practices

### Property-Based Testing with proptest

For testing complex behaviors that should hold for all possible inputs:

```bash
# Install proptest for property-based testing
cargo add proptest --dev

# Run property tests
cargo nextest run --workspace --test property_tests
```

**Example property test:**
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_template_rendering_never_panics(
        template in ".*",
        variables in any::<HashMap<String, String>>()
    ) {
        // Should never panic, even with invalid input
        let result = render_template(&template, &variables);
        assert!(result.is_ok() || result.is_err()); // Should either succeed or fail gracefully
    }
}
```

### Code Coverage Tracking

Monitor test coverage to identify untested code paths:

```bash
# Install coverage tools
cargo install cargo-tarpaulin

# Generate coverage report
cargo tarpaulin --workspace --out Html --out Lcov

# CI integration example
cargo tarpaulin --workspace --out Lcov
# Upload to codecov or similar service
```

### Mutation Testing

Verify that tests actually catch bugs:

```bash
# Install mutation testing
cargo install cargo-mutants

# Run mutation testing (takes time, run on CI)
cargo mutants --workspace
```

### Benchmark Testing

Track performance regressions:

```bash
# Install benchmarking framework
cargo install criterion

# Run benchmarks
cargo criterion

# Compare against baseline
cargo criterion --bench template_generation
```

## Best Practices

1. **Always use `cargo make` commands** - Never use direct `cargo` commands
2. **Default to fast tests** - Use `cargo make test` for normal development
3. **Opt-in for live tests** - Only run live tests when explicitly needed
4. **Use feature flags** - Control test compilation with feature flags
5. **Mock during tests** - CLI commands automatically use mocks in test mode
6. **Document requirements** - Each test type documents its prerequisites

## Troubleshooting

### Tests are slow
- Ensure you're not running live LLM tests by default
- Use `cargo make test` for fast tests
- Use `cargo nextest run` for 60% faster test execution
- Check that feature flags are not enabled by default
- Run property tests and benchmarks on CI, not locally

### API key errors
- Live tests require API keys to be set
- Use mock clients for development and testing
- Check environment variables are properly set

### Ollama connection errors
- Ensure Ollama is running: `ollama serve`
- Pull required model: `ollama pull qwen3-coder:30b`
- Check Ollama is accessible on port 11434

## Performance Expectations

- **Fast tests**: Should complete in under 30 seconds
- **cargo-nextest**: Should complete in under 15 seconds (60% faster)
- **Ollama tests**: May take 1-2 minutes depending on model size
- **OpenAI/Anthropic tests**: May take 10-30 seconds depending on API latency
- **All live tests**: May take 2-5 minutes total
- **Property tests**: May take 5-10 minutes (run on CI)
- **Mutation tests**: May take 30+ minutes (run on CI only)

## AI Implementation Testing

Recent AI module enhancements include comprehensive testing for:

### Graph Reference File Generation
- Tests verify automatic `*_reference.rs` file creation for generated RDF graphs
- Validates metadata extraction and type-safe access
- Ensures deterministic outputs and proper error handling

### SPARQL Query Generation with Context
- Tests actual graph loading instead of placeholder implementations
- Verifies SPARQL queries are generated based on real graph data
- Ensures proper error handling for missing or invalid graph files

### Iterative Template Validation
- Tests iterative validation logic with configurable thresholds
- Validates automatic improvement based on validation feedback
- Ensures proper progress reporting and iteration limits

### Enhanced Error Handling
- Tests verify no `.unwrap()` or `.expect()` calls in AI command code
- Validates proper error propagation with `Result<T>` types
- Ensures clear, actionable error messages for users

## Security Considerations

- Never commit API keys to version control
- Use environment variables for sensitive configuration
- Live tests should only run in trusted environments
- Mock clients prevent accidental API charges during development


