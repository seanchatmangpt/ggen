<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Testing Guide](#testing-guide)
  - [Running Tests](#running-tests)
    - [Fast Tests (Default - No API Calls)](#fast-tests-default---no-api-calls)
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
  - [Security Considerations](#security-considerations)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Testing Guide

This guide explains how to run tests for ggen, including fast unit tests and opt-in live LLM integration tests.

## Running Tests

### Fast Tests (Default - No API Calls)
```bash
cargo make test
```
Runs all unit tests and integration tests with mocked LLM responses. This is the default test mode and should be fast.

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
- Check that feature flags are not enabled by default

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
- **Ollama tests**: May take 1-2 minutes depending on model size
- **OpenAI/Anthropic tests**: May take 10-30 seconds depending on API latency
- **All live tests**: May take 2-5 minutes total

## Security Considerations

- Never commit API keys to version control
- Use environment variables for sensitive configuration
- Live tests should only run in trusted environments
- Mock clients prevent accidental API charges during development
