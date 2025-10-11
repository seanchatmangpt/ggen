# Runtime Model Configuration

## Overview

The ggen AI system now supports **runtime model configuration** through environment variables. This allows you to change the model used by the system without recompiling or modifying code.

## Environment Variables

### Primary Configuration Variables

- **`GGEN_DEFAULT_MODEL`**: The default model to use across all providers
- **`DEFAULT_MODEL`**: Fallback if `GGEN_DEFAULT_MODEL` is not set

### Provider-Specific Variables

These override the default model for specific providers:

- **`OPENAI_MODEL`**: Model for OpenAI provider (e.g., `gpt-4`, `gpt-3.5-turbo`)
- **`ANTHROPIC_MODEL`**: Model for Anthropic provider (e.g., `claude-3-5-sonnet-20240229`)
- **`OLLAMA_MODEL`**: Model for Ollama provider (e.g., `qwen3-coder:30b`, `llama3.2`)

### Additional Configuration

- **`GGEN_MAX_TOKENS`**: Maximum tokens for generation (default: 4096)
- **`GGEN_TEMPERATURE`**: Temperature for sampling (default: 0.7)
- **`GGEN_TOP_P`**: Top-p for nucleus sampling (default: 0.9)
- **`GGEN_LLM_PROVIDER`**: Default provider (`openai`, `anthropic`, `ollama`, `mock`)
- **`GGEN_LLM_MODEL`**: Model for LLM configuration
- **`GGEN_LLM_MAX_TOKENS`**: Max tokens for LLM configuration
- **`GGEN_LLM_TEMPERATURE`**: Temperature for LLM configuration
- **`GGEN_LLM_TOP_P`**: Top-p for LLM configuration

## Priority Order

The system checks environment variables in the following order:

1. **Provider-specific variable** (e.g., `OPENAI_MODEL`)
2. **`GGEN_DEFAULT_MODEL`**
3. **`DEFAULT_MODEL`**
4. **Hardcoded default** (last resort)

### Example Priority for OpenAI

```bash
# Check order:
1. OPENAI_MODEL="gpt-4"           # Highest priority
2. GGEN_DEFAULT_MODEL="claude-3"  # Second priority
3. DEFAULT_MODEL="llama3.2"       # Third priority
4. "gpt-3.5-turbo"                # Fallback default
```

## Usage Examples

### Basic Usage

Set a default model for all providers:

```bash
export GGEN_DEFAULT_MODEL="gpt-4"
cargo run --bin ggen-ai
```

### Provider-Specific Models

Use different models for different providers:

```bash
# OpenAI uses GPT-4
export OPENAI_MODEL="gpt-4"

# Anthropic uses Claude 3 Opus
export ANTHROPIC_MODEL="claude-3-opus-20240229"

# Ollama uses Qwen3 Coder
export OLLAMA_MODEL="qwen3-coder:30b"

cargo run --bin ggen-ai
```

### Complete Configuration

```bash
# Set provider and model
export GGEN_LLM_PROVIDER="openai"
export OPENAI_MODEL="gpt-4"

# Set generation parameters
export GGEN_MAX_TOKENS="8192"
export GGEN_TEMPERATURE="0.5"
export GGEN_TOP_P="0.95"

# Set API key
export OPENAI_API_KEY="sk-..."

cargo run --bin ggen-ai
```

### Using with Ollama

```bash
# Use Ollama with Qwen3 Coder
export GGEN_LLM_PROVIDER="ollama"
export OLLAMA_MODEL="qwen3-coder:30b"

# Make sure Ollama is running
ollama serve &

# Pull the model if needed
ollama pull qwen3-coder:30b

cargo run --bin ggen-ai
```

### Testing Different Models

Quick test with different models:

```bash
# Test with GPT-4
OPENAI_MODEL="gpt-4" cargo run --example genai_multi_provider_compare

# Test with Claude 3 Sonnet
ANTHROPIC_MODEL="claude-3-5-sonnet-20240229" cargo run --example genai_multi_provider_compare

# Test with local Ollama
OLLAMA_MODEL="llama3.2" cargo run --example genai_ollama_loop
```

## Configuration Files Affected

The following configuration files now use runtime environment variables:

- `ggen-ai/src/client.rs` - Main LlmConfig default
- `ggen-ai/src/config/openai.rs` - OpenAI configuration
- `ggen-ai/src/config/global.rs` - Global provider configuration
- `ggen-ai/src/config/ai.rs` - AI configuration
- `ggen-ai/src/mcp/server.rs` - MCP server configuration

## Default Values

If no environment variables are set, these defaults are used:

| Provider   | Default Model                |
|------------|------------------------------|
| OpenAI     | `qwen3-coder:30b`           |
| Anthropic  | `qwen3-coder:30b`           |
| Ollama     | `qwen3-coder:30b`           |
| Mock       | `mock-model`                |

**Note:** The default model is `qwen3-coder:30b` for all providers, which is designed to work with Ollama for local development.

## Benefits

1. **No Recompilation**: Change models without rebuilding
2. **Environment-Specific**: Use different models in dev/staging/prod
3. **Cost Control**: Use cheaper models for testing
4. **Provider Flexibility**: Easy switching between providers
5. **CI/CD Friendly**: Configure via environment in deployment pipelines

## Best Practices

### Development

```bash
# Default is already Ollama with qwen3-coder:30b
# No configuration needed!

# Or explicitly set:
export GGEN_LLM_PROVIDER="ollama"
export OLLAMA_MODEL="qwen3-coder:30b"
```

### Testing

```bash
# Use mock provider for tests
export GGEN_LLM_PROVIDER="mock"
export GGEN_TEST_MODE="true"
```

### Production

```bash
# Use production-grade models
export GGEN_LLM_PROVIDER="openai"
export OPENAI_MODEL="gpt-4"
export OPENAI_API_KEY="${PROD_OPENAI_KEY}"
```

### Cost Optimization

```bash
# Use cheaper models for non-critical tasks
export GGEN_DEFAULT_MODEL="gpt-3.5-turbo"
export GGEN_MAX_TOKENS="2048"
```

## Troubleshooting

### Model Not Found

```bash
# Error: Model 'xyz' not found
# Solution: Check if model exists and is accessible
ollama list  # For Ollama
```

### API Key Issues

```bash
# Error: Authentication failed
# Solution: Set the correct API key
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
```

### Invalid Configuration

```bash
# Error: Invalid model configuration
# Solution: Check environment variable values
env | grep GGEN
env | grep OPENAI
env | grep ANTHROPIC
env | grep OLLAMA
```

## Migration Guide

### From Hardcoded Models

**Before:**
```rust
let config = LlmConfig {
    model: "gpt-3.5-turbo".to_string(),
    // ...
};
```

**After:**
```rust
// Automatically uses environment variables
let config = LlmConfig::default();

// Or explicitly:
let config = LlmConfig {
    model: std::env::var("GGEN_DEFAULT_MODEL")
        .unwrap_or_else(|_| "gpt-3.5-turbo".to_string()),
    // ...
};
```

### From Config Files

Instead of editing config files, set environment variables:

```bash
# Instead of editing config.toml
export OPENAI_MODEL="gpt-4"
export GGEN_MAX_TOKENS="8192"
```

## Examples

See the `examples/` directory for usage examples:

- `examples/genai_ollama_loop.rs` - Interactive Ollama loop
- `examples/genai_multi_provider_compare.rs` - Multi-provider comparison
- `examples/genai_ollama_stream.rs` - Streaming with Ollama

## Further Reading

- [Ollama Documentation](https://ollama.ai/docs)
- [OpenAI Models](https://platform.openai.com/docs/models)
- [Anthropic Models](https://docs.anthropic.com/claude/docs/models-overview)
- [GenAI Rust Library](https://github.com/jeremychone/rust-genai)
