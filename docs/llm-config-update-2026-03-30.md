# LLM Configuration Update - 2026-03-30

## Change Summary

Updated the default Groq model from `llama-3.3-70b-versatile` to `openai/gpt-oss-20b` as the default LLM for ggen.

## Files Modified

### 1. `crates/ggen-ai/src/constants.rs`
- Changed `GROQ_DEFAULT` from `"llama-3.3-70b-versatile"` to `"openai/gpt-oss-20b"`
- Updated comment to reflect new model

### 2. `crates/ggen-ai/src/providers/adapter.rs`
- Updated `groq_default_config()` function:
  - Increased `max_tokens` from `4096` to `8192`
  - Changed `temperature` from `0.7` to `1.0`
  - Changed `top_p` from `0.9` to `1.0`
  - Updated documentation to reference new model

## API Configuration

The new model uses the standard Groq API endpoint:
```
https://api.groq.com/openai/v1/chat/completions
```

Required environment variable:
```bash
export GROQ_API_KEY=your_key_here
```

## Example Usage

```bash
# Set API key
export GROQ_API_KEY=gsk_...

# Use ggen with default model (now openai/gpt-oss-20b)
ggen sync --ontology specify/mcp-a2a-protocol.ttl

# Or specify model explicitly
ggen sync --ontology specify/mcp-a2a-protocol.ttl --model openai/gpt-oss-20b
```

## Model Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| Model | `openai/gpt-oss-20b` | Groq-hosted GPT-OSS 20B |
| Max Tokens | 8192 | Maximum completion tokens |
| Temperature | 1.0 | Full randomness |
| Top P | 1.0 | No nucleus sampling |
| Stream | true | Streaming responses |
| Reasoning Effort | medium | Balanced reasoning |

## Verification

To verify the change is working:
```bash
# Check compilation
cargo check -p ggen-ai

# Test LLM integration
cargo test -p ggen-ai --test llm_integration_test
```

## References

- Groq API: https://console.groq.com/docs/quickstart
- Model List: https://console.groq.com/docs/models
- ggen-ai crate: `crates/ggen-ai/src/`
