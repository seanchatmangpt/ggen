# Ollama-First Design for ggen-ai

## Philosophy

**Ollama is the default with zero requirements.**

ggen-ai is designed to work out-of-the-box with local Ollama installation, requiring no API keys, no environment variables, and no configuration. Cloud providers (OpenAI, Anthropic) are optional upgrades that require explicit API keys.

## Design Decisions

### 1. No Upfront Checks
- ❌ **Don't** check if Ollama is running during initialization
- ❌ **Don't** curl health endpoints to verify availability
- ❌ **Don't** warn users about missing providers
- ✅ **Do** assume Ollama and fail fast on first request if unavailable

**Rationale**: Checking availability upfront adds latency, complexity, and false positives. Let genai handle the connection and fail with a clear error when actually used.

### 2. Fail Fast, Clear Errors
When a user makes their first request without Ollama running:
```
Error: LLM provider 'Ollama' error: Connection refused (os error 111)
Hint: Install and start Ollama: `ollama serve`
```

**Rationale**: Developers understand connection errors. The error happens at use-time, not config-time, which is more intuitive.

### 3. API Keys Only for Cloud Providers
```rust
match provider {
    LlmProvider::Ollama => {
        // No API key required - fail fast on first request
        GenAiClient::new(config)?
    }
    LlmProvider::OpenAI => {
        // Require API key explicitly
        if std::env::var("OPENAI_API_KEY").is_err() {
            return Err(GgenAiError::missing_env_var("OPENAI_API_KEY"));
        }
        GenAiClient::new(config)?
    }
    // ... same for Anthropic
}
```

**Rationale**:
- Ollama = local, no secrets, zero config
- Cloud providers = require secrets, explicit opt-in
- Clear error messages guide users to set API keys when needed

### 4. Configuration Hierarchy

```
1. Explicit GGEN_LLM_PROVIDER env var (user override)
2. Default to Ollama (always)
```

That's it. No fallback chain, no auto-detection, no "smart" behavior.

**Rationale**: Simple and predictable. Users know exactly what will happen.

## User Experience

### First-time User (Ollama installed)
```bash
$ ggen generate "REST API for users"
✓ Generating... (uses Ollama automatically)
```
**Zero configuration needed.**

### First-time User (Ollama not installed)
```bash
$ ggen generate "REST API for users"
Error: Connection to Ollama failed
Hint: Install Ollama from https://ollama.ai
      Then run: ollama serve
```
**Clear error with actionable guidance.**

### Cloud Provider User
```bash
$ export OPENAI_API_KEY=sk-...
$ export GGEN_LLM_PROVIDER=openai
$ ggen generate "REST API for users"
✓ Generating... (uses OpenAI)
```
**Explicit opt-in with required credentials.**

### Missing API Key
```bash
$ export GGEN_LLM_PROVIDER=openai
$ ggen generate "REST API for users"
Error: Missing required environment variable: OPENAI_API_KEY
      Set it with: export OPENAI_API_KEY=your_key
```
**Fail fast with clear instructions.**

## Code Changes

### Before (Complex)
```rust
fn detect_available_provider() -> LlmProvider {
    // Check explicit env var
    if let Ok(provider) = std::env::var("GGEN_LLM_PROVIDER") { ... }

    // Check if Ollama is running (curl health endpoint)
    if check_ollama_available() { return Ollama; }

    // Check for API keys
    if std::env::var("OPENAI_API_KEY").is_ok() { return OpenAI; }
    if std::env::var("ANTHROPIC_API_KEY").is_ok() { return Anthropic; }

    // Fallback to Mock with warnings
    eprintln!("Warning: No provider detected...");
    LlmProvider::Mock
}
```

### After (Simple)
```rust
fn detect_available_provider() -> LlmProvider {
    // Check explicit env var
    if let Ok(provider) = std::env::var("GGEN_LLM_PROVIDER") {
        return match provider.to_lowercase().as_str() {
            "openai" => LlmProvider::OpenAI,
            "anthropic" => LlmProvider::Anthropic,
            "ollama" => LlmProvider::Ollama,
            "mock" => LlmProvider::Mock,
            _ => LlmProvider::Ollama,
        };
    }

    // Default to Ollama (no requirements)
    LlmProvider::Ollama
}
```

## Benefits

### 1. Simplicity
- 15 lines of code → 11 lines
- No curl subprocess spawning
- No health check logic
- No fallback chain

### 2. Performance
- No upfront latency checking Ollama availability
- No wasted API calls to health endpoints
- Initialization is instant

### 3. Predictability
- Users always know what provider will be used
- No "magic" auto-detection
- Clear error messages at use-time

### 4. Developer Experience
- Works out-of-the-box with Ollama
- Zero configuration for default use case
- Explicit opt-in for cloud providers

## Environment Variables

### Zero Config (Default)
```bash
# No environment variables needed
$ ggen generate "code"
# Uses Ollama automatically
```

### Cloud Provider
```bash
# Explicit provider selection + API key
$ export GGEN_LLM_PROVIDER=openai
$ export OPENAI_API_KEY=sk-...
$ ggen generate "code"
```

### Override Model
```bash
# Still uses Ollama, different model
$ export GGEN_LLM_MODEL=llama3:70b
$ ggen generate "code"
```

## Testing

### Unit Tests
```rust
#[test]
fn test_default_config() {
    let config = GlobalLlmConfig::default();
    // Ollama is always the default (no requirements)
    assert_eq!(config.provider, LlmProvider::Ollama);
}

#[test]
fn test_openai_requires_api_key() {
    std::env::remove_var("OPENAI_API_KEY");
    let config = GlobalLlmConfig::default();
    let result = config.create_provider_client(&LlmProvider::OpenAI);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("OPENAI_API_KEY"));
}

#[test]
fn test_ollama_no_api_key() {
    // Ollama should work without any environment variables
    let config = GlobalLlmConfig::default();
    let result = config.create_provider_client(&LlmProvider::Ollama);
    // Will succeed (actual connection failure happens on first request)
    assert!(result.is_ok());
}
```

## Migration Guide

### For Users
**No changes required!** If you were using ggen-ai with Ollama, it continues to work exactly the same.

If you were relying on auto-detection based on API keys:
```bash
# Before: Just had OPENAI_API_KEY set
export OPENAI_API_KEY=sk-...

# After: Explicitly select OpenAI provider
export GGEN_LLM_PROVIDER=openai
export OPENAI_API_KEY=sk-...
```

### For Developers
If your code was checking for provider availability:
```rust
// Before
if GlobalLlmConfig::check_ollama_available() {
    // Use Ollama
}

// After
// Just use it - will fail fast if unavailable
let client = config.create_client()?;
```

## Future Considerations

### Multi-Provider Fallback?
**No.** Explicit is better than implicit. Users should know which provider they're using.

### Auto-detect from API keys?
**No.** Requires environment variable scanning on every init. Explicit provider selection is clearer.

### Smart provider selection?
**No.** "Smart" behavior leads to surprising outcomes. Simple and predictable wins.

## Summary

| Aspect | Before | After |
|--------|--------|-------|
| **Default** | Check Ollama → Check API keys → Mock | Ollama (always) |
| **Upfront checks** | curl health endpoint | None |
| **API keys** | Optional for all | Required for cloud only |
| **Errors** | Init-time warnings | Use-time clear errors |
| **Lines of code** | ~30 | ~11 |
| **Latency** | Health check overhead | Zero |

**Result**: Simpler, faster, more predictable, better UX.
