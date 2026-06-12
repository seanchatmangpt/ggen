<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Configure Language Models (LLMs) in ggen](#how-to-configure-language-models-llms-in-ggen)
  - [Table of Contents](#table-of-contents)
  - [Quick Start](#quick-start)
    - [Option 1: Local AI (FREE - Recommended for Development)](#option-1-local-ai-free---recommended-for-development)
    - [Option 2: OpenAI (GPT-4o)](#option-2-openai-gpt-4o)
    - [Option 3: Anthropic (Claude)](#option-3-anthropic-claude)
  - [Supported Models](#supported-models)
    - [Free Local Models (via Ollama)](#free-local-models-via-ollama)
    - [Cloud-Based Models](#cloud-based-models)
      - [OpenAI](#openai)
      - [Anthropic (Claude)](#anthropic-claude)
      - [Other Providers (via genai crate)](#other-providers-via-genai-crate)
  - [Configuration Methods](#configuration-methods)
    - [Method 1: Environment Variables (Easiest)](#method-1-environment-variables-easiest)
    - [Method 2: .env File (Project-Specific)](#method-2-env-file-project-specific)
    - [Method 3: Configuration File (System-Wide)](#method-3-configuration-file-system-wide)
    - [Method 4: Project-Specific Config](#method-4-project-specific-config)
    - [Method 5: CLI Flags (One-Time Override)](#method-5-cli-flags-one-time-override)
  - [Provider Setup](#provider-setup)
    - [Setup: Ollama (Local, FREE)](#setup-ollama-local-free)
    - [Setup: OpenAI (GPT-4o)](#setup-openai-gpt-4o)
    - [Setup: Anthropic (Claude)](#setup-anthropic-claude)
  - [Testing Your Setup](#testing-your-setup)
    - [Test 1: Verify Provider Auto-Detection](#test-1-verify-provider-auto-detection)
    - [Test 2: Simple Generation](#test-2-simple-generation)
    - [Test 3: Check Model Availability](#test-3-check-model-availability)
    - [Test 4: Monitor Token Usage](#test-4-monitor-token-usage)
    - [Test 5: Verify Streaming (Optional)](#test-5-verify-streaming-optional)
  - [Troubleshooting](#troubleshooting)
    - [Problem: "No LLM configured"](#problem-no-llm-configured)
    - [Problem: "API key invalid"](#problem-api-key-invalid)
    - [Problem: "Model not found"](#problem-model-not-found)
    - [Problem: "Rate limit exceeded"](#problem-rate-limit-exceeded)
    - [Problem: Ollama won't start or connect](#problem-ollama-wont-start-or-connect)
    - [Problem: "Connection timeout"](#problem-connection-timeout)
  - [Cost Optimization](#cost-optimization)
    - [Free Option: Ollama Development](#free-option-ollama-development)
    - [Budget Option: Shared Cache](#budget-option-shared-cache)
    - [Production Optimization](#production-optimization)
    - [Monitoring Costs](#monitoring-costs)
  - [Advanced Configuration](#advanced-configuration)
    - [Custom Base URLs (Enterprise Proxies)](#custom-base-urls-enterprise-proxies)
    - [Multiple Providers (Fallback)](#multiple-providers-fallback)
    - [Organization IDs (OpenAI)](#organization-ids-openai)
    - [Programmatic Configuration (Rust)](#programmatic-configuration-rust)
  - [Summary](#summary)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Configure Language Models (LLMs) in ggen

This guide covers setting up ggen to work with different AI language models for generating code, ontologies, and SPARQL queries.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Supported Models](#supported-models)
3. [Configuration Methods](#configuration-methods)
4. [Provider Setup](#provider-setup)
5. [Testing Your Setup](#testing-your-setup)
6. [Troubleshooting](#troubleshooting)
7. [Cost Optimization](#cost-optimization)
8. [Advanced Configuration](#advanced-configuration)

---

## Quick Start

### Option 1: Local AI (FREE - Recommended for Development)

**Best for:** Getting started without API keys

```bash
# 1. Install Ollama (if not already installed)
# Download from https://ollama.ai

# 2. Start Ollama
ollama serve &

# 3. Pull a model (one-time)
ollama pull qwen3-coder:30b

# 4. Configure ggen (optional - auto-detected)
export GGEN_LLM_PROVIDER=ollama
export OLLAMA_MODEL=qwen3-coder:30b

# 5. Try generating
ggen ai generate "Create a REST API in Rust"
```

**Success:** You see generated code without any API costs.

---

### Option 2: OpenAI (GPT-4o)

**Best for:** Production use, highest quality

```bash
# 1. Get API key from https://platform.openai.com/api-keys
# Create API key with organization access

# 2. Configure environment
export GGEN_LLM_PROVIDER=openai
export OPENAI_API_KEY=sk-... # Your API key
export OPENAI_MODEL=gpt-4o

# 3. Try generating
ggen ai generate "Create a REST API in Rust"
```

**Success:** You see generated code and receive a token usage report.

---

### Option 3: Anthropic (Claude)

**Best for:** Safety-focused, detailed reasoning

```bash
# 1. Get API key from https://console.anthropic.com/keys
# Create new API key

# 2. Configure environment
export GGEN_LLM_PROVIDER=anthropic
export ANTHROPIC_API_KEY=sk-ant-... # Your API key
export ANTHROPIC_MODEL=claude-3-sonnet-20240229

# 3. Try generating
ggen ai generate "Create a REST API in Rust"
```

**Success:** You see generated code with Claude's detailed responses.

---

## Supported Models

### Free Local Models (via Ollama)

| Model | Best For | Memory | Speed |
|-------|----------|--------|-------|
| **qwen3-coder:30b** | Code generation (recommended) | 32GB+ | Fast |
| **qwen2.5-coder:32b** | Code generation alternative | 32GB+ | Fast |
| **codellama:34b** | General code tasks | 32GB+ | Medium |
| **llama3.2:70b** | Complex reasoning | 80GB+ | Slower |
| **mistral:7b** | Lightweight, quick | 4GB+ | Very fast |

**Installation:**
```bash
# Install Ollama
# Download from https://ollama.ai

# Pull a model (downloads ~5-30GB)
ollama pull qwen3-coder:30b

# List available models
ollama list
```

### Cloud-Based Models

#### OpenAI
- `gpt-4o` - Latest, fastest, most capable
- `gpt-4-turbo` - Previous generation, slightly cheaper
- `gpt-3.5-turbo` - Budget option, older

**Pricing (approx):**
- gpt-4o: $2.50 per 1M input tokens, $10 per 1M output tokens
- gpt-4-turbo: $10 per 1M input tokens, $30 per 1M output tokens
- gpt-3.5-turbo: $0.50 per 1M input tokens, $1.50 per 1M output tokens

#### Anthropic (Claude)
- `claude-3-5-sonnet-20241022` - Recommended, fastest
- `claude-3-opus-20240229` - Most powerful, slower
- `claude-3-haiku-20240307` - Budget option

**Pricing (approx):**
- Claude 3.5 Sonnet: $3 per 1M input, $15 per 1M output
- Claude 3 Opus: $15 per 1M input, $75 per 1M output
- Claude 3 Haiku: $0.80 per 1M input, $4 per 1M output

#### Other Providers (via genai crate)
- **Google Gemini** - https://makersuite.google.com/app/apikey
- **DeepSeek** - Fast Chinese model
- **Groq** - Ultra-fast inference
- **Cohere** - Specialized for certain tasks

---

## Configuration Methods

ggen loads configuration in this priority order:

```
1. CLI flags (highest priority)
   ↓
2. Environment variables
   ↓
3. Project config (./ggen.toml)
   ↓
4. User config (~/.config/ggen/config.toml)
   ↓
5. System config (/etc/ggen/config.toml)
   ↓
6. Default values (lowest priority)
```

### Method 1: Environment Variables (Easiest)

```bash
# Set provider and model
export GGEN_LLM_PROVIDER=openai
export GGEN_LLM_MODEL=gpt-4o

# Set API key (provider-specific)
export OPENAI_API_KEY=sk-...                    # For OpenAI
export ANTHROPIC_API_KEY=sk-ant-...            # For Anthropic
export OLLAMA_BASE_URL=http://localhost:11434  # For Ollama

# Optional: fine-tune generation parameters
export GGEN_LLM_TEMPERATURE=0.7       # 0.0-2.0, default: 0.7
export GGEN_LLM_MAX_TOKENS=8192       # default: 4096
export GGEN_LLM_TOP_P=0.9             # 0.0-1.0, default: 0.9
export GGEN_LLM_STREAMING=true        # Enable response streaming
```

**Persistence:** Add to your shell profile (`~/.bashrc`, `~/.zshrc`):

```bash
# ~/.bashrc or ~/.zshrc
export OPENAI_API_KEY="sk-..."
export GGEN_LLM_PROVIDER=openai
export GGEN_LLM_MODEL=gpt-4o
```

Then reload:
```bash
source ~/.bashrc  # or source ~/.zshrc
```

### Method 2: .env File (Project-Specific)

Create `.env` in your project root:

```bash
# .env
GGEN_LLM_PROVIDER=openai
OPENAI_API_KEY=sk-...
OPENAI_MODEL=gpt-4o
GGEN_LLM_TEMPERATURE=0.7
GGEN_LLM_MAX_TOKENS=8192
```

ggen automatically loads this file.

**Warning:** Never commit `.env` to git! Add to `.gitignore`:
```
.env
.env.local
```

### Method 3: Configuration File (System-Wide)

Create `~/.config/ggen/config.toml`:

```toml
# Global defaults for all ggen projects
[llm]
provider = "openai"
default_model = "gpt-4o"
default_temperature = 0.7
default_max_tokens = 8192
timeout_seconds = 30
use_streaming = false

[llm.providers.openai]
base_url = "https://api.openai.com/v1"
model = "gpt-4o"
# DO NOT PUT API KEY HERE!
# Use OPENAI_API_KEY environment variable instead

[llm.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"
```

Then all projects will use these defaults.

### Method 4: Project-Specific Config

Create `ggen.toml` in your project root:

```toml
[llm]
provider = "openai"
default_model = "gpt-4o"

[generation]
temperature = 0.3        # More deterministic for APIs
max_tokens = 4096
```

This overrides user-level config for this project only.

### Method 5: CLI Flags (One-Time Override)

```bash
# Override configuration for a single command
ggen ai generate \
  --model gpt-4o \
  --max-tokens 8192 \
  --temperature 0.5 \
  "Create a REST API"

# Or stream responses
ggen ai generate \
  --stream \
  "Generate TypeScript interfaces"
```

---

## Provider Setup

### Setup: Ollama (Local, FREE)

**Prerequisites:** Docker or Ollama binary installed

```bash
# 1. Download Ollama
# Visit https://ollama.ai

# 2. Install (macOS, Windows, Linux available)
# Follow installation instructions for your OS

# 3. Start Ollama (it runs as a background service)
ollama serve &

# 4. Pull a coding model (one-time, ~15-30 minutes)
ollama pull qwen3-coder:30b

# 5. Verify it's running
curl http://localhost:11434/api/generate -d '{"model":"qwen3-coder:30b","prompt":"Hello"}'

# 6. Configure ggen
export GGEN_LLM_PROVIDER=ollama
export OLLAMA_MODEL=qwen3-coder:30b

# 7. Test
ggen ai generate "Create a struct in Rust"
```

**Advantages:**
- ✅ Completely free
- ✅ No API limits or rate limiting
- ✅ Privacy - models run locally
- ✅ Works offline once models are downloaded

**Disadvantages:**
- ❌ Requires significant disk space (20-30GB)
- ❌ Slower than cloud models
- ❌ Requires local compute (GPU recommended)

**Troubleshooting Ollama:**
```bash
# Is Ollama running?
ps aux | grep ollama

# Can't connect? Try:
# 1. Make sure Ollama is running: ollama serve
# 2. Check the port: curl http://localhost:11434/api/tags
# 3. Restart Ollama if stuck

# Out of memory? Reduce model size:
ollama pull mistral:7b  # Smaller model
```

---

### Setup: OpenAI (GPT-4o)

**Prerequisites:** OpenAI account

```bash
# 1. Create account at https://platform.openai.com

# 2. Get API key
#    - Visit https://platform.openai.com/api-keys
#    - Click "Create new secret key"
#    - Copy the key (you won't see it again!)

# 3. Configure in shell or .env
export OPENAI_API_KEY=sk-...
export GGEN_LLM_PROVIDER=openai
export OPENAI_MODEL=gpt-4o

# 4. Test
ggen ai generate "Create a REST API"

# 5. Monitor costs
#    - Visit https://platform.openai.com/usage to see usage
#    - Set up usage alerts in billing settings
```

**Cost Management:**
```bash
# Check token usage (displayed after each command)
ggen ai generate "my prompt" --verbose
# Output: "Prompt tokens: 45, Completion tokens: 203, Total tokens: 248"

# Set a max budget (API will reject requests if exceeded)
# In OpenAI dashboard: Settings → Billing → Usage Limits

# Use cheaper models for development
export OPENAI_MODEL=gpt-3.5-turbo  # ~80% cheaper than gpt-4o
```

**Rate Limiting:**
If you hit rate limits, you'll see:
```
Error: 429 Too Many Requests
```

Solutions:
- Use exponential backoff (ggen does this automatically)
- Wait a few minutes before retrying
- Upgrade to higher tier account
- Use Ollama for development instead

---

### Setup: Anthropic (Claude)

**Prerequisites:** Anthropic account

```bash
# 1. Create account at https://console.anthropic.com

# 2. Get API key
#    - Visit https://console.anthropic.com/account/keys
#    - Click "Create Key"
#    - Copy the key

# 3. Configure
export ANTHROPIC_API_KEY=sk-ant-...
export GGEN_LLM_PROVIDER=anthropic
export ANTHROPIC_MODEL=claude-3-sonnet-20240229

# 4. Test
ggen ai generate "Create a REST API"
```

**Model Selection:**
- `claude-3-5-sonnet-20241022` - Recommended (fast, capable)
- `claude-3-opus-20240229` - Most powerful (slower)
- `claude-3-haiku-20240307` - Budget (fastest)

**Cost per 1M tokens** (approx):
- Sonnet: $3 input, $15 output
- Opus: $15 input, $75 output
- Haiku: $0.80 input, $4 output

---

## Testing Your Setup

### Test 1: Verify Provider Auto-Detection

```bash
# If you've set API keys, ggen auto-detects the provider
unset GGEN_LLM_PROVIDER  # Remove explicit provider setting
ggen ai list-models

# Output should show which provider was auto-detected:
# Using provider: openai (detected from OPENAI_API_KEY)
```

**Auto-detection priority:**
1. Explicit `GGEN_LLM_PROVIDER` environment variable
2. `OPENAI_API_KEY` detected → uses OpenAI
3. `ANTHROPIC_API_KEY` detected → uses Anthropic
4. Falls back to Ollama (requires local setup)

### Test 2: Simple Generation

```bash
# Test your LLM configuration
ggen ai generate "Create a Rust struct named User with name and email fields"

# Expected output: Rust code with struct definition
```

### Test 3: Check Model Availability

```bash
# List available models for current provider
ggen ai list-models

# Output should show models you have access to
```

### Test 4: Monitor Token Usage

```bash
# Run with verbose flag to see token counts
ggen ai generate --verbose "Create a simple REST API"

# Output should include:
# Prompt tokens: XXX
# Completion tokens: XXX
# Total tokens: XXX
# Estimated cost: $X.XX
```

### Test 5: Verify Streaming (Optional)

```bash
# If you enabled streaming, responses appear in real-time
export GGEN_LLM_STREAMING=true
ggen ai generate "Create a REST API"

# Output should appear line-by-line as it's generated
```

---

## Troubleshooting

### Problem: "No LLM configured"

**Error Message:**
```
Error: No language model configured. Please set OPENAI_API_KEY,
ANTHROPIC_API_KEY, or start Ollama
```

**Solutions:**

1. **Check environment variables:**
   ```bash
   echo $OPENAI_API_KEY
   echo $ANTHROPIC_API_KEY
   echo $GGEN_LLM_PROVIDER
   ```

2. **For OpenAI:**
   ```bash
   export OPENAI_API_KEY=sk-...
   # Verify it's set
   echo $OPENAI_API_KEY
   ```

3. **For Anthropic:**
   ```bash
   export ANTHROPIC_API_KEY=sk-ant-...
   # Verify it's set
   echo $ANTHROPIC_API_KEY
   ```

4. **For Ollama:**
   ```bash
   # Start Ollama if not running
   ollama serve &

   # Verify it's running
   curl http://localhost:11434/api/tags
   ```

5. **Check .env file exists:**
   ```bash
   # If using .env file
   cat .env | grep OPENAI_API_KEY

   # Make sure it's in the directory where you run ggen
   pwd
   ls -la .env
   ```

---

### Problem: "API key invalid"

**Error Message:**
```
Error: 401 Unauthorized. Invalid API key.
```

**Solutions:**

1. **Verify API key is correct:**
   ```bash
   # OpenAI key should start with sk-
   echo $OPENAI_API_KEY | head -c 10
   # Should output: sk-

   # Anthropic key should start with sk-ant-
   echo $ANTHROPIC_API_KEY | head -c 10
   # Should output: sk-ant-
   ```

2. **Check API key hasn't been revoked:**
   - OpenAI: https://platform.openai.com/api-keys (look for "Expired" status)
   - Anthropic: https://console.anthropic.com/account/keys

3. **Regenerate API key:**
   - OpenAI: Delete old key, create new one
   - Anthropic: Create new key

4. **Check no extra spaces:**
   ```bash
   # Bad: has trailing space
   export OPENAI_API_KEY="sk-... "

   # Good: no extra spaces
   export OPENAI_API_KEY="sk-..."
   ```

---

### Problem: "Model not found"

**Error Message:**
```
Error: Model 'gpt-4o' not available. Check your API access.
```

**Solutions:**

1. **Verify you have access to the model:**
   - For GPT-4o: You need paid OpenAI account (not free tier)
   - For Claude: Your API key tier must support the model
   - For Ollama: Model must be downloaded with `ollama pull`

2. **List available models:**
   ```bash
   ggen ai list-models
   ```

3. **Use a different model:**
   ```bash
   # Try a cheaper OpenAI model
   export OPENAI_MODEL=gpt-3.5-turbo

   # Or use Ollama (always available after download)
   export GGEN_LLM_PROVIDER=ollama
   ```

---

### Problem: "Rate limit exceeded"

**Error Message:**
```
Error: 429 Too Many Requests - Rate limit exceeded
```

**Solutions:**

1. **Wait before retrying:**
   ```bash
   # Wait 30 seconds, then retry
   sleep 30
   ggen ai generate "your prompt"
   ```

2. **Reduce request frequency:**
   - Don't run multiple ggen commands in parallel
   - Add delays between commands

3. **Use cheaper/local provider:**
   ```bash
   # Switch to Ollama (no rate limits)
   export GGEN_LLM_PROVIDER=ollama
   ```

4. **Upgrade your API account:**
   - OpenAI: Add credit/upgrade plan
   - Anthropic: Contact support

---

### Problem: Ollama won't start or connect

**Error Message:**
```
Error: Failed to connect to Ollama at http://localhost:11434
```

**Solutions:**

1. **Verify Ollama is installed:**
   ```bash
   which ollama
   # Should show: /usr/local/bin/ollama (or similar)
   ```

2. **Start Ollama:**
   ```bash
   ollama serve &

   # On macOS: Ollama starts automatically in menu bar
   # On Linux: Run in background or systemd service
   # On Windows: App runs in background
   ```

3. **Check Ollama is listening:**
   ```bash
   curl http://localhost:11434/api/tags
   # Should return JSON list of models
   ```

4. **Verify models are downloaded:**
   ```bash
   ollama list
   # Should show: qwen3-coder:30b (or your model)
   ```

5. **Pull a model if missing:**
   ```bash
   ollama pull qwen3-coder:30b
   ```

6. **Check port 11434 is available:**
   ```bash
   # If something else is using port 11434
   lsof -i :11434

   # Kill the process and restart Ollama
   ```

---

### Problem: "Connection timeout"

**Error Message:**
```
Error: Connection timeout. The API took too long to respond.
```

**Solutions:**

1. **Increase timeout:**
   ```bash
   export GGEN_LLM_TIMEOUT=60  # seconds
   ```

2. **Check your internet connection:**
   ```bash
   ping api.openai.com  # For OpenAI
   ping api.anthropic.com  # For Anthropic
   ```

3. **Try a different model (might be faster):**
   ```bash
   export OPENAI_MODEL=gpt-3.5-turbo  # Faster, cheaper
   ```

4. **Check API status:**
   - OpenAI: https://status.openai.com
   - Anthropic: https://www.anthropic-status.com

---

## Cost Optimization

### Free Option: Ollama Development

```bash
# Use Ollama for development/testing (completely free)
export GGEN_LLM_PROVIDER=ollama
export OLLAMA_MODEL=qwen3-coder:30b

ggen ai generate "your prompt"  # No API costs!
```

**Cost: $0**

### Budget Option: Shared Cache

Enable response caching to avoid re-generating the same prompts:

```bash
# Cache configuration (automatic - no setup needed)
# ggen caches responses for 1 hour
# Identical prompts reuse cached responses
# Saves 30-60% on API costs

# Check cache status
ggen ai cache info
```

### Production Optimization

```bash
# Use cheaper models for deterministic tasks
export GGEN_LLM_TEMPERATURE=0.3       # Less variability
export GGEN_LLM_MODEL=gpt-3.5-turbo   # Cheaper than gpt-4o

# For complex tasks, use more capable (expensive) models
export GGEN_LLM_MODEL=gpt-4o          # More capable
export GGEN_LLM_TEMPERATURE=0.7       # More creative
```

### Monitoring Costs

```bash
# OpenAI: Check usage dashboard
# https://platform.openai.com/usage

# Anthropic: Check usage and billing
# https://console.anthropic.com/account

# Add alerts in your provider's dashboard
# Stop generation if costs exceed budget
```

---

## Advanced Configuration

### Custom Base URLs (Enterprise Proxies)

If you're behind a corporate proxy or using a self-hosted API:

```bash
# Custom OpenAI endpoint
export OPENAI_BASE_URL=https://your-proxy.company.com/v1

# Custom Anthropic endpoint
export ANTHROPIC_BASE_URL=https://your-proxy.company.com/anthropic

# Custom Ollama endpoint
export OLLAMA_BASE_URL=http://your-ollama-server:11434
```

### Multiple Providers (Fallback)

Configure fallback to secondary provider if primary fails:

```toml
# In ~/.config/ggen/config.toml

[llm]
provider = "openai"
fallback_provider = "ollama"  # Fall back to Ollama if OpenAI fails

[llm.providers.openai]
model = "gpt-4o"

[llm.providers.ollama]
model = "qwen3-coder:30b"
```

### Organization IDs (OpenAI)

For OpenAI organization accounts:

```bash
export OPENAI_API_KEY=sk-...
export OPENAI_ORG_ID=org-...
```

### Programmatic Configuration (Rust)

If embedding ggen in your Rust application:

```rust
use ggen_ai::{GenAiClient, LlmConfig};

let config = LlmConfig {
    model: "gpt-4o".to_string(),
    max_tokens: Some(8192),
    temperature: Some(0.7),
    top_p: Some(0.9),
    stop: None,
    extra: Default::default(),
};

let client = GenAiClient::new(config)?;
```

---

## Summary

| Scenario | Provider | Cost | Setup Time |
|----------|----------|------|-----------|
| **Learning/Development** | Ollama | Free | 30 min (download) |
| **Production API** | OpenAI GPT-4o | Low-Medium | 5 min |
| **Safety-First** | Anthropic Claude | Medium | 5 min |
| **Budget Production** | OpenAI GPT-3.5 | Very Low | 5 min |
| **Offline/Secure** | Ollama | Free | 30 min |

**Recommended Starting Point:**
1. Start with **Ollama** locally (free, understand the system)
2. Try **OpenAI** for production (best quality/cost balance)
3. Use **Anthropic** for safety-critical code generation

---

## Next Steps

Now that you've configured an LLM, try:

1. **[AI-Powered Generation Tutorial](../tutorials/ai-powered-generation.md)** - Use AI to generate ontologies
2. **[Generate Your First Code](../tutorials/zero-to-generated-code.md)** - Create a working example
3. **[Marketplace Workflow](../tutorials/marketplace-workflow.md)** - Discover pre-built templates

---

**Need Help?**
- Check troubleshooting section above
- Review provider documentation (OpenAI, Anthropic, Ollama)
- Run `ggen ai --help` for command options
- Visit the [reference documentation](../reference/cli.md) for detailed options
