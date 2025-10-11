# Hardcoded Configuration Analysis

## Executive Summary

This document analyzes all hardcoded configuration values in the ggen codebase and provides recommendations for which values must be configurable vs. acceptable defaults.

## Critical Findings

### 🔴 MUST BE CONFIGURABLE (Production Impact)

#### 1. LLM Provider Endpoints & Credentials

**Location:** `ggen-ai/src/config/`

**Current Hardcoded Values:**
- `ollama.rs:22` - `http://localhost:11434` (Ollama base URL)
- `ollama.rs:23` - `qwen3-coder:30b` (default model)
- `ollama.rs:24` - `30` seconds (timeout)
- `openai.rs:26` - `https://api.openai.com/v1` (OpenAI base URL)
- `openai.rs:27` - `gpt-3.5-turbo` (default model)
- `anthropic.rs:24` - `https://api.anthropic.com` (Anthropic base URL)
- `anthropic.rs:25` - `claude-3-sonnet-20240229` (default model)

**Production Impact:** HIGH
- Different deployment environments need different endpoints
- Local vs cloud deployments require different configurations
- Model selection affects cost and performance
- Timeout values affect reliability in different network conditions

**Status:** ✅ ALREADY CONFIGURABLE via environment variables
- `OLLAMA_BASE_URL`, `OLLAMA_MODEL`, `OLLAMA_TIMEOUT`
- `OPENAI_API_KEY`, `OPENAI_BASE_URL`, `OPENAI_MODEL`
- `ANTHROPIC_API_KEY`, `ANTHROPIC_BASE_URL`, `ANTHROPIC_MODEL`

**Recommendation:** Add TOML configuration support for better user experience

---

#### 2. GitHub API Configuration

**Location:** `ggen-core/src/github.rs:99`

**Current Hardcoded Values:**
- `https://api.github.com` (GitHub API base URL)
- `30` seconds (request timeout)
- User agent: `"ggen-cli"`

**Production Impact:** HIGH
- GitHub Enterprise users need custom endpoints
- Rate limiting may require different timeout values
- Corporate proxies may require custom timeouts

**Status:** ⚠️ PARTIALLY CONFIGURABLE
- Token via `GITHUB_TOKEN` or `GH_TOKEN` env vars
- Base URL is hardcoded

**Recommendation:** Add environment variable support
```toml
[github]
base_url = "https://api.github.com"  # Override for GHE
timeout_seconds = 30
user_agent = "ggen-cli"
```

---

#### 3. Registry Configuration

**Location:** `ggen-core/src/registry.rs:97`

**Current Hardcoded Values:**
- `https://seanchatmangpt.github.io/ggen/registry/` (registry URL)
- `30` seconds (HTTP timeout)

**Production Impact:** HIGH
- Organizations need private registries
- Different environments need different registry sources
- Network conditions affect appropriate timeout values

**Status:** ✅ CONFIGURABLE via `GGEN_REGISTRY_URL`

**Recommendation:** Add to TOML config for discoverability
```toml
[registry]
url = "https://seanchatmangpt.github.io/ggen/registry/"
timeout_seconds = 30
mirror_urls = []  # Fallback mirrors
```

---

#### 4. File Watch & Debounce Settings

**Location:** `cli/src/cmds/project/watch.rs`

**Current Hardcoded Values:**
- `poll_interval: 1000` ms (file system polling)
- `debounce: 500` ms (default debounce)

**Production Impact:** MEDIUM
- High-frequency file systems need different intervals
- Network file systems may need longer debounce
- CI environments may need faster response

**Status:** ⚠️ PARTIALLY CONFIGURABLE
- CLI args available but no config file support

**Recommendation:** Add to TOML config
```toml
[watch]
default_poll_interval_ms = 1000
default_debounce_ms = 500
```

---

#### 5. CI/Release Timeouts

**Location:** `cli/src/cmds/ci/release.rs:622`

**Current Hardcoded Values:**
- `timeout: 1800` seconds (30 minutes for release workflows)

**Production Impact:** MEDIUM-HIGH
- Large projects need longer timeouts
- Fast CI systems can use shorter timeouts
- Network conditions affect build times

**Status:** ⚠️ PARTIALLY CONFIGURABLE
- Test values hardcoded
- CLI may have options

**Recommendation:** Add to TOML config
```toml
[ci.release]
default_timeout_seconds = 1800
workflow_timeout_seconds = 3600
homebrew_timeout_seconds = 1800
```

---

### 🟡 ACCEPTABLE DEFAULTS (Good Practices)

#### 6. LLM Generation Parameters

**Location:** `ggen-ai/src/config/ai.rs`

**Current Defaults:**
- `temperature: 0.7`
- `max_tokens: 4096`
- `top_p: 0.9`
- `streaming: false`

**Rationale:** These are industry-standard defaults that work well for most use cases

**Status:** ✅ CONFIGURABLE via `GGEN_GENERATION_*` env vars

**Recommendation:** Keep as defaults, already configurable

---

#### 7. Global LLM Settings

**Location:** `ggen-ai/src/config/global.rs`

**Current Defaults:**
- Default provider: `Ollama` (for local development)
- Default model: `llama3.2` (Ollama), `gpt-3.5-turbo` (OpenAI)
- Default timeout: `30` seconds
- Streaming: `false`

**Rationale:** Sensible defaults for local development

**Status:** ✅ CONFIGURABLE via `GGEN_LLM_*` env vars

**Recommendation:** Keep as defaults, add TOML support

---

#### 8. Performance Test Targets

**Location:** `ggen-ai/tests/autonomous_performance.rs`

**Current Constants:**
- `TARGET_CYCLE_TIME_SECONDS: 360` (6 minutes)
- `TARGET_EVOLUTION_TIME_SECONDS: 30`
- `TARGET_REGENERATION_TIME_SECONDS: 120` (2 minutes)

**Rationale:** Test performance benchmarks should be consistent

**Status:** ✅ APPROPRIATE
- These are test targets, not runtime configuration
- Should remain as constants for benchmark consistency

**Recommendation:** Keep as constants

---

#### 9. Test Timeouts

**Location:** Various test files

**Current Values:**
- `Duration::from_secs(10)` - Quick tests
- `Duration::from_secs(30)` - Standard tests
- `Duration::from_secs(60-120)` - Integration tests

**Rationale:** Test timeouts should be consistent and generous

**Status:** ✅ APPROPRIATE
- Test values should be hardcoded for consistency
- Generous timeouts prevent false failures

**Recommendation:** Keep as hardcoded test values

---

#### 10. WIP Integration Settings

**Location:** `config/wip_integration.toml`

**Current Defaults:**
- `ws_url: ws://localhost:8080/events`
- `reconnect_delay_secs: 5`
- `batch_size: 10`
- `confidence_threshold: 0.7`

**Status:** ✅ ALREADY IN TOML CONFIG

**Recommendation:** Good example to follow for other configs

---

### 🟢 TEST/DEVELOPMENT ONLY (Safe to Keep Hardcoded)

#### 11. Example URLs in Tests

**Examples:**
- `http://example.org/` - RDF/SPARQL test data
- `https://example.com` - Mock registry URLs
- `http://www.w3.org/1999/02/22-rdf-syntax-ns#` - Standard RDF namespaces

**Rationale:** Test fixtures should be stable and predictable

**Recommendation:** Keep as hardcoded test data

---

#### 12. Magic Numbers in Algorithms

**Examples:**
- Circuit breaker settings (5 failures, 3 successes, 60s reset)
- Batch sizes for processing
- Buffer sizes

**Rationale:** Algorithm constants should be tuned and stable

**Recommendation:** Keep as constants, possibly with comments explaining tuning

---

## Recommended Configuration Architecture

### 1. Configuration Priority (Highest to Lowest)

```
1. Command-line arguments (--arg)
2. Environment variables (GGEN_*)
3. Project config file (ggen.toml)
4. User config file (~/.config/ggen/config.toml)
5. System config file (/etc/ggen/config.toml)
6. Built-in defaults (code constants)
```

### 2. Proposed TOML Structure

Create `/Users/sac/ggen/config/defaults.toml`:

```toml
# ggen Default Configuration
# This file documents all configurable settings with their defaults

[llm]
# Default LLM provider (openai, anthropic, ollama, mock)
provider = "ollama"

# Global LLM settings (can be overridden per provider)
default_model = "llama3.2"
default_temperature = 0.7
default_max_tokens = 2048
default_top_p = 0.9
timeout_seconds = 30
use_streaming = false

[llm.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"
timeout = 30

[llm.providers.openai]
# API key must be set via OPENAI_API_KEY environment variable
base_url = "https://api.openai.com/v1"
model = "gpt-3.5-turbo"
# organization = ""  # Optional

[llm.providers.anthropic]
# API key must be set via ANTHROPIC_API_KEY environment variable
base_url = "https://api.anthropic.com"
model = "claude-3-sonnet-20240229"

[github]
# GitHub API configuration
# Token must be set via GITHUB_TOKEN or GH_TOKEN environment variable
base_url = "https://api.github.com"
timeout_seconds = 30
user_agent = "ggen-cli"
# enterprise_base_url = ""  # Uncomment for GitHub Enterprise

[registry]
# Package registry configuration
url = "https://seanchatmangpt.github.io/ggen/registry/"
timeout_seconds = 30
# mirror_urls = []  # Fallback registry mirrors
# cache_ttl_hours = 24

[watch]
# File watching configuration
default_poll_interval_ms = 1000
default_debounce_ms = 500
# recursive = false
# clear_screen = false

[ci.release]
# CI/CD release configuration
default_timeout_seconds = 1800
workflow_timeout_seconds = 3600
homebrew_timeout_seconds = 1800

[generation]
# Code generation settings
temperature = 0.7
max_tokens = 4096
top_p = 0.9
streaming = false

[security]
# Security settings
validate_signatures = false
# public_key = ""  # Ed25519 public key for signature verification

[logging]
# Logging configuration
level = "info"  # trace, debug, info, warn, error
format = "pretty"  # pretty, json
# log_file = ""  # Optional file path
```

### 3. Environment Variable Mapping

All TOML settings should be accessible via environment variables:

```bash
# LLM Configuration
GGEN_LLM_PROVIDER=ollama
GGEN_LLM_MODEL=llama3.2
GGEN_LLM_TEMPERATURE=0.7
GGEN_LLM_MAX_TOKENS=2048
GGEN_LLM_TIMEOUT=30

# Provider-specific
OLLAMA_BASE_URL=http://localhost:11434
OLLAMA_MODEL=qwen3-coder:30b
OLLAMA_TIMEOUT=30

OPENAI_API_KEY=sk-...
OPENAI_BASE_URL=https://api.openai.com/v1
OPENAI_MODEL=gpt-3.5-turbo

ANTHROPIC_API_KEY=sk-...
ANTHROPIC_BASE_URL=https://api.anthropic.com
ANTHROPIC_MODEL=claude-3-sonnet-20240229

# GitHub
GITHUB_TOKEN=ghp_...
GGEN_GITHUB_BASE_URL=https://api.github.com
GGEN_GITHUB_TIMEOUT=30

# Registry
GGEN_REGISTRY_URL=https://seanchatmangpt.github.io/ggen/registry/

# Generation
GGEN_GENERATION_TEMPERATURE=0.7
GGEN_GENERATION_MAX_TOKENS=4096
GGEN_GENERATION_TOP_P=0.9
GGEN_GENERATION_STREAMING=false
```

### 4. Implementation Priority

#### Phase 1: Critical (Immediate)
1. ✅ LLM provider configs (already done via env vars)
2. ✅ Registry URL (already done via env var)
3. 🔲 Create `config/defaults.toml` with documentation
4. 🔲 Add TOML config loader to `ggen-ai`

#### Phase 2: Important (Next Release)
1. 🔲 GitHub Enterprise support (custom base URL)
2. 🔲 Watch configuration (poll interval, debounce)
3. 🔲 CI timeout configuration
4. 🔲 User config file support (`~/.config/ggen/config.toml`)

#### Phase 3: Nice-to-Have (Future)
1. 🔲 Per-project overrides in `ggen.toml`
2. 🔲 Config validation and migration tools
3. 🔲 Config generation CLI command (`ggen config init`)

### 5. Security Considerations

#### Never Store in Config Files
- API keys (OpenAI, Anthropic, GitHub)
- Tokens and credentials
- Private keys

#### Always Use Environment Variables or Secret Management
```bash
# Good: Use env vars or secret managers
export OPENAI_API_KEY=$(secret-manager get openai-key)
export GITHUB_TOKEN=$(gh auth token)

# Bad: Never in config files
# api_key = "sk-proj-..." ❌
```

#### Sensitive Configuration Files
- Add to `.gitignore`: `config/*.local.toml`, `.env`, `.env.local`
- Use file permissions: `chmod 600 ~/.config/ggen/config.toml`
- Validate on load: Warn if config files are world-readable

## Summary Matrix

| Configuration | Current Status | Must Configure? | Method | Priority |
|--------------|----------------|-----------------|---------|----------|
| Ollama URL | ✅ ENV | Yes (prod) | ENV + TOML | High |
| OpenAI URL | ✅ ENV | Yes (prod) | ENV + TOML | High |
| Anthropic URL | ✅ ENV | Yes (prod) | ENV + TOML | High |
| GitHub API URL | ❌ Hardcoded | Yes (enterprise) | ENV + TOML | High |
| Registry URL | ✅ ENV | Yes (private) | ENV + TOML | High |
| Watch intervals | ⚠️ CLI only | Maybe (tuning) | TOML | Medium |
| CI timeouts | ⚠️ Hardcoded | Maybe (tuning) | TOML | Medium |
| LLM defaults | ✅ ENV | No (good defaults) | ENV + TOML | Low |
| Test timeouts | ✅ Hardcoded | No (consistency) | Hardcoded | N/A |
| Test fixtures | ✅ Hardcoded | No (stability) | Hardcoded | N/A |

## Next Steps

1. **Create `config/defaults.toml`** with all documented defaults ✅
2. **Add TOML config loader** to `ggen-ai/src/config/mod.rs`
3. **Add GitHub base URL** environment variable support
4. **Update documentation** with configuration examples
5. **Add config validation** to prevent common mistakes
6. **Create migration guide** for existing users

## Example Usage Patterns

### Development (Local Ollama)
```toml
# ~/.config/ggen/config.toml
[llm]
provider = "ollama"

[llm.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"
```

### Production (Cloud APIs)
```bash
# Use environment variables for secrets
export OPENAI_API_KEY="sk-proj-..."
export GGEN_LLM_PROVIDER="openai"
```

### Enterprise (Self-Hosted)
```toml
# /etc/ggen/config.toml
[llm.providers.ollama]
base_url = "https://ollama.internal.company.com"

[github]
base_url = "https://github.company.com/api/v3"

[registry]
url = "https://ggen-registry.company.com"
```

---

**Analysis Date:** 2025-10-10
**Codebase Version:** master branch
**Analyst:** Claude Code Quality Analyzer
