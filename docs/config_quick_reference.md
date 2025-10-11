<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Configuration Quick Reference](#configuration-quick-reference)
  - [Environment Variables Cheat Sheet](#environment-variables-cheat-sheet)
    - [LLM Providers](#llm-providers)
    - [GitHub Integration](#github-integration)
    - [Package Registry](#package-registry)
    - [Code Generation](#code-generation)
    - [Logging](#logging)
    - [Development/Testing](#developmenttesting)
  - [Common Configurations](#common-configurations)
    - [🏠 Local Development (Ollama)](#-local-development-ollama)
    - [☁️ Cloud Development (OpenAI)](#-cloud-development-openai)
    - [🏢 Enterprise Setup](#-enterprise-setup)
    - [🧪 Testing/CI](#-testingci)
  - [Configuration Files (Coming in Phase 2)](#configuration-files-coming-in-phase-2)
    - [User Config: `~/.config/ggen/config.toml`](#user-config-configggenconfigtoml)
    - [Project Config: `./ggen.toml`](#project-config-ggentoml)
  - [Priority Order](#priority-order)
  - [Validation](#validation)
    - [Check Your Configuration](#check-your-configuration)
    - [Common Issues](#common-issues)
  - [Security Best Practices](#security-best-practices)
    - [✅ DO:](#-do)
    - [❌ DON'T:](#-dont)
  - [Platform-Specific Config Locations](#platform-specific-config-locations)
    - [Linux](#linux)
    - [macOS](#macos)
    - [Windows](#windows)
  - [Quick Examples](#quick-examples)
    - [Use Different Model](#use-different-model)
    - [Increase Timeout](#increase-timeout)
    - [Verbose Logging](#verbose-logging)
    - [Multiple Environments](#multiple-environments)
  - [Reference Documents](#reference-documents)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Configuration Quick Reference

## Environment Variables Cheat Sheet

### LLM Providers

```bash
# Provider Selection
export GGEN_LLM_PROVIDER=ollama          # openai, anthropic, ollama, mock

# Ollama (Local AI)
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b
export OLLAMA_TIMEOUT=30

# OpenAI
export OPENAI_API_KEY=sk-...             # REQUIRED for OpenAI
export OPENAI_BASE_URL=https://api.openai.com/v1
export OPENAI_MODEL=gpt-3.5-turbo
export OPENAI_ORGANIZATION=org-...       # Optional

# Anthropic
export ANTHROPIC_API_KEY=sk-ant-...      # REQUIRED for Anthropic
export ANTHROPIC_BASE_URL=https://api.anthropic.com
export ANTHROPIC_MODEL=claude-3-sonnet-20240229
```

### GitHub Integration

```bash
# GitHub API
export GITHUB_TOKEN=ghp_...              # REQUIRED for private repos
export GH_TOKEN=ghp_...                  # Alternative to GITHUB_TOKEN
export GGEN_GITHUB_BASE_URL=https://api.github.com
export GGEN_GITHUB_TIMEOUT=30

# GitHub Enterprise
export GGEN_GITHUB_BASE_URL=https://github.company.com/api/v3
```

### Package Registry

```bash
# Registry Configuration
export GGEN_REGISTRY_URL=https://seanchatmangpt.github.io/ggen/registry/
export GGEN_REGISTRY_TIMEOUT=30
```

### Code Generation

```bash
# Generation Parameters
export GGEN_GENERATION_TEMPERATURE=0.7   # 0.0-2.0 (creativity)
export GGEN_GENERATION_MAX_TOKENS=4096   # Max output length
export GGEN_GENERATION_TOP_P=0.9         # 0.0-1.0 (diversity)
export GGEN_GENERATION_STREAMING=false   # Enable streaming
```

### Logging

```bash
# Log Configuration
export GGEN_LOG_LEVEL=info               # trace, debug, info, warn, error
export RUST_LOG=ggen=debug               # Alternative (Rust standard)
export GGEN_LOG_FORMAT=pretty            # pretty, json, compact
```

### Development/Testing

```bash
# Test Mode (uses mock clients, no API calls)
export GGEN_TEST_MODE=1
export GGEN_ALLOW_LIVE_CALLS=0           # Disable live API calls in tests
```

---

## Common Configurations

### 🏠 Local Development (Ollama)

```bash
# Use local Ollama instance
export GGEN_LLM_PROVIDER=ollama
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=qwen3-coder:30b
```

### ☁️ Cloud Development (OpenAI)

```bash
# Use OpenAI API
export GGEN_LLM_PROVIDER=openai
export OPENAI_API_KEY=sk-proj-...
export OPENAI_MODEL=gpt-4
```

### 🏢 Enterprise Setup

```bash
# GitHub Enterprise + Private Registry
export GGEN_GITHUB_BASE_URL=https://github.company.com/api/v3
export GITHUB_TOKEN=ghp_...
export GGEN_REGISTRY_URL=https://registry.company.com/
```

### 🧪 Testing/CI

```bash
# Mock everything for tests
export GGEN_TEST_MODE=1
export GGEN_LLM_PROVIDER=mock
export GGEN_LOG_LEVEL=warn
```

---

## Configuration Files (Coming in Phase 2)

### User Config: `~/.config/ggen/config.toml`

```toml
[llm]
provider = "ollama"

[llm.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"
```

### Project Config: `./ggen.toml`

```toml
[generation]
temperature = 0.9
max_tokens = 8000

[logging]
level = "debug"
```

---

## Priority Order

1. 🥇 **Command-line arguments** (highest priority)
2. 🥈 **Environment variables**
3. 🥉 **Project config** (./ggen.toml)
4. 4️⃣ **User config** (~/.config/ggen/config.toml)
5. 5️⃣ **System config** (/etc/ggen/config.toml)
6. 6️⃣ **Built-in defaults** (lowest priority)

---

## Validation

### Check Your Configuration

```bash
# Show effective configuration (future)
ggen config show

# Validate configuration (future)
ggen config validate

# Test connection
ggen ai generate "test" --verbose
```

### Common Issues

**❌ API Key Not Found**
```bash
Error: OPENAI_API_KEY environment variable not set
```
**✅ Solution:** `export OPENAI_API_KEY=sk-...`

---

**❌ Connection Refused (Ollama)**
```bash
Error: Connection refused at http://localhost:11434
```
**✅ Solution:** Start Ollama: `ollama serve`

---

**❌ GitHub API Rate Limited**
```bash
Error: API rate limit exceeded
```
**✅ Solution:** Set `GITHUB_TOKEN=ghp_...`

---

## Security Best Practices

### ✅ DO:
- Store API keys in environment variables
- Use `.env` files (add to `.gitignore`)
- Use secret managers in production
- Restrict file permissions: `chmod 600 ~/.config/ggen/config.toml`

### ❌ DON'T:
- Commit API keys to git
- Store keys in TOML files
- Share keys in logs/screenshots
- Use production keys in development

---

## Platform-Specific Config Locations

### Linux
```
System: /etc/ggen/config.toml
User:   ~/.config/ggen/config.toml
        $XDG_CONFIG_HOME/ggen/config.toml
Project: ./ggen.toml
```

### macOS
```
System: /Library/Application Support/ggen/config.toml
User:   ~/Library/Application Support/ggen/config.toml
        ~/.config/ggen/config.toml
Project: ./ggen.toml
```

### Windows
```
System: C:\ProgramData\ggen\config.toml
User:   %APPDATA%\ggen\config.toml
        %USERPROFILE%\.config\ggen\config.toml
Project: .\ggen.toml
```

---

## Quick Examples

### Use Different Model

```bash
# Temporary override
OLLAMA_MODEL=codellama ggen ai generate "create API"

# Permanent
export OLLAMA_MODEL=codellama
```

### Increase Timeout

```bash
# For slow connections
export OLLAMA_TIMEOUT=120
export GGEN_GITHUB_TIMEOUT=60
```

### Verbose Logging

```bash
# Debug mode
GGEN_LOG_LEVEL=debug ggen ai generate "test"

# Or with Rust standard
RUST_LOG=ggen=debug,ggen_ai=trace ggen ai generate "test"
```

### Multiple Environments

```bash
# Development
alias ggen-dev='GGEN_LLM_PROVIDER=ollama ggen'

# Production
alias ggen-prod='GGEN_LLM_PROVIDER=openai OPENAI_API_KEY=$PROD_KEY ggen'

# Testing
alias ggen-test='GGEN_TEST_MODE=1 ggen'
```

---

## Reference Documents

- **Full Analysis:** `/Users/sac/ggen/docs/hardcoded_config_analysis.md`
- **Migration Plan:** `/Users/sac/ggen/docs/config_migration_plan.md`
- **Default Config:** `/Users/sac/ggen/config/defaults.toml`
- **WIP Integration:** `/Users/sac/ggen/config/wip_integration.toml`

---

**Last Updated:** 2025-10-10
**Version:** 1.0
