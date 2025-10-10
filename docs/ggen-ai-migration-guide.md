<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-ai Migration Guide](#ggen-ai-migration-guide)
  - [Executive Summary](#executive-summary)
    - [What Changed](#what-changed)
    - [Why These Changes Were Made](#why-these-changes-were-made)
    - [Impact Assessment](#impact-assessment)
  - [Breaking Changes](#breaking-changes)
    - [1. Provider Initialization](#1-provider-initialization)
      - [OpenAI Client](#openai-client)
      - [Anthropic Client](#anthropic-client)
      - [Ollama Client](#ollama-client)
    - [2. Error Handling Changes](#2-error-handling-changes)
    - [3. Model Name Updates](#3-model-name-updates)
  - [Configuration Migration](#configuration-migration)
    - [Step 1: Create `.env` File](#step-1-create-env-file)
    - [Step 2: Add Dependencies](#step-2-add-dependencies)
    - [Step 3: Load Environment Variables](#step-3-load-environment-variables)
    - [Step 4: Optional - Use TOML Configuration](#step-4-optional---use-toml-configuration)
  - [Code Examples](#code-examples)
    - [Example 1: Simple Migration](#example-1-simple-migration)
    - [Example 2: Multiple Providers](#example-2-multiple-providers)
    - [Example 3: Custom Configuration](#example-3-custom-configuration)
    - [Example 4: Error Handling](#example-4-error-handling)
    - [Example 5: Testing with Mock Configuration](#example-5-testing-with-mock-configuration)
  - [Updated Features](#updated-features)
    - [1. Latest Model Support](#1-latest-model-support)
    - [2. Flexible Endpoint Configuration](#2-flexible-endpoint-configuration)
    - [3. Advanced Configuration Options](#3-advanced-configuration-options)
    - [4. Validation](#4-validation)
  - [Error Handling Updates](#error-handling-updates)
    - [New Error Types](#new-error-types)
    - [Error Handling Patterns](#error-handling-patterns)
  - [Testing Your Migration](#testing-your-migration)
    - [Step 1: Compilation Check](#step-1-compilation-check)
    - [Step 2: Environment Variable Tests](#step-2-environment-variable-tests)
    - [Step 3: Integration Tests](#step-3-integration-tests)
    - [Step 4: Error Handling Tests](#step-4-error-handling-tests)
  - [Common Issues and Solutions](#common-issues-and-solutions)
    - [Issue 1: Environment Variables Not Loading](#issue-1-environment-variables-not-loading)
    - [Issue 2: Compilation Errors with Old API](#issue-2-compilation-errors-with-old-api)
    - [Issue 3: Model Not Found](#issue-3-model-not-found)
    - [Issue 4: Timeout Errors](#issue-4-timeout-errors)
    - [Issue 5: Custom Endpoint Not Working](#issue-5-custom-endpoint-not-working)
    - [Issue 6: Ollama Connection Issues](#issue-6-ollama-connection-issues)
  - [Troubleshooting Checklist](#troubleshooting-checklist)
    - [Pre-Migration Checklist](#pre-migration-checklist)
    - [Migration Checklist](#migration-checklist)
    - [Post-Migration Checklist](#post-migration-checklist)
  - [Best Practices](#best-practices)
    - [1. Never Commit API Keys](#1-never-commit-api-keys)
    - [2. Use Environment-Specific Configurations](#2-use-environment-specific-configurations)
    - [3. Validate Early](#3-validate-early)
    - [4. Log Configuration (Without Secrets)](#4-log-configuration-without-secrets)
    - [5. Use Latest Models](#5-use-latest-models)
  - [Migration Support](#migration-support)
    - [Resources](#resources)
    - [Getting Help](#getting-help)
    - [Reporting Issues](#reporting-issues)
  - [Summary](#summary)
    - [What You Need To Do](#what-you-need-to-do)
    - [What Stays The Same](#what-stays-the-same)
    - [Timeline](#timeline)
    - [Final Notes](#final-notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-ai Migration Guide

**Version:** 2.0.0
**Date:** 2025-10-10
**Status:** Official Migration Guide

---

## Executive Summary

### What Changed

The ggen-ai crate has undergone a comprehensive refactoring to align with Rust best practices and the patterns established in ggen-core. The changes focus on:

1. **Configuration Management**: Moved from hardcoded values to environment-based configuration
2. **Error Handling**: Replaced string-based errors with structured error types
3. **API Modernization**: Updated provider model lists and fixed unused parameters
4. **Type Safety**: Introduced builder patterns and proper validation

### Why These Changes Were Made

**Previous Issues:**
- Hardcoded API endpoints and model names prevented flexibility
- String-based errors made debugging difficult
- Missing environment variable support required code changes for configuration
- Outdated model lists excluded newer, better models
- Unused parameters created misleading APIs

**Benefits:**
- **Configuration Flexibility**: Use `.env` files or environment variables
- **Better Error Messages**: Structured errors with context
- **Latest Models**: Support for GPT-4o, Claude 3.5, and more
- **Type Safety**: Compile-time validation of configurations
- **Production Ready**: Follows industry best practices

### Impact Assessment

| Impact Level | Description | Migration Effort |
|--------------|-------------|------------------|
| **High** | Provider initialization | 30 minutes |
| **Medium** | Configuration setup | 15 minutes |
| **Low** | Model name updates | 5 minutes |
| **None** | LLM interactions | No changes needed |

**Total Migration Time:** ~1 hour for typical projects

---

## Breaking Changes

### 1. Provider Initialization

#### OpenAI Client

**Before (v1.x):**
```rust
use ggen_ai::providers::OpenAIClient;

let client = OpenAIClient::new("sk-your-api-key");
```

**After (v2.0):**
```rust
use ggen_ai::{OpenAIClient, OpenAIConfig};

// Method 1: From environment variables
let config = OpenAIConfig::from_env()?;
let client = OpenAIClient::new(config)?;

// Method 2: Explicit configuration
let config = OpenAIConfig::new("sk-your-api-key")
    .with_default_model("gpt-4o")
    .with_timeout(60);
let client = OpenAIClient::new(config)?;

// Method 3: Builder pattern
let config = OpenAIConfig::builder()
    .api_key("sk-your-api-key")
    .default_model("gpt-4o-mini")
    .timeout_secs(120)
    .build()?;
let client = OpenAIClient::new(config)?;
```

#### Anthropic Client

**Before (v1.x):**
```rust
use ggen_ai::providers::AnthropicClient;

let client = AnthropicClient::new("sk-ant-your-api-key");
```

**After (v2.0):**
```rust
use ggen_ai::{AnthropicClient, AnthropicConfig};

// From environment
let config = AnthropicConfig::from_env()?;
let client = AnthropicClient::new(config)?;

// Explicit
let config = AnthropicConfig::new("sk-ant-your-api-key")
    .with_default_model("claude-3-5-sonnet-20241022");
let client = AnthropicClient::new(config)?;
```

#### Ollama Client

**Before (v1.x):**
```rust
use ggen_ai::providers::OllamaClient;

// Always connected to localhost:11434
let client = OllamaClient::new();
```

**After (v2.0):**
```rust
use ggen_ai::{OllamaClient, OllamaConfig};

// From environment (supports remote Ollama)
let config = OllamaConfig::from_env()?;
let client = OllamaClient::new(config)?;

// Custom endpoint
let config = OllamaConfig::new()
    .with_base_url("http://my-server:11434")
    .with_default_model("qwen3-coder:30b");
let client = OllamaClient::new(config)?;
```

### 2. Error Handling Changes

**Before (v1.x):**
```rust
// Errors were generic strings
match client.complete("prompt").await {
    Ok(response) => println!("{}", response),
    Err(e) => eprintln!("Error: {}", e), // Just a string
}
```

**After (v2.0):**
```rust
use ggen_ai::error::GgenAiError;

match client.complete("prompt").await {
    Ok(response) => println!("{}", response),
    Err(GgenAiError::Configuration(msg)) => {
        eprintln!("Config error: {}", msg);
    }
    Err(GgenAiError::LlmProvider { provider, message }) => {
        eprintln!("Provider {} error: {}", provider, message);
    }
    Err(GgenAiError::Network(msg)) => {
        eprintln!("Network error: {}", msg);
    }
    Err(e) => eprintln!("Other error: {}", e),
}
```

### 3. Model Name Updates

Several models have been added or updated. Review your code for hardcoded model names.

**OpenAI - New Models Available:**
```rust
// Old models (still supported)
"gpt-4"
"gpt-4-turbo"
"gpt-3.5-turbo"

// NEW: Latest models (recommended)
"gpt-4o"              // Latest GPT-4 Omni
"gpt-4o-mini"         // Cost-effective GPT-4o
"gpt-4-turbo-preview" // Preview of next generation
```

**Anthropic - New Models Available:**
```rust
// Old models (still supported)
"claude-3-opus-20240229"
"claude-3-sonnet-20240229"
"claude-3-haiku-20240307"

// NEW: Claude 3.5 models (faster and smarter)
"claude-3-5-sonnet-20241022"  // Latest Sonnet (recommended)
"claude-3-5-sonnet-20240620"  // Previous Sonnet
"claude-3-5-haiku-20241022"   // Latest Haiku
```

**Migration Tip:** Search your codebase for old model names:
```bash
# Find old OpenAI models
grep -r "gpt-3.5-turbo\|gpt-4-turbo" --include="*.rs"

# Find old Anthropic models
grep -r "claude-3-opus-20240229\|claude-3-sonnet-20240229" --include="*.rs"
```

---

## Configuration Migration

### Step 1: Create `.env` File

Create a `.env` file in your project root (or copy from `ggen-ai/.env.example`):

```bash
# OpenAI Configuration
OPENAI_API_KEY=sk-your-openai-key-here
# OPENAI_BASE_URL=https://api.openai.com/v1  # Optional: custom endpoint
# OPENAI_DEFAULT_MODEL=gpt-4o  # Optional: default model
# OPENAI_TIMEOUT_SECS=30  # Optional: request timeout

# Anthropic Configuration
ANTHROPIC_API_KEY=sk-ant-your-anthropic-key-here
# ANTHROPIC_BASE_URL=https://api.anthropic.com/v1  # Optional
# ANTHROPIC_DEFAULT_MODEL=claude-3-5-sonnet-20241022  # Optional
# ANTHROPIC_TIMEOUT_SECS=30  # Optional

# Ollama Configuration (all optional)
# OLLAMA_BASE_URL=http://localhost:11434
# OLLAMA_DEFAULT_MODEL=qwen3-coder:30b
# OLLAMA_TIMEOUT_SECS=60

# Global Configuration
# AI_DEFAULT_PROVIDER=openai  # Which provider to use by default

# Logging
RUST_LOG=ggen_ai=info

# Feature Flags
GGEN_AI_ENABLE_STREAMING=true
GGEN_AI_ENABLE_CACHING=false
GGEN_AI_ENABLE_RETRIES=true
```

### Step 2: Add Dependencies

Ensure your `Cargo.toml` includes:

```toml
[dependencies]
ggen-ai = "2.0"
dotenvy = "0.15"  # For loading .env files
tokio = { version = "1.0", features = ["full"] }
```

### Step 3: Load Environment Variables

In your application entry point:

```rust
use ggen_ai::{OpenAIConfig, OpenAIClient};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load .env file
    dotenvy::dotenv().ok();

    // Create client from environment
    let config = OpenAIConfig::from_env()?;
    let client = OpenAIClient::new(config)?;

    // Your code here
    Ok(())
}
```

### Step 4: Optional - Use TOML Configuration

For more complex setups, create `config.toml`:

```toml
[openai]
api_key = "sk-your-key"
base_url = "https://api.openai.com/v1"
default_model = "gpt-4o"
timeout_secs = 60

[anthropic]
api_key = "sk-ant-your-key"
base_url = "https://api.anthropic.com/v1"
default_model = "claude-3-5-sonnet-20241022"
timeout_secs = 30

[ollama]
base_url = "http://localhost:11434"
default_model = "qwen3-coder:30b"
timeout_secs = 90

default_provider = "openai"
```

Load with:

```rust
use ggen_ai::config::AiConfig;

let config = AiConfig::from_file("config.toml")?;
let openai_config = config.get_openai()?;
let client = OpenAIClient::new(openai_config.clone())?;
```

---

## Code Examples

### Example 1: Simple Migration

**Before:**
```rust
use ggen_ai::providers::OpenAIClient;

async fn old_way() {
    let client = OpenAIClient::new("sk-hardcoded-key");
    let response = client.complete("Hello").await.unwrap();
    println!("{}", response);
}
```

**After:**
```rust
use ggen_ai::{OpenAIClient, OpenAIConfig};

async fn new_way() -> Result<(), Box<dyn std::error::Error>> {
    dotenvy::dotenv().ok();

    let config = OpenAIConfig::from_env()?;
    let client = OpenAIClient::new(config)?;

    let response = client.complete("Hello").await?;
    println!("{}", response);
    Ok(())
}
```

### Example 2: Multiple Providers

**Before:**
```rust
let openai = OpenAIClient::new("key1");
let anthropic = AnthropicClient::new("key2");
```

**After:**
```rust
use ggen_ai::config::AiConfig;

let config = AiConfig::from_env()?;

let openai = OpenAIClient::new(config.get_openai()?.clone())?;
let anthropic = AnthropicClient::new(config.get_anthropic()?.clone())?;
```

### Example 3: Custom Configuration

**Before:**
```rust
// Not possible - had to modify source code
```

**After:**
```rust
let config = OpenAIConfig::builder()
    .api_key(std::env::var("CUSTOM_KEY")?)
    .base_url("https://my-proxy.com/v1")
    .default_model("gpt-4o-mini")
    .timeout_secs(120)
    .header("X-Custom-Header", "value")
    .build()?;

let client = OpenAIClient::new(config)?;
```

### Example 4: Error Handling

**Before:**
```rust
match client.complete("prompt").await {
    Ok(r) => println!("{}", r),
    Err(e) => eprintln!("{}", e),  // Generic string error
}
```

**After:**
```rust
use ggen_ai::error::GgenAiError;

match client.complete("prompt").await {
    Ok(r) => println!("{}", r),
    Err(GgenAiError::RateLimited { retry_after }) => {
        eprintln!("Rate limited, retry after {} seconds", retry_after);
        tokio::time::sleep(tokio::time::Duration::from_secs(retry_after)).await;
        // Retry logic
    }
    Err(GgenAiError::InvalidApiKey { provider }) => {
        eprintln!("Invalid API key for {}", provider);
        // Handle authentication
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

### Example 5: Testing with Mock Configuration

**Before:**
```rust
#[test]
fn test_something() {
    let client = OpenAIClient::new("test-key");
    // Test
}
```

**After:**
```rust
#[tokio::test]
async fn test_something() {
    let config = OpenAIConfig::new("test-key")
        .with_base_url("http://localhost:8080")  // Mock server
        .with_timeout(5);

    let client = OpenAIClient::new(config).unwrap();
    // Test with mock server
}
```

---

## Updated Features

### 1. Latest Model Support

**OpenAI Models:**
```rust
// GPT-4o (latest, recommended)
let config = OpenAIConfig::new(api_key)
    .with_default_model("gpt-4o");

// GPT-4o-mini (cost-effective)
let config = OpenAIConfig::new(api_key)
    .with_default_model("gpt-4o-mini");

// All supported models
let models = client.supported_models();
// Returns: ["gpt-4", "gpt-4-turbo", "gpt-4-turbo-preview",
//           "gpt-4o", "gpt-4o-mini", "gpt-3.5-turbo", "gpt-3.5-turbo-16k"]
```

**Anthropic Models:**
```rust
// Claude 3.5 Sonnet (latest, recommended)
let config = AnthropicConfig::new(api_key)
    .with_default_model("claude-3-5-sonnet-20241022");

// Claude 3.5 Haiku (fast and efficient)
let config = AnthropicConfig::new(api_key)
    .with_default_model("claude-3-5-haiku-20241022");

// All supported models
let models = client.supported_models();
// Returns: ["claude-3-5-sonnet-20241022", "claude-3-5-sonnet-20240620",
//           "claude-3-5-haiku-20241022", "claude-3-opus-20240229", ...]
```

### 2. Flexible Endpoint Configuration

**Custom OpenAI-Compatible Endpoints:**
```rust
// Use with Azure OpenAI
let config = OpenAIConfig::new(api_key)
    .with_base_url("https://your-resource.openai.azure.com/v1")
    .with_header("api-key", azure_key);

// Use with local proxy
let config = OpenAIConfig::new(api_key)
    .with_base_url("http://localhost:8000/v1");
```

**Remote Ollama:**
```rust
// Connect to remote Ollama server
let config = OllamaConfig::new()
    .with_base_url("http://gpu-server.local:11434")
    .with_default_model("llama2:70b");
```

### 3. Advanced Configuration Options

**Timeout Configuration:**
```rust
// Short timeout for quick requests
let config = OpenAIConfig::new(api_key)
    .with_timeout(10);  // 10 seconds

// Long timeout for complex generation
let config = OllamaConfig::new()
    .with_timeout(300);  // 5 minutes
```

**Custom Headers:**
```rust
let config = OpenAIConfig::new(api_key)
    .with_header("X-Organization-ID", "org-123")
    .with_header("X-Request-ID", &request_id);
```

### 4. Validation

All configurations now validate before use:

```rust
let config = OpenAIConfig::new("")  // Empty API key
    .with_timeout(0);  // Invalid timeout

// Validation happens automatically
match OpenAIClient::new(config) {
    Ok(client) => { /* ... */ }
    Err(GgenAiError::Configuration(msg)) => {
        eprintln!("Invalid config: {}", msg);
        // "API key cannot be empty"
    }
    Err(e) => eprintln!("{}", e),
}
```

---

## Error Handling Updates

### New Error Types

```rust
pub enum GgenAiError {
    /// Configuration errors (missing keys, invalid values)
    Configuration(String),

    /// Provider-specific errors (API failures)
    LlmProvider { provider: String, message: String },

    /// Network errors (timeouts, connection failures)
    Network(String),

    /// API key is invalid or missing
    InvalidApiKey { provider: String },

    /// Rate limited by API
    RateLimited { retry_after: u64 },

    /// Request timeout
    Timeout { seconds: u64 },

    /// JSON parsing errors
    JsonParse(String),

    /// IO errors
    Io(String),
}
```

### Error Handling Patterns

**Pattern 1: Specific Error Handling**
```rust
match client.complete("prompt").await {
    Ok(response) => process(response),
    Err(GgenAiError::RateLimited { retry_after }) => {
        log::warn!("Rate limited, waiting {} seconds", retry_after);
        tokio::time::sleep(Duration::from_secs(retry_after)).await;
        // Retry
    }
    Err(GgenAiError::InvalidApiKey { provider }) => {
        log::error!("Invalid API key for {}", provider);
        return Err("Authentication failed".into());
    }
    Err(e) => {
        log::error!("LLM error: {}", e);
        return Err(e.into());
    }
}
```

**Pattern 2: Retry with Backoff**
```rust
use ggen_ai::error::GgenAiError;

async fn complete_with_retry(
    client: &OpenAIClient,
    prompt: &str,
    max_retries: u32
) -> Result<String, GgenAiError> {
    let mut attempts = 0;

    loop {
        match client.complete(prompt).await {
            Ok(response) => return Ok(response),
            Err(GgenAiError::RateLimited { retry_after }) if attempts < max_retries => {
                attempts += 1;
                log::info!("Retry {}/{}, waiting {}s", attempts, max_retries, retry_after);
                tokio::time::sleep(Duration::from_secs(retry_after)).await;
            }
            Err(GgenAiError::Network(msg)) if attempts < max_retries => {
                attempts += 1;
                let backoff = 2u64.pow(attempts);
                log::info!("Network error, retry {}/{}, backoff {}s", attempts, max_retries, backoff);
                tokio::time::sleep(Duration::from_secs(backoff)).await;
            }
            Err(e) => return Err(e),
        }
    }
}
```

**Pattern 3: Graceful Degradation**
```rust
async fn get_completion(prompt: &str) -> Result<String, Box<dyn std::error::Error>> {
    // Try OpenAI first
    if let Ok(config) = OpenAIConfig::from_env() {
        if let Ok(client) = OpenAIClient::new(config) {
            if let Ok(response) = client.complete(prompt).await {
                return Ok(response);
            }
        }
    }

    // Fall back to Anthropic
    if let Ok(config) = AnthropicConfig::from_env() {
        if let Ok(client) = AnthropicClient::new(config) {
            if let Ok(response) = client.complete(prompt).await {
                return Ok(response);
            }
        }
    }

    // Fall back to local Ollama
    if let Ok(config) = OllamaConfig::from_env() {
        if let Ok(client) = OllamaClient::new(config) {
            return Ok(client.complete(prompt).await?);
        }
    }

    Err("No available LLM provider".into())
}
```

---

## Testing Your Migration

### Step 1: Compilation Check

```bash
# Clean build to catch all issues
cargo clean
cargo check --package ggen-ai

# Check for warnings
cargo clippy --package ggen-ai
```

### Step 2: Environment Variable Tests

**Test 1: Basic Configuration**
```bash
# Set environment variables
export OPENAI_API_KEY="sk-test-key"
export ANTHROPIC_API_KEY="sk-ant-test-key"

# Run tests
cargo test --package ggen-ai test_config

# Expected output:
# test test_openai_config_from_env ... ok
# test test_anthropic_config_from_env ... ok
```

**Test 2: Model Support**
```bash
cargo test --package ggen-ai test_supported_models

# Expected output should include:
# - gpt-4o, gpt-4o-mini for OpenAI
# - claude-3-5-sonnet-20241022 for Anthropic
```

**Test 3: Provider Initialization**
```bash
# Test OpenAI
cargo test --package ggen-ai test_openai_client_creation

# Test Anthropic
cargo test --package ggen-ai test_anthropic_client_creation

# Test Ollama
cargo test --package ggen-ai test_ollama_client_creation
```

### Step 3: Integration Tests

**Test with Real APIs** (requires valid API keys):

```bash
# OpenAI integration test
export OPENAI_API_KEY="your-real-key"
cargo test --package ggen-ai test_openai_completion -- --ignored

# Anthropic integration test
export ANTHROPIC_API_KEY="your-real-key"
cargo test --package ggen-ai test_anthropic_completion -- --ignored
```

### Step 4: Error Handling Tests

```rust
#[tokio::test]
async fn test_invalid_api_key() {
    let config = OpenAIConfig::new("invalid-key");
    let client = OpenAIClient::new(config).unwrap();

    let result = client.complete("test").await;
    assert!(matches!(result, Err(GgenAiError::InvalidApiKey { .. })));
}

#[tokio::test]
async fn test_configuration_validation() {
    let config = OpenAIConfig::new("")  // Empty key
        .with_timeout(0);  // Invalid timeout

    let result = OpenAIClient::new(config);
    assert!(matches!(result, Err(GgenAiError::Configuration(_))));
}
```

---

## Common Issues and Solutions

### Issue 1: Environment Variables Not Loading

**Problem:**
```rust
Error: Configuration("OPENAI_API_KEY environment variable not set")
```

**Solution:**
```rust
// Add this at the start of your main function
dotenvy::dotenv().ok();

// Or load explicitly
dotenvy::from_filename(".env").ok();
```

**Verify:**
```bash
# Check .env file exists
ls -la .env

# Check variable is set
echo $OPENAI_API_KEY
```

### Issue 2: Compilation Errors with Old API

**Problem:**
```
error[E0061]: this function takes 1 argument but 2 were supplied
  --> src/main.rs:10:18
   |
10 |     let client = OpenAIClient::new("api-key", config);
   |                  ^^^^^^^^^^^^^^^^^
```

**Solution:**
Update to new API - `new()` now takes only config:
```rust
// Old: OpenAIClient::new("api-key", config)
// New:
let config = OpenAIConfig::new("api-key");
let client = OpenAIClient::new(config)?;
```

### Issue 3: Model Not Found

**Problem:**
```
Error: LlmProvider { provider: "openai", message: "Model gpt-3.5-turbo-16k not found" }
```

**Solution:**
Check supported models list has been updated. Update your code:
```rust
// Old model
let model = "gpt-3.5-turbo-16k";

// New model (better performance)
let model = "gpt-4o-mini";

// Verify support
if client.supports_model(model) {
    // Use model
} else {
    // Fall back
}
```

### Issue 4: Timeout Errors

**Problem:**
```
Error: Timeout { seconds: 30 }
```

**Solution:**
Increase timeout for long-running requests:
```rust
let config = OpenAIConfig::new(api_key)
    .with_timeout(120);  // 2 minutes

// Or set in environment
// OPENAI_TIMEOUT_SECS=120
```

### Issue 5: Custom Endpoint Not Working

**Problem:**
```
Error: Network("connection refused")
```

**Solution:**
Verify URL format and accessibility:
```rust
// Ensure URL includes /v1 suffix for OpenAI-compatible endpoints
let config = OpenAIConfig::new(api_key)
    .with_base_url("https://api.example.com/v1");  // Note /v1

// Test endpoint
curl https://api.example.com/v1/models
```

### Issue 6: Ollama Connection Issues

**Problem:**
```
Error: Network("connection refused to localhost:11434")
```

**Solution:**
```bash
# Check Ollama is running
ollama list

# If not running, start it
ollama serve

# Or set custom URL
export OLLAMA_BASE_URL="http://remote-server:11434"
```

---

## Troubleshooting Checklist

### Pre-Migration Checklist

- [ ] Backup your code
- [ ] Review all files using ggen-ai providers
- [ ] Note all hardcoded API keys and models
- [ ] Check Cargo.toml for ggen-ai version

### Migration Checklist

- [ ] Create `.env` file with API keys
- [ ] Update Cargo.toml to ggen-ai 2.0
- [ ] Update provider initialization code
- [ ] Replace hardcoded models with latest versions
- [ ] Update error handling to use new error types
- [ ] Add `dotenvy::dotenv().ok()` to main()
- [ ] Run `cargo check` and fix errors
- [ ] Run `cargo clippy` and fix warnings

### Post-Migration Checklist

- [ ] All tests pass (`cargo test`)
- [ ] No compilation warnings
- [ ] Environment variables load correctly
- [ ] API calls work with new configuration
- [ ] Error handling works as expected
- [ ] Remove old commented code
- [ ] Update documentation
- [ ] Commit changes

---

## Best Practices

### 1. Never Commit API Keys

```bash
# Add to .gitignore
echo ".env" >> .gitignore
echo "config.toml" >> .gitignore

# Provide example files
cp .env .env.example
# Remove actual keys from .env.example
```

### 2. Use Environment-Specific Configurations

```rust
// Development
let config = if cfg!(debug_assertions) {
    OpenAIConfig::new(api_key)
        .with_base_url("http://localhost:8080")  // Mock server
} else {
    // Production
    OpenAIConfig::from_env()?
};
```

### 3. Validate Early

```rust
// Validate configuration at startup
async fn validate_providers() -> Result<(), Box<dyn std::error::Error>> {
    if let Ok(config) = OpenAIConfig::from_env() {
        config.validate()?;
        let client = OpenAIClient::new(config)?;
        log::info!("OpenAI provider initialized");
    }

    if let Ok(config) = AnthropicConfig::from_env() {
        config.validate()?;
        let client = AnthropicClient::new(config)?;
        log::info!("Anthropic provider initialized");
    }

    Ok(())
}
```

### 4. Log Configuration (Without Secrets)

```rust
use log::info;

let config = OpenAIConfig::from_env()?;
info!("OpenAI config: base_url={}, model={:?}, timeout={}s",
    config.base_url,
    config.default_model,
    config.timeout_secs
);
// Never log: config.api_key
```

### 5. Use Latest Models

```rust
// Prefer latest models for best results
const RECOMMENDED_OPENAI_MODEL: &str = "gpt-4o";
const RECOMMENDED_ANTHROPIC_MODEL: &str = "claude-3-5-sonnet-20241022";

let config = OpenAIConfig::from_env()?
    .with_default_model(RECOMMENDED_OPENAI_MODEL);
```

---

## Migration Support

### Resources

- **Documentation**: [ggen-ai README](../ggen-ai/README.md)
- **Examples**: [ggen-ai/examples/](../ggen-ai/examples/)
- **API Reference**: Run `cargo doc --open --package ggen-ai`

### Getting Help

1. **Check this guide** for your specific issue
2. **Review error messages** - they now include helpful context
3. **Run tests** with verbose output: `cargo test -- --nocapture`
4. **Check logs** with `RUST_LOG=ggen_ai=debug`

### Reporting Issues

If you encounter problems:

1. **Verify configuration**:
   ```bash
   cargo run --example check-config
   ```

2. **Enable debug logging**:
   ```rust
   env_logger::init();
   std::env::set_var("RUST_LOG", "ggen_ai=debug");
   ```

3. **Create minimal reproduction**:
   ```rust
   use ggen_ai::{OpenAIConfig, OpenAIClient};

   #[tokio::main]
   async fn main() -> Result<(), Box<dyn std::error::Error>> {
       dotenvy::dotenv().ok();
       let config = OpenAIConfig::from_env()?;
       let client = OpenAIClient::new(config)?;
       println!("Success!");
       Ok(())
   }
   ```

---

## Summary

### What You Need To Do

1. **Create `.env` file** with your API keys
2. **Update initialization code** to use config objects
3. **Update model names** to latest versions (optional but recommended)
4. **Update error handling** to use structured error types (optional but recommended)
5. **Test everything** works

### What Stays The Same

- LLM interaction methods (complete, stream, etc.)
- Generator APIs (TemplateGenerator, SparqlGenerator, etc.)
- MCP server integration
- Return types for completions

### Timeline

| Task | Time Required |
|------|---------------|
| Create .env file | 5 minutes |
| Update initialization | 20 minutes |
| Update model names | 5 minutes |
| Update error handling | 15 minutes |
| Testing | 15 minutes |
| **Total** | **~1 hour** |

### Final Notes

This migration improves:
- **Security** - API keys in environment, not code
- **Flexibility** - Easy to switch endpoints and models
- **Debugging** - Better error messages
- **Maintenance** - Follows Rust best practices

The new API is more verbose but much more powerful and maintainable. Once migrated, you'll have a production-ready LLM integration.

**Questions?** Check the troubleshooting section or run the examples in `ggen-ai/examples/`.

---

**Document Version:** 1.0
**Last Updated:** 2025-10-10
**Maintainer:** ggen-ai Documentation Team
