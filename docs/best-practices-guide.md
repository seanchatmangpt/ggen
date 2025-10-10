<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-ai Best Practices Guide](#ggen-ai-best-practices-guide)
  - [Table of Contents](#table-of-contents)
  - [Architecture Overview](#architecture-overview)
    - [Current Structure](#current-structure)
    - [Key Design Patterns](#key-design-patterns)
  - [Configuration Management](#configuration-management)
    - [Problem Areas Identified](#problem-areas-identified)
    - [Best Practice Solution](#best-practice-solution)
      - [1. Create a Configuration Module](#1-create-a-configuration-module)
      - [2. Configuration File Template](#2-configuration-file-template)
      - [3. Environment Variables Template](#3-environment-variables-template)
  - [Error Handling Patterns](#error-handling-patterns)
    - [Current Implementation Analysis](#current-implementation-analysis)
    - [Enhanced Error Handling](#enhanced-error-handling)
      - [1. Enhanced Error Type](#1-enhanced-error-type)
      - [2. Retry Logic Template](#2-retry-logic-template)
  - [Testing Strategies](#testing-strategies)
    - [Current Testing Analysis](#current-testing-analysis)
    - [Comprehensive Testing Strategy](#comprehensive-testing-strategy)
      - [1. Test Organization Structure](#1-test-organization-structure)
      - [2. Test Fixtures Template](#2-test-fixtures-template)
      - [3. Integration Test Template](#3-integration-test-template)
      - [4. Property-Based Testing Template](#4-property-based-testing-template)
  - [Code Organization](#code-organization)
    - [Module Design Principles](#module-design-principles)
    - [File Size Guidelines](#file-size-guidelines)
    - [Module Split Template](#module-split-template)
  - [Implementation Templates](#implementation-templates)
    - [1. Provider Implementation Template](#1-provider-implementation-template)
    - [2. Generator Implementation Template](#2-generator-implementation-template)
  - [Summary](#summary)
    - [Implementation Checklist](#implementation-checklist)
    - [Quick Reference](#quick-reference)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-ai Best Practices Guide

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Configuration Management](#configuration-management)
3. [Error Handling Patterns](#error-handling-patterns)
4. [Testing Strategies](#testing-strategies)
5. [Code Organization](#code-organization)
6. [Implementation Templates](#implementation-templates)

---

## Architecture Overview

### Current Structure

The ggen-ai crate is organized into the following modules:

```
ggen-ai/
├── src/
│   ├── client.rs           # LLM client abstraction
│   ├── error.rs            # Error type definitions
│   ├── lib.rs              # Public API
│   ├── providers/          # LLM provider implementations
│   │   ├── adapter.rs      # Mock client & utilities
│   │   ├── anthropic.rs    # Anthropic provider
│   │   ├── openai.rs       # OpenAI provider
│   │   └── ollama.rs       # Ollama provider
│   ├── generators/         # AI-powered generators
│   │   ├── template.rs     # Template generation
│   │   ├── sparql.rs       # SPARQL generation
│   │   ├── ontology.rs     # Ontology generation
│   │   └── refactor.rs     # Refactoring assistant
│   ├── prompts/            # Prompt templates
│   └── mcp/                # MCP server integration
└── Cargo.toml
```

### Key Design Patterns

1. **Trait-based abstraction**: `LlmClient` trait allows provider interchangeability
2. **Async/await**: All I/O operations use tokio async runtime
3. **Error propagation**: Custom error types with `thiserror` for clear error messages
4. **Builder pattern**: Prompt builders for flexible template generation
5. **Streaming support**: Futures streams for long-running operations

---

## Configuration Management

### Problem Areas Identified

**Current Issues:**
- ❌ API keys hardcoded in test code
- ❌ No environment variable management
- ❌ Base URLs hardcoded in providers
- ❌ Configuration scattered across modules

### Best Practice Solution

#### 1. Create a Configuration Module

**File: `ggen-ai/src/config.rs`**

```rust
//! Configuration management for ggen-ai

use serde::{Deserialize, Serialize};
use std::path::Path;

/// Global configuration for ggen-ai
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenAiConfig {
    /// LLM provider configurations
    pub providers: ProviderConfigs,

    /// Default provider to use
    pub default_provider: String,

    /// Logging configuration
    pub logging: LoggingConfig,

    /// Feature flags
    pub features: FeatureFlags,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderConfigs {
    pub openai: Option<OpenAIConfig>,
    pub anthropic: Option<AnthropicConfig>,
    pub ollama: Option<OllamaConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpenAIConfig {
    /// API key from environment variable
    #[serde(skip_serializing)]
    pub api_key: Option<String>,

    /// Base URL (defaults to official API)
    pub base_url: Option<String>,

    /// Default model
    pub default_model: String,

    /// Request timeout in seconds
    pub timeout_secs: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnthropicConfig {
    #[serde(skip_serializing)]
    pub api_key: Option<String>,

    pub base_url: Option<String>,
    pub default_model: String,
    pub timeout_secs: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OllamaConfig {
    pub base_url: String,
    pub default_model: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoggingConfig {
    pub level: String,
    pub format: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeatureFlags {
    pub enable_streaming: bool,
    pub enable_caching: bool,
    pub enable_retries: bool,
}

impl Default for GgenAiConfig {
    fn default() -> Self {
        Self {
            providers: ProviderConfigs {
                openai: Some(OpenAIConfig {
                    api_key: None,
                    base_url: None,
                    default_model: "gpt-4".to_string(),
                    timeout_secs: 60,
                }),
                anthropic: Some(AnthropicConfig {
                    api_key: None,
                    base_url: None,
                    default_model: "claude-3-sonnet-20240229".to_string(),
                    timeout_secs: 60,
                }),
                ollama: Some(OllamaConfig {
                    base_url: "http://localhost:11434".to_string(),
                    default_model: "llama2".to_string(),
                }),
            },
            default_provider: "openai".to_string(),
            logging: LoggingConfig {
                level: "info".to_string(),
                format: "pretty".to_string(),
            },
            features: FeatureFlags {
                enable_streaming: true,
                enable_caching: false,
                enable_retries: true,
            },
        }
    }
}

impl GgenAiConfig {
    /// Load configuration from TOML file
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, Box<dyn std::error::Error>> {
        let contents = std::fs::read_to_string(path)?;
        let mut config: Self = toml::from_str(&contents)?;

        // Load API keys from environment
        config.load_env_vars()?;

        Ok(config)
    }

    /// Load configuration from environment variables
    pub fn from_env() -> Result<Self, Box<dyn std::error::Error>> {
        let mut config = Self::default();
        config.load_env_vars()?;
        Ok(config)
    }

    /// Load API keys from environment variables
    fn load_env_vars(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        // OpenAI
        if let Some(ref mut openai) = self.providers.openai {
            openai.api_key = std::env::var("OPENAI_API_KEY").ok();
            if let Ok(base_url) = std::env::var("OPENAI_BASE_URL") {
                openai.base_url = Some(base_url);
            }
        }

        // Anthropic
        if let Some(ref mut anthropic) = self.providers.anthropic {
            anthropic.api_key = std::env::var("ANTHROPIC_API_KEY").ok();
            if let Ok(base_url) = std::env::var("ANTHROPIC_BASE_URL") {
                anthropic.base_url = Some(base_url);
            }
        }

        // Ollama
        if let Some(ref mut ollama) = self.providers.ollama {
            if let Ok(base_url) = std::env::var("OLLAMA_BASE_URL") {
                ollama.base_url = base_url;
            }
        }

        Ok(())
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<(), String> {
        // Check if at least one provider is configured
        let has_provider = match self.default_provider.as_str() {
            "openai" => self.providers.openai.as_ref()
                .and_then(|c| c.api_key.as_ref())
                .is_some(),
            "anthropic" => self.providers.anthropic.as_ref()
                .and_then(|c| c.api_key.as_ref())
                .is_some(),
            "ollama" => true, // Ollama doesn't require API key
            _ => false,
        };

        if !has_provider {
            return Err(format!(
                "No valid configuration found for provider: {}. Please set the appropriate API key environment variable.",
                self.default_provider
            ));
        }

        Ok(())
    }
}

/// Configuration builder for tests and programmatic use
pub struct ConfigBuilder {
    config: GgenAiConfig,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self {
            config: GgenAiConfig::default(),
        }
    }

    pub fn with_openai(mut self, api_key: String, model: Option<String>) -> Self {
        self.config.providers.openai = Some(OpenAIConfig {
            api_key: Some(api_key),
            base_url: None,
            default_model: model.unwrap_or_else(|| "gpt-4".to_string()),
            timeout_secs: 60,
        });
        self.config.default_provider = "openai".to_string();
        self
    }

    pub fn with_anthropic(mut self, api_key: String, model: Option<String>) -> Self {
        self.config.providers.anthropic = Some(AnthropicConfig {
            api_key: Some(api_key),
            base_url: None,
            default_model: model.unwrap_or_else(|| "claude-3-sonnet-20240229".to_string()),
            timeout_secs: 60,
        });
        self.config.default_provider = "anthropic".to_string();
        self
    }

    pub fn with_ollama(mut self, base_url: String, model: Option<String>) -> Self {
        self.config.providers.ollama = Some(OllamaConfig {
            base_url,
            default_model: model.unwrap_or_else(|| "llama2".to_string()),
        });
        self.config.default_provider = "ollama".to_string();
        self
    }

    pub fn build(self) -> GgenAiConfig {
        self.config
    }
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = GgenAiConfig::default();
        assert_eq!(config.default_provider, "openai");
        assert!(config.providers.openai.is_some());
    }

    #[test]
    fn test_config_builder() {
        let config = ConfigBuilder::new()
            .with_openai("test-key".to_string(), Some("gpt-3.5-turbo".to_string()))
            .build();

        assert_eq!(config.default_provider, "openai");
        assert_eq!(
            config.providers.openai.unwrap().default_model,
            "gpt-3.5-turbo"
        );
    }

    #[test]
    fn test_validation_fails_without_api_key() {
        let config = GgenAiConfig::default();
        assert!(config.validate().is_err());
    }
}
```

#### 2. Configuration File Template

**File: `ggen-ai/config.toml.example`**

```toml
# ggen-ai Configuration File
# Copy to config.toml and fill in your API keys

# Default LLM provider to use
default_provider = "openai"

[providers.openai]
# API key loaded from OPENAI_API_KEY environment variable
# base_url = "https://api.openai.com/v1"  # Optional: custom base URL
default_model = "gpt-4"
timeout_secs = 60

[providers.anthropic]
# API key loaded from ANTHROPIC_API_KEY environment variable
# base_url = "https://api.anthropic.com/v1"  # Optional: custom base URL
default_model = "claude-3-sonnet-20240229"
timeout_secs = 60

[providers.ollama]
base_url = "http://localhost:11434"
default_model = "llama2"

[logging]
level = "info"  # trace, debug, info, warn, error
format = "pretty"  # pretty, json

[features]
enable_streaming = true
enable_caching = false
enable_retries = true
```

#### 3. Environment Variables Template

**File: `.env.example`**

```bash
# OpenAI Configuration
OPENAI_API_KEY=your_openai_api_key_here
# OPENAI_BASE_URL=https://api.openai.com/v1  # Optional

# Anthropic Configuration
ANTHROPIC_API_KEY=your_anthropic_api_key_here
# ANTHROPIC_BASE_URL=https://api.anthropic.com/v1  # Optional

# Ollama Configuration
# OLLAMA_BASE_URL=http://localhost:11434  # Optional

# Logging
RUST_LOG=ggen_ai=info

# Feature Flags
GGEN_AI_ENABLE_STREAMING=true
GGEN_AI_ENABLE_CACHING=false
GGEN_AI_ENABLE_RETRIES=true
```

---

## Error Handling Patterns

### Current Implementation Analysis

**Strengths:**
- ✅ Uses `thiserror` for custom error types
- ✅ Implements `From` traits for conversion
- ✅ Context-specific error variants

**Areas for Improvement:**
- ⚠️ Limited error context in some cases
- ⚠️ No error recovery strategies
- ⚠️ Missing structured error metadata

### Enhanced Error Handling

#### 1. Enhanced Error Type

**File: `ggen-ai/src/error.rs` (Enhanced)**

```rust
//! Enhanced error types for ggen-ai

use std::fmt;

/// Errors that can occur in ggen-ai operations
#[derive(Debug, thiserror::Error)]
pub enum GgenAiError {
    /// LLM provider errors with context
    #[error("LLM provider error ({provider}): {message}")]
    LlmProvider {
        provider: String,
        message: String,
        #[source]
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
    },

    /// HTTP request errors with retry information
    #[error("HTTP error (attempt {attempt}/{max_attempts}): {0}")]
    Http {
        #[source]
        source: reqwest::Error,
        attempt: u32,
        max_attempts: u32,
    },

    /// Configuration errors with helpful hints
    #[error("Configuration error: {message}\nHint: {hint}")]
    Configuration {
        message: String,
        hint: String,
    },

    /// Validation errors with field information
    #[error("Validation error in {field}: {message}")]
    Validation {
        field: String,
        message: String,
        value: Option<String>,
    },

    /// Template generation errors with partial content
    #[error("Template generation error: {message}")]
    TemplateGeneration {
        message: String,
        partial_content: Option<String>,
    },

    /// Rate limit errors with retry-after information
    #[error("Rate limit exceeded. Retry after {retry_after_secs} seconds")]
    RateLimit {
        retry_after_secs: u64,
        provider: String,
    },

    /// Authentication errors
    #[error("Authentication failed for provider {provider}: {message}")]
    Authentication {
        provider: String,
        message: String,
    },

    /// Timeout errors
    #[error("Operation timed out after {timeout_secs} seconds")]
    Timeout {
        timeout_secs: u64,
        operation: String,
    },

    /// JSON errors
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    /// IO errors
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// UTF-8 errors
    #[error("UTF-8 conversion error: {0}")]
    Utf8(#[from] std::string::FromUtf8Error),

    /// ggen-core errors
    #[error("ggen-core error: {0}")]
    GgenCore(#[from] ggen_utils::error::Error),

    /// Generic errors
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

/// Result type for ggen-ai operations
pub type Result<T> = std::result::Result<T, GgenAiError>;

impl GgenAiError {
    /// Create a new LLM provider error with context
    pub fn llm_provider_with_context(
        provider: impl Into<String>,
        message: impl Into<String>,
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
    ) -> Self {
        Self::LlmProvider {
            provider: provider.into(),
            message: message.into(),
            source,
        }
    }

    /// Create a configuration error with helpful hint
    pub fn config_with_hint(message: impl Into<String>, hint: impl Into<String>) -> Self {
        Self::Configuration {
            message: message.into(),
            hint: hint.into(),
        }
    }

    /// Create a validation error with field context
    pub fn validation_error(
        field: impl Into<String>,
        message: impl Into<String>,
        value: Option<String>,
    ) -> Self {
        Self::Validation {
            field: field.into(),
            message: message.into(),
            value,
        }
    }

    /// Create a rate limit error
    pub fn rate_limit(provider: impl Into<String>, retry_after_secs: u64) -> Self {
        Self::RateLimit {
            provider: provider.into(),
            retry_after_secs,
        }
    }

    /// Check if error is retryable
    pub fn is_retryable(&self) -> bool {
        matches!(
            self,
            GgenAiError::Http { .. } |
            GgenAiError::Timeout { .. } |
            GgenAiError::RateLimit { .. }
        )
    }

    /// Get retry delay in seconds
    pub fn retry_delay(&self) -> Option<u64> {
        match self {
            GgenAiError::RateLimit { retry_after_secs, .. } => Some(*retry_after_secs),
            GgenAiError::Http { attempt, .. } => {
                // Exponential backoff: 2^attempt seconds
                Some(2u64.pow(*attempt))
            }
            _ => None,
        }
    }
}

/// Error context for wrapping errors with additional information
pub struct ErrorContext {
    pub operation: String,
    pub provider: Option<String>,
    pub metadata: std::collections::HashMap<String, String>,
}

impl ErrorContext {
    pub fn new(operation: impl Into<String>) -> Self {
        Self {
            operation: operation.into(),
            provider: None,
            metadata: std::collections::HashMap::new(),
        }
    }

    pub fn with_provider(mut self, provider: impl Into<String>) -> Self {
        self.provider = Some(provider.into());
        self
    }

    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_is_retryable() {
        let rate_limit = GgenAiError::rate_limit("openai", 30);
        assert!(rate_limit.is_retryable());

        let validation = GgenAiError::validation_error("field", "message", None);
        assert!(!validation.is_retryable());
    }

    #[test]
    fn test_retry_delay() {
        let rate_limit = GgenAiError::rate_limit("openai", 30);
        assert_eq!(rate_limit.retry_delay(), Some(30));
    }
}
```

#### 2. Retry Logic Template

**File: `ggen-ai/src/retry.rs`**

```rust
//! Retry logic for resilient API calls

use crate::error::{GgenAiError, Result};
use std::time::Duration;
use tokio::time::sleep;

/// Retry configuration
#[derive(Debug, Clone)]
pub struct RetryConfig {
    pub max_attempts: u32,
    pub initial_delay_ms: u64,
    pub max_delay_ms: u64,
    pub exponential_base: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_attempts: 3,
            initial_delay_ms: 1000,
            max_delay_ms: 30000,
            exponential_base: 2.0,
        }
    }
}

/// Retry a function with exponential backoff
pub async fn retry_with_backoff<F, Fut, T>(
    config: RetryConfig,
    mut operation: F,
) -> Result<T>
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = Result<T>>,
{
    let mut attempt = 1;

    loop {
        match operation().await {
            Ok(result) => return Ok(result),
            Err(err) if err.is_retryable() && attempt < config.max_attempts => {
                let delay_ms = if let Some(retry_after) = err.retry_delay() {
                    retry_after * 1000
                } else {
                    let delay = config.initial_delay_ms
                        * (config.exponential_base.powi(attempt as i32 - 1) as u64);
                    delay.min(config.max_delay_ms)
                };

                tracing::warn!(
                    "Attempt {}/{} failed: {}. Retrying in {}ms",
                    attempt,
                    config.max_attempts,
                    err,
                    delay_ms
                );

                sleep(Duration::from_millis(delay_ms)).await;
                attempt += 1;
            }
            Err(err) => {
                tracing::error!(
                    "Operation failed after {} attempts: {}",
                    attempt,
                    err
                );
                return Err(err);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_retry_success_on_second_attempt() {
        let mut counter = 0;
        let config = RetryConfig::default();

        let result = retry_with_backoff(config, || async {
            counter += 1;
            if counter < 2 {
                Err(GgenAiError::Timeout {
                    timeout_secs: 30,
                    operation: "test".to_string(),
                })
            } else {
                Ok(42)
            }
        })
        .await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
        assert_eq!(counter, 2);
    }
}
```

---

## Testing Strategies

### Current Testing Analysis

**Strengths:**
- ✅ Uses MockClient for unit testing
- ✅ Tests with temporary files
- ✅ Async test support with tokio-test

**Areas for Improvement:**
- ⚠️ No integration tests directory
- ⚠️ Limited test coverage for error cases
- ⚠️ No property-based testing

### Comprehensive Testing Strategy

#### 1. Test Organization Structure

```
ggen-ai/
├── tests/                    # Integration tests
│   ├── common/              # Shared test utilities
│   │   ├── mod.rs
│   │   ├── fixtures.rs      # Test fixtures
│   │   └── helpers.rs       # Test helpers
│   ├── providers_test.rs    # Provider integration tests
│   ├── generators_test.rs   # Generator integration tests
│   └── e2e_test.rs          # End-to-end tests
└── src/
    └── */tests.rs           # Unit tests (embedded)
```

#### 2. Test Fixtures Template

**File: `ggen-ai/tests/common/fixtures.rs`**

```rust
//! Test fixtures for ggen-ai tests

use ggen_ai::client::{LlmClient, LlmConfig, LlmResponse, UsageStats};
use ggen_ai::error::Result;
use async_trait::async_trait;
use std::sync::{Arc, Mutex};

/// Configurable mock client for testing
#[derive(Debug, Clone)]
pub struct TestClient {
    responses: Arc<Mutex<Vec<String>>>,
    call_count: Arc<Mutex<usize>>,
    should_fail: Arc<Mutex<bool>>,
}

impl TestClient {
    pub fn new(responses: Vec<String>) -> Self {
        Self {
            responses: Arc::new(Mutex::new(responses)),
            call_count: Arc::new(Mutex::new(0)),
            should_fail: Arc::new(Mutex::new(false)),
        }
    }

    pub fn with_failure() -> Self {
        let mut client = Self::new(vec![]);
        *client.should_fail.lock().unwrap() = true;
        client
    }

    pub fn call_count(&self) -> usize {
        *self.call_count.lock().unwrap()
    }

    pub fn add_response(&self, response: String) {
        self.responses.lock().unwrap().push(response);
    }
}

#[async_trait]
impl LlmClient for TestClient {
    async fn complete(&self, prompt: &str, _config: Option<LlmConfig>) -> Result<LlmResponse> {
        let mut count = self.call_count.lock().unwrap();
        *count += 1;

        if *self.should_fail.lock().unwrap() {
            return Err(ggen_ai::error::GgenAiError::llm_provider_with_context(
                "test",
                "Simulated failure",
                None,
            ));
        }

        let responses = self.responses.lock().unwrap();
        let response = responses.get(*count - 1)
            .cloned()
            .unwrap_or_else(|| "Default response".to_string());

        Ok(LlmResponse {
            content: response,
            usage: Some(UsageStats {
                prompt_tokens: prompt.len() as u32 / 4,
                completion_tokens: 100,
                total_tokens: (prompt.len() as u32 / 4) + 100,
            }),
            model: "test-model".to_string(),
            finish_reason: Some("stop".to_string()),
        })
    }

    async fn stream_complete(
        &self,
        _prompt: &str,
        _config: Option<LlmConfig>,
    ) -> Result<futures::stream::BoxStream<'static, Result<ggen_ai::client::LlmChunk>>> {
        unimplemented!("Use regular complete for tests")
    }

    async fn embed(&self, _text: &str) -> Result<Vec<f32>> {
        Ok(vec![0.1; 1536])
    }

    fn provider_name(&self) -> &str {
        "test"
    }

    fn supported_models(&self) -> Vec<String> {
        vec!["test-model".to_string()]
    }
}

/// Sample template fixtures
pub mod templates {
    pub const SIMPLE_TEMPLATE: &str = r#"---
to: "src/{{ name }}.rs"
vars:
  - name: "name"
    type: "string"
---
pub struct {{ name }} {
    pub id: u32,
}
"#;

    pub const REST_CONTROLLER_TEMPLATE: &str = r#"---
to: "src/controllers/{{ resource }}_controller.ts"
vars:
  - name: "resource"
    type: "string"
  - name: "methods"
    type: "array"
---
import { Request, Response } from 'express';

export class {{ resource | capitalize }}Controller {
  {{#each methods}}
  async {{ this }}(req: Request, res: Response) {
    // Implementation for {{ this }}
  }
  {{/each}}
}
"#;
}

/// Sample SPARQL fixtures
pub mod sparql {
    pub const SIMPLE_QUERY: &str = r#"
SELECT ?subject ?predicate ?object
WHERE {
  ?subject ?predicate ?object .
}
LIMIT 10
"#;
}
```

#### 3. Integration Test Template

**File: `ggen-ai/tests/generators_test.rs`**

```rust
//! Integration tests for generators

mod common;

use ggen_ai::generators::TemplateGenerator;
use ggen_ai::client::LlmConfig;
use common::fixtures::{TestClient, templates};

#[tokio::test]
async fn test_template_generator_with_custom_config() {
    let client = TestClient::new(vec![templates::SIMPLE_TEMPLATE.to_string()]);
    let config = LlmConfig {
        model: "test-model".to_string(),
        temperature: Some(0.5),
        ..Default::default()
    };

    let generator = TemplateGenerator::with_config(Box::new(client.clone()), config);

    let result = generator
        .generate_template("Create a user struct", vec!["Include id field"])
        .await;

    assert!(result.is_ok());
    assert_eq!(client.call_count(), 1);
}

#[tokio::test]
async fn test_template_generator_handles_errors() {
    let client = TestClient::with_failure();
    let generator = TemplateGenerator::new(Box::new(client));

    let result = generator
        .generate_template("This will fail", vec![])
        .await;

    assert!(result.is_err());
}

#[tokio::test]
async fn test_rest_controller_generation() {
    let client = TestClient::new(vec![templates::REST_CONTROLLER_TEMPLATE.to_string()]);
    let generator = TemplateGenerator::new(Box::new(client));

    let result = generator
        .generate_rest_controller(
            "User management API",
            "TypeScript",
            "Express",
        )
        .await;

    assert!(result.is_ok());
}
```

#### 4. Property-Based Testing Template

**File: `ggen-ai/tests/property_tests.rs`** (requires `proptest` in dev-dependencies)

```rust
//! Property-based tests for invariants

use proptest::prelude::*;
use ggen_ai::client::LlmConfig;

proptest! {
    #[test]
    fn test_llm_config_temperature_bounds(temp in 0.0f32..2.0f32) {
        let config = LlmConfig {
            temperature: Some(temp),
            ..Default::default()
        };

        // Temperature should always be in valid range
        assert!(config.temperature.unwrap() >= 0.0);
        assert!(config.temperature.unwrap() <= 2.0);
    }

    #[test]
    fn test_llm_config_top_p_bounds(top_p in 0.0f32..1.0f32) {
        let config = LlmConfig {
            top_p: Some(top_p),
            ..Default::default()
        };

        // top_p should always be in valid range
        assert!(config.top_p.unwrap() >= 0.0);
        assert!(config.top_p.unwrap() <= 1.0);
    }
}
```

---

## Code Organization

### Module Design Principles

1. **Single Responsibility**: Each module has one clear purpose
2. **Dependency Injection**: Use traits for testability
3. **Error Boundaries**: Handle errors at appropriate levels
4. **Documentation**: Every public item documented

### File Size Guidelines

- **Recommended**: 200-500 lines per file
- **Maximum**: 800 lines before splitting
- **Refactor triggers**:
  - Multiple unrelated structs
  - Complex nested logic
  - Duplicate code patterns

### Module Split Template

When a module grows too large:

```rust
// Before: large_module.rs (1000+ lines)
pub struct Thing { ... }
impl Thing { ... }
pub struct OtherThing { ... }
impl OtherThing { ... }

// After: Split into multiple files
// large_module/
// ├── mod.rs
// ├── thing.rs
// ├── other_thing.rs
// └── common.rs
```

**File: `module/mod.rs`**

```rust
mod thing;
mod other_thing;
mod common;

pub use thing::Thing;
pub use other_thing::OtherThing;
pub(crate) use common::*;
```

---

## Implementation Templates

### 1. Provider Implementation Template

**Template for adding new LLM providers:**

```rust
//! New Provider implementation

use async_trait::async_trait;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
use crate::error::{GgenAiError, Result};
use crate::config::NewProviderConfig;

/// New Provider API client
#[derive(Debug)]
pub struct NewProviderClient {
    client: Client,
    config: NewProviderConfig,
}

impl NewProviderClient {
    /// Create a new client from configuration
    pub fn from_config(config: NewProviderConfig) -> Result<Self> {
        // Validate configuration
        if config.api_key.is_none() {
            return Err(GgenAiError::config_with_hint(
                "Missing API key for NewProvider",
                "Set NEWPROVIDER_API_KEY environment variable",
            ));
        }

        Ok(Self {
            client: Client::new(),
            config,
        })
    }

    /// Create client with API key
    pub fn new(api_key: String) -> Self {
        Self {
            client: Client::new(),
            config: NewProviderConfig {
                api_key: Some(api_key),
                base_url: "https://api.newprovider.com/v1".to_string(),
                default_model: "default-model".to_string(),
                timeout_secs: 60,
            },
        }
    }
}

// Request/Response types
#[derive(Serialize)]
struct CompletionRequest {
    model: String,
    prompt: String,
    // ... other fields
}

#[derive(Deserialize)]
struct CompletionResponse {
    // ... response fields
}

#[async_trait]
impl LlmClient for NewProviderClient {
    async fn complete(&self, prompt: &str, config: Option<LlmConfig>) -> Result<LlmResponse> {
        let config = config.unwrap_or_default();

        // Build request
        let request = CompletionRequest {
            model: config.model,
            prompt: prompt.to_string(),
        };

        // Make API call with retry logic
        let response = crate::retry::retry_with_backoff(
            crate::retry::RetryConfig::default(),
            || async {
                self.client
                    .post(&format!("{}/completions", self.config.base_url))
                    .header("Authorization", format!("Bearer {}", self.config.api_key.as_ref().unwrap()))
                    .json(&request)
                    .send()
                    .await
                    .map_err(|e| GgenAiError::Http {
                        source: e,
                        attempt: 1,
                        max_attempts: 3,
                    })
            },
        )
        .await?;

        // Check status
        if !response.status().is_success() {
            let status = response.status();
            let error_text = response.text().await.unwrap_or_default();

            return Err(if status.as_u16() == 429 {
                GgenAiError::rate_limit("newprovider", 60)
            } else {
                GgenAiError::llm_provider_with_context(
                    "newprovider",
                    format!("API error ({}): {}", status, error_text),
                    None,
                )
            });
        }

        // Parse response
        let completion: CompletionResponse = response.json().await?;

        // Convert to LlmResponse
        Ok(LlmResponse {
            content: "parsed content".to_string(),
            usage: Some(UsageStats {
                prompt_tokens: 0,
                completion_tokens: 0,
                total_tokens: 0,
            }),
            model: config.model,
            finish_reason: Some("stop".to_string()),
        })
    }

    async fn stream_complete(
        &self,
        _prompt: &str,
        _config: Option<LlmConfig>,
    ) -> Result<futures::stream::BoxStream<'static, Result<LlmChunk>>> {
        // Implement streaming if supported
        Err(GgenAiError::llm_provider_with_context(
            "newprovider",
            "Streaming not yet implemented",
            None,
        ))
    }

    async fn embed(&self, _text: &str) -> Result<Vec<f32>> {
        // Implement embeddings if supported
        Err(GgenAiError::llm_provider_with_context(
            "newprovider",
            "Embeddings not supported",
            None,
        ))
    }

    fn provider_name(&self) -> &str {
        "newprovider"
    }

    fn supported_models(&self) -> Vec<String> {
        vec![
            "model-1".to_string(),
            "model-2".to_string(),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_client_creation() {
        let client = NewProviderClient::new("test-key".to_string());
        assert_eq!(client.provider_name(), "newprovider");
    }

    #[test]
    fn test_config_validation() {
        let config = NewProviderConfig {
            api_key: None,
            ..Default::default()
        };

        let result = NewProviderClient::from_config(config);
        assert!(result.is_err());
    }
}
```

### 2. Generator Implementation Template

**Template for adding new generators:**

```rust
//! New Generator implementation

use crate::client::{LlmClient, LlmConfig};
use crate::error::Result;
use crate::prompts::NewPromptBuilder;

/// New AI-powered generator
#[derive(Debug)]
pub struct NewGenerator {
    client: Box<dyn LlmClient>,
    config: LlmConfig,
}

impl NewGenerator {
    /// Create a new generator
    pub fn new(client: Box<dyn LlmClient>) -> Self {
        Self {
            client,
            config: LlmConfig::default(),
        }
    }

    /// Create with custom config
    pub fn with_config(client: Box<dyn LlmClient>, config: LlmConfig) -> Self {
        Self { client, config }
    }

    /// Main generation method
    pub async fn generate(
        &self,
        input: &str,
        options: GeneratorOptions,
    ) -> Result<GeneratedOutput> {
        // Build prompt
        let prompt = NewPromptBuilder::new(input.to_string())
            .with_options(options)
            .build()?;

        // Generate
        let response = self.client
            .complete(&prompt, Some(self.config.clone()))
            .await?;

        // Parse and validate
        self.parse_output(&response.content)
    }

    fn parse_output(&self, content: &str) -> Result<GeneratedOutput> {
        // Parsing logic
        Ok(GeneratedOutput {
            content: content.to_string(),
        })
    }
}

/// Generator options
#[derive(Debug, Clone)]
pub struct GeneratorOptions {
    pub option1: String,
    pub option2: bool,
}

/// Generated output
#[derive(Debug)]
pub struct GeneratedOutput {
    pub content: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_generator() {
        let client = Box::new(MockClient::with_response("Generated content"));
        let generator = NewGenerator::new(client);

        let options = GeneratorOptions {
            option1: "test".to_string(),
            option2: true,
        };

        let result = generator.generate("input", options).await;
        assert!(result.is_ok());
    }
}
```

---

## Summary

### Implementation Checklist

When adding new features:

- [ ] Add configuration to `config.rs`
- [ ] Update `.env.example` with new variables
- [ ] Use enhanced error types with context
- [ ] Implement retry logic for network operations
- [ ] Add unit tests with MockClient
- [ ] Add integration tests in `tests/`
- [ ] Document public API with examples
- [ ] Update configuration guide
- [ ] Add logging statements
- [ ] Validate inputs early

### Quick Reference

**Configuration:**
- Config file: `config.toml`
- Environment: `.env`
- Load: `GgenAiConfig::from_env()`

**Error Handling:**
- Use `GgenAiError` variants
- Add context with helper methods
- Check `is_retryable()` for retry logic

**Testing:**
- Unit: Embedded `#[cfg(test)]`
- Integration: `tests/` directory
- Fixtures: `tests/common/fixtures.rs`
- Mock: `TestClient` or `MockClient`

**Code Style:**
- Max 500 lines per file
- Document all public items
- Use `tracing` for logging
- Validate early, fail fast
