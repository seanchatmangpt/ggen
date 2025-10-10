<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-core Best Practices Research Report](#ggen-core-best-practices-research-report)
  - [Executive Summary](#executive-summary)
  - [1. Configuration Management](#1-configuration-management)
    - [1.1 Core Patterns](#11-core-patterns)
    - [1.2 Environment Variable Integration](#12-environment-variable-integration)
    - [1.3 Configuration Validation](#13-configuration-validation)
  - [2. Error Handling](#2-error-handling)
    - [2.1 Error Type Design](#21-error-type-design)
    - [2.2 Error Conversions](#22-error-conversions)
    - [2.3 Error Handling in API Calls](#23-error-handling-in-api-calls)
  - [3. LLM Provider Integration](#3-llm-provider-integration)
    - [3.1 Provider Trait Design](#31-provider-trait-design)
    - [3.2 Provider Implementation Structure](#32-provider-implementation-structure)
    - [3.3 HTTP Request Building](#33-http-request-building)
    - [3.4 Streaming Implementation](#34-streaming-implementation)
  - [4. Type Definitions and Traits](#4-type-definitions-and-traits)
    - [4.1 Message Type Design](#41-message-type-design)
    - [4.2 Builder Pattern for Complex Types](#42-builder-pattern-for-complex-types)
    - [4.3 Enum Design](#43-enum-design)
  - [5. Generator Patterns](#5-generator-patterns)
    - [5.1 Context Pattern](#51-context-pattern)
    - [5.2 Generator Orchestration](#52-generator-orchestration)
  - [6. Testing Patterns](#6-testing-patterns)
    - [6.1 Unit Test Structure](#61-unit-test-structure)
    - [6.2 Async Testing](#62-async-testing)
    - [6.3 Test Helpers and Fixtures](#63-test-helpers-and-fixtures)
  - [7. Module Organization](#7-module-organization)
    - [7.1 Module Structure](#71-module-structure)
    - [7.2 Documentation](#72-documentation)
  - [8. Dependency Management](#8-dependency-management)
    - [8.1 Cargo.toml Organization](#81-cargotoml-organization)
  - [9. Key Refactoring Recommendations for ggen-ai](#9-key-refactoring-recommendations-for-ggen-ai)
    - [9.1 Immediate Actions](#91-immediate-actions)
    - [9.2 Structural Improvements](#92-structural-improvements)
    - [9.3 Code Quality](#93-code-quality)
  - [10. Migration Path](#10-migration-path)
    - [Phase 1: Foundation (Week 1)](#phase-1-foundation-week-1)
    - [Phase 2: Provider Standardization (Week 2)](#phase-2-provider-standardization-week-2)
    - [Phase 3: Testing & Documentation (Week 3)](#phase-3-testing--documentation-week-3)
    - [Phase 4: Optimization (Week 4)](#phase-4-optimization-week-4)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-core Best Practices Research Report

## Executive Summary

This document provides a comprehensive analysis of `ggen-core` implementation patterns, extracted from studying the LLM integration module, configuration management, error handling, generator patterns, and testing strategies. These best practices serve as the foundation for refactoring `ggen-ai` implementations.

---

## 1. Configuration Management

### 1.1 Core Patterns

**Builder Pattern with Fluent API**
```rust
// ProviderConfig uses builder pattern (config.rs:34-68)
pub fn new(api_key: impl Into<String>) -> Self {
    Self {
        api_key: api_key.into(),
        endpoint: None,
        default_model: None,
        timeout_secs: default_timeout(),
        headers: HashMap::new(),
    }
}

// Fluent chainable methods
pub fn with_endpoint(mut self, endpoint: impl Into<String>) -> Self {
    self.endpoint = Some(endpoint.into());
    self
}
```

**Key Principles:**
- Use `impl Into<String>` for flexible string inputs
- Provide sensible defaults via functions (`default_timeout()`)
- Enable method chaining with `mut self` and returning `Self`
- Validate configuration before use

### 1.2 Environment Variable Integration

**Pattern from config.rs:145-164**
```rust
pub fn from_env() -> LlmResult<Self> {
    let mut config = Self::new();

    // OpenAI
    if let Ok(api_key) = std::env::var("OPENAI_API_KEY") {
        config = config.add_provider("openai", ProviderConfig::new(api_key))?;
    }

    // Anthropic
    if let Ok(api_key) = std::env::var("ANTHROPIC_API_KEY") {
        config = config.add_provider("anthropic", ProviderConfig::new(api_key))?;
    }

    // Default provider
    if let Ok(default) = std::env::var("LLM_DEFAULT_PROVIDER") {
        config = config.with_default_provider(default);
    }

    Ok(config)
}
```

**Best Practices:**
- ✅ Use `std::env::var()` with `if let Ok()` pattern
- ✅ Fail gracefully - missing env vars don't cause errors
- ✅ Support multiple providers via separate env vars
- ✅ Follow naming convention: `{PROVIDER}_API_KEY`
- ✅ Return `Result` type for error handling

### 1.3 Configuration Validation

**Pattern from config.rs:71-85**
```rust
pub fn validate(&self) -> LlmResult<()> {
    if self.api_key.is_empty() {
        return Err(LlmError::ConfigError {
            message: "API key cannot be empty".to_string(),
        });
    }

    if self.timeout_secs == 0 {
        return Err(LlmError::ConfigError {
            message: "Timeout must be greater than 0".to_string(),
        });
    }

    Ok(())
}
```

**Best Practices:**
- ✅ Validate before use, not just at construction
- ✅ Return early on validation failures
- ✅ Provide descriptive error messages
- ✅ Use custom error types for clarity

---

## 2. Error Handling

### 2.1 Error Type Design

**Pattern from error.rs:12-64**
```rust
/// Comprehensive error type for LLM operations
#[derive(Error, Debug)]
pub enum LlmError {
    /// API key is missing or invalid
    #[error("Invalid or missing API key for provider: {provider}")]
    InvalidApiKey { provider: String },

    /// Network or HTTP request failed
    #[error("Network request failed: {message}")]
    NetworkError { message: String },

    /// API returned an error response
    #[error("API error from {provider}: {status_code} - {message}")]
    ApiError {
        provider: String,
        status_code: u16,
        message: String,
    },

    // ... more variants
}

/// Result type alias
pub type LlmResult<T> = Result<T, LlmError>;
```

**Key Principles:**
- ✅ Use `thiserror::Error` for automatic trait implementations
- ✅ Include structured data in error variants (not just strings)
- ✅ Document each error variant with `///` comments
- ✅ Use `#[error(...)]` for display formatting with interpolation
- ✅ Create type alias `LlmResult<T>` for convenience

### 2.2 Error Conversions

**Pattern from error.rs:66-91**
```rust
impl From<reqwest::Error> for LlmError {
    fn from(err: reqwest::Error) -> Self {
        if err.is_timeout() {
            LlmError::Timeout { seconds: 30 }
        } else if err.is_status() {
            let status = err.status().unwrap();
            LlmError::ApiError {
                provider: "unknown".to_string(),
                status_code: status.as_u16(),
                message: err.to_string(),
            }
        } else {
            LlmError::NetworkError {
                message: err.to_string(),
            }
        }
    }
}
```

**Best Practices:**
- ✅ Implement `From<ExternalError>` for automatic conversions
- ✅ Map external errors to domain-specific error types
- ✅ Preserve error context (status codes, messages)
- ✅ Use pattern matching to categorize errors

### 2.3 Error Handling in API Calls

**Pattern from anthropic.rs:143-151**
```rust
if !response.status().is_success() {
    let status = response.status().as_u16();
    let error_text = response.text().await.unwrap_or_default();
    return Err(LlmError::ApiError {
        provider: "anthropic".to_string(),
        status_code: status,
        message: error_text,
    });
}
```

**Best Practices:**
- ✅ Check response status before parsing
- ✅ Extract error details from response body
- ✅ Use `unwrap_or_default()` for fallback values
- ✅ Return early with descriptive errors

---

## 3. LLM Provider Integration

### 3.1 Provider Trait Design

**Pattern from provider.rs:18-62**
```rust
#[async_trait]
pub trait LlmProvider: Send + Sync {
    /// Get the provider name
    fn name(&self) -> &str;

    /// Get supported models
    fn supported_models(&self) -> Vec<String>;

    /// Check if model is supported
    fn supports_model(&self, model: &str) -> bool {
        self.supported_models()
            .iter()
            .any(|m| m.eq_ignore_ascii_case(model))
    }

    /// Non-streaming chat
    async fn chat(&self, request: ChatRequest) -> LlmResult<ChatResponse>;

    /// Streaming chat
    async fn chat_stream(
        &self,
        request: ChatRequest,
    ) -> LlmResult<Pin<Box<dyn Stream<Item = LlmResult<StreamChunk>> + Send>>>;

    /// Validate configuration
    async fn validate(&self) -> LlmResult<()>;

    /// Get default model
    fn default_model(&self) -> &str;
}
```

**Key Principles:**
- ✅ Use `#[async_trait]` for async trait methods
- ✅ Require `Send + Sync` for multi-threaded usage
- ✅ Provide default implementations where possible
- ✅ Separate metadata methods (name, models) from operations
- ✅ Return `LlmResult` for all fallible operations

### 3.2 Provider Implementation Structure

**Pattern from anthropic.rs:23-52**
```rust
pub struct AnthropicProvider {
    client: Client,
    config: ProviderConfig,
    base_url: String,
}

impl AnthropicProvider {
    /// Create with API key only
    pub fn new(api_key: impl Into<String>) -> Self {
        Self::with_config(ProviderConfig::new(api_key))
    }

    /// Create with full configuration
    pub fn with_config(config: ProviderConfig) -> Self {
        let base_url = config
            .endpoint
            .clone()
            .unwrap_or_else(|| ANTHROPIC_API_BASE.to_string());

        let client = Client::builder()
            .timeout(Duration::from_secs(config.timeout_secs))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            client,
            config,
            base_url,
        }
    }
}
```

**Best Practices:**
- ✅ Store `reqwest::Client` for connection pooling
- ✅ Store configuration for reference
- ✅ Provide both simple (`new`) and advanced (`with_config`) constructors
- ✅ Use constants for default endpoints
- ✅ Configure client timeout from config

### 3.3 HTTP Request Building

**Pattern from anthropic.rs:54-72**
```rust
fn build_headers(&self) -> reqwest::header::HeaderMap {
    let mut headers = reqwest::header::HeaderMap::new();
    headers.insert("x-api-key", self.config.api_key.parse().unwrap());
    headers.insert("anthropic-version", ANTHROPIC_VERSION.parse().unwrap());
    headers.insert("Content-Type", "application/json".parse().unwrap());

    // Add custom headers
    for (key, value) in &self.config.headers {
        if let (Ok(k), Ok(v)) = (
            reqwest::header::HeaderName::try_from(key),
            reqwest::header::HeaderValue::try_from(value),
        ) {
            headers.insert(k, v);
        }
    }

    headers
}
```

**Best Practices:**
- ✅ Separate header building into dedicated method
- ✅ Set required headers first
- ✅ Support custom headers from configuration
- ✅ Handle header parsing errors gracefully

### 3.4 Streaming Implementation

**Pattern from anthropic.rs:157-192**
```rust
async fn chat_stream(
    &self,
    request: ChatRequest,
) -> LlmResult<Pin<Box<dyn Stream<Item = LlmResult<StreamChunk>> + Send>>> {
    let mut anthropic_request = self.to_anthropic_request(&request);
    anthropic_request.stream = Some(true);

    let url = format!("{}/messages", self.base_url);
    let response = self
        .client
        .post(&url)
        .headers(self.build_headers())
        .json(&anthropic_request)
        .send()
        .await?;

    if !response.status().is_success() {
        let status = response.status().as_u16();
        let error_text = response.text().await.unwrap_or_default();
        return Err(LlmError::ApiError {
            provider: "anthropic".to_string(),
            status_code: status,
            message: error_text,
        });
    }

    let stream = response.bytes_stream().map(|result| {
        result
            .map_err(|e| LlmError::StreamError {
                message: e.to_string(),
            })
            .and_then(parse_anthropic_sse)
    });

    Ok(Box::pin(stream))
}
```

**Best Practices:**
- ✅ Use `bytes_stream()` for efficient streaming
- ✅ Transform stream with `map` and error handling
- ✅ Parse SSE (Server-Sent Events) format
- ✅ Return pinned boxed stream
- ✅ Handle errors at stream level

---

## 4. Type Definitions and Traits

### 4.1 Message Type Design

**Pattern from types.rs:20-52**
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub role: Role,
    pub content: String,
}

impl Message {
    pub fn new(role: Role, content: impl Into<String>) -> Self {
        Self {
            role,
            content: content.into(),
        }
    }

    /// Convenience constructors
    pub fn system(content: impl Into<String>) -> Self {
        Self::new(Role::System, content)
    }

    pub fn user(content: impl Into<String>) -> Self {
        Self::new(Role::User, content)
    }

    pub fn assistant(content: impl Into<String>) -> Self {
        Self::new(Role::Assistant, content)
    }
}
```

**Best Practices:**
- ✅ Derive common traits: `Debug, Clone, Serialize, Deserialize`
- ✅ Provide convenience constructors for common patterns
- ✅ Use `impl Into<String>` for flexibility
- ✅ Public fields for struct transparency

### 4.2 Builder Pattern for Complex Types

**Pattern from types.rs:75-153**
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatRequest {
    pub model: String,
    pub messages: Vec<Message>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temperature: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_tokens: Option<u32>,
    #[serde(default)]
    pub stream: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub top_p: Option<f32>,
}

impl ChatRequest {
    pub fn builder() -> ChatRequestBuilder {
        ChatRequestBuilder::default()
    }
}

#[derive(Default)]
pub struct ChatRequestBuilder {
    model: Option<String>,
    messages: Vec<Message>,
    temperature: Option<f32>,
    max_tokens: Option<u32>,
    stream: bool,
    top_p: Option<f32>,
}

impl ChatRequestBuilder {
    pub fn model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    pub fn message(mut self, role: impl Into<Role>, content: impl Into<String>) -> Self {
        self.messages.push(Message::new(role.into(), content));
        self
    }

    pub fn build(self) -> Result<ChatRequest, String> {
        let model = self.model.ok_or("Model is required")?;
        if self.messages.is_empty() {
            return Err("At least one message is required".to_string());
        }

        Ok(ChatRequest {
            model,
            messages: self.messages,
            temperature: self.temperature,
            max_tokens: self.max_tokens,
            stream: self.stream,
            top_p: self.top_p,
        })
    }
}
```

**Best Practices:**
- ✅ Use `#[serde(skip_serializing_if = "Option::is_none")]` for optional fields
- ✅ Use `#[serde(default)]` for boolean flags
- ✅ Separate builder struct from main struct
- ✅ Validate in `build()` method
- ✅ Return descriptive `Result<T, String>` from builder

### 4.3 Enum Design

**Pattern from types.rs:8-18**
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Role {
    System,
    User,
    Assistant,
}

impl Role {
    pub fn parse_role(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "system" => Some(Role::System),
            "user" => Some(Role::User),
            "assistant" => Some(Role::Assistant),
            _ => None,
        }
    }
}
```

**Best Practices:**
- ✅ Derive `Copy` for small enums
- ✅ Use `#[serde(rename_all = "lowercase")]` for API consistency
- ✅ Implement parsing methods for string conversion
- ✅ Return `Option` for fallible parsing

---

## 5. Generator Patterns

### 5.1 Context Pattern

**Pattern from generator.rs:12-47**
```rust
pub struct GenContext {
    pub template_path: PathBuf,
    pub output_root: PathBuf,
    pub vars: BTreeMap<String, String>,
    pub global_prefixes: BTreeMap<String, String>,
    pub base: Option<String>,
    pub dry_run: bool,
}

impl GenContext {
    pub fn new(template_path: PathBuf, output_root: PathBuf) -> Self {
        Self {
            template_path,
            output_root,
            vars: BTreeMap::new(),
            global_prefixes: BTreeMap::new(),
            base: None,
            dry_run: false,
        }
    }

    pub fn with_vars(mut self, vars: BTreeMap<String, String>) -> Self {
        self.vars = vars;
        self
    }

    pub fn dry(mut self, dry: bool) -> Self {
        self.dry_run = dry;
        self
    }
}
```

**Best Practices:**
- ✅ Use `BTreeMap` for ordered key-value storage
- ✅ Use `PathBuf` for filesystem paths
- ✅ Provide builder-style methods
- ✅ Support dry-run mode for testing

### 5.2 Generator Orchestration

**Pattern from generator.rs:55-106**
```rust
pub struct Generator {
    pub pipeline: Pipeline,
    pub ctx: GenContext,
}

impl Generator {
    pub fn new(pipeline: Pipeline, ctx: GenContext) -> Self {
        Self { pipeline, ctx }
    }

    pub fn generate(&mut self) -> Result<PathBuf> {
        let input = fs::read_to_string(&self.ctx.template_path)?;
        let mut tmpl = Template::parse(&input)?;

        // Context
        let mut tctx = Context::from_serialize(&self.ctx.vars)?;
        insert_env(&mut tctx);

        // Render frontmatter
        tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;

        // Process graph
        tmpl.process_graph(
            &mut self.pipeline.graph,
            &mut self.pipeline.tera,
            &tctx,
            &self.ctx.template_path,
        )?;

        // Render body
        let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

        // Determine output path
        let output_path = if let Some(to_path) = &tmpl.front.to {
            let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
            self.ctx.output_root.join(rendered_to)
        } else {
            // Default output path
            let template_name = self
                .ctx
                .template_path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy();
            self.ctx.output_root.join(format!("{}.out", template_name))
        };

        if !self.ctx.dry_run {
            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&output_path, rendered)?;
        }

        Ok(output_path)
    }
}
```

**Best Practices:**
- ✅ Use `&mut self` for stateful operations
- ✅ Break down complex operations into steps
- ✅ Create parent directories automatically
- ✅ Respect dry-run mode
- ✅ Return generated path for verification

---

## 6. Testing Patterns

### 6.1 Unit Test Structure

**Pattern from types.rs:199-236**
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let msg = Message::user("Hello");
        assert_eq!(msg.role, Role::User);
        assert_eq!(msg.content, "Hello");
    }

    #[test]
    fn test_chat_request_builder() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Hello")
            .temperature(0.7)
            .build()
            .unwrap();

        assert_eq!(request.model, "gpt-4");
        assert_eq!(request.messages.len(), 1);
        assert_eq!(request.temperature, Some(0.7));
    }

    #[test]
    fn test_builder_validation() {
        let result = ChatRequest::builder().build();
        assert!(result.is_err());
    }
}
```

**Best Practices:**
- ✅ Use `#[cfg(test)]` for test modules
- ✅ Import with `use super::*;`
- ✅ Test positive and negative cases
- ✅ Use descriptive test names: `test_what_it_tests`
- ✅ Test builder validation separately

### 6.2 Async Testing

**Pattern from llm_integration_tests.rs:147-175**
```rust
#[tokio::test]
#[ignore] // Requires API key
async fn test_openai_chat_completion() {
    let api_key = std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set");
    let provider = OpenAiProvider::new(api_key);

    let request = ChatRequest::builder()
        .model("gpt-4o-mini")
        .message(Role::User, "Say 'test' and nothing else")
        .max_tokens(10)
        .build()
        .unwrap();

    let response = provider.chat(request).await.unwrap();
    assert!(!response.content.is_empty());
    assert_eq!(response.model, "gpt-4o-mini");
}

#[tokio::test]
#[ignore] // Requires API key
async fn test_provider_validation() {
    let api_key = std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set");
    let provider = OpenAiProvider::new(api_key);

    assert!(provider.validate().await.is_ok());

    let invalid_provider = OpenAiProvider::new("invalid-key");
    assert!(invalid_provider.validate().await.is_err());
}
```

**Best Practices:**
- ✅ Use `#[tokio::test]` for async tests
- ✅ Use `#[ignore]` for tests requiring external resources
- ✅ Document why tests are ignored
- ✅ Test both success and failure cases
- ✅ Use `expect()` with descriptive messages

### 6.3 Test Helpers and Fixtures

**Pattern from generator.rs:122-131**
```rust
fn create_test_pipeline() -> Pipeline {
    Pipeline::new().unwrap()
}

fn create_test_template(content: &str) -> (TempDir, PathBuf) {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let template_path = temp_dir.path().join("test.tmpl");
    fs::write(&template_path, content).expect("Failed to write test template");
    (temp_dir, template_path)
}
```

**Best Practices:**
- ✅ Create helper functions for common setups
- ✅ Use `tempfile::TempDir` for temporary files
- ✅ Return both temp dir and paths (prevents early cleanup)
- ✅ Use descriptive helper names

---

## 7. Module Organization

### 7.1 Module Structure

**Pattern from llm/mod.rs:34-54**
```rust
pub mod config;
pub mod error;
pub mod provider;
pub mod streaming;
pub mod types;

// Provider implementations
pub mod openai;
pub mod anthropic;

// Re-exports for convenience
pub use config::{LlmConfig, ProviderConfig};
pub use error::{LlmError, LlmResult};
pub use provider::LlmProvider;
pub use streaming::{StreamHandler, StreamChunk};
pub use types::{ChatRequest, ChatResponse, Message, Role};

// Provider re-exports
pub use anthropic::AnthropicProvider;
pub use openai::OpenAiProvider;
```

**Best Practices:**
- ✅ Organize by concern (config, error, types, etc.)
- ✅ Re-export commonly used types at module root
- ✅ Keep implementation modules separate
- ✅ Use descriptive module names

### 7.2 Documentation

**Pattern from various files:**
```rust
//! LLM integration module for multi-provider support
//!
//! This module provides a unified interface for interacting with multiple LLM providers
//! (OpenAI, Anthropic, etc.) with support for streaming responses, configuration management,
//! and robust error handling.
//!
//! # Architecture
//!
//! The module follows the adapter pattern inspired by rust-genai:
//! - `LlmProvider` trait: Unified interface for all providers
//! - Provider-specific implementations in submodules
//! - Streaming support via async/await and tokio streams
//! - Type-safe configuration with builder pattern
//!
//! # Example
//!
//! ```no_run
//! use ggen_core::llm::{LlmProvider, OpenAiProvider, ChatRequest};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let provider = OpenAiProvider::new("your-api-key");
//!     let request = ChatRequest::builder()
//!         .model("gpt-4")
//!         .message("user", "Hello!")
//!         .build()?;
//!
//!     let response = provider.chat(request).await?;
//!     println!("{}", response.content);
//!     Ok(())
//! }
//! ```
```

**Best Practices:**
- ✅ Use `//!` for module-level documentation
- ✅ Include architecture overview
- ✅ Provide usage examples
- ✅ Document public APIs with `///`
- ✅ Use `# Example` sections in docs

---

## 8. Dependency Management

### 8.1 Cargo.toml Organization

**Pattern from ggen-core/Cargo.toml:16-64**
```toml
[dependencies]
# Core utilities
anyhow = "1.0"
thiserror = "2.0"

# Async/HTTP
tokio = { version = "1.0", features = ["full"] }
reqwest = { version = "0.11", features = [
  "json",
  "rustls-tls",
  "stream",
], default-features = false }
async-trait = "0.1"
futures = "0.3"
bytes = "1.5"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
serde_yaml = "0.9"

# ... more organized sections
```

**Best Practices:**
- ✅ Group dependencies by purpose (comments)
- ✅ Specify features explicitly
- ✅ Disable default features when not needed
- ✅ Pin major versions for stability
- ✅ Use workspace dependencies for internal crates

---

## 9. Key Refactoring Recommendations for ggen-ai

### 9.1 Immediate Actions

1. **Adopt Error Handling Pattern**
   - Replace string-based errors with structured `thiserror::Error` enum
   - Create `GgenAiError` and `GgenAiResult<T>` type aliases
   - Implement `From` conversions for external error types

2. **Configuration Management**
   - Implement `ProviderConfig` pattern with builder methods
   - Add `from_env()` support for all providers
   - Add validation methods that run before operations

3. **Provider Trait Standardization**
   - Define unified `LlmProvider` trait
   - Ensure all providers implement the same interface
   - Add `validate()` methods to all providers

4. **Testing Infrastructure**
   - Add `#[cfg(test)]` modules to all files
   - Create test helpers and fixtures
   - Add integration tests with `#[ignore]` for API tests
   - Use `tempfile` for file system tests

### 9.2 Structural Improvements

1. **Module Organization**
   - Separate concerns: `config.rs`, `error.rs`, `types.rs`, `provider.rs`
   - Move provider implementations to separate modules
   - Create `mod.rs` with strategic re-exports

2. **Builder Pattern Usage**
   - Add builder pattern for complex request types
   - Implement validation in `build()` methods
   - Use fluent API with method chaining

3. **Streaming Support**
   - Implement proper `StreamChunk` and `StreamHandler` types
   - Use `Pin<Box<dyn Stream>>` for async streams
   - Handle SSE parsing uniformly

### 9.3 Code Quality

1. **Documentation**
   - Add module-level documentation (`//!`)
   - Include examples in documentation
   - Document all public APIs

2. **Type Safety**
   - Use `enum` for fixed sets of values (like `Role`)
   - Avoid string-based configuration where possible
   - Use newtypes for domain-specific strings

3. **Error Context**
   - Include provider name in all errors
   - Preserve HTTP status codes
   - Add retry-after information for rate limits

---

## 10. Migration Path

### Phase 1: Foundation (Week 1)
- [ ] Create error types module
- [ ] Implement configuration management
- [ ] Add validation infrastructure

### Phase 2: Provider Standardization (Week 2)
- [ ] Define unified provider trait
- [ ] Refactor existing providers to match
- [ ] Add streaming support

### Phase 3: Testing & Documentation (Week 3)
- [ ] Add comprehensive unit tests
- [ ] Create integration test suite
- [ ] Write module documentation

### Phase 4: Optimization (Week 4)
- [ ] Add connection pooling
- [ ] Implement caching where appropriate
- [ ] Performance testing and tuning

---

## Conclusion

The `ggen-core` codebase demonstrates excellent Rust practices:

- **Type Safety**: Extensive use of enums, newtypes, and builder patterns
- **Error Handling**: Structured errors with context and automatic conversions
- **Async/Await**: Proper use of tokio and async-trait
- **Configuration**: Flexible builder pattern with environment variable support
- **Testing**: Comprehensive test coverage with appropriate fixtures
- **Documentation**: Clear module and API documentation with examples

By adopting these patterns in `ggen-ai`, we can achieve:
- Better maintainability through clear separation of concerns
- Improved error diagnostics with structured error types
- More flexible configuration management
- Consistent provider interfaces
- Comprehensive test coverage
- Professional-grade documentation

This refactoring will align `ggen-ai` with Rust best practices and make it production-ready.
