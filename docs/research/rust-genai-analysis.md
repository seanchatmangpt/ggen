<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [rust-genai Architecture Analysis](#rust-genai-architecture-analysis)
  - [Executive Summary](#executive-summary)
  - [1. Core Architectural Patterns](#1-core-architectural-patterns)
    - [1.1 Adapter Pattern Implementation](#11-adapter-pattern-implementation)
    - [1.2 Adapter Trait Design](#12-adapter-trait-design)
  - [2. Multi-Provider Support Architecture](#2-multi-provider-support-architecture)
    - [2.1 Model Name to Adapter Resolution](#21-model-name-to-adapter-resolution)
    - [2.2 Provider-Specific Features](#22-provider-specific-features)
  - [3. Streaming Response Architecture](#3-streaming-response-architecture)
    - [3.1 Multi-Layer Stream Design](#31-multi-layer-stream-design)
    - [3.2 Captured Content Pattern](#32-captured-content-pattern)
  - [4. Configuration and Resolver System](#4-configuration-and-resolver-system)
    - [4.1 AuthResolver Pattern](#41-authresolver-pattern)
    - [4.2 ServiceTargetResolver](#42-servicetargetresolver)
  - [5. Error Handling Patterns](#5-error-handling-patterns)
    - [5.1 Comprehensive Error Types](#51-comprehensive-error-types)
    - [5.2 Graceful Degradation](#52-graceful-degradation)
  - [6. Type Safety and Configuration](#6-type-safety-and-configuration)
    - [6.1 ChatOptions Cascading](#61-chatoptions-cascading)
    - [6.2 Provider-Specific Options Mapping](#62-provider-specific-options-mapping)
  - [7. Advanced Features](#7-advanced-features)
    - [7.1 Structured Output (JSON Mode)](#71-structured-output-json-mode)
    - [7.2 Tool/Function Calling](#72-toolfunction-calling)
    - [7.3 Multi-Modal Support (Images, PDFs)](#73-multi-modal-support-images-pdfs)
    - [7.4 Embedding Support](#74-embedding-support)
  - [8. Usage Tracking and Token Counting](#8-usage-tracking-and-token-counting)
    - [8.1 Unified Usage Structure](#81-unified-usage-structure)
    - [8.2 Provider Normalization](#82-provider-normalization)
  - [9. Reasoning/Thinking Support](#9-reasoningthinking-support)
    - [9.1 Unified Reasoning API](#91-unified-reasoning-api)
    - [9.2 DeepSeek Reasoning Content Extraction](#92-deepseek-reasoning-content-extraction)
  - [10. Web Client Abstraction](#10-web-client-abstraction)
    - [10.1 WebRequestData Structure](#101-webrequestdata-structure)
    - [10.2 WebResponse Structure](#102-webresponse-structure)
  - [11. Key Integration Opportunities for ggen](#11-key-integration-opportunities-for-ggen)
    - [11.1 Adapter Pattern for MCP Tools](#111-adapter-pattern-for-mcp-tools)
    - [11.2 Streaming Generator Integration](#112-streaming-generator-integration)
    - [11.3 Configuration Resolver Pattern](#113-configuration-resolver-pattern)
    - [11.4 Error Context Pattern](#114-error-context-pattern)
    - [11.5 Usage Tracking for Generator Workflows](#115-usage-tracking-for-generator-workflows)
  - [12. Dependencies and Build Configuration](#12-dependencies-and-build-configuration)
    - [12.1 Core Dependencies](#121-core-dependencies)
    - [12.2 Lints Configuration](#122-lints-configuration)
  - [13. Advanced Patterns and Best Practices](#13-advanced-patterns-and-best-practices)
    - [13.1 value-ext JSON Helper Pattern](#131-value-ext-json-helper-pattern)
    - [13.2 Default Max Tokens Strategy](#132-default-max-tokens-strategy)
    - [13.3 MessageContent Flattening](#133-messagecontent-flattening)
  - [14. Notable Design Decisions](#14-notable-design-decisions)
    - [14.1 No SDK Dependencies](#141-no-sdk-dependencies)
    - [14.2 Fallback to Ollama](#142-fallback-to-ollama)
    - [14.3 Stream Capture Design](#143-stream-capture-design)
    - [14.4 Anthropic Cache Control](#144-anthropic-cache-control)
  - [15. Testing and Quality Patterns](#15-testing-and-quality-patterns)
    - [15.1 Example-Driven Development](#151-example-driven-development)
    - [15.2 Tracing Integration](#152-tracing-integration)
  - [16. Recommendations for ggen](#16-recommendations-for-ggen)
    - [Priority 1: Architectural Patterns (High Impact)](#priority-1-architectural-patterns-high-impact)
    - [Priority 2: Feature Enhancements (Medium Impact)](#priority-2-feature-enhancements-medium-impact)
    - [Priority 3: Developer Experience (Lower Impact, High Value)](#priority-3-developer-experience-lower-impact-high-value)
  - [17. Potential Challenges and Mitigations](#17-potential-challenges-and-mitigations)
    - [Challenge 1: Provider API Changes](#challenge-1-provider-api-changes)
    - [Challenge 2: Streaming Complexity](#challenge-2-streaming-complexity)
    - [Challenge 3: Tool Calling Variability](#challenge-3-tool-calling-variability)
    - [Challenge 4: Type Safety vs. Flexibility](#challenge-4-type-safety-vs-flexibility)
  - [18. Code Quality Observations](#18-code-quality-observations)
    - [Strengths](#strengths)
    - [Areas for ggen Learning](#areas-for-ggen-learning)
  - [19. Concrete Implementation Roadmap for ggen](#19-concrete-implementation-roadmap-for-ggen)
    - [Phase 1: Foundation (Week 1-2)](#phase-1-foundation-week-1-2)
    - [Phase 2: Configuration (Week 3)](#phase-2-configuration-week-3)
    - [Phase 3: Streaming (Week 4)](#phase-3-streaming-week-4)
    - [Phase 4: Usage Tracking (Week 5)](#phase-4-usage-tracking-week-5)
  - [20. Summary and Key Takeaways](#20-summary-and-key-takeaways)
    - [Architecture Highlights](#architecture-highlights)
    - [Innovation Points](#innovation-points)
    - [Direct Applications to ggen](#direct-applications-to-ggen)
    - [Files to Study Deeply](#files-to-study-deeply)
    - [Metrics and Scale](#metrics-and-scale)
  - [Appendix A: Provider Comparison Matrix](#appendix-a-provider-comparison-matrix)
  - [Appendix B: Example Code Snippets](#appendix-b-example-code-snippets)
    - [Multi-Provider Hello World](#multi-provider-hello-world)
    - [Custom Auth Example](#custom-auth-example)
    - [Streaming with Tool Calls](#streaming-with-tool-calls)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# rust-genai Architecture Analysis

**Research Agent Report - Comprehensive LLM Multi-Provider Library Analysis**

Date: 2025-10-10
Source: https://github.com/jeremychone/rust-genai (v0.4.2)
Scope: Multi-provider LLM abstraction patterns for ggen integration

---

## Executive Summary

rust-genai is a sophisticated native Rust library providing unified access to 13+ LLM providers through a clean adapter pattern. Key innovations include native protocol implementations (not SDK wrappers), intelligent streaming architecture, extensible resolver systems, and comprehensive type safety. The library achieves provider abstraction while preserving native features like Gemini's thinking budget and Anthropic's prompt caching.

**Supported Providers**: OpenAI, Anthropic, Gemini, Cohere, Groq, DeepSeek, xAI/Grok, Ollama, Fireworks, Together.ai, Nebius, Zhipu, plus OpenAI Responses API (gpt-5-codex)

---

## 1. Core Architectural Patterns

### 1.1 Adapter Pattern Implementation

**Central Dispatcher Pattern**:
```rust
pub struct AdapterDispatcher;

impl AdapterDispatcher {
    pub fn to_web_request_data(...) -> Result<WebRequestData> {
        match adapter_kind {
            AdapterKind::OpenAI => OpenAIAdapter::to_web_request_data(...),
            AdapterKind::Anthropic => AnthropicAdapter::to_web_request_data(...),
            AdapterKind::Gemini => GeminiAdapter::to_web_request_data(...),
            // ... 10+ more adapters
        }
    }
}
```

**Key Pattern**: Single dispatcher delegates to provider-specific adapters via match statements, ensuring compile-time exhaustiveness checking.

### 1.2 Adapter Trait Design

```rust
pub trait Adapter {
    fn default_auth() -> AuthData;
    fn default_endpoint() -> Endpoint;
    fn get_service_url(model: &ModelIden, service_type: ServiceType, endpoint: Endpoint) -> Result<String>;

    // Request transformation
    fn to_web_request_data(target: ServiceTarget, service_type: ServiceType,
                           chat_req: ChatRequest, options: ChatOptionsSet) -> Result<WebRequestData>;

    // Response transformation
    fn to_chat_response(model_iden: ModelIden, web_response: WebResponse,
                        options_set: ChatOptionsSet) -> Result<ChatResponse>;
    fn to_chat_stream(model_iden: ModelIden, reqwest_builder: RequestBuilder,
                      options_set: ChatOptionsSet) -> Result<ChatStreamResponse>;

    // Embeddings
    fn to_embed_request_data(...) -> Result<WebRequestData>;
    fn to_embed_response(...) -> Result<EmbedResponse>;
}
```

**Innovation**: Each adapter implements full lifecycle (auth → request → response → stream), enabling provider-specific optimizations while maintaining unified API.

---

## 2. Multi-Provider Support Architecture

### 2.1 Model Name to Adapter Resolution

**Static Prefix Mapping**:
```rust
impl AdapterKind {
    pub fn from_model(model: &str) -> Result<Self> {
        // Namespace support: "anthropic::claude-3" -> Anthropic
        if let (_, Some(ns)) = ModelName::model_name_and_namespace(model) {
            if let Some(adapter) = Self::from_lower_str(ns) {
                return Ok(adapter);
            }
        }

        // Prefix-based detection
        if model.starts_with("gpt") || model.starts_with("o1") || model.starts_with("o3") {
            Ok(Self::OpenAI)
        } else if model.starts_with("claude") {
            Ok(Self::Anthropic)
        } else if model.starts_with("gemini") {
            Ok(Self::Gemini)
        } else if groq::MODELS.contains(&model) {
            Ok(Self::Groq)
        } else {
            Ok(Self::Ollama) // Fallback for local models
        }
    }
}
```

**Flexibility**: Supports both automatic detection and explicit namespacing (`together::meta-llama/Llama-3`).

### 2.2 Provider-Specific Features

**Example: Anthropic Reasoning Budget**
```rust
// Model name parsing: "claude-sonnet-4-5-medium" -> model + thinking budget
let (model_name, thinking_budget) = match (raw_model_name, options_set.reasoning_effort()) {
    (model, None) => {
        if let Some((prefix, last)) = raw_model_name.rsplit_once('-') {
            let reasoning = match last {
                "zero" => None,
                "low" => Some(1024),
                "medium" => Some(8000),
                "high" => Some(24000),
                _ => None,
            };
            (if reasoning.is_some() { prefix } else { model }, reasoning)
        }
    }
    (model, Some(effort)) => {
        let effort = match effort {
            ReasoningEffort::Low => Some(1024),
            ReasoningEffort::Medium => Some(8000),
            ReasoningEffort::High => Some(24000),
            ReasoningEffort::Budget(budget) => Some(*budget),
        };
        (model, effort)
    }
};

if let Some(budget) = thinking_budget {
    payload.x_insert("thinking", json!({
        "type": "enabled",
        "budget_tokens": budget
    }))?;
}
```

**Pattern**: Adapter-specific logic handles native features while maintaining unified API surface.

---

## 3. Streaming Response Architecture

### 3.1 Multi-Layer Stream Design

```rust
// Layer 1: Provider-specific streamer
pub struct OpenAIStreamer {
    event_source: EventSource,
    model_iden: ModelIden,
    options_set: ChatOptionsSet<'_, '_>,
}

impl Stream for OpenAIStreamer {
    type Item = Result<InterStreamEvent>;  // Internal representation

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        // Parse provider SSE events → InterStreamEvent
        match event {
            "content_block_delta" => InterStreamEvent::Chunk(text),
            "thinking_delta" => InterStreamEvent::ReasoningChunk(text),
            "tool_use_delta" => InterStreamEvent::ToolCallChunk(tool_call),
            // ... provider-specific event mapping
        }
    }
}

// Layer 2: Intermediate stream abstraction
pub enum InterStreamEvent {
    Start,
    Chunk(String),
    ReasoningChunk(String),
    ToolCallChunk(ToolCall),
    End(InterStreamEnd { captured_usage, captured_content, ... }),
}

// Layer 3: Public API stream
pub struct ChatStream {
    inter_stream: Pin<Box<dyn Stream<Item = Result<InterStreamEvent>> + Send>>,
}

pub enum ChatStreamEvent {
    Start,
    Chunk(StreamChunk),
    ReasoningChunk(StreamChunk),
    ToolCallChunk(ToolChunk),
    End(StreamEnd),
}
```

**Innovation**: Three-layer architecture decouples provider SSE formats from public API, enabling provider-agnostic streaming.

### 3.2 Captured Content Pattern

```rust
pub struct StreamEnd {
    pub captured_usage: Option<Usage>,
    pub captured_content: Option<MessageContent>,
    pub captured_reasoning_content: Option<String>,
}

// Enable via options
let options = ChatOptions::default()
    .with_capture_usage(true)
    .with_capture_content(true)
    .with_capture_reasoning(true);

// Stream accumulates content automatically
while let Some(event) = stream.next().await {
    match event? {
        ChatStreamEvent::Chunk(chunk) => print!("{}", chunk.content),
        ChatStreamEvent::End(end) => {
            let full_text = end.captured_first_text(); // Complete response
            let usage = end.captured_usage; // Token counts
        }
    }
}
```

**Pattern**: Optional content capture eliminates need for manual concatenation while preserving streaming benefits.

---

## 4. Configuration and Resolver System

### 4.1 AuthResolver Pattern

```rust
pub enum AuthResolver {
    ResolverFn(Arc<Box<dyn AuthResolverFn>>),
    ResolverAsyncFn(Arc<Box<dyn AuthResolverAsyncFn>>),
}

pub trait AuthResolverFn: Send + Sync {
    fn exec_fn(&self, model_iden: ModelIden) -> Result<Option<AuthData>>;
    fn clone_box(&self) -> Box<dyn AuthResolverFn>;
}

// Usage: Custom auth per provider
let client = Client::builder()
    .with_auth_resolver(|model_iden: ModelIden| {
        match model_iden.adapter_kind {
            AdapterKind::OpenAI => Ok(Some(AuthData::from_api_key("custom-key"))),
            AdapterKind::Anthropic => Ok(Some(AuthData::from_env("CUSTOM_ANTHROPIC_KEY"))),
            _ => Ok(None) // Use defaults
        }
    })
    .build();
```

**Flexibility**: Supports sync/async resolvers, environment variables, fixed values, and dynamic functions.

### 4.2 ServiceTargetResolver

```rust
pub struct ServiceTarget {
    pub model: ModelIden,
    pub auth: AuthData,
    pub endpoint: Endpoint,
}

pub enum ServiceTargetResolver {
    ResolverFn(Arc<Box<dyn ServiceTargetResolverFn>>),
    ResolverAsyncFn(Arc<Box<dyn ServiceTargetResolverAsyncFn>>),
}

// Custom endpoint/auth/model override
let client = Client::builder()
    .with_service_target_resolver(|target: ServiceTarget| {
        Ok(ServiceTarget {
            endpoint: Endpoint::from_url("https://custom-proxy.com/v1"),
            auth: AuthData::RequestOverride {
                url: "https://aws-bedrock.com/...".into(),
                headers: custom_aws_headers(),
            },
            ..target
        })
    })
    .build();
```

**Use Case**: Enables AWS Bedrock, Vertex AI, custom proxies without SDK dependencies.

---

## 5. Error Handling Patterns

### 5.1 Comprehensive Error Types

```rust
#[derive(Debug, Display, From)]
pub enum Error {
    // Input validation
    #[display("Chat Request has no messages. (for model {model_iden})")]
    ChatReqHasNoMessages { model_iden: ModelIden },

    #[display("Role '{role}' not supported for model '{model_iden}'")]
    MessageRoleNotSupported { model_iden: ModelIden, role: ChatRole },

    // Auth errors
    #[display("Model '{model_iden}' requires an API key.")]
    RequiresApiKey { model_iden: ModelIden },

    // Web errors
    #[display("Web call failed for model '{model_iden}'.\nCause: {webc_error}")]
    WebModelCall { model_iden: ModelIden, webc_error: webc::Error },

    // Stream errors
    #[display("Failed to parse stream data for model '{model_iden}'.\nCause: {serde_error}")]
    StreamParse { model_iden: ModelIden, serde_error: serde_json::Error },

    // Feature support
    #[display("Adapter '{adapter_kind}' does not support feature '{feature}'")]
    AdapterNotSupported { adapter_kind: AdapterKind, feature: String },
}
```

**Pattern**: Rich error context with model information, using `derive_more::Display` for automatic formatting.

### 5.2 Graceful Degradation

```rust
// Usage parsing with fallback
let usage = body.x_take("usage")
    .map(|value| OpenAIAdapter::into_usage(model_iden.adapter_kind, value))
    .unwrap_or_default();

// Usage deserialization with error logging
let usage: Usage = serde_json::from_value(usage_value)
    .map_err(|err| {
        error!("Fail to deserialize usage. Cause: {err}");
        err
    })
    .unwrap_or_default();
```

**Philosophy**: Never fail response parsing due to optional metadata failures.

---

## 6. Type Safety and Configuration

### 6.1 ChatOptions Cascading

```rust
pub struct ChatOptionsSet<'req, 'cli> {
    request_options: Option<&'req ChatOptions>,
    client_options: Option<&'cli ChatOptions>,
}

impl ChatOptionsSet<'_, '_> {
    pub fn temperature(&self) -> Option<f64> {
        self.request_options.and_then(|o| o.temperature)
            .or_else(|| self.client_options.and_then(|o| o.temperature))
    }

    pub fn max_tokens(&self) -> Option<u32> {
        self.request_options.and_then(|o| o.max_tokens)
            .or_else(|| self.client_options.and_then(|o| o.max_tokens))
    }
}

// Usage
let client = Client::builder()
    .with_chat_options(ChatOptions::default().with_temperature(0.7))
    .build();

// Request-level override
client.exec_chat("gpt-4o", request,
    Some(&ChatOptions::default().with_temperature(0.9)))
```

**Pattern**: Request options override client options, with graceful fallback.

### 6.2 Provider-Specific Options Mapping

```rust
// ChatOptions → Provider Payload
if let Some(temperature) = options_set.temperature() {
    payload.x_insert("temperature", temperature)?;
}

if let Some(max_tokens) = options_set.max_tokens() {
    payload.x_insert("max_tokens", max_tokens)?; // OpenAI/Anthropic
    // or
    payload.x_insert("maxOutputTokens", max_tokens)?; // Gemini
}

if let Some(top_p) = options_set.top_p() {
    payload.x_insert("top_p", top_p)?; // OpenAI/Anthropic
    // or
    payload.x_insert("topP", top_p)?; // Gemini
}
```

**Mapping Table** (from README):

| GenAI Option  | OpenAI           | Anthropic        | Gemini                         | Cohere       |
|---------------|------------------|------------------|--------------------------------|--------------|
| temperature   | temperature      | temperature      | generationConfig.temperature   | temperature  |
| max_tokens    | max_tokens       | max_tokens       | generationConfig.maxOutputTokens| max_tokens   |
| top_p         | top_p            | top_p            | generationConfig.topP          | p            |

---

## 7. Advanced Features

### 7.1 Structured Output (JSON Mode)

```rust
pub enum ChatResponseFormat {
    JsonMode,
    JsonSpec(JsonSpec),
}

pub struct JsonSpec {
    pub name: String,
    pub schema: serde_json::Value,
}

// OpenAI implementation
let response_format = match options_set.response_format() {
    ChatResponseFormat::JsonMode => {
        json!({"type": "json_object"})
    }
    ChatResponseFormat::JsonSpec(spec) => {
        let mut schema = spec.schema.clone();
        // Auto-inject additionalProperties: false for strict mode
        schema.x_walk(|parent_map, name| {
            if name == "type" && parent_map.get("type") == Some("object") {
                parent_map.insert("additionalProperties".into(), false.into());
            }
            true
        });

        json!({
            "type": "json_schema",
            "json_schema": {
                "name": spec.name,
                "strict": true,
                "schema": schema,
            }
        })
    }
};
```

**Innovation**: Automatic schema transformation for provider-specific strict mode requirements.

### 7.2 Tool/Function Calling

**Unified Tool Representation**:
```rust
pub struct Tool {
    pub name: String,
    pub description: Option<String>,
    pub schema: serde_json::Value,
}

pub struct ToolCall {
    pub call_id: String,
    pub fn_name: String,
    pub fn_arguments: serde_json::Value,
}

pub struct ToolResponse {
    pub call_id: String,
    pub content: String,
}
```

**Provider Translation** (OpenAI):
```rust
// Request
json!({
    "type": "function",
    "function": {
        "name": tool.name,
        "description": tool.description,
        "parameters": tool.schema,
        "strict": false,
    }
})

// Response parsing
fn parse_tool_call(raw_tool_call: Value) -> Result<ToolCall> {
    #[derive(Deserialize)]
    struct InterimToolFnCall {
        id: String,
        function: InterimFunction,
    }

    #[derive(Deserialize)]
    struct InterimFunction {
        name: String,
        arguments: Value,
    }

    let interim = serde_json::from_value::<InterimToolFnCall>(raw_tool_call)?;

    Ok(ToolCall {
        call_id: interim.id,
        fn_name: interim.function.name,
        fn_arguments: match interim.function.arguments {
            Value::Object(obj) => Value::Object(obj),
            Value::String(txt) => serde_json::from_str(&txt)?, // Parse JSON string
            _ => return Err(Error::InvalidJsonResponseElement { ... }),
        },
    })
}
```

**Stream Support**: Tool calls can be streamed incrementally via `ToolCallChunk` events.

### 7.3 Multi-Modal Support (Images, PDFs)

```rust
pub enum BinarySource {
    Url(String),
    Base64(String),
}

pub struct Binary {
    pub content_type: String,
    pub name: Option<String>,
    pub source: BinarySource,
}

impl Binary {
    pub fn is_image(&self) -> bool {
        self.content_type.starts_with("image/")
    }
}

// OpenAI implementation
ContentPart::Binary(binary) => {
    if binary.is_image() {
        match &binary.source {
            BinarySource::Url(url) => {
                json!({"type": "image_url", "image_url": {"url": url}})
            }
            BinarySource::Base64(content) => {
                let image_url = format!("data:{};base64,{}", binary.content_type, content);
                json!({"type": "image_url", "image_url": {"url": image_url}})
            }
        }
    } else {
        // PDF/document support
        match &binary.source {
            BinarySource::Base64(content) => {
                json!({
                    "type": "file",
                    "file": {
                        "filename": binary.name,
                        "file_data": format!("data:{};base64,{}", binary.content_type, content)
                    }
                })
            }
        }
    }
}
```

### 7.4 Embedding Support

```rust
pub struct EmbedRequest {
    inputs: Vec<String>,
}

pub struct EmbedResponse {
    pub model_iden: ModelIden,
    pub embeddings: Vec<Vec<f64>>,
    pub usage: Option<Usage>,
}

// Client API
let response = client.embed("text-embedding-3-small", "Hello world", None).await?;
let embedding = response.embeddings.first().unwrap();

// Batch embedding
let response = client.embed_batch("text-embedding-3-small",
    vec!["text1".into(), "text2".into()], None).await?;
```

**Supported Providers**: OpenAI, Gemini, Cohere.

---

## 8. Usage Tracking and Token Counting

### 8.1 Unified Usage Structure

```rust
pub struct Usage {
    pub prompt_tokens: Option<i32>,
    pub prompt_tokens_details: Option<PromptTokensDetails>,
    pub completion_tokens: Option<i32>,
    pub completion_tokens_details: Option<CompletionTokensDetails>,
    pub total_tokens: Option<i32>,
}

pub struct PromptTokensDetails {
    pub cached_tokens: Option<i32>,
    pub cache_creation_tokens: Option<i32>,
    pub audio_tokens: Option<i32>,
}

pub struct CompletionTokensDetails {
    pub reasoning_tokens: Option<i32>,
    pub audio_tokens: Option<i32>,
}
```

### 8.2 Provider Normalization

**Anthropic** (cache tokens normalization):
```rust
// Anthropic reports input_tokens separately from cache tokens
let input_tokens: i32 = usage_value.x_take("input_tokens").unwrap_or(0);
let cache_creation_tokens: i32 = usage_value.x_take("cache_creation_input_tokens").unwrap_or(0);
let cache_read_tokens: i32 = usage_value.x_take("cache_read_input_tokens").unwrap_or(0);

// Normalize to OpenAI style: prompt_tokens includes all input
let prompt_tokens = input_tokens + cache_creation_tokens + cache_read_tokens;

Usage {
    prompt_tokens: Some(prompt_tokens),
    prompt_tokens_details: Some(PromptTokensDetails {
        cached_tokens: Some(cache_read_tokens),
        cache_creation_tokens: Some(cache_creation_tokens),
        audio_tokens: None,
    }),
    completion_tokens: Some(output_tokens),
    total_tokens: Some(prompt_tokens + output_tokens),
}
```

**xAI Bug Workaround**:
```rust
// xAI grok-3 reports reasoning_tokens separately instead of including in completion_tokens
if matches!(adapter, AdapterKind::Xai) {
    if let Some(reasoning_tokens) = usage.completion_tokens_details
        .as_ref()
        .and_then(|d| d.reasoning_tokens)
    {
        let completion_tokens = usage.completion_tokens.unwrap_or(0);
        usage.completion_tokens = Some(completion_tokens + reasoning_tokens);
    }
}
```

**Pattern**: Adapter-specific normalization ensures consistent token counting across providers.

---

## 9. Reasoning/Thinking Support

### 9.1 Unified Reasoning API

```rust
pub enum ReasoningEffort {
    Minimal,
    Low,
    Medium,
    High,
    Budget(u32), // Token budget
}

// Extract from model name or options
let (reasoning_effort, model_name) = options_set
    .reasoning_effort()
    .cloned()
    .map(|v| (Some(v), model_name))
    .unwrap_or_else(|| ReasoningEffort::from_model_name(model_name));

// OpenAI implementation
if let Some(reasoning_effort) = reasoning_effort {
    if let Some(keyword) = reasoning_effort.as_keyword() {
        payload.x_insert("reasoning_effort", keyword)?;
    }
}
```

### 9.2 DeepSeek Reasoning Content Extraction

```rust
// DeepSeek returns reasoning in <think>...</think> tags
fn extract_think(content: String) -> (String, Option<String>) {
    let start_tag = "<think>";
    let end_tag = "</think>";

    if let Some(start) = content.find(start_tag) {
        if let Some(end) = content[start + start_tag.len()..].find(end_tag) {
            let start_pos = start;
            let end_pos = start + start_tag.len() + end;

            let think_content = &content[start_pos + start_tag.len()..end_pos].trim();
            let before_think = &content[..start_pos];
            let after_think = &content[end_pos + end_tag.len()..].trim_start();

            let cleaned_content = format!("{before_think}{after_think}");

            return (cleaned_content, Some(think_content.to_string()));
        }
    }

    (content, None)
}

// Usage in response parsing
if reasoning_content.is_none() && options_set.normalize_reasoning_content() {
    let (content, reasoning) = extract_think(text_content);
    reasoning_content = reasoning;
    text_content = content;
}
```

**Innovation**: Automatic extraction of reasoning content from inline tags, normalizing DeepSeek/Ollama to native thinking APIs.

---

## 10. Web Client Abstraction

### 10.1 WebRequestData Structure

```rust
pub struct WebRequestData {
    pub url: String,
    pub headers: Headers,
    pub payload: serde_json::Value,
}

// Adapter returns this, client handles HTTP
let WebRequestData { url, headers, payload } =
    AdapterDispatcher::to_web_request_data(target, service_type, chat_req, options_set)?;

let web_response = client.web_client()
    .do_post(&url, &headers, payload)
    .await?;
```

### 10.2 WebResponse Structure

```rust
pub struct WebResponse {
    pub body: serde_json::Value,
}

// Adapter parses provider-specific JSON
let chat_response = AdapterDispatcher::to_chat_response(model_iden, web_response, options_set)?;
```

**Pattern**: Clean separation between HTTP transport and provider-specific payload/response handling.

---

## 11. Key Integration Opportunities for ggen

### 11.1 Adapter Pattern for MCP Tools

**Current ggen**: Hardcoded OpenAI client
**Enhancement**: rust-genai style adapter pattern for MCP tool providers

```rust
// Proposed architecture
pub trait McpToolAdapter {
    fn tool_to_provider_format(&self, tool: &Tool) -> serde_json::Value;
    fn parse_tool_call(&self, response: &serde_json::Value) -> Result<Vec<ToolCall>>;
    fn format_tool_response(&self, response: &ToolResponse) -> serde_json::Value;
}

// OpenAI MCP adapter
impl McpToolAdapter for OpenAIMcpAdapter {
    fn tool_to_provider_format(&self, tool: &Tool) -> serde_json::Value {
        json!({
            "type": "function",
            "function": {
                "name": tool.name,
                "description": tool.description,
                "parameters": tool.schema,
            }
        })
    }
}

// Anthropic MCP adapter
impl McpToolAdapter for AnthropicMcpAdapter {
    fn tool_to_provider_format(&self, tool: &Tool) -> serde_json::Value {
        json!({
            "name": tool.name,
            "input_schema": tool.schema,
            "description": tool.description,
        })
    }
}
```

### 11.2 Streaming Generator Integration

**rust-genai pattern**: Multi-layer stream abstraction
**ggen opportunity**: Stream MCP tool calls and generator output

```rust
pub enum GeneratorStreamEvent {
    ToolCall(McpToolCall),
    ToolResult(McpToolResult),
    ContentChunk(String),
    Usage(TokenUsage),
}

// Stream generator execution with tool calls
let mut stream = generator.execute_stream(&prompt, options).await?;
while let Some(event) = stream.next().await {
    match event? {
        GeneratorStreamEvent::ToolCall(call) => {
            // Execute MCP tool
            let result = mcp_client.call_tool(&call).await?;
            // Send back to model
            generator.send_tool_result(result).await?;
        }
        GeneratorStreamEvent::ContentChunk(chunk) => print!("{}", chunk),
        GeneratorStreamEvent::Usage(usage) => log_usage(usage),
    }
}
```

### 11.3 Configuration Resolver Pattern

**Apply to ggen**:
```rust
pub struct GeneratorConfig {
    pub provider: ProviderKind,
    pub model: String,
    pub temperature: Option<f64>,
    pub max_tokens: Option<u32>,
    pub mcp_tools: Vec<McpTool>,
}

// Custom resolver for different contexts
let generator = Generator::builder()
    .with_config_resolver(|context: GeneratorContext| {
        match context.task_type {
            TaskType::CodeGeneration => GeneratorConfig {
                provider: ProviderKind::Anthropic,
                model: "claude-sonnet-4-5".into(),
                temperature: Some(0.2),
                mcp_tools: vec![code_analysis_tool(), file_ops_tool()],
            },
            TaskType::Documentation => GeneratorConfig {
                provider: ProviderKind::OpenAI,
                model: "gpt-4o".into(),
                temperature: Some(0.7),
                mcp_tools: vec![markdown_tool(), example_tool()],
            },
        }
    })
    .build();
```

### 11.4 Error Context Pattern

**Enhance ggen errors** with rust-genai style rich context:
```rust
#[derive(Debug, Display)]
pub enum GgenError {
    #[display("Generator '{generator_id}' failed for provider '{provider}'.\nCause: {cause}")]
    GeneratorFailed {
        generator_id: String,
        provider: ProviderKind,
        cause: String,
    },

    #[display("MCP tool '{tool_name}' not found in server '{server_name}'")]
    McpToolNotFound {
        tool_name: String,
        server_name: String,
    },

    #[display("Template rendering failed for '{template_path}'.\nLine {line}: {cause}")]
    TemplateRenderFailed {
        template_path: String,
        line: usize,
        cause: String,
    },
}
```

### 11.5 Usage Tracking for Generator Workflows

```rust
pub struct GeneratorUsage {
    pub total_tokens: u32,
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub tool_execution_count: usize,
    pub cache_hits: u32,
    pub cost_estimate: f64,
}

// Track across multi-step workflows
let usage = generator.execute_with_tracking(&workflow).await?;
println!("Total cost: ${:.4}", usage.cost_estimate);
println!("Cache savings: {} tokens", usage.cache_hits);
```

---

## 12. Dependencies and Build Configuration

### 12.1 Core Dependencies

```toml
[dependencies]
# Async runtime
tokio = { version = "1", features = [
  "macros",
  "rt-multi-thread",
  "io-std",
  "test-util",
] }
futures = "0.3"
tokio-stream = "0.1"

# JSON handling
serde = { version = "1", features = [
  "derive",
  "rc",
] }
serde_json = "1"
serde_with = "3.12.0"
value-ext = "0.1.2"  # Custom JSON value extensions

# HTTP client
reqwest = { version = "0.12", default-features = false, features = [
  "json",
  "rustls-tls",
] }
reqwest-eventsource = "0.6"  # SSE streaming
eventsource-stream = "0.2"
bytes = "1.6"

# Utilities
derive_more = { version = "2", features = [
  "from",
  "display",
] }
tracing = { version = "0.1", features = [
  "default",
] }
```

**Key Choice**: `reqwest` for HTTP (not `hyper` directly) enables cleaner async API.

### 12.2 Lints Configuration

```toml
[lints.rust]
unsafe_code = "forbid"
```

**Philosophy**: Zero unsafe code, pure safe Rust implementation.

---

## 13. Advanced Patterns and Best Practices

### 13.1 value-ext JSON Helper Pattern

```rust
use value_ext::JsonValueExt;

// Navigate and extract with x_* methods
let text: String = json_value.x_take("/choices/0/message/content")?;
let usage: Value = json_value.x_take("usage")?;

// Insert with path syntax
payload.x_insert("/tools", tools_array)?;
payload.x_insert("temperature", 0.7)?;

// Walk schema trees
schema.x_walk(|parent_map, name| {
    if name == "type" && parent_map.get("type") == Some("object") {
        parent_map.insert("additionalProperties".into(), false.into());
    }
    true
});
```

**Benefit**: Eliminates verbose JSON traversal code, reduces errors.

### 13.2 Default Max Tokens Strategy

**Problem**: Different providers have different defaults and requirements
**Solution**: Adapter-specific intelligent defaults

```rust
// Anthropic: max_tokens is REQUIRED
let max_tokens = options_set.max_tokens().unwrap_or_else(|| {
    if model_name.contains("claude-sonnet-4") {
        64000  // Latest models
    } else if model_name.contains("claude-opus-4") {
        32000
    } else if model_name.contains("claude-3-5") {
        8192
    } else if model_name.contains("3-opus") || model_name.contains("3-haiku") {
        4096
    } else {
        64000  // Conservative fallback
    }
});

// OpenAI: max_tokens is optional, custom can provide default
if let Some(max_tokens) = options_set.max_tokens() {
    payload.x_insert("max_tokens", max_tokens)?;
} else if let Some(custom) = custom.as_ref() {
    if let Some(default_max) = custom.default_max_tokens {
        payload.x_insert("max_tokens", default_max)?;
    }
}
```

### 13.3 MessageContent Flattening

**Philosophy**: Simplify multi-part messages while preserving flexibility

```rust
pub struct MessageContent {
    parts: Vec<ContentPart>,
}

pub enum ContentPart {
    Text(String),
    Binary(Binary),
    ToolCall(ToolCall),
    ToolResponse(ToolResponse),
}

// Convenience methods
impl MessageContent {
    pub fn from_text(text: impl Into<String>) -> Self { ... }
    pub fn from_tool_calls(calls: Vec<ToolCall>) -> Self { ... }

    pub fn is_text_only(&self) -> bool { ... }
    pub fn joined_texts(&self) -> Option<String> { ... }
    pub fn first_text(&self) -> Option<&str> { ... }
    pub fn texts(&self) -> Vec<&str> { ... }
    pub fn tool_calls(&self) -> Vec<&ToolCall> { ... }
}
```

**Pattern**: Single unified structure with role-specific accessors.

---

## 14. Notable Design Decisions

### 14.1 No SDK Dependencies

**Decision**: Native HTTP/JSON implementation instead of provider SDKs
**Rationale**:
- APIs are similar across providers (chat completion pattern)
- SDKs add dependency bloat and version conflicts
- Lower-level control enables better normalization
- Cumulative knowledge across providers vs. per-SDK learning

**Trade-off**: Must manually track API changes, but gains flexibility.

### 14.2 Fallback to Ollama

```rust
pub fn from_model(model: &str) -> Result<Self> {
    // ... detection logic ...
    else {
        Ok(Self::Ollama) // Unknown models assume local Ollama
    }
}
```

**Rationale**: Enables using any model name with local Ollama server without requiring registration.

### 14.3 Stream Capture Design

**Decision**: Optional capture flags vs. always capturing
**Rationale**:
- Performance: Avoid unnecessary allocations for pure streaming use cases
- Flexibility: Let users opt-in to convenience features
- Memory: Large responses don't accumulate unless needed

```rust
// Minimal streaming (no capture)
while let Some(event) = stream.next().await {
    if let ChatStreamEvent::Chunk(chunk) = event? {
        print!("{}", chunk.content);
    }
}

// Convenience mode (with capture)
let options = ChatOptions::default()
    .with_capture_content(true)
    .with_capture_usage(true);

let mut stream = client.exec_chat_stream("gpt-4o", request, Some(&options)).await?;
while let Some(event) = stream.next().await {
    if let ChatStreamEvent::End(end) = event? {
        let full_text = end.captured_first_text().unwrap();
        let tokens = end.captured_usage.unwrap().total_tokens;
    }
}
```

### 14.4 Anthropic Cache Control

**Feature**: Prompt caching support via message options

```rust
let msg = ChatMessage::user("Long context...")
    .with_cache_control(true);

// Translates to Anthropic's cache control
json!({
    "role": "user",
    "content": [{
        "type": "text",
        "text": "Long context...",
        "cache_control": {"type": "ephemeral"}
    }]
})
```

**Innovation**: Unified API for provider-specific cost optimization features.

---

## 15. Testing and Quality Patterns

### 15.1 Example-Driven Development

All major features have runnable examples:
- `examples/c00-readme.rs` - Multi-provider hello world
- `examples/c01-conv.rs` - Conversation flow
- `examples/c02-auth.rs` - Custom auth
- `examples/c06-target-resolver.rs` - Custom endpoints
- `examples/c07-image.rs` - Multi-modal
- `examples/c08-tooluse.rs` - Function calling
- `examples/c10-tooluse-streaming.rs` - Streaming tool calls

**Pattern**: Examples serve as integration tests and documentation.

### 15.2 Tracing Integration

```toml
[dependencies]
tracing = { version = "0.1", features = [
  "default",
] }

[dev-dependencies]
tracing-subscriber = { version = "0.3", features = [
  "env-filter",
] }
```

**Usage**: Built-in observability for debugging provider interactions.

---

## 16. Recommendations for ggen

### Priority 1: Architectural Patterns (High Impact)

1. **Adopt Adapter Pattern for MCP Integration**
   - Create `McpAdapter` trait similar to rust-genai's `Adapter`
   - Implement provider-specific tool formatting (OpenAI, Anthropic, Gemini)
   - Centralize dispatch logic in `McpDispatcher`
   - Benefit: Support multiple LLM providers with MCP tools seamlessly

2. **Implement Resolver Pattern for Configuration**
   - Add `ConfigResolver` for context-aware generator selection
   - Add `AuthResolver` for flexible credential management
   - Add `ToolResolver` for dynamic MCP tool discovery
   - Benefit: Eliminate hardcoded configuration, enable runtime customization

3. **Multi-Layer Error Handling**
   - Rich error context with generator/provider/tool information
   - Use `derive_more::Display` for automatic formatting
   - Graceful degradation for optional features
   - Benefit: Better debugging, clearer error messages for users

### Priority 2: Feature Enhancements (Medium Impact)

4. **Streaming Architecture**
   - Multi-layer stream abstraction (provider → internal → public)
   - Optional content capture for convenience
   - Benefit: Real-time feedback for long-running generators, better UX

5. **Usage Tracking and Cost Estimation**
   - Unified `Usage` structure across providers
   - Provider-specific normalization (cache tokens, reasoning tokens)
   - Cost calculation based on provider pricing
   - Benefit: Transparency for users, optimize generator costs

6. **Provider-Agnostic Tool Calling**
   - Unified `Tool`, `ToolCall`, `ToolResponse` structures
   - Automatic translation between provider formats
   - Support streaming tool calls
   - Benefit: MCP tools work across all providers without changes

### Priority 3: Developer Experience (Lower Impact, High Value)

7. **JSON Helper Utilities**
   - Consider `value-ext` or similar for cleaner JSON manipulation
   - Path-based extraction (`x_take("/path/to/value")`)
   - Benefit: Reduce boilerplate, fewer errors in JSON handling

8. **Example-Driven Testing**
   - Add runnable examples for each generator type
   - Examples double as integration tests
   - Benefit: Living documentation, catch regressions

9. **Tracing/Observability**
   - Add `tracing` instrumentation to generator execution
   - Log provider interactions, tool calls, token usage
   - Benefit: Easier debugging, production monitoring

---

## 17. Potential Challenges and Mitigations

### Challenge 1: Provider API Changes

**Risk**: Provider APIs change, breaking adapters
**Mitigation** (from rust-genai):
- Version API endpoints in configuration
- Graceful degradation for optional fields
- Comprehensive error messages with provider context
- Automated testing against real provider APIs

### Challenge 2: Streaming Complexity

**Risk**: Different SSE formats across providers
**Mitigation**:
- Multi-layer stream architecture isolates provider differences
- Intermediate `InterStreamEvent` abstraction
- Provider-specific streamers handle parsing

### Challenge 3: Tool Calling Variability

**Risk**: Each provider has different tool calling formats
**Mitigation**:
- Unified internal representation (`Tool`, `ToolCall`)
- Adapter-specific translation in `to_web_request_data`
- Separate parsing logic in `to_chat_response`

### Challenge 4: Type Safety vs. Flexibility

**Risk**: Strongly typed Rust vs. dynamic JSON APIs
**Mitigation**:
- Use `serde_json::Value` for provider-specific parts
- Strongly typed public API surface
- Helper traits (`JsonValueExt`) for safe traversal

---

## 18. Code Quality Observations

### Strengths

1. **Modular Architecture**: Clear separation of concerns (adapters, resolvers, client, chat)
2. **Type Safety**: Extensive use of Rust's type system for compile-time guarantees
3. **Error Handling**: Comprehensive error types with rich context
4. **Documentation**: Inline comments, examples, detailed README
5. **Zero Unsafe Code**: `unsafe_code = "forbid"` lint
6. **Async First**: Native async/await, no blocking operations
7. **Extensibility**: Resolver pattern enables runtime customization without forking

### Areas for ggen Learning

1. **Dispatcher Pattern**: Single point of control for provider selection
2. **Trait-Based Adapters**: Each provider implements full lifecycle
3. **Multi-Layer Streams**: Decouples provider SSE from public API
4. **Resolver Pattern**: Flexible configuration without hardcoding
5. **Graceful Defaults**: Intelligent fallbacks (Ollama, max_tokens)
6. **Normalization**: Provider-specific quirks hidden from users
7. **Value Extension Traits**: Cleaner JSON handling

---

## 19. Concrete Implementation Roadmap for ggen

### Phase 1: Foundation (Week 1-2)

```rust
// 1. Define adapter trait for MCP tool providers
pub trait LlmAdapter {
    fn name(&self) -> &str;
    fn supports_streaming(&self) -> bool;
    fn supports_tool_calling(&self) -> bool;

    fn build_tool_request(&self, prompt: &str, tools: &[McpTool],
                          options: &GeneratorOptions) -> Result<ProviderRequest>;
    fn parse_response(&self, response: ProviderResponse) -> Result<GeneratorOutput>;
}

// 2. Implement OpenAI adapter
pub struct OpenAIAdapter;
impl LlmAdapter for OpenAIAdapter { ... }

// 3. Implement Anthropic adapter
pub struct AnthropicAdapter;
impl LlmAdapter for AnthropicAdapter { ... }

// 4. Create dispatcher
pub struct AdapterDispatcher {
    adapters: HashMap<String, Box<dyn LlmAdapter>>,
}
```

### Phase 2: Configuration (Week 3)

```rust
// 1. Add resolver pattern
pub trait ConfigResolver {
    fn resolve(&self, context: &GeneratorContext) -> GeneratorConfig;
}

// 2. Update Generator API
pub struct Generator {
    dispatcher: AdapterDispatcher,
    config_resolver: Option<Box<dyn ConfigResolver>>,
    mcp_client: McpClient,
}

impl Generator {
    pub fn with_config_resolver(mut self, resolver: impl ConfigResolver + 'static) -> Self {
        self.config_resolver = Some(Box::new(resolver));
        self
    }
}
```

### Phase 3: Streaming (Week 4)

```rust
// 1. Define stream events
pub enum GeneratorStreamEvent {
    Start,
    ContentChunk(String),
    ToolCall(McpToolCall),
    End(StreamEnd),
}

// 2. Implement streaming
impl Generator {
    pub async fn execute_stream(&self, prompt: &str) -> Result<impl Stream<Item = Result<GeneratorStreamEvent>>> {
        let adapter = self.dispatcher.get_adapter(&self.config.provider)?;
        adapter.execute_stream(prompt, &self.config.mcp_tools).await
    }
}
```

### Phase 4: Usage Tracking (Week 5)

```rust
// 1. Define usage structure
pub struct GeneratorUsage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub tool_execution_count: usize,
    pub estimated_cost_usd: f64,
}

// 2. Add to responses
pub struct GeneratorOutput {
    pub content: String,
    pub tool_results: Vec<ToolResult>,
    pub usage: GeneratorUsage,
}
```

---

## 20. Summary and Key Takeaways

### Architecture Highlights

1. **Adapter Pattern**: Dispatcher + trait-based adapters for 13+ providers
2. **Resolver Pattern**: Runtime configuration (auth, endpoints, service targets)
3. **Multi-Layer Streaming**: Provider SSE → InterStream → Public API
4. **Unified Abstractions**: MessageContent, Usage, Tool, ToolCall across providers
5. **Type Safety**: Strong typing with `Result<T>` and comprehensive error types

### Innovation Points

1. **Native Implementation**: No SDK dependencies, direct HTTP/JSON
2. **Provider Feature Preservation**: Gemini thinking budget, Anthropic caching
3. **Automatic Normalization**: Usage tokens, reasoning content, max tokens
4. **Graceful Fallbacks**: Ollama for unknown models, intelligent defaults
5. **Streaming Capture**: Optional content accumulation for convenience

### Direct Applications to ggen

1. **MCP Tool Abstraction**: Provider-agnostic tool calling via adapters
2. **Generator Configuration**: Resolver pattern for context-aware selection
3. **Streaming Generators**: Real-time output with tool call support
4. **Cost Tracking**: Unified usage tracking across providers
5. **Error Handling**: Rich context for debugging generator failures

### Files to Study Deeply

1. **`src/adapter/dispatcher.rs`** - Central dispatch pattern
2. **`src/adapter/adapters/openai/adapter_impl.rs`** - Full adapter implementation
3. **`src/adapter/adapters/anthropic/adapter_impl.rs`** - Alternative provider style
4. **`src/chat/chat_stream.rs`** - Multi-layer streaming architecture
5. **`src/resolver/service_target_resolver.rs`** - Runtime configuration pattern
6. **`src/error.rs`** - Comprehensive error handling
7. **`examples/c00-readme.rs`** - Multi-provider usage example

### Metrics and Scale

- **13 providers** supported with single API
- **4 service types**: Chat, ChatStream, Embed, (more planned)
- **3-layer streaming** architecture for provider abstraction
- **Zero unsafe code** - pure safe Rust
- **300+ LOC** per adapter average (OpenAI: ~630, Anthropic: ~625)

---

## Appendix A: Provider Comparison Matrix

| Feature                  | OpenAI | Anthropic | Gemini | Cohere | Groq | Ollama |
|--------------------------|--------|-----------|--------|--------|------|--------|
| Chat Completion          | ✅      | ✅         | ✅      | ✅      | ✅    | ✅      |
| Streaming                | ✅      | ✅         | ✅      | ✅      | ✅    | ✅      |
| Tool Calling             | ✅      | ✅         | ✅      | ✅      | ✅    | ✅      |
| Streaming Tool Calls     | ✅      | ✅         | ✅      | ❌      | ✅    | ❌      |
| Image Input              | ✅      | ✅         | ✅      | ❌      | ❌    | ✅      |
| PDF Input                | ✅      | ✅         | ❌      | ❌      | ❌    | ❌      |
| Embeddings               | ✅      | ❌         | ✅      | ✅      | ❌    | ✅      |
| Structured Output        | ✅      | ❌         | ✅      | ❌      | ❌    | ❌      |
| Reasoning Content        | ✅      | ✅         | ❌      | ❌      | ❌    | ✅*     |
| Prompt Caching           | ✅      | ✅         | ❌      | ❌      | ❌    | ❌      |
| Thinking Budget          | ✅      | ✅         | ✅      | ❌      | ❌    | ❌      |

*Via DeepSeek models running on Ollama

---

## Appendix B: Example Code Snippets

### Multi-Provider Hello World

```rust
use genai::chat::{ChatMessage, ChatRequest};
use genai::Client;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = Client::default();
    let request = ChatRequest::new(vec![
        ChatMessage::system("Be concise"),
        ChatMessage::user("Why is the sky blue?"),
    ]);

    // OpenAI
    let response = client.exec_chat("gpt-4o", request.clone(), None).await?;
    println!("OpenAI: {}", response.first_text().unwrap());

    // Anthropic
    let response = client.exec_chat("claude-sonnet-4-5", request.clone(), None).await?;
    println!("Anthropic: {}", response.first_text().unwrap());

    // Gemini
    let response = client.exec_chat("gemini-2.0-flash", request, None).await?;
    println!("Gemini: {}", response.first_text().unwrap());

    Ok(())
}
```

### Custom Auth Example

```rust
use genai::resolver::{AuthData, AuthResolver};
use genai::Client;

let client = Client::builder()
    .with_auth_resolver(AuthResolver::from_resolver_fn(|model_iden| {
        match model_iden.adapter_kind {
            AdapterKind::OpenAI => Ok(Some(AuthData::from_api_key(
                std::env::var("CUSTOM_OPENAI_KEY")?
            ))),
            AdapterKind::Anthropic => Ok(Some(AuthData::from_api_key(
                std::env::var("CUSTOM_ANTHROPIC_KEY")?
            ))),
            _ => Ok(None), // Use defaults
        }
    }))
    .build();
```

### Streaming with Tool Calls

```rust
use genai::chat::{ChatMessage, ChatRequest, ChatStreamEvent};
use genai::Client;

let client = Client::default();
let tools = vec![/* MCP tools */];

let mut stream = client.exec_chat_stream("gpt-4o",
    ChatRequest::new_with_tools(
        vec![ChatMessage::user("What's the weather?")],
        tools,
    ),
    None
).await?;

while let Some(event) = stream.next().await {
    match event? {
        ChatStreamEvent::Chunk(chunk) => print!("{}", chunk.content),
        ChatStreamEvent::ToolCallChunk(tool_chunk) => {
            println!("\nTool call: {}", tool_chunk.tool_call.fn_name);
        }
        ChatStreamEvent::End(end) => {
            println!("\n\nTokens used: {}", end.captured_usage.unwrap().total_tokens);
        }
        _ => {}
    }
}
```

---

**End of Report**

This comprehensive analysis provides actionable patterns for enhancing ggen with rust-genai's proven multi-provider architecture, streaming capabilities, and configuration flexibility.
