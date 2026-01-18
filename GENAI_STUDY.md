# Study: genai Package & DSPy Predictor Integration

## Overview

The `genai` crate is a **unified multi-provider LLM client** that abstracts different AI providers (OpenAI, Anthropic, Ollama, etc.) under a single interface.

**ggen-ai** wraps genai in three layers:

```
┌─────────────────────────────────────────┐
│ DSPy Layer (dspy/*.rs)                  │
│ - Signature (task definition)           │
│ - Predictor (LLM executor) ← NEEDS FIX  │
│ - ChainOfThought (reasoning)            │
│ - Module trait (composable units)       │
└────────────────────┬────────────────────┘
                     │
┌────────────────────▼────────────────────┐
│ Client Layer (client.rs)                │
│ - LlmClient trait (abstract interface)  │
│ - GenAiClient (wraps genai::Client)     │
│ - LlmConfig (parameters + validation)   │
│ - LlmResponse/LlmChunk (typed output)   │
└────────────────────┬────────────────────┘
                     │
┌────────────────────▼────────────────────┐
│ genai::Client (external crate 0.4)      │
│ - ChatRequest/ChatMessage               │
│ - ChatOptions (temp, tokens, etc)       │
│ - exec_chat() → sync requests           │
│ - exec_chat_stream() → streaming        │
└─────────────────────────────────────────┘
```

---

## Key Classes & Concepts

### 1. **genai::Client** (External Crate)

The lowest-level abstraction. Provides:

```rust
use genai::{Client, chat::{ChatMessage, ChatRequest, ChatOptions}};

let client = Client::default(); // Auto-detects provider from env

// Single request
let req = ChatRequest::new(vec![ChatMessage::user("Explain Rust")]);
let opts = ChatOptions::default()
    .with_temperature(0.7)
    .with_max_tokens(4096);

let response = client
    .exec_chat("gpt-3.5-turbo", req, Some(&opts))
    .await?;

// Get the content
let text = response.first_text().unwrap_or_default();

// Streaming
let stream = client.exec_chat_stream("gpt-3.5-turbo", req, Some(&opts)).await?;
// Stream emits ChatStreamEvent::Chunk, ChatStreamEvent::End, etc
```

**Key Methods**:
- `exec_chat()` - Synchronous request
- `exec_chat_stream()` - Streaming request
- Auto-detects provider from `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, etc

---

### 2. **LlmConfig** (ggen-ai/src/client.rs)

Type-safe configuration for LLM requests:

```rust
pub struct LlmConfig {
    pub model: String,                           // "gpt-3.5-turbo"
    pub max_tokens: Option<u32>,                 // 4096
    pub temperature: Option<f32>,                // 0.7
    pub top_p: Option<f32>,                      // 0.9
    pub stop: Option<Vec<String>>,               // ["<END>"]
    pub extra: HashMap<String, serde_json::Value>, // custom fields
}
```

**Constructor patterns**:

```rust
// Default from environment variables
let config = LlmConfig::default();
// Looks for: GGEN_DEFAULT_MODEL, GGEN_LLM_TEMPERATURE, etc

// Explicit
let config = LlmConfig {
    model: "gpt-4".to_string(),
    temperature: Some(0.7),
    max_tokens: Some(2048),
    ..Default::default()
};

// Validation
config.validate()?; // Ensures model is set, temp in [0, 2], etc
```

---

### 3. **LlmClient Trait** (ggen-ai/src/client.rs)

Abstract interface over any LLM provider:

```rust
#[async_trait]
pub trait LlmClient: Send + Sync + std::fmt::Debug {
    /// Synchronous completion
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;

    /// Streaming completion
    async fn complete_stream(&self, prompt: &str)
        -> Result<BoxStream<'static, LlmChunk>>;

    /// Get current configuration
    fn get_config(&self) -> &LlmConfig;

    /// Update configuration
    fn update_config(&mut self, config: LlmConfig);
}
```

**Implementations**:
- `GenAiClient` - Uses genai::Client (real LLMs)
- `MockClient` - Returns pre-defined responses (testing)

---

### 4. **GenAiClient** (ggen-ai/src/client.rs)

The main implementation wrapping genai:

```rust
pub struct GenAiClient {
    client: Client,          // genai::Client
    config: LlmConfig,
}

impl GenAiClient {
    pub fn new(config: LlmConfig) -> Result<Self> {
        config.validate()?;
        Ok(Self {
            client: Client::default(), // Auto-detects provider
            config,
        })
    }

    fn create_chat_options(&self) -> ChatOptions {
        ChatOptions::default()
            .with_temperature(self.config.temperature.unwrap_or(0.7) as f64)
            .with_max_tokens(self.config.max_tokens.unwrap_or(4096))
            .with_top_p(self.config.top_p.unwrap_or(0.9) as f64)
    }
}

#[async_trait]
impl LlmClient for GenAiClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        let chat_req = ChatRequest::new(vec![ChatMessage::user(prompt)]);
        let chat_options = self.create_chat_options();

        let response = self
            .client
            .exec_chat(&self.config.model, chat_req, Some(&chat_options))
            .await
            .map_err(|e| GgenAiError::llm_provider(
                "GenAI",
                format!("Request failed: {}", e)
            ))?;

        Ok(LlmResponse {
            content: response.first_text().unwrap_or_default().to_string(),
            usage: Some(UsageStats {
                prompt_tokens: response.usage.prompt_tokens.unwrap_or(0) as u32,
                completion_tokens: response.usage.completion_tokens.unwrap_or(0) as u32,
                total_tokens: response.usage.total_tokens.unwrap_or(0) as u32,
            }),
            model: self.config.model.clone(),
            finish_reason: Some("stop".to_string()),
            extra: HashMap::new(),
        })
    }

    async fn complete_stream(&self, prompt: &str)
        -> Result<BoxStream<'static, LlmChunk>> {
        let chat_req = ChatRequest::new(vec![ChatMessage::user(prompt)]);
        let chat_options = self.create_chat_options();

        let stream = self
            .client
            .exec_chat_stream(&self.config.model, chat_req, Some(&chat_options))
            .await?;

        let model = self.config.model.clone();
        let stream = stream.stream.map(move |chunk_result| {
            match chunk_result {
                Ok(genai::chat::ChatStreamEvent::Chunk(chunk)) => LlmChunk {
                    content: chunk.content,
                    model: model.clone(),
                    finish_reason: None,
                    usage: None,
                    extra: HashMap::new(),
                },
                Ok(genai::chat::ChatStreamEvent::End(end)) => LlmChunk {
                    content: String::new(),
                    model: model.clone(),
                    finish_reason: Some("stop".to_string()),
                    usage: end.captured_usage.map(|u| UsageStats {
                        prompt_tokens: u.prompt_tokens.unwrap_or(0) as u32,
                        completion_tokens: u.completion_tokens.unwrap_or(0) as u32,
                        total_tokens: u.total_tokens.unwrap_or(0) as u32,
                    }),
                    extra: HashMap::new(),
                },
                // ... handle other events
                _ => { /* default handling */ }
            }
        });

        Ok(Box::pin(stream))
    }
}
```

**Key Points**:
- `GenAiClient::new()` validates config
- Creates `genai::Client::default()` (auto-detects provider from env)
- `complete()` wraps sync request
- `complete_stream()` wraps streaming with event handling

---

### 5. **Provider Configuration** (ggen-ai/src/config/global.rs)

Auto-detects and configures the right provider:

```rust
pub enum LlmProvider {
    OpenAI,    // Needs OPENAI_API_KEY
    Anthropic, // Needs ANTHROPIC_API_KEY
    Ollama,    // Needs local Ollama running on http://localhost:11434
    Mock,      // For testing
}

impl Default for GlobalLlmConfig {
    fn default() -> Self {
        let mut providers = HashMap::new();

        // Configure each provider
        providers.insert(LlmProvider::OpenAI, LlmConfig {
            model: "gpt-3.5-turbo".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            ..
        });

        providers.insert(LlmProvider::Anthropic, LlmConfig {
            model: "claude-haiku-4-5-20251001".to_string(),
            max_tokens: Some(4096),
            ..
        });

        // etc for Ollama, Mock

        Self {
            provider: Self::detect_available_provider(), // Auto-detect
            providers,
            settings: GlobalSettings { .. }
        }
    }
}

impl GlobalLlmConfig {
    fn detect_available_provider() -> LlmProvider {
        // 1. Check GGEN_LLM_PROVIDER env var
        // 2. Default to Ollama (local, no auth needed)
        // 3. Check for API keys at runtime when used
    }
}
```

---

## How Predictor Should Use This

### Current (Broken)

```rust
// dspy/predictor.rs lines 121-137
async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
    self.validate_inputs(&inputs)?;
    let _prompt = self.build_prompt(&inputs)?;  // ← Built but UNUSED

    // TODO: Call actual LLM
    let response = format!(
        "Mock response from {} with temperature {}",
        self.llm_provider, self.temperature
    );

    self.parse_output(&response)
}
```

**Problems**:
1. `build_prompt()` result is discarded (`_prompt`)
2. Never creates LLM client
3. Returns mock response
4. `llm_provider` is just a string, not typed

---

### Fixed Version

```rust
use std::sync::Arc;
use crate::client::LlmClient;

pub struct Predictor {
    signature: Signature,
    llm_provider: String,
    temperature: f32,
    client: Arc<dyn LlmClient>,  // ← ADD THIS
}

impl Predictor {
    pub fn new(signature: Signature, client: Arc<dyn LlmClient>) -> Self {
        Self {
            signature,
            llm_provider: client.get_config().model.clone(),
            temperature: 0.7,
            client,
        }
    }

    pub fn with_provider(mut self, provider: impl Into<String>) -> Self {
        self.llm_provider = provider.into();
        self
    }

    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 1.0);
        self
    }
}

#[async_trait::async_trait]
impl Module for Predictor {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self,
        inputs: HashMap<String, Value>
    ) -> ModuleResult<HashMap<String, Value>> {
        // 1. Validate inputs
        self.validate_inputs(&inputs)?;

        // 2. Build prompt
        let prompt = self.build_prompt(&inputs)?;

        // 3. Create LLM config with temperature override
        let mut config = self.client.get_config().clone();
        config.temperature = Some(self.temperature);

        // 4. Call actual LLM
        let llm_response = self
            .client
            .complete(&prompt)
            .await
            .map_err(|e| ModuleError::execution(
                format!("LLM call failed: {}", e)
            ))?;

        // 5. Parse and validate output
        let outputs = self.parse_output(&llm_response.content)?;

        // 6. Validate outputs against signature
        crate::dspy::signature_validator::SignatureValidator::new(&self.signature)
            .validate_output(&outputs)
            .map_err(|e| ModuleError::validation(
                format!("Output validation failed: {}", e)
            ))?;

        Ok(outputs)
    }
}
```

---

## Integration Flow

```
User Input (English)
    │
    ▼
Signature::new("TaskName", "description")
    .with_input(InputField::new("input1", "...", "String"))
    .with_output(OutputField::new("output1", "...", "String"))
    │
    ▼
Predictor::new(signature, llm_client)
    .with_temperature(0.7)
    │
    ▼
predictor.forward(inputs).await
    ├─> build_prompt() → "Input: ...\n\nOutput: "
    │
    ├─> llm_client.complete(prompt).await
    │   ├─> GenAiClient::complete()
    │   │   ├─> ChatRequest::new()
    │   │   ├─> genai::Client::exec_chat()
    │   │   └─> LlmResponse { content: "...", usage, ... }
    │   │
    │   └─> or streaming with llm_client.complete_stream()
    │
    ├─> parse_output(content) → HashMap<String, Value>
    │
    ├─> SignatureValidator::validate_output()
    │   └─> Check JSON schema compliance
    │
    └─> Return HashMap<String, Value>
```

---

## Usage Examples

### Example 1: Simple QA (Sync)

```rust
use ggen_ai::client::{GenAiClient, LlmConfig};
use ggen_ai::dspy::{Signature, InputField, OutputField, Module};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Create LLM client
    let config = LlmConfig::default();
    let client = std::sync::Arc::new(GenAiClient::new(config)?);

    // 2. Define task
    let sig = Signature::new("QA", "Question answering")
        .with_input(InputField::new("question", "The question", "String"))
        .with_output(OutputField::new("answer", "The answer", "String"));

    // 3. Create predictor
    let predictor = ggen_ai::dspy::Predictor::new(sig, client);

    // 4. Execute
    let mut inputs = HashMap::new();
    inputs.insert("question".to_string(), Value::String("What is Rust?".into()));

    let outputs = predictor.forward(inputs).await?;
    println!("Answer: {}", outputs["answer"]);

    Ok(())
}
```

### Example 2: Template Generation (What TemplateGenerator Should Do)

```rust
let sig = Signature::new("TemplateGen", "Generate Tera template from description")
    .with_input(InputField::new("description", "What to generate", "String"))
    .with_input(InputField::new("language", "Target language", "String"))
    .with_output(OutputField::new("template", "Tera template code", "String"))
    .with_output(OutputField::new("frontmatter", "Template metadata", "String"));

let predictor = Predictor::new(sig, client)
    .with_temperature(0.8); // Higher for more creative templates

let mut inputs = HashMap::new();
inputs.insert("description".to_string(),
    Value::String("REST API controller for User CRUD".into()));
inputs.insert("language".to_string(),
    Value::String("rust".into()));

let outputs = predictor.forward(inputs).await?;
// outputs["template"] = valid Tera template
// outputs["frontmatter"] = metadata JSON
```

### Example 3: Chain of Thought (Reasoning)

```rust
let sig = Signature::new("Reasoning", "Explain step-by-step");
let cot = ChainOfThought::new(sig);  // Auto-adds reasoning instruction

// Same as predictor but with reasoning step injection:
// "Think through this step-by-step before providing your answer."

cot.predictor().forward(inputs).await?;
```

---

## Provider Detection (How genai Chooses)

When you call `Client::default()`:

```
1. Check environment variables:
   - OPENAI_API_KEY → Use OpenAI (GPT models)
   - ANTHROPIC_API_KEY → Use Anthropic (Claude models)
   - OLLAMA_ENDPOINT → Use Ollama (local models)

2. If no env vars, default to:
   - OLLAMA on http://localhost:11434 (requires running locally)

3. At call time:
   - genai::Client::exec_chat("model-name", ...)
   - Looks up model in provider registry
   - Routes to correct provider

Example:
   export ANTHROPIC_API_KEY="sk-ant-..."
   let client = Client::default();
   client.exec_chat("claude-opus-4-5", req, opts).await ✓
```

---

## Key Takeaways for Fixing Predictor

1. **Inject LlmClient**: Add `client: Arc<dyn LlmClient>` field to Predictor
2. **Use build_prompt() result**: Don't discard it
3. **Call complete()**: Use `self.client.complete(&prompt).await?`
4. **Validate outputs**: Use `SignatureValidator` on parsed response
5. **Handle errors**: Return `ModuleError` on LLM/validation failures
6. **Support temperature**: Update config.temperature before calling

---

## Files to Study (Priority Order)

| File | Why | Lines |
|------|-----|-------|
| `src/client.rs` | The LlmClient trait and GenAiClient implementation | 186-292 |
| `src/config/global.rs` | Provider detection and configuration | 52-180 |
| `src/dspy/predictor.rs` | The broken Predictor that needs fixing | 121-137 |
| `src/dspy/signature.rs` | How tasks are defined | Full |
| `src/dspy/signature_validator.rs` | How outputs are validated | Full |
| `src/constants.rs` | Configuration constants and env vars | Full |
| `src/providers/adapter.rs` | Mock client pattern for testing | 40-94 |

---

## Testing Strategy

For testing without real LLM:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_predictor_with_mock() {
        let mock = std::sync::Arc::new(
            MockClient::with_response("answer: 42")
        );

        let sig = Signature::new("QA", "...")
            .with_input(InputField::new("question", "...", "String"))
            .with_output(OutputField::new("answer", "...", "String"));

        let pred = Predictor::new(sig, mock);

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What?".into()));

        let result = pred.forward(inputs).await;
        assert!(result.is_ok());
    }
}
```

---

## Summary

| Component | Status | What It Does |
|-----------|--------|--------------|
| **genai** (external) | ✅ Working | Multi-provider LLM abstraction |
| **GenAiClient** | ✅ Working | Wraps genai, validates config, handles streams |
| **LlmClient trait** | ✅ Working | Abstract interface, mockable |
| **Config system** | ✅ Working | Env-based provider detection |
| **Predictor** | 🔴 Broken | Needs LlmClient injection + actual call |
| **SignatureValidator** | ✅ Working | Validates output matches signature |
| **TemplateGenerator** | 🔴 Missing | Needs implementation |
| **CLI integration** | 🔴 Missing | No `ggen init --wizard` command |
