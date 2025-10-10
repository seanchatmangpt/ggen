# LLM Integration Architecture Design for ggen

## Executive Summary

This document presents a comprehensive architecture for integrating multi-provider LLM capabilities into ggen (Graph-aware Generator), drawing patterns from rust-genai while maintaining ggen's core philosophy of deterministic, RDF-backed code generation. The design ensures type-safety, extensibility, and seamless integration with ggen's existing template pipeline.

## 1. System Overview

### 1.1 Design Principles

1. **Provider Agnostic**: Abstract LLM interactions behind unified interfaces
2. **Type Safety**: Leverage Rust's type system for compile-time guarantees
3. **Streaming First**: Design for real-time response streaming
4. **Deterministic Integration**: Maintain ggen's deterministic generation where possible
5. **Modular Architecture**: Enable independent evolution of components
6. **RDF Integration**: Capture LLM interactions as knowledge graph triples
7. **Error Resilience**: Comprehensive error handling and fallback strategies

### 1.2 Core Components

```
┌─────────────────────────────────────────────────────────────────┐
│                        ggen CLI Layer                           │
│  (cli/src/cmds/*)                                               │
└────────────────────┬────────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────────┐
│                   Generator Pipeline                            │
│  (ggen-core/src/generator.rs)                                   │
│  • Template Processing                                          │
│  • Context Management                                           │
│  • Graph Integration                                            │
└────────────────────┬────────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────────┐
│                  LLM Integration Layer                          │
│  (ggen-core/src/llm/)                                           │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Adapter Registry      │  Configuration Manager         │  │
│  │  • Provider Discovery  │  • API Key Management          │  │
│  │  • Capability Detection│  • Model Selection             │  │
│  │  • Load Balancing      │  • Retry Policies              │  │
│  └──────────────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │            Provider-Agnostic Interface                   │  │
│  │  • ChatRequest / ChatResponse                            │  │
│  │  • StreamingAdapter                                      │  │
│  │  • ErrorStrategy                                         │  │
│  └──────────────────────────────────────────────────────────┘  │
└────────────────────┬────────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────────┐
│              Provider Implementations                           │
│  (ggen-core/src/llm/providers/)                                 │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │ OpenAI   │  │ Anthropic│  │ Ollama   │  │ Cohere   │       │
│  │ Adapter  │  │ Adapter  │  │ Adapter  │  │ Adapter  │  ...  │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘       │
└────────────────────┬────────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────────┐
│                 Graph Integration                               │
│  (ggen-core/src/graph.rs)                                       │
│  • Capture LLM requests/responses as RDF                        │
│  • Query historical interactions                                │
│  • Track model performance metrics                              │
└─────────────────────────────────────────────────────────────────┘
```

## 2. Component Architecture

### 2.1 Provider-Agnostic Interface Layer

Located in `ggen-core/src/llm/mod.rs`:

```rust
// Core trait for all LLM adapters
pub trait LlmAdapter: Send + Sync {
    /// Get adapter metadata
    fn metadata(&self) -> AdapterMetadata;

    /// Execute chat completion
    async fn chat_completion(
        &self,
        request: ChatRequest,
    ) -> Result<ChatResponse, LlmError>;

    /// Stream chat completion
    async fn chat_stream(
        &self,
        request: ChatRequest,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<ChatChunk, LlmError>> + Send>>, LlmError>;

    /// Check if adapter supports streaming
    fn supports_streaming(&self) -> bool;

    /// Get supported model list
    async fn list_models(&self) -> Result<Vec<ModelInfo>, LlmError>;
}

// Common request structure
pub struct ChatRequest {
    pub model: String,
    pub messages: Vec<ChatMessage>,
    pub temperature: Option<f32>,
    pub max_tokens: Option<u32>,
    pub top_p: Option<f32>,
    pub stream: bool,
    pub metadata: HashMap<String, String>,
}

// Common response structure
pub struct ChatResponse {
    pub content: String,
    pub model: String,
    pub usage: TokenUsage,
    pub finish_reason: FinishReason,
    pub metadata: HashMap<String, String>,
}

// Streaming chunk
pub struct ChatChunk {
    pub delta: String,
    pub finish_reason: Option<FinishReason>,
}
```

### 2.2 Configuration Management

Located in `ggen-core/src/llm/config.rs`:

```rust
// Configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmConfig {
    /// Default provider to use
    pub default_provider: String,

    /// Provider-specific configurations
    pub providers: HashMap<String, ProviderConfig>,

    /// Global retry policy
    pub retry_policy: RetryPolicy,

    /// Load balancing strategy
    pub load_balance: LoadBalanceStrategy,

    /// RDF tracking configuration
    pub rdf_tracking: RdfTrackingConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderConfig {
    pub api_key_env: String,
    pub base_url: Option<String>,
    pub default_model: String,
    pub timeout_secs: u64,
    pub max_retries: u32,
}

// Load balancing strategies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LoadBalanceStrategy {
    RoundRobin,
    LeastLatency,
    CostOptimized,
    Failover(Vec<String>), // Ordered list of providers
}
```

### 2.3 Adapter Registry

Located in `ggen-core/src/llm/registry.rs`:

```rust
pub struct AdapterRegistry {
    adapters: HashMap<String, Arc<dyn LlmAdapter>>,
    capabilities: HashMap<String, AdapterCapabilities>,
}

impl AdapterRegistry {
    /// Register a new adapter
    pub fn register(&mut self, name: String, adapter: Arc<dyn LlmAdapter>) {
        // Auto-detect capabilities
        let capabilities = self.detect_capabilities(&adapter);
        self.adapters.insert(name.clone(), adapter);
        self.capabilities.insert(name, capabilities);
    }

    /// Get adapter by name
    pub fn get(&self, name: &str) -> Option<Arc<dyn LlmAdapter>> {
        self.adapters.get(name).cloned()
    }

    /// Select best adapter based on requirements
    pub fn select_adapter(
        &self,
        requirements: AdapterRequirements,
    ) -> Option<Arc<dyn LlmAdapter>> {
        // Capability-based selection logic
    }

    /// List all available adapters
    pub fn list_adapters(&self) -> Vec<String> {
        self.adapters.keys().cloned().collect()
    }
}
```

### 2.4 Error Handling Framework

Located in `ggen-core/src/llm/error.rs`:

```rust
#[derive(Debug, thiserror::Error)]
pub enum LlmError {
    #[error("Configuration error: {0}")]
    Configuration(String),

    #[error("Authentication failed: {0}")]
    Authentication(String),

    #[error("Network error: {0}")]
    Network(#[from] reqwest::Error),

    #[error("Rate limit exceeded: retry after {retry_after:?}")]
    RateLimit { retry_after: Option<Duration> },

    #[error("Invalid model: {model}")]
    InvalidModel { model: String },

    #[error("Response parsing error: {0}")]
    Parse(String),

    #[error("Streaming error: {0}")]
    Stream(String),

    #[error("Provider error: {provider} - {message}")]
    Provider { provider: String, message: String },
}

// Error recovery strategy
pub trait ErrorStrategy: Send + Sync {
    fn should_retry(&self, error: &LlmError, attempt: u32) -> bool;
    fn backoff_duration(&self, attempt: u32) -> Duration;
}
```

## 3. Integration with ggen Pipeline

### 3.1 Template Frontmatter Extensions

Extend `Frontmatter` in `ggen-core/src/template.rs`:

```rust
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Frontmatter {
    // Existing fields...

    // LLM integration fields
    #[serde(default)]
    pub llm_generate: Option<LlmGenerateConfig>,

    #[serde(default)]
    pub llm_enhance: Option<LlmEnhanceConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmGenerateConfig {
    /// Provider to use (defaults to global config)
    pub provider: Option<String>,

    /// Model to use
    pub model: String,

    /// Prompt template
    pub prompt: String,

    /// Variable to store result
    pub output_var: String,

    /// Stream to variable
    pub stream: bool,

    /// Temperature
    pub temperature: Option<f32>,

    /// Max tokens
    pub max_tokens: Option<u32>,
}
```

Example template usage:

```yaml
---
to: "src/{{module}}/mod.rs"
llm_generate:
  provider: anthropic
  model: claude-3-5-sonnet-20241022
  prompt: |
    Generate a Rust module for {{module}} with the following functionality:
    {{description}}

    Requirements:
    - Follow Rust best practices
    - Include comprehensive documentation
    - Add unit tests
  output_var: generated_code
  stream: true
  temperature: 0.3
rdf_inline:
  - "@prefix gen: <http://ggen.dev/ns/> . gen:{{module}} gen:generatedBy 'claude-3-5-sonnet' ; gen:timestamp '{{timestamp}}' ."
---
{{generated_code}}
```

### 3.2 Generator Integration

Modify `ggen-core/src/generator.rs`:

```rust
pub struct Generator {
    pub pipeline: Pipeline,
    pub ctx: GenContext,
    pub llm_client: Option<Arc<LlmClient>>, // New field
}

impl Generator {
    pub async fn generate(&mut self) -> Result<PathBuf> {
        // Existing template parsing...

        // Process LLM generation if configured
        if let Some(llm_config) = &tmpl.front.llm_generate {
            let generated = self.process_llm_generation(llm_config, &tctx).await?;
            tctx.insert(&llm_config.output_var, &generated);

            // Track in RDF graph
            self.track_llm_interaction(&llm_config, &generated)?;
        }

        // Continue with existing rendering logic...
    }

    async fn process_llm_generation(
        &mut self,
        config: &LlmGenerateConfig,
        context: &Context,
    ) -> Result<String> {
        let llm_client = self.llm_client
            .as_ref()
            .ok_or_else(|| anyhow!("LLM client not initialized"))?;

        // Render prompt template
        let prompt = self.pipeline.tera.render_str(&config.prompt, context)?;

        // Build request
        let request = ChatRequest {
            model: config.model.clone(),
            messages: vec![ChatMessage::User(prompt)],
            temperature: config.temperature,
            max_tokens: config.max_tokens,
            stream: config.stream,
            metadata: HashMap::new(),
        };

        // Execute based on streaming preference
        if config.stream {
            self.process_streaming_generation(llm_client, request).await
        } else {
            self.process_completion(llm_client, request).await
        }
    }
}
```

### 3.3 RDF Integration

Extend `ggen-core/src/graph.rs`:

```rust
impl Graph {
    /// Insert LLM interaction metadata
    pub fn track_llm_interaction(
        &mut self,
        provider: &str,
        model: &str,
        request: &ChatRequest,
        response: &ChatResponse,
    ) -> Result<()> {
        let timestamp = chrono::Utc::now().to_rfc3339();
        let interaction_id = format!("interaction_{}", uuid::Uuid::new_v4());

        let ttl = format!(r#"
            @prefix llm: <http://ggen.dev/ns/llm/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            llm:{interaction_id} a llm:Interaction ;
                llm:provider "{provider}" ;
                llm:model "{model}" ;
                llm:timestamp "{timestamp}"^^xsd:dateTime ;
                llm:promptTokens {prompt_tokens} ;
                llm:completionTokens {completion_tokens} ;
                llm:totalTokens {total_tokens} ;
                llm:finishReason "{finish_reason}" .
        "#,
            prompt_tokens = response.usage.prompt_tokens,
            completion_tokens = response.usage.completion_tokens,
            total_tokens = response.usage.total_tokens,
            finish_reason = response.finish_reason.as_str(),
        );

        self.insert_turtle(&ttl)?;
        Ok(())
    }

    /// Query LLM interaction history
    pub fn query_llm_history(
        &self,
        provider: Option<&str>,
        since: Option<chrono::DateTime<chrono::Utc>>,
    ) -> Result<Vec<LlmInteraction>> {
        // SPARQL query to retrieve historical interactions
        let query = if let Some(provider) = provider {
            format!(r#"
                PREFIX llm: <http://ggen.dev/ns/llm/>
                SELECT ?interaction ?model ?timestamp ?tokens
                WHERE {{
                    ?interaction a llm:Interaction ;
                        llm:provider "{provider}" ;
                        llm:model ?model ;
                        llm:timestamp ?timestamp ;
                        llm:totalTokens ?tokens .
                }}
                ORDER BY DESC(?timestamp)
            "#)
        } else {
            // Query all providers
            unimplemented!()
        };

        // Execute and parse results
        unimplemented!()
    }
}
```

## 4. Provider Implementations

### 4.1 Provider Structure

Each provider implementation in `ggen-core/src/llm/providers/`:

```
llm/
├── providers/
│   ├── mod.rs           # Re-exports
│   ├── openai.rs        # OpenAI adapter
│   ├── anthropic.rs     # Anthropic adapter
│   ├── ollama.rs        # Ollama adapter
│   ├── cohere.rs        # Cohere adapter
│   └── gemini.rs        # Google Gemini adapter
```

### 4.2 Example: Anthropic Adapter

Located in `ggen-core/src/llm/providers/anthropic.rs`:

```rust
pub struct AnthropicAdapter {
    client: reqwest::Client,
    api_key: String,
    base_url: String,
}

impl AnthropicAdapter {
    pub fn new(config: ProviderConfig) -> Result<Self> {
        let api_key = std::env::var(&config.api_key_env)
            .map_err(|_| LlmError::Configuration(
                format!("Missing API key: {}", config.api_key_env)
            ))?;

        Ok(Self {
            client: reqwest::Client::builder()
                .timeout(Duration::from_secs(config.timeout_secs))
                .build()?,
            api_key,
            base_url: config.base_url
                .unwrap_or_else(|| "https://api.anthropic.com".to_string()),
        })
    }
}

#[async_trait]
impl LlmAdapter for AnthropicAdapter {
    fn metadata(&self) -> AdapterMetadata {
        AdapterMetadata {
            provider: "anthropic".to_string(),
            version: "2023-06-01".to_string(),
            supports_streaming: true,
            supports_function_calling: true,
        }
    }

    async fn chat_completion(
        &self,
        request: ChatRequest,
    ) -> Result<ChatResponse, LlmError> {
        // Convert to Anthropic format
        let anthropic_request = self.to_anthropic_format(&request)?;

        // Make API call
        let response = self.client
            .post(format!("{}/v1/messages", self.base_url))
            .header("x-api-key", &self.api_key)
            .header("anthropic-version", "2023-06-01")
            .json(&anthropic_request)
            .send()
            .await?;

        // Parse response
        self.parse_response(response).await
    }

    async fn chat_stream(
        &self,
        request: ChatRequest,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<ChatChunk, LlmError>> + Send>>, LlmError> {
        // SSE streaming implementation
        unimplemented!()
    }

    fn supports_streaming(&self) -> bool {
        true
    }

    async fn list_models(&self) -> Result<Vec<ModelInfo>, LlmError> {
        Ok(vec![
            ModelInfo {
                id: "claude-3-5-sonnet-20241022".to_string(),
                name: "Claude 3.5 Sonnet".to_string(),
                context_window: 200_000,
                max_output: 8_192,
            },
            // ... other models
        ])
    }
}
```

## 5. CLI Integration

### 5.1 New Command Structure

Add to `cli/src/cmds/mod.rs`:

```rust
#[derive(Subcommand, Debug)]
pub enum Commands {
    // Existing commands...

    /// LLM-related commands
    #[command(subcommand)]
    Llm(LlmCommands),
}

#[derive(Subcommand, Debug)]
pub enum LlmCommands {
    /// List available providers
    Providers,

    /// List models for a provider
    Models {
        #[arg(short, long)]
        provider: String,
    },

    /// Test LLM connection
    Test {
        #[arg(short, long)]
        provider: Option<String>,

        #[arg(short, long)]
        model: Option<String>,
    },

    /// Configure LLM settings
    Config {
        #[arg(short, long)]
        provider: String,

        #[arg(long)]
        api_key_env: Option<String>,

        #[arg(long)]
        default_model: Option<String>,
    },

    /// Query LLM interaction history from RDF
    History {
        #[arg(short, long)]
        provider: Option<String>,

        #[arg(long)]
        since: Option<String>,
    },
}
```

## 6. Configuration File Structure

Extend `ggen.toml` configuration:

```toml
[llm]
default_provider = "anthropic"

[llm.retry]
max_attempts = 3
backoff_ms = [1000, 2000, 5000]

[llm.load_balance]
strategy = "failover"
fallback_order = ["anthropic", "openai", "ollama"]

[llm.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
default_model = "claude-3-5-sonnet-20241022"
timeout_secs = 120
max_retries = 3

[llm.providers.openai]
api_key_env = "OPENAI_API_KEY"
default_model = "gpt-4-turbo-preview"
timeout_secs = 60
max_retries = 3

[llm.providers.ollama]
base_url = "http://localhost:11434"
default_model = "llama3.2"
timeout_secs = 300

[llm.rdf_tracking]
enabled = true
track_prompts = true
track_responses = true
track_usage = true
```

## 7. Data Flow Diagrams

### 7.1 Non-Streaming Generation Flow

```
┌──────────┐
│ Template │
│  Parser  │
└─────┬────┘
      │
      ▼
┌─────────────────┐
│   Frontmatter   │
│   Renderer      │
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ LLM Config      │◄───── Read llm_generate config
│ Processor       │
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ Adapter         │
│ Registry        │◄───── Select provider
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ Provider        │
│ Adapter         │◄───── Execute request
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ Response        │
│ Parser          │
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ RDF Tracker     │◄───── Store metadata
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ Tera Context    │◄───── Insert as variable
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ Template        │
│ Renderer        │
└─────┬───────────┘
      │
      ▼
┌─────────────────┐
│ Output File     │
└─────────────────┘
```

### 7.2 Streaming Generation Flow

```
┌──────────┐
│ Template │
│  Parser  │
└─────┬────┘
      │
      ▼
┌─────────────────┐
│ Stream          │
│ Initiator       │
└─────┬───────────┘
      │
      ▼
┌─────────────────┐     ┌──────────────┐
│ Provider        │────▶│ SSE Stream   │
│ Adapter         │     └──────┬───────┘
└─────────────────┘            │
                               │ Chunks
                               ▼
                    ┌──────────────────┐
                    │ Stream Processor │
                    └──────┬───────────┘
                           │
                           ▼
                    ┌──────────────────┐
                    │ Accumulator      │
                    └──────┬───────────┘
                           │
                           ▼
                    ┌──────────────────┐
                    │ RDF Tracker      │
                    └──────┬───────────┘
                           │
                           ▼
                    ┌──────────────────┐
                    │ Template Context │
                    └──────┬───────────┘
                           │
                           ▼
                    ┌──────────────────┐
                    │ Output File      │
                    └──────────────────┘
```

## 8. Migration Strategy

### 8.1 Phase 1: Foundation (Weeks 1-2)

1. Create `ggen-core/src/llm/` module structure
2. Implement core traits and error types
3. Add configuration structures
4. Update `Cargo.toml` dependencies

**Dependencies to add:**
```toml
[dependencies]
# HTTP client
reqwest = { version = "0.12", features = ["json", "stream"] }
# Async runtime (already in workspace)
tokio = { version = "1.0", features = ["full"] }
# Streaming
futures = "0.3"
async-stream = "0.3"
# Error handling
thiserror = "1.0"
anyhow = "1.0"
# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
# UUID generation
uuid = { version = "1.0", features = ["v4"] }
```

### 8.2 Phase 2: Provider Implementation (Weeks 3-4)

1. Implement Anthropic adapter (primary)
2. Implement OpenAI adapter
3. Implement Ollama adapter (local model support)
4. Add adapter registry and configuration loading

### 8.3 Phase 3: Pipeline Integration (Weeks 5-6)

1. Extend `Frontmatter` structure
2. Modify `Generator` to support LLM operations
3. Implement streaming support
4. Add RDF tracking functionality

### 8.4 Phase 4: CLI and Tooling (Week 7)

1. Add `ggen llm` subcommands
2. Implement configuration commands
3. Add testing and validation tools
4. Create example templates

### 8.5 Phase 5: Testing and Documentation (Week 8)

1. Unit tests for all adapters
2. Integration tests with mock providers
3. End-to-end tests with real LLM calls
4. Comprehensive documentation
5. Migration guide from manual LLM integration

## 9. Testing Strategy

### 9.1 Unit Tests

Located in each module's `tests` submodule:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adapter_registry_registration() {
        let mut registry = AdapterRegistry::new();
        let adapter = Arc::new(MockAdapter::new());
        registry.register("mock".to_string(), adapter);
        assert!(registry.get("mock").is_some());
    }

    #[tokio::test]
    async fn test_anthropic_chat_completion() {
        // Mock server test
    }
}
```

### 9.2 Integration Tests

Located in `ggen-core/tests/llm_integration.rs`:

```rust
#[tokio::test]
async fn test_end_to_end_generation() {
    // Test complete flow from template to output
}

#[tokio::test]
async fn test_streaming_generation() {
    // Test streaming flow
}

#[tokio::test]
async fn test_rdf_tracking() {
    // Test RDF metadata capture
}
```

### 9.3 Mock Provider

Create `ggen-core/src/llm/providers/mock.rs` for testing:

```rust
pub struct MockAdapter {
    responses: Vec<ChatResponse>,
    current_index: AtomicUsize,
}

impl MockAdapter {
    pub fn with_responses(responses: Vec<ChatResponse>) -> Self {
        Self {
            responses,
            current_index: AtomicUsize::new(0),
        }
    }
}

#[async_trait]
impl LlmAdapter for MockAdapter {
    async fn chat_completion(
        &self,
        _request: ChatRequest,
    ) -> Result<ChatResponse, LlmError> {
        let index = self.current_index.fetch_add(1, Ordering::SeqCst);
        Ok(self.responses[index % self.responses.len()].clone())
    }

    // ... other trait methods
}
```

## 10. Performance Considerations

### 10.1 Caching Strategy

Implement response caching to reduce API calls:

```rust
pub struct CachedAdapter<T: LlmAdapter> {
    inner: T,
    cache: Arc<RwLock<LruCache<RequestHash, ChatResponse>>>,
}

impl<T: LlmAdapter> CachedAdapter<T> {
    pub fn new(inner: T, cache_size: usize) -> Self {
        Self {
            inner,
            cache: Arc::new(RwLock::new(LruCache::new(cache_size))),
        }
    }
}
```

### 10.2 Connection Pooling

Reuse HTTP connections via `reqwest::Client` pooling.

### 10.3 Async Execution

Leverage Tokio for concurrent LLM requests when processing multiple templates.

## 11. Security Considerations

### 11.1 API Key Management

1. **Never hardcode API keys** - always use environment variables
2. **Support secret management systems** - integrate with vault solutions
3. **Validate permissions** - check API key permissions before operations
4. **Rotation support** - handle API key rotation gracefully

### 11.2 Prompt Injection Protection

1. **Input sanitization** - escape user-provided template variables
2. **Prompt templates** - use structured prompts with clear boundaries
3. **Content filtering** - validate LLM responses before insertion

### 11.3 Rate Limiting

1. **Client-side rate limiting** - respect provider rate limits
2. **Backoff strategies** - exponential backoff on rate limit errors
3. **Queue management** - queue requests when approaching limits

## 12. Observability and Monitoring

### 12.1 Metrics Collection

Track via RDF and optional metrics backend:

- Request count per provider/model
- Success/failure rates
- Token usage statistics
- Latency measurements
- Cost tracking

### 12.2 Logging Strategy

```rust
use tracing::{info, warn, error, debug};

impl Generator {
    async fn process_llm_generation(&mut self, config: &LlmGenerateConfig) -> Result<String> {
        info!(
            provider = %config.provider.as_deref().unwrap_or("default"),
            model = %config.model,
            "Starting LLM generation"
        );

        let start = std::time::Instant::now();

        match self.execute_llm_request(config).await {
            Ok(response) => {
                let duration = start.elapsed();
                info!(
                    duration_ms = duration.as_millis(),
                    tokens = response.usage.total_tokens,
                    "LLM generation completed"
                );
                Ok(response.content)
            }
            Err(e) => {
                error!(error = %e, "LLM generation failed");
                Err(e)
            }
        }
    }
}
```

## 13. Extension Points

### 13.1 Custom Adapters

Users can implement custom adapters:

```rust
// In user code
struct CustomAdapter;

#[async_trait]
impl LlmAdapter for CustomAdapter {
    // Implementation
}

// Register in configuration
fn main() {
    let mut registry = AdapterRegistry::new();
    registry.register(
        "custom".to_string(),
        Arc::new(CustomAdapter::new())
    );
}
```

### 13.2 Middleware Pattern

Support adapter middleware for cross-cutting concerns:

```rust
pub trait AdapterMiddleware: Send + Sync {
    async fn before_request(&self, request: &mut ChatRequest) -> Result<(), LlmError>;
    async fn after_response(&self, response: &mut ChatResponse) -> Result<(), LlmError>;
}

pub struct MiddlewareAdapter<T: LlmAdapter> {
    inner: T,
    middleware: Vec<Box<dyn AdapterMiddleware>>,
}
```

## 14. API Specifications

### 14.1 Frontmatter API

Complete `llm_generate` configuration options:

```yaml
llm_generate:
  # Provider selection
  provider: anthropic  # Optional, defaults to config
  model: claude-3-5-sonnet-20241022  # Required

  # Prompt configuration
  prompt: |  # Required
    Your prompt here with {{template_vars}}

  # Output configuration
  output_var: result  # Required - variable name for result
  stream: false  # Optional, default false

  # Model parameters
  temperature: 0.7  # Optional, 0.0-1.0
  max_tokens: 4096  # Optional
  top_p: 0.9  # Optional

  # Error handling
  retry_on_error: true  # Optional, default true
  fallback_providers: [openai, ollama]  # Optional

  # Caching
  cache_key: "{{module}}_generation"  # Optional
  cache_ttl_seconds: 3600  # Optional
```

### 14.2 RDF Vocabulary

LLM interaction ontology:

```turtle
@prefix llm: <http://ggen.dev/ns/llm/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

llm:Interaction a rdfs:Class ;
    rdfs:label "LLM Interaction" ;
    rdfs:comment "Represents a single interaction with an LLM provider" .

llm:provider a rdf:Property ;
    rdfs:domain llm:Interaction ;
    rdfs:range xsd:string .

llm:model a rdf:Property ;
    rdfs:domain llm:Interaction ;
    rdfs:range xsd:string .

llm:timestamp a rdf:Property ;
    rdfs:domain llm:Interaction ;
    rdfs:range xsd:dateTime .

llm:promptTokens a rdf:Property ;
    rdfs:domain llm:Interaction ;
    rdfs:range xsd:integer .

llm:completionTokens a rdf:Property ;
    rdfs:domain llm:Interaction ;
    rdfs:range xsd:integer .

llm:totalTokens a rdf:Property ;
    rdfs:domain llm:Interaction ;
    rdfs:range xsd:integer .
```

## 15. Future Enhancements

### 15.1 Multi-Modal Support

Extend adapters to support:
- Image generation
- Vision/image understanding
- Audio generation/transcription
- Embeddings generation

### 15.2 Advanced Features

- **Function calling**: Support tool/function calling for providers that offer it
- **Semantic caching**: Use embeddings for semantic cache lookup
- **Cost optimization**: Automatic model selection based on cost/quality tradeoffs
- **A/B testing**: Compare outputs from multiple models
- **Fine-tuning integration**: Support for fine-tuned models

### 15.3 Distributed Generation

- **Worker pools**: Distribute LLM calls across multiple workers
- **Queue systems**: Integration with job queues for batch processing
- **Result aggregation**: Combine results from multiple LLM calls

## 16. Conclusion

This architecture provides a robust, extensible foundation for integrating LLM capabilities into ggen while maintaining its core philosophy of deterministic, graph-aware code generation. The design prioritizes:

1. **Type safety** through Rust's strong type system
2. **Extensibility** via trait-based abstractions
3. **Observability** through RDF tracking and structured logging
4. **Reliability** with comprehensive error handling and retry logic
5. **Performance** via streaming, caching, and async execution

The phased implementation approach ensures incremental value delivery while maintaining backward compatibility with existing ggen functionality.

## Appendix A: File Structure

```
ggen-core/src/
├── llm/
│   ├── mod.rs              # Public API and core traits
│   ├── error.rs            # Error types and strategies
│   ├── config.rs           # Configuration structures
│   ├── registry.rs         # Adapter registry
│   ├── client.rs           # High-level LLM client
│   ├── cache.rs            # Response caching
│   ├── middleware.rs       # Middleware support
│   ├── streaming.rs        # Streaming utilities
│   └── providers/
│       ├── mod.rs          # Provider re-exports
│       ├── anthropic.rs    # Anthropic implementation
│       ├── openai.rs       # OpenAI implementation
│       ├── ollama.rs       # Ollama implementation
│       ├── cohere.rs       # Cohere implementation
│       ├── gemini.rs       # Google Gemini implementation
│       └── mock.rs         # Mock provider for testing
├── generator.rs            # Updated with LLM support
├── template.rs             # Updated frontmatter
└── graph.rs                # Updated with LLM tracking

cli/src/cmds/
├── llm.rs                  # LLM CLI commands
└── mod.rs                  # Updated command enum

tests/
└── llm_integration.rs      # Integration tests
```

## Appendix B: Example Templates

### B.1 Code Generation Template

```yaml
---
to: "src/{{module}}/{{name | snake_case}}.rs"
llm_generate:
  model: claude-3-5-sonnet-20241022
  prompt: |
    Generate a Rust module implementing {{description}}.

    Module name: {{module}}::{{name}}
    Requirements:
    {{requirements}}

    Include:
    - Comprehensive documentation
    - Error handling with Result types
    - Unit tests with #[cfg(test)]
    - Examples in doc comments
  output_var: rust_code
  temperature: 0.3
  max_tokens: 4096
rdf_inline:
  - "@prefix gen: <http://ggen.dev/ns/> . gen:{{module}}_{{name}} a gen:GeneratedModule ; gen:timestamp '{{timestamp}}' ; gen:llmModel 'claude-3-5-sonnet-20241022' ."
---
{{rust_code}}
```

### B.2 Documentation Enhancement Template

```yaml
---
to: "docs/{{topic}}.md"
llm_enhance:
  model: claude-3-5-sonnet-20241022
  prompt: |
    Enhance the following documentation with:
    - Clear examples
    - Best practices
    - Common pitfalls
    - Related concepts

    Original content:
    {{original_content}}
  output_var: enhanced_docs
---
{{enhanced_docs}}
```

## Appendix C: Configuration Examples

### C.1 Minimal Configuration

```toml
[llm]
default_provider = "anthropic"

[llm.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
default_model = "claude-3-5-sonnet-20241022"
```

### C.2 Production Configuration

```toml
[llm]
default_provider = "anthropic"

[llm.retry]
max_attempts = 5
backoff_ms = [1000, 2000, 4000, 8000, 16000]

[llm.load_balance]
strategy = "failover"
fallback_order = ["anthropic", "openai", "ollama"]

[llm.cache]
enabled = true
max_entries = 1000
ttl_seconds = 3600

[llm.providers.anthropic]
api_key_env = "ANTHROPIC_API_KEY"
default_model = "claude-3-5-sonnet-20241022"
timeout_secs = 180
max_retries = 3

[llm.providers.openai]
api_key_env = "OPENAI_API_KEY"
default_model = "gpt-4-turbo-preview"
timeout_secs = 120
max_retries = 3

[llm.providers.ollama]
base_url = "http://localhost:11434"
default_model = "llama3.2"
timeout_secs = 300

[llm.rdf_tracking]
enabled = true
track_prompts = true
track_responses = false  # Don't track full responses for privacy
track_usage = true
track_latency = true
```

---

**Document Version**: 1.0
**Last Updated**: 2025-10-10
**Author**: Feature Architecture Agent
**Status**: Draft for Review
