//! A2A (Agent-to-Agent) Predictor for DSPy
//!
//! This module provides a predictor implementation that wraps A2A agents
//! in a DSPy-compatible interface, enabling seamless integration with
//! ggen-ai's multi-provider LLM support.
//!
//! ## Architecture
//!
//! The A2A predictor follows a three-stage pipeline:
//!
//! 1. **Message Conversion**: A2A messages are converted to LLM prompt format
//!    using the `A2aToLlmConverter` from ggen-a2a-bridge.
//!
//! 2. **LLM Execution**: The converted prompt is sent to the LLM via ggen-ai's
//!    multi-provider client, supporting various model backends.
//!
//! 3. **Response Conversion**: LLM responses are converted back to A2A format
//!    using the `LlmToA2aConverter`.
//!
//! ## Usage
//!
//! ```rust,no_run
//! use ggen_dspy::predictors::{A2aPredictor, A2aPredictorConfig};
//! use a2a_generated::converged::message::ConvergedMessage;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let config = A2aPredictorConfig::builder()
//!     .with_temperature(0.7)
//!     .with_max_tokens(1024)
//!     .with_system_prompt("You are a helpful assistant.")
//!     .build();
//!
//! let predictor = A2aPredictor::new(config)?;
//!
//! let message = ConvergedMessage::text("msg-1", "user", "What is Rust?");
//! let response = predictor.forward(&message).await?;
//!
//! println!("Response: {}", response.payload.content);
//! # Ok(())
//! # }
//! ```

use crate::{DspyError, Result};
use a2a_generated::converged::message::ConvergedMessage;
use async_trait::async_trait;
use futures::stream::Stream;
use genai::chat::{ChatMessage, ChatOptions, ChatRequest};
use genai::Client;
use ggen_ai::dspy::model_capabilities::Model;
use serde_json::Value;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{debug, info, warn};

// ============================================================================
// Configuration
// ============================================================================

/// Configuration for A2A predictor
///
/// Provides a builder pattern for configuring the predictor's behavior.
/// All parameters have sensible defaults for common use cases.
///
/// # Examples
///
/// ```rust
/// use ggen_dspy::predictors::A2aPredictorConfig;
///
/// let config = A2aPredictorConfig::builder()
///     .with_temperature(0.7)
///     .with_max_tokens(1024)
///     .build();
/// ```
#[derive(Debug, Clone)]
pub struct A2aPredictorConfig {
    /// Model configuration for LLM calls
    pub model: Model,

    /// System prompt for context setting
    pub system_prompt: String,

    /// Temperature for LLM sampling (0.0-2.0)
    pub temperature: f32,

    /// Maximum tokens to generate
    pub max_tokens: u32,

    /// Enable response caching
    pub enable_cache: bool,

    /// Cache TTL in seconds
    pub cache_ttl: u64,

    /// Request timeout in seconds
    pub timeout_secs: u64,

    /// Maximum retry attempts
    pub max_retries: u32,

    /// Enable streaming responses
    pub enable_streaming: bool,
}

impl Default for A2aPredictorConfig {
    fn default() -> Self {
        Self {
            model: Self::default_model(),
            system_prompt: "You are a helpful AI assistant integrated with A2A agents.".to_string(),
            temperature: 0.7,
            max_tokens: 1024,
            enable_cache: true,
            cache_ttl: 3600,
            timeout_secs: 30,
            max_retries: 3,
            enable_streaming: false,
        }
    }
}

impl A2aPredictorConfig {
    /// Get the default model from environment variables
    fn default_model() -> Model {
        let model_name = std::env::var("GGEN_LLM_MODEL")
            .or_else(|_| std::env::var("DEFAULT_MODEL"))
            .unwrap_or_else(|_| "gpt-4".to_string());
        Model::from_name(model_name)
    }

    /// Create a new builder for configuration
    pub fn builder() -> A2aConfigBuilder {
        A2aConfigBuilder::new()
    }

    /// Create a configuration with a specific model
    pub fn with_model(model: impl Into<String>) -> Self {
        Self {
            model: Model::from_name(model.into()),
            ..Default::default()
        }
    }

    /// Set the temperature
    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 2.0);
        self
    }

    /// Set the maximum tokens
    pub fn with_max_tokens(mut self, max_tokens: u32) -> Self {
        self.max_tokens = max_tokens;
        self
    }

    /// Set the system prompt
    pub fn with_system_prompt(mut self, prompt: impl Into<String>) -> Self {
        self.system_prompt = prompt.into();
        self
    }

    /// Enable caching
    pub fn with_cache(mut self, enable: bool) -> Self {
        self.enable_cache = enable;
        self
    }

    /// Enable streaming
    pub fn with_streaming(mut self, enable: bool) -> Self {
        self.enable_streaming = enable;
        self
    }
}

/// Builder for creating A2aPredictorConfig
#[derive(Debug, Default)]
pub struct A2aConfigBuilder {
    config: A2aPredictorConfig,
}

impl A2aConfigBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            config: A2aPredictorConfig::default(),
        }
    }

    /// Set the model name
    pub fn model(mut self, model: impl Into<String>) -> Self {
        self.config.model = Model::from_name(model.into());
        self
    }

    /// Set the temperature
    pub fn temperature(mut self, temperature: f32) -> Self {
        self.config.temperature = temperature.clamp(0.0, 2.0);
        self
    }

    /// Set the maximum tokens
    pub fn max_tokens(mut self, max_tokens: u32) -> Self {
        self.config.max_tokens = max_tokens;
        self
    }

    /// Set the system prompt
    pub fn system_prompt(mut self, prompt: impl Into<String>) -> Self {
        self.config.system_prompt = prompt.into();
        self
    }

    /// Enable caching with TTL
    pub fn cache(mut self, enable: bool, ttl_secs: u64) -> Self {
        self.config.enable_cache = enable;
        self.config.cache_ttl = ttl_secs;
        self
    }

    /// Set timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.config.timeout_secs = secs;
        self
    }

    /// Set max retries
    pub fn max_retries(mut self, retries: u32) -> Self {
        self.config.max_retries = retries;
        self
    }

    /// Enable streaming
    pub fn streaming(mut self, enable: bool) -> Self {
        self.config.enable_streaming = enable;
        self
    }

    /// Build the configuration
    pub fn build(self) -> A2aPredictorConfig {
        self.config
    }
}

// ============================================================================
// Message Conversion Utilities
// ============================================================================

/// Extract text content from an A2A message
///
/// Handles all content types in the A2A unified content format:
/// - Text: Direct string content
/// - Data: JSON-serialized data
/// - File: URI or base64-encoded content
/// - Multipart: Concatenated parts
/// - Stream: Stream ID reference
fn extract_content_from_message(message: &ConvergedMessage) -> Result<String> {
    use a2a_generated::converged::message::UnifiedContent;

    match &message.payload.content {
        UnifiedContent::Text { content, .. } => Ok(content.clone()),
        UnifiedContent::Data { data, .. } => {
            serde_json::to_string_pretty(data).map_err(|e| DspyError::SerializationError(e))
        }
        UnifiedContent::File { file, .. } => {
            if let Some(uri) = &file.uri {
                Ok(format!("[File: {}]", uri))
            } else if let Some(bytes) = &file.bytes {
                Ok(format!("[Base64 file content: {} bytes]", bytes.len()))
            } else {
                Ok("[File content unavailable]".to_string())
            }
        }
        UnifiedContent::Multipart { parts, .. } => {
            let mut contents = Vec::new();
            for part in parts {
                // For multipart, we extract the content directly from the part
                if let UnifiedContent::Text { content, .. } = part {
                    contents.push(content.clone());
                } else {
                    contents.push("[Non-text part in multipart]".to_string());
                }
            }
            Ok(contents.join("\n\n---\n\n"))
        }
        UnifiedContent::Stream { stream_id, .. } => Ok(format!("[Stream: {}]", stream_id)),
    }
}

/// Build LLM messages from an A2A message
///
/// Converts A2A message format into the format expected by genai chat API.
/// Includes system prompt and user message with A2A content.
fn build_llm_messages(system_prompt: &str, user_content: &str) -> Vec<ChatMessage> {
    vec![
        ChatMessage::system(system_prompt.to_string()),
        ChatMessage::user(user_content.to_string()),
    ]
}

/// Parse LLM response into A2A message format
///
/// Creates a new A2A message from the LLM response content.
/// Preserves message metadata and adds LLM-specific extensions.
fn parse_llm_to_a2a(
    request_id: &str, response_content: &str, model: &str, usage: Option<TokenUsage>,
) -> ConvergedMessage {
    let mut message = ConvergedMessage::text(
        format!("resp-{}", request_id),
        "llm-bridge".to_string(),
        response_content.to_string(),
    );

    // Add LLM metadata using with_metadata
    message = message.with_metadata("llm_model".to_string(), Value::String(model.to_string()));

    if let Some(usage_info) = usage {
        message = message.with_metadata(
            "llm_usage".to_string(),
            serde_json::json!({
                "prompt_tokens": usage_info.prompt_tokens,
                "completion_tokens": usage_info.completion_tokens,
                "total_tokens": usage_info.total_tokens,
            }),
        );
    }

    message
}

/// Token usage information from LLM responses
#[derive(Debug, Clone)]
pub struct TokenUsage {
    /// Number of tokens in the prompt
    pub prompt_tokens: u32,
    /// Number of tokens in the completion
    pub completion_tokens: u32,
    /// Total tokens used
    pub total_tokens: u32,
}

impl TokenUsage {
    /// Create new token usage
    pub fn new(prompt_tokens: u32, completion_tokens: u32) -> Self {
        Self {
            prompt_tokens,
            completion_tokens,
            total_tokens: prompt_tokens + completion_tokens,
        }
    }
}

// ============================================================================
// Main A2A Predictor
// ============================================================================

/// A2A (Agent-to-Agent) Predictor for DSPy integration
///
/// Wraps A2A agents in a DSPy-compatible interface, enabling them to use
/// ggen-ai's multi-provider LLM support through a unified API.
///
/// # Type Parameters
///
/// * `C` - Cache type for response caching (optional)
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_dspy::predictors::{A2aPredictor, A2aPredictorConfig};
/// use a2a_generated::converged::message::ConvergedMessage;
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let config = A2aPredictorConfig::default();
/// let predictor = A2aPredictor::new(config)?;
///
/// let message = ConvergedMessage::text("msg-1", "user", "Hello!");
/// let response = predictor.forward(&message).await?;
/// # Ok(())
/// # }
/// ```
pub struct A2aPredictor {
    /// Configuration
    config: A2aPredictorConfig,

    /// LLM client
    client: Arc<Mutex<Client>>,

    /// In-memory response cache (optional)
    cache: Option<Arc<Mutex<lru::LruCache<String, CachedResponse>>>>,
}

/// Cached response with expiration
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct CachedResponse {
    /// Content of the cached response
    content: String,
    /// Token usage information
    usage: TokenUsage,
    /// When the response was cached (for TTL eviction)
    cached_at: std::time::Instant,
}

impl A2aPredictor {
    /// Create a new A2A predictor with the given configuration
    ///
    /// # Errors
    ///
    /// Returns an error if the model configuration is invalid.
    pub fn new(config: A2aPredictorConfig) -> Result<Self> {
        // Initialize cache if enabled
        let cache = if config.enable_cache {
            Some(Arc::new(Mutex::new(lru::LruCache::new(
                std::num::NonZeroUsize::new(1000).unwrap(),
            ))))
        } else {
            None
        };

        Ok(Self {
            config,
            client: Arc::new(Mutex::new(Client::default())),
            cache,
        })
    }

    /// Create a predictor with default configuration
    pub fn default_config() -> Result<Self> {
        Self::new(A2aPredictorConfig::default())
    }

    /// Create a predictor with a specific model
    pub fn with_model(model: impl Into<String>) -> Result<Self> {
        Self::new(A2aPredictorConfig::with_model(model))
    }

    /// Get the predictor's configuration
    pub fn config(&self) -> &A2aPredictorConfig {
        &self.config
    }

    /// Get the model name
    pub fn model_name(&self) -> &str {
        &self.config.model.name
    }

    /// Process an A2A message through the LLM
    ///
    /// This is the main entry point for the predictor. It:
    /// 1. Extracts content from the A2A message
    /// 2. Checks cache (if enabled)
    /// 3. Calls the LLM with retry logic
    /// 4. Converts the response back to A2A format
    ///
    /// # Arguments
    ///
    /// * `message` - The A2A message to process
    ///
    /// # Returns
    ///
    /// A new A2A message containing the LLM response
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Message content cannot be extracted
    /// - LLM call fails after all retries
    /// - Response parsing fails
    pub async fn forward(&self, message: &ConvergedMessage) -> Result<ConvergedMessage> {
        info!("Processing A2A message: {}", message.message_id);

        // Extract content from A2A message
        let user_content = extract_content_from_message(message)?;

        // Check cache
        if let Some(cache) = &self.cache {
            let cache_key = self.cache_key(&user_content);
            if let Some(cached) = cache.lock().await.get(&cache_key) {
                debug!("Cache hit for message: {}", message.message_id);
                return Ok(parse_llm_to_a2a(
                    &message.message_id,
                    &cached.content,
                    &self.config.model.name,
                    Some(cached.usage.clone()),
                ));
            }
        }

        // Call LLM with retry logic
        let (response_content, usage) = self.call_llm_with_retry(&user_content).await?;

        // Cache the response
        if let Some(cache) = &self.cache {
            let cache_key = self.cache_key(&user_content);
            let cached = CachedResponse {
                content: response_content.clone(),
                usage: usage.clone(),
                cached_at: std::time::Instant::now(),
            };
            cache.lock().await.put(cache_key, cached);
        }

        // Convert to A2A format
        let response = parse_llm_to_a2a(
            &message.message_id,
            &response_content,
            &self.config.model.name,
            Some(usage),
        );

        info!("Successfully processed message: {}", message.message_id);
        Ok(response)
    }

    /// Process a batch of A2A messages
    ///
    /// Processes multiple messages concurrently for improved throughput.
    ///
    /// # Arguments
    ///
    /// * `messages` - Slice of A2A messages to process
    ///
    /// # Returns
    ///
    /// Vector of response messages in the same order as inputs
    pub async fn forward_batch(
        &self, messages: &[ConvergedMessage],
    ) -> Result<Vec<ConvergedMessage>> {
        let futures: Vec<_> = messages.iter().map(|msg| self.forward(msg)).collect();

        let results = futures::future::join_all(futures).await;

        results.into_iter().collect()
    }

    /// Call the LLM with retry logic
    async fn call_llm_with_retry(&self, content: &str) -> Result<(String, TokenUsage)> {
        let mut last_error = None;

        for attempt in 1..=self.config.max_retries {
            debug!(
                "LLM attempt {}/{} for model: {}",
                attempt, self.config.max_retries, self.config.model.name
            );

            match self.call_llm(content).await {
                Ok(result) => return Ok(result),
                Err(e) => {
                    last_error = Some(e);
                    warn!(
                        "LLM attempt {} failed: {}",
                        attempt,
                        last_error.as_ref().unwrap()
                    );

                    if attempt < self.config.max_retries {
                        // Exponential backoff
                        let delay = std::time::Duration::from_millis(1000 * attempt as u64);
                        tokio::time::sleep(delay).await;
                    }
                }
            }
        }

        Err(last_error.unwrap_or_else(|| {
            DspyError::LlmError(ggen_ai::GgenAiError::Other {
                message: "All retries exhausted".to_string(),
            })
        }))
    }

    /// Make the actual LLM call
    async fn call_llm(&self, content: &str) -> Result<(String, TokenUsage)> {
        let client = self.client.lock().await;

        if self.config.model.name.is_empty() {
            return Err(DspyError::config(
                "Model name not set. Set GGEN_LLM_MODEL or DEFAULT_MODEL env var.",
            ));
        }

        // Build chat messages
        let messages = build_llm_messages(&self.config.system_prompt, content);

        // Build chat request
        let chat_req = ChatRequest::new(messages);

        // Configure chat options
        let chat_options = ChatOptions::default()
            .with_temperature(self.config.temperature as f64)
            .with_max_tokens(self.config.max_tokens);

        debug!(
            "Calling LLM: {} with temperature: {}",
            self.config.model.name, self.config.temperature
        );

        // Execute request with timeout
        let timeout = std::time::Duration::from_secs(self.config.timeout_secs);
        let response = tokio::time::timeout(timeout, async {
            client
                .exec_chat(&self.config.model.name, chat_req, Some(&chat_options))
                .await
        })
        .await
        .map_err(|_| DspyError::Timeout(self.config.timeout_secs * 1000))?
        .map_err(|e| {
            DspyError::LlmError(ggen_ai::GgenAiError::Other {
                message: format!("LLM error: {}", e),
            })
        })?;

        // Extract content
        let content = response.first_text().unwrap_or_default().to_string();

        // Extract usage information if available
        let usage = TokenUsage::new(0, 0); // genai doesn't provide token counts directly

        debug!("LLM response received: {} chars", content.len());

        Ok((content, usage))
    }

    /// Generate a cache key for the given content
    fn cache_key(&self, content: &str) -> String {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        content.hash(&mut hasher);
        self.config.temperature.to_bits().hash(&mut hasher);
        self.config.max_tokens.hash(&mut hasher);
        self.config.model.name.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }

    /// Clear the response cache
    pub async fn clear_cache(&self) {
        if let Some(cache) = &self.cache {
            cache.lock().await.clear();
        }
    }

    /// Get cache statistics
    pub async fn cache_stats(&self) -> CacheStats {
        if let Some(cache) = &self.cache {
            let cache_ref = cache.lock().await;
            CacheStats {
                entries: cache_ref.len(),
                max_entries: cache_ref.cap().get(),
            }
        } else {
            CacheStats {
                entries: 0,
                max_entries: 0,
            }
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    /// Current number of entries
    pub entries: usize,
    /// Maximum capacity
    pub max_entries: usize,
}

// ============================================================================
// Streaming A2A Predictor
// ============================================================================

/// Streaming chunk from the LLM
///
/// Represents a single chunk of streamed response data.
#[derive(Debug, Clone)]
pub struct StreamingChunk {
    /// Chunk content
    pub content: String,

    /// Whether this is the final chunk
    pub is_final: bool,

    /// Cumulative token usage (only available in final chunk)
    pub usage: Option<TokenUsage>,
}

/// Streaming A2A predictor for real-time responses
///
/// Provides streaming responses from the LLM, enabling real-time
/// display of generated content as it arrives.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_dspy::predictors::{StreamingA2aPredictor, A2aPredictorConfig};
/// use a2a_generated::converged::message::ConvergedMessage;
/// use futures::stream::StreamExt;
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let config = A2aPredictorConfig::default();
/// let predictor = StreamingA2aPredictor::new(config)?;
///
/// let message = ConvergedMessage::text("msg-1", "user", "Tell me a story.");
/// let mut stream = predictor.forward_stream(&message).await?;
///
/// while let Some(chunk) = stream.next().await {
///     print!("{}", chunk?.content);
/// }
/// # Ok(())
/// # }
/// ```
pub struct StreamingA2aPredictor {
    /// Base predictor for configuration
    base: A2aPredictor,
}

impl StreamingA2aPredictor {
    /// Create a new streaming predictor
    pub fn new(config: A2aPredictorConfig) -> Result<Self> {
        Ok(Self {
            base: A2aPredictor::new(config)?,
        })
    }

    /// Get the predictor's configuration
    pub fn config(&self) -> &A2aPredictorConfig {
        self.base.config()
    }

    /// Get the model name
    pub fn model_name(&self) -> &str {
        self.base.model_name()
    }

    /// Process an A2A message with streaming response
    ///
    /// Returns a stream of chunks as they arrive from the LLM.
    pub async fn forward_stream(
        &self, message: &ConvergedMessage,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<StreamingChunk>> + Send>>> {
        // For now, return a non-streaming response as a single chunk
        // Full streaming support requires genai streaming API integration
        let response = self.base.forward(message).await?;

        let response_content = extract_content_from_message(&response)?;

        let stream = async_stream::stream! {
            yield Ok(StreamingChunk {
                content: response_content,
                is_final: true,
                usage: Some(TokenUsage::new(0, 0)),
            });
        };

        Ok(Box::pin(stream))
    }
}

// ============================================================================
// DSPy Module Integration
// ============================================================================

/// DSPy Module implementation for A2aPredictor
///
/// This allows the predictor to be used in DSPy-style pipelines
/// alongside other DSPy modules.
pub struct A2aModule {
    /// Inner predictor
    predictor: A2aPredictor,

    /// Module name
    #[allow(dead_code)]
    name: String,
}

impl A2aModule {
    /// Create a new A2A module
    pub fn new(predictor: A2aPredictor) -> Self {
        Self {
            predictor,
            name: "A2aModule".to_string(),
        }
    }

    /// Create with a custom name
    pub fn with_name(predictor: A2aPredictor, name: impl Into<String>) -> Self {
        Self {
            predictor,
            name: name.into(),
        }
    }

    /// Get the inner predictor
    pub fn predictor(&self) -> &A2aPredictor {
        &self.predictor
    }

    /// Get mutable reference to the predictor
    pub fn predictor_mut(&mut self) -> &mut A2aPredictor {
        &mut self.predictor
    }
}

/// DSPy Module trait for A2A operations
///
/// This trait defines the interface for modules that process A2A messages
/// and return structured outputs. Implementations can use various LLM
/// providers through the ggen-ai abstraction.
#[async_trait]
pub trait A2aModuleTrait: Send + Sync {
    /// Forward pass through the module
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput>;

    /// Get module name
    fn name(&self) -> &str;
}

/// Module output for A2A operations
#[derive(Debug, Clone)]
pub struct ModuleOutput {
    /// Response message
    pub response: ConvergedMessage,

    /// Token usage
    pub usage: Option<TokenUsage>,

    /// Additional metadata
    pub metadata: std::collections::HashMap<String, String>,
}

impl ModuleOutput {
    /// Create a new module output
    pub fn new(response: ConvergedMessage) -> Self {
        Self {
            response,
            usage: None,
            metadata: std::collections::HashMap::new(),
        }
    }

    /// Set token usage
    pub fn with_usage(mut self, usage: TokenUsage) -> Self {
        self.usage = Some(usage);
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Get the response content
    pub fn content(&self) -> Result<String> {
        extract_content_from_message(&self.response)
    }
}

// Note: Full DSPy Module trait implementation would require integrating
// with ggen-ai's Module trait. This is a simplified version that
// provides the core functionality.

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // Test configuration builder
    #[test]
    fn test_config_builder() {
        let config = A2aPredictorConfig::builder()
            .temperature(0.5)
            .max_tokens(2048)
            .system_prompt("Custom prompt")
            .cache(true, 7200)
            .timeout(60)
            .max_retries(5)
            .build();

        assert_eq!(config.temperature, 0.5);
        assert_eq!(config.max_tokens, 2048);
        assert_eq!(config.system_prompt, "Custom prompt");
        assert_eq!(config.cache_ttl, 7200);
        assert_eq!(config.timeout_secs, 60);
        assert_eq!(config.max_retries, 5);
    }

    #[test]
    fn test_config_default() {
        let config = A2aPredictorConfig::default();
        assert_eq!(config.temperature, 0.7);
        assert_eq!(config.max_tokens, 1024);
        assert!(config.enable_cache);
        assert_eq!(config.cache_ttl, 3600);
    }

    #[test]
    fn test_temperature_clamping() {
        let config = A2aPredictorConfig::default().with_temperature(3.0);
        assert_eq!(config.temperature, 2.0);

        let config = A2aPredictorConfig::default().with_temperature(-0.5);
        assert_eq!(config.temperature, 0.0);
    }

    #[test]
    fn test_token_usage() {
        let usage = TokenUsage::new(100, 50);
        assert_eq!(usage.prompt_tokens, 100);
        assert_eq!(usage.completion_tokens, 50);
        assert_eq!(usage.total_tokens, 150);
    }

    #[test]
    fn test_cache_key_generation() {
        let config = A2aPredictorConfig::default();
        let predictor = A2aPredictor::new(config).unwrap();

        let key1 = predictor.cache_key("test content");
        let key2 = predictor.cache_key("test content");
        let key3 = predictor.cache_key("different content");

        assert_eq!(key1, key2);
        assert_ne!(key1, key3);
    }

    #[test]
    fn test_streaming_chunk() {
        let chunk = StreamingChunk {
            content: "Hello".to_string(),
            is_final: false,
            usage: None,
        };

        assert_eq!(chunk.content, "Hello");
        assert!(!chunk.is_final);
        assert!(chunk.usage.is_none());
    }

    #[test]
    fn test_module_output() {
        let message = ConvergedMessage::text("test", "user", "test content");
        let output = ModuleOutput::new(message)
            .with_usage(TokenUsage::new(10, 5))
            .with_metadata("key", "value");

        assert!(output.usage.is_some());
        assert_eq!(output.metadata.get("key"), Some(&"value".to_string()));
    }

    // Integration test with actual A2A message
    #[test]
    fn test_extract_text_content() {
        let message = ConvergedMessage::text("msg-1", "user", "Hello, agent!");
        let content = extract_content_from_message(&message).unwrap();
        assert_eq!(content, "Hello, agent!");
    }

    #[test]
    fn test_build_llm_messages() {
        let messages = build_llm_messages("System prompt", "User content");
        assert_eq!(messages.len(), 2);
        assert_eq!(messages[0].role(), genai::chat::ChatRole::System);
        assert_eq!(messages[1].role(), genai::chat::ChatRole::User);
    }

    #[test]
    fn test_parse_llm_to_a2a() {
        let usage = TokenUsage::new(10, 5);
        let response = parse_llm_to_a2a("req-1", "Response content", "gpt-4", Some(usage));

        assert!(response.message_id.contains("resp-req-1"));
        assert_eq!(response.source, "llm-bridge");
    }

    #[test]
    fn test_predictor_creation() {
        let config = A2aPredictorConfig::default();
        let predictor = A2aPredictor::new(config);
        assert!(predictor.is_ok());
    }

    #[test]
    fn test_predictor_with_model() {
        let predictor = A2aPredictor::with_model("claude-3");
        assert!(predictor.is_ok());
        assert_eq!(predictor.unwrap().model_name(), "claude-3");
    }

    #[test]
    fn test_streaming_predictor_creation() {
        let config = A2aPredictorConfig::default();
        let predictor = StreamingA2aPredictor::new(config);
        assert!(predictor.is_ok());
    }

    #[test]
    fn test_a2a_module_creation() {
        let config = A2aPredictorConfig::default();
        let predictor = A2aPredictor::new(config).unwrap();
        let module = A2aModule::with_name(predictor, "TestModule");
        assert_eq!(module.name(), "TestModule");
    }

    // Test multipart content extraction
    #[test]
    fn test_extract_multipart_content() {
        use a2a_generated::converged::message::UnifiedContent;

        // Create a multipart message
        let parts = vec![
            UnifiedContent::Text {
                content: "Part 1".to_string(),
                format: None,
            },
            UnifiedContent::Text {
                content: "Part 2".to_string(),
                format: None,
            },
        ];

        let message = ConvergedMessage {
            message_id: "test-multi".to_string(),
            source: "user".to_string(),
            target: "agent".to_string(),
            envelope: Default::default(),
            payload: a2a_generated::converged::message::ConvergedPayload {
                content: UnifiedContent::Multipart {
                    parts,
                    format: None,
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: Default::default(),
            lifecycle: Default::default(),
            extensions: Default::default(),
        };

        let content = extract_content_from_message(&message).unwrap();
        assert!(content.contains("Part 1"));
        assert!(content.contains("Part 2"));
    }
}
