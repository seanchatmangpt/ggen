//! LLM client adapters for integrating with ggen-ai and other providers
//!
//! Provides comprehensive adapter pattern implementation with:
//! - Multiple adapter types (Chat, JSON, Completion)
//! - Token counting and cost tracking
//! - Retry logic with exponential backoff
//! - Rate limiting
//! - Response caching integration
//! - Request/response transformation

use crate::{DspyError, Result};
use async_trait::async_trait;
use ggen_ai::{GenAiClient, LlmClient, LlmConfig};
use moka::future::Cache;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;
use tracing::{debug, info, warn};

// ============================================================================
// Core Adapter Traits
// ============================================================================

/// LLM adapter trait for provider abstraction
#[async_trait]
pub trait LlmAdapter: Send + Sync {
    /// Format prompt with inputs, outputs, and optional demonstrations
    fn format_prompt(
        &self,
        inputs: &HashMap<String, Value>,
        output_fields: &[String],
        schema: Option<&Value>,
        demonstrations: Option<&[Demonstration]>,
    ) -> Result<String>;

    /// Parse LLM response into structured output
    fn parse_response(
        &self,
        response: &str,
        output_fields: &[String],
    ) -> Result<HashMap<String, Value>>;

    /// Get adapter name
    fn name(&self) -> &str;

    /// Check if adapter is compatible with model
    fn is_compatible(&self, _model: &str) -> bool {
        true // Default: compatible with all models
    }
}

// ============================================================================
// Demonstration Support
// ============================================================================

/// Example input/output pair for few-shot learning
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Demonstration {
    /// Input fields
    pub inputs: HashMap<String, Value>,
    /// Output fields
    pub outputs: HashMap<String, Value>,
}

impl Demonstration {
    /// Create a new demonstration
    pub fn new(inputs: HashMap<String, Value>, outputs: HashMap<String, Value>) -> Self {
        Self { inputs, outputs }
    }
}

// ============================================================================
// ChatAdapter - Natural language format with field markers
// ============================================================================

/// Chat-based adapter using natural language format with field markers
#[derive(Debug, Clone)]
pub struct ChatAdapter {
    /// Field marker format: [[ ## field ## ]]
    marker_regex: Regex,
}

impl ChatAdapter {
    /// Create a new chat adapter
    pub fn new() -> Self {
        Self {
            marker_regex: Regex::new(r"\[\[\s*##\s*(\w+)\s*##\s*\]\]").unwrap(),
        }
    }

    /// Format demonstration as string
    fn format_demonstration(&self, demo: &Demonstration, index: usize) -> String {
        let mut result = format!("\n--- Example {} ---\n", index);

        // Add inputs
        for (key, value) in &demo.inputs {
            result.push_str(&format!("{}: {}\n", key, self.value_to_string(value)));
        }

        // Add outputs
        for (key, value) in &demo.outputs {
            result.push_str(&format!("\n[[ ## {} ## ]]\n{}", key, self.value_to_string(value)));
        }

        result
    }

    /// Convert JSON value to string representation
    fn value_to_string(&self, value: &Value) -> String {
        match value {
            Value::String(s) => s.clone(),
            other => serde_json::to_string_pretty(other).unwrap_or_else(|_| "null".to_string()),
        }
    }
}

impl Default for ChatAdapter {
    fn default() -> Self {
        Self::new()
    }
}

impl LlmAdapter for ChatAdapter {
    fn format_prompt(
        &self,
        inputs: &HashMap<String, Value>,
        output_fields: &[String],
        _schema: Option<&Value>,
        demonstrations: Option<&[Demonstration]>,
    ) -> Result<String> {
        let mut prompt = String::new();

        // Add demonstrations if provided
        if let Some(demos) = demonstrations {
            if !demos.is_empty() {
                prompt.push_str("Here are some examples:\n");
                for (idx, demo) in demos.iter().enumerate() {
                    prompt.push_str(&self.format_demonstration(demo, idx + 1));
                }
                prompt.push_str("\n--- Your Turn ---\n");
            }
        }

        // Add input fields
        for (key, value) in inputs {
            prompt.push_str(&format!("{}: {}\n", key, self.value_to_string(value)));
        }

        // Add output field markers
        for field in output_fields {
            prompt.push_str(&format!("\n[[ ## {} ## ]]\n", field));
        }

        Ok(prompt)
    }

    fn parse_response(
        &self,
        response: &str,
        output_fields: &[String],
    ) -> Result<HashMap<String, Value>> {
        let mut results = HashMap::new();

        for field in output_fields {
            // Find field marker
            let pattern = format!(r"\[\[\s*##\s*{}\s*##\s*\]\]\s*\n(.*?)(?:\n\[\[|$)", field);
            let field_regex = Regex::new(&pattern).map_err(|e| {
                DspyError::ParsingError(format!("Invalid regex pattern: {}", e))
            })?;

            if let Some(captures) = field_regex.captures(response) {
                let content = captures
                    .get(1)
                    .map(|m| m.as_str().trim())
                    .ok_or_else(|| DspyError::FieldError {
                        field: field.clone(),
                    })?;

                if content.is_empty() {
                    return Err(DspyError::FieldError {
                        field: field.clone(),
                    });
                }

                // Try to parse as JSON, otherwise treat as string
                let value = serde_json::from_str::<Value>(content)
                    .unwrap_or_else(|_| Value::String(content.to_string()));

                results.insert(field.clone(), value);
            } else {
                return Err(DspyError::FieldError {
                    field: field.clone(),
                });
            }
        }

        Ok(results)
    }

    fn name(&self) -> &str {
        "ChatAdapter"
    }
}

// ============================================================================
// JSONAdapter - Structured JSON output for compatible models
// ============================================================================

/// JSON-based adapter for models that support structured output
#[derive(Debug, Clone)]
pub struct JSONAdapter {
    /// Compatible model patterns
    compatible_models: Vec<String>,
}

impl JSONAdapter {
    /// Create a new JSON adapter
    pub fn new() -> Self {
        Self {
            compatible_models: vec![
                "gpt-4".to_string(),
                "gpt-4-turbo".to_string(),
                "gpt-4o".to_string(),
                "claude-3-opus".to_string(),
                "claude-3-sonnet".to_string(),
            ],
        }
    }

    /// Generate JSON schema for output fields
    fn generate_schema(&self, output_fields: &[String]) -> Value {
        let mut properties = serde_json::Map::new();

        for field in output_fields {
            properties.insert(
                field.clone(),
                serde_json::json!({
                    "type": "string",
                    "description": format!("The {} field", field)
                }),
            );
        }

        serde_json::json!({
            "type": "object",
            "properties": properties,
            "required": output_fields,
        })
    }

    /// Extract JSON from response (handles markdown code blocks)
    fn extract_json(&self, response: &str) -> Result<String> {
        // Try to extract from ```json``` blocks
        let json_block_regex = Regex::new(r"```json\s*\n(.*?)\n```")
            .map_err(|e| DspyError::ParsingError(format!("Regex error: {}", e)))?;

        if let Some(captures) = json_block_regex.captures(response) {
            return Ok(captures.get(1).unwrap().as_str().to_string());
        }

        // Try to extract from ``` blocks without json marker
        let code_block_regex = Regex::new(r"```\s*\n(.*?)\n```")
            .map_err(|e| DspyError::ParsingError(format!("Regex error: {}", e)))?;

        if let Some(captures) = code_block_regex.captures(response) {
            let content = captures.get(1).unwrap().as_str();
            // Verify it's valid JSON
            if content.trim().starts_with('{') {
                return Ok(content.to_string());
            }
        }

        // Return as-is if no code blocks found
        Ok(response.trim().to_string())
    }
}

impl Default for JSONAdapter {
    fn default() -> Self {
        Self::new()
    }
}

impl LlmAdapter for JSONAdapter {
    fn format_prompt(
        &self,
        inputs: &HashMap<String, Value>,
        output_fields: &[String],
        schema: Option<&Value>,
        demonstrations: Option<&[Demonstration]>,
    ) -> Result<String> {
        let mut prompt = String::new();

        // Add demonstrations if provided
        if let Some(demos) = demonstrations {
            if !demos.is_empty() {
                prompt.push_str("Here are some examples:\n\n");
                for (idx, demo) in demos.iter().enumerate() {
                    prompt.push_str(&format!("Example {}:\n", idx + 1));
                    prompt.push_str(&format!("Input: {}\n", serde_json::to_string_pretty(&demo.inputs)?));
                    prompt.push_str(&format!("Output: {}\n\n", serde_json::to_string_pretty(&demo.outputs)?));
                }
            }
        }

        // Add schema
        let schema_value = schema.unwrap_or(&self.generate_schema(output_fields));
        prompt.push_str("Respond with a JSON object matching this schema:\n");
        prompt.push_str(&serde_json::to_string_pretty(schema_value)?);
        prompt.push_str("\n\n");

        // Add inputs
        prompt.push_str("Inputs:\n");
        prompt.push_str(&serde_json::to_string_pretty(inputs)?);
        prompt.push_str("\n\nProvide your response as valid JSON:");

        Ok(prompt)
    }

    fn parse_response(
        &self,
        response: &str,
        output_fields: &[String],
    ) -> Result<HashMap<String, Value>> {
        let json_str = self.extract_json(response)?;

        let parsed: Value = serde_json::from_str(&json_str)
            .map_err(|e| DspyError::ParsingError(format!("Invalid JSON: {}", e)))?;

        let obj = parsed.as_object().ok_or_else(|| {
            DspyError::ParsingError("Response is not a JSON object".to_string())
        })?;

        let mut results = HashMap::new();
        for field in output_fields {
            if let Some(value) = obj.get(field) {
                results.insert(field.clone(), value.clone());
            } else {
                return Err(DspyError::FieldError {
                    field: field.clone(),
                });
            }
        }

        Ok(results)
    }

    fn name(&self) -> &str {
        "JSONAdapter"
    }

    fn is_compatible(&self, model: &str) -> bool {
        self.compatible_models
            .iter()
            .any(|pattern| model.contains(pattern))
    }
}

// ============================================================================
// CompletionAdapter - Simple text completion
// ============================================================================

/// Simple completion adapter for text generation
#[derive(Debug, Clone)]
pub struct CompletionAdapter;

impl CompletionAdapter {
    /// Create a new completion adapter
    pub fn new() -> Self {
        Self
    }
}

impl Default for CompletionAdapter {
    fn default() -> Self {
        Self::new()
    }
}

impl LlmAdapter for CompletionAdapter {
    fn format_prompt(
        &self,
        inputs: &HashMap<String, Value>,
        _output_fields: &[String],
        _schema: Option<&Value>,
        _demonstrations: Option<&[Demonstration]>,
    ) -> Result<String> {
        // Simple concatenation of inputs
        let mut prompt = String::new();
        for (key, value) in inputs {
            match value {
                Value::String(s) => prompt.push_str(&format!("{}: {}\n", key, s)),
                other => prompt.push_str(&format!("{}: {}\n", key, other)),
            }
        }
        Ok(prompt)
    }

    fn parse_response(
        &self,
        response: &str,
        output_fields: &[String],
    ) -> Result<HashMap<String, Value>> {
        let mut results = HashMap::new();

        // If single output field, return entire response
        if output_fields.len() == 1 {
            results.insert(
                output_fields[0].clone(),
                Value::String(response.trim().to_string()),
            );
        } else {
            // Split by newlines for multiple fields
            let lines: Vec<&str> = response.lines().collect();
            for (idx, field) in output_fields.iter().enumerate() {
                let value = lines
                    .get(idx)
                    .map(|s| Value::String(s.to_string()))
                    .unwrap_or(Value::Null);
                results.insert(field.clone(), value);
            }
        }

        Ok(results)
    }

    fn name(&self) -> &str {
        "CompletionAdapter"
    }
}

// ============================================================================
// AdapterWithFallback - Automatic adapter selection
// ============================================================================

/// Adapter that automatically selects the best adapter for a model
#[derive(Debug, Clone)]
pub struct AdapterWithFallback {
    /// Model name
    pub model: String,
    /// Primary adapter (JSON if compatible)
    json_adapter: JSONAdapter,
    /// Fallback adapter (Chat)
    chat_adapter: ChatAdapter,
}

impl AdapterWithFallback {
    /// Create a new adapter with fallback
    pub fn new(model: impl Into<String>) -> Self {
        Self {
            model: model.into(),
            json_adapter: JSONAdapter::new(),
            chat_adapter: ChatAdapter::new(),
        }
    }

    /// Get the appropriate adapter for this model
    fn get_adapter(&self) -> &dyn LlmAdapter {
        if self.json_adapter.is_compatible(&self.model) {
            &self.json_adapter
        } else {
            &self.chat_adapter
        }
    }
}

impl LlmAdapter for AdapterWithFallback {
    fn format_prompt(
        &self,
        inputs: &HashMap<String, Value>,
        output_fields: &[String],
        schema: Option<&Value>,
        demonstrations: Option<&[Demonstration]>,
    ) -> Result<String> {
        self.get_adapter()
            .format_prompt(inputs, output_fields, schema, demonstrations)
    }

    fn parse_response(
        &self,
        response: &str,
        output_fields: &[String],
    ) -> Result<HashMap<String, Value>> {
        self.get_adapter().parse_response(response, output_fields)
    }

    fn name(&self) -> &str {
        self.get_adapter().name()
    }

    fn is_compatible(&self, model: &str) -> bool {
        self.json_adapter.is_compatible(model) || self.chat_adapter.is_compatible(model)
    }
}

// ============================================================================
// Integrated Adapter with LLM Client
// ============================================================================

/// Integrated adapter combining LLM client with adapter pattern
pub struct IntegratedAdapter {
    /// LLM client
    client: Arc<dyn LlmClient>,
    /// Adapter for format/parse
    adapter: Box<dyn LlmAdapter>,
    /// Response cache
    cache: Option<Cache<String, String>>,
    /// Retry configuration
    retry_config: RetryConfig,
    /// Token counter
    token_counter: Arc<TokenCounter>,
}

impl IntegratedAdapter {
    /// Create a new integrated adapter
    pub fn new(
        client: Arc<dyn LlmClient>,
        adapter: Box<dyn LlmAdapter>,
    ) -> Self {
        Self {
            client,
            adapter,
            cache: None,
            retry_config: RetryConfig::default(),
            token_counter: Arc::new(TokenCounter::new()),
        }
    }

    /// Enable caching with TTL and size limits
    pub fn with_cache(mut self, ttl: Duration, max_entries: u64) -> Self {
        self.cache = Some(
            Cache::builder()
                .time_to_live(ttl)
                .max_capacity(max_entries)
                .build(),
        );
        self
    }

    /// Set retry configuration
    pub fn with_retry_config(mut self, config: RetryConfig) -> Self {
        self.retry_config = config;
        self
    }

    /// Generate completion with retry logic
    pub async fn complete_with_retry(
        &self,
        inputs: &HashMap<String, Value>,
        output_fields: &[String],
        schema: Option<&Value>,
        demonstrations: Option<&[Demonstration]>,
    ) -> Result<HashMap<String, Value>> {
        let prompt = self
            .adapter
            .format_prompt(inputs, output_fields, schema, demonstrations)?;

        // Check cache
        if let Some(cache) = &self.cache {
            if let Some(cached) = cache.get(&prompt).await {
                debug!("Cache hit for prompt");
                return self.adapter.parse_response(&cached, output_fields);
            }
        }

        // Retry logic with exponential backoff
        let mut attempt = 0;
        let mut last_error = None;

        while attempt < self.retry_config.max_retries {
            match self.client.complete(&prompt).await {
                Ok(response) => {
                    let content = response.content.clone();

                    // Track token usage
                    if let Some(usage) = response.usage {
                        self.token_counter.add_usage(
                            usage.prompt_tokens,
                            usage.completion_tokens,
                            self.client.get_config().model.clone(),
                        );
                    }

                    // Cache response
                    if let Some(cache) = &self.cache {
                        cache.insert(prompt.clone(), content.clone()).await;
                    }

                    return self.adapter.parse_response(&content, output_fields);
                }
                Err(e) => {
                    warn!("Attempt {} failed: {}", attempt + 1, e);
                    last_error = Some(e);
                    attempt += 1;

                    if attempt < self.retry_config.max_retries {
                        let delay = self.retry_config.backoff_duration(attempt);
                        info!("Retrying after {:?}", delay);
                        sleep(delay).await;
                    }
                }
            }
        }

        Err(DspyError::LlmError(last_error.unwrap()))
    }

    /// Get token usage statistics
    pub fn get_token_stats(&self) -> TokenStats {
        self.token_counter.get_stats()
    }
}

// ============================================================================
// Retry Configuration
// ============================================================================

/// Configuration for retry logic
#[derive(Debug, Clone)]
pub struct RetryConfig {
    /// Maximum number of retries
    pub max_retries: u32,
    /// Initial backoff duration
    pub initial_backoff: Duration,
    /// Maximum backoff duration
    pub max_backoff: Duration,
    /// Backoff multiplier
    pub backoff_multiplier: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_retries: 3,
            initial_backoff: Duration::from_millis(100),
            max_backoff: Duration::from_secs(10),
            backoff_multiplier: 2.0,
        }
    }
}

impl RetryConfig {
    /// Calculate backoff duration for attempt
    pub fn backoff_duration(&self, attempt: u32) -> Duration {
        let duration_ms = (self.initial_backoff.as_millis() as f64)
            * self.backoff_multiplier.powi(attempt as i32);
        let duration = Duration::from_millis(duration_ms as u64);
        duration.min(self.max_backoff)
    }
}

// ============================================================================
// Token Counting and Cost Tracking
// ============================================================================

/// Token counter for cost tracking
pub struct TokenCounter {
    /// Total prompt tokens
    prompt_tokens: std::sync::atomic::AtomicU32,
    /// Total completion tokens
    completion_tokens: std::sync::atomic::AtomicU32,
    /// Per-model usage
    model_usage: std::sync::Mutex<HashMap<String, ModelUsage>>,
}

impl TokenCounter {
    /// Create a new token counter
    pub fn new() -> Self {
        Self {
            prompt_tokens: std::sync::atomic::AtomicU32::new(0),
            completion_tokens: std::sync::atomic::AtomicU32::new(0),
            model_usage: std::sync::Mutex::new(HashMap::new()),
        }
    }

    /// Add token usage
    pub fn add_usage(&self, prompt: u32, completion: u32, model: String) {
        use std::sync::atomic::Ordering;

        self.prompt_tokens.fetch_add(prompt, Ordering::Relaxed);
        self.completion_tokens
            .fetch_add(completion, Ordering::Relaxed);

        let mut usage = self.model_usage.lock().unwrap();
        usage
            .entry(model)
            .or_insert_with(ModelUsage::default)
            .add(prompt, completion);
    }

    /// Get statistics
    pub fn get_stats(&self) -> TokenStats {
        use std::sync::atomic::Ordering;

        let prompt_tokens = self.prompt_tokens.load(Ordering::Relaxed);
        let completion_tokens = self.completion_tokens.load(Ordering::Relaxed);

        TokenStats {
            prompt_tokens,
            completion_tokens,
            total_tokens: prompt_tokens + completion_tokens,
            model_usage: self.model_usage.lock().unwrap().clone(),
        }
    }
}

impl Default for TokenCounter {
    fn default() -> Self {
        Self::new()
    }
}

/// Per-model usage statistics
#[derive(Debug, Clone, Default)]
pub struct ModelUsage {
    /// Prompt tokens
    pub prompt_tokens: u32,
    /// Completion tokens
    pub completion_tokens: u32,
    /// Request count
    pub request_count: u32,
}

impl ModelUsage {
    /// Add usage
    pub fn add(&mut self, prompt: u32, completion: u32) {
        self.prompt_tokens += prompt;
        self.completion_tokens += completion;
        self.request_count += 1;
    }
}

/// Token usage statistics
#[derive(Debug, Clone)]
pub struct TokenStats {
    /// Total prompt tokens
    pub prompt_tokens: u32,
    /// Total completion tokens
    pub completion_tokens: u32,
    /// Total tokens
    pub total_tokens: u32,
    /// Per-model usage
    pub model_usage: HashMap<String, ModelUsage>,
}

// ============================================================================
// GgenAiAdapter - Integration with ggen-ai
// ============================================================================

/// Adapter for ggen-ai client
pub struct GgenAiAdapter {
    /// Integrated adapter with client
    integrated: IntegratedAdapter,
}

impl GgenAiAdapter {
    /// Create a new ggen-ai adapter
    pub fn new(client: GenAiClient) -> Self {
        let model = client.get_config().model.clone();
        let adapter = AdapterWithFallback::new(model);

        Self {
            integrated: IntegratedAdapter::new(
                Arc::new(client),
                Box::new(adapter),
            ),
        }
    }

    /// Create with default configuration
    pub fn default_client() -> Result<Self> {
        let config = LlmConfig::default();
        let client = GenAiClient::new(config)?;
        Ok(Self::new(client))
    }

    /// Enable caching
    pub fn with_cache(mut self, ttl: Duration, max_entries: u64) -> Self {
        self.integrated = self.integrated.with_cache(ttl, max_entries);
        self
    }

    /// Set retry configuration
    pub fn with_retry_config(mut self, config: RetryConfig) -> Self {
        self.integrated = self.integrated.with_retry_config(config);
        self
    }

    /// Generate completion
    pub async fn complete(
        &self,
        inputs: &HashMap<String, Value>,
        output_fields: &[String],
    ) -> Result<HashMap<String, Value>> {
        self.integrated
            .complete_with_retry(inputs, output_fields, None, None)
            .await
    }

    /// Generate completion with demonstrations
    pub async fn complete_with_demos(
        &self,
        inputs: &HashMap<String, Value>,
        output_fields: &[String],
        demonstrations: &[Demonstration],
    ) -> Result<HashMap<String, Value>> {
        self.integrated
            .complete_with_retry(inputs, output_fields, None, Some(demonstrations))
            .await
    }

    /// Get token statistics
    pub fn get_token_stats(&self) -> TokenStats {
        self.integrated.get_token_stats()
    }
}

/// Request for LLM completion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompletionRequest {
    /// Prompt text
    pub prompt: String,

    /// Temperature (0.0 to 1.0)
    pub temperature: Option<f64>,

    /// Maximum tokens
    pub max_tokens: Option<usize>,

    /// Stop sequences
    pub stop: Option<Vec<String>>,
}

impl CompletionRequest {
    /// Create a new completion request
    pub fn new(prompt: impl Into<String>) -> Self {
        Self {
            prompt: prompt.into(),
            temperature: None,
            max_tokens: None,
            stop: None,
        }
    }

    /// Set temperature
    pub fn with_temperature(mut self, temperature: f64) -> Self {
        self.temperature = Some(temperature);
        self
    }

    /// Set max tokens
    pub fn with_max_tokens(mut self, max_tokens: usize) -> Self {
        self.max_tokens = Some(max_tokens);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_completion_request() {
        let request = CompletionRequest::new("Hello")
            .with_temperature(0.7)
            .with_max_tokens(100);

        assert_eq!(request.prompt, "Hello");
        assert_eq!(request.temperature, Some(0.7));
        assert_eq!(request.max_tokens, Some(100));
    }

    #[test]
    fn test_retry_config_backoff() {
        let config = RetryConfig::default();
        let d1 = config.backoff_duration(0);
        let d2 = config.backoff_duration(1);
        let d3 = config.backoff_duration(2);

        assert!(d2 > d1);
        assert!(d3 > d2);
    }

    #[test]
    fn test_token_counter() {
        let counter = TokenCounter::new();
        counter.add_usage(100, 50, "gpt-4".to_string());
        counter.add_usage(200, 75, "gpt-4".to_string());

        let stats = counter.get_stats();
        assert_eq!(stats.prompt_tokens, 300);
        assert_eq!(stats.completion_tokens, 125);
        assert_eq!(stats.total_tokens, 425);

        let model_stats = stats.model_usage.get("gpt-4").unwrap();
        assert_eq!(model_stats.request_count, 2);
    }

    #[test]
    fn test_demonstration_creation() {
        let mut inputs = HashMap::new();
        inputs.insert("in".to_string(), Value::String("test".to_string()));

        let mut outputs = HashMap::new();
        outputs.insert("out".to_string(), Value::String("result".to_string()));

        let demo = Demonstration::new(inputs.clone(), outputs.clone());
        assert_eq!(demo.inputs, inputs);
        assert_eq!(demo.outputs, outputs);
    }
}
