//! Model Capabilities - Type-safe model representation
//!
//! This module addresses the fact that `model: String` is insufficient.
//! Your constraints already know things the model does not.
//!
//! A model is more than a name - it has:
//! - Context window size
//! - Structured output support
//! - Determinism guarantees
//! - Cost characteristics
//! - Failure semantics
//!
//! Rust will make you encode this, whether you like it or not.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Model identifier with capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Model {
    /// Model name/identifier (e.g., "gpt-4", "claude-3-opus")
    pub name: String,

    /// Provider (e.g., "openai", "anthropic", "local")
    pub provider: ModelProvider,

    /// Capabilities of this model
    pub capabilities: ModelCapabilities,

    /// Cost information
    pub cost: Option<ModelCost>,

    /// Model-specific configuration
    pub config: ModelConfig,
}

impl Model {
    /// Create a model from just a name (capabilities will be inferred or default)
    pub fn from_name(name: impl Into<String>) -> Self {
        let name = name.into();
        let provider = ModelProvider::infer_from_name(&name);
        let capabilities = ModelCapabilities::infer_from_name(&name);

        Self {
            name,
            provider,
            capabilities,
            cost: None,
            config: ModelConfig::default(),
        }
    }

    /// Create with explicit provider
    pub fn new(name: impl Into<String>, provider: ModelProvider) -> Self {
        let name = name.into();
        let capabilities = ModelCapabilities::infer_from_name(&name);

        Self {
            name,
            provider,
            capabilities,
            cost: None,
            config: ModelConfig::default(),
        }
    }

    /// Set capabilities
    pub fn with_capabilities(mut self, capabilities: ModelCapabilities) -> Self {
        self.capabilities = capabilities;
        self
    }

    /// Set cost info
    pub fn with_cost(mut self, cost: ModelCost) -> Self {
        self.cost = Some(cost);
        self
    }

    /// Set config
    pub fn with_config(mut self, config: ModelConfig) -> Self {
        self.config = config;
        self
    }

    /// Check if model supports structured JSON output
    pub fn supports_structured_output(&self) -> bool {
        self.capabilities.structured_output
    }

    /// Check if model supports function/tool calling
    pub fn supports_function_calling(&self) -> bool {
        self.capabilities.function_calling
    }

    /// Get maximum context length
    pub fn max_context(&self) -> usize {
        self.capabilities.max_context_tokens
    }

    /// Check if a prompt length is within limits
    pub fn fits_context(&self, estimated_tokens: usize) -> bool {
        estimated_tokens < self.capabilities.max_context_tokens
    }

    /// Calculate estimated cost for a request
    pub fn estimate_cost(&self, input_tokens: usize, output_tokens: usize) -> Option<f64> {
        self.cost.as_ref().map(|c| {
            let input_cost = (input_tokens as f64 / 1_000_000.0) * c.input_per_million;
            let output_cost = (output_tokens as f64 / 1_000_000.0) * c.output_per_million;
            input_cost + output_cost
        })
    }
}

/// Model provider
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ModelProvider {
    OpenAI,
    Anthropic,
    Google,
    Cohere,
    Mistral,
    Ollama,
    Local,
    Custom,
}

impl ModelProvider {
    /// Infer provider from model name
    pub fn infer_from_name(name: &str) -> Self {
        let name_lower = name.to_lowercase();

        if name_lower.starts_with("gpt") || name_lower.contains("openai") {
            ModelProvider::OpenAI
        } else if name_lower.starts_with("claude") || name_lower.contains("anthropic") {
            ModelProvider::Anthropic
        } else if name_lower.starts_with("gemini") || name_lower.contains("google") {
            ModelProvider::Google
        } else if name_lower.starts_with("command") || name_lower.contains("cohere") {
            ModelProvider::Cohere
        } else if name_lower.starts_with("mistral") || name_lower.contains("mixtral") {
            ModelProvider::Mistral
        } else if name_lower.contains("ollama") || name_lower.contains("llama") {
            ModelProvider::Ollama
        } else if name_lower.contains("local") || name_lower.starts_with("file://") {
            ModelProvider::Local
        } else {
            ModelProvider::Custom
        }
    }

    /// Get provider name as string
    pub fn as_str(&self) -> &'static str {
        match self {
            ModelProvider::OpenAI => "openai",
            ModelProvider::Anthropic => "anthropic",
            ModelProvider::Google => "google",
            ModelProvider::Cohere => "cohere",
            ModelProvider::Mistral => "mistral",
            ModelProvider::Ollama => "ollama",
            ModelProvider::Local => "local",
            ModelProvider::Custom => "custom",
        }
    }
}

/// Model capabilities specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelCapabilities {
    /// Maximum context window in tokens
    pub max_context_tokens: usize,

    /// Maximum output tokens per request
    pub max_output_tokens: usize,

    /// Whether model supports structured JSON output natively
    pub structured_output: bool,

    /// Whether model supports function/tool calling
    pub function_calling: bool,

    /// Whether model supports system messages
    pub system_messages: bool,

    /// Whether model can stream responses
    pub streaming: bool,

    /// Whether model supports vision/images
    pub vision: bool,

    /// Typical latency class (affects retry strategies)
    pub latency_class: LatencyClass,

    /// Reliability/uptime characteristics
    pub reliability: ReliabilityClass,

    /// Model modality capabilities
    pub modalities: Vec<Modality>,
}

impl Default for ModelCapabilities {
    fn default() -> Self {
        Self {
            max_context_tokens: 4096,
            max_output_tokens: 2048,
            structured_output: false,
            function_calling: false,
            system_messages: true,
            streaming: true,
            vision: false,
            latency_class: LatencyClass::Medium,
            reliability: ReliabilityClass::Standard,
            modalities: vec![Modality::Text],
        }
    }
}

impl ModelCapabilities {
    /// Create capabilities for a known model
    pub fn infer_from_name(name: &str) -> Self {
        let name_lower = name.to_lowercase();

        // GPT-4 family
        if name_lower.contains("gpt-4") {
            return Self {
                max_context_tokens: if name_lower.contains("turbo") { 128_000 } else { 8_192 },
                max_output_tokens: 4_096,
                structured_output: true, // JSON mode
                function_calling: true,
                system_messages: true,
                streaming: true,
                vision: name_lower.contains("vision") || name_lower.contains("turbo"),
                latency_class: LatencyClass::Medium,
                reliability: ReliabilityClass::High,
                modalities: vec![Modality::Text],
            };
        }

        // GPT-3.5
        if name_lower.contains("gpt-3.5") {
            return Self {
                max_context_tokens: 16_385,
                max_output_tokens: 4_096,
                structured_output: true,
                function_calling: true,
                system_messages: true,
                streaming: true,
                vision: false,
                latency_class: LatencyClass::Fast,
                reliability: ReliabilityClass::High,
                modalities: vec![Modality::Text],
            };
        }

        // Claude 3 family
        if name_lower.contains("claude-3") {
            return Self {
                max_context_tokens: 200_000,
                max_output_tokens: 4_096,
                structured_output: false, // No native JSON mode
                function_calling: true,
                system_messages: true,
                streaming: true,
                vision: true,
                latency_class: if name_lower.contains("haiku") {
                    LatencyClass::Fast
                } else if name_lower.contains("opus") {
                    LatencyClass::Slow
                } else {
                    LatencyClass::Medium
                },
                reliability: ReliabilityClass::High,
                modalities: vec![Modality::Text, Modality::Image],
            };
        }

        // Gemini
        if name_lower.contains("gemini") {
            return Self {
                max_context_tokens: 1_000_000, // Gemini 1.5 Pro
                max_output_tokens: 8_192,
                structured_output: true,
                function_calling: true,
                system_messages: true,
                streaming: true,
                vision: true,
                latency_class: LatencyClass::Medium,
                reliability: ReliabilityClass::Standard,
                modalities: vec![Modality::Text, Modality::Image, Modality::Video],
            };
        }

        // Mistral
        if name_lower.contains("mistral") || name_lower.contains("mixtral") {
            return Self {
                max_context_tokens: 32_000,
                max_output_tokens: 4_096,
                structured_output: true,
                function_calling: name_lower.contains("large"),
                system_messages: true,
                streaming: true,
                vision: false,
                latency_class: LatencyClass::Fast,
                reliability: ReliabilityClass::Standard,
                modalities: vec![Modality::Text],
            };
        }

        // Default/unknown
        Self::default()
    }

    /// Builder: set max context
    pub fn max_context(mut self, tokens: usize) -> Self {
        self.max_context_tokens = tokens;
        self
    }

    /// Builder: set max output
    pub fn max_output(mut self, tokens: usize) -> Self {
        self.max_output_tokens = tokens;
        self
    }

    /// Builder: enable structured output
    pub fn with_structured_output(mut self) -> Self {
        self.structured_output = true;
        self
    }

    /// Builder: enable function calling
    pub fn with_function_calling(mut self) -> Self {
        self.function_calling = true;
        self
    }

    /// Builder: enable vision
    pub fn with_vision(mut self) -> Self {
        self.vision = true;
        self.modalities.push(Modality::Image);
        self
    }
}

/// Latency classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LatencyClass {
    /// <1s typical first token
    Fast,

    /// 1-3s typical first token
    Medium,

    /// >3s typical first token
    Slow,
}

impl LatencyClass {
    /// Suggested timeout for this latency class
    pub fn suggested_timeout_ms(&self) -> u64 {
        match self {
            LatencyClass::Fast => 30_000,
            LatencyClass::Medium => 60_000,
            LatencyClass::Slow => 120_000,
        }
    }
}

/// Reliability classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ReliabilityClass {
    /// >99.9% uptime, robust rate limiting
    High,

    /// >99% uptime
    Standard,

    /// May have availability issues
    Low,
}

impl ReliabilityClass {
    /// Suggested retry count for this reliability class
    pub fn suggested_retries(&self) -> u32 {
        match self {
            ReliabilityClass::High => 2,
            ReliabilityClass::Standard => 3,
            ReliabilityClass::Low => 5,
        }
    }
}

/// Model modality
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Modality {
    Text,
    Image,
    Audio,
    Video,
}

/// Model cost information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelCost {
    /// Cost per million input tokens (USD)
    pub input_per_million: f64,

    /// Cost per million output tokens (USD)
    pub output_per_million: f64,

    /// Whether there are additional charges (vision, etc.)
    pub additional_charges: HashMap<String, f64>,
}

impl ModelCost {
    pub fn new(input: f64, output: f64) -> Self {
        Self {
            input_per_million: input,
            output_per_million: output,
            additional_charges: HashMap::new(),
        }
    }

    /// Known cost for GPT-4 Turbo
    pub fn gpt4_turbo() -> Self {
        Self::new(10.0, 30.0)
    }

    /// Known cost for GPT-3.5 Turbo
    pub fn gpt35_turbo() -> Self {
        Self::new(0.5, 1.5)
    }

    /// Known cost for Claude 3 Opus
    pub fn claude3_opus() -> Self {
        Self::new(15.0, 75.0)
    }

    /// Known cost for Claude 3 Sonnet
    pub fn claude3_sonnet() -> Self {
        Self::new(3.0, 15.0)
    }

    /// Known cost for Claude 3 Haiku
    pub fn claude3_haiku() -> Self {
        Self::new(0.25, 1.25)
    }
}

/// Model-specific configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ModelConfig {
    /// Default temperature
    pub default_temperature: Option<f64>,

    /// Default max tokens for output
    pub default_max_tokens: Option<usize>,

    /// API base URL override
    pub api_base: Option<String>,

    /// API version
    pub api_version: Option<String>,

    /// Extra headers
    #[serde(default)]
    pub extra_headers: HashMap<String, String>,

    /// Model-specific parameters
    #[serde(default)]
    pub parameters: HashMap<String, serde_json::Value>,
}

impl ModelConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn temperature(mut self, temp: f64) -> Self {
        self.default_temperature = Some(temp);
        self
    }

    pub fn max_tokens(mut self, tokens: usize) -> Self {
        self.default_max_tokens = Some(tokens);
        self
    }

    pub fn api_base(mut self, base: impl Into<String>) -> Self {
        self.api_base = Some(base.into());
        self
    }
}

/// Model registry for looking up capabilities
#[derive(Debug, Clone, Default)]
pub struct ModelRegistry {
    models: HashMap<String, Model>,
}

impl ModelRegistry {
    pub fn new() -> Self {
        let mut registry = Self::default();

        // Pre-register common models
        registry.register(Model::from_name("gpt-4").with_cost(ModelCost::gpt4_turbo()));
        registry.register(Model::from_name("gpt-4-turbo").with_cost(ModelCost::gpt4_turbo()));
        registry.register(Model::from_name("gpt-3.5-turbo").with_cost(ModelCost::gpt35_turbo()));
        registry.register(Model::from_name("claude-3-opus").with_cost(ModelCost::claude3_opus()));
        registry.register(Model::from_name("claude-3-sonnet").with_cost(ModelCost::claude3_sonnet()));
        registry.register(Model::from_name("claude-3-haiku").with_cost(ModelCost::claude3_haiku()));

        registry
    }

    /// Register a model
    pub fn register(&mut self, model: Model) {
        self.models.insert(model.name.clone(), model);
    }

    /// Get a model by name, creating default if not found
    pub fn get(&self, name: &str) -> Model {
        self.models.get(name).cloned().unwrap_or_else(|| Model::from_name(name))
    }

    /// Check if a model is registered
    pub fn contains(&self, name: &str) -> bool {
        self.models.contains_key(name)
    }

    /// List all registered model names
    pub fn names(&self) -> Vec<&str> {
        self.models.keys().map(|s| s.as_str()).collect()
    }
}

/// Global default registry
pub fn default_registry() -> ModelRegistry {
    ModelRegistry::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_model_from_name() {
        let model = Model::from_name("gpt-4-turbo");
        assert_eq!(model.name, "gpt-4-turbo");
        assert_eq!(model.provider, ModelProvider::OpenAI);
        assert!(model.capabilities.function_calling);
    }

    #[test]
    fn test_provider_inference() {
        assert_eq!(ModelProvider::infer_from_name("gpt-4"), ModelProvider::OpenAI);
        assert_eq!(ModelProvider::infer_from_name("claude-3-opus"), ModelProvider::Anthropic);
        assert_eq!(ModelProvider::infer_from_name("gemini-pro"), ModelProvider::Google);
        assert_eq!(ModelProvider::infer_from_name("mistral-large"), ModelProvider::Mistral);
    }

    #[test]
    fn test_capabilities_inference() {
        let caps = ModelCapabilities::infer_from_name("gpt-4-turbo");
        assert_eq!(caps.max_context_tokens, 128_000);
        assert!(caps.function_calling);
        assert!(caps.vision);

        let claude_caps = ModelCapabilities::infer_from_name("claude-3-opus");
        assert_eq!(claude_caps.max_context_tokens, 200_000);
        assert!(claude_caps.vision);
    }

    #[test]
    fn test_cost_calculation() {
        let model = Model::from_name("gpt-4").with_cost(ModelCost::gpt4_turbo());

        // 1000 input tokens, 500 output tokens
        let cost = model.estimate_cost(1000, 500).unwrap();

        // 1000/1M * $10 + 500/1M * $30 = $0.01 + $0.015 = $0.025
        assert!((cost - 0.025).abs() < 0.001);
    }

    #[test]
    fn test_context_fit() {
        let model = Model::from_name("gpt-4-turbo");
        assert!(model.fits_context(100_000));
        assert!(!model.fits_context(200_000));
    }

    #[test]
    fn test_model_registry() {
        let registry = ModelRegistry::new();

        assert!(registry.contains("gpt-4"));
        assert!(registry.contains("claude-3-opus"));

        let model = registry.get("gpt-4");
        assert_eq!(model.provider, ModelProvider::OpenAI);

        // Unknown model gets created with defaults
        let unknown = registry.get("some-unknown-model");
        assert_eq!(unknown.provider, ModelProvider::Custom);
    }

    #[test]
    fn test_latency_class() {
        assert_eq!(LatencyClass::Fast.suggested_timeout_ms(), 30_000);
        assert_eq!(LatencyClass::Slow.suggested_timeout_ms(), 120_000);
    }

    #[test]
    fn test_reliability_class() {
        assert_eq!(ReliabilityClass::High.suggested_retries(), 2);
        assert_eq!(ReliabilityClass::Low.suggested_retries(), 5);
    }

    #[test]
    fn test_model_config() {
        let config = ModelConfig::new()
            .temperature(0.7)
            .max_tokens(2048)
            .api_base("https://custom.api.com");

        assert_eq!(config.default_temperature, Some(0.7));
        assert_eq!(config.default_max_tokens, Some(2048));
        assert_eq!(config.api_base, Some("https://custom.api.com".to_string()));
    }

    #[test]
    fn test_capabilities_builder() {
        let caps = ModelCapabilities::default()
            .max_context(100_000)
            .max_output(8_000)
            .with_structured_output()
            .with_function_calling()
            .with_vision();

        assert_eq!(caps.max_context_tokens, 100_000);
        assert_eq!(caps.max_output_tokens, 8_000);
        assert!(caps.structured_output);
        assert!(caps.function_calling);
        assert!(caps.vision);
    }
}
