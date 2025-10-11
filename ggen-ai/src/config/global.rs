//! Global LLM configuration management

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use crate::{AnthropicClient, MockClient, OllamaClient, OpenAIClient};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

/// Global LLM configuration manager
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlobalLlmConfig {
    /// Default provider to use
    pub provider: LlmProvider,
    /// Provider-specific configurations
    pub providers: HashMap<LlmProvider, LlmConfig>,
    /// Global settings
    pub settings: GlobalSettings,
}

/// Supported LLM providers
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum LlmProvider {
    /// OpenAI (GPT models)
    OpenAI,
    /// Anthropic (Claude models)
    Anthropic,
    /// Ollama (local models)
    Ollama,
    /// Mock provider for testing
    Mock,
}

/// Global settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlobalSettings {
    /// Default model name
    pub default_model: Option<String>,
    /// Default temperature
    pub default_temperature: Option<f32>,
    /// Default max tokens
    pub default_max_tokens: Option<u32>,
    /// Default top-p
    pub default_top_p: Option<f32>,
    /// Whether to use streaming by default
    pub use_streaming: bool,
    /// Timeout in seconds
    pub timeout_seconds: Option<u32>,
}

impl Default for GlobalLlmConfig {
    fn default() -> Self {
        let mut providers = HashMap::new();

        use crate::constants::{env_vars, llm, models, testing};

        // OpenAI defaults
        providers.insert(
            LlmProvider::OpenAI,
            LlmConfig {
                model: std::env::var(env_vars::OPENAI_MODEL)
                    .or_else(|_| std::env::var(env_vars::DEFAULT_MODEL))
                    .or_else(|_| std::env::var("DEFAULT_MODEL"))
                    .unwrap_or_else(|_| models::OPENAI_DEFAULT.to_string()),
                max_tokens: Some(llm::DEFAULT_MAX_TOKENS),
                temperature: Some(llm::DEFAULT_TEMPERATURE),
                top_p: Some(llm::DEFAULT_TOP_P),
                stop: None,
                extra: HashMap::new(),
            },
        );

        // Anthropic defaults
        providers.insert(
            LlmProvider::Anthropic,
            LlmConfig {
                model: std::env::var(env_vars::ANTHROPIC_MODEL)
                    .or_else(|_| std::env::var(env_vars::DEFAULT_MODEL))
                    .or_else(|_| std::env::var("DEFAULT_MODEL"))
                    .unwrap_or_else(|_| models::ANTHROPIC_DEFAULT.to_string()),
                max_tokens: Some(llm::DEFAULT_MAX_TOKENS),
                temperature: Some(llm::DEFAULT_TEMPERATURE),
                top_p: Some(llm::DEFAULT_TOP_P),
                stop: None,
                extra: HashMap::new(),
            },
        );

        // Ollama defaults
        providers.insert(
            LlmProvider::Ollama,
            LlmConfig {
                model: std::env::var(env_vars::OLLAMA_MODEL)
                    .or_else(|_| std::env::var(env_vars::DEFAULT_MODEL))
                    .or_else(|_| std::env::var("DEFAULT_MODEL"))
                    .unwrap_or_else(|_| models::OLLAMA_DEFAULT.to_string()),
                max_tokens: Some(llm::DEFAULT_MAX_TOKENS),
                temperature: Some(llm::DEFAULT_TEMPERATURE),
                top_p: Some(llm::DEFAULT_TOP_P),
                stop: None,
                extra: HashMap::new(),
            },
        );

        // Mock defaults
        providers.insert(
            LlmProvider::Mock,
            LlmConfig {
                model: models::MOCK_MODEL.to_string(),
                max_tokens: Some(testing::MOCK_MAX_TOKENS),
                temperature: Some(testing::MOCK_TEMPERATURE),
                top_p: Some(testing::MOCK_TOP_P),
                stop: None,
                extra: HashMap::new(),
            },
        );

        Self {
            provider: Self::detect_available_provider(), // Auto-detect available provider
            providers,
            settings: GlobalSettings {
                default_model: None,
                default_temperature: Some(llm::DEFAULT_TEMPERATURE),
                default_max_tokens: Some(2048),
                default_top_p: Some(llm::DEFAULT_TOP_P),
                use_streaming: false,
                timeout_seconds: Some(llm::DEFAULT_TIMEOUT_SECS),
            },
        }
    }
}

impl GlobalLlmConfig {
    /// Auto-detect best available LLM provider
    fn detect_available_provider() -> LlmProvider {
        // 1. Check for explicit environment variable
        if let Ok(provider) = std::env::var("GGEN_LLM_PROVIDER") {
            return match provider.to_lowercase().as_str() {
                "openai" => LlmProvider::OpenAI,
                "anthropic" => LlmProvider::Anthropic,
                "ollama" => LlmProvider::Ollama,
                "mock" => LlmProvider::Mock,
                _ => LlmProvider::Ollama,
            };
        }

        // 2. Prefer Ollama if available locally (default for ggen)
        if Self::check_ollama_available() {
            return LlmProvider::Ollama;
        }

        // 3. Check for API keys as fallback
        if std::env::var("OPENAI_API_KEY").is_ok() {
            return LlmProvider::OpenAI;
        }
        if std::env::var("ANTHROPIC_API_KEY").is_ok() {
            return LlmProvider::Anthropic;
        }

        // 4. Fallback to Mock provider with warning
        eprintln!("Warning: No LLM provider detected. Using Mock provider.");
        eprintln!("Set OPENAI_API_KEY, ANTHROPIC_API_KEY, or install Ollama.");
        eprintln!("To use Ollama, start it with: ollama serve");
        LlmProvider::Mock
    }

    /// Check if Ollama is running locally
    fn check_ollama_available() -> bool {
        // Try to connect to Ollama's health endpoint
        std::process::Command::new("curl")
            .args(&[
                "-s",
                "-o",
                "/dev/null",
                "-w",
                "%{http_code}",
                "http://localhost:11434/api/tags",
            ])
            .output()
            .ok()
            .and_then(|output| String::from_utf8(output.stdout).ok())
            .map(|status| status == "200")
            .unwrap_or(false)
    }

    /// Create a new global configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Load configuration from environment variables
    pub fn from_env() -> Self {
        let mut config = Self::default();

        // Provider selection
        if let Ok(provider) = std::env::var("GGEN_LLM_PROVIDER") {
            config.provider = match provider.to_lowercase().as_str() {
                "openai" => LlmProvider::OpenAI,
                "anthropic" => LlmProvider::Anthropic,
                "ollama" => LlmProvider::Ollama,
                "mock" => LlmProvider::Mock,
                _ => LlmProvider::Ollama,
            };
        }

        // Model selection
        if let Ok(model) = std::env::var("GGEN_LLM_MODEL") {
            config.settings.default_model = Some(model);
        }

        // Temperature
        if let Ok(temp) = std::env::var("GGEN_LLM_TEMPERATURE") {
            if let Ok(temp_val) = temp.parse::<f32>() {
                config.settings.default_temperature = Some(temp_val);
            }
        }

        // Max tokens
        if let Ok(tokens) = std::env::var("GGEN_LLM_MAX_TOKENS") {
            if let Ok(tokens_val) = tokens.parse::<u32>() {
                config.settings.default_max_tokens = Some(tokens_val);
            }
        }

        // Streaming
        if let Ok(streaming) = std::env::var("GGEN_LLM_STREAMING") {
            config.settings.use_streaming = streaming.to_lowercase() == "true" || streaming == "1";
        }

        // Update provider configs with global settings
        for (_, provider_config) in config.providers.iter_mut() {
            if let Some(ref model) = config.settings.default_model {
                provider_config.model = model.clone();
            }
            if let Some(temp) = config.settings.default_temperature {
                provider_config.temperature = Some(temp);
            }
            if let Some(max_tokens) = config.settings.default_max_tokens {
                provider_config.max_tokens = Some(max_tokens);
            }
            if let Some(top_p) = config.settings.default_top_p {
                provider_config.top_p = Some(top_p);
            }
        }

        config
    }

    /// Get configuration for a specific provider
    pub fn get_provider_config(&self, provider: &LlmProvider) -> Option<&LlmConfig> {
        self.providers.get(provider)
    }

    /// Get configuration for the default provider
    pub fn get_default_config(&self) -> Option<&LlmConfig> {
        self.get_provider_config(&self.provider)
    }

    /// Set the default provider
    pub fn set_provider(&mut self, provider: LlmProvider) {
        self.provider = provider;
    }

    /// Update provider configuration
    pub fn update_provider_config(&mut self, provider: LlmProvider, config: LlmConfig) {
        self.providers.insert(provider, config);
    }

    /// Create an LLM client for the default provider
    pub fn create_client(&self) -> Result<Arc<dyn LlmClient>> {
        self.create_provider_client(&self.provider)
    }

    /// Create an LLM client for a specific provider
    pub fn create_provider_client(&self, provider: &LlmProvider) -> Result<Arc<dyn LlmClient>> {
        let config = self
            .get_provider_config(provider)
            .ok_or_else(|| {
                GgenAiError::configuration(&format!(
                    "No configuration found for provider: {:?}",
                    provider
                ))
            })?
            .clone();

        match provider {
            LlmProvider::OpenAI => {
                let client = OpenAIClient::new(config)?;
                Ok(Arc::new(client))
            }
            LlmProvider::Anthropic => {
                let client = AnthropicClient::new(config)?;
                Ok(Arc::new(client))
            }
            LlmProvider::Ollama => {
                let client = OllamaClient::new(config)?;
                Ok(Arc::new(client))
            }
            LlmProvider::Mock => {
                let client = MockClient::with_response("Mock response");
                Ok(Arc::new(client))
            }
        }
    }

    /// Create a mock client for testing
    pub fn create_mock_client(&self, response: &str) -> Arc<dyn LlmClient> {
        Arc::new(MockClient::with_response(response))
    }

    /// Check if we're in test mode
    pub fn is_test_mode(&self) -> bool {
        std::env::var("GGEN_TEST_MODE").is_ok() || cfg!(test)
    }

    /// Create a client appropriate for the current context (test vs production)
    pub fn create_contextual_client(&self) -> Result<Arc<dyn LlmClient>> {
        if self.is_test_mode() {
            Ok(self.create_mock_client("Mock response for testing"))
        } else {
            self.create_client()
        }
    }

    /// Get the name of the current provider as a string
    pub fn provider_name(&self) -> &'static str {
        match self.provider {
            LlmProvider::OpenAI => "OpenAI",
            LlmProvider::Anthropic => "Anthropic",
            LlmProvider::Ollama => "Ollama",
            LlmProvider::Mock => "Mock",
        }
    }
}

/// Global configuration instance
static mut GLOBAL_CONFIG: Option<GlobalLlmConfig> = None;
static INIT: std::sync::Once = std::sync::Once::new();

/// Initialize the global configuration
pub fn init_global_config() -> &'static GlobalLlmConfig {
    unsafe {
        INIT.call_once(|| {
            GLOBAL_CONFIG = Some(GlobalLlmConfig::from_env());
        });
        GLOBAL_CONFIG
            .as_ref()
            .expect("Global config should be initialized after call_once")
    }
}

/// Get the global configuration
pub fn get_global_config() -> &'static GlobalLlmConfig {
    unsafe {
        if GLOBAL_CONFIG.is_none() {
            init_global_config();
        }
        GLOBAL_CONFIG
            .as_ref()
            .expect("Global config should be initialized")
    }
}

/// Set the global configuration
pub fn set_global_config(config: GlobalLlmConfig) {
    unsafe {
        GLOBAL_CONFIG = Some(config);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = GlobalLlmConfig::default();
        assert_eq!(config.provider, LlmProvider::Ollama);
        assert!(config.providers.contains_key(&LlmProvider::OpenAI));
        assert!(config.providers.contains_key(&LlmProvider::Anthropic));
        assert!(config.providers.contains_key(&LlmProvider::Ollama));
        assert!(config.providers.contains_key(&LlmProvider::Mock));
    }

    #[test]
    fn test_provider_config_access() {
        let config = GlobalLlmConfig::default();
        let ollama_config = config.get_provider_config(&LlmProvider::Ollama);
        assert!(ollama_config.is_some());
        // Updated to match current default model
        assert_eq!(ollama_config.unwrap().model, "qwen3-coder:30b");
    }

    #[test]
    fn test_mock_client_creation() {
        let config = GlobalLlmConfig::default();
        let client = config.create_mock_client("Test response");
        assert!(client.get_config().model == "mock-model");
    }
}
