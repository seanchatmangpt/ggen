//! CLI integration for ggen-ai

use crate::client::LlmClient;
use crate::config::{get_global_config, GlobalLlmConfig, LlmProvider};
use crate::error::Result;
use clap::{Arg, ArgMatches, Command};
use std::sync::Arc;

/// CLI configuration builder for ggen-ai
pub struct CliConfigBuilder {
    command: Command,
}

impl CliConfigBuilder {
    /// Create a new CLI configuration builder
    pub fn new(name: &'static str) -> Self {
        Self {
            command: Command::new(name),
        }
    }

    /// Add LLM provider argument
    pub fn with_llm_provider(mut self) -> Self {
        self.command = self.command.arg(
            Arg::new("llm-provider")
                .long("llm-provider")
                .short('p')
                .value_name("PROVIDER")
                .help("LLM provider to use (openai, anthropic, ollama, mock)")
                .value_parser(["openai", "anthropic", "ollama", "mock"]),
        );
        self
    }

    /// Add model argument
    pub fn with_model(mut self) -> Self {
        self.command = self.command.arg(
            Arg::new("model")
                .long("model")
                .short('m')
                .value_name("MODEL")
                .help("Model name to use"),
        );
        self
    }

    /// Add temperature argument
    pub fn with_temperature(mut self) -> Self {
        self.command = self.command.arg(
            Arg::new("temperature")
                .long("temperature")
                .short('t')
                .value_name("TEMPERATURE")
                .help("Temperature for generation (0.0-2.0)")
                .value_parser(clap::value_parser!(f32)),
        );
        self
    }

    /// Add max tokens argument
    pub fn with_max_tokens(mut self) -> Self {
        self.command = self.command.arg(
            Arg::new("max-tokens")
                .long("max-tokens")
                .value_name("MAX_TOKENS")
                .help("Maximum tokens to generate")
                .value_parser(clap::value_parser!(u32)),
        );
        self
    }

    /// Add streaming argument
    pub fn with_streaming(mut self) -> Self {
        self.command = self.command.arg(
            Arg::new("streaming")
                .long("streaming")
                .short('s')
                .help("Enable streaming responses")
                .action(clap::ArgAction::SetTrue),
        );
        self
    }

    /// Add test mode argument
    pub fn with_test_mode(mut self) -> Self {
        self.command = self.command.arg(
            Arg::new("test-mode")
                .long("test-mode")
                .help("Use mock client for testing")
                .action(clap::ArgAction::SetTrue),
        );
        self
    }

    /// Build the command
    pub fn build(self) -> Command {
        self.command
    }
}

/// Extract LLM configuration from CLI arguments
pub fn extract_llm_config(matches: &ArgMatches) -> Result<GlobalLlmConfig> {
    let mut config = get_global_config().clone();

    // Provider selection
    if let Some(provider_str) = matches.get_one::<String>("llm-provider") {
        let provider = match provider_str.as_str() {
            "openai" => LlmProvider::OpenAI,
            "anthropic" => LlmProvider::Anthropic,
            "ollama" => LlmProvider::Ollama,
            "mock" => LlmProvider::Mock,
            _ => return Err(crate::error::GgenAiError::configuration("Invalid provider")),
        };
        config.set_provider(provider);
    }

    // Model selection
    if let Some(model) = matches.get_one::<String>("model") {
        config.settings.default_model = Some(model.clone());

        // Update the current provider's config
        if let Some(provider_config) = config.providers.get_mut(&config.provider) {
            provider_config.model = model.clone();
        }
    }

    // Temperature
    if let Some(temperature) = matches.get_one::<f32>("temperature") {
        config.settings.default_temperature = Some(*temperature);

        // Update the current provider's config
        if let Some(provider_config) = config.providers.get_mut(&config.provider) {
            provider_config.temperature = Some(*temperature);
        }
    }

    // Max tokens
    if let Some(max_tokens) = matches.get_one::<u32>("max-tokens") {
        config.settings.default_max_tokens = Some(*max_tokens);

        // Update the current provider's config
        if let Some(provider_config) = config.providers.get_mut(&config.provider) {
            provider_config.max_tokens = Some(*max_tokens);
        }
    }

    // Streaming
    if matches.get_flag("streaming") {
        config.settings.use_streaming = true;
    }

    // Test mode
    if matches.get_flag("test-mode") {
        config.set_provider(LlmProvider::Mock);
    }

    Ok(config)
}

/// Create a client from CLI arguments
pub fn create_client_from_args(matches: &ArgMatches) -> Result<Arc<dyn LlmClient>> {
    let config = extract_llm_config(matches)?;
    config.create_contextual_client()
}

/// Create a client with specific configuration
pub fn create_client_with_config(config: &GlobalLlmConfig) -> Result<Arc<dyn LlmClient>> {
    config.create_contextual_client()
}

/// Helper function to add common LLM arguments to any command
pub fn add_llm_args(command: Command) -> Command {
    CliConfigBuilder::new("")
        .with_llm_provider()
        .with_model()
        .with_temperature()
        .with_max_tokens()
        .with_streaming()
        .with_test_mode()
        .build()
        .get_arguments()
        .fold(command, |cmd, arg| cmd.arg(arg.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cli_config_builder() {
        let cmd = CliConfigBuilder::new("test")
            .with_llm_provider()
            .with_model()
            .with_temperature()
            .build();

        assert!(cmd
            .get_arguments()
            .any(|arg| arg.get_id() == "llm-provider"));
        assert!(cmd.get_arguments().any(|arg| arg.get_id() == "model"));
        assert!(cmd.get_arguments().any(|arg| arg.get_id() == "temperature"));
    }

    #[test]
    fn test_extract_llm_config() {
        let cmd = CliConfigBuilder::new("test")
            .with_llm_provider()
            .with_model()
            .with_temperature()
            .with_max_tokens()
            .with_streaming()
            .with_test_mode()
            .build();

        let matches = cmd
            .try_get_matches_from(&["test", "--llm-provider", "ollama", "--model", "llama3.2"])
            .expect("CLI parsing should succeed in test");
        let config =
            extract_llm_config(&matches).expect("Config extraction should succeed in test");

        assert_eq!(config.provider, LlmProvider::Ollama);
        if let Some(provider_config) = config.providers.get(&LlmProvider::Ollama) {
            assert_eq!(provider_config.model, "llama3.2");
        }
    }
}
