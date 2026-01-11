//! Configuration system for DSPy modules
//!
//! Provides:
//! - Global settings with validation
//! - Thread-local context overrides
//! - Cache management (memory + disk)
//! - Usage tracking

use crate::DspyError;
use serde::{Deserialize, Serialize};
use std::sync::OnceLock;
use std::time::Duration;

pub mod cache;
pub mod context;
pub mod usage;

pub use cache::{CacheConfig, CacheManager, CacheStats};
pub use context::{with_context, ContextBuilder};
pub use usage::{UsageStats, UsageTracker};

/// Global DSPy settings singleton
static GLOBAL_SETTINGS: OnceLock<DspySettings> = OnceLock::new();

/// DSPy settings with LM configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DspySettings {
    /// Default temperature for LLM sampling (0.0-2.0)
    pub default_temperature: f64,

    /// Default top_p for nucleus sampling (0.0-1.0)
    pub default_top_p: f64,

    /// Default maximum tokens for generation
    pub default_max_tokens: usize,

    /// Request timeout in seconds
    pub timeout_seconds: u64,

    /// Enable response streaming
    pub use_streaming: bool,

    /// Track token usage and costs
    pub track_usage: bool,

    /// Enable caching by default
    pub enable_cache: bool,
}

impl Default for DspySettings {
    fn default() -> Self {
        Self {
            default_temperature: 0.7,
            default_top_p: 0.9,
            default_max_tokens: 1024,
            timeout_seconds: 120,
            use_streaming: false,
            track_usage: false,
            enable_cache: true,
        }
    }
}

impl DspySettings {
    /// Create new settings with defaults
    pub fn new() -> Self {
        Self::default()
    }

    /// Set default temperature (0.0-2.0)
    pub fn with_temperature(mut self, temperature: f64) -> Self {
        self.default_temperature = temperature;
        self
    }

    /// Set default max tokens
    pub fn with_max_tokens(mut self, max_tokens: usize) -> Self {
        self.default_max_tokens = max_tokens;
        self
    }

    /// Set default top_p (0.0-1.0)
    pub fn with_top_p(mut self, top_p: f64) -> Self {
        self.default_top_p = top_p;
        self
    }

    /// Enable or disable streaming
    pub fn with_streaming(mut self, enabled: bool) -> Self {
        self.use_streaming = enabled;
        self
    }

    /// Set timeout in seconds
    pub fn with_timeout(mut self, seconds: u64) -> Self {
        self.timeout_seconds = seconds;
        self
    }

    /// Enable or disable usage tracking
    pub fn with_usage_tracking(mut self, enabled: bool) -> Self {
        self.track_usage = enabled;
        self
    }

    /// Enable or disable caching
    pub fn with_cache(mut self, enabled: bool) -> Self {
        self.enable_cache = enabled;
        self
    }

    /// Validate settings
    pub fn validate(&self) -> Result<(), DspyError> {
        if !(0.0..=2.0).contains(&self.default_temperature) {
            return Err(DspyError::config_error(format!(
                "Temperature must be in range 0.0-2.0, got {}",
                self.default_temperature
            )));
        }

        if !(0.0..=1.0).contains(&self.default_top_p) {
            return Err(DspyError::config_error(format!(
                "Top-p must be in range 0.0-1.0, got {}",
                self.default_top_p
            )));
        }

        if self.default_max_tokens == 0 {
            return Err(DspyError::config_error(
                "Max tokens must be greater than 0",
            ));
        }

        if self.timeout_seconds == 0 {
            return Err(DspyError::config_error("Timeout must be greater than 0"));
        }

        Ok(())
    }

    /// Get timeout as Duration
    pub fn timeout(&self) -> Duration {
        Duration::from_secs(self.timeout_seconds)
    }

    /// Get ggen-ai global LLM config
    pub fn llm_config(&self) -> &'static ggen_ai::GlobalLlmConfig {
        ggen_ai::get_global_config()
    }
}

/// Initialize global DSPy configuration
pub fn init_dspy_config() -> &'static DspySettings {
    GLOBAL_SETTINGS.get_or_init(|| {
        let settings = DspySettings::default();
        settings.validate().expect("Invalid default settings");
        settings
    })
}

/// Get global DSPy configuration
pub fn get_dspy_config() -> &'static DspySettings {
    init_dspy_config()
}

/// Configure global DSPy settings
pub fn configure_dspy(settings: DspySettings) -> Result<(), DspyError> {
    settings.validate()?;
    GLOBAL_SETTINGS
        .set(settings)
        .map_err(|_| DspyError::config_error("Configuration already initialized"))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_settings() {
        let settings = DspySettings::default();
        assert!(settings.validate().is_ok());
        assert_eq!(settings.default_temperature, 0.7);
        assert_eq!(settings.default_max_tokens, 1024);
    }

    #[test]
    fn test_builder_pattern() {
        let settings = DspySettings::new()
            .with_temperature(0.5)
            .with_max_tokens(4096)
            .with_top_p(0.95)
            .with_streaming(true)
            .with_timeout(60)
            .with_usage_tracking(true);

        assert_eq!(settings.default_temperature, 0.5);
        assert_eq!(settings.default_max_tokens, 4096);
        assert_eq!(settings.default_top_p, 0.95);
        assert!(settings.use_streaming);
        assert_eq!(settings.timeout_seconds, 60);
        assert!(settings.track_usage);
    }

    #[test]
    fn test_validation() {
        // Valid settings
        assert!(DspySettings::default().validate().is_ok());

        // Invalid temperature
        let invalid_temp = DspySettings::new().with_temperature(3.0);
        assert!(invalid_temp.validate().is_err());

        // Invalid top_p
        let invalid_top_p = DspySettings::new().with_top_p(1.5);
        assert!(invalid_top_p.validate().is_err());

        // Invalid max_tokens
        let invalid_tokens = DspySettings::new().with_max_tokens(0);
        assert!(invalid_tokens.validate().is_err());

        // Invalid timeout
        let invalid_timeout = DspySettings::new().with_timeout(0);
        assert!(invalid_timeout.validate().is_err());
    }
}
