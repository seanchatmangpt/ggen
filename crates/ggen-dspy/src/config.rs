//! Configuration types for modules and optimizers

use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Main DSPy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DspyConfig {
    /// Cache configuration
    pub cache: CacheConfig,

    /// Module configuration
    pub module: ModuleConfig,

    /// Optimizer parameters
    pub optimizer: OptimizerParams,
}

impl Default for DspyConfig {
    fn default() -> Self {
        Self {
            cache: CacheConfig::default(),
            module: ModuleConfig::default(),
            optimizer: OptimizerParams::default(),
        }
    }
}

/// Cache configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheConfig {
    /// Enable caching
    pub enabled: bool,

    /// Maximum cache size in entries
    pub max_size: usize,

    /// Time-to-live in seconds
    pub ttl_seconds: u64,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            max_size: 1000,
            ttl_seconds: 3600, // 1 hour
        }
    }
}

impl CacheConfig {
    /// Get TTL as Duration
    pub fn ttl(&self) -> Duration {
        Duration::from_secs(self.ttl_seconds)
    }
}

/// Module configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleConfig {
    /// Temperature for LLM sampling
    pub temperature: f64,

    /// Maximum tokens for generation
    pub max_tokens: usize,

    /// Enable verbose logging
    pub verbose: bool,
}

impl Default for ModuleConfig {
    fn default() -> Self {
        Self {
            temperature: 0.7,
            max_tokens: 1024,
            verbose: false,
        }
    }
}

/// Optimizer parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizerParams {
    /// Number of bootstrap examples
    pub num_examples: usize,

    /// Maximum optimization iterations
    pub max_iterations: usize,

    /// Convergence threshold
    pub convergence_threshold: f64,

    /// Enable early stopping
    pub early_stopping: bool,
}

impl Default for OptimizerParams {
    fn default() -> Self {
        Self {
            num_examples: 5,
            max_iterations: 10,
            convergence_threshold: 0.01,
            early_stopping: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = DspyConfig::default();
        assert!(config.cache.enabled);
        assert_eq!(config.module.max_tokens, 1024);
        assert_eq!(config.optimizer.num_examples, 5);
    }

    #[test]
    fn test_cache_ttl() {
        let config = CacheConfig::default();
        assert_eq!(config.ttl(), Duration::from_secs(3600));
    }
}
