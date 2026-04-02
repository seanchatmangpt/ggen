//! Core library for advanced lifecycle example
//!
//! Demonstrates: proper error handling, logging, and testable design

use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Configuration for the application
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub name: String,
    pub version: String,
    pub enabled: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            name: "example-app".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            enabled: true,
        }
    }
}

/// Core processor that demonstrates lifecycle usage
pub struct Processor {
    config: Config,
}

impl Processor {
    pub fn new(config: Config) -> Self {
        tracing::info!(
            name = %config.name,
            version = %config.version,
            "Initializing processor"
        );
        Self { config }
    }

    pub fn process(&self, input: &str) -> Result<String> {
        if !self.config.enabled {
            anyhow::bail!("Processor is disabled");
        }

        tracing::debug!(input = %input, "Processing input");

        // Simple transformation for demonstration
        let result = input.to_uppercase();

        tracing::info!(
            input_len = input.len(),
            output_len = result.len(),
            "Processing complete"
        );

        Ok(result)
    }

    pub fn batch_process(&self, inputs: &[String]) -> Result<Vec<String>> {
        inputs.iter().map(|s| self.process(s)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_processor_basic() {
        let config = Config::default();
        let processor = Processor::new(config);

        let result = processor.process("hello").unwrap();
        assert_eq!(result, "HELLO");
    }

    #[test]
    fn test_processor_disabled() {
        let mut config = Config::default();
        config.enabled = false;
        let processor = Processor::new(config);

        assert!(processor.process("hello").is_err());
    }

    #[test]
    fn test_batch_processing() {
        let config = Config::default();
        let processor = Processor::new(config);

        let inputs = vec!["hello".to_string(), "world".to_string()];
        let results = processor.batch_process(&inputs).unwrap();

        assert_eq!(results, vec!["HELLO", "WORLD"]);
    }
}
