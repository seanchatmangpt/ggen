//! Optimization algorithms for DSPy modules
//!
//! This module provides self-improvement algorithms that optimize module
//! parameters and prompts based on training data.

use crate::{Module, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub mod bootstrap;
pub mod mipro;

pub use bootstrap::BootstrapFewShot;
pub use mipro::MiproOptimizer;

/// Optimizer trait for module optimization
#[async_trait]
pub trait Optimizer: Send + Sync {
    /// Compile/optimize a module with training data
    async fn compile(
        &self,
        module: Box<dyn Module>,
        trainset: &[TrainExample],
    ) -> Result<Box<dyn Module>>;

    /// Get optimizer name
    fn name(&self) -> &str;
}

/// Configuration for optimizers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizerConfig {
    /// Number of training examples to use
    pub num_examples: usize,

    /// Maximum iterations for optimization
    pub max_iterations: usize,

    /// Convergence threshold (0.0 to 1.0)
    pub convergence_threshold: f64,

    /// Enable verbose logging
    pub verbose: bool,

    /// Random seed for reproducibility
    pub seed: Option<u64>,
}

impl Default for OptimizerConfig {
    fn default() -> Self {
        Self {
            num_examples: 5,
            max_iterations: 10,
            convergence_threshold: 0.01,
            verbose: false,
            seed: None,
        }
    }
}

impl OptimizerConfig {
    /// Create a new optimizer config
    pub fn new() -> Self {
        Self::default()
    }

    /// Set number of examples
    pub fn with_num_examples(mut self, num: usize) -> Self {
        self.num_examples = num;
        self
    }

    /// Set max iterations
    pub fn with_max_iterations(mut self, max: usize) -> Self {
        self.max_iterations = max;
        self
    }

    /// Set convergence threshold
    pub fn with_convergence_threshold(mut self, threshold: f64) -> Self {
        self.convergence_threshold = threshold;
        self
    }

    /// Enable verbose mode
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Set random seed
    pub fn with_seed(mut self, seed: u64) -> Self {
        self.seed = Some(seed);
        self
    }
}

/// Training example for optimization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainExample {
    /// Input fields
    pub inputs: HashMap<String, String>,

    /// Expected output fields
    pub outputs: HashMap<String, String>,
}

impl TrainExample {
    /// Create a new training example
    pub fn new(
        inputs: HashMap<String, String>,
        outputs: HashMap<String, String>,
    ) -> Self {
        Self { inputs, outputs }
    }

    /// Create from key-value pairs
    pub fn from_pairs(
        inputs: &[(&str, &str)],
        outputs: &[(&str, &str)],
    ) -> Self {
        let inputs = inputs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        let outputs = outputs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        Self::new(inputs, outputs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_optimizer_config() {
        let config = OptimizerConfig::new()
            .with_num_examples(10)
            .with_max_iterations(20)
            .with_verbose(true);

        assert_eq!(config.num_examples, 10);
        assert_eq!(config.max_iterations, 20);
        assert!(config.verbose);
    }

    #[test]
    fn test_train_example() {
        let example = TrainExample::from_pairs(
            &[("question", "What is Rust?")],
            &[("answer", "A systems programming language")],
        );

        assert_eq!(example.inputs.get("question").map(|s| s.as_str()), Some("What is Rust?"));
        assert_eq!(example.outputs.get("answer").map(|s| s.as_str()), Some("A systems programming language"));
    }
}
