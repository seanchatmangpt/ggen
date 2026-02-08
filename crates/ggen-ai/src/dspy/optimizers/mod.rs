//! DSPy Optimizer Infrastructure
//!
//! Provides optimization strategies for DSPy modules:
//! - `Optimizer` trait - Common interface for all optimizers
//! - `Metric` trait - Dual-mode evaluation (bool/float)
//! - `Trace` - Execution trace support for advanced metrics
//! - `OptimizationStatistics` - Performance tracking

use crate::dspy::optimizer::Example;
use crate::dspy::{Module, ModuleError};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::time::SystemTime;

pub mod bootstrap_random_search;
pub mod copro;
pub mod knn_fewshot;
pub mod labeled_fewshot;

pub use bootstrap_random_search::BootstrapFewShotWithRandomSearch;
pub use copro::COPRO;
pub use knn_fewshot::{CosineVectorizer, KNNFewShot, Vectorizer};
pub use labeled_fewshot::LabeledFewShot;

/// Execution trace step for advanced metric validation
#[derive(Debug, Clone)]
pub struct TraceStep {
    /// Name of the predictor that executed this step
    pub predictor_name: String,

    /// Input values for this step
    pub inputs: HashMap<String, Value>,

    /// Output values from this step
    pub outputs: HashMap<String, Value>,

    /// Timestamp when step executed
    pub timestamp: SystemTime,
}

/// Complete execution trace
#[derive(Debug, Clone)]
pub struct Trace {
    /// Ordered sequence of execution steps
    pub steps: Vec<TraceStep>,

    /// Total tokens used (if available)
    pub total_tokens: Option<usize>,

    /// Total latency in milliseconds
    pub latency_ms: Option<u64>,
}

impl Trace {
    /// Create empty trace
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            total_tokens: None,
            latency_ms: None,
        }
    }

    /// Add a step to the trace
    pub fn add_step(&mut self, step: TraceStep) {
        self.steps.push(step);
    }
}

impl Default for Trace {
    fn default() -> Self {
        Self::new()
    }
}

/// Optimization statistics for tracking performance
#[derive(Debug, Clone)]
pub struct OptimizationStatistics {
    /// Total number of examples attempted
    pub total_attempts: usize,

    /// Number of successful demonstrations collected
    pub successful_demos: usize,

    /// Number of failed attempts
    pub failed_demos: usize,

    /// Average metric score (0.0-1.0)
    pub avg_metric_score: f64,

    /// Total optimization time in milliseconds
    pub optimization_time_ms: u64,

    /// Additional metadata
    pub metadata: HashMap<String, Value>,
}

impl OptimizationStatistics {
    /// Create new statistics tracker
    pub fn new() -> Self {
        Self {
            total_attempts: 0,
            successful_demos: 0,
            failed_demos: 0,
            avg_metric_score: 0.0,
            optimization_time_ms: 0,
            metadata: HashMap::new(),
        }
    }

    /// Calculate success rate
    pub fn success_rate(&self) -> f64 {
        if self.total_attempts == 0 {
            0.0
        } else {
            self.successful_demos as f64 / self.total_attempts as f64
        }
    }
}

impl Default for OptimizationStatistics {
    fn default() -> Self {
        Self::new()
    }
}

/// Metric trait for dual-mode evaluation
///
/// Supports both:
/// - `evaluate`: Returns numerical score (0.0-1.0) for optimization
/// - `validate`: Returns boolean for bootstrapping with optional trace
#[async_trait]
pub trait Metric: Send + Sync {
    /// Evaluate example with numerical score (0.0-1.0, higher is better)
    ///
    /// Used during evaluation and optimization phases.
    fn evaluate(
        &self, example: &Example, output: &HashMap<String, Value>,
    ) -> Result<f64, ModuleError>;

    /// Validate example with boolean pass/fail
    ///
    /// Used during bootstrapping to filter demonstrations.
    /// Optional trace parameter enables intermediate step validation.
    fn validate(
        &self, example: &Example, output: &HashMap<String, Value>, trace: Option<&Trace>,
    ) -> Result<bool, ModuleError>;
}

/// Simple exact match metric
pub struct ExactMatchMetric {
    field_name: String,
}

impl ExactMatchMetric {
    /// Create new exact match metric for specified output field
    pub fn new(field_name: impl Into<String>) -> Self {
        Self {
            field_name: field_name.into(),
        }
    }
}

#[async_trait]
impl Metric for ExactMatchMetric {
    fn evaluate(
        &self, example: &Example, output: &HashMap<String, Value>,
    ) -> Result<f64, ModuleError> {
        let expected = example.outputs.get(&self.field_name);
        let actual = output.get(&self.field_name);

        Ok(if expected == actual { 1.0 } else { 0.0 })
    }

    fn validate(
        &self, example: &Example, output: &HashMap<String, Value>, _trace: Option<&Trace>,
    ) -> Result<bool, ModuleError> {
        let expected = example.outputs.get(&self.field_name);
        let actual = output.get(&self.field_name);

        Ok(expected == actual)
    }
}

/// Fuzzy match metric using string similarity
pub struct FuzzyMatchMetric {
    field_name: String,
    threshold: f64,
}

impl FuzzyMatchMetric {
    /// Create new fuzzy match metric
    ///
    /// # Arguments
    /// * `field_name` - Output field to compare
    /// * `threshold` - Similarity threshold (0.0-1.0), default 0.8
    pub fn new(field_name: impl Into<String>, threshold: f64) -> Self {
        Self {
            field_name: field_name.into(),
            threshold: threshold.clamp(0.0, 1.0),
        }
    }
}

#[async_trait]
impl Metric for FuzzyMatchMetric {
    fn evaluate(
        &self, example: &Example, output: &HashMap<String, Value>,
    ) -> Result<f64, ModuleError> {
        let expected = example
            .outputs
            .get(&self.field_name)
            .and_then(|v| v.as_str())
            .unwrap_or("");
        let actual = output
            .get(&self.field_name)
            .and_then(|v| v.as_str())
            .unwrap_or("");

        // Simple normalized Levenshtein distance
        let distance = levenshtein_distance(expected, actual);
        let max_len = expected.len().max(actual.len());

        if max_len == 0 {
            return Ok(1.0);
        }

        let similarity = 1.0 - (distance as f64 / max_len as f64);
        Ok(similarity)
    }

    fn validate(
        &self, example: &Example, output: &HashMap<String, Value>, _trace: Option<&Trace>,
    ) -> Result<bool, ModuleError> {
        let score = self.evaluate(example, output)?;
        Ok(score >= self.threshold)
    }
}

/// Calculate Levenshtein distance between two strings
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();

    if len1 == 0 {
        return len2;
    }
    if len2 == 0 {
        return len1;
    }

    let mut matrix = vec![vec![0usize; len2 + 1]; len1 + 1];

    for i in 0..=len1 {
        matrix[i][0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let cost = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = (matrix[i][j + 1] + 1)
                .min(matrix[i + 1][j] + 1)
                .min(matrix[i][j] + cost);
        }
    }

    matrix[len1][len2]
}

/// Optimizer trait - common interface for all optimizers
#[async_trait]
pub trait Optimizer: Send + Sync {
    /// Compile/optimize a student module using training examples
    ///
    /// # Arguments
    /// * `student` - The module to optimize
    /// * `trainset` - Training examples with inputs and expected outputs
    ///
    /// # Returns
    /// An optimized module ready for deployment
    async fn compile(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<Box<dyn Module>, ModuleError>;

    /// Compile with detailed statistics tracking
    ///
    /// Returns both the optimized module and performance statistics.
    async fn compile_with_stats(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<(Box<dyn Module>, OptimizationStatistics), ModuleError> {
        let start = SystemTime::now();
        let optimized = self.compile(student, trainset).await?;
        let elapsed = start.elapsed().unwrap_or_default().as_millis() as u64;

        let stats = OptimizationStatistics {
            total_attempts: trainset.len(),
            successful_demos: 0, // Subclasses should override
            failed_demos: 0,
            avg_metric_score: 0.0,
            optimization_time_ms: elapsed,
            metadata: HashMap::new(),
        };

        Ok((optimized, stats))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trace_creation() {
        let trace = Trace::new();
        assert_eq!(trace.steps.len(), 0);
        assert!(trace.total_tokens.is_none());
        assert!(trace.latency_ms.is_none());
    }

    #[test]
    fn test_trace_add_step() {
        let mut trace = Trace::new();

        let step = TraceStep {
            predictor_name: "test".to_string(),
            inputs: HashMap::new(),
            outputs: HashMap::new(),
            timestamp: SystemTime::now(),
        };

        trace.add_step(step);
        assert_eq!(trace.steps.len(), 1);
    }

    #[test]
    fn test_optimization_statistics() {
        let stats = OptimizationStatistics::new();
        assert_eq!(stats.total_attempts, 0);
        assert_eq!(stats.successful_demos, 0);
        assert_eq!(stats.success_rate(), 0.0);
    }

    #[test]
    fn test_optimization_statistics_success_rate() {
        let mut stats = OptimizationStatistics::new();
        stats.total_attempts = 10;
        stats.successful_demos = 8;

        assert_eq!(stats.success_rate(), 0.8);
    }

    #[test]
    fn test_exact_match_metric() {
        let metric = ExactMatchMetric::new("answer");

        let mut example_outputs = HashMap::new();
        example_outputs.insert("answer".to_string(), Value::String("42".to_string()));
        let example = Example::new(HashMap::new(), example_outputs.clone());

        // Test evaluate
        let score = metric.evaluate(&example, &example_outputs).unwrap();
        assert_eq!(score, 1.0);

        // Test validate
        let valid = metric.validate(&example, &example_outputs, None).unwrap();
        assert!(valid);

        // Test mismatch
        let mut wrong_outputs = HashMap::new();
        wrong_outputs.insert("answer".to_string(), Value::String("wrong".to_string()));

        let score = metric.evaluate(&example, &wrong_outputs).unwrap();
        assert_eq!(score, 0.0);

        let valid = metric.validate(&example, &wrong_outputs, None).unwrap();
        assert!(!valid);
    }

    #[test]
    fn test_fuzzy_match_metric() {
        let metric = FuzzyMatchMetric::new("text", 0.8);

        let mut example_outputs = HashMap::new();
        example_outputs.insert("text".to_string(), Value::String("hello world".to_string()));
        let example = Example::new(HashMap::new(), example_outputs);

        // Exact match
        let mut exact = HashMap::new();
        exact.insert("text".to_string(), Value::String("hello world".to_string()));
        let score = metric.evaluate(&example, &exact).unwrap();
        assert_eq!(score, 1.0);

        // Similar
        let mut similar = HashMap::new();
        similar.insert("text".to_string(), Value::String("hello worlb".to_string()));
        let score = metric.evaluate(&example, &similar).unwrap();
        assert!(score > 0.8);

        // Very different
        let mut different = HashMap::new();
        different.insert("text".to_string(), Value::String("xyz".to_string()));
        let score = metric.evaluate(&example, &different).unwrap();
        assert!(score < 0.5);
    }

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("", ""), 0);
        assert_eq!(levenshtein_distance("abc", "abc"), 0);
        assert_eq!(levenshtein_distance("abc", "ab"), 1);
        assert_eq!(levenshtein_distance("abc", "xyz"), 3);
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
    }

    #[test]
    fn test_fuzzy_match_threshold() {
        let metric = FuzzyMatchMetric::new("text", 0.9);

        let mut example_outputs = HashMap::new();
        example_outputs.insert("text".to_string(), Value::String("hello".to_string()));
        let example = Example::new(HashMap::new(), example_outputs);

        // High similarity but below threshold
        let mut outputs = HashMap::new();
        outputs.insert("text".to_string(), Value::String("helo".to_string()));

        let score = metric.evaluate(&example, &outputs).unwrap();
        let valid = metric.validate(&example, &outputs, None).unwrap();

        // Score should be ~0.8, below 0.9 threshold
        assert!(score < 0.9);
        assert!(!valid);
    }
}
