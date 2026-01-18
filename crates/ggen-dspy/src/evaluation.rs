//! Evaluation metrics and scoring for optimization loops

use crate::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Evaluator trait for scoring module outputs
#[async_trait::async_trait]
pub trait Evaluator: Send + Sync {
    /// Evaluate a prediction against ground truth
    async fn evaluate(&self, prediction: &str, ground_truth: &str) -> Result<MetricValue>;

    /// Get evaluator name
    fn name(&self) -> &str;
}

/// Metric value with score and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricValue {
    /// Score value (0.0 to 1.0)
    pub score: f64,

    /// Optional metadata
    pub metadata: HashMap<String, String>,
}

impl MetricValue {
    /// Create a new metric value
    pub fn new(score: f64) -> Self {
        Self {
            score,
            metadata: HashMap::new(),
        }
    }

    /// Create with metadata
    pub fn with_metadata(score: f64, metadata: HashMap<String, String>) -> Self {
        Self { score, metadata }
    }
}

/// Evaluation result for a set of predictions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvaluationResult {
    /// Mean score across all examples
    pub mean_score: f64,

    /// Standard deviation
    pub std_dev: f64,

    /// Individual scores
    pub scores: Vec<f64>,

    /// Additional metrics
    pub metrics: HashMap<String, f64>,
}

impl EvaluationResult {
    /// Create a new evaluation result
    pub fn new(scores: Vec<f64>) -> Self {
        let mean_score = scores.iter().sum::<f64>() / scores.len() as f64;
        let variance = scores
            .iter()
            .map(|s| (s - mean_score).powi(2))
            .sum::<f64>()
            / scores.len() as f64;
        let std_dev = variance.sqrt();

        Self {
            mean_score,
            std_dev,
            scores,
            metrics: HashMap::new(),
        }
    }

    /// Add a metric
    pub fn add_metric(&mut self, name: impl Into<String>, value: f64) {
        self.metrics.insert(name.into(), value);
    }
}

/// Collection of evaluation metrics
#[derive(Debug, Clone, Default)]
pub struct EvaluationMetrics {
    /// Registered metrics
    pub metrics: HashMap<String, MetricValue>,
}

impl EvaluationMetrics {
    /// Create new metrics collection
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a metric
    pub fn add(&mut self, name: impl Into<String>, value: MetricValue) {
        self.metrics.insert(name.into(), value);
    }

    /// Get a metric
    pub fn get(&self, name: &str) -> Option<&MetricValue> {
        self.metrics.get(name)
    }

    /// Get mean score across all metrics
    pub fn mean_score(&self) -> f64 {
        if self.metrics.is_empty() {
            return 0.0;
        }
        self.metrics.values().map(|m| m.score).sum::<f64>() / self.metrics.len() as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metric_value() {
        let metric = MetricValue::new(0.95);
        assert_eq!(metric.score, 0.95);
    }

    #[test]
    fn test_evaluation_result() {
        let scores = vec![0.8, 0.9, 0.85];
        let result = EvaluationResult::new(scores);
        assert!((result.mean_score - 0.85).abs() < 0.01);
    }

    #[test]
    fn test_evaluation_metrics() {
        let mut metrics = EvaluationMetrics::new();
        metrics.add("accuracy", MetricValue::new(0.9));
        assert_eq!(metrics.get("accuracy").map(|m| m.score), Some(0.9));
    }
}
