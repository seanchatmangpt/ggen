//! Confidence intervals and uncertainty quantification for generated code
//!
//! This module provides confidence scoring and interval computation for
//! code generation with uncertainty quantification.

use crate::Probability;
use serde::{Deserialize, Serialize};
use statrs::distribution::{ContinuousCDF, Normal};

/// Confidence interval for a parameter or estimate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfidenceInterval {
    /// Point estimate (mean)
    pub point_estimate: f64,
    /// Lower bound
    pub lower: f64,
    /// Upper bound
    pub upper: f64,
    /// Confidence level (e.g., 0.95 for 95% CI)
    pub level: f64,
}

impl ConfidenceInterval {
    /// Create a new confidence interval
    pub fn new(point_estimate: f64, lower: f64, upper: f64, level: f64) -> Self {
        Self {
            point_estimate,
            lower,
            upper,
            level,
        }
    }

    /// Compute from normal distribution
    pub fn from_normal(mean: f64, std_dev: f64, level: f64) -> Self {
        let normal = Normal::new(mean, std_dev).expect("Valid normal distribution");
        let alpha = 1.0 - level;
        let z_score = normal.inverse_cdf(1.0 - alpha / 2.0);

        let margin = z_score * std_dev;
        Self {
            point_estimate: mean,
            lower: mean - margin,
            upper: mean + margin,
            level,
        }
    }

    /// Width of the interval
    pub fn width(&self) -> f64 {
        self.upper - self.lower
    }

    /// Check if value is in the interval
    pub fn contains(&self, value: f64) -> bool {
        value >= self.lower && value <= self.upper
    }

    /// Margin of error
    pub fn margin_of_error(&self) -> f64 {
        self.width() / 2.0
    }

    /// Relative error (margin / point_estimate)
    pub fn relative_error(&self) -> f64 {
        if self.point_estimate.abs() > 1e-10 {
            self.margin_of_error() / self.point_estimate.abs()
        } else {
            f64::INFINITY
        }
    }
}

/// Confidence score for a piece of generated code or type inference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfidenceScore {
    /// Overall confidence [0.0, 1.0]
    pub overall: f64,
    /// Component scores
    pub components: Vec<ComponentScore>,
    /// Uncertainty sources
    pub uncertainties: Vec<UncertaintySource>,
}

impl ConfidenceScore {
    /// Create a new confidence score
    pub fn new(overall: f64) -> Self {
        Self {
            overall: overall.clamp(0.0, 1.0),
            components: Vec::new(),
            uncertainties: Vec::new(),
        }
    }

    /// Add a component score
    pub fn add_component(&mut self, name: impl Into<String>, score: f64) {
        self.components.push(ComponentScore {
            name: name.into(),
            score: score.clamp(0.0, 1.0),
        });
    }

    /// Add an uncertainty source
    pub fn add_uncertainty(&mut self, source: UncertaintySource) {
        self.uncertainties.push(source);
    }

    /// Recompute overall score from components (weighted average)
    pub fn recompute(&mut self, weights: &[f64]) {
        if self.components.is_empty() {
            return;
        }

        let weights = if weights.is_empty() {
            vec![1.0 / self.components.len() as f64; self.components.len()]
        } else {
            weights.to_vec()
        };

        let sum: f64 = self
            .components
            .iter()
            .zip(weights.iter())
            .map(|(comp, w)| comp.score * w)
            .sum();

        let weight_sum: f64 = weights.iter().sum();
        self.overall = (sum / weight_sum).clamp(0.0, 1.0);
    }

    /// Check if confidence meets threshold
    pub fn meets_threshold(&self, threshold: f64) -> bool {
        self.overall >= threshold
    }

    /// Get uncertainty level (1 - confidence)
    pub fn uncertainty(&self) -> f64 {
        1.0 - self.overall
    }
}

/// Component of a confidence score
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentScore {
    /// Component name
    pub name: String,
    /// Score [0.0, 1.0]
    pub score: f64,
}

/// Source of uncertainty in the system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UncertaintySource {
    /// Insufficient training data
    InsufficientData { samples: usize, required: usize },
    /// Ambiguous input
    AmbiguousInput { alternatives: usize },
    /// Model uncertainty
    ModelUncertainty { variance: f64 },
    /// Conflicting evidence
    ConflictingEvidence { conflict_score: f64 },
    /// Missing schema information
    MissingSchema { missing_fields: Vec<String> },
    /// Noisy data
    NoisyData { signal_to_noise: f64 },
    /// Other source
    Other { description: String },
}

/// Builder for confidence scores
pub struct ConfidenceScoreBuilder {
    score: ConfidenceScore,
}

impl ConfidenceScoreBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            score: ConfidenceScore::new(0.0),
        }
    }

    /// Add a component
    pub fn component(mut self, name: impl Into<String>, score: f64) -> Self {
        self.score.add_component(name, score);
        self
    }

    /// Add uncertainty source
    pub fn uncertainty(mut self, source: UncertaintySource) -> Self {
        self.score.add_uncertainty(source);
        self
    }

    /// Build with weighted average
    pub fn build(mut self, weights: &[f64]) -> ConfidenceScore {
        self.score.recompute(weights);
        self.score
    }

    /// Build with equal weights
    pub fn build_equal(mut self) -> ConfidenceScore {
        self.score.recompute(&[]);
        self.score
    }
}

impl Default for ConfidenceScoreBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_confidence_interval() {
        let ci = ConfidenceInterval::from_normal(10.0, 2.0, 0.95);
        assert!((ci.point_estimate - 10.0).abs() < 0.01);
        assert!(ci.contains(10.0));
        assert!((ci.width() - 7.84).abs() < 0.1);
    }

    #[test]
    fn test_confidence_score() {
        let mut score = ConfidenceScore::new(0.0);
        score.add_component("data_quality", 0.8);
        score.add_component("model_accuracy", 0.9);
        score.add_component("schema_completeness", 0.7);
        score.recompute(&[]);

        assert!((score.overall - 0.8).abs() < 0.01);
        assert!(score.meets_threshold(0.75));
    }

    #[test]
    fn test_weighted_confidence() {
        let mut score = ConfidenceScore::new(0.0);
        score.add_component("critical", 0.5);
        score.add_component("optional", 1.0);
        score.recompute(&[0.9, 0.1]); // Weight critical component more

        assert!((score.overall - 0.55).abs() < 0.01);
    }

    #[test]
    fn test_confidence_builder() {
        let score = ConfidenceScoreBuilder::new()
            .component("data", 0.8)
            .component("model", 0.9)
            .uncertainty(UncertaintySource::InsufficientData {
                samples: 100,
                required: 1000,
            })
            .build_equal();

        assert!((score.overall - 0.85).abs() < 0.01);
        assert_eq!(score.uncertainties.len(), 1);
    }

    #[test]
    fn test_margin_of_error() {
        let ci = ConfidenceInterval::new(100.0, 95.0, 105.0, 0.95);
        assert_eq!(ci.margin_of_error(), 5.0);
        assert!((ci.relative_error() - 0.05).abs() < 0.01);
    }
}
