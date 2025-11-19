//! Probabilistic type system with uncertainty quantification
//!
//! This module provides types for representing uncertain type information
//! with probability distributions and confidence intervals.

use crate::Probability;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A probabilistic type with associated confidence scores
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticType {
    /// Name of the entity/variable
    pub name: String,
    /// Distribution over possible types
    pub distribution: TypeDistribution,
    /// Source of type information
    pub source: TypeSource,
}

impl ProbabilisticType {
    /// Create a new probabilistic type
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            distribution: TypeDistribution::default(),
            source: TypeSource::Unknown,
        }
    }

    /// Add a type hypothesis with probability
    pub fn add_hypothesis(&mut self, type_name: impl Into<String>, probability: f64) {
        self.distribution.add(type_name, probability);
    }

    /// Get the most likely type
    pub fn most_likely_type(&self) -> Option<(&str, f64)> {
        self.distribution.most_likely()
    }

    /// Get confidence in the most likely type
    pub fn confidence(&self) -> f64 {
        self.most_likely_type()
            .map(|(_, prob)| prob)
            .unwrap_or(0.0)
    }

    /// Check if type is certain (single type with p >= threshold)
    pub fn is_certain(&self, threshold: f64) -> bool {
        self.distribution.types.len() == 1 && self.confidence() >= threshold
    }
}

/// Distribution over possible types
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TypeDistribution {
    /// Map of type names to probabilities
    pub types: HashMap<String, f64>,
}

impl TypeDistribution {
    /// Create a new empty distribution
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a type with probability
    pub fn add(&mut self, type_name: impl Into<String>, probability: f64) {
        let type_name = type_name.into();
        *self.types.entry(type_name).or_insert(0.0) += probability;
    }

    /// Normalize probabilities to sum to 1.0
    pub fn normalize(&mut self) {
        let sum: f64 = self.types.values().sum();
        if sum > 0.0 {
            for prob in self.types.values_mut() {
                *prob /= sum;
            }
        }
    }

    /// Get the most likely type
    pub fn most_likely(&self) -> Option<(&str, f64)> {
        self.types
            .iter()
            .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
            .map(|(name, prob)| (name.as_str(), *prob))
    }

    /// Get entropy of the distribution (measure of uncertainty)
    pub fn entropy(&self) -> f64 {
        self.types
            .values()
            .filter(|&&p| p > 0.0)
            .map(|&p| -p * p.log2())
            .sum()
    }

    /// Get probability for a specific type
    pub fn get(&self, type_name: &str) -> f64 {
        self.types.get(type_name).copied().unwrap_or(0.0)
    }
}

/// Source of type information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TypeSource {
    /// Inferred from data samples
    DataInference { samples: usize, confidence: f64 },
    /// Provided by user/schema
    Schema { schema_name: String },
    /// Inferred by machine learning model
    MLInference { model: String, confidence: f64 },
    /// From fuzzy logic rules
    FuzzyRules { rules: Vec<String> },
    /// Bayesian inference
    BayesianInference { priors: String },
    /// Unknown source
    Unknown,
}

/// Uncertain type representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UncertainType {
    /// Possible types with their probabilities
    pub candidates: Vec<(String, Probability)>,
    /// Minimum confidence threshold
    pub threshold: f64,
}

impl UncertainType {
    /// Create a new uncertain type
    pub fn new(threshold: f64) -> Self {
        Self {
            candidates: Vec::new(),
            threshold,
        }
    }

    /// Add a type candidate
    pub fn add_candidate(&mut self, type_name: impl Into<String>, probability: Probability) {
        self.candidates.push((type_name.into(), probability));
    }

    /// Get candidates above threshold
    pub fn above_threshold(&self) -> Vec<(&str, f64)> {
        self.candidates
            .iter()
            .filter(|(_, p)| p.value() >= self.threshold)
            .map(|(name, p)| (name.as_str(), p.value()))
            .collect()
    }

    /// Resolve to single type if confidence is high enough
    pub fn resolve(&self) -> Option<String> {
        let above = self.above_threshold();
        if above.len() == 1 {
            Some(above[0].0.to_string())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_distribution() {
        let mut dist = TypeDistribution::new();
        dist.add("String", 0.7);
        dist.add("Integer", 0.2);
        dist.add("Float", 0.1);

        assert_eq!(dist.most_likely(), Some(("String", 0.7)));
        assert!((dist.entropy() - 0.8018).abs() < 0.01);
    }

    #[test]
    fn test_normalize() {
        let mut dist = TypeDistribution::new();
        dist.add("String", 70.0);
        dist.add("Integer", 20.0);
        dist.add("Float", 10.0);
        dist.normalize();

        assert_eq!(dist.get("String"), 0.7);
        assert_eq!(dist.get("Integer"), 0.2);
        assert_eq!(dist.get("Float"), 0.1);
    }

    #[test]
    fn test_probabilistic_type() {
        let mut ptype = ProbabilisticType::new("user_id");
        ptype.add_hypothesis("String", 0.95);
        ptype.add_hypothesis("Integer", 0.05);

        assert!(ptype.is_certain(0.9));
        assert!(!ptype.is_certain(0.99));
        assert_eq!(ptype.confidence(), 0.95);
    }

    #[test]
    fn test_uncertain_type() {
        let mut utype = UncertainType::new(0.5);
        utype.add_candidate("String", Probability::new(0.7));
        utype.add_candidate("Integer", Probability::new(0.2));
        utype.add_candidate("Float", Probability::new(0.1));

        let above = utype.above_threshold();
        assert_eq!(above.len(), 1);
        assert_eq!(above[0].0, "String");
        assert_eq!(utype.resolve(), Some("String".to_string()));
    }
}
