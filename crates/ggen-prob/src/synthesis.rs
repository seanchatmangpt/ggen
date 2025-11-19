//! Statistical type synthesis from noisy real-world data sources
//!
//! This module provides algorithms for inferring types from noisy, incomplete,
//! or ambiguous data using statistical methods.

use crate::{
    confidence::{ConfidenceScore, ConfidenceScoreBuilder, UncertaintySource},
    types::{ProbabilisticType, TypeDistribution, TypeSource},
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Statistical type synthesizer
pub struct TypeSynthesizer {
    /// Configuration for synthesis
    config: SynthesisConfig,
    /// Collected samples
    samples: Vec<DataSample>,
}

impl TypeSynthesizer {
    /// Create a new type synthesizer
    pub fn new(config: SynthesisConfig) -> Self {
        Self {
            config,
            samples: Vec::new(),
        }
    }

    /// Add a data sample
    pub fn add_sample(&mut self, sample: DataSample) {
        self.samples.push(sample);
    }

    /// Add multiple samples
    pub fn add_samples(&mut self, samples: impl IntoIterator<Item = DataSample>) {
        self.samples.extend(samples);
    }

    /// Synthesize type from collected samples
    pub fn synthesize(&self, field_name: &str) -> Option<ProbabilisticType> {
        let field_samples: Vec<_> = self
            .samples
            .iter()
            .filter_map(|s| s.fields.get(field_name))
            .collect();

        if field_samples.is_empty() {
            return None;
        }

        let mut ptype = ProbabilisticType::new(field_name);

        // Analyze samples for type inference
        let type_counts = self.count_types(&field_samples);
        let total = field_samples.len() as f64;

        // Build distribution from counts
        for (type_name, count) in type_counts {
            let probability = count as f64 / total;
            ptype.add_hypothesis(type_name, probability);
        }

        ptype.distribution.normalize();
        ptype.source = TypeSource::DataInference {
            samples: field_samples.len(),
            confidence: ptype.confidence(),
        };

        Some(ptype)
    }

    /// Synthesize all fields
    pub fn synthesize_all(&self) -> HashMap<String, ProbabilisticType> {
        let mut field_names = std::collections::HashSet::new();
        for sample in &self.samples {
            field_names.extend(sample.fields.keys().cloned());
        }

        field_names
            .into_iter()
            .filter_map(|name| self.synthesize(&name).map(|pt| (name, pt)))
            .collect()
    }

    /// Count type occurrences
    fn count_types(&self, samples: &[&FieldValue]) -> HashMap<String, usize> {
        let mut counts = HashMap::new();

        for sample in samples {
            let inferred_type = self.infer_type_from_value(sample);
            *counts.entry(inferred_type).or_insert(0) += 1;
        }

        counts
    }

    /// Infer type from a single value
    fn infer_type_from_value(&self, value: &FieldValue) -> String {
        match value {
            FieldValue::String(s) => {
                // Try to parse as other types
                if s.parse::<i64>().is_ok() {
                    "Integer".to_string()
                } else if s.parse::<f64>().is_ok() {
                    "Float".to_string()
                } else if s.parse::<bool>().is_ok() {
                    "Boolean".to_string()
                } else {
                    "String".to_string()
                }
            }
            FieldValue::Integer(_) => "Integer".to_string(),
            FieldValue::Float(_) => "Float".to_string(),
            FieldValue::Boolean(_) => "Boolean".to_string(),
            FieldValue::Null => "Null".to_string(),
            FieldValue::Array(_) => "Array".to_string(),
            FieldValue::Object(_) => "Object".to_string(),
        }
    }

    /// Compute confidence score for synthesis
    pub fn compute_confidence(&self, field_name: &str) -> ConfidenceScore {
        let field_samples: Vec<_> = self
            .samples
            .iter()
            .filter_map(|s| s.fields.get(field_name))
            .collect();

        let mut builder = ConfidenceScoreBuilder::new();

        // Sample size confidence
        let sample_confidence = if field_samples.len() >= self.config.min_samples {
            1.0
        } else {
            field_samples.len() as f64 / self.config.min_samples as f64
        };
        builder = builder.component("sample_size", sample_confidence);

        // Type consistency confidence
        let type_counts = self.count_types(&field_samples);
        let max_count = type_counts.values().max().copied().unwrap_or(0);
        let consistency = max_count as f64 / field_samples.len() as f64;
        builder = builder.component("consistency", consistency);

        // Noise level
        let noise_level = 1.0 - consistency;
        if noise_level > self.config.noise_threshold {
            builder = builder.uncertainty(UncertaintySource::NoisyData {
                signal_to_noise: consistency / noise_level,
            });
        }

        // Missing data
        let missing_rate = 1.0 - (field_samples.len() as f64 / self.samples.len() as f64);
        builder = builder.component("completeness", 1.0 - missing_rate);

        builder.build_equal()
    }
}

/// Configuration for type synthesis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SynthesisConfig {
    /// Minimum number of samples required
    pub min_samples: usize,
    /// Confidence threshold for type assignment
    pub confidence_threshold: f64,
    /// Noise tolerance (0.0 = no noise, 1.0 = all noise)
    pub noise_threshold: f64,
    /// Handle missing data
    pub handle_missing: bool,
}

impl Default for SynthesisConfig {
    fn default() -> Self {
        Self {
            min_samples: 10,
            confidence_threshold: 0.7,
            noise_threshold: 0.3,
            handle_missing: true,
        }
    }
}

/// A data sample from real-world source
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataSample {
    /// Sample ID
    pub id: String,
    /// Field values
    pub fields: HashMap<String, FieldValue>,
    /// Sample quality score [0.0, 1.0]
    pub quality: f64,
}

impl DataSample {
    /// Create a new data sample
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            fields: HashMap::new(),
            quality: 1.0,
        }
    }

    /// Add a field
    pub fn with_field(mut self, name: impl Into<String>, value: FieldValue) -> Self {
        self.fields.insert(name.into(), value);
        self
    }

    /// Set quality score
    pub fn with_quality(mut self, quality: f64) -> Self {
        self.quality = quality.clamp(0.0, 1.0);
        self
    }
}

/// Field value in a data sample
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FieldValue {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    Array(Vec<FieldValue>),
    Object(HashMap<String, FieldValue>),
}

/// Statistical synthesis with Bayesian priors
pub struct StatisticalSynthesis {
    /// Type synthesizer
    synthesizer: TypeSynthesizer,
    /// Prior type distributions
    priors: HashMap<String, TypeDistribution>,
}

impl StatisticalSynthesis {
    /// Create new statistical synthesis
    pub fn new(config: SynthesisConfig) -> Self {
        Self {
            synthesizer: TypeSynthesizer::new(config),
            priors: HashMap::new(),
        }
    }

    /// Set prior distribution for a field
    pub fn set_prior(&mut self, field_name: impl Into<String>, prior: TypeDistribution) {
        self.priors.insert(field_name.into(), prior);
    }

    /// Add sample
    pub fn add_sample(&mut self, sample: DataSample) {
        self.synthesizer.add_sample(sample);
    }

    /// Synthesize with Bayesian update
    pub fn synthesize(&self, field_name: &str) -> Option<ProbabilisticType> {
        // Get likelihood from data
        let mut ptype = self.synthesizer.synthesize(field_name)?;

        // Apply prior if available
        if let Some(prior) = self.priors.get(field_name) {
            // Bayesian update: posterior ∝ likelihood × prior
            for (type_name, prior_prob) in &prior.types {
                let likelihood = ptype.distribution.get(type_name);
                let posterior = likelihood * prior_prob;
                ptype.distribution.types.insert(type_name.clone(), posterior);
            }
            ptype.distribution.normalize();
        }

        Some(ptype)
    }

    /// Synthesize all fields
    pub fn synthesize_all(&self) -> HashMap<String, ProbabilisticType> {
        let mut field_names = std::collections::HashSet::new();
        for sample in &self.synthesizer.samples {
            field_names.extend(sample.fields.keys().cloned());
        }

        field_names
            .into_iter()
            .filter_map(|name| self.synthesize(&name).map(|pt| (name, pt)))
            .collect()
    }

    /// Get confidence for a field
    pub fn confidence(&self, field_name: &str) -> ConfidenceScore {
        self.synthesizer.compute_confidence(field_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_synthesizer() {
        let config = SynthesisConfig::default();
        let mut synthesizer = TypeSynthesizer::new(config);

        // Add samples
        for i in 0..20 {
            let sample = DataSample::new(format!("sample_{}", i))
                .with_field("id", FieldValue::String(i.to_string()))
                .with_field("age", FieldValue::Integer(20 + i))
                .with_field("score", FieldValue::Float(85.5 + i as f64));

            synthesizer.add_sample(sample);
        }

        let id_type = synthesizer.synthesize("id").unwrap();
        let age_type = synthesizer.synthesize("age").unwrap();
        let score_type = synthesizer.synthesize("score").unwrap();

        assert_eq!(id_type.most_likely_type().unwrap().0, "Integer");
        assert_eq!(age_type.most_likely_type().unwrap().0, "Integer");
        assert_eq!(score_type.most_likely_type().unwrap().0, "Float");
    }

    #[test]
    fn test_noisy_data() {
        let config = SynthesisConfig::default();
        let mut synthesizer = TypeSynthesizer::new(config);

        // Add mostly integers with some strings
        for i in 0..15 {
            synthesizer.add_sample(
                DataSample::new(format!("sample_{}", i))
                    .with_field("value", FieldValue::Integer(i)),
            );
        }

        // Add some noisy string values
        for i in 15..20 {
            synthesizer.add_sample(
                DataSample::new(format!("sample_{}", i))
                    .with_field("value", FieldValue::String("invalid".to_string())),
            );
        }

        let ptype = synthesizer.synthesize("value").unwrap();
        assert_eq!(ptype.most_likely_type().unwrap().0, "Integer");

        let confidence = synthesizer.compute_confidence("value");
        assert!(confidence.overall < 1.0); // Should reflect noise
    }

    #[test]
    fn test_statistical_synthesis() {
        let config = SynthesisConfig::default();
        let mut synthesis = StatisticalSynthesis::new(config);

        // Set a prior favoring String
        let mut prior = TypeDistribution::new();
        prior.add("String", 0.8);
        prior.add("Integer", 0.2);
        synthesis.set_prior("id", prior);

        // Add samples that suggest Integer
        for i in 0..10 {
            synthesis.add_sample(
                DataSample::new(format!("sample_{}", i))
                    .with_field("id", FieldValue::Integer(1000 + i)),
            );
        }

        let ptype = synthesis.synthesize("id").unwrap();
        // Posterior should balance prior and likelihood
        assert!(ptype.distribution.get("Integer") > 0.0);
    }

    #[test]
    fn test_confidence_computation() {
        let config = SynthesisConfig {
            min_samples: 5,
            ..Default::default()
        };
        let mut synthesizer = TypeSynthesizer::new(config);

        // Add few consistent samples
        for i in 0..3 {
            synthesizer.add_sample(
                DataSample::new(format!("sample_{}", i))
                    .with_field("value", FieldValue::Integer(i)),
            );
        }

        let confidence = synthesizer.compute_confidence("value");
        assert!(confidence.overall < 1.0); // Low sample count
    }
}
