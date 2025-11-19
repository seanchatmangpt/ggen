//! General inference engine combining multiple probabilistic methods
//!
//! This module provides a unified inference engine that combines Bayesian inference,
//! fuzzy logic, and statistical methods for reasoning over uncertain ontologies.

use crate::{
    bayesian::{BayesianInference, BayesianNetwork, BayesianTypeInference, TypeEvidence},
    confidence::ConfidenceScore,
    fuzzy::{FuzzyLogic, FuzzyTypeClassifier},
    ontology::ProbabilisticOntology,
    synthesis::{StatisticalSynthesis, SynthesisConfig},
    types::{ProbabilisticType, TypeDistribution},
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Unified inference engine
pub struct InferenceEngine {
    /// Bayesian inference component
    bayesian: Option<BayesianInference>,
    /// Fuzzy logic component
    fuzzy: FuzzyLogic,
    /// Statistical synthesis component
    synthesis: Option<StatisticalSynthesis>,
    /// Configuration
    config: InferenceConfig,
}

impl InferenceEngine {
    /// Create a new inference engine
    pub fn new(config: InferenceConfig) -> Self {
        Self {
            bayesian: None,
            fuzzy: FuzzyLogic::new(),
            synthesis: None,
            config,
        }
    }

    /// Set Bayesian network
    pub fn with_bayesian(mut self, network: BayesianNetwork) -> Self {
        self.bayesian = Some(BayesianInference::new(network));
        self
    }

    /// Set fuzzy logic system
    pub fn with_fuzzy(mut self, fuzzy: FuzzyLogic) -> Self {
        self.fuzzy = fuzzy;
        self
    }

    /// Set statistical synthesis
    pub fn with_synthesis(mut self, synthesis: StatisticalSynthesis) -> Self {
        self.synthesis = Some(synthesis);
        self
    }

    /// Perform inference on an ontology
    pub fn infer_ontology(&mut self, ontology: &ProbabilisticOntology) -> InferenceResult {
        let mut result = InferenceResult::new();

        // Run Bayesian inference if available
        if let Some(bayesian) = &mut self.bayesian {
            let posteriors = bayesian.infer(HashMap::new());
            result.bayesian_posteriors = posteriors;
        }

        // Compute confidence scores
        result.confidence = ontology.confidence_score();

        // Infer types for entities
        for (entity_name, entity) in &ontology.entities {
            if let Some((type_name, prob)) = entity.most_likely_type() {
                result.inferred_types.insert(
                    entity_name.clone(),
                    InferredType {
                        type_name: type_name.to_string(),
                        confidence: prob,
                        method: InferenceMethod::Ontology,
                    },
                );
            }
        }

        result
    }

    /// Infer type for a variable from multiple evidence sources
    pub fn infer_type(
        &mut self,
        variable_name: &str,
        evidence: &TypeInferenceEvidence,
    ) -> Option<ProbabilisticType> {
        let mut ptype = ProbabilisticType::new(variable_name);

        // Combine evidence from different sources
        let mut weights = HashMap::new();

        // Bayesian evidence
        if !evidence.bayesian_evidence.is_empty() {
            let mut priors = TypeDistribution::new();
            for (type_name, prior) in &self.config.type_priors {
                priors.add(type_name, *prior);
            }

            let mut bayesian_inference = BayesianTypeInference::new(priors);
            for ev in &evidence.bayesian_evidence {
                bayesian_inference.add_evidence(ev.clone());
            }

            let posterior = bayesian_inference.compute_posterior();
            for (type_name, prob) in posterior.types {
                *weights.entry(type_name).or_insert(0.0) += prob * self.config.bayesian_weight;
            }
        }

        // Fuzzy logic evidence
        if !evidence.fuzzy_features.is_empty() {
            let fuzzy_result = self.fuzzy.infer(&evidence.fuzzy_features);
            for (type_name, membership) in fuzzy_result {
                *weights.entry(type_name).or_insert(0.0) += membership * self.config.fuzzy_weight;
            }
        }

        // Statistical evidence
        if let Some(synthesis) = &self.synthesis {
            if let Some(synth_type) = synthesis.synthesize(variable_name) {
                for (type_name, prob) in synth_type.distribution.types {
                    *weights.entry(type_name).or_insert(0.0) += prob * self.config.statistical_weight;
                }
            }
        }

        // Normalize and build final distribution
        if !weights.is_empty() {
            for (type_name, weight) in weights {
                ptype.add_hypothesis(type_name, weight);
            }
            ptype.distribution.normalize();
            Some(ptype)
        } else {
            None
        }
    }

    /// Get configuration
    pub fn config(&self) -> &InferenceConfig {
        &self.config
    }
}

/// Configuration for the inference engine
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceConfig {
    /// Weight for Bayesian inference [0.0, 1.0]
    pub bayesian_weight: f64,
    /// Weight for fuzzy logic [0.0, 1.0]
    pub fuzzy_weight: f64,
    /// Weight for statistical synthesis [0.0, 1.0]
    pub statistical_weight: f64,
    /// Prior type probabilities
    pub type_priors: HashMap<String, f64>,
    /// Confidence threshold
    pub confidence_threshold: f64,
}

impl Default for InferenceConfig {
    fn default() -> Self {
        let mut type_priors = HashMap::new();
        type_priors.insert("String".to_string(), 0.3);
        type_priors.insert("Integer".to_string(), 0.2);
        type_priors.insert("Float".to_string(), 0.2);
        type_priors.insert("Boolean".to_string(), 0.1);
        type_priors.insert("Object".to_string(), 0.2);

        Self {
            bayesian_weight: 0.4,
            fuzzy_weight: 0.3,
            statistical_weight: 0.3,
            type_priors,
            confidence_threshold: 0.7,
        }
    }
}

/// Result of inference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceResult {
    /// Inferred types for entities
    pub inferred_types: HashMap<String, InferredType>,
    /// Bayesian posterior probabilities
    pub bayesian_posteriors: HashMap<String, f64>,
    /// Overall confidence score
    pub confidence: ConfidenceScore,
}

impl InferenceResult {
    /// Create a new inference result
    pub fn new() -> Self {
        Self {
            inferred_types: HashMap::new(),
            bayesian_posteriors: HashMap::new(),
            confidence: ConfidenceScore::new(0.0),
        }
    }

    /// Get inferred type for an entity
    pub fn get_type(&self, entity: &str) -> Option<&InferredType> {
        self.inferred_types.get(entity)
    }

    /// Check if confidence meets threshold
    pub fn meets_threshold(&self, threshold: f64) -> bool {
        self.confidence.meets_threshold(threshold)
    }
}

impl Default for InferenceResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Inferred type with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferredType {
    /// Type name
    pub type_name: String,
    /// Confidence in this inference [0.0, 1.0]
    pub confidence: f64,
    /// Inference method used
    pub method: InferenceMethod,
}

/// Method used for inference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InferenceMethod {
    /// Bayesian inference
    Bayesian,
    /// Fuzzy logic
    Fuzzy,
    /// Statistical synthesis
    Statistical,
    /// Combined methods
    Hybrid,
    /// From ontology
    Ontology,
}

/// Evidence for type inference
#[derive(Debug, Clone, Default)]
pub struct TypeInferenceEvidence {
    /// Bayesian evidence
    pub bayesian_evidence: Vec<TypeEvidence>,
    /// Fuzzy features
    pub fuzzy_features: HashMap<String, f64>,
}

impl TypeInferenceEvidence {
    /// Create new evidence
    pub fn new() -> Self {
        Self::default()
    }

    /// Add Bayesian evidence
    pub fn add_bayesian(mut self, evidence: TypeEvidence) -> Self {
        self.bayesian_evidence.push(evidence);
        self
    }

    /// Add fuzzy feature
    pub fn add_fuzzy(mut self, feature: impl Into<String>, value: f64) -> Self {
        self.fuzzy_features.insert(feature.into(), value);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bayesian::BeliefNode;

    #[test]
    fn test_inference_engine() {
        let config = InferenceConfig::default();
        let engine = InferenceEngine::new(config);

        assert!(engine.bayesian.is_none());
        assert!(engine.synthesis.is_none());
    }

    #[test]
    fn test_ontology_inference() {
        let mut ontology = ProbabilisticOntology::new("test");
        ontology.add_type_belief("user_id", "String", 0.9);
        ontology.add_type_belief("user_id", "Integer", 0.1);

        let config = InferenceConfig::default();
        let mut engine = InferenceEngine::new(config);

        let result = engine.infer_ontology(&ontology);
        assert!(result.inferred_types.contains_key("user_id"));

        let inferred = result.get_type("user_id").unwrap();
        assert_eq!(inferred.type_name, "String");
    }

    #[test]
    fn test_hybrid_inference() {
        let config = InferenceConfig::default();
        let mut engine = InferenceEngine::new(config);

        let evidence = TypeInferenceEvidence::new()
            .add_bayesian(TypeEvidence::new("String", 0.8, "has_quotes"))
            .add_fuzzy("has_quotes", 0.9);

        let result = engine.infer_type("field1", &evidence);
        assert!(result.is_some());

        let ptype = result.unwrap();
        assert!(ptype.confidence() > 0.0);
    }

    #[test]
    fn test_inference_result() {
        let mut result = InferenceResult::new();
        result.inferred_types.insert(
            "x".to_string(),
            InferredType {
                type_name: "Integer".to_string(),
                confidence: 0.85,
                method: InferenceMethod::Bayesian,
            },
        );

        assert_eq!(result.get_type("x").unwrap().type_name, "Integer");
    }
}
