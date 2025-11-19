//! Probabilistic SPARQL extensions for uncertain RDF graphs
//!
//! This module extends SPARQL with probabilistic query capabilities,
//! allowing queries over uncertain RDF graphs with confidence scores.

use crate::{
    confidence::ConfidenceScore,
    ontology::{ProbabilisticOntology, UncertainRelation},
    Probability,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Probabilistic SPARQL query
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticQuery {
    /// SPARQL query string
    pub query: String,
    /// Minimum confidence threshold
    pub min_confidence: f64,
    /// Include uncertainty in results
    pub include_uncertainty: bool,
    /// Query options
    pub options: QueryOptions,
}

impl ProbabilisticQuery {
    /// Create a new probabilistic query
    pub fn new(query: impl Into<String>) -> Self {
        Self {
            query: query.into(),
            min_confidence: 0.5,
            include_uncertainty: true,
            options: QueryOptions::default(),
        }
    }

    /// Set minimum confidence threshold
    pub fn with_min_confidence(mut self, min_confidence: f64) -> Self {
        self.min_confidence = min_confidence.clamp(0.0, 1.0);
        self
    }

    /// Set whether to include uncertainty
    pub fn with_uncertainty(mut self, include: bool) -> Self {
        self.include_uncertainty = include;
        self
    }

    /// Set query options
    pub fn with_options(mut self, options: QueryOptions) -> Self {
        self.options = options;
        self
    }
}

/// Options for probabilistic queries
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryOptions {
    /// Maximum number of results
    pub limit: Option<usize>,
    /// Sort by confidence
    pub sort_by_confidence: bool,
    /// Aggregation method for multiple paths
    pub aggregation: AggregationMethod,
}

impl Default for QueryOptions {
    fn default() -> Self {
        Self {
            limit: None,
            sort_by_confidence: true,
            aggregation: AggregationMethod::Maximum,
        }
    }
}

/// Method for aggregating probabilities along multiple paths
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AggregationMethod {
    /// Take maximum probability
    Maximum,
    /// Take minimum probability
    Minimum,
    /// Take average probability
    Average,
    /// Multiply probabilities (independent events)
    Product,
    /// Noisy-OR combination
    NoisyOr,
}

/// Result of a probabilistic query
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticQueryResult {
    /// Bindings with confidence scores
    pub bindings: Vec<ProbabilisticBinding>,
    /// Query metadata
    pub metadata: QueryMetadata,
}

impl ProbabilisticQueryResult {
    /// Create a new result
    pub fn new() -> Self {
        Self {
            bindings: Vec::new(),
            metadata: QueryMetadata::default(),
        }
    }

    /// Add a binding
    pub fn add_binding(&mut self, binding: ProbabilisticBinding) {
        self.bindings.push(binding);
    }

    /// Sort by confidence
    pub fn sort_by_confidence(&mut self) {
        self.bindings.sort_by(|a, b| {
            b.confidence.partial_cmp(&a.confidence).unwrap()
        });
    }

    /// Filter by confidence threshold
    pub fn filter_by_confidence(&mut self, threshold: f64) {
        self.bindings.retain(|b| b.confidence >= threshold);
    }

    /// Get bindings above threshold
    pub fn above_threshold(&self, threshold: f64) -> Vec<&ProbabilisticBinding> {
        self.bindings
            .iter()
            .filter(|b| b.confidence >= threshold)
            .collect()
    }
}

impl Default for ProbabilisticQueryResult {
    fn default() -> Self {
        Self::new()
    }
}

/// A single binding with confidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticBinding {
    /// Variable bindings
    pub bindings: HashMap<String, String>,
    /// Confidence in this binding [0.0, 1.0]
    pub confidence: f64,
    /// Explanation of confidence
    pub explanation: Option<ConfidenceExplanation>,
}

impl ProbabilisticBinding {
    /// Create a new probabilistic binding
    pub fn new(confidence: f64) -> Self {
        Self {
            bindings: HashMap::new(),
            confidence: confidence.clamp(0.0, 1.0),
            explanation: None,
        }
    }

    /// Add a variable binding
    pub fn bind(mut self, variable: impl Into<String>, value: impl Into<String>) -> Self {
        self.bindings.insert(variable.into(), value.into());
        self
    }

    /// Set explanation
    pub fn with_explanation(mut self, explanation: ConfidenceExplanation) -> Self {
        self.explanation = Some(explanation);
        self
    }
}

/// Explanation of how confidence was computed
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfidenceExplanation {
    /// Steps in the derivation
    pub steps: Vec<DerivationStep>,
    /// Aggregation method used
    pub aggregation: AggregationMethod,
}

impl ConfidenceExplanation {
    /// Create a new explanation
    pub fn new(aggregation: AggregationMethod) -> Self {
        Self {
            steps: Vec::new(),
            aggregation,
        }
    }

    /// Add a step
    pub fn add_step(&mut self, step: DerivationStep) {
        self.steps.push(step);
    }
}

/// A step in confidence derivation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DerivationStep {
    /// Description of the step
    pub description: String,
    /// Confidence contribution
    pub confidence: f64,
    /// Triple involved (subject, predicate, object)
    pub triple: Option<(String, String, String)>,
}

/// Query metadata
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct QueryMetadata {
    /// Number of results
    pub result_count: usize,
    /// Average confidence
    pub average_confidence: f64,
    /// Minimum confidence in results
    pub min_confidence: f64,
    /// Maximum confidence in results
    pub max_confidence: f64,
}

/// Probabilistic SPARQL executor
pub struct ProbabilisticSparql {
    /// The probabilistic ontology to query
    ontology: ProbabilisticOntology,
}

impl ProbabilisticSparql {
    /// Create a new probabilistic SPARQL executor
    pub fn new(ontology: ProbabilisticOntology) -> Self {
        Self { ontology }
    }

    /// Execute a probabilistic query
    pub fn execute(&self, query: &ProbabilisticQuery) -> ProbabilisticQueryResult {
        let mut result = ProbabilisticQueryResult::new();

        // Simple pattern matching for demonstration
        // In a full implementation, this would parse and execute SPARQL
        if query.query.contains("SELECT") {
            self.execute_select(query, &mut result);
        }

        // Apply filters and sorting
        result.filter_by_confidence(query.min_confidence);

        if query.options.sort_by_confidence {
            result.sort_by_confidence();
        }

        if let Some(limit) = query.options.limit {
            result.bindings.truncate(limit);
        }

        // Compute metadata
        self.compute_metadata(&mut result);

        result
    }

    /// Execute a SELECT query
    fn execute_select(&self, query: &ProbabilisticQuery, result: &mut ProbabilisticQueryResult) {
        // For each entity, create bindings
        for (entity_name, entity) in &self.ontology.entities {
            if let Some((type_name, confidence)) = entity.most_likely_type() {
                if confidence >= query.min_confidence {
                    let mut binding = ProbabilisticBinding::new(confidence);
                    binding.bindings.insert("entity".to_string(), entity_name.clone());
                    binding.bindings.insert("type".to_string(), type_name.to_string());

                    if query.include_uncertainty {
                        let mut explanation = ConfidenceExplanation::new(
                            query.options.aggregation.clone()
                        );
                        explanation.add_step(DerivationStep {
                            description: format!("Type inference for {}", entity_name),
                            confidence,
                            triple: Some((
                                entity_name.clone(),
                                "rdf:type".to_string(),
                                type_name.to_string(),
                            )),
                        });
                        binding = binding.with_explanation(explanation);
                    }

                    result.add_binding(binding);
                }
            }
        }
    }

    /// Compute query metadata
    fn compute_metadata(&self, result: &mut ProbabilisticQueryResult) {
        let count = result.bindings.len();
        result.metadata.result_count = count;

        if count > 0 {
            let confidences: Vec<f64> = result.bindings.iter().map(|b| b.confidence).collect();

            result.metadata.average_confidence =
                confidences.iter().sum::<f64>() / count as f64;
            result.metadata.min_confidence =
                confidences.iter().copied().fold(f64::INFINITY, f64::min);
            result.metadata.max_confidence =
                confidences.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        }
    }

    /// Get the ontology
    pub fn ontology(&self) -> &ProbabilisticOntology {
        &self.ontology
    }
}

/// Aggregate multiple confidence scores
pub fn aggregate_confidence(scores: &[f64], method: &AggregationMethod) -> f64 {
    if scores.is_empty() {
        return 0.0;
    }

    match method {
        AggregationMethod::Maximum => {
            scores.iter().copied().fold(f64::NEG_INFINITY, f64::max)
        }
        AggregationMethod::Minimum => {
            scores.iter().copied().fold(f64::INFINITY, f64::min)
        }
        AggregationMethod::Average => {
            scores.iter().sum::<f64>() / scores.len() as f64
        }
        AggregationMethod::Product => {
            scores.iter().product()
        }
        AggregationMethod::NoisyOr => {
            // Noisy-OR: 1 - ∏(1 - p_i)
            1.0 - scores.iter().map(|&p| 1.0 - p).product::<f64>()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_probabilistic_query() {
        let query = ProbabilisticQuery::new("SELECT ?x WHERE { ?x rdf:type ?type }")
            .with_min_confidence(0.7)
            .with_uncertainty(true);

        assert_eq!(query.min_confidence, 0.7);
        assert!(query.include_uncertainty);
    }

    #[test]
    fn test_query_result() {
        let mut result = ProbabilisticQueryResult::new();

        result.add_binding(
            ProbabilisticBinding::new(0.9)
                .bind("x", "Entity1")
                .bind("type", "Person"),
        );

        result.add_binding(
            ProbabilisticBinding::new(0.7)
                .bind("x", "Entity2")
                .bind("type", "Organization"),
        );

        result.sort_by_confidence();
        assert_eq!(result.bindings[0].confidence, 0.9);

        result.filter_by_confidence(0.8);
        assert_eq!(result.bindings.len(), 1);
    }

    #[test]
    fn test_probabilistic_sparql() {
        let mut ontology = ProbabilisticOntology::new("test");
        ontology.add_type_belief("user1", "Person", 0.95);
        ontology.add_type_belief("user2", "Agent", 0.75);

        let sparql = ProbabilisticSparql::new(ontology);
        let query = ProbabilisticQuery::new("SELECT ?entity ?type")
            .with_min_confidence(0.7);

        let result = sparql.execute(&query);
        assert_eq!(result.bindings.len(), 2);
        assert!(result.metadata.average_confidence > 0.7);
    }

    #[test]
    fn test_aggregate_confidence() {
        let scores = vec![0.8, 0.7, 0.9];

        assert_eq!(aggregate_confidence(&scores, &AggregationMethod::Maximum), 0.9);
        assert_eq!(aggregate_confidence(&scores, &AggregationMethod::Minimum), 0.7);
        assert!((aggregate_confidence(&scores, &AggregationMethod::Average) - 0.8).abs() < 0.01);
        assert!((aggregate_confidence(&scores, &AggregationMethod::Product) - 0.504).abs() < 0.01);
    }

    #[test]
    fn test_noisy_or() {
        let scores = vec![0.5, 0.5];
        let result = aggregate_confidence(&scores, &AggregationMethod::NoisyOr);
        assert!((result - 0.75).abs() < 0.01); // 1 - (0.5 * 0.5) = 0.75
    }
}
