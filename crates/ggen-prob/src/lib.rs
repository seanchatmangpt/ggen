//! # ggen-prob: Probabilistic Programming Framework for Uncertain Ontologies
//!
//! This crate provides a comprehensive probabilistic programming framework for handling
//! uncertainty in ontologies, semantic graphs, and type systems.
//!
//! ## Features
//!
//! - **Bayesian Inference**: Apply Bayesian reasoning to semantic graphs and ontologies
//! - **Fuzzy Logic**: Handle ambiguous domain modeling with fuzzy sets and logic
//! - **Statistical Type Synthesis**: Infer types from noisy real-world data
//! - **Confidence Intervals**: Generate code with uncertainty quantification
//! - **Probabilistic SPARQL**: Extended SPARQL queries with probability weights
//!
//! ## Example
//!
//! ```rust
//! use ggen_prob::{ProbabilisticOntology, BayesianInference, ConfidenceInterval};
//!
//! // Create a probabilistic ontology
//! let mut ontology = ProbabilisticOntology::new();
//!
//! // Add uncertain types with confidence scores
//! ontology.add_type_belief("user_id", "String", 0.95);
//! ontology.add_type_belief("user_id", "Integer", 0.05);
//!
//! // Perform Bayesian inference
//! let inference = BayesianInference::new(&ontology);
//! let posterior = inference.infer_type("user_id");
//! ```

pub mod bayesian;
pub mod confidence;
pub mod fuzzy;
pub mod inference;
pub mod ontology;
pub mod sparql;
pub mod synthesis;
pub mod types;
pub mod graph;

// Re-export key types
pub use bayesian::{BayesianInference, BayesianNetwork, BeliefNode};
pub use confidence::{ConfidenceInterval, ConfidenceScore};
pub use fuzzy::{FuzzySet, FuzzyLogic, MembershipFunction};
pub use inference::{InferenceEngine, InferenceResult};
pub use ontology::{ProbabilisticOntology, UncertainRelation};
pub use sparql::{ProbabilisticQuery, ProbabilisticSparql};
pub use synthesis::{TypeSynthesizer, StatisticalSynthesis};
pub use types::{ProbabilisticType, TypeDistribution, UncertainType};
pub use graph::{ProbabilisticGraph, BeliefPropagation};

use ggen_utils::error::Result;

/// Version information for the probabilistic framework
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Probability value constrained to [0.0, 1.0]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
pub struct Probability(f64);

impl Probability {
    /// Create a new probability value
    ///
    /// # Panics
    /// Panics if value is not in [0.0, 1.0]
    pub fn new(value: f64) -> Self {
        assert!(
            (0.0..=1.0).contains(&value),
            "Probability must be in [0.0, 1.0], got {}",
            value
        );
        Self(value)
    }

    /// Try to create a probability value
    pub fn try_new(value: f64) -> Result<Self> {
        if (0.0..=1.0).contains(&value) {
            Ok(Self(value))
        } else {
            Err(ggen_utils::error::GgenError::new(&format!(
                "Probability must be in [0.0, 1.0], got {}",
                value
            )))
        }
    }

    /// Get the inner value
    pub fn value(&self) -> f64 {
        self.0
    }

    /// Complement probability (1 - p)
    pub fn complement(&self) -> Self {
        Self(1.0 - self.0)
    }
}

impl From<Probability> for f64 {
    fn from(p: Probability) -> f64 {
        p.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_probability_valid() {
        let p = Probability::new(0.5);
        assert_eq!(p.value(), 0.5);
    }

    #[test]
    fn test_probability_complement() {
        let p = Probability::new(0.3);
        assert_eq!(p.complement().value(), 0.7);
    }

    #[test]
    #[should_panic]
    fn test_probability_invalid() {
        Probability::new(1.5);
    }

    #[test]
    fn test_probability_try_new() {
        assert!(Probability::try_new(0.5).is_ok());
        assert!(Probability::try_new(1.5).is_err());
        assert!(Probability::try_new(-0.1).is_err());
    }
}
