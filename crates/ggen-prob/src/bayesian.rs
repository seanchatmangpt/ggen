//! Bayesian inference for semantic graphs and ontologies
//!
//! This module provides Bayesian networks, belief propagation, and
//! probabilistic reasoning for uncertain ontologies.

use crate::{Probability, types::TypeDistribution};
use ndarray::{Array1, Array2};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Bayesian network for probabilistic reasoning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BayesianNetwork {
    /// Nodes in the network
    pub nodes: Vec<BeliefNode>,
    /// Edges (parent -> child relationships)
    pub edges: Vec<(usize, usize)>,
    /// Node name to index mapping
    node_index: HashMap<String, usize>,
}

impl BayesianNetwork {
    /// Create a new Bayesian network
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            node_index: HashMap::new(),
        }
    }

    /// Add a node to the network
    pub fn add_node(&mut self, node: BeliefNode) -> usize {
        let index = self.nodes.len();
        self.node_index.insert(node.name.clone(), index);
        self.nodes.push(node);
        index
    }

    /// Add an edge (parent -> child)
    pub fn add_edge(&mut self, parent: usize, child: usize) {
        self.edges.push((parent, child));
    }

    /// Get node by name
    pub fn get_node(&self, name: &str) -> Option<&BeliefNode> {
        self.node_index
            .get(name)
            .and_then(|&idx| self.nodes.get(idx))
    }

    /// Get mutable node by name
    pub fn get_node_mut(&mut self, name: &str) -> Option<&mut BeliefNode> {
        self.node_index
            .get(name)
            .copied()
            .and_then(|idx| self.nodes.get_mut(idx))
    }

    /// Get parents of a node
    pub fn parents(&self, node_idx: usize) -> Vec<usize> {
        self.edges
            .iter()
            .filter(|(_, child)| *child == node_idx)
            .map(|(parent, _)| *parent)
            .collect()
    }

    /// Get children of a node
    pub fn children(&self, node_idx: usize) -> Vec<usize> {
        self.edges
            .iter()
            .filter(|(parent, _)| *parent == node_idx)
            .map(|(_, child)| *child)
            .collect()
    }

    /// Perform belief propagation
    pub fn propagate_beliefs(&mut self, max_iterations: usize) -> f64 {
        let mut max_change = f64::INFINITY;
        let mut iteration = 0;

        while iteration < max_iterations && max_change > 1e-6 {
            max_change = 0.0;

            // Forward pass: propagate from parents to children
            for i in 0..self.nodes.len() {
                let parents = self.parents(i);
                if !parents.is_empty() {
                    let old_belief = self.nodes[i].belief;

                    // Collect parent beliefs
                    let parent_beliefs: Vec<f64> = parents
                        .iter()
                        .map(|&p| self.nodes[p].belief)
                        .collect();

                    // Update belief based on parents (simplified)
                    let avg_parent = parent_beliefs.iter().sum::<f64>() / parent_beliefs.len() as f64;
                    self.nodes[i].belief = (avg_parent + self.nodes[i].prior) / 2.0;

                    let change = (self.nodes[i].belief - old_belief).abs();
                    max_change = max_change.max(change);
                }
            }

            iteration += 1;
        }

        max_change
    }

    /// Compute posterior probabilities given evidence
    pub fn infer(&mut self, evidence: &HashMap<String, f64>) -> HashMap<String, f64> {
        // Set evidence
        for (name, value) in evidence {
            if let Some(node) = self.get_node_mut(name) {
                node.evidence = Some(*value);
                node.belief = *value;
            }
        }

        // Propagate beliefs
        self.propagate_beliefs(100);

        // Collect posteriors
        self.nodes
            .iter()
            .map(|node| (node.name.clone(), node.belief))
            .collect()
    }
}

impl Default for BayesianNetwork {
    fn default() -> Self {
        Self::new()
    }
}

/// A node in a Bayesian network representing a belief
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BeliefNode {
    /// Node name/identifier
    pub name: String,
    /// Prior probability
    pub prior: f64,
    /// Current belief (posterior)
    pub belief: f64,
    /// Observed evidence (if any)
    pub evidence: Option<f64>,
    /// Conditional probability table (CPT)
    pub cpt: Option<ConditionalProbabilityTable>,
}

impl BeliefNode {
    /// Create a new belief node
    pub fn new(name: impl Into<String>, prior: f64) -> Self {
        Self {
            name: name.into(),
            prior: prior.clamp(0.0, 1.0),
            belief: prior.clamp(0.0, 1.0),
            evidence: None,
            cpt: None,
        }
    }

    /// Set evidence for this node
    pub fn set_evidence(&mut self, value: f64) {
        self.evidence = Some(value.clamp(0.0, 1.0));
        self.belief = value.clamp(0.0, 1.0);
    }

    /// Update belief
    pub fn update_belief(&mut self, new_belief: f64) {
        self.belief = new_belief.clamp(0.0, 1.0);
    }
}

/// Conditional Probability Table for Bayesian networks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionalProbabilityTable {
    /// Probabilities indexed by parent configurations
    pub table: HashMap<Vec<bool>, f64>,
}

impl ConditionalProbabilityTable {
    /// Create a new CPT
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    /// Add an entry to the CPT
    pub fn add_entry(&mut self, parent_values: Vec<bool>, probability: f64) {
        self.table.insert(parent_values, probability.clamp(0.0, 1.0));
    }

    /// Get probability for parent configuration
    pub fn get_probability(&self, parent_values: &[bool]) -> Option<f64> {
        self.table.get(parent_values).copied()
    }
}

impl Default for ConditionalProbabilityTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Bayesian inference engine
pub struct BayesianInference {
    /// The Bayesian network
    network: BayesianNetwork,
}

impl BayesianInference {
    /// Create a new inference engine
    pub fn new(network: BayesianNetwork) -> Self {
        Self { network }
    }

    /// Perform inference with evidence
    pub fn infer(&mut self, evidence: HashMap<String, f64>) -> HashMap<String, f64> {
        self.network.infer(&evidence)
    }

    /// Compute posterior for a specific variable
    pub fn infer_variable(&mut self, variable: &str, evidence: HashMap<String, f64>) -> Option<f64> {
        let posteriors = self.infer(evidence);
        posteriors.get(variable).copied()
    }

    /// Get the network
    pub fn network(&self) -> &BayesianNetwork {
        &self.network
    }

    /// Get mutable network
    pub fn network_mut(&mut self) -> &mut BayesianNetwork {
        &mut self.network
    }
}

/// Bayesian update for type inference
pub struct BayesianTypeInference {
    /// Prior distribution over types
    priors: TypeDistribution,
    /// Evidence from data samples
    evidence: Vec<TypeEvidence>,
}

impl BayesianTypeInference {
    /// Create new Bayesian type inference
    pub fn new(priors: TypeDistribution) -> Self {
        Self {
            priors,
            evidence: Vec::new(),
        }
    }

    /// Add evidence from data
    pub fn add_evidence(&mut self, evidence: TypeEvidence) {
        self.evidence.push(evidence);
    }

    /// Compute posterior distribution
    pub fn compute_posterior(&self) -> TypeDistribution {
        let mut posterior = self.priors.clone();

        // Update based on evidence (simplified Bayesian update)
        for ev in &self.evidence {
            let likelihood = ev.likelihood;
            for (type_name, prob) in posterior.types.iter_mut() {
                if type_name == &ev.type_name {
                    *prob *= likelihood;
                } else {
                    *prob *= 1.0 - likelihood;
                }
            }
        }

        posterior.normalize();
        posterior
    }
}

/// Evidence for type inference
#[derive(Debug, Clone)]
pub struct TypeEvidence {
    /// Type being evidenced
    pub type_name: String,
    /// Likelihood of this type given the evidence
    pub likelihood: f64,
    /// Source of evidence
    pub source: String,
}

impl TypeEvidence {
    /// Create new type evidence
    pub fn new(type_name: impl Into<String>, likelihood: f64, source: impl Into<String>) -> Self {
        Self {
            type_name: type_name.into(),
            likelihood: likelihood.clamp(0.0, 1.0),
            source: source.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_belief_node() {
        let mut node = BeliefNode::new("type_is_string", 0.5);
        assert_eq!(node.belief, 0.5);

        node.set_evidence(0.8);
        assert_eq!(node.belief, 0.8);
    }

    #[test]
    fn test_bayesian_network() {
        let mut network = BayesianNetwork::new();

        let n1 = network.add_node(BeliefNode::new("has_quotes", 0.5));
        let n2 = network.add_node(BeliefNode::new("is_string", 0.3));
        network.add_edge(n1, n2);

        assert_eq!(network.parents(n2), vec![n1]);
        assert_eq!(network.children(n1), vec![n2]);
    }

    #[test]
    fn test_belief_propagation() {
        let mut network = BayesianNetwork::new();

        let n1 = network.add_node(BeliefNode::new("evidence", 0.9));
        let n2 = network.add_node(BeliefNode::new("hypothesis", 0.5));
        network.add_edge(n1, n2);

        network.propagate_beliefs(10);

        // Child belief should be influenced by parent
        assert!(network.nodes[n2].belief > 0.5);
    }

    #[test]
    fn test_bayesian_inference() {
        let mut network = BayesianNetwork::new();
        network.add_node(BeliefNode::new("is_numeric", 0.3));
        network.add_node(BeliefNode::new("has_decimal", 0.2));

        let mut inference = BayesianInference::new(network);
        let mut evidence = HashMap::new();
        evidence.insert("has_decimal".to_string(), 0.9);

        let posterior = inference.infer(evidence);
        assert!(posterior.contains_key("is_numeric"));
        assert_eq!(posterior.get("has_decimal"), Some(&0.9));
    }

    #[test]
    fn test_bayesian_type_inference() {
        let mut priors = TypeDistribution::new();
        priors.add("String", 0.4);
        priors.add("Integer", 0.3);
        priors.add("Float", 0.3);

        let mut inference = BayesianTypeInference::new(priors);
        inference.add_evidence(TypeEvidence::new("String", 0.9, "has_quotes"));
        inference.add_evidence(TypeEvidence::new("String", 0.8, "no_numbers"));

        let posterior = inference.compute_posterior();
        assert!(posterior.get("String") > posterior.get("Integer"));
    }
}
