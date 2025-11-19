//! Probabilistic graph operations and belief propagation
//!
//! This module provides probabilistic graph structures and belief propagation
//! algorithms for reasoning over uncertain semantic graphs.

use crate::{bayesian::BayesianNetwork, ontology::UncertainRelation, Probability};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Probabilistic graph with uncertain edges
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticGraph {
    /// Nodes with beliefs
    pub nodes: HashMap<String, NodeBelief>,
    /// Probabilistic edges
    pub edges: Vec<ProbabilisticEdge>,
}

impl ProbabilisticGraph {
    /// Create a new probabilistic graph
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            edges: Vec::new(),
        }
    }

    /// Add a node
    pub fn add_node(&mut self, name: impl Into<String>, belief: NodeBelief) {
        self.nodes.insert(name.into(), belief);
    }

    /// Add an edge
    pub fn add_edge(&mut self, edge: ProbabilisticEdge) {
        self.edges.push(edge);
    }

    /// Get node
    pub fn get_node(&self, name: &str) -> Option<&NodeBelief> {
        self.nodes.get(name)
    }

    /// Get mutable node
    pub fn get_node_mut(&mut self, name: &str) -> Option<&mut NodeBelief> {
        self.nodes.get_mut(name)
    }

    /// Get neighbors of a node
    pub fn neighbors(&self, node: &str) -> Vec<(&str, f64)> {
        self.edges
            .iter()
            .filter_map(|e| {
                if e.source == node {
                    Some((e.target.as_str(), e.probability))
                } else if e.target == node {
                    Some((e.source.as_str(), e.probability))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get outgoing edges from a node
    pub fn outgoing_edges(&self, node: &str) -> Vec<&ProbabilisticEdge> {
        self.edges
            .iter()
            .filter(|e| e.source == node)
            .collect()
    }

    /// Get incoming edges to a node
    pub fn incoming_edges(&self, node: &str) -> Vec<&ProbabilisticEdge> {
        self.edges
            .iter()
            .filter(|e| e.target == node)
            .collect()
    }

    /// Convert to Bayesian network
    pub fn to_bayesian_network(&self) -> BayesianNetwork {
        let mut network = BayesianNetwork::new();
        let mut node_indices = HashMap::new();

        // Add all nodes
        for (name, belief) in &self.nodes {
            let node = crate::bayesian::BeliefNode::new(name, belief.prior);
            let idx = network.add_node(node);
            node_indices.insert(name.clone(), idx);
        }

        // Add edges
        for edge in &self.edges {
            if let (Some(&source_idx), Some(&target_idx)) = (
                node_indices.get(&edge.source),
                node_indices.get(&edge.target),
            ) {
                network.add_edge(source_idx, target_idx);
            }
        }

        network
    }

    /// Perform belief propagation
    pub fn propagate_beliefs(&mut self, max_iterations: usize) {
        let mut network = self.to_bayesian_network();
        network.propagate_beliefs(max_iterations);

        // Update node beliefs
        for (_i, node) in network.nodes.iter().enumerate() {
            if let Some(graph_node) = self.nodes.get_mut(&node.name) {
                graph_node.belief = node.belief;
            }
        }
    }
}

impl Default for ProbabilisticGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// Node in a probabilistic graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeBelief {
    /// Prior probability
    pub prior: f64,
    /// Current belief
    pub belief: f64,
    /// Node attributes
    pub attributes: HashMap<String, String>,
}

impl NodeBelief {
    /// Create a new node belief
    pub fn new(prior: f64) -> Self {
        Self {
            prior: prior.clamp(0.0, 1.0),
            belief: prior.clamp(0.0, 1.0),
            attributes: HashMap::new(),
        }
    }

    /// Add an attribute
    pub fn with_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.insert(key.into(), value.into());
        self
    }
}

/// Probabilistic edge between nodes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticEdge {
    /// Source node
    pub source: String,
    /// Target node
    pub target: String,
    /// Edge label/type
    pub label: String,
    /// Probability of this edge [0.0, 1.0]
    pub probability: f64,
}

impl ProbabilisticEdge {
    /// Create a new probabilistic edge
    pub fn new(
        source: impl Into<String>,
        target: impl Into<String>,
        label: impl Into<String>,
        probability: f64,
    ) -> Self {
        Self {
            source: source.into(),
            target: target.into(),
            label: label.into(),
            probability: probability.clamp(0.0, 1.0),
        }
    }
}

/// Belief propagation algorithm implementation
pub struct BeliefPropagation {
    /// The graph to propagate beliefs on
    graph: ProbabilisticGraph,
    /// Messages between nodes
    messages: HashMap<(String, String), f64>,
}

impl BeliefPropagation {
    /// Create a new belief propagation instance
    pub fn new(graph: ProbabilisticGraph) -> Self {
        Self {
            graph,
            messages: HashMap::new(),
        }
    }

    /// Run belief propagation
    pub fn run(&mut self, max_iterations: usize, convergence_threshold: f64) -> usize {
        let mut iteration = 0;

        while iteration < max_iterations {
            let mut max_change: f64 = 0.0;

            // Send messages from each node to its neighbors
            let nodes: Vec<_> = self.graph.nodes.keys().cloned().collect();

            for node in &nodes {
                let neighbors = self.graph.neighbors(node);

                for (neighbor, edge_prob) in neighbors {
                    let old_message = self.messages
                        .get(&(node.clone(), neighbor.to_string()))
                        .copied()
                        .unwrap_or(0.5);

                    let new_message = self.compute_message(node, neighbor, edge_prob);

                    self.messages.insert(
                        (node.clone(), neighbor.to_string()),
                        new_message,
                    );

                    let change = (new_message - old_message).abs();
                    max_change = max_change.max(change);
                }
            }

            // Update beliefs
            for node_name in &nodes {
                if let Some(node) = self.graph.get_node_mut(node_name) {
                    node.belief = self.compute_belief(node_name);
                }
            }

            iteration += 1;

            if max_change < convergence_threshold {
                break;
            }
        }

        iteration
    }

    /// Compute message from one node to another
    fn compute_message(&self, from: &str, to: &str, edge_prob: f64) -> f64 {
        let from_node = self.graph.get_node(from).unwrap();

        // Collect incoming messages (excluding from 'to')
        let incoming: Vec<f64> = self.graph
            .neighbors(from)
            .iter()
            .filter(|(n, _)| *n != to)
            .filter_map(|(n, _)| {
                self.messages.get(&(n.to_string(), from.to_string())).copied()
            })
            .collect();

        // Compute product of incoming messages
        let product = if incoming.is_empty() {
            from_node.prior
        } else {
            incoming.iter().product::<f64>()
        };

        // Weight by edge probability
        (product * edge_prob).clamp(0.0, 1.0)
    }

    /// Compute belief for a node
    fn compute_belief(&self, node: &str) -> f64 {
        let node_belief = self.graph.get_node(node).unwrap();

        // Collect all incoming messages
        let incoming: Vec<f64> = self.graph
            .neighbors(node)
            .iter()
            .filter_map(|(n, _)| {
                self.messages.get(&(n.to_string(), node.to_string())).copied()
            })
            .collect();

        if incoming.is_empty() {
            node_belief.prior
        } else {
            let product: f64 = incoming.iter().product();
            (node_belief.prior * product).clamp(0.0, 1.0)
        }
    }

    /// Get the updated graph
    pub fn graph(&self) -> &ProbabilisticGraph {
        &self.graph
    }

    /// Get mutable graph
    pub fn graph_mut(&mut self) -> &mut ProbabilisticGraph {
        &mut self.graph
    }
}

/// Build a probabilistic graph from uncertain relations
pub struct ProbabilisticGraphBuilder {
    graph: ProbabilisticGraph,
}

impl ProbabilisticGraphBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            graph: ProbabilisticGraph::new(),
        }
    }

    /// Add nodes from relations
    pub fn from_relations(mut self, relations: &[UncertainRelation]) -> Self {
        let mut nodes = HashSet::new();

        // Collect all unique nodes
        for rel in relations {
            nodes.insert(rel.subject.clone());
            nodes.insert(rel.object.clone());
        }

        // Add nodes to graph
        for node in nodes {
            self.graph.add_node(node.clone(), NodeBelief::new(0.5));
        }

        // Add edges
        for rel in relations {
            self.graph.add_edge(ProbabilisticEdge::new(
                rel.subject.clone(),
                rel.object.clone(),
                rel.predicate.clone(),
                rel.confidence,
            ));
        }

        self
    }

    /// Build the graph
    pub fn build(self) -> ProbabilisticGraph {
        self.graph
    }
}

impl Default for ProbabilisticGraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ontology::UncertainRelation;

    #[test]
    fn test_probabilistic_graph() {
        let mut graph = ProbabilisticGraph::new();

        graph.add_node("A", NodeBelief::new(0.8));
        graph.add_node("B", NodeBelief::new(0.5));
        graph.add_node("C", NodeBelief::new(0.6));

        graph.add_edge(ProbabilisticEdge::new("A", "B", "relates", 0.9));
        graph.add_edge(ProbabilisticEdge::new("B", "C", "depends", 0.7));

        assert_eq!(graph.nodes.len(), 3);
        assert_eq!(graph.edges.len(), 2);

        let neighbors = graph.neighbors("B");
        assert_eq!(neighbors.len(), 2);
    }

    #[test]
    fn test_belief_propagation() {
        let mut graph = ProbabilisticGraph::new();

        graph.add_node("evidence", NodeBelief::new(0.9));
        graph.add_node("hypothesis", NodeBelief::new(0.5));

        graph.add_edge(ProbabilisticEdge::new("evidence", "hypothesis", "supports", 0.8));

        let mut bp = BeliefPropagation::new(graph);
        let iterations = bp.run(100, 1e-6);

        let hypothesis = bp.graph().get_node("hypothesis").unwrap();
        assert!(hypothesis.belief > 0.5); // Should increase due to evidence
        assert!(iterations > 0);
    }

    #[test]
    fn test_graph_builder() {
        let relations = vec![
            UncertainRelation::new("Person", "subClassOf", "Entity", 0.9),
            UncertainRelation::new("Student", "subClassOf", "Person", 0.95),
        ];

        let graph = ProbabilisticGraphBuilder::new()
            .from_relations(&relations)
            .build();

        assert_eq!(graph.nodes.len(), 3); // Person, Entity, Student
        assert_eq!(graph.edges.len(), 2);
    }

    #[test]
    fn test_to_bayesian_network() {
        let mut graph = ProbabilisticGraph::new();
        graph.add_node("A", NodeBelief::new(0.7));
        graph.add_node("B", NodeBelief::new(0.5));
        graph.add_edge(ProbabilisticEdge::new("A", "B", "causes", 0.8));

        let network = graph.to_bayesian_network();
        assert_eq!(network.nodes.len(), 2);
    }
}
