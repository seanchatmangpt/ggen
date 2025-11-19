//! Collective ontology system for hive-mind intelligences.
//!
//! Knowledge is distributed across multiple nodes with consensus mechanisms.
//! No single entity holds complete knowledge - it emerges from the collective.

use crate::ontology::{
    Concept, ConceptType, CognitiveArchitecture, OntologyError, OntologyMetadata, Relation,
    ValidationReport, XenoOntology, BiologicalBasis, OriginInfo,
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Collective ontology for hive-mind intelligence.
///
/// Knowledge is stored in a distributed fashion across multiple nodes.
/// Consensus is achieved through voting and information propagation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectiveOntology {
    id: Uuid,
    metadata: OntologyMetadata,
    concepts: HashMap<Uuid, CollectiveConcept>,
    relations: HashMap<Uuid, Relation>,
    nodes: HashMap<Uuid, CollectiveNode>,
    consensus_threshold: f64,
}

/// A concept in the collective mind
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectiveConcept {
    /// Base concept
    pub concept: Concept,
    /// Which nodes hold this concept
    pub node_distribution: HashMap<Uuid, f64>, // node_id -> confidence
    /// Consensus level (0.0 to 1.0)
    pub consensus: f64,
    /// Number of confirming nodes
    pub confirmations: usize,
}

/// A node in the collective
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectiveNode {
    /// Node identifier
    pub id: Uuid,
    /// Node reliability score
    pub reliability: f64,
    /// Concepts held by this node
    pub concepts: Vec<Uuid>,
    /// Connected nodes
    pub connections: Vec<Uuid>,
}

impl CollectiveOntology {
    /// Create a new collective ontology
    #[must_use]
    pub fn new(civilization: &str, consensus_threshold: f64) -> Self {
        let origin = OriginInfo {
            civilization: civilization.to_string(),
            location: Some("Distributed across star cluster".to_string()),
            age_years: None,
            biological_basis: BiologicalBasis::Collective,
            kardashev_level: Some(2.8),
        };

        let metadata = OntologyMetadata::new(
            format!("Collective Ontology - {}", civilization),
            origin,
        );

        Self {
            id: Uuid::new_v4(),
            metadata,
            concepts: HashMap::new(),
            relations: HashMap::new(),
            nodes: HashMap::new(),
            consensus_threshold: consensus_threshold.clamp(0.0, 1.0),
        }
    }

    /// Add a node to the collective
    pub fn add_node(&mut self, reliability: f64) -> Uuid {
        let node = CollectiveNode {
            id: Uuid::new_v4(),
            reliability: reliability.clamp(0.0, 1.0),
            concepts: Vec::new(),
            connections: Vec::new(),
        };
        let id = node.id;
        self.nodes.insert(id, node);
        id
    }

    /// Connect two nodes
    pub fn connect_nodes(&mut self, node1: Uuid, node2: Uuid) -> Result<(), OntologyError> {
        if let Some(n1) = self.nodes.get_mut(&node1) {
            if !n1.connections.contains(&node2) {
                n1.connections.push(node2);
            }
        }

        if let Some(n2) = self.nodes.get_mut(&node2) {
            if !n2.connections.contains(&node1) {
                n2.connections.push(node1);
            }
        }

        Ok(())
    }

    /// Add a concept to a specific node
    pub fn add_concept_to_node(
        &mut self,
        concept: Concept,
        node_id: Uuid,
        confidence: f64,
    ) -> Result<(), OntologyError> {
        let concept_id = concept.id;

        // Update or create collective concept
        let collective_concept = self.concepts.entry(concept_id).or_insert_with(|| {
            CollectiveConcept {
                concept: concept.clone(),
                node_distribution: HashMap::new(),
                consensus: 0.0,
                confirmations: 0,
            }
        });

        collective_concept
            .node_distribution
            .insert(node_id, confidence);

        // Recalculate consensus
        self.recalculate_consensus(concept_id)?;

        // Update node
        if let Some(node) = self.nodes.get_mut(&node_id) {
            if !node.concepts.contains(&concept_id) {
                node.concepts.push(concept_id);
            }
        }

        Ok(())
    }

    /// Recalculate consensus for a concept
    fn recalculate_consensus(&mut self, concept_id: Uuid) -> Result<(), OntologyError> {
        if let Some(collective_concept) = self.concepts.get_mut(&concept_id) {
            let mut weighted_sum = 0.0;
            let mut weight_total = 0.0;

            for (node_id, confidence) in &collective_concept.node_distribution {
                if let Some(node) = self.nodes.get(node_id) {
                    weighted_sum += confidence * node.reliability;
                    weight_total += node.reliability;
                }
            }

            collective_concept.consensus = if weight_total > 0.0 {
                weighted_sum / weight_total
            } else {
                0.0
            };

            collective_concept.confirmations = collective_concept.node_distribution.len();
        }

        Ok(())
    }

    /// Get consensus level for a concept
    #[must_use]
    pub fn get_consensus(&self, concept_id: Uuid) -> Option<f64> {
        self.concepts.get(&concept_id).map(|c| c.consensus)
    }

    /// Check if concept has reached consensus threshold
    #[must_use]
    pub fn has_consensus(&self, concept_id: Uuid) -> bool {
        self.get_consensus(concept_id)
            .map_or(false, |c| c >= self.consensus_threshold)
    }

    /// Get number of nodes
    #[must_use]
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}

#[async_trait]
impl XenoOntology for CollectiveOntology {
    fn id(&self) -> Uuid {
        self.id
    }

    fn metadata(&self) -> &OntologyMetadata {
        &self.metadata
    }

    fn architecture(&self) -> CognitiveArchitecture {
        CognitiveArchitecture::Collective
    }

    async fn query_concepts(&self, query: &str) -> Result<Vec<Concept>, OntologyError> {
        let concepts: Vec<Concept> = if query == "*" {
            self.concepts
                .values()
                .filter(|c| self.has_consensus(c.concept.id))
                .map(|c| c.concept.clone())
                .collect()
        } else {
            self.concepts
                .values()
                .filter(|c| {
                    self.has_consensus(c.concept.id)
                        && (c.concept.identifier.contains(query)
                            || c.concept.label.as_ref().map_or(false, |l| l.contains(query)))
                })
                .map(|c| c.concept.clone())
                .collect()
        };

        Ok(concepts)
    }

    async fn get_relations(&self) -> Result<Vec<Relation>, OntologyError> {
        Ok(self.relations.values().cloned().collect())
    }

    async fn add_concept(&mut self, concept: Concept) -> Result<(), OntologyError> {
        // Add to first available node with default confidence
        if let Some((node_id, _)) = self.nodes.iter().next() {
            self.add_concept_to_node(concept, *node_id, 0.5)?;
        } else {
            return Err(OntologyError::Unknown(
                "No nodes available in collective".to_string(),
            ));
        }
        Ok(())
    }

    async fn add_relation(&mut self, relation: Relation) -> Result<(), OntologyError> {
        self.relations.insert(relation.id, relation);
        Ok(())
    }

    async fn serialize(&self) -> Result<Vec<u8>, OntologyError> {
        serde_json::to_vec(self).map_err(|e| {
            OntologyError::SerializationError(format!("Failed to serialize: {}", e))
        })
    }

    async fn deserialize(data: &[u8]) -> Result<Self, OntologyError> {
        serde_json::from_slice(data).map_err(|e| {
            OntologyError::DeserializationError(format!("Failed to deserialize: {}", e))
        })
    }

    async fn validate(&self) -> Result<ValidationReport, OntologyError> {
        let mut report = ValidationReport::valid();

        // Check consensus levels
        for (id, collective_concept) in &self.concepts {
            if collective_concept.consensus < self.consensus_threshold {
                report.add_warning(format!(
                    "Concept {} has not reached consensus threshold ({:.2} < {:.2})",
                    id, collective_concept.consensus, self.consensus_threshold
                ));
            }
        }

        // Check node connectivity
        let isolated_nodes: Vec<Uuid> = self
            .nodes
            .values()
            .filter(|n| n.connections.is_empty())
            .map(|n| n.id)
            .collect();

        if !isolated_nodes.is_empty() {
            report.add_warning(format!(
                "{} isolated nodes detected",
                isolated_nodes.len()
            ));
        }

        Ok(report)
    }

    async fn compatibility_score(&self, other: &dyn XenoOntology) -> Result<f64, OntologyError> {
        match other.architecture() {
            CognitiveArchitecture::Collective => Ok(1.0),
            CognitiveArchitecture::Probabilistic => Ok(0.5),
            _ => Ok(0.2),
        }
    }
}
