//! Temporal ontology system for non-linear time perception.
//!
//! Time is not linear - events and concepts exist in a multidimensional
//! temporal space with complex causality relationships.

use crate::ontology::{
    Concept, ConceptType, CognitiveArchitecture, OntologyError, OntologyMetadata, Relation,
    ValidationReport, XenoOntology, BiologicalBasis, OriginInfo,
    concept::TemporalMarker,
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Temporal ontology for beings with non-linear time perception.
///
/// Concepts exist in multidimensional temporal space. Causality is complex
/// and may include loops, branches, and parallel timelines.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemporalOntology {
    id: Uuid,
    metadata: OntologyMetadata,
    concepts: HashMap<Uuid, Concept>,
    relations: HashMap<Uuid, Relation>,
    timelines: Vec<Timeline>,
    current_timeline: Uuid,
}

/// A timeline or temporal branch
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Timeline {
    /// Timeline identifier
    pub id: Uuid,
    /// Parent timeline (if branched)
    pub parent: Option<Uuid>,
    /// Branch point coordinates
    pub branch_point: Option<Vec<f64>>,
    /// Concepts in this timeline
    pub concepts: Vec<Uuid>,
    /// Probability of this timeline
    pub probability: f64,
}

impl TemporalOntology {
    /// Create a new temporal ontology
    #[must_use]
    pub fn new(civilization: &str) -> Self {
        let origin = OriginInfo {
            civilization: civilization.to_string(),
            location: Some("Near temporal anomaly".to_string()),
            age_years: None,
            biological_basis: BiologicalBasis::Unknown,
            kardashev_level: Some(3.5),
        };

        let metadata = OntologyMetadata::new(
            format!("Temporal Ontology - {}", civilization),
            origin,
        );

        let main_timeline = Timeline {
            id: Uuid::new_v4(),
            parent: None,
            branch_point: None,
            concepts: Vec::new(),
            probability: 1.0,
        };

        let current_timeline = main_timeline.id;

        Self {
            id: Uuid::new_v4(),
            metadata,
            concepts: HashMap::new(),
            relations: HashMap::new(),
            timelines: vec![main_timeline],
            current_timeline,
        }
    }

    /// Create a temporal concept
    #[must_use]
    pub fn create_temporal_concept(
        &self,
        identifier: String,
        time_coords: Vec<f64>,
        causality: Option<Vec<f64>>,
    ) -> Concept {
        let marker = TemporalMarker {
            coordinates: time_coords,
            causality,
            uncertainty: Some(0.0),
        };

        let native_form = serde_json::to_vec(&marker).unwrap_or_default();

        let mut concept = Concept::new(
            identifier,
            native_form,
            ConceptType::TemporalEvent,
        );

        concept.temporal_markers.push(marker);
        concept
    }

    /// Create a new timeline branch
    pub fn create_branch(
        &mut self,
        parent_timeline: Uuid,
        branch_point: Vec<f64>,
        probability: f64,
    ) -> Uuid {
        let timeline = Timeline {
            id: Uuid::new_v4(),
            parent: Some(parent_timeline),
            branch_point: Some(branch_point),
            concepts: Vec::new(),
            probability: probability.clamp(0.0, 1.0),
        };

        let id = timeline.id;
        self.timelines.push(timeline);
        id
    }

    /// Switch to a different timeline
    pub fn switch_timeline(&mut self, timeline_id: Uuid) -> Result<(), OntologyError> {
        if self.timelines.iter().any(|t| t.id == timeline_id) {
            self.current_timeline = timeline_id;
            Ok(())
        } else {
            Err(OntologyError::Unknown(format!(
                "Timeline {} not found",
                timeline_id
            )))
        }
    }

    /// Get current timeline
    #[must_use]
    pub fn current_timeline(&self) -> Option<&Timeline> {
        self.timelines.iter().find(|t| t.id == self.current_timeline)
    }

    /// Calculate temporal distance between two concepts
    #[must_use]
    pub fn temporal_distance(&self, c1: &Concept, c2: &Concept) -> f64 {
        if c1.temporal_markers.is_empty() || c2.temporal_markers.is_empty() {
            return f64::INFINITY;
        }

        let coords1 = &c1.temporal_markers[0].coordinates;
        let coords2 = &c2.temporal_markers[0].coordinates;

        // Euclidean distance in temporal space
        let min_len = coords1.len().min(coords2.len());
        let mut sum_sq = 0.0;

        for i in 0..min_len {
            let diff = coords1[i] - coords2[i];
            sum_sq += diff * diff;
        }

        sum_sq.sqrt()
    }

    /// Check if two events have causal relationship
    #[must_use]
    pub fn has_causal_relationship(&self, c1: &Concept, c2: &Concept) -> bool {
        if c1.temporal_markers.is_empty() || c2.temporal_markers.is_empty() {
            return false;
        }

        // Check if causality vectors exist and have non-zero correlation
        if let (Some(ref caus1), Some(ref caus2)) = (
            &c1.temporal_markers[0].causality,
            &c2.temporal_markers[0].causality,
        ) {
            let min_len = caus1.len().min(caus2.len());
            let mut correlation = 0.0;

            for i in 0..min_len {
                correlation += caus1[i] * caus2[i];
            }

            correlation.abs() > 0.5
        } else {
            false
        }
    }

    /// Get all timelines
    #[must_use]
    pub fn timelines(&self) -> &[Timeline] {
        &self.timelines
    }
}

#[async_trait]
impl XenoOntology for TemporalOntology {
    fn id(&self) -> Uuid {
        self.id
    }

    fn metadata(&self) -> &OntologyMetadata {
        &self.metadata
    }

    fn architecture(&self) -> CognitiveArchitecture {
        CognitiveArchitecture::Temporal
    }

    async fn query_concepts(&self, query: &str) -> Result<Vec<Concept>, OntologyError> {
        if query == "*" {
            Ok(self.concepts.values().cloned().collect())
        } else {
            Ok(self
                .concepts
                .values()
                .filter(|c| {
                    c.identifier.contains(query)
                        || c.label.as_ref().map_or(false, |l| l.contains(query))
                })
                .cloned()
                .collect())
        }
    }

    async fn get_relations(&self) -> Result<Vec<Relation>, OntologyError> {
        Ok(self.relations.values().cloned().collect())
    }

    async fn add_concept(&mut self, concept: Concept) -> Result<(), OntologyError> {
        let id = concept.id;
        self.concepts.insert(id, concept);

        // Add to current timeline
        if let Some(timeline) = self.timelines.iter_mut().find(|t| t.id == self.current_timeline) {
            timeline.concepts.push(id);
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

        // Check timeline probabilities
        let total_probability: f64 = self.timelines.iter().map(|t| t.probability).sum();

        if (total_probability - 1.0).abs() > 0.01 {
            report.add_warning(format!(
                "Timeline probabilities don't sum to 1.0: {}",
                total_probability
            ));
        }

        // Check for temporal paradoxes (simplified check)
        for concept in self.concepts.values() {
            if concept.temporal_markers.len() > 1 {
                report.add_warning(format!(
                    "Concept {} exists at multiple temporal coordinates",
                    concept.id
                ));
            }
        }

        Ok(report)
    }

    async fn compatibility_score(&self, other: &dyn XenoOntology) -> Result<f64, OntologyError> {
        match other.architecture() {
            CognitiveArchitecture::Temporal => Ok(1.0),
            _ => Ok(0.15),
        }
    }
}
