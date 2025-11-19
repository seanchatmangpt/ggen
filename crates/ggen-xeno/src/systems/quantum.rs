//! Quantum ontology system for quantum coherent life forms.
//!
//! Knowledge is represented through quantum superposition and entanglement.
//! Multiple states exist simultaneously until observation collapses them.

use crate::ontology::{
    Concept, ConceptType, CognitiveArchitecture, OntologyError, OntologyMetadata, Relation,
    ValidationReport, XenoOntology, BiologicalBasis, OriginInfo,
    concept::QuantumState,
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Quantum ontology for quantum coherent intelligence.
///
/// Knowledge exists in superposition - concepts can represent multiple
/// states simultaneously. Observation (querying) causes wave function collapse.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuantumOntology {
    id: Uuid,
    metadata: OntologyMetadata,
    concepts: HashMap<Uuid, Concept>,
    relations: HashMap<Uuid, Relation>,
    entanglement_graph: HashMap<Uuid, Vec<Uuid>>,
    measurement_history: Vec<MeasurementEvent>,
}

/// A measurement event that collapsed quantum states
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MeasurementEvent {
    /// When the measurement occurred
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Concept that was measured
    pub concept_id: Uuid,
    /// Collapsed state
    pub collapsed_state: String,
    /// Previous superposition
    pub previous_states: Vec<(String, f64)>,
}

impl QuantumOntology {
    /// Create a new quantum ontology
    #[must_use]
    pub fn new(civilization: &str) -> Self {
        let origin = OriginInfo {
            civilization: civilization.to_string(),
            location: Some("Quantum coherent nebula".to_string()),
            age_years: None,
            biological_basis: BiologicalBasis::Quantum,
            kardashev_level: Some(3.0),
        };

        let metadata = OntologyMetadata::new(
            format!("Quantum Ontology - {}", civilization),
            origin,
        );

        Self {
            id: Uuid::new_v4(),
            metadata,
            concepts: HashMap::new(),
            relations: HashMap::new(),
            entanglement_graph: HashMap::new(),
            measurement_history: Vec::new(),
        }
    }

    /// Create a quantum superposition concept
    #[must_use]
    pub fn create_superposition(
        &self,
        identifier: String,
        states: Vec<(String, f64)>,
    ) -> Concept {
        // Normalize amplitudes
        let total: f64 = states.iter().map(|(_, amp)| amp * amp).sum();
        let normalized: Vec<(String, f64)> = states
            .into_iter()
            .map(|(state, amp)| (state, amp / total.sqrt()))
            .collect();

        let quantum_state = QuantumState {
            states: normalized.clone(),
            entangled_with: Vec::new(),
            basis: Some("computational".to_string()),
        };

        // Encode quantum state as bytes
        let native_form = serde_json::to_vec(&quantum_state).unwrap_or_default();

        let mut concept = Concept::new(
            identifier,
            native_form,
            ConceptType::QuantumSuperposition,
        );
        concept.quantum_state = Some(quantum_state);
        concept
    }

    /// Entangle two concepts
    pub fn entangle(&mut self, c1_id: Uuid, c2_id: Uuid) -> Result<(), OntologyError> {
        // Add to entanglement graph
        self.entanglement_graph
            .entry(c1_id)
            .or_default()
            .push(c2_id);
        self.entanglement_graph
            .entry(c2_id)
            .or_default()
            .push(c1_id);

        // Update concepts if they exist
        if let Some(c1) = self.concepts.get_mut(&c1_id) {
            if let Some(ref mut qstate) = c1.quantum_state {
                qstate.entangled_with.push(c2_id);
            }
        }

        if let Some(c2) = self.concepts.get_mut(&c2_id) {
            if let Some(ref mut qstate) = c2.quantum_state {
                qstate.entangled_with.push(c1_id);
            }
        }

        Ok(())
    }

    /// Measure (observe) a quantum concept, collapsing its superposition
    pub fn measure(&mut self, concept_id: Uuid) -> Result<String, OntologyError> {
        let concept = self.concepts.get(&concept_id).ok_or_else(|| {
            OntologyError::ConceptNotFound(concept_id.to_string())
        })?;

        if let Some(ref qstate) = concept.quantum_state {
            // Collapse to a state based on amplitudes (simplified)
            let collapsed = qstate
                .states
                .first()
                .map(|(s, _)| s.clone())
                .unwrap_or_else(|| "unknown".to_string());

            // Record measurement
            self.measurement_history.push(MeasurementEvent {
                timestamp: chrono::Utc::now(),
                concept_id,
                collapsed_state: collapsed.clone(),
                previous_states: qstate.states.clone(),
            });

            Ok(collapsed)
        } else {
            Err(OntologyError::Unknown(
                "Concept is not in quantum superposition".to_string(),
            ))
        }
    }

    /// Get measurement history
    #[must_use]
    pub fn measurement_history(&self) -> &[MeasurementEvent] {
        &self.measurement_history
    }
}

#[async_trait]
impl XenoOntology for QuantumOntology {
    fn id(&self) -> Uuid {
        self.id
    }

    fn metadata(&self) -> &OntologyMetadata {
        &self.metadata
    }

    fn architecture(&self) -> CognitiveArchitecture {
        CognitiveArchitecture::Quantum
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
        self.concepts.insert(concept.id, concept);
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

        // Check quantum state validity
        for concept in self.concepts.values() {
            if let Some(ref qstate) = concept.quantum_state {
                // Check amplitude normalization
                let total: f64 = qstate.states.iter().map(|(_, amp)| amp * amp).sum();
                if (total - 1.0).abs() > 0.01 {
                    report.add_warning(format!(
                        "Concept {} has unnormalized quantum state (sum: {})",
                        concept.id, total
                    ));
                }

                // Check entanglement consistency
                for entangled_id in &qstate.entangled_with {
                    if !self.concepts.contains_key(entangled_id) {
                        report.add_error(format!(
                            "Concept {} entangled with non-existent concept {}",
                            concept.id, entangled_id
                        ));
                    }
                }
            }
        }

        Ok(report)
    }

    async fn compatibility_score(&self, other: &dyn XenoOntology) -> Result<f64, OntologyError> {
        match other.architecture() {
            CognitiveArchitecture::Quantum => Ok(1.0),
            CognitiveArchitecture::Crystalline => Ok(0.4),
            CognitiveArchitecture::Probabilistic => Ok(0.6),
            _ => Ok(0.2),
        }
    }
}
