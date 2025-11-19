//! Crystalline ontology system for silicon-based life forms.
//!
//! This module implements a knowledge representation system based on
//! crystalline resonance patterns, as might be used by silicon-based
//! life forms that encode information through vibrational frequencies.

use crate::ontology::{
    Concept, ConceptType, CognitiveArchitecture, OntologyError, OntologyMetadata, Relation,
    ValidationReport, XenoOntology, BiologicalBasis, OriginInfo,
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Crystalline ontology for silicon-based intelligence.
///
/// Knowledge is represented as resonance patterns in crystalline matrices.
/// Time is not a primary organizing principle - instead, knowledge is
/// organized by harmonic relationships and geometric patterns.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrystallineOntology {
    id: Uuid,
    metadata: OntologyMetadata,
    concepts: HashMap<Uuid, Concept>,
    relations: HashMap<Uuid, Relation>,
    resonance_matrix: Vec<Vec<f64>>,
}

impl CrystallineOntology {
    /// Create a new crystalline ontology
    #[must_use]
    pub fn new(civilization: &str) -> Self {
        let origin = OriginInfo {
            civilization: civilization.to_string(),
            location: Some("Unknown silicon-rich world".to_string()),
            age_years: None,
            biological_basis: BiologicalBasis::Silicon,
            kardashev_level: Some(2.5),
        };

        let metadata = OntologyMetadata::new(
            format!("Crystalline Ontology - {}", civilization),
            origin,
        );

        Self {
            id: Uuid::new_v4(),
            metadata,
            concepts: HashMap::new(),
            relations: HashMap::new(),
            resonance_matrix: Vec::new(),
        }
    }

    /// Add a resonance pattern
    pub fn add_resonance_pattern(&mut self, pattern: Vec<f64>) {
        self.resonance_matrix.push(pattern);
    }

    /// Get resonance patterns
    #[must_use]
    pub fn resonance_patterns(&self) -> &[Vec<f64>] {
        &self.resonance_matrix
    }

    /// Create a crystalline concept from frequency data
    #[must_use]
    pub fn create_pattern_concept(
        &self,
        identifier: String,
        frequencies: Vec<f64>,
    ) -> Concept {
        // Encode frequencies as bytes
        let mut native_form = Vec::new();
        for freq in &frequencies {
            native_form.extend_from_slice(&freq.to_le_bytes());
        }

        Concept::new(identifier, native_form, ConceptType::CrystallinePattern)
            .with_label(format!("Resonance Pattern {}", frequencies.len()))
            .with_description(format!(
                "A crystalline resonance pattern with {} harmonic frequencies",
                frequencies.len()
            ))
    }

    /// Calculate harmonic similarity between two concepts
    #[must_use]
    pub fn harmonic_similarity(&self, c1: &Concept, c2: &Concept) -> f64 {
        // Extract frequencies from native form
        let freq1 = self.extract_frequencies(&c1.native_form);
        let freq2 = self.extract_frequencies(&c2.native_form);

        if freq1.is_empty() || freq2.is_empty() {
            return 0.0;
        }

        // Calculate correlation between frequency patterns
        let min_len = freq1.len().min(freq2.len());
        let mut correlation = 0.0;

        for i in 0..min_len {
            let diff = (freq1[i] - freq2[i]).abs();
            correlation += 1.0 / (1.0 + diff);
        }

        correlation / min_len as f64
    }

    fn extract_frequencies(&self, native_form: &[u8]) -> Vec<f64> {
        let mut frequencies = Vec::new();
        let mut i = 0;

        while i + 8 <= native_form.len() {
            let bytes: [u8; 8] = native_form[i..i + 8].try_into().unwrap_or([0; 8]);
            frequencies.push(f64::from_le_bytes(bytes));
            i += 8;
        }

        frequencies
    }
}

#[async_trait]
impl XenoOntology for CrystallineOntology {
    fn id(&self) -> Uuid {
        self.id
    }

    fn metadata(&self) -> &OntologyMetadata {
        &self.metadata
    }

    fn architecture(&self) -> CognitiveArchitecture {
        CognitiveArchitecture::Crystalline
    }

    async fn query_concepts(&self, query: &str) -> Result<Vec<Concept>, OntologyError> {
        if query == "*" {
            Ok(self.concepts.values().cloned().collect())
        } else {
            // Simple substring matching
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

        // Check that all relations reference existing concepts
        for relation in self.relations.values() {
            if !self.concepts.contains_key(&relation.source) {
                report.add_error(format!(
                    "Relation {} references non-existent source concept {}",
                    relation.id, relation.source
                ));
            }
            if !self.concepts.contains_key(&relation.target) {
                report.add_error(format!(
                    "Relation {} references non-existent target concept {}",
                    relation.id, relation.target
                ));
            }
        }

        Ok(report)
    }

    async fn compatibility_score(&self, other: &dyn XenoOntology) -> Result<f64, OntologyError> {
        match other.architecture() {
            CognitiveArchitecture::Crystalline => Ok(1.0),
            CognitiveArchitecture::Quantum => Ok(0.4), // Some overlap through wave functions
            CognitiveArchitecture::Collective => Ok(0.3),
            CognitiveArchitecture::Temporal => Ok(0.2),
            _ => Ok(0.1),
        }
    }
}
