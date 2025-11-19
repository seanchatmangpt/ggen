//! Universal semantic translator for cross-species ontology translation.
//!
//! This module provides the core translation infrastructure for converting
//! between human and alien knowledge representation systems.

use crate::ontology::{Concept, OntologyError, Relation, XenoOntology, CognitiveArchitecture};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

pub mod strategies;
pub mod bridges;

/// Universal translator for xenobiological ontologies.
///
/// This translator uses multiple translation strategies and maintains
/// a knowledge base of translation bridges between different cognitive architectures.
pub struct UniversalTranslator {
    /// Translation bridges between architectures
    bridges: HashMap<(CognitiveArchitecture, CognitiveArchitecture), Box<dyn TranslationBridge>>,

    /// Translation cache
    cache: HashMap<String, TranslationResult>,

    /// Configuration
    config: TranslatorConfig,
}

impl UniversalTranslator {
    /// Create a new universal translator
    #[must_use]
    pub fn new() -> Self {
        Self {
            bridges: HashMap::new(),
            cache: HashMap::new(),
            config: TranslatorConfig::default(),
        }
    }

    /// Register a translation bridge
    pub fn register_bridge(
        &mut self,
        from: CognitiveArchitecture,
        to: CognitiveArchitecture,
        bridge: Box<dyn TranslationBridge>,
    ) {
        self.bridges.insert((from, to), bridge);
    }

    /// Translate a concept from alien to human representation
    ///
    /// # Errors
    ///
    /// Returns an error if translation fails or no bridge is available
    pub async fn to_human(
        &self,
        concept: &Concept,
        architecture: CognitiveArchitecture,
    ) -> Result<HumanConcept, OntologyError> {
        let cache_key = format!("{:?}-{}", concept.id, architecture);

        if let Some(cached) = self.cache.get(&cache_key) {
            return Ok(cached.human_concept.clone());
        }

        // Find appropriate bridge
        let bridge = self.bridges.get(&(architecture, CognitiveArchitecture::Unknown))
            .ok_or_else(|| OntologyError::TranslationError(
                format!("No bridge available for architecture: {:?}", architecture)
            ))?;

        bridge.translate_to_human(concept).await
    }

    /// Translate a human concept to alien representation
    ///
    /// # Errors
    ///
    /// Returns an error if translation fails or no bridge is available
    pub async fn from_human(
        &self,
        human: &HumanConcept,
        target_architecture: CognitiveArchitecture,
    ) -> Result<Concept, OntologyError> {
        let bridge = self.bridges.get(&(CognitiveArchitecture::Unknown, target_architecture))
            .ok_or_else(|| OntologyError::TranslationError(
                format!("No bridge available for target architecture: {:?}", target_architecture)
            ))?;

        bridge.translate_from_human(human).await
    }

    /// Translate between two alien ontologies
    ///
    /// # Errors
    ///
    /// Returns an error if translation fails
    pub async fn translate_ontology(
        &self,
        source: &dyn XenoOntology,
        target_architecture: CognitiveArchitecture,
    ) -> Result<Vec<Concept>, OntologyError> {
        let source_concepts = source.query_concepts("*").await?;
        let mut translated = Vec::new();

        for concept in source_concepts {
            // First translate to human
            let human = self.to_human(&concept, source.architecture()).await?;
            // Then to target architecture
            let alien = self.from_human(&human, target_architecture).await?;
            translated.push(alien);
        }

        Ok(translated)
    }

    /// Calculate semantic similarity between concepts
    ///
    /// # Errors
    ///
    /// Returns an error if similarity calculation fails
    pub async fn semantic_similarity(
        &self,
        c1: &Concept,
        c2: &Concept,
        arch1: CognitiveArchitecture,
        arch2: CognitiveArchitecture,
    ) -> Result<f64, OntologyError> {
        // Translate both to human representation
        let h1 = self.to_human(c1, arch1).await?;
        let h2 = self.to_human(c2, arch2).await?;

        // Calculate similarity in human space
        Ok(h1.similarity(&h2))
    }
}

impl Default for UniversalTranslator {
    fn default() -> Self {
        Self::new()
    }
}

/// Configuration for the translator
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TranslatorConfig {
    /// Enable caching
    pub enable_cache: bool,

    /// Maximum cache size
    pub max_cache_size: usize,

    /// Translation quality threshold (0.0 to 1.0)
    pub quality_threshold: f64,

    /// Enable lossy translation (may lose some information)
    pub allow_lossy: bool,
}

impl Default for TranslatorConfig {
    fn default() -> Self {
        Self {
            enable_cache: true,
            max_cache_size: 10000,
            quality_threshold: 0.7,
            allow_lossy: false,
        }
    }
}

/// Human-understandable concept representation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HumanConcept {
    /// Unique identifier
    pub id: Uuid,

    /// Human-readable name
    pub name: String,

    /// Description
    pub description: String,

    /// Category
    pub category: String,

    /// Properties
    pub properties: HashMap<String, serde_json::Value>,

    /// Analogies to help understanding
    pub analogies: Vec<String>,

    /// Confidence in translation (0.0 to 1.0)
    pub confidence: f64,

    /// Information loss indicator
    pub information_loss: f64,
}

impl HumanConcept {
    /// Calculate similarity with another human concept
    #[must_use]
    pub fn similarity(&self, other: &HumanConcept) -> f64 {
        let mut similarity = 0.0;
        let mut factors = 0;

        // Name similarity (simple substring matching)
        if self.name.to_lowercase().contains(&other.name.to_lowercase())
            || other.name.to_lowercase().contains(&self.name.to_lowercase())
        {
            similarity += 0.3;
        }
        factors += 1;

        // Category match
        if self.category == other.category {
            similarity += 0.4;
        }
        factors += 1;

        // Property overlap
        let common_keys: usize = self
            .properties
            .keys()
            .filter(|k| other.properties.contains_key(*k))
            .count();

        let total_keys = self.properties.len() + other.properties.len() - common_keys;
        if total_keys > 0 {
            similarity += 0.3 * (common_keys as f64 / total_keys as f64);
        }
        factors += 1;

        similarity / factors as f64
    }
}

/// Result of a translation operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TranslationResult {
    /// The human concept
    pub human_concept: HumanConcept,

    /// Translation metadata
    pub metadata: TranslationMetadata,
}

/// Metadata about a translation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TranslationMetadata {
    /// Source architecture
    pub source_architecture: CognitiveArchitecture,

    /// Translation method used
    pub method: String,

    /// Quality score (0.0 to 1.0)
    pub quality: f64,

    /// Processing time in milliseconds
    pub processing_time_ms: u64,
}

/// Trait for translation bridges between cognitive architectures
#[async_trait]
pub trait TranslationBridge: Send + Sync {
    /// Translate alien concept to human representation
    async fn translate_to_human(&self, concept: &Concept) -> Result<HumanConcept, OntologyError>;

    /// Translate human concept to alien representation
    async fn translate_from_human(&self, human: &HumanConcept) -> Result<Concept, OntologyError>;

    /// Get the quality score for this bridge (0.0 to 1.0)
    fn quality_score(&self) -> f64;
}
