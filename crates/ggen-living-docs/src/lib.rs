//! # Living Documentation Ecosystem
//!
//! A comprehensive living documentation system that creates a symbiotic relationship
//! between code and documentation through:
//!
//! - **Semantic Ontology**: RDF-based knowledge graph of code structure
//! - **Automated Narrative Generation**: Human-readable docs from code analysis
//! - **Interactive Storytelling**: Web-based interactive documentation portal
//! - **NLU Bidirectional Sync**: Natural language updates synced with semantic schemas
//!
//! ## Architecture
//!
//! ```text
//! ┌────────────────────────────────────────────────────────┐
//! │              Living Documentation System                │
//! ├────────────────────────────────────────────────────────┤
//! │                                                          │
//! │  Code → Ontology → Narratives → Interactive Interface  │
//! │           ↑                                     ↓        │
//! │           └──────── NLU Bidirectional ─────────┘        │
//! │                                                          │
//! └────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ggen_living_docs::{LivingDocSystem, Config};
//!
//! # async fn example() -> anyhow::Result<()> {
//! // Initialize the living documentation system
//! let config = Config::default();
//! let mut system = LivingDocSystem::new(config)?;
//!
//! // Extract code ontology from project
//! system.extract_ontology("./src").await?;
//!
//! // Generate narratives
//! let narratives = system.generate_narratives().await?;
//!
//! // Start interactive server
//! system.serve("127.0.0.1:8080").await?;
//! # Ok(())
//! # }
//! ```

pub mod ontology;
pub mod extractor;
pub mod narrative;
pub mod interface;
pub mod nlu;
pub mod templates;
pub mod hooks;
pub mod config;
pub mod errors;

// Re-exports
pub use config::Config;
pub use errors::{Error, Result};
pub use ontology::CodeOntology;
pub use extractor::CodeExtractor;
pub use narrative::NarrativeGenerator;
pub use interface::InteractiveServer;
pub use nlu::NluEngine;

use std::path::Path;
use tracing::{info, instrument};

/// The main living documentation system orchestrator
pub struct LivingDocSystem {
    config: Config,
    ontology: CodeOntology,
    extractor: CodeExtractor,
    narrative_gen: NarrativeGenerator,
    nlu_engine: NluEngine,
}

impl LivingDocSystem {
    /// Create a new living documentation system with the given configuration
    #[instrument(skip(config))]
    pub fn new(config: Config) -> Result<Self> {
        info!("Initializing Living Documentation System");

        let ontology = CodeOntology::new(&config.ontology_config)?;
        let extractor = CodeExtractor::new(&config.extractor_config)?;
        let narrative_gen = NarrativeGenerator::new(&config.narrative_config)?;
        let nlu_engine = NluEngine::new(&config.nlu_config)?;

        Ok(Self {
            config,
            ontology,
            extractor,
            narrative_gen,
            nlu_engine,
        })
    }

    /// Extract code ontology from the given source directory
    #[instrument(skip(self))]
    pub async fn extract_ontology(&mut self, source_dir: impl AsRef<Path>) -> Result<()> {
        info!("Extracting ontology from: {:?}", source_dir.as_ref());

        // Extract code structure
        let code_entities = self.extractor.extract_from_directory(source_dir).await?;

        // Build RDF graph
        self.ontology.build_from_entities(code_entities).await?;

        info!("Ontology extraction complete");
        Ok(())
    }

    /// Generate documentation narratives from the current ontology
    #[instrument(skip(self))]
    pub async fn generate_narratives(&self) -> Result<Vec<String>> {
        info!("Generating documentation narratives");

        let narratives = self.narrative_gen
            .generate_from_ontology(&self.ontology)
            .await?;

        info!("Generated {} narratives", narratives.len());
        Ok(narratives)
    }

    /// Start the interactive documentation server
    #[instrument(skip(self))]
    pub async fn serve(&self, addr: &str) -> Result<()> {
        info!("Starting interactive documentation server at {}", addr);

        let server = InteractiveServer::new(
            self.config.interface_config.clone(),
            self.ontology.clone(),
        )?;

        server.start(addr).await?;
        Ok(())
    }

    /// Perform bidirectional sync: natural language → semantic schema
    #[instrument(skip(self, nl_input))]
    pub async fn sync_from_natural_language(&mut self, nl_input: &str) -> Result<()> {
        info!("Syncing from natural language input");

        // Parse natural language to structured updates
        let updates = self.nlu_engine.parse_to_semantic_updates(nl_input).await?;

        // Apply updates to ontology
        self.ontology.apply_updates(updates).await?;

        // Regenerate affected narratives
        self.generate_narratives().await?;

        info!("Bidirectional sync complete");
        Ok(())
    }

    /// Query the documentation using natural language
    #[instrument(skip(self, query))]
    pub async fn query(&self, query: &str) -> Result<String> {
        info!("Processing natural language query: {}", query);

        let response = self.nlu_engine
            .query_ontology(&self.ontology, query)
            .await?;

        Ok(response)
    }

    /// Validate that documentation is in sync with code
    #[instrument(skip(self))]
    pub async fn validate(&self) -> Result<ValidationReport> {
        info!("Validating documentation sync");

        let report = self.ontology.validate_completeness().await?;
        Ok(report)
    }

    /// Export the ontology as RDF/Turtle
    #[instrument(skip(self))]
    pub async fn export_ontology(&self, output_path: impl AsRef<Path>) -> Result<()> {
        info!("Exporting ontology to: {:?}", output_path.as_ref());

        self.ontology.export_turtle(output_path).await?;
        Ok(())
    }
}

/// Validation report for documentation completeness
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ValidationReport {
    pub total_entities: usize,
    pub documented_entities: usize,
    pub undocumented_entities: Vec<String>,
    pub missing_examples: Vec<String>,
    pub coverage_percentage: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_system_initialization() {
        let config = Config::default();
        let system = LivingDocSystem::new(config);
        assert!(system.is_ok());
    }
}
