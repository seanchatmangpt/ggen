//! Natural Language Understanding for bidirectional sync
//!
//! Enables natural language queries and updates to the semantic schema.

mod parser;
mod query_engine;

pub use parser::NlParser;
pub use query_engine::QueryEngine;

use crate::{Error, Result, config::NluConfig, ontology::{CodeOntology, SemanticUpdate}};
use tracing::{debug, info, instrument};

/// Natural Language Understanding engine
pub struct NluEngine {
    config: NluConfig,
    parser: NlParser,
    query_engine: QueryEngine,
}

impl NluEngine {
    /// Create a new NLU engine
    #[instrument(skip(config))]
    pub fn new(config: &NluConfig) -> Result<Self> {
        info!("Initializing NLU engine");

        Ok(Self {
            config: config.clone(),
            parser: NlParser::new(),
            query_engine: QueryEngine::new(),
        })
    }

    /// Parse natural language input into semantic updates
    #[instrument(skip(self, input))]
    pub async fn parse_to_semantic_updates(&self, input: &str) -> Result<Vec<SemanticUpdate>> {
        debug!("Parsing natural language input to semantic updates");

        let updates = self.parser.parse_updates(input)?;

        info!("Parsed {} semantic updates", updates.len());
        Ok(updates)
    }

    /// Query the ontology using natural language
    #[instrument(skip(self, ontology, query))]
    pub async fn query_ontology(&self, ontology: &CodeOntology, query: &str) -> Result<String> {
        debug!("Processing natural language query");

        // Convert natural language to SPARQL
        let sparql_query = self.query_engine.nl_to_sparql(query)?;

        // Execute query
        let results = ontology.query(&sparql_query).await?;

        // Convert results to natural language response
        let response = self.query_engine.results_to_nl(&results)?;

        Ok(response)
    }

    /// Understand the intent of a natural language input
    pub fn understand_intent(&self, input: &str) -> Result<Intent> {
        self.parser.parse_intent(input)
    }
}

/// Natural language intent
#[derive(Debug, Clone, PartialEq)]
pub enum Intent {
    /// Query for information
    Query { topic: String },

    /// Update documentation
    UpdateDoc { entity: String, content: String },

    /// Add relationship
    AddRelationship { source: String, target: String, relationship_type: String },

    /// Search
    Search { query: String },

    /// Unknown intent
    Unknown,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nlu_engine_creation() {
        let config = NluConfig::default();
        let engine = NluEngine::new(&config);
        assert!(engine.is_ok());
    }
}
