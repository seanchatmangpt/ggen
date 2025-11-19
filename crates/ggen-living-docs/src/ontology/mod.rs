//! Semantic ontology system for code understanding
//!
//! This module provides RDF-based semantic representation of code structure,
//! enabling rich queries and automated documentation generation.

mod schema;
mod graph;
mod queries;

pub use schema::{CodeEntity, EntityKind, Relationship, RelationshipKind};
pub use graph::OntologyGraph;
pub use queries::SparqlQueries;

use crate::{Error, Result, ValidationReport, config::OntologyConfig};
use std::path::Path;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, instrument};

/// The main code ontology system
///
/// Manages an RDF knowledge graph representing the code structure,
/// relationships, and documentation.
#[derive(Clone)]
pub struct CodeOntology {
    config: OntologyConfig,
    graph: Arc<RwLock<OntologyGraph>>,
    queries: Arc<SparqlQueries>,
}

impl CodeOntology {
    /// Create a new code ontology
    #[instrument(skip(config))]
    pub fn new(config: &OntologyConfig) -> Result<Self> {
        info!("Creating new code ontology");

        let graph = OntologyGraph::new(config)?;
        let queries = SparqlQueries::new(&config.base_uri);

        Ok(Self {
            config: config.clone(),
            graph: Arc::new(RwLock::new(graph)),
            queries: Arc::new(queries),
        })
    }

    /// Build ontology from extracted code entities
    #[instrument(skip(self, entities))]
    pub async fn build_from_entities(&self, entities: Vec<CodeEntity>) -> Result<()> {
        info!("Building ontology from {} entities", entities.len());

        let mut graph = self.graph.write().await;

        for entity in entities {
            debug!("Adding entity: {:?}", entity.name);
            graph.add_entity(&entity)?;
        }

        info!("Ontology build complete");
        Ok(())
    }

    /// Apply semantic updates to the ontology
    #[instrument(skip(self, updates))]
    pub async fn apply_updates(&mut self, updates: Vec<SemanticUpdate>) -> Result<()> {
        info!("Applying {} semantic updates", updates.len());

        let mut graph = self.graph.write().await;

        for update in updates {
            match update {
                SemanticUpdate::AddTriple { subject, predicate, object } => {
                    graph.add_triple(&subject, &predicate, &object)?;
                }
                SemanticUpdate::RemoveTriple { subject, predicate, object } => {
                    graph.remove_triple(&subject, &predicate, &object)?;
                }
                SemanticUpdate::UpdateDocumentation { entity_id, documentation } => {
                    graph.update_documentation(&entity_id, &documentation)?;
                }
            }
        }

        Ok(())
    }

    /// Query the ontology using SPARQL
    #[instrument(skip(self, query))]
    pub async fn query(&self, query: &str) -> Result<serde_json::Value> {
        debug!("Executing SPARQL query");

        let graph = self.graph.read().await;
        graph.execute_query(query)
    }

    /// Get all entities of a specific kind
    #[instrument(skip(self))]
    pub async fn get_entities_by_kind(&self, kind: EntityKind) -> Result<Vec<CodeEntity>> {
        let query = self.queries.entities_by_kind(kind);
        let graph = self.graph.read().await;
        graph.get_entities(&query)
    }

    /// Get relationships for an entity
    #[instrument(skip(self))]
    pub async fn get_relationships(&self, entity_id: &str) -> Result<Vec<Relationship>> {
        let query = self.queries.relationships_for_entity(entity_id);
        let graph = self.graph.read().await;
        graph.get_relationships(&query)
    }

    /// Validate documentation completeness
    #[instrument(skip(self))]
    pub async fn validate_completeness(&self) -> Result<ValidationReport> {
        info!("Validating documentation completeness");

        let graph = self.graph.read().await;
        let all_entities = graph.get_all_entities()?;

        let total_entities = all_entities.len();
        let documented_entities: Vec<_> = all_entities
            .iter()
            .filter(|e| e.documentation.is_some() && !e.documentation.as_ref().unwrap().is_empty())
            .collect();

        let documented_count = documented_entities.len();
        let coverage_percentage = if total_entities > 0 {
            (documented_count as f64 / total_entities as f64) * 100.0
        } else {
            0.0
        };

        let undocumented_entities: Vec<String> = all_entities
            .iter()
            .filter(|e| e.documentation.is_none() || e.documentation.as_ref().unwrap().is_empty())
            .map(|e| e.name.clone())
            .collect();

        let missing_examples: Vec<String> = all_entities
            .iter()
            .filter(|e| e.examples.is_empty())
            .map(|e| e.name.clone())
            .collect();

        Ok(ValidationReport {
            total_entities,
            documented_entities: documented_count,
            undocumented_entities,
            missing_examples,
            coverage_percentage,
        })
    }

    /// Export ontology as RDF Turtle format
    #[instrument(skip(self))]
    pub async fn export_turtle(&self, output_path: impl AsRef<Path>) -> Result<()> {
        info!("Exporting ontology to Turtle format");

        let graph = self.graph.read().await;
        graph.export_turtle(output_path)?;

        Ok(())
    }

    /// Get dependency graph for visualization
    #[instrument(skip(self))]
    pub async fn get_dependency_graph(&self) -> Result<DependencyGraph> {
        let graph = self.graph.read().await;
        graph.build_dependency_graph()
    }
}

/// Semantic update operation
#[derive(Debug, Clone)]
pub enum SemanticUpdate {
    /// Add a new RDF triple
    AddTriple {
        subject: String,
        predicate: String,
        object: String,
    },

    /// Remove an RDF triple
    RemoveTriple {
        subject: String,
        predicate: String,
        object: String,
    },

    /// Update entity documentation
    UpdateDocumentation {
        entity_id: String,
        documentation: String,
    },
}

/// Dependency graph for visualization
#[derive(Debug, Clone, serde::Serialize)]
pub struct DependencyGraph {
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct GraphNode {
    pub id: String,
    pub label: String,
    pub kind: EntityKind,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct GraphEdge {
    pub source: String,
    pub target: String,
    pub kind: RelationshipKind,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_ontology_creation() {
        let config = OntologyConfig::default();
        let ontology = CodeOntology::new(&config);
        assert!(ontology.is_ok());
    }
}
