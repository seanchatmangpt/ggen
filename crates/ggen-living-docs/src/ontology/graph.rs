//! RDF graph management for code ontology

use super::schema::{CodeEntity, EntityKind, Relationship};
use super::{DependencyGraph, GraphNode, GraphEdge};
use crate::{Error, Result, config::OntologyConfig};
use oxigraph::store::Store;
use oxigraph::model::*;
use oxigraph::sparql::QueryResults;
use std::path::Path;
use std::fs;
use tracing::{debug, warn};

/// RDF graph storage and querying for code ontology
pub struct OntologyGraph {
    store: Store,
    base_uri: String,
    namespace: String,
}

impl OntologyGraph {
    /// Create a new ontology graph
    pub fn new(config: &OntologyConfig) -> Result<Self> {
        // Create store directory if it doesn't exist
        if let Some(parent) = config.store_path.parent() {
            fs::create_dir_all(parent).map_err(Error::Io)?;
        }

        let store = Store::open(&config.store_path)
            .map_err(|e| Error::Ontology(format!("Failed to open RDF store: {}", e)))?;

        Ok(Self {
            store,
            base_uri: config.base_uri.clone(),
            namespace: config.namespace_prefix.clone(),
        })
    }

    /// Add a code entity to the graph
    pub fn add_entity(&mut self, entity: &CodeEntity) -> Result<()> {
        debug!("Adding entity to graph: {}", entity.id);

        let entity_uri = self.entity_uri(&entity.id);
        let entity_subject = NamedNode::new(&entity_uri)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;

        // Add entity type
        let rdf_type = NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let entity_type = NamedNode::new(&format!("{}#{}", self.base_uri, entity.kind))
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;

        self.store.insert(&Quad::new(
            entity_subject.clone(),
            rdf_type,
            entity_type,
            GraphName::DefaultGraph,
        )).map_err(|e| Error::Ontology(format!("Failed to insert triple: {}", e)))?;

        // Add entity name
        let name_predicate = self.predicate_uri("name");
        self.add_literal_triple(&entity_subject, &name_predicate, &entity.name)?;

        // Add file path
        let file_predicate = self.predicate_uri("filePath");
        self.add_literal_triple(&entity_subject, &file_predicate, &entity.file_path)?;

        // Add documentation if present
        if let Some(doc) = &entity.documentation {
            let doc_predicate = self.predicate_uri("documentation");
            self.add_literal_triple(&entity_subject, &doc_predicate, doc)?;
        }

        // Add relationships
        for relationship in &entity.relationships {
            self.add_relationship(relationship)?;
        }

        // Add attributes
        for (key, value) in &entity.attributes {
            let attr_predicate = self.predicate_uri(key);
            self.add_literal_triple(&entity_subject, &attr_predicate, value)?;
        }

        Ok(())
    }

    /// Add a relationship to the graph
    fn add_relationship(&mut self, relationship: &Relationship) -> Result<()> {
        let source_uri = self.entity_uri(&relationship.source);
        let target_uri = self.entity_uri(&relationship.target);

        let source = NamedNode::new(&source_uri)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let predicate = NamedNode::new(&format!("{}#{}", self.base_uri, relationship.kind))
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let target = NamedNode::new(&target_uri)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;

        self.store.insert(&Quad::new(
            source,
            predicate,
            target,
            GraphName::DefaultGraph,
        )).map_err(|e| Error::Ontology(format!("Failed to insert relationship: {}", e)))?;

        Ok(())
    }

    /// Add a literal triple
    fn add_literal_triple(&mut self, subject: &NamedNode, predicate: &str, value: &str) -> Result<()> {
        let pred = NamedNode::new(predicate)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let obj = Literal::new_simple_literal(value);

        self.store.insert(&Quad::new(
            subject.clone(),
            pred,
            obj,
            GraphName::DefaultGraph,
        )).map_err(|e| Error::Ontology(format!("Failed to insert triple: {}", e)))?;

        Ok(())
    }

    /// Add an RDF triple
    pub fn add_triple(&mut self, subject: &str, predicate: &str, object: &str) -> Result<()> {
        let subj = NamedNode::new(subject)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let pred = NamedNode::new(predicate)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let obj = NamedNode::new(object)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;

        self.store.insert(&Quad::new(
            subj,
            pred,
            obj,
            GraphName::DefaultGraph,
        )).map_err(|e| Error::Ontology(format!("Failed to insert triple: {}", e)))?;

        Ok(())
    }

    /// Remove an RDF triple
    pub fn remove_triple(&mut self, subject: &str, predicate: &str, object: &str) -> Result<()> {
        let subj = NamedNode::new(subject)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let pred = NamedNode::new(predicate)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;
        let obj = NamedNode::new(object)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;

        self.store.remove(&Quad::new(
            subj,
            pred,
            obj,
            GraphName::DefaultGraph,
        )).map_err(|e| Error::Ontology(format!("Failed to remove triple: {}", e)))?;

        Ok(())
    }

    /// Update entity documentation
    pub fn update_documentation(&mut self, entity_id: &str, documentation: &str) -> Result<()> {
        let entity_uri = self.entity_uri(entity_id);
        let entity = NamedNode::new(&entity_uri)
            .map_err(|e| Error::Ontology(format!("Invalid URI: {}", e)))?;

        // Remove old documentation
        let doc_predicate = self.predicate_uri("documentation");
        // Note: In a real implementation, we'd query for existing doc and remove it first

        // Add new documentation
        self.add_literal_triple(&entity, &doc_predicate, documentation)?;

        Ok(())
    }

    /// Execute a SPARQL query
    pub fn execute_query(&self, query: &str) -> Result<serde_json::Value> {
        debug!("Executing SPARQL query");

        let results = self.store.query(query)
            .map_err(|e| Error::Ontology(format!("Query failed: {}", e)))?;

        // Convert results to JSON
        match results {
            QueryResults::Solutions(solutions) => {
                let mut rows = Vec::new();
                for solution in solutions {
                    let solution = solution
                        .map_err(|e| Error::Ontology(format!("Solution error: {}", e)))?;

                    let mut row = serde_json::Map::new();
                    for (var, value) in solution.iter() {
                        row.insert(var.as_str().to_string(), serde_json::Value::String(value.to_string()));
                    }
                    rows.push(serde_json::Value::Object(row));
                }
                Ok(serde_json::Value::Array(rows))
            }
            QueryResults::Boolean(b) => {
                Ok(serde_json::json!({ "boolean": b }))
            }
            QueryResults::Graph(_) => {
                Ok(serde_json::json!({ "graph": "Graph result not yet supported" }))
            }
        }
    }

    /// Get entities matching a query
    pub fn get_entities(&self, query: &str) -> Result<Vec<CodeEntity>> {
        // For simplicity, returning empty vec
        // In a real implementation, parse SPARQL results into CodeEntity objects
        warn!("get_entities: Returning empty result (not implemented)");
        Ok(Vec::new())
    }

    /// Get relationships matching a query
    pub fn get_relationships(&self, query: &str) -> Result<Vec<Relationship>> {
        warn!("get_relationships: Returning empty result (not implemented)");
        Ok(Vec::new())
    }

    /// Get all entities from the graph
    pub fn get_all_entities(&self) -> Result<Vec<CodeEntity>> {
        warn!("get_all_entities: Returning empty result (not implemented)");
        Ok(Vec::new())
    }

    /// Build dependency graph for visualization
    pub fn build_dependency_graph(&self) -> Result<DependencyGraph> {
        // Simplified implementation
        Ok(DependencyGraph {
            nodes: Vec::new(),
            edges: Vec::new(),
        })
    }

    /// Export graph as Turtle format
    pub fn export_turtle(&self, output_path: impl AsRef<Path>) -> Result<()> {
        debug!("Exporting graph to Turtle format");

        // Get all quads from the store
        let mut turtle_content = String::new();
        turtle_content.push_str(&format!("@prefix {} : <{}> .\n\n", self.namespace, self.base_uri));

        // In a real implementation, serialize all quads to Turtle format
        fs::write(output_path.as_ref(), turtle_content)
            .map_err(Error::Io)?;

        Ok(())
    }

    /// Helper to create entity URI
    fn entity_uri(&self, id: &str) -> String {
        format!("{}{}", self.base_uri, id)
    }

    /// Helper to create predicate URI
    fn predicate_uri(&self, name: &str) -> String {
        format!("{}{}", self.base_uri, name)
    }
}
