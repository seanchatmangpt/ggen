//! μ₂ (Extract): SPARQL queries, OWL inference, rule execution

use crate::error::Result;
use crate::normalize::Normalizer;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ElixirModule {
    pub module_name: String,
    pub doc: Option<String>,
    pub entities: Vec<Entity>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Entity {
    pub name: String,
    pub plural: Option<String>,
    pub attributes: Vec<Attribute>,
    pub relationships: Vec<Relationship>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Attribute {
    pub name: String,
    pub type_: String,
    pub required: bool,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Relationship {
    pub name: String,
    pub cardinality: String,
    pub target_entity: String,
    pub inverse_of: Option<String>,
}

pub struct Extractor {
    context: ExtractionContext,
}

#[derive(Debug, Clone)]
struct ExtractionContext {
    entities: HashMap<String, Entity>,
}

impl Extractor {
    pub fn new(_normalizer: &Normalizer) -> Result<Self> {
        Ok(Self {
            context: ExtractionContext {
                entities: HashMap::new(),
            },
        })
    }

    pub fn extract_entities(&mut self, store: &oxigraph::store::Store) -> Result<Vec<Entity>> {
        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX craft: <http://craftplan.org/ontology/>
            SELECT ?entity ?name ?plural WHERE {
                ?entity a craft:Entity ;
                        craft:name ?name .
                OPTIONAL { ?entity craft:pluralName ?plural }
            }
            "#;

        let results = SparqlEvaluator::new()
            .parse_query(query)
            .map_err(|e| crate::error::CraftplanError::sparql_query(query, e.to_string()))?
            .on_store(store)
            .execute()
            .map_err(|e| crate::error::CraftplanError::sparql_query(query, e.to_string()))?;

        let mut entities = Vec::new();

        // Process QueryResults based on its actual type
        match results {
            QueryResults::Solutions(mut solutions) => {
                while let Some(solution) = solutions.next() {
                    let solution = solution.map_err(|e| {
                        crate::error::CraftplanError::sparql_query(query, e.to_string())
                    })?;

                    let name = solution.get("name").ok_or_else(|| {
                        crate::error::CraftplanError::rdf_validation("Missing 'name' binding")
                    })?;

                    let name_str = name.to_string();
                    let plural_str = solution.get("plural").map(|p| p.to_string());

                    entities.push(Entity {
                        name: name_str,
                        plural: plural_str,
                        attributes: Vec::new(),
                        relationships: Vec::new(),
                    });
                }
            }
            QueryResults::Boolean(_) => {
                return Err(crate::error::CraftplanError::sparql_query(
                    query,
                    "Expected SELECT query, got ASK".to_string(),
                ));
            }
            QueryResults::Graph(_) => {
                return Err(crate::error::CraftplanError::sparql_query(
                    query,
                    "Expected SELECT query, got CONSTRUCT/DESCRIBE".to_string(),
                ));
            }
        }

        Ok(entities)
    }

    pub fn extract_attributes(
        &self, store: &oxigraph::store::Store, entity_name: &str,
    ) -> Result<Vec<Attribute>> {
        let query = format!(
            r#"
            PREFIX craft: <http://craftplan.org/ontology/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            SELECT ?attr_name ?type ?required ?doc WHERE {{
                ?entity craft:name "{entity_name}" ;
                        craft:hasAttribute ?attr .
                ?attr craft:name ?attr_name ;
                      craft:type ?type .
                OPTIONAL {{ ?attr craft:required ?required }}
                OPTIONAL {{ ?attr craft:documentation ?doc }}
            }}
            "#
        );

        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .map_err(|e| crate::error::CraftplanError::sparql_query(&query, e.to_string()))?
            .on_store(store)
            .execute()
            .map_err(|e| crate::error::CraftplanError::sparql_query(&query, e.to_string()))?;

        let mut attributes = Vec::new();

        // Process QueryResults based on its actual type
        match results {
            QueryResults::Solutions(mut solutions) => {
                while let Some(solution) = solutions.next() {
                    let solution = solution.map_err(|e| {
                        crate::error::CraftplanError::sparql_query(&query, e.to_string())
                    })?;

                    if let (Some(attr_name), Some(type_)) = (solution.get("attr_name"), solution.get("type")) {
                        let required = solution
                            .get("required")
                            .map(|r| r.to_string() == "true")
                            .unwrap_or(false);

                        let doc = solution.get("doc").map(|d| d.to_string());

                        attributes.push(Attribute {
                            name: attr_name.to_string(),
                            type_: type_.to_string(),
                            required,
                            doc,
                        });
                    }
                }
            }
            QueryResults::Boolean(_) => {
                return Err(crate::error::CraftplanError::sparql_query(
                    &query,
                    "Expected SELECT query, got ASK".to_string(),
                ));
            }
            QueryResults::Graph(_) => {
                return Err(crate::error::CraftplanError::sparql_query(
                    &query,
                    "Expected SELECT query, got CONSTRUCT/DESCRIBE".to_string(),
                ));
            }
        }

        Ok(attributes)
    }

    pub fn context(&self) -> &ExtractionContext {
        &self.context
    }
}
