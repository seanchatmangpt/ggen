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
pub struct ExtractionContext {
    pub _entities: HashMap<String, Entity>,
}

impl Extractor {
    pub fn new(_normalizer: &Normalizer) -> Result<Self> {
        Ok(Self {
            context: ExtractionContext {
                _entities: HashMap::new(),
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
            QueryResults::Solutions(solutions) => {
                for solution in solutions {
                    let solution = solution.map_err(|e| {
                        crate::error::CraftplanError::sparql_query(query, e.to_string())
                    })?;

                    let name = solution.get("name").ok_or_else(|| {
                        crate::error::CraftplanError::rdf_validation("Missing 'name' binding")
                    })?;

                    // Extract string value from RDF literal, stripping quotes
                    let name_str = name.to_string();
                    let name_str = name_str.strip_prefix('"').unwrap_or(&name_str);
                    let name_str = name_str.strip_suffix('"').unwrap_or(name_str);
                    let name_str = name_str.to_string();

                    let plural_str = solution.get("plural").map(|p| {
                        let p_str = p.to_string();
                        let p_str = p_str.strip_prefix('"').unwrap_or(&p_str);
                        let p_str = p_str.strip_suffix('"').unwrap_or(p_str);
                        p_str.to_string()
                    });

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
            QueryResults::Solutions(solutions) => {
                for solution in solutions {
                    let solution = solution.map_err(|e| {
                        crate::error::CraftplanError::sparql_query(&query, e.to_string())
                    })?;

                    if let (Some(attr_name), Some(type_)) =
                        (solution.get("attr_name"), solution.get("type"))
                    {
                        // Strip quotes from RDF literals
                        let attr_name_str = attr_name.to_string();
                        let attr_name_str = attr_name_str.strip_prefix('"').unwrap_or(&attr_name_str);
                        let attr_name_str = attr_name_str.strip_suffix('"').unwrap_or(attr_name_str);

                        let type_str = type_.to_string();
                        let type_str = type_str.strip_prefix('"').unwrap_or(&type_str);
                        let type_str = type_str.strip_suffix('"').unwrap_or(type_str);

                        let required = solution
                            .get("required")
                            .map(|r| {
                                let r_str = r.to_string();
                                let r_str = r_str.strip_prefix('"').unwrap_or(&r_str);
                                r_str.strip_suffix('"').unwrap_or(r_str) == "true"
                            })
                            .unwrap_or(false);

                        let doc = solution.get("doc").map(|d| {
                            let d_str = d.to_string();
                            let d_str = d_str.strip_prefix('"').unwrap_or(&d_str);
                            let d_str = d_str.strip_suffix('"').unwrap_or(d_str);
                            d_str.to_string()
                        });

                        attributes.push(Attribute {
                            name: attr_name_str.to_string(),
                            type_: type_str.to_string(),
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
