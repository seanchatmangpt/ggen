//! μ₁ (Normalize): RDF validation, SHACL shapes, dependency resolution

use crate::error::{CraftplanError, Result};
use oxigraph::io::RdfFormat;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;
use std::path::Path;

/// Normalized data from RDF validation stage
#[derive(Clone)]
pub struct NormalizedData {
    /// The RDF store containing validated triples
    pub store: Store,
    /// List of entity IRIs resolved during validation
    pub entities: Vec<String>,
}

pub struct Normalizer {
    store: Store,
}

impl Normalizer {
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new().map_err(|e| {
                CraftplanError::rdf_validation(format!("Failed to create RDF store: {}", e))
            })?,
        })
    }

    pub fn load_rdf(&mut self, path: impl AsRef<Path>) -> Result<()> {
        let path = path.as_ref();
        if !path.exists() {
            return Err(CraftplanError::FileNotFound {
                path: path.to_path_buf(),
            });
        }

        let rdf_content = std::fs::read_to_string(path).map_err(|e| CraftplanError::Io {
            path: path.to_path_buf(),
            source: e,
        })?;

        self.store
            .load_from_reader(RdfFormat::Turtle, rdf_content.as_bytes())
            .map_err(|e| CraftplanError::Parse {
                file_type: "Turtle".to_string(),
                reason: e.to_string(),
            })?;

        Ok(())
    }

    pub fn validate(&self) -> Result<usize> {
        let count = self.store.len().map_err(|e| {
            CraftplanError::rdf_validation(format!("Failed to get store size: {}", e))
        })?;

        if count == 0 {
            return Err(CraftplanError::rdf_validation(
                "RDF store is empty - no triples loaded",
            ));
        }

        Ok(count)
    }

    pub fn store(&self) -> &Store {
        &self.store
    }

    pub fn resolve_dependencies(&self) -> Result<Vec<String>> {
        let mut dependencies = Vec::new();

        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?entity WHERE {
                ?entity a ?type .
                FILTER(isBlank(?entity) = false)
            }
            "#;

        let results = SparqlEvaluator::new()
            .parse_query(query)
            .map_err(|e| CraftplanError::sparql_query(query, e.to_string()))?
            .on_store(&self.store)
            .execute()
            .map_err(|e| CraftplanError::sparql_query(query, e.to_string()))?;

        // Process QueryResults based on its actual type
        match results {
            QueryResults::Solutions(mut solutions) => {
                while let Some(solution) = solutions.next() {
                    let solution =
                        solution.map_err(|e| CraftplanError::sparql_query(query, e.to_string()))?;

                    if let Some(entity) = solution.get("entity") {
                        let entity_str = entity.to_string();
                        dependencies.push(entity_str);
                    }
                }
            }
            QueryResults::Boolean(_) => {
                return Err(CraftplanError::sparql_query(
                    query,
                    "Expected SELECT query, got ASK".to_string(),
                ));
            }
            QueryResults::Graph(_) => {
                return Err(CraftplanError::sparql_query(
                    query,
                    "Expected SELECT query, got CONSTRUCT/DESCRIBE".to_string(),
                ));
            }
        }

        Ok(dependencies)
    }
}

impl Default for Normalizer {
    fn default() -> Self {
        Self::new().expect("Failed to create normalizer")
    }
}
