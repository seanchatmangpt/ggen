//! μ₁ (Normalize): RDF validation, SHACL shapes, dependency resolution

use crate::error::{CraftplanError, Result};
use oxigraph::store::Store;
use std::path::Path;

pub struct Normalizer {
    store: Store,
}

impl Normalizer {
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new(),
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

        oxigraph::io::read::parse Turtle.parse(&mut self.store, &rdf_content, None)
            .map_err(|e| CraftplanError::Parse {
                file_type: "Turtle".to_string(),
                reason: e.to_string(),
            })?;

        Ok(())
    }

    pub fn validate(&self) -> Result<usize> {
        let count = self.store.len();

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

        let results = self
            .store
            .query(query)
            .map_err(|e| CraftplanError::sparql_query(query, e.to_string()))?;

        for result in results {
            if let Ok(entity) = result.get("entity") {
                let entity_str = entity.to_string();
                dependencies.push(entity_str);
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
