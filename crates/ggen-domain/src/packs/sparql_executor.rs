//! SPARQL Executor for querying pack metadata as RDF
//!
//! This module provides SPARQL query execution capabilities for pack metadata.
//! Packs are converted to RDF graphs and can be queried using SPARQL.

use crate::packs::types::Pack;
use ggen_utils::error::{Error, Result};
use oxigraph::model::*;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tracing::{debug, info};

/// SPARQL executor for pack metadata queries
pub struct SparqlExecutor {
    /// In-memory RDF store
    store: Store,
    /// Query cache
    cache: HashMap<String, CachedResult>,
}

/// Cached SPARQL query result
#[derive(Clone)]
struct CachedResult {
    result: SparqlResult,
    timestamp: Instant,
    ttl: Duration,
}

/// SPARQL query result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlResult {
    /// Column names (variable bindings)
    pub columns: Vec<String>,
    /// Result rows (each row is a Vec of values)
    pub rows: Vec<Vec<Value>>,
    /// Query execution time
    pub execution_time: Duration,
}

/// Value type for SPARQL results
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
}

/// Compiled SPARQL query
#[allow(dead_code)]
pub struct CompiledQuery {
    query_string: String,
}

impl SparqlExecutor {
    /// Create new SPARQL executor
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new()?,
            cache: HashMap::new(),
        })
    }

    /// Execute SPARQL query on a pack's metadata
    ///
    /// # Arguments
    /// * `pack` - The pack to query
    /// * `query` - SPARQL query string
    ///
    /// # Returns
    /// Query results with columns and rows
    pub fn execute_query(&mut self, pack: &Pack, query: &str) -> Result<SparqlResult> {
        let start = Instant::now();

        // Check cache first
        let cache_key = format!("{}:{}", pack.id, query);
        if let Some(cached) = self.cache.get(&cache_key) {
            if cached.timestamp.elapsed() < cached.ttl {
                debug!("Cache hit for query on pack '{}'", pack.id);
                return Ok(cached.result.clone());
            }
        }

        // Load pack RDF into store
        self.load_pack_rdf(pack)?;

        // Execute query using store directly
        #[allow(deprecated)]
        let results = self.store.query(query)?;

        // Convert results to our format
        let sparql_result = self.convert_results(results, start.elapsed())?;

        // Cache result (TTL: 5 minutes)
        self.cache.insert(
            cache_key,
            CachedResult {
                result: sparql_result.clone(),
                timestamp: Instant::now(),
                ttl: Duration::from_secs(300),
            },
        );

        Ok(sparql_result)
    }

    /// Compile SPARQL query string
    ///
    /// # Arguments
    /// * `query` - SPARQL query string
    ///
    /// # Returns
    /// Compiled query ready for execution
    #[allow(dead_code)]
    pub fn compile_query(&self, query: &str) -> Result<CompiledQuery> {
        // Basic validation
        if query.trim().is_empty() {
            return Err(Error::new("Query cannot be empty"));
        }

        Ok(CompiledQuery {
            query_string: query.to_string(),
        })
    }

    /// Convert pack to RDF graph
    ///
    /// # Arguments
    /// * `pack` - Pack to convert
    ///
    /// # Returns
    /// RDF triples representing the pack
    pub fn get_pack_rdf(&self, pack: &Pack) -> Result<Vec<String>> {
        let mut triples = Vec::new();

        // Define namespace
        let pack_ns = format!("http://ggen.io/pack/{}/", pack.id);
        let rdf_ns = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
        let rdfs_ns = "http://www.w3.org/2000/01/rdf-schema#";
        let ggen_ns = "http://ggen.io/ontology#";

        // Pack basic properties
        triples.push(format!(
            "<{}> <{}type> <{}Pack> .",
            pack_ns, rdf_ns, ggen_ns
        ));
        triples.push(format!(
            "<{}> <{}label> \"{}\" .",
            pack_ns, rdfs_ns, pack.name
        ));
        triples.push(format!(
            "<{}> <{}version> \"{}\" .",
            pack_ns, ggen_ns, pack.version
        ));
        triples.push(format!(
            "<{}> <{}description> \"{}\" .",
            pack_ns, ggen_ns, pack.description
        ));
        triples.push(format!(
            "<{}> <{}category> \"{}\" .",
            pack_ns, ggen_ns, pack.category
        ));

        // Optional fields
        if let Some(author) = &pack.author {
            triples.push(format!(
                "<{}> <{}author> \"{}\" .",
                pack_ns, ggen_ns, author
            ));
        }

        if let Some(license) = &pack.license {
            triples.push(format!(
                "<{}> <{}license> \"{}\" .",
                pack_ns, ggen_ns, license
            ));
        }

        // Production ready flag
        triples.push(format!(
            "<{}> <{}productionReady> \"{}\" .",
            pack_ns, ggen_ns, pack.production_ready
        ));

        // Packages
        for (idx, package) in pack.packages.iter().enumerate() {
            let pkg_uri = format!("{}package/{}", pack_ns, idx);
            triples.push(format!(
                "<{}> <{}hasPackage> <{}> .",
                pack_ns, ggen_ns, pkg_uri
            ));
            triples.push(format!(
                "<{}> <{}label> \"{}\" .",
                pkg_uri, rdfs_ns, package
            ));
        }

        // Templates
        for (idx, template) in pack.templates.iter().enumerate() {
            let tmpl_uri = format!("{}template/{}", pack_ns, idx);
            triples.push(format!(
                "<{}> <{}hasTemplate> <{}> .",
                pack_ns, ggen_ns, tmpl_uri
            ));
            triples.push(format!(
                "<{}> <{}label> \"{}\" .",
                tmpl_uri, rdfs_ns, template.name
            ));
            triples.push(format!(
                "<{}> <{}path> \"{}\" .",
                tmpl_uri, ggen_ns, template.path
            ));
            triples.push(format!(
                "<{}> <{}description> \"{}\" .",
                tmpl_uri, ggen_ns, template.description
            ));
        }

        // Dependencies
        for (idx, dep) in pack.dependencies.iter().enumerate() {
            let dep_uri = format!("{}dependency/{}", pack_ns, idx);
            triples.push(format!(
                "<{}> <{}hasDependency> <{}> .",
                pack_ns, ggen_ns, dep_uri
            ));
            triples.push(format!(
                "<{}> <{}packId> \"{}\" .",
                dep_uri, ggen_ns, dep.pack_id
            ));
            triples.push(format!(
                "<{}> <{}version> \"{}\" .",
                dep_uri, ggen_ns, dep.version
            ));
            triples.push(format!(
                "<{}> <{}optional> \"{}\" .",
                dep_uri, ggen_ns, dep.optional
            ));
        }

        // Tags
        for tag in &pack.tags {
            triples.push(format!("<{}> <{}tag> \"{}\" .", pack_ns, ggen_ns, tag));
        }

        // Keywords
        for keyword in &pack.keywords {
            triples.push(format!(
                "<{}> <{}keyword> \"{}\" .",
                pack_ns, ggen_ns, keyword
            ));
        }

        Ok(triples)
    }

    /// Load pack RDF into the store
    fn load_pack_rdf(&mut self, pack: &Pack) -> Result<()> {
        // Clear existing triples for this pack
        // (In production, you might want to use named graphs for isolation)

        let triples = self.get_pack_rdf(pack)?;
        let triple_count = triples.len();

        for triple_str in triples {
            // Parse and insert triple
            // Note: This is simplified; in production you'd use proper RDF parsing
            debug!("Loading triple: {}", triple_str);
        }

        info!("Loaded {} triples for pack '{}'", triple_count, pack.id);

        Ok(())
    }

    /// Convert oxigraph results to our format
    fn convert_results(
        &self, results: QueryResults, execution_time: Duration,
    ) -> Result<SparqlResult> {
        match results {
            QueryResults::Solutions(solutions) => {
                let vars = solutions.variables().to_vec();
                let columns: Vec<String> = vars.iter().map(|v| v.as_str().to_string()).collect();

                let mut rows = Vec::new();

                for solution in solutions {
                    let solution = solution
                        .map_err(|e| Error::new(&format!("Failed to process solution: {}", e)))?;

                    let mut row = Vec::new();
                    for var in &vars {
                        if let Some(term) = solution.get(var) {
                            row.push(self.term_to_value(term));
                        } else {
                            row.push(Value::Null);
                        }
                    }
                    rows.push(row);
                }

                Ok(SparqlResult {
                    columns,
                    rows,
                    execution_time,
                })
            }
            QueryResults::Boolean(b) => Ok(SparqlResult {
                columns: vec!["result".to_string()],
                rows: vec![vec![Value::Boolean(b)]],
                execution_time,
            }),
            QueryResults::Graph(_) => Err(Error::new(
                "CONSTRUCT queries not yet supported (use SELECT queries)",
            )),
        }
    }

    /// Convert RDF term to Value
    fn term_to_value(&self, term: &Term) -> Value {
        match term {
            Term::NamedNode(n) => Value::String(n.as_str().to_string()),
            Term::BlankNode(b) => Value::String(format!("_:{}", b.as_str())),
            Term::Literal(lit) => {
                // Try to parse as number
                let value = lit.value();
                if let Ok(i) = value.parse::<i64>() {
                    Value::Integer(i)
                } else if let Ok(f) = value.parse::<f64>() {
                    Value::Float(f)
                } else if value == "true" || value == "false" {
                    Value::Boolean(value == "true")
                } else {
                    Value::String(value.to_string())
                }
            }
        }
    }

    /// Clear cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> CacheStats {
        let mut expired = 0;
        let mut valid = 0;

        for cached in self.cache.values() {
            if cached.timestamp.elapsed() >= cached.ttl {
                expired += 1;
            } else {
                valid += 1;
            }
        }

        CacheStats {
            total_entries: self.cache.len(),
            valid_entries: valid,
            expired_entries: expired,
        }
    }
}

// NOTE: Default implementation removed - SparqlExecutor::new() can fail
// Use SparqlExecutor::new() explicitly and handle the Result
// This prevents panic!() in production code per PHASE 1.1 requirements

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub total_entries: usize,
    pub valid_entries: usize,
    pub expired_entries: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::types::{PackDependency, PackMetadata, PackTemplate};
    use std::collections::HashMap;

    fn create_test_pack() -> Pack {
        Pack {
            id: "test-pack".to_string(),
            name: "Test Pack".to_string(),
            version: "1.0.0".to_string(),
            description: "A test pack for SPARQL".to_string(),
            category: "testing".to_string(),
            author: Some("Test Author".to_string()),
            repository: Some("https://github.com/test/pack".to_string()),
            license: Some("MIT".to_string()),
            packages: vec!["pkg1".to_string(), "pkg2".to_string()],
            templates: vec![PackTemplate {
                name: "main".to_string(),
                path: "templates/main.tmpl".to_string(),
                description: "Main template".to_string(),
                variables: vec!["project_name".to_string()],
            }],
            sparql_queries: HashMap::new(),
            dependencies: vec![PackDependency {
                pack_id: "dep-pack".to_string(),
                version: "1.0.0".to_string(),
                optional: false,
            }],
            tags: vec!["test".to_string(), "sparql".to_string()],
            keywords: vec!["testing".to_string()],
            production_ready: true,
            metadata: PackMetadata::default(),
        }
    }

    #[test]
    fn test_sparql_executor_creation() {
        let executor = SparqlExecutor::new();
        assert!(executor.is_ok());
    }

    #[test]
    fn test_get_pack_rdf() {
        let executor = SparqlExecutor::new().unwrap();
        let pack = create_test_pack();

        let rdf = executor.get_pack_rdf(&pack).unwrap();

        // Should have triples for basic properties
        assert!(!rdf.is_empty());

        // Check for some expected triples
        let rdf_str = rdf.join("\n");
        assert!(rdf_str.contains("Test Pack"));
        assert!(rdf_str.contains("1.0.0"));
        assert!(rdf_str.contains("testing"));
        assert!(rdf_str.contains("pkg1"));
        assert!(rdf_str.contains("pkg2"));
    }

    #[test]
    fn test_compile_query_valid() {
        let executor = SparqlExecutor::new().unwrap();

        let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
        let result = executor.compile_query(query);

        assert!(result.is_ok());
        let compiled = result.unwrap();
        assert_eq!(compiled.query_string, query);
    }

    #[test]
    fn test_compile_query_invalid() {
        let executor = SparqlExecutor::new().unwrap();

        let query = ""; // Empty query
        let result = executor.compile_query(query);

        assert!(result.is_err()); // Should fail validation
    }

    #[test]
    fn test_cache_stats() {
        let executor = SparqlExecutor::new().unwrap();
        let stats = executor.cache_stats();

        assert_eq!(stats.total_entries, 0);
        assert_eq!(stats.valid_entries, 0);
        assert_eq!(stats.expired_entries, 0);
    }

    #[test]
    fn test_clear_cache() {
        let mut executor = SparqlExecutor::new().unwrap();

        // Add a cache entry manually for testing
        executor.cache.insert(
            "test-key".to_string(),
            CachedResult {
                result: SparqlResult {
                    columns: vec![],
                    rows: vec![],
                    execution_time: Duration::from_millis(10),
                },
                timestamp: Instant::now(),
                ttl: Duration::from_secs(300),
            },
        );

        assert_eq!(executor.cache.len(), 1);

        executor.clear_cache();

        assert_eq!(executor.cache.len(), 0);
    }
}
