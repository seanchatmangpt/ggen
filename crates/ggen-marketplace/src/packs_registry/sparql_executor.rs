//! SPARQL Executor for querying pack metadata as RDF
//!
//! This module provides SPARQL query execution capabilities for pack metadata.
//! Packs are converted to RDF graphs and can be queried using SPARQL.

use crate::marketplace::error::{Error, Result};
use crate::packs_registry::types::Pack;
use oxigraph::io::RdfFormat;
use oxigraph::model::*;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tracing::{debug, info};

/// Escape a string for use inside an N-Triples/Turtle quoted string literal (`"..."`):
/// backslash and double-quote must be escaped, and raw newlines/carriage-returns/tabs must be
/// escaped too since N-Triples string literals are single-line.
fn nt_escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// SPARQL executor for pack metadata queries
pub struct SparqlExecutor {
    /// In-memory RDF store
    store: Store,
    /// Query cache
    cache: HashMap<String, CachedResult>,
    /// Pack ids already loaded into `store`, so repeated queries over the same pack set don't
    /// re-insert (and don't silently duplicate) triples.
    loaded_pack_ids: std::collections::HashSet<String>,
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

/// Outcome of [`run_pack_query`]: the resolved scope description alongside the real
/// [`SparqlResult`] the query produced.
#[derive(Debug, Clone)]
pub struct PackQueryOutcome {
    /// `"pack:<id>"` when `pack_id` was given, else `"all-packs"`.
    pub scope: String,
    pub packs_queried: usize,
    pub result: SparqlResult,
}

/// Run a SPARQL query over one pack's RDF facts, or over every pack in the registry.
///
/// `pack_id = Some(id)` scopes to that one pack's facts; `pack_id = None` unions every pack
/// in the local registry. This is the single implementation shared by the `ggen pack query`
/// CLI verb and the `ggen_pack_query` MCP tool -- both are thin adapters around this.
pub fn run_pack_query(sparql: &str, pack_id: Option<&str>) -> Result<PackQueryOutcome> {
    let mut executor = SparqlExecutor::new()?;

    if let Some(id) = pack_id {
        let pack = crate::packs_registry::metadata::load_pack_metadata(id)
            .map_err(|e| Error::Other(format!("Pack '{}' not found: {}", id, e)))?;
        let result = executor.execute_query(&pack, sparql)?;
        Ok(PackQueryOutcome {
            scope: format!("pack:{}", id),
            packs_queried: 1,
            result,
        })
    } else {
        let packs = crate::packs_registry::metadata::list_packs(None)?;
        let packs_queried = packs.len();
        let result = executor.execute_query_over_packs(&packs, sparql)?;
        Ok(PackQueryOutcome {
            scope: "all-packs".to_string(),
            packs_queried,
            result,
        })
    }
}

impl SparqlExecutor {
    /// Create new SPARQL executor
    pub fn new() -> Result<Self> {
        Ok(Self {
            store: Store::new()
                .map_err(|e| Error::Other(format!("Failed to create RDF store: {e}")))?,
            cache: HashMap::new(),
            loaded_pack_ids: std::collections::HashSet::new(),
        })
    }

    /// Execute SPARQL query on a single pack's metadata.
    ///
    /// # Arguments
    /// * `pack` - The pack to query
    /// * `query` - SPARQL query string
    ///
    /// # Returns
    /// Query results with columns and rows
    pub fn execute_query(&mut self, pack: &Pack, query: &str) -> Result<SparqlResult> {
        let start = Instant::now();

        // Validate SPARQL syntax before touching pack RDF -- a syntactically invalid query
        // must fail fast without loading any pack data.
        let prepared = SparqlEvaluator::new()
            .parse_query(query)
            .map_err(|e| Error::Other(format!("SPARQL query failed: {e}")))?;

        // Check cache first
        let cache_key = format!("{}:{}", pack.id, query);
        if let Some(cached) = self.cache.get(&cache_key) {
            if cached.timestamp.elapsed() < cached.ttl {
                debug!("Cache hit for query on pack '{}'", pack.id);
                return Ok(cached.result.clone());
            }
        }

        // Load pack RDF into store (idempotent -- only inserted once per pack id).
        self.load_pack_rdf(pack)?;

        // Execute query using the store via the non-deprecated SparqlEvaluator surface.
        let results = prepared
            .on_store(&self.store)
            .execute()
            .map_err(|e| Error::Other(format!("SPARQL query failed: {e}")))?;

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

    /// Execute one SPARQL query over the union of several packs' RDF -- the machine-facing
    /// "search the whole marketplace" surface: load every pack's facts into one shared store
    /// (idempotent per pack id) and run a single query across all of them at once, rather than
    /// requiring a caller to loop `execute_query` per pack and merge results by hand.
    pub fn execute_query_over_packs(
        &mut self, packs: &[Pack], query: &str,
    ) -> Result<SparqlResult> {
        let start = Instant::now();

        // Validate SPARQL syntax before loading any pack RDF -- a syntactically invalid
        // query must fail fast without paying the I/O/parse cost of the whole registry.
        let prepared = SparqlEvaluator::new()
            .parse_query(query)
            .map_err(|e| Error::Other(format!("SPARQL query failed: {e}")))?;

        for pack in packs {
            self.load_pack_rdf(pack)?;
        }
        let results = prepared
            .on_store(&self.store)
            .execute()
            .map_err(|e| Error::Other(format!("SPARQL query failed: {e}")))?;
        self.convert_results(results, start.elapsed())
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
            return Err(Error::Other("Query cannot be empty".to_string()));
        }

        Ok(CompiledQuery {
            query_string: query.to_string(),
        })
    }

    /// Convert pack to RDF graph, as real N-Triples lines (`<s> <p> "o" .` / `<s> <p> <o> .`),
    /// suitable for direct parsing via `Store::load_from_reader(RdfFormat::NTriples, ...)`.
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
        let ggen_ns = "https://ggen.io/marketplace/";

        // Pack basic properties
        triples.push(format!(
            "<{}> <{}type> <{}Pack> .",
            pack_ns, rdf_ns, ggen_ns
        ));
        triples.push(format!(
            "<{}> <{}label> \"{}\" .",
            pack_ns,
            rdfs_ns,
            nt_escape(&pack.name)
        ));
        triples.push(format!(
            "<{}> <{}version> \"{}\" .",
            pack_ns,
            ggen_ns,
            nt_escape(&pack.version)
        ));
        triples.push(format!(
            "<{}> <{}description> \"{}\" .",
            pack_ns,
            ggen_ns,
            nt_escape(&pack.description)
        ));
        triples.push(format!(
            "<{}> <{}category> \"{}\" .",
            pack_ns,
            ggen_ns,
            nt_escape(&pack.category)
        ));

        // Optional fields
        if let Some(author) = &pack.author {
            triples.push(format!(
                "<{}> <{}author> \"{}\" .",
                pack_ns,
                ggen_ns,
                nt_escape(author)
            ));
        }

        if let Some(license) = &pack.license {
            triples.push(format!(
                "<{}> <{}license> \"{}\" .",
                pack_ns,
                ggen_ns,
                nt_escape(license)
            ));
        }

        // Production ready flag (a real xsd:boolean literal, not a quoted string, so
        // `FILTER(?ready = true)` works without a string-to-boolean cast)
        triples.push(format!(
            "<{}> <{}productionReady> \"{}\"^^<http://www.w3.org/2001/XMLSchema#boolean> .",
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
                pkg_uri,
                rdfs_ns,
                nt_escape(package)
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
                tmpl_uri,
                rdfs_ns,
                nt_escape(&template.name)
            ));
            triples.push(format!(
                "<{}> <{}path> \"{}\" .",
                tmpl_uri,
                ggen_ns,
                nt_escape(&template.path)
            ));
            triples.push(format!(
                "<{}> <{}description> \"{}\" .",
                tmpl_uri,
                ggen_ns,
                nt_escape(&template.description)
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
                dep_uri,
                ggen_ns,
                nt_escape(&dep.pack_id)
            ));
            triples.push(format!(
                "<{}> <{}version> \"{}\" .",
                dep_uri,
                ggen_ns,
                nt_escape(&dep.version)
            ));
            triples.push(format!(
                "<{}> <{}optional> \"{}\"^^<http://www.w3.org/2001/XMLSchema#boolean> .",
                dep_uri, ggen_ns, dep.optional
            ));
        }

        // Tags
        for tag in &pack.tags {
            triples.push(format!(
                "<{}> <{}tag> \"{}\" .",
                pack_ns,
                ggen_ns,
                nt_escape(tag)
            ));
        }

        // Keywords
        for keyword in &pack.keywords {
            triples.push(format!(
                "<{}> <{}keyword> \"{}\" .",
                pack_ns,
                ggen_ns,
                nt_escape(keyword)
            ));
        }

        Ok(triples)
    }

    /// Load pack RDF into the store. Idempotent per pack id: a pack already present in
    /// `loaded_pack_ids` is not re-parsed/re-inserted, so `execute_query`/
    /// `execute_query_over_packs` can be called repeatedly (including over an overlapping set of
    /// packs) without duplicating triples.
    ///
    /// This actually parses and inserts the generated N-Triples into `self.store` -- a prior
    /// version of this function built the triple strings and only logged them via `debug!`,
    /// which meant every query in this executor silently ran against an empty store (Decorative
    /// Completion: `get_pack_rdf` looked real, but no fact it produced was ever queryable).
    fn load_pack_rdf(&mut self, pack: &Pack) -> Result<()> {
        if self.loaded_pack_ids.contains(&pack.id) {
            debug!(
                "Pack '{}' already loaded into SPARQL store; skipping",
                pack.id
            );
            return Ok(());
        }

        let triples = self.get_pack_rdf(pack)?;
        let triple_count = triples.len();
        let document = triples.join("\n");

        self.store
            .load_from_reader(RdfFormat::NTriples, document.as_bytes())
            .map_err(|e| {
                Error::Other(format!(
                    "Failed to parse/insert RDF for pack '{}': {}",
                    pack.id, e
                ))
            })?;

        self.loaded_pack_ids.insert(pack.id.clone());
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
                        .map_err(|e| Error::Other(format!("Failed to process solution: {}", e)))?;

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
            QueryResults::Graph(_) => Err(Error::Other(
                "CONSTRUCT queries not yet supported (use SELECT queries)".to_string(),
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
            Term::Triple(t) => Value::String(format!("{}", t)),
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

impl Default for SparqlExecutor {
    fn default() -> Self {
        Self::new().expect("Failed to create default SPARQL executor")
    }
}

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
    use crate::packs_registry::types::{PackDependency, PackMetadata, PackTemplate};
    use std::collections::HashMap;

    fn create_test_pack() -> Pack {
        Pack {
            id: "test-pack".to_string(),
            name: "Test Pack".to_string(),
            version: "1.0.0".to_string(),
            description: "A test pack for SPARQL".to_string(),
            category: "testing".to_string(),
            registry_type: None,
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

    /// State-based regression test for the real bug this file had: `load_pack_rdf` used to
    /// build N-Triples strings and only `debug!`-log them, never inserting them into the store,
    /// so every query ran against an empty graph. This proves a query against real, loaded pack
    /// facts returns the real value, not an empty result set.
    #[test]
    fn execute_query_returns_real_loaded_pack_facts() {
        let mut executor = SparqlExecutor::new().unwrap();
        let pack = create_test_pack();

        let result = executor
            .execute_query(
                &pack,
                "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \
                 PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \
                 PREFIX ggen: <https://ggen.io/marketplace/> \
                 SELECT ?label WHERE { ?pack a ggen:Pack ; rdfs:label ?label }",
            )
            .unwrap();

        assert_eq!(result.rows.len(), 1, "expected exactly one pack label row");
        match &result.rows[0][0] {
            Value::String(label) => assert_eq!(label, "Test Pack"),
            other => panic!("expected a string label, got {other:?}"),
        }
    }

    /// Proves `execute_query_over_packs` unions facts from multiple distinct packs into one
    /// queryable graph -- the real "search the whole marketplace at once" capability, not a
    /// per-pack loop the caller has to merge by hand.
    #[test]
    fn execute_query_over_packs_unions_multiple_packs() {
        let mut executor = SparqlExecutor::new().unwrap();
        let mut pack_a = create_test_pack();
        pack_a.id = "pack-a".to_string();
        pack_a.name = "Pack A".to_string();
        let mut pack_b = create_test_pack();
        pack_b.id = "pack-b".to_string();
        pack_b.name = "Pack B".to_string();

        let result = executor
            .execute_query_over_packs(
                &[pack_a, pack_b],
                "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \
                 PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \
                 PREFIX ggen: <https://ggen.io/marketplace/> \
                 SELECT ?label WHERE { ?pack a ggen:Pack ; rdfs:label ?label } ORDER BY ?label",
            )
            .unwrap();

        let labels: Vec<String> = result
            .rows
            .iter()
            .map(|row| match &row[0] {
                Value::String(s) => s.clone(),
                other => panic!("expected a string label, got {other:?}"),
            })
            .collect();
        assert_eq!(labels, vec!["Pack A".to_string(), "Pack B".to_string()]);
    }

    /// A literal containing characters that would corrupt a naively-interpolated N-Triples
    /// string (quotes, a backslash, a newline) must still round-trip through real parsing --
    /// this is exactly the class of bug `nt_escape` exists to prevent.
    #[test]
    fn load_pack_rdf_escapes_literals_with_quotes_and_newlines() {
        let mut executor = SparqlExecutor::new().unwrap();
        let mut pack = create_test_pack();
        pack.description = "A \"quoted\" pack with a backslash \\ and a\nnewline".to_string();

        let result = executor
            .execute_query(
                &pack,
                "PREFIX ggen: <https://ggen.io/marketplace/> \
                 SELECT ?desc WHERE { ?pack a ggen:Pack ; ggen:description ?desc }",
            )
            .unwrap();

        assert_eq!(result.rows.len(), 1);
        match &result.rows[0][0] {
            Value::String(desc) => assert_eq!(desc, &pack.description),
            other => panic!("expected a string description, got {other:?}"),
        }
    }

    /// `run_pack_query` against a real, deliberately-nonexistent pack id must return a real
    /// `Err` (not panic, not a silently-empty success) -- the underlying real-loaded-facts and
    /// union behavior are already covered by `execute_query_returns_real_loaded_pack_facts` and
    /// `execute_query_over_packs_unions_multiple_packs` above.
    #[test]
    fn run_pack_query_returns_err_for_nonexistent_pack() {
        let result = run_pack_query(
            "SELECT ?s WHERE { ?s ?p ?o }",
            Some("definitely-does-not-exist-pack-id"),
        );
        assert!(result.is_err(), "expected Err for nonexistent pack id");
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
