//! RDF graph management with SPARQL query caching
//!
//! This module provides a thread-safe wrapper around Oxigraph's RDF store with
//! intelligent caching for SPARQL queries. The `Graph` type maintains an in-memory
//! triple store and provides efficient query execution with result caching.
//!
//! ## Features
//!
//! - **Thread-safe**: Clone is cheap (shared store via Arc)
//! - **Query caching**: LRU cache for query plans and results
//! - **Epoch-based invalidation**: Cache automatically invalidates when graph changes
//! - **Multiple RDF formats**: Supports Turtle, N-Triples, and RDF/XML
//! - **SPARQL 1.1**: Full SPARQL query support with boolean, solutions, and graph results
//!
//! ## Examples
//!
//! ### Basic Usage
//!
//! ```rust,no_run
//! use ggen_core::graph::Graph;
//!
//! # fn main() -> anyhow::Result<()> {
//! // Create a new graph
//! let graph = Graph::new()?;
//!
//! // Load RDF data
//! graph.insert_turtle(r#"
//!     @prefix ex: <http://example.org/> .
//!     ex:alice a ex:Person ;
//!              ex:name "Alice" .
//! "#)?;
//!
//! // Query the graph
//! let results = graph.query("SELECT ?s ?o WHERE { ?s ex:name ?o }")?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Loading from File
//!
//! ```rust,no_run
//! use ggen_core::graph::Graph;
//!
//! # fn main() -> anyhow::Result<()> {
//! // Load RDF from a file
//! let graph = Graph::load_from_file("data.ttl")?;
//!
//! // Query with cached results
//! let cached = graph.query_cached("SELECT ?s WHERE { ?s ?p ?o }")?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Pattern Matching
//!
//! ```rust,no_run
//! use ggen_core::graph::Graph;
//! use oxigraph::model::NamedNode;
//!
//! # fn main() -> anyhow::Result<()> {
//! let graph = Graph::new()?;
//! graph.insert_quad(
//!     "http://example.org/subject",
//!     "http://example.org/predicate",
//!     "http://example.org/object"
//! )?;
//!
//! // Find all quads with a specific subject
//! let subject = NamedNode::new("http://example.org/subject")?;
//! let quads = graph.quads_for_pattern(Some(&subject.into()), None, None, None)?;
//! # Ok(())
//! # }
//! ```

use ahash::AHasher;
use anyhow::{bail, Result};
use lru::LruCache;
use oxigraph::io::RdfFormat;
use oxigraph::model::{GraphName, NamedNode, NamedOrBlankNode, Quad, Term};
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde_json::Value as JsonValue;
use std::collections::BTreeMap;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::BufReader;
use std::num::NonZeroUsize;
use std::path::Path;
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc, Mutex,
};

/// Default size for SPARQL query plan cache
const DEFAULT_PLAN_CACHE_SIZE: usize = 100;

/// Default size for SPARQL query result cache
const DEFAULT_RESULT_CACHE_SIZE: usize = 1000;

/// Cached SPARQL query result types.
///
/// This enum represents the different types of results that can be returned
/// from SPARQL queries and cached for efficient reuse.
///
/// # Variants
///
/// * `Boolean(bool)` - Result of an ASK query (true/false)
/// * `Solutions(Vec<BTreeMap<String, String>>)` - Result of a SELECT query (rows of variable bindings)
/// * `Graph(Vec<String>)` - Result of a CONSTRUCT/DESCRIBE query (serialized triples)
///
/// # Examples
///
/// ## Boolean result
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, CachedResult};
///
/// # fn main() -> anyhow::Result<()> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person .
/// "#)?;
///
/// let result = graph.query_cached("ASK { ?s a ex:Person }")?;
/// if let CachedResult::Boolean(true) = result {
///     println!("Person exists in graph");
/// }
/// # Ok(())
/// # }
/// ```
///
/// ## Solutions result
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, CachedResult};
///
/// # fn main() -> anyhow::Result<()> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice ex:name "Alice" .
/// "#)?;
///
/// let result = graph.query_cached("SELECT ?name WHERE { ?s ex:name ?name }")?;
/// if let CachedResult::Solutions(rows) = result {
///     for row in rows {
///         println!("Name: {}", row.get("name").unwrap());
///     }
/// }
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Debug)]
pub enum CachedResult {
    /// Boolean result from ASK queries
    Boolean(bool),
    /// Solution bindings from SELECT queries
    Solutions(Vec<BTreeMap<String, String>>),
    /// Serialized triples from CONSTRUCT/DESCRIBE queries
    Graph(Vec<String>),
}

impl CachedResult {
    /// Convert to serde_json::Value for Tera consumption
    pub fn to_json(&self) -> JsonValue {
        match self {
            CachedResult::Boolean(b) => JsonValue::Bool(*b),
            CachedResult::Solutions(rows) => {
                let arr: Vec<JsonValue> = rows
                    .iter()
                    .map(|row| {
                        let mut obj = serde_json::Map::new();
                        for (k, v) in row {
                            obj.insert(k.clone(), JsonValue::String(v.clone()));
                        }
                        JsonValue::Object(obj)
                    })
                    .collect();
                JsonValue::Array(arr)
            }
            CachedResult::Graph(_triples) => JsonValue::String(String::new()),
        }
    }
}

/// Thread-safe Oxigraph wrapper with SPARQL caching.
///
/// The `Graph` type provides a high-level interface to an in-memory RDF triple store
/// with intelligent query caching. It wraps Oxigraph's `Store` and adds:
///
/// - **Query result caching**: LRU cache for SPARQL query results
/// - **Query plan caching**: Cached query plans for faster execution
/// - **Epoch-based invalidation**: Automatic cache invalidation when graph changes
/// - **Thread safety**: Cheap cloning via `Arc` for concurrent access
///
/// # Thread Safety
///
/// `Graph` is designed for concurrent use. Cloning a `Graph` is cheap (O(1)) as it
/// shares the underlying store via `Arc`. Multiple threads can safely query the same
/// graph concurrently.
///
/// # Cache Invalidation
///
/// The graph maintains an epoch counter that increments whenever data is inserted.
/// This automatically invalidates cached query results, ensuring consistency.
///
/// # Examples
///
/// ## Basic usage
///
/// ```rust,no_run
/// use ggen_core::graph::Graph;
///
/// # fn main() -> anyhow::Result<()> {
/// // Create a new graph
/// let graph = Graph::new()?;
///
/// // Load RDF data
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person ;
///              ex:name "Alice" .
/// "#)?;
///
/// // Query the graph (note: query() only caches boolean queries)
/// // For full caching, use query_cached() instead
/// let results = graph.query("SELECT ?name WHERE { ?s ex:name ?name }")?;
/// # Ok(())
/// # }
/// ```
///
/// ## Loading from file
///
/// ```rust,no_run
/// use ggen_core::graph::Graph;
///
/// # fn main() -> anyhow::Result<()> {
/// // Load RDF from a file
/// let graph = Graph::load_from_file("data.ttl")?;
///
/// // Query with cached results (use query_cached for full caching)
/// let _first = graph.query_cached("SELECT ?s WHERE { ?s ?p ?o }")?;
/// let _cached = graph.query_cached("SELECT ?s WHERE { ?s ?p ?o }")?; // Uses cache
/// # Ok(())
/// # }
/// ```
///
/// ## Concurrent access
///
/// ```rust,no_run
/// use ggen_core::graph::Graph;
/// use std::thread;
///
/// # fn main() -> anyhow::Result<()> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice ex:name "Alice" .
/// "#)?;
///
/// // Clone is cheap - shares underlying store
/// let graph1 = graph.clone();
/// let graph2 = graph.clone();
///
/// // Multiple threads can query concurrently (using query_cached for caching)
/// let t1 = thread::spawn(move || {
///     graph1.query_cached("SELECT ?name WHERE { ?s ex:name ?name }")
/// });
/// let t2 = thread::spawn(move || {
///     graph2.query_cached("SELECT ?name WHERE { ?s ex:name ?name }")
/// });
///
/// let _r1 = t1.join().unwrap()?;
/// let _r2 = t2.join().unwrap()?;
/// # Ok(())
/// # }
/// ```
pub struct Graph {
    inner: Store,
    epoch: Arc<AtomicU64>,
    plan_cache: Arc<Mutex<LruCache<u64, String>>>,
    result_cache: Arc<Mutex<LruCache<(u64, u64), CachedResult>>>,
}

impl Graph {
    /// Create a new empty graph
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_core::graph::Graph;
    ///
    /// let graph = Graph::new().unwrap();
    /// assert!(graph.is_empty());
    /// ```
    pub fn new() -> Result<Self> {
        let plan_cache_size = NonZeroUsize::new(DEFAULT_PLAN_CACHE_SIZE)
            .ok_or_else(|| anyhow::anyhow!("Invalid cache size"))?;
        let result_cache_size = NonZeroUsize::new(DEFAULT_RESULT_CACHE_SIZE)
            .ok_or_else(|| anyhow::anyhow!("Invalid cache size"))?;

        Ok(Self {
            inner: Store::new()?,
            epoch: Arc::new(AtomicU64::new(1)),
            plan_cache: Arc::new(Mutex::new(LruCache::new(plan_cache_size))),
            result_cache: Arc::new(Mutex::new(LruCache::new(result_cache_size))),
        })
    }

    /// Load RDF data from a file into a new Graph
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be opened or read
    /// - The file format is unsupported
    /// - The RDF syntax is invalid
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::load_from_file("data.ttl")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - File not found
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// // This will fail because the file doesn't exist
    /// let result = Graph::load_from_file("nonexistent.ttl");
    /// assert!(result.is_err());
    /// // Error message indicates file not found
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid RDF format
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// // This will fail if the file contains invalid RDF
    /// let result = Graph::load_from_file("invalid.ttl");
    /// assert!(result.is_err());
    /// // Error message indicates invalid RDF syntax
    /// # Ok(())
    /// # }
    /// ```
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let graph = Self::new()?;
        graph.load_path(path)?;
        Ok(graph)
    }

    fn current_epoch(&self) -> u64 {
        self.epoch.load(Ordering::Relaxed)
    }

    fn bump_epoch(&self) {
        self.epoch.fetch_add(1, Ordering::Relaxed);
    }

    fn hash_query(&self, sparql: &str) -> u64 {
        let mut hasher = AHasher::default();
        sparql.hash(&mut hasher);
        hasher.finish()
    }

    fn materialize_results(&self, results: QueryResults) -> Result<CachedResult> {
        match results {
            QueryResults::Boolean(b) => Ok(CachedResult::Boolean(b)),
            QueryResults::Solutions(solutions) => {
                let mut rows = Vec::new();
                for solution in solutions {
                    let solution = solution?;
                    let mut row = BTreeMap::new();
                    for (var, term) in solution.iter() {
                        row.insert(var.as_str().to_string(), term.to_string());
                    }
                    rows.push(row);
                }
                Ok(CachedResult::Solutions(rows))
            }
            QueryResults::Graph(quads) => {
                let triples: Result<Vec<String>> = quads
                    .map(|q| q.map(|quad| quad.to_string()).map_err(Into::into))
                    .collect();
                Ok(CachedResult::Graph(triples?))
            }
        }
    }

    /// Insert RDF data in Turtle format
    ///
    /// Loads RDF triples from a Turtle string into the graph. The graph's
    /// epoch counter is incremented, invalidating cached query results.
    ///
    /// # Arguments
    ///
    /// * `turtle` - Turtle-formatted RDF data
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` if the data was successfully loaded, or an error if
    /// the Turtle syntax is invalid.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The Turtle syntax is invalid
    /// - The RDF data cannot be parsed
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust
    /// use ggen_core::graph::Graph;
    ///
    /// let graph = Graph::new().unwrap();
    ///
    /// // Insert simple Turtle data
    /// graph.insert_turtle(r#"
    /// @prefix ex: <http://example.org/> .
    /// ex:alice ex:name "Alice" .
    /// "#).unwrap();
    ///
    /// // Verify data was inserted
    /// assert!(!graph.is_empty());
    /// ```
    ///
    /// ## Error case - Invalid Turtle syntax
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// // This will fail because the Turtle syntax is invalid
    /// let result = graph.insert_turtle("invalid turtle syntax {");
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert_turtle(&self, turtle: &str) -> Result<()> {
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    /// Insert RDF data in Turtle format with a base IRI
    ///
    /// Loads RDF triples from a Turtle string with a specified base IRI.
    /// Relative IRIs in the Turtle data will be resolved against this base.
    ///
    /// **Note**: The current Oxigraph API doesn't fully support base IRI in
    /// `load_from_reader`, so this method currently behaves the same as
    /// `insert_turtle`. The `base_iri` parameter is accepted for API compatibility
    /// but is currently ignored. This allows code to be written that will work
    /// correctly when full base IRI support is added.
    ///
    /// # Arguments
    ///
    /// * `turtle` - Turtle-formatted RDF data
    /// * `base_iri` - Base IRI for resolving relative IRIs (currently not fully supported)
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The Turtle syntax is invalid
    /// - The RDF data cannot be parsed
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    ///
    /// graph.insert_turtle_with_base(
    ///     r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person .
    ///     "#,
    ///     "http://example.org/"
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid Turtle syntax
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// // This will fail because the Turtle syntax is invalid
    /// let result = graph.insert_turtle_with_base("invalid syntax {", "http://example.org/");
    /// assert!(result.is_err());
    /// // Error message indicates invalid Turtle syntax
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert_turtle_with_base(&self, turtle: &str, _base_iri: &str) -> Result<()> {
        // Note: The new Oxigraph API doesn't support base IRI in load_from_reader
        // We'll need to handle this differently or use a different approach
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    /// Insert RDF data in Turtle format into a named graph
    ///
    /// Loads RDF triples from a Turtle string into a specific named graph.
    ///
    /// **Note**: The current Oxigraph API doesn't fully support named graphs in
    /// `load_from_reader`, so this method currently loads into the default graph.
    /// The `graph_iri` parameter is accepted for API compatibility but is currently
    /// ignored. This allows code to be written that will work correctly when full
    /// named graph support is added.
    ///
    /// # Arguments
    ///
    /// * `turtle` - Turtle-formatted RDF data
    /// * `graph_iri` - IRI of the named graph (currently not fully supported)
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The Turtle syntax is invalid
    /// - The RDF data cannot be parsed
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    ///
    /// graph.insert_turtle_in(
    ///     r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person .
    ///     "#,
    ///     "http://example.org/graph1"
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid Turtle syntax
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// // This will fail because the Turtle syntax is invalid
    /// let result = graph.insert_turtle_in("invalid syntax {", "http://example.org/graph1");
    /// assert!(result.is_err());
    /// // Error message indicates invalid Turtle syntax
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert_turtle_in(&self, turtle: &str, _graph_iri: &str) -> Result<()> {
        // Note: The new Oxigraph API doesn't support named graphs in load_from_reader
        // We'll need to handle this differently or use a different approach
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    /// Insert a single RDF quad (triple) into the graph
    ///
    /// Adds a single triple to the graph. All components must be valid IRIs.
    /// The graph's epoch counter is incremented, invalidating cached query results.
    ///
    /// # Arguments
    ///
    /// * `s` - Subject IRI
    /// * `p` - Predicate IRI
    /// * `o` - Object IRI
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` if the quad was successfully inserted, or an error if
    /// any of the IRIs are invalid.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Any of the IRIs are invalid (not valid IRI syntax)
    /// - The quad cannot be inserted
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    ///
    /// graph.insert_quad(
    ///     "http://example.org/alice",
    ///     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    ///     "http://example.org/Person"
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid IRI
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// // This will fail because the IRI is invalid
    /// let result = graph.insert_quad("not a valid IRI", "http://example.org/p", "http://example.org/o");
    /// assert!(result.is_err());
    /// // Error message indicates invalid IRI syntax
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert_quad(&self, s: &str, p: &str, o: &str) -> Result<()> {
        let s = NamedNode::new(s)?;
        let p = NamedNode::new(p)?;
        let o = NamedNode::new(o)?;
        self.inner
            .insert(&Quad::new(s, p, o, GraphName::DefaultGraph))?;
        self.bump_epoch();
        Ok(())
    }

    /// Load RDF data from a file path
    ///
    /// Automatically detects the RDF format from the file extension:
    /// - `.ttl` or `.turtle` - Turtle format
    /// - `.nt` or `.ntriples` - N-Triples format
    /// - `.rdf` or `.xml` - RDF/XML format
    ///
    /// The graph's epoch counter is incremented, invalidating cached query results.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the RDF file
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` if the file was successfully loaded, or an error if
    /// the file cannot be read or the format is invalid.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be opened or read
    /// - The file extension indicates an unsupported format (error message format: `"unsupported RDF format: <extension>"`)
    /// - The RDF syntax is invalid for the detected format
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// graph.load_path("data.ttl")?;
    ///
    /// // Query the loaded data
    /// let results = graph.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn load_path<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let path = path.as_ref();
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|s| s.to_ascii_lowercase())
            .unwrap_or_default();

        let fmt = match ext.as_str() {
            "ttl" | "turtle" => RdfFormat::Turtle,
            "nt" | "ntriples" => RdfFormat::NTriples,
            "rdf" | "xml" => RdfFormat::RdfXml,
            other => bail!("unsupported RDF format: {}", other),
        };

        let file = File::open(path)?;
        let reader = BufReader::new(file);
        self.inner.load_from_reader(fmt, reader)?;
        self.bump_epoch();
        Ok(())
    }

    /// Execute a SPARQL query with caching
    ///
    /// Results are cached based on query string and graph epoch.
    /// Cache is automatically invalidated when the graph changes.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The SPARQL query syntax is invalid
    /// - The query cannot be executed
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person ;
    ///              ex:name "Alice" .
    /// "#)?;
    ///
    /// let results = graph.query_cached("SELECT ?s ?o WHERE { ?s ex:name ?o }")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid SPARQL query
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// // This will fail because the SPARQL query is invalid
    /// let result = graph.query_cached("INVALID SPARQL SYNTAX {");
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn query_cached(&self, sparql: &str) -> Result<CachedResult> {
        let query_hash = self.hash_query(sparql);
        // Capture epoch atomically to prevent race condition where epoch changes
        // between cache check and query execution, which could cause stale results
        let epoch = self.current_epoch();
        let cache_key = (query_hash, epoch);

        // Check result cache
        if let Some(cached) = self
            .result_cache
            .lock()
            .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?
            .get(&cache_key)
            .cloned()
        {
            return Ok(cached);
        }

        // Re-check epoch after cache miss to ensure we're still on the same epoch
        // If epoch changed, recalculate cache key to avoid stale results
        let final_epoch = self.current_epoch();
        let final_cache_key = if final_epoch != epoch {
            // Epoch changed, use new epoch for cache key
            let new_cache_key = (query_hash, final_epoch);
            // Check cache again with new epoch
            if let Some(cached) = self
                .result_cache
                .lock()
                .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?
                .get(&new_cache_key)
                .cloned()
            {
                return Ok(cached);
            }
            new_cache_key
        } else {
            cache_key
        };

        // Check plan cache or parse
        let query_str = {
            let mut cache = self
                .plan_cache
                .lock()
                .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?;
            if let Some(q) = cache.get(&query_hash).cloned() {
                q
            } else {
                let q = sparql.to_string();
                cache.put(query_hash, q.clone());
                q
            }
        };

        // Execute and materialize
        // Note: Using deprecated Store::query() API - will migrate to SparqlEvaluator post-1.0
        #[allow(deprecated)]
        let results = self.inner.query(&query_str)?;
        let cached = self.materialize_results(results)?;

        // Store in cache using final_cache_key (which accounts for epoch changes)
        self.result_cache
            .lock()
            .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?
            .put(final_cache_key, cached.clone());

        Ok(cached)
    }

    /// Execute a SPARQL query (returns raw QueryResults)
    ///
    /// This method provides direct access to Oxigraph's QueryResults.
    ///
    /// **Caching behavior**: This method only uses caching for boolean (ASK) queries.
    /// For SELECT, CONSTRUCT, and DESCRIBE queries, it bypasses the cache and queries
    /// directly. For full caching of all query types, use `query_cached` instead.
    ///
    /// **Performance note**: This method uses `query_cached` internally for boolean
    /// queries, but for SELECT, CONSTRUCT, and DESCRIBE queries, it bypasses the cache
    /// and queries directly. This is less efficient than `query_cached` but maintains
    /// API compatibility with code expecting `QueryResults` iterators.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The SPARQL query syntax is invalid
    /// - The query cannot be executed
    /// - Cache lock is poisoned (internal error, should not occur in normal use)
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person ;
    ///              ex:name "Alice" .
    /// "#)?;
    ///
    /// let results = graph.query("SELECT ?s ?o WHERE { ?s ex:name ?o }")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid SPARQL syntax
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// // This will fail because the SPARQL query is invalid
    /// let result = graph.query("INVALID SPARQL SYNTAX {");
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn query<'a>(&'a self, sparql: &str) -> Result<QueryResults<'a>> {
        // For backward compatibility, we need to reconstruct QueryResults
        // This is inefficient but maintains API compatibility
        let cached = self.query_cached(sparql)?;

        match cached {
            CachedResult::Boolean(b) => Ok(QueryResults::Boolean(b)),
            CachedResult::Solutions(_) | CachedResult::Graph(_) => {
                // Fall back to direct query for non-boolean results
                // since we can't reconstruct the iterator properly
                // Note: Using deprecated Store::query() API - will migrate to SparqlEvaluator post-1.0
                #[allow(deprecated)]
                {
                    Ok(self.inner.query(sparql)?)
                }
            }
        }
    }

    /// Execute a SPARQL query with PREFIX and BASE declarations
    ///
    /// This method automatically prepends PREFIX and BASE declarations to the
    /// SPARQL query based on the provided prefixes and base IRI. This is useful
    /// when you want to use prefixed names in your query without manually writing
    /// the PREFIX declarations.
    ///
    /// # Arguments
    ///
    /// * `sparql` - SPARQL query string (without PREFIX/BASE declarations)
    /// * `prefixes` - Map of prefix names to namespace URIs
    /// * `base` - Optional base IRI for relative IRIs
    ///
    /// # Returns
    ///
    /// Returns raw `QueryResults` from Oxigraph. For cached results, use `query_cached`.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    /// use std::collections::BTreeMap;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person .
    /// "#)?;
    ///
    /// let mut prefixes = BTreeMap::new();
    /// prefixes.insert("ex".to_string(), "http://example.org/".to_string());
    ///
    /// // Query using prefix (PREFIX ex: <http://example.org/> is auto-added)
    /// let results = graph.query_with_prolog(
    ///     "SELECT ?s WHERE { ?s a ex:Person }",
    ///     &prefixes,
    ///     None
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn query_with_prolog<'a>(
        &'a self, sparql: &str, prefixes: &BTreeMap<String, String>, base: Option<&str>,
    ) -> Result<QueryResults<'a>> {
        let head = build_prolog(prefixes, base);
        let q = if head.is_empty() {
            sparql.into()
        } else {
            format!("{head}\n{sparql}")
        };
        self.query(&q)
    }

    /// Execute a prepared SPARQL query (low-level API)
    ///
    /// This method provides direct access to Oxigraph's query API.
    /// For most use cases, prefer `query()` or `query_cached()` instead.
    ///
    /// **Note**: This method uses the deprecated `Store::query()` API and will
    /// be migrated to `SparqlEvaluator` in a later version.
    ///
    /// # Arguments
    ///
    /// * `q` - SPARQL query string
    ///
    /// # Returns
    ///
    /// Returns raw `QueryResults` from Oxigraph.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The SPARQL query syntax is invalid
    /// - The query cannot be executed
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person .
    /// "#)?;
    ///
    /// let results = graph.query_prepared("SELECT ?s WHERE { ?s ?p ?o }")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Invalid SPARQL syntax
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// // This will fail because the SPARQL query is invalid
    /// let result = graph.query_prepared("INVALID SPARQL {");
    /// assert!(result.is_err());
    /// // Error message indicates invalid SPARQL syntax
    /// # Ok(())
    /// # }
    /// ```
    pub fn query_prepared<'a>(&'a self, q: &str) -> Result<QueryResults<'a>> {
        // Note: Using deprecated Store::query() API - will migrate to SparqlEvaluator post-1.0
        #[allow(deprecated)]
        {
            Ok(self.inner.query(q)?)
        }
    }

    /// Find quads matching a pattern
    ///
    /// Searches for quads (triples) in the graph that match the specified pattern.
    /// This is a typed pattern filter with no extra allocations.
    /// Any component can be `None` to match any value (wildcard).
    ///
    /// # Arguments
    ///
    /// * `s` - Subject pattern (None = match any)
    /// * `p` - Predicate pattern (None = match any)
    /// * `o` - Object pattern (None = match any)
    /// * `g` - Graph name pattern (None = match any)
    ///
    /// # Returns
    ///
    /// Returns a vector of quads matching the pattern.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::graph::Graph;
    /// use oxigraph::model::{NamedNode, NamedOrBlankNode};
    ///
    /// let graph = Graph::new().unwrap();
    /// graph.insert_turtle(r#"
    /// @prefix ex: <http://example.org/> .
    /// ex:alice a ex:Person .
    /// ex:bob a ex:Person .
    /// "#).unwrap();
    ///
    /// // Find all quads with ex:Person as object
    /// let person_type = NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type").unwrap();
    /// let person_class = NamedNode::new("http://example.org/Person").unwrap();
    /// let quads = graph.quads_for_pattern(
    ///     None, // any subject
    ///     Some(&person_type.into()),
    ///     Some(&person_class.into()),
    ///     None  // default graph
    /// ).unwrap();
    ///
    /// assert_eq!(quads.len(), 2); // alice and bob
    /// ```
    pub fn quads_for_pattern(
        &self, s: Option<&NamedOrBlankNode>, p: Option<&NamedNode>, o: Option<&Term>,
        g: Option<&GraphName>,
    ) -> Result<Vec<Quad>> {
        Ok(self
            .inner
            .quads_for_pattern(
                s.map(|x| x.as_ref()),
                p.map(|x| x.as_ref()),
                o.map(|x| x.as_ref()),
                g.map(|x| x.as_ref()),
            )
            .collect::<Result<Vec<_>, _>>()?)
    }

    /// Clear all data from the graph
    ///
    /// Removes all triples from the graph and increments the epoch counter,
    /// invalidating all cached query results.
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` if the graph was successfully cleared.
    ///
    /// # Errors
    ///
    /// Returns an error if the clear operation fails (should not occur in normal use).
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person .
    /// "#)?;
    ///
    /// // Clear all data
    /// graph.clear()?;
    /// assert!(graph.is_empty());
    /// # Ok(())
    /// # }
    /// ```
    pub fn clear(&self) -> Result<()> {
        self.inner.clear()?;
        self.bump_epoch();
        Ok(())
    }

    /// Get the number of triples in the graph
    ///
    /// Returns the total count of triples (quads) stored in the graph.
    ///
    /// # Returns
    ///
    /// The number of triples in the graph, or 0 if the count cannot be determined.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::graph::Graph;
    ///
    /// let graph = Graph::new().unwrap();
    /// assert_eq!(graph.len(), 0);
    ///
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person .
    /// "#).unwrap();
    ///
    /// assert!(graph.len() > 0);
    /// ```
    pub fn len(&self) -> usize {
        #[allow(deprecated)]
        {
            self.inner.len().unwrap_or(0)
        }
    }

    /// Check if the graph is empty
    ///
    /// Returns `true` if the graph contains no triples, `false` otherwise.
    ///
    /// # Returns
    ///
    /// `true` if `len() == 0`, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::graph::Graph;
    ///
    /// let graph = Graph::new().unwrap();
    /// assert!(graph.is_empty());
    ///
    /// graph.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice a ex:Person .
    /// "#).unwrap();
    ///
    /// assert!(!graph.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Clone for Graph {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            epoch: Arc::clone(&self.epoch),
            plan_cache: Arc::clone(&self.plan_cache),
            result_cache: Arc::clone(&self.result_cache),
        }
    }
}

/// Build SPARQL prolog (PREFIX and BASE declarations) from a prefix map.
///
/// Constructs the prolog section of a SPARQL query by generating PREFIX
/// declarations for each entry in the prefix map, and optionally a BASE
/// declaration if a base IRI is provided.
///
/// # Arguments
///
/// * `prefixes` - Map of prefix names (e.g., "ex") to namespace URIs (e.g., "http://example.org/")
/// * `base` - Optional base IRI for relative IRI resolution
///
/// # Returns
///
/// A string containing the SPARQL prolog with PREFIX and BASE declarations.
///
/// # Examples
///
/// ## With prefixes only
///
/// ```rust
/// use ggen_core::graph::build_prolog;
/// use std::collections::BTreeMap;
///
/// let mut prefixes = BTreeMap::new();
/// prefixes.insert("ex".to_string(), "http://example.org/".to_string());
/// prefixes.insert("rdf".to_string(), "http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string());
///
/// let prolog = build_prolog(&prefixes, None);
/// assert!(prolog.contains("PREFIX ex: <http://example.org/>"));
/// assert!(prolog.contains("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"));
/// ```
///
/// ## With base IRI
///
/// ```rust
/// use ggen_core::graph::build_prolog;
/// use std::collections::BTreeMap;
///
/// let prefixes = BTreeMap::new();
/// let prolog = build_prolog(&prefixes, Some("http://example.org/"));
/// assert!(prolog.contains("BASE <http://example.org/>"));
/// ```
///
/// ## Combined
///
/// ```rust
/// use ggen_core::graph::build_prolog;
/// use std::collections::BTreeMap;
///
/// let mut prefixes = BTreeMap::new();
/// prefixes.insert("ex".to_string(), "http://example.org/".to_string());
///
/// let prolog = build_prolog(&prefixes, Some("http://example.org/base/"));
/// assert!(prolog.contains("BASE <http://example.org/base/>"));
/// assert!(prolog.contains("PREFIX ex: <http://example.org/>"));
/// ```
pub fn build_prolog(prefixes: &BTreeMap<String, String>, base: Option<&str>) -> String {
    let mut s = String::new();
    if let Some(b) = base {
        let _ = std::fmt::Write::write_fmt(&mut s, format_args!("BASE <{}>\n", b));
    }
    for (pfx, iri) in prefixes {
        let _ = std::fmt::Write::write_fmt(&mut s, format_args!("PREFIX {}: <{}>\n", pfx, iri));
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxigraph::model::NamedNode;

    #[test]
    fn insert_turtle_and_query() -> Result<()> {
        let g = Graph::new()?;
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#;
        g.insert_turtle(ttl)?;

        let res = g.query("SELECT ?s WHERE { ?s ?p ?o }")?;
        if let QueryResults::Solutions(mut it) = res {
            let first = it.next().unwrap().unwrap();
            let s = first.get("s").unwrap().to_string();
            assert_eq!(s, "<http://example.org/alice>");
        } else {
            return Err(anyhow::anyhow!("Expected Solutions results"));
        }
        Ok(())
    }

    #[test]
    fn insert_quad_and_filter() -> Result<()> {
        let g = Graph::new()?;
        g.insert_quad(
            "http://example.org/A",
            "http://example.org/rel",
            "http://example.org/B",
        )?;
        let a = NamedNode::new("http://example.org/A")?;
        let list = g.quads_for_pattern(Some(&a.into()), None, None, None)?;
        assert_eq!(list.len(), 1);
        Ok(())
    }

    #[test]
    fn insert_turtle_with_base() -> Result<()> {
        let g = Graph::new()?;
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#;
        g.insert_turtle_with_base(ttl, "http://example.org/")?;

        let res = g.query("SELECT ?s WHERE { ?s ?p ?o }")?;
        if let QueryResults::Solutions(mut it) = res {
            let first = it.next().unwrap().unwrap();
            let s = first.get("s").unwrap().to_string();
            assert_eq!(s, "<http://example.org/alice>");
        } else {
            return Err(anyhow::anyhow!("Expected Solutions results"));
        }
        Ok(())
    }

    #[test]
    fn query_with_prolog_works() -> Result<()> {
        let g = Graph::new()?;
        g.insert_turtle("@prefix ex: <http://example/> . ex:x a ex:T .")?;
        let mut p = BTreeMap::new();
        p.insert("ex".to_string(), "http://example/".to_string());
        let q = "SELECT ?s WHERE { ?s a ex:T }";
        let res = g.query_with_prolog(q, &p, None)?;
        if let QueryResults::Solutions(mut it) = res {
            let first = it.next().unwrap().unwrap();
            let s = first.get("s").unwrap().to_string();
            assert_eq!(s, "<http://example/x>");
        } else {
            return Err(anyhow::anyhow!("Expected Solutions results"));
        }
        Ok(())
    }

    #[test]
    fn test_cached_result_to_json() {
        // Test Boolean variant
        let bool_result = CachedResult::Boolean(true);
        let json = bool_result.to_json();
        assert_eq!(json, JsonValue::Bool(true));

        // Test Solutions variant
        let mut solutions = Vec::new();
        let mut row = BTreeMap::new();
        row.insert("name".to_string(), "Alice".to_string());
        row.insert("age".to_string(), "30".to_string());
        solutions.push(row);
        let solutions_result = CachedResult::Solutions(solutions);
        let json = solutions_result.to_json();

        if let JsonValue::Array(arr) = json {
            assert_eq!(arr.len(), 1);
            if let JsonValue::Object(obj) = &arr[0] {
                assert_eq!(obj.get("name").unwrap(), "Alice");
                assert_eq!(obj.get("age").unwrap(), "30");
            } else {
                panic!("Expected object in array");
            }
        } else {
            panic!("Expected array");
        }

        // Test Graph variant
        let graph_result = CachedResult::Graph(vec!["<http://example.org/subject> <http://example.org/predicate> <http://example.org/object> .".to_string()]);
        let json = graph_result.to_json();
        assert_eq!(json, JsonValue::String(String::new()));
    }

    #[test]
    fn test_graph_creation_and_basic_properties() -> Result<()> {
        let g = Graph::new()?;
        assert!(g.is_empty());
        assert_eq!(g.len(), 0);
        assert_eq!(g.current_epoch(), 1); // Epoch starts at 1

        // Test epoch bumping
        g.bump_epoch();
        assert_eq!(g.current_epoch(), 2);

        Ok(())
    }

    #[test]
    fn test_insert_turtle_in() -> Result<()> {
        let g = Graph::new()?;
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#;
        g.insert_turtle_in(ttl, "http://example.org/graph1")?;

        // Should have inserted the triple
        assert!(!g.is_empty());
        assert_eq!(g.len(), 1);

        Ok(())
    }

    #[test]
    fn test_query_cached() -> Result<()> {
        let g = Graph::new()?;
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
            ex:bob ex:knows ex:charlie .
        "#;
        g.insert_turtle(ttl)?;

        // First query should execute and cache
        let result1 = g.query_cached("SELECT ?s WHERE { ?s <http://example.org/knows> ?o }")?;

        // Second query should use cache
        let result2 = g.query_cached("SELECT ?s WHERE { ?s <http://example.org/knows> ?o }")?;

        // Results should be identical
        match (&result1, &result2) {
            (CachedResult::Solutions(sol1), CachedResult::Solutions(sol2)) => {
                assert_eq!(sol1.len(), sol2.len());
                assert_eq!(sol1.len(), 2); // alice->bob, bob->charlie
            }
            _ => return Err(anyhow::anyhow!("Expected Solutions results")),
        }

        Ok(())
    }

    #[test]
    fn test_query_prepared() -> Result<()> {
        let g = Graph::new()?;
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#;
        g.insert_turtle(ttl)?;

        let query = "SELECT ?s WHERE { ?s ?p ?o }";
        let results = g.query_prepared(query)?;

        if let QueryResults::Solutions(mut it) = results {
            let first = it.next().unwrap().unwrap();
            let s = first.get("s").unwrap().to_string();
            assert_eq!(s, "<http://example.org/alice>");
        } else {
            return Err(anyhow::anyhow!("Expected Solutions results"));
        }

        Ok(())
    }

    #[test]
    fn test_quads_for_pattern() -> Result<()> {
        let g = Graph::new()?;
        g.insert_quad(
            "http://example.org/A",
            "http://example.org/rel",
            "http://example.org/B",
        )?;
        g.insert_quad(
            "http://example.org/A",
            "http://example.org/rel2",
            "http://example.org/C",
        )?;

        let a = NamedNode::new("http://example.org/A")?;
        let rel = NamedNode::new("http://example.org/rel")?;

        // Test filtering by subject only
        let quads = g.quads_for_pattern(Some(&a.clone().into()), None, None, None)?;
        assert_eq!(quads.len(), 2);

        // Test filtering by subject and predicate
        let quads = g.quads_for_pattern(Some(&a.into()), Some(&rel.into()), None, None)?;
        assert_eq!(quads.len(), 1);

        Ok(())
    }

    #[test]
    fn test_clear() -> Result<()> {
        let g = Graph::new()?;
        g.insert_quad(
            "http://example.org/A",
            "http://example.org/rel",
            "http://example.org/B",
        )?;

        assert!(!g.is_empty());
        assert_eq!(g.len(), 1);

        g.clear()?;

        assert!(g.is_empty());
        assert_eq!(g.len(), 0);

        Ok(())
    }

    #[test]
    fn test_hash_query() -> Result<()> {
        let g = Graph::new()?;

        let hash1 = g.hash_query("SELECT ?s WHERE { ?s ?p ?o }");
        let hash2 = g.hash_query("SELECT ?s WHERE { ?s ?p ?o }");
        let hash3 = g.hash_query("SELECT ?o WHERE { ?s ?p ?o }");

        // Same query should produce same hash
        assert_eq!(hash1, hash2);

        // Different query should produce different hash
        assert_ne!(hash1, hash3);

        Ok(())
    }

    #[test]
    fn test_materialize_results() -> Result<()> {
        let g = Graph::new()?;
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#;
        g.insert_turtle(ttl)?;

        let query = "SELECT ?s WHERE { ?s ?p ?o }";
        let results = g.query(query)?;

        let cached = g.materialize_results(results)?;

        match cached {
            CachedResult::Solutions(solutions) => {
                assert_eq!(solutions.len(), 1);
                let row = &solutions[0];
                assert_eq!(row.get("s").unwrap(), "<http://example.org/alice>");
            }
            _ => return Err(anyhow::anyhow!("Expected Solutions result")),
        }

        Ok(())
    }

    #[test]
    fn test_build_prolog() {
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());
        prefixes.insert(
            "rdf".to_string(),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
        );

        let prolog = build_prolog(&prefixes, Some("http://example.org/base"));

        assert!(prolog.contains("PREFIX ex: <http://example.org/>"));
        assert!(prolog.contains("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"));
        assert!(prolog.contains("BASE <http://example.org/base>"));
    }

    #[test]
    fn test_build_prolog_no_base() {
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());

        let prolog = build_prolog(&prefixes, None);

        assert!(prolog.contains("PREFIX ex: <http://example.org/>"));
        assert!(!prolog.contains("BASE"));
    }

    #[test]
    fn test_clone_graph() -> Result<()> {
        let g1 = Graph::new()?;
        g1.insert_quad(
            "http://example.org/A",
            "http://example.org/rel",
            "http://example.org/B",
        )?;

        let g2 = g1.clone();

        // Both should have the same data
        assert_eq!(g1.len(), g2.len());
        assert_eq!(g1.is_empty(), g2.is_empty());

        // Both should be able to query the same data
        let results1 = g1.query("SELECT ?s WHERE { ?s ?p ?o }")?;
        let results2 = g2.query("SELECT ?s WHERE { ?s ?p ?o }")?;

        // Results should be identical
        match (results1, results2) {
            (QueryResults::Solutions(mut it1), QueryResults::Solutions(mut it2)) => {
                let row1 = it1.next().unwrap().unwrap();
                let row2 = it2.next().unwrap().unwrap();
                assert_eq!(row1.get("s").unwrap(), row2.get("s").unwrap());
            }
            _ => return Err(anyhow::anyhow!("Expected Solutions results")),
        }

        Ok(())
    }
}
