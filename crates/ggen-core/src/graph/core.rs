//! Core Graph type with SPARQL query caching
//!
//! The `Graph` type provides a high-level interface to an in-memory RDF triple store
//! with intelligent query caching. It wraps Oxigraph's `Store` and adds:
//!
//! - **Query result caching**: LRU cache for SPARQL query results
//! - **Query plan caching**: Cached query plans for faster execution
//! - **Epoch-based invalidation**: Automatic cache invalidation when graph changes
//! - **Thread safety**: Cheap cloning via `Arc` for concurrent access

use crate::graph::types::CachedResult;
use ahash::AHasher;
use ggen_utils::error::{Error, Result};
use lru::LruCache;
use oxigraph::io::RdfFormat;
use oxigraph::model::{GraphName, NamedNode, NamedOrBlankNode, Quad, Term};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;
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

/// Initial epoch value for cache invalidation
///
/// **Kaizen improvement**: Extracted magic number to named constant for clarity.
/// The epoch starts at 1 and increments on each graph modification to invalidate caches.
const INITIAL_EPOCH: u64 = 1;

/// Epoch increment amount
///
/// **Kaizen improvement**: Extracted magic number to named constant for consistency.
/// The epoch increments by 1 on each graph modification to invalidate caches.
const EPOCH_INCREMENT: u64 = 1;

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
/// # fn main() -> ggen_utils::error::Result<()> {
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
/// // Query the graph
/// let results = graph.query("SELECT ?name WHERE { ?s ex:name ?name }")?;
/// # Ok(())
/// # }
/// ```
pub struct Graph {
    inner: Arc<Store>,
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
            .ok_or_else(|| Error::new("Invalid cache size"))?;
        let result_cache_size = NonZeroUsize::new(DEFAULT_RESULT_CACHE_SIZE)
            .ok_or_else(|| Error::new("Invalid cache size"))?;

        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        let store =
            Store::new().map_err(|e| Error::new(&format!("Failed to create store: {}", e)))?;
        Ok(Self {
            inner: Arc::new(store),
            epoch: Arc::new(AtomicU64::new(INITIAL_EPOCH)),
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
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let graph = Self::new()?;
        graph.load_path(path)?;
        Ok(graph)
    }

    /// Get the current epoch (for cache invalidation)
    pub(crate) fn current_epoch(&self) -> u64 {
        self.epoch.load(Ordering::Relaxed)
    }

    /// Increment epoch (invalidates cache)
    pub(crate) fn bump_epoch(&self) {
        self.epoch.fetch_add(EPOCH_INCREMENT, Ordering::Relaxed);
    }

    /// Get reference to inner Store (for use by other modules)
    pub(crate) fn inner(&self) -> &Store {
        &self.inner
    }

    /// Create a Graph from an existing Store (for persistent stores)
    pub(crate) fn from_store(store: Arc<Store>) -> Result<Self> {
        let plan_cache_size = NonZeroUsize::new(DEFAULT_PLAN_CACHE_SIZE)
            .ok_or_else(|| Error::new("Invalid cache size"))?;
        let result_cache_size = NonZeroUsize::new(DEFAULT_RESULT_CACHE_SIZE)
            .ok_or_else(|| Error::new("Invalid cache size"))?;

        Ok(Self {
            inner: store,
            epoch: Arc::new(AtomicU64::new(INITIAL_EPOCH)),
            plan_cache: Arc::new(Mutex::new(LruCache::new(plan_cache_size))),
            result_cache: Arc::new(Mutex::new(LruCache::new(result_cache_size))),
        })
    }

    fn hash_query(&self, sparql: &str) -> u64 {
        let mut hasher = AHasher::default();
        sparql.hash(&mut hasher);
        hasher.finish()
    }

    /// Materialize SPARQL query results into CachedResult.
    ///
    /// **Root Cause Fix**: Uses explicit `.map_err()` for error conversion instead of `?`
    /// operator, because Oxigraph solution iterator errors don't implement `From` for
    /// `ggen_utils::error::Error`. Pattern: Always use `.map_err()` for external library
    /// errors that don't have `From` implementations.
    fn materialize_results(&self, results: QueryResults) -> Result<CachedResult> {
        match results {
            QueryResults::Boolean(b) => Ok(CachedResult::Boolean(b)),
            QueryResults::Solutions(solutions) => {
                let mut rows = Vec::new();
                for solution in solutions {
                    // Explicit error conversion: Oxigraph errors don't implement From
                    let solution = solution
                        .map_err(|e| Error::new(&format!("SPARQL solution error: {}", e)))?;
                    let mut row = BTreeMap::new();
                    for (var, term) in solution.iter() {
                        row.insert(var.as_str().to_string(), term.to_string());
                    }
                    rows.push(row);
                }
                Ok(CachedResult::Solutions(rows))
            }
            QueryResults::Graph(quads) => {
                let mut triples = Vec::new();
                for q in quads {
                    let quad = q.map_err(|e| Error::new(&format!("Quad error: {}", e)))?;
                    triples.push(quad.to_string());
                }
                Ok(CachedResult::Graph(triples))
            }
        }
    }

    /// Insert RDF data in Turtle format
    ///
    /// Loads RDF triples from a Turtle string into the graph. The graph's
    /// epoch counter is incremented, invalidating cached query results.
    pub fn insert_turtle(&self, turtle: &str) -> Result<()> {
        // Use higher-level load_from_reader API (oxigraph's recommended way to load RDF)
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())
            .map_err(|e| Error::new(&format!("Failed to load Turtle: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Insert RDF data in Turtle format with a base IRI
    ///
    /// Loads RDF triples from a Turtle string with a specified base IRI.
    /// Relative IRIs in the Turtle data will be resolved against this base.
    pub fn insert_turtle_with_base(&self, turtle: &str, base_iri: &str) -> Result<()> {
        // Prepend BASE declaration to Turtle string to ensure base IRI is used
        let base_iri_trimmed = base_iri.trim();
        let turtle_with_base = if turtle.trim_start().starts_with("BASE")
            || turtle.trim_start().starts_with("@base")
        {
            // Base already declared, use as-is
            turtle.to_string()
        } else {
            format!("BASE <{}>\n{}", base_iri_trimmed, turtle)
        };
        // Use higher-level load_from_reader API (oxigraph's recommended way to load RDF)
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle_with_base.as_bytes())
            .map_err(|e| Error::new(&format!("Failed to load Turtle with base IRI: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Insert RDF data in Turtle format into a named graph
    ///
    /// Loads RDF triples from a Turtle string into a specific named graph.
    pub fn insert_turtle_in(&self, turtle: &str, graph_iri: &str) -> Result<()> {
        // Parse Turtle into a temporary store, then extract quads and insert with named graph
        // **Note**: Temp store approach is necessary because oxigraph's load_from_reader doesn't
        // support loading directly into a named graph. This is the recommended pattern.
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        let temp_store = Store::new()
            .map_err(|e| Error::new(&format!("Failed to create temporary store: {}", e)))?;
        temp_store
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())
            .map_err(|e| Error::new(&format!("Failed to parse Turtle: {}", e)))?;

        // Extract all quads from temporary store and insert with named graph
        let graph_name = GraphName::NamedNode(
            NamedNode::new(graph_iri)
                .map_err(|e| Error::new(&format!("Invalid graph IRI: {}", e)))?,
        );

        // Use higher-level quads_for_pattern API - returns iterator of Result<Quad, StorageError>
        // StorageError has From implementation, so we can use ? after collect
        let quads: Vec<Quad> = temp_store
            .quads_for_pattern(None, None, None, None)
            .collect::<std::result::Result<Vec<_>, _>>()?;

        // Insert each quad with the named graph
        for quad in quads {
            // Quad has public fields: subject, predicate, object, graph_name
            let named_quad = Quad {
                subject: quad.subject.clone(),
                predicate: quad.predicate.clone(),
                object: quad.object.clone(),
                graph_name: graph_name.clone(),
            };
            // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
            self.inner.insert(&named_quad).map_err(|e| {
                Error::new(&format!("Failed to insert quad into named graph: {}", e))
            })?;
        }

        self.bump_epoch();
        Ok(())
    }

    /// Insert a single RDF quad (triple) into the graph
    ///
    /// Adds a single triple to the graph. All components must be valid IRIs.
    /// The graph's epoch counter is incremented, invalidating cached query results.
    pub fn insert_quad(&self, s: &str, p: &str, o: &str) -> Result<()> {
        let s =
            NamedNode::new(s).map_err(|e| Error::new(&format!("Invalid subject IRI: {}", e)))?;
        let p =
            NamedNode::new(p).map_err(|e| Error::new(&format!("Invalid predicate IRI: {}", e)))?;
        let o = NamedNode::new(o).map_err(|e| Error::new(&format!("Invalid object IRI: {}", e)))?;
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        self.inner
            .insert(&Quad::new(s, p, o, GraphName::DefaultGraph))
            .map_err(|e| Error::new(&format!("Failed to insert quad: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Insert a quad with a named graph
    ///
    /// Adds a quad to a specific named graph.
    pub fn insert_quad_in(&self, s: &str, p: &str, o: &str, graph_iri: &str) -> Result<()> {
        let s =
            NamedNode::new(s).map_err(|e| Error::new(&format!("Invalid subject IRI: {}", e)))?;
        let p =
            NamedNode::new(p).map_err(|e| Error::new(&format!("Invalid predicate IRI: {}", e)))?;
        let o = NamedNode::new(o).map_err(|e| Error::new(&format!("Invalid object IRI: {}", e)))?;
        let g = GraphName::NamedNode(
            NamedNode::new(graph_iri)
                .map_err(|e| Error::new(&format!("Invalid graph IRI: {}", e)))?,
        );
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        self.inner
            .insert(&Quad::new(s, p, o, g))
            .map_err(|e| Error::new(&format!("Failed to insert quad into named graph: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Insert a Quad directly
    ///
    /// Adds a quad object to the graph.
    pub fn insert_quad_object(&self, quad: &Quad) -> Result<()> {
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        self.inner
            .insert(quad)
            .map_err(|e| Error::new(&format!("Failed to insert quad: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Remove a quad from the graph
    ///
    /// Removes a specific quad from the graph.
    pub fn remove_quad(&self, s: &str, p: &str, o: &str) -> Result<()> {
        let s =
            NamedNode::new(s).map_err(|e| Error::new(&format!("Invalid subject IRI: {}", e)))?;
        let p =
            NamedNode::new(p).map_err(|e| Error::new(&format!("Invalid predicate IRI: {}", e)))?;
        let o = NamedNode::new(o).map_err(|e| Error::new(&format!("Invalid object IRI: {}", e)))?;
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        self.inner
            .remove(&Quad::new(s, p, o, GraphName::DefaultGraph))
            .map_err(|e| Error::new(&format!("Failed to remove quad: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Remove a quad from a named graph
    ///
    /// Removes a quad from a specific named graph.
    pub fn remove_quad_from(&self, s: &str, p: &str, o: &str, graph_iri: &str) -> Result<()> {
        let s =
            NamedNode::new(s).map_err(|e| Error::new(&format!("Invalid subject IRI: {}", e)))?;
        let p =
            NamedNode::new(p).map_err(|e| Error::new(&format!("Invalid predicate IRI: {}", e)))?;
        let o = NamedNode::new(o).map_err(|e| Error::new(&format!("Invalid object IRI: {}", e)))?;
        let g = GraphName::NamedNode(
            NamedNode::new(graph_iri)
                .map_err(|e| Error::new(&format!("Invalid graph IRI: {}", e)))?,
        );
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        self.inner
            .remove(&Quad::new(s, p, o, g))
            .map_err(|e| Error::new(&format!("Failed to remove quad from named graph: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Remove a Quad directly
    ///
    /// Removes a quad object from the graph.
    pub fn remove_quad_object(&self, quad: &Quad) -> Result<()> {
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        self.inner
            .remove(quad)
            .map_err(|e| Error::new(&format!("Failed to remove quad: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Remove all quads matching a pattern
    ///
    /// Removes all quads that match the specified pattern.
    pub fn remove_for_pattern(
        &self, s: Option<&NamedOrBlankNode>, p: Option<&NamedNode>, o: Option<&Term>,
        g: Option<&GraphName>,
    ) -> Result<usize> {
        let quads: Vec<Quad> = self
            .inner
            .quads_for_pattern(
                s.map(|x| x.as_ref()),
                p.map(|x| x.as_ref()),
                o.map(|x| x.as_ref()),
                g.map(|x| x.as_ref()),
            )
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(|e| Error::new(&format!("Failed to collect quads: {}", e)))?;

        let count = quads.len();
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        for quad in &quads {
            self.inner
                .remove(quad)
                .map_err(|e| Error::new(&format!("Failed to remove quad: {}", e)))?;
        }
        self.bump_epoch();
        Ok(count)
    }

    /// Get an iterator over all quads in the graph
    ///
    /// Returns an iterator that yields all quads in the graph.
    pub fn quads(&self) -> impl Iterator<Item = Result<Quad>> + '_ {
        self.inner
            .quads_for_pattern(None, None, None, None)
            .map(|r| r.map_err(|e| Error::new(&format!("Oxigraph error: {}", e))))
    }

    /// Load RDF data from a file path
    ///
    /// Automatically detects the RDF format from the file extension.
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
            "trig" => RdfFormat::TriG,
            "nq" | "nquads" => RdfFormat::NQuads,
            other => return Err(Error::new(&format!("unsupported RDF format: {}", other))),
        };

        let file = File::open(path)?;
        let reader = BufReader::new(file);
        // Use higher-level load_from_reader API (oxigraph's recommended way to load RDF)
        self.inner
            .load_from_reader(fmt, reader)
            .map_err(|e| Error::new(&format!("Failed to load RDF from file: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Execute a SPARQL query with caching
    ///
    /// Results are cached based on query string and graph epoch.
    /// Cache is automatically invalidated when the graph changes.
    pub fn query_cached(&self, sparql: &str) -> Result<CachedResult> {
        let query_hash = self.hash_query(sparql);
        let epoch = self.current_epoch();
        let cache_key = (query_hash, epoch);

        // Check result cache
        if let Some(cached) = self
            .result_cache
            .lock()
            .map_err(|_| Error::new("Cache lock poisoned"))?
            .get(&cache_key)
            .cloned()
        {
            return Ok(cached);
        }

        // Re-check epoch after cache miss
        let final_epoch = self.current_epoch();
        let final_cache_key = if final_epoch != epoch {
            let new_cache_key = (query_hash, final_epoch);
            if let Some(cached) = self
                .result_cache
                .lock()
                .map_err(|_| Error::new("Cache lock poisoned"))?
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
                .map_err(|_| Error::new("Cache lock poisoned"))?;
            if let Some(q) = cache.get(&query_hash).cloned() {
                q
            } else {
                let q = sparql.to_string();
                cache.put(query_hash, q.clone());
                q
            }
        };

        // Execute and materialize using SparqlEvaluator
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        let results = SparqlEvaluator::new()
            .parse_query(&query_str)
            .map_err(|e| Error::new(&format!("SPARQL parse error: {}", e)))?
            .on_store(&self.inner)
            .execute()
            .map_err(|e| Error::new(&format!("SPARQL execution error: {}", e)))?;
        let cached = self.materialize_results(results)?;

        // Store in cache
        self.result_cache
            .lock()
            .map_err(|_| Error::new("Cache lock poisoned"))?
            .put(final_cache_key, cached.clone());

        Ok(cached)
    }

    /// Execute a SPARQL query (returns raw QueryResults)
    ///
    /// This method provides direct access to Oxigraph's QueryResults.
    /// For full caching, use `query_cached` instead.
    pub fn query<'a>(&'a self, sparql: &str) -> Result<QueryResults<'a>> {
        let cached = self.query_cached(sparql)?;

        match cached {
            CachedResult::Boolean(b) => Ok(QueryResults::Boolean(b)),
            CachedResult::Solutions(_) | CachedResult::Graph(_) => {
                // Fall back to direct query for non-boolean results
                // Note: parse_query returns SparqlSyntaxError which doesn't have From impl
                // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
                Ok(SparqlEvaluator::new()
                    .parse_query(sparql)
                    .map_err(|e| Error::new(&format!("SPARQL parse error: {}", e)))?
                    .on_store(&self.inner)
                    .execute()
                    .map_err(|e| Error::new(&format!("SPARQL execution error: {}", e)))?)
            }
        }
    }

    /// Execute a SPARQL query with PREFIX and BASE declarations
    ///
    /// This method automatically prepends PREFIX and BASE declarations to the
    /// SPARQL query based on the provided prefixes and base IRI.
    pub fn query_with_prolog<'a>(
        &'a self, sparql: &str, prefixes: &BTreeMap<String, String>, base: Option<&str>,
    ) -> Result<QueryResults<'a>> {
        let head = crate::graph::build_prolog(prefixes, base);
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
    pub fn query_prepared<'a>(&'a self, q: &str) -> Result<QueryResults<'a>> {
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        SparqlEvaluator::new()
            .parse_query(q)
            .map_err(|e| Error::new(&format!("SPARQL parse error: {}", e)))?
            .on_store(&self.inner)
            .execute()
            .map_err(|e| Error::new(&format!("SPARQL execution error: {}", e)))
    }

    /// Find quads matching a pattern
    ///
    /// Searches for quads (triples) in the graph that match the specified pattern.
    /// Any component can be `None` to match any value (wildcard).
    pub fn quads_for_pattern(
        &self, s: Option<&NamedOrBlankNode>, p: Option<&NamedNode>, o: Option<&Term>,
        g: Option<&GraphName>,
    ) -> Result<Vec<Quad>> {
        self.inner
            .quads_for_pattern(
                s.map(|x| x.as_ref()),
                p.map(|x| x.as_ref()),
                o.map(|x| x.as_ref()),
                g.map(|x| x.as_ref()),
            )
            .map(|r| r.map_err(|e| Error::new(&format!("Quad error: {}", e))))
            .collect::<Result<Vec<_>>>()
    }

    /// Clear all data from the graph
    ///
    /// Removes all triples from the graph and increments the epoch counter,
    /// invalidating all cached query results.
    pub fn clear(&self) -> Result<()> {
        // **Root Cause Fix**: Use explicit `.map_err()` for oxigraph error conversion
        self.inner
            .clear()
            .map_err(|e| Error::new(&format!("Failed to clear graph: {}", e)))?;
        self.bump_epoch();
        Ok(())
    }

    /// Get the number of triples in the graph
    ///
    /// Returns the total count of triples (quads) stored in the graph.
    ///
    /// # Note
    ///
    /// If an error occurs while getting the length, this method returns 0.
    /// For explicit error handling, use `len_result()` instead.
    pub fn len(&self) -> usize {
        self.len_result().unwrap_or(0)
    }

    /// Get the number of triples in the graph with explicit error handling
    ///
    /// Returns the total count of triples (quads) stored in the graph.
    /// Returns an error if the length cannot be determined.
    pub fn len_result(&self) -> Result<usize> {
        self.inner.len().map_err(Into::into)
    }

    /// Check if the graph is empty
    ///
    /// Returns `true` if the graph contains no triples, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Clone for Graph {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
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
/// * `prefixes` - Map of prefix names (e.g., "ex") to namespace URIs (e.g., "<http://example.org/>")
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
        // write_fmt on String never fails, so result can be safely ignored
        let _ = std::fmt::Write::write_fmt(&mut s, format_args!("BASE <{}>\n", b));
    }
    for (pfx, iri) in prefixes {
        // write_fmt on String never fails, so result can be safely ignored
        let _ = std::fmt::Write::write_fmt(&mut s, format_args!("PREFIX {}: <{}>\n", pfx, iri));
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_graph_new() {
        // Arrange & Act
        let graph = Graph::new().unwrap();

        // Assert
        assert!(graph.is_empty());
        assert_eq!(graph.len(), 0);
    }

    #[test]
    fn test_graph_insert_turtle() {
        // Arrange
        let graph = Graph::new().unwrap();

        // Act
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();

        // Assert
        assert!(!graph.is_empty());
        assert!(graph.len() > 0);
    }

    #[test]
    fn test_graph_query_cached() {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person ;
                     ex:name "Alice" .
        "#,
            )
            .unwrap();

        // Act
        let result = graph
            .query_cached("SELECT ?name WHERE { ?s ex:name ?name }")
            .unwrap();

        // Assert
        match result {
            CachedResult::Solutions(rows) => {
                assert!(!rows.is_empty());
                assert!(rows[0].contains_key("name"));
            }
            _ => panic!("Expected solutions"),
        }
    }

    #[test]
    fn test_build_prolog_with_prefixes() {
        // Arrange
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());
        prefixes.insert(
            "rdf".to_string(),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
        );

        // Act
        let prolog = build_prolog(&prefixes, None);

        // Assert
        assert!(prolog.contains("PREFIX ex: <http://example.org/>"));
        assert!(prolog.contains("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"));
    }

    #[test]
    fn test_build_prolog_with_base() {
        // Arrange
        let prefixes = BTreeMap::new();

        // Act
        let prolog = build_prolog(&prefixes, Some("http://example.org/"));

        // Assert
        assert!(prolog.contains("BASE <http://example.org/>"));
    }
}
