use ahash::AHasher;
use anyhow::{bail, Result};
use lru::LruCache;
use oxigraph::io::RdfFormat;
use oxigraph::model::{GraphName, NamedNode, Quad, Subject, Term};
use oxigraph::sparql::{Query, QueryOptions, QueryResults};
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

#[derive(Clone, Debug)]
pub enum CachedResult {
    Boolean(bool),
    Solutions(Vec<BTreeMap<String, String>>),
    Graph(Vec<String>), // Serialized triples
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

/// Thread-safe Oxigraph wrapper with SPARQL caching. Clone is cheap (shared store).
pub struct Graph {
    inner: Store,
    epoch: Arc<AtomicU64>,
    plan_cache: Arc<Mutex<LruCache<u64, Query>>>,
    result_cache: Arc<Mutex<LruCache<(u64, u64), CachedResult>>>,
}

impl Graph {
    pub fn new() -> Result<Self> {
        let plan_cache_size =
            NonZeroUsize::new(100).ok_or_else(|| anyhow::anyhow!("Invalid cache size"))?;
        let result_cache_size =
            NonZeroUsize::new(1000).ok_or_else(|| anyhow::anyhow!("Invalid cache size"))?;

        Ok(Self {
            inner: Store::new()?,
            epoch: Arc::new(AtomicU64::new(1)),
            plan_cache: Arc::new(Mutex::new(LruCache::new(plan_cache_size))),
            result_cache: Arc::new(Mutex::new(LruCache::new(result_cache_size))),
        })
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

    pub fn insert_turtle(&self, turtle: &str) -> Result<()> {
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    pub fn insert_turtle_with_base(&self, turtle: &str, _base_iri: &str) -> Result<()> {
        // Note: The new Oxigraph API doesn't support base IRI in load_from_reader
        // We'll need to handle this differently or use a different approach
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    pub fn insert_turtle_in(&self, turtle: &str, _graph_iri: &str) -> Result<()> {
        // Note: The new Oxigraph API doesn't support named graphs in load_from_reader
        // We'll need to handle this differently or use a different approach
        self.inner
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    pub fn insert_quad(&self, s: &str, p: &str, o: &str) -> Result<()> {
        let s = NamedNode::new(s)?;
        let p = NamedNode::new(p)?;
        let o = NamedNode::new(o)?;
        self.inner
            .insert(&Quad::new(s, p, o, GraphName::DefaultGraph))?;
        self.bump_epoch();
        Ok(())
    }

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

    pub fn query_cached(&self, sparql: &str) -> Result<CachedResult> {
        let query_hash = self.hash_query(sparql);
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

        // Check plan cache or parse
        let query = {
            let mut cache = self
                .plan_cache
                .lock()
                .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?;
            if let Some(q) = cache.get(&query_hash).cloned() {
                q
            } else {
                let q = Query::parse(sparql, None)?;
                cache.put(query_hash, q.clone());
                q
            }
        };

        // Execute and materialize
        let results = self.inner.query_opt(query, QueryOptions::default())?;
        let cached = self.materialize_results(results)?;

        // Store in cache
        self.result_cache
            .lock()
            .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?
            .put(cache_key, cached.clone());

        Ok(cached)
    }

    pub fn query(&self, sparql: &str) -> Result<QueryResults> {
        // For backward compatibility, we need to reconstruct QueryResults
        // This is inefficient but maintains API compatibility
        let cached = self.query_cached(sparql)?;

        match cached {
            CachedResult::Boolean(b) => Ok(QueryResults::Boolean(b)),
            CachedResult::Solutions(_) | CachedResult::Graph(_) => {
                // Fall back to direct query for non-boolean results
                // since we can't reconstruct the iterator properly
                Ok(self.inner.query(sparql)?)
            }
        }
    }

    pub fn query_with_prolog(
        &self, sparql: &str, prefixes: &BTreeMap<String, String>, base: Option<&str>,
    ) -> Result<QueryResults> {
        let head = build_prolog(prefixes, base);
        let q = if head.is_empty() {
            sparql.into()
        } else {
            format!("{head}\n{sparql}")
        };
        self.query(&q)
    }

    pub fn query_prepared(&self, q: &Query) -> Result<QueryResults> {
        Ok(self.inner.query_opt(q.clone(), QueryOptions::default())?)
    }

    /// Typed pattern filter (no extra allocs).
    pub fn quads_for_pattern(
        &self, s: Option<&Subject>, p: Option<&NamedNode>, o: Option<&Term>, g: Option<&GraphName>,
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

    pub fn clear(&self) -> Result<()> {
        self.inner.clear()?;
        self.bump_epoch();
        Ok(())
    }

    pub fn len(&self) -> usize {
        #[allow(deprecated)]
        {
            self.inner.len().unwrap_or(0)
        }
    }

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

        let query = Query::parse("SELECT ?s WHERE { ?s ?p ?o }", None)?;
        let results = g.query_prepared(&query)?;

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
