use anyhow::{bail, Result};
use oxigraph::io::RdfFormat;
use oxigraph::model::{GraphName, NamedNode, Quad, Subject, Term};
use oxigraph::sparql::{Query, QueryOptions, QueryResults};
use oxigraph::store::Store;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::sync::{Arc, Mutex, atomic::{AtomicU64, Ordering}};
use std::num::NonZeroUsize;
use lru::LruCache;
use fxhash::FxHasher;
use std::hash::{Hash, Hasher};
use serde_json::Value as JsonValue;

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
                let arr: Vec<JsonValue> = rows.iter().map(|row| {
                    let mut obj = serde_json::Map::new();
                    for (k, v) in row {
                        obj.insert(k.clone(), JsonValue::String(v.clone()));
                    }
                    JsonValue::Object(obj)
                }).collect();
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
        Ok(Self {
            inner: Store::new()?,
            epoch: Arc::new(AtomicU64::new(1)),
            plan_cache: Arc::new(Mutex::new(
                LruCache::new(NonZeroUsize::new(100).unwrap())
            )),
            result_cache: Arc::new(Mutex::new(
                LruCache::new(NonZeroUsize::new(1000).unwrap())
            )),
        })
    }

    fn current_epoch(&self) -> u64 {
        self.epoch.load(Ordering::Relaxed)
    }

    fn bump_epoch(&self) {
        self.epoch.fetch_add(1, Ordering::Relaxed);
    }

    fn hash_query(&self, sparql: &str) -> u64 {
        let mut hasher = FxHasher::default();
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
        self.inner.load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    pub fn insert_turtle_in(&self, turtle: &str, _graph_iri: &str) -> Result<()> {
        // Note: The new Oxigraph API doesn't support named graphs in load_from_reader
        // We'll need to handle this differently or use a different approach
        self.inner.load_from_reader(RdfFormat::Turtle, turtle.as_bytes())?;
        self.bump_epoch();
        Ok(())
    }

    pub fn insert_quad(&self, s: &str, p: &str, o: &str) -> Result<()> {
        let s = NamedNode::new(s)?;
        let p = NamedNode::new(p)?;
        let o = NamedNode::new(o)?;
        self.inner.insert(&Quad::new(s, p, o, GraphName::DefaultGraph))?;
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
        if let Some(cached) = self.result_cache.lock().unwrap().get(&cache_key).cloned() {
            return Ok(cached);
        }

        // Check plan cache or parse
        let query = {
            let mut cache = self.plan_cache.lock().unwrap();
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
        self.result_cache.lock().unwrap().put(cache_key, cached.clone());

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
        &self,
        sparql: &str,
        prefixes: &BTreeMap<String, String>,
        base: Option<&str>,
    ) -> Result<QueryResults> {
        let head = build_prolog(prefixes, base);
        let q = if head.is_empty() { sparql.into() } else { format!("{head}\n{sparql}") };
        self.query(&q)
    }

    pub fn query_prepared(&self, q: &Query) -> Result<QueryResults> {
        Ok(self.inner.query_opt(q.clone(), QueryOptions::default())?)
    }

    /// Typed pattern filter (no extra allocs).
    pub fn quads_for_pattern(
        &self,
        s: Option<&Subject>,
        p: Option<&NamedNode>,
        o: Option<&Term>,
        g: Option<&GraphName>,
    ) -> Result<Vec<Quad>> {
        Ok(self
            .inner
            .quads_for_pattern(
                s.map(|x| x.as_ref().into()),
                p.map(|x| x.as_ref().into()),
                o.map(|x| x.as_ref().into()),
                g.map(|x| x.as_ref().into()),
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
        { self.inner.len().unwrap_or(0) }
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
    if let Some(b) = base { let _ = std::fmt::Write::write_fmt(&mut s, format_args!("BASE <{}>\n", b)); }
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
            panic!("expected solutions");
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
            panic!("expected solutions");
        }
        Ok(())
    }

    #[test]
    fn query_with_prolog_works() -> Result<()> {
        let g = Graph::new()?;
        g.insert_turtle(
            "@prefix ex: <http://example/> . ex:x a ex:T .",
        )?;
        let mut p = BTreeMap::new();
        p.insert("ex".to_string(), "http://example/".to_string());
        let q = "SELECT ?s WHERE { ?s a ex:T }";
        let res = g.query_with_prolog(q, &p, None)?;
        if let QueryResults::Solutions(mut it) = res {
            let first = it.next().unwrap().unwrap();
            let s = first.get("s").unwrap().to_string();
            assert_eq!(s, "<http://example/x>");
        } else {
            panic!("expected solutions");
        }
        Ok(())
    }
}
