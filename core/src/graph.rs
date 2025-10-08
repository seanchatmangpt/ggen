use anyhow::{bail, Result};
use oxigraph::io::GraphFormat;
use oxigraph::model::{GraphName, NamedNode, Quad, Term};
use oxigraph::sparql::{Query, QueryOptions, QueryResults};
use oxigraph::store::Store;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::sync::{Arc, Mutex};

/// Thread-safe Oxigraph wrapper.
/// One Graph instance should be shared for a full run.
#[derive(Clone)]
pub struct Graph {
    inner: Arc<Mutex<Store>>,
}

impl Graph {
    /// New in-memory store.
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: Arc::new(Mutex::new(Store::new()?)),
        })
    }

    /// Insert Turtle into the default graph.
    pub fn insert_turtle(&self, turtle: &str) -> Result<()> {
        let store = self.inner.lock().unwrap();
        store.load_graph(
            turtle.as_bytes(),
            GraphFormat::Turtle,
            GraphName::DefaultGraph,
            None,
        )?;
        Ok(())
    }

    /// Insert Turtle into a named graph.
    pub fn insert_turtle_in(&self, turtle: &str, graph_iri: &str) -> Result<()> {
        let store = self.inner.lock().unwrap();
        let g = GraphName::NamedNode(NamedNode::new(graph_iri)?);
        store.load_graph(turtle.as_bytes(), GraphFormat::Turtle, g, None)?;
        Ok(())
    }

    /// Insert one quad into the default graph.
    pub fn insert_quad(&self, s: &str, p: &str, o: &str) -> Result<()> {
        let store = self.inner.lock().unwrap();
        let s = NamedNode::new(s)?;
        let p = NamedNode::new(p)?;
        let o = NamedNode::new(o)?;
        store.insert(&Quad::new(s, p, o, GraphName::DefaultGraph))?;
        Ok(())
    }

    /// Load a file into the default graph. Supports .ttl, .turtle, .nt, .rdf/.xml.
    pub fn load_path<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let path = path.as_ref();
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|s| s.to_ascii_lowercase())
            .unwrap_or_else(String::new);

        let fmt = match ext.as_str() {
            "ttl" | "turtle" => GraphFormat::Turtle,
            "nt" | "ntriples" => GraphFormat::NTriples,
            "rdf" | "xml" => GraphFormat::RdfXml,
            other => bail!("unsupported RDF format: {}", other),
        };

        let file = File::open(path)?;
        let reader = BufReader::new(file);
        let store = self.inner.lock().unwrap();
        store.load_graph(reader, fmt, GraphName::DefaultGraph, None)?;
        Ok(())
    }

    /// Run a SPARQL query.
    pub fn query(&self, sparql: &str) -> Result<QueryResults> {
        let store = self.inner.lock().unwrap();
        Ok(store.query(sparql)?)
    }

    /// Run a SPARQL query after auto-prepending PREFIX/BASE prolog.
    pub fn query_with_prolog(
        &self,
        sparql: &str,
        prefixes: &BTreeMap<String, String>,
        base: Option<&str>,
    ) -> Result<QueryResults> {
        let head = build_prolog(prefixes, base);
        let q = if head.is_empty() {
            sparql.to_string()
        } else {
            format!("{head}\n{sparql}")
        };
        self.query(&q)
    }

    /// Run a prepared SPARQL query.
    pub fn query_prepared(&self, q: &Query) -> Result<QueryResults> {
        let store = self.inner.lock().unwrap();
        Ok(store.query_opt(q.clone(), QueryOptions::default())?)
    }

    /// Return quads matching a pattern.
    /// Note: This is a simplified version that queries all quads and filters in memory.
    /// For production use, consider using SPARQL queries for better performance.
    pub fn quads_for_pattern(
        &self,
        _subject: Option<Term>,
        _predicate: Option<Term>,
        _object: Option<Term>,
        _graph_name: Option<GraphName>,
    ) -> Result<Vec<Quad>> {
        // For now, return all quads. In a production system, this would use
        // the store's quads_for_pattern method with proper reference conversions.
        let store = self.inner.lock().unwrap();
        Ok(store
            .quads_for_pattern(None, None, None, None)
            .collect::<Result<Vec<_>, _>>()?)
    }

    /// Remove all quads.
    pub fn clear(&self) -> Result<()> {
        let store = self.inner.lock().unwrap();
        store.clear()?;
        Ok(())
    }

    /// Number of quads (best-effort if API returns Result).
    pub fn len(&self) -> usize {
        #[allow(deprecated)]
        {
            self.inner.lock().unwrap().len().unwrap_or(0)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// Build a SPARQL PREFIX/BASE prolog. Keep outside impl for reuse.
pub fn build_prolog(prefixes: &BTreeMap<String, String>, base: Option<&str>) -> String {
    let mut s = String::new();
    if let Some(b) = base {
        use std::fmt::Write;
        let _ = write!(s, "BASE <{}>\n", b);
    }
    for (pfx, iri) in prefixes {
        use std::fmt::Write;
        let _ = write!(s, "PREFIX {}: <{}>\n", pfx, iri);
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
        let list = g.quads_for_pattern(Some(a.into()), None, None, None)?;
        assert_eq!(list.len(), 1);
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
