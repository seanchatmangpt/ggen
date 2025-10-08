use anyhow::{bail, Result};
use oxigraph::io::RdfFormat;
use oxigraph::sparql::{Query, QueryResults};
use oxigraph::store::Store;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

/// Thread-safe Oxigraph wrapper. Clone is cheap (shared store).
#[derive(Clone)]
pub struct Graph {
    inner: Store,
}

impl Graph {
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: Store::new()?,
        })
    }

    pub fn insert_turtle(&mut self, turtle: &str) -> Result<()> {
        self.inner.load_from_reader(
            RdfFormat::Turtle,
            turtle.as_bytes(),
        )?;
        Ok(())
    }

    pub fn query(&self, sparql: &str) -> Result<QueryResults> {
        let query = Query::parse(sparql, None)?;
        Ok(self.inner.query(query)?)
    }

    pub fn load_path(&mut self, path: &Path) -> Result<()> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        
        let format = match path.extension().and_then(|s| s.to_str()) {
            Some("ttl") | Some("turtle") => RdfFormat::Turtle,
            Some("rdf") | Some("xml") => RdfFormat::RdfXml,
            Some("nt") => RdfFormat::NTriples,
            _ => bail!("Unknown RDF format for file: {}", path.display()),
        };

        self.inner.load_from_reader(format, reader)?;
        Ok(())
    }
}

pub fn build_prolog(prefixes: &BTreeMap<String, String>, base: Option<&str>) -> String {
    let mut prolog = String::new();
    
    if let Some(base_iri) = base {
        prolog.push_str(&format!("BASE <{}>\n", base_iri));
    }
    
    for (prefix, iri) in prefixes {
        prolog.push_str(&format!("PREFIX {}: <{}>\n", prefix, iri));
    }
    
    prolog
}