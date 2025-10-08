use anyhow::Result;
use serde_json::Value;
use std::collections::BTreeMap;
use tera::{Context, Function as TeraFunction, Tera};

use crate::graph::{build_prolog, Graph};
use crate::register;

pub struct Pipeline {
    pub(crate) tera: Tera,
    pub(crate) graph: Graph,
}

impl Pipeline {
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        tera.autoescape_on(vec![]);
        register::register_all(&mut tera);
        Ok(Self { tera, graph: Graph::new()? })
    }

    /// Register SPARQL + local() for a given BASE+PREFIX set
    pub fn register_prefixes(&mut self, base: Option<&str>, prefixes: &BTreeMap<String, String>) {
        let prolog = build_prolog(prefixes, base);
        self.tera.register_function("sparql", SparqlFn { graph: self.graph.clone(), prolog });
        self.tera.register_function("local", LocalFn);
    }

    /// Pure render of a template body with a ready Context.
    pub fn render_body(&mut self, body: &str, ctx: &Context) -> Result<String> {
        Ok(self.tera.render_str(body, ctx)?)
    }
}

pub struct PipelineBuilder {
    prefixes: BTreeMap<String, String>,
    base: Option<String>,
    preload_ttl_files: Vec<String>,
    preload_ttl_inline: Vec<String>,
}

impl PipelineBuilder {
    pub fn new() -> Self {
        Self {
            prefixes: BTreeMap::new(),
            base: None,
            preload_ttl_files: vec![],
            preload_ttl_inline: vec![],
        }
    }
    pub fn with_prefixes(mut self, pfx: BTreeMap<String, String>, base: Option<String>) -> Self {
        self.prefixes = pfx; self.base = base; self
    }
    pub fn with_rdf_files<S: Into<String>>(mut self, files: impl IntoIterator<Item = S>) -> Self {
        self.preload_ttl_files = files.into_iter().map(Into::into).collect(); self
    }
    pub fn with_inline_rdf<S: Into<String>>(mut self, blocks: impl IntoIterator<Item = S>) -> Self {
        self.preload_ttl_inline = blocks.into_iter().map(Into::into).collect(); self
    }
    pub fn build(mut self) -> Result<Pipeline> {
        let mut p = Pipeline::new()?;
        for f in &self.preload_ttl_files {
            let ttl = std::fs::read_to_string(f)?;
            p.graph.insert_turtle(&ttl)?;
        }
        for ttl in &self.preload_ttl_inline {
            p.graph.insert_turtle(ttl)?;
        }
        p.register_prefixes(self.base.as_deref(), &self.prefixes);
        Ok(p)
    }
}

/* ---------- Tera helpers ---------- */
#[derive(Clone)]
struct SparqlFn {
    graph: Graph,
    prolog: String,
}

impl TeraFunction for SparqlFn {
    fn call(&self, args: &std::collections::HashMap<String, Value>) -> tera::Result<Value> {
        let q = args.get("query").and_then(|v| v.as_str()).ok_or_else(|| tera::Error::msg("sparql: query required"))?;
        let want = args.get("var").and_then(|v| v.as_str());

        let final_q = if self.prolog.is_empty() { q.to_string() } else { format!("{}\n{}", self.prolog, q) };
        let res = self.graph.query(&final_q).map_err(|e| tera::Error::msg(e.to_string()))?;

        use oxigraph::sparql::QueryResults;
        match res {
            QueryResults::Solutions(solutions) => {
                let mut rows = Vec::new();
                for sol in solutions {
                    let sol = sol.map_err(|e| tera::Error::msg(e.to_string()))?;
                    let mut row = serde_json::Map::new();
                    for (v, term) in sol.iter() {
                        row.insert(v.as_str().to_string(), serde_json::Value::String(term.to_string()));
                    }
                    rows.push(serde_json::Value::Object(row));
                }
                if let Some(name) = want {
                    if let Some(serde_json::Value::Object(obj)) = rows.first() {
                        if let Some(val) = obj.get(name) {
                            return Ok(val.clone());
                        }
                    }
                    return Ok(serde_json::Value::String(String::new()));
                }
                Ok(serde_json::Value::Array(rows))
            }
            QueryResults::Boolean(b) => Ok(serde_json::Value::Bool(b)),
            QueryResults::Graph(_) => Ok(serde_json::Value::String(String::new())),
        }
    }
}

#[derive(Clone)]
struct LocalFn;
impl TeraFunction for LocalFn {
    fn call(&self, args: &std::collections::HashMap<String, Value>) -> tera::Result<Value> {
        let iri = args.get("iri").and_then(|v| v.as_str()).unwrap_or_default();
        let s = iri.trim();
        let s = s.strip_prefix('<').and_then(|x| x.strip_suffix('>')).unwrap_or(s);
        let idx = s.rfind(|c| c == '#' || c == '/').map(|i| i + 1).unwrap_or(0);
        Ok(serde_json::Value::String(s[idx..].to_string()))
    }
}
