use anyhow::Result;
use std::collections::BTreeMap;
use std::env;
use std::path::{Path, PathBuf};
use tera::{Context, Function as TeraFunction, Tera, Value as TeraValue};

use crate::graph::{build_prolog, Graph};
use crate::register;
use crate::template::Template;

pub struct Pipeline {
    pub tera: Tera,
    pub graph: Graph,
}

impl Pipeline {
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        tera.autoescape_on(vec![]);
        
        // Register all text transformation filters
        crate::register::register_all(&mut tera);
        
        Ok(Self { tera, graph: Graph::new()? })
    }

    /// Parse → render frontmatter → load RDF → register funcs → render body
    pub fn run(&mut self, input: &str, mut vars: Context) -> Result<String> {
        let mut template = Template::parse(input)?;

        insert_env(&mut vars);
        template.render_frontmatter(&mut self.tera, &vars)?;

        // Register template-defined vars into the context (after frontmatter render)
        for (k, v) in &template.front.vars {
            vars.insert(k, v);
        }

        // Register SPARQL + local() in Tera with prolog from frontmatter
        let prolog = build_prolog(&template.front.prefixes, template.front.base.as_deref());
        self.tera.register_function(
            "sparql",
            SparqlFn { graph: self.graph.clone(), prolog: prolog.clone() },
        );
        self.tera.register_function("local", LocalFn);

        // Load RDF and execute SPARQL declared in frontmatter
        template.process_graph(&self.graph, &mut self.tera, &vars)?;

        // Render body
        let out = template.render(&mut self.tera, &vars)?;
        Ok(out)
    }

    /// Run pipeline from a template file path
    pub fn run_from_path(
        &mut self,
        template_path: &Path,
        out_root: &Path,
        vars: &BTreeMap<String, String>,
        dry: bool,
    ) -> Result<PathBuf> {
        // Read template file
        let input = std::fs::read_to_string(template_path)?;
        
        // Create Tera context from vars
        let mut ctx = Context::from_serialize(vars)?;
        
        // Parse template to get frontmatter
        let mut template = Template::parse(&input)?;
        
        // Render frontmatter first to get the final 'to' field
        insert_env(&mut ctx);
        template.render_frontmatter(&mut self.tera, &ctx)?;
        
        // Register template-defined vars into the context (after frontmatter render)
        for (k, v) in &template.front.vars {
            ctx.insert(k, v);
        }
        
        // Determine output path from frontmatter or default
        let out_path = if let Some(to_path) = &template.front.to {
            // Render the 'to' field as a template
            let rendered_to = self.tera.render_str(to_path, &ctx)?;
            out_root.join(rendered_to)
        } else {
            out_root.join("out.txt")
        };
        
        // Run the pipeline
        let rendered = self.run(&input, ctx)?;
        
        // Write output unless dry run
        if !dry {
            // Ensure output directory exists
            if let Some(parent) = out_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&out_path, rendered)?;
        }
        
        Ok(out_path)
    }
}

/* ---------- Tera helpers ---------- */

#[derive(Clone)]
struct SparqlFn {
    graph: Graph,
    prolog: String,
}

impl TeraFunction for SparqlFn {
    fn call(&self, args: &std::collections::HashMap<String, TeraValue>) -> tera::Result<TeraValue> {
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
    fn call(&self, args: &std::collections::HashMap<String, TeraValue>) -> tera::Result<TeraValue> {
        let iri = args.get("iri").and_then(|v| v.as_str()).unwrap_or_default();
        let s = iri.trim();
        let s = s.strip_prefix('<').and_then(|x| x.strip_suffix('>')).unwrap_or(s);
        let idx = s.rfind(|c| c == '#' || c == '/').map(|i| i + 1).unwrap_or(0);
        Ok(serde_json::Value::String(s[idx..].to_string()))
    }
}

/* ---------- context helpers ---------- */

fn insert_env(ctx: &mut Context) {
    let mut env_map: BTreeMap<String, String> = BTreeMap::new();
    for (k, v) in env::vars() {
        env_map.insert(k, v);
    }
    ctx.insert("env", &env_map);

    if let Ok(cwd) = env::current_dir() {
        ctx.insert("cwd", &cwd.display().to_string());
    }
}
