use anyhow::Result;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tera::{Context, Function as TeraFunction, Tera};
use oxigraph::sparql::QueryResults;
use sha2::{Digest, Sha256};

use crate::graph::{build_prolog, Graph};
use crate::register;
use crate::template::Frontmatter;

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

    /// Render a template file and return a Plan
    pub fn render_file(&mut self, template_path: &Path, vars: &BTreeMap<String, String>, dry_run: bool) -> Result<Plan> {
        use std::env;
        
        // Trace template path if RGEN_TRACE is set
        if env::var("RGEN_TRACE").is_ok() {
            eprintln!("=== RGEN TRACE ===");
            eprintln!("Template path: {}", template_path.display());
        }
        
        // Read template file
        let input = std::fs::read_to_string(template_path)?;
        
        // Create Tera context from vars
        let mut ctx = Context::from_serialize(vars)?;
        
        // Parse template to get frontmatter
        let mut template = crate::template::Template::parse(&input)?;
        
        // Render frontmatter first to get the final 'to' field
        template.render_frontmatter(&mut self.tera, &ctx)?;
        
        // Trace frontmatter if RGEN_TRACE is set
        if env::var("RGEN_TRACE").is_ok() {
            eprintln!("Resolved frontmatter:");
            eprintln!("{:#?}", template.front);
        }
        
        // Validate frontmatter after rendering
        crate::validate_frontmatter::validate_frontmatter(&template.front)?;
        
        // Auto-bless context variables (Name, locals)
        crate::register::bless_context(&mut ctx);
        
        // Register template-defined vars into the context (after frontmatter render)
        for (k, v) in &template.front.vars {
            ctx.insert(k, v);
        }
        
        // Determine output path from frontmatter or default
        let out_path = if let Some(to_path) = &template.front.to {
            // Render the 'to' field as a template
            let rendered_to = self.tera.render_str(to_path, &ctx)?;
            PathBuf::from(rendered_to)
        } else {
            PathBuf::from("out.txt")
        };
        
        // Trace output path if RGEN_TRACE is set
        if env::var("RGEN_TRACE").is_ok() {
            eprintln!("Target output path: {}", out_path.display());
        }
        
        // Load RDF and execute SPARQL declared in frontmatter
        template.process_graph(&mut self.graph, &mut self.tera, &ctx)?;
        
        // Render body
        let rendered = template.render(&mut self.tera, &ctx)?;
        
        // Create plan
        let plan = Plan {
            template_path: template_path.to_path_buf(),
            output_path: out_path,
            content: rendered,
            frontmatter: template.front,
            dry_run,
        };
        
        Ok(plan)
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
    
    pub fn with_rdf_file<S: Into<String>>(mut self, file: S) -> Self {
        self.preload_ttl_files.push(file.into()); self
    }
    pub fn with_inline_rdf<S: Into<String>>(mut self, blocks: impl IntoIterator<Item = S>) -> Self {
        self.preload_ttl_inline = blocks.into_iter().map(Into::into).collect(); self
    }
    pub fn build(self) -> Result<Pipeline> {
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

        let final_q = if self.prolog.is_empty() {
            q.to_string()
        } else {
            format!("{}\n{}", self.prolog, q)
        };

        let results = self.graph.query(&final_q)
            .map_err(|e| tera::Error::msg(e.to_string()))?;

        let json_val = match results {
            QueryResults::Boolean(b) => serde_json::Value::Bool(b),
            QueryResults::Solutions(solutions) => {
                let mut rows = Vec::new();
                for solution in solutions {
                    let solution = solution.map_err(|e| tera::Error::msg(e.to_string()))?;
                    let mut row = serde_json::Map::new();
                    for (var, term) in solution.iter() {
                        row.insert(var.to_string(), serde_json::Value::String(term.to_string()));
                    }
                    rows.push(serde_json::Value::Object(row));
                }
                serde_json::Value::Array(rows)
            }
            QueryResults::Graph(_) => serde_json::Value::Array(Vec::new()), // Graph results not supported in templates
        };

        // Handle var extraction if requested
        if let Some(var_name) = want {
            if let serde_json::Value::Array(rows) = &json_val {
                if let Some(serde_json::Value::Object(obj)) = rows.first() {
                    if let Some(val) = obj.get(var_name) {
                        return Ok(val.clone());
                    }
                }
            }
            return Ok(serde_json::Value::String(String::new()));
        }

        Ok(json_val)
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

/// Plan represents a generation plan that can be applied or previewed
pub struct Plan {
    template_path: PathBuf,
    output_path: PathBuf,
    content: String,
    frontmatter: Frontmatter,
    dry_run: bool,
}

impl Plan {
    /// Apply the plan by writing the content to the output path
    pub fn apply(&self) -> Result<()> {
        if self.dry_run {
            return Ok(());
        }
        
        // Check idempotency guards
        if let Some(skip_if) = &self.frontmatter.skip_if {
            if self.output_path.exists() {
                let existing_content = std::fs::read_to_string(&self.output_path)?;
                if regex::Regex::new(skip_if)?.is_match(&existing_content) {
                    println!("Skipped: pattern '{}' found in existing file", skip_if);
                    return Ok(());
                }
            }
        }
        
        if self.frontmatter.unless_exists && self.output_path.exists() {
            println!("Skipped: file already exists and unless_exists=true");
            return Ok(());
        }
        
        // Create parent directories if needed
        if let Some(parent) = self.output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        
        // Write content
        std::fs::write(&self.output_path, &self.content)?;
        
        println!("Generated: {}", self.output_path.display());
        Ok(())
    }
    
    /// Print unified diff of what would be written
    pub fn print_diff(&self) -> Result<()> {
        println!("DRY RUN - Would generate: {}", self.output_path.display());
        
        if self.output_path.exists() {
            let existing_content = std::fs::read_to_string(&self.output_path)?;
            print_unified_diff(&existing_content, &self.content, &self.output_path);
        } else {
            println!("--- /dev/null");
            println!("+++ {}", self.output_path.display());
            for line in self.content.lines() {
                println!("+{}", line);
            }
        }
        
        Ok(())
    }
    
    /// Get content hash for determinism
    pub fn content_hash(&self) -> Option<String> {
        let mut hasher = Sha256::new();
        hasher.update(self.content.as_bytes());
        Some(format!("{:x}", hasher.finalize()))
    }
}

/// Print unified diff between two strings
fn print_unified_diff(old: &str, new: &str, path: &Path) {
    println!("--- {}", path.display());
    println!("+++ {}", path.display());
    
    let old_lines: Vec<&str> = old.lines().collect();
    let new_lines: Vec<&str> = new.lines().collect();
    
    // Simple diff implementation - in production, use a proper diff library
    let max_lines = old_lines.len().max(new_lines.len());
    
    for i in 0..max_lines {
        let old_line = old_lines.get(i);
        let new_line = new_lines.get(i);
        
        match (old_line, new_line) {
            (Some(old), Some(new)) if old == new => {
                println!(" {}", old);
            }
            (Some(old), Some(new)) => {
                println!("-{}", old);
                println!("+{}", new);
            }
            (Some(old), None) => {
                println!("-{}", old);
            }
            (None, Some(new)) => {
                println!("+{}", new);
            }
            (None, None) => break,
        }
    }
}
