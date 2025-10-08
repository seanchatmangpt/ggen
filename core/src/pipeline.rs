use anyhow::Result;
use std::collections::BTreeMap;
use std::env;
use std::path::{Path, PathBuf};
use tera::{Context, Function as TeraFunction, Tera, Value as TeraValue};

use crate::config::RgenConfig;
use crate::generator::Generator;
use crate::graph::{build_prolog, Graph};
use crate::template::Template;
use crate::tera_env::build_tera_with_glob;
use crate::register::bless_context;
use crate::validate_frontmatter::validate_frontmatter;
// EolNormalizer and SkipIfGenerator are available for future use

pub struct Pipeline {
    pub tera: Tera,
    pub graph: Graph,
    pub templates_dir: PathBuf,
}

pub struct PipelineBuilder {
    config: Option<RgenConfig>,
    config_dir: Option<PathBuf>,
}

impl Pipeline {
    pub fn new() -> Result<Self> {
        let mut tera = Tera::default();
        tera.autoescape_on(vec![]);
        
        // Register all text transformation filters
        crate::register::register_all(&mut tera);
        
        Ok(Self { 
            tera, 
            graph: Graph::new()?,
            templates_dir: PathBuf::from("templates"), // Default templates directory
        })
    }
    
    /// Get the templates directory path.
    pub fn templates_dir(&self) -> PathBuf {
        self.templates_dir.clone()
    }

    /// Parse → render frontmatter → load RDF → register funcs → render body
    pub fn run(&mut self, input: &str, mut vars: Context) -> Result<String> {
        let mut template = Template::parse(input)?;

        insert_env(&mut vars);
        template.render_frontmatter(&mut self.tera, &vars)?;

        // Trace frontmatter if RGEN_TRACE is set
        if env::var("RGEN_TRACE").is_ok() {
            eprintln!("=== RGEN TRACE ===");
            eprintln!("Resolved frontmatter:");
            eprintln!("{:#?}", template.front);
        }

        // Validate frontmatter after rendering
        validate_frontmatter(&template.front)?;

        // Auto-bless context variables (Name, locals)
        bless_context(&mut vars);

        // Register template-defined vars into the context (after frontmatter render)
        for (k, v) in &template.front.vars {
            vars.insert(k, v);
        }

        // Register SPARQL + local() in Tera with prolog from frontmatter
        let prolog = build_prolog(&template.front.prefixes, template.front.base.as_deref());
        
        // Trace SPARQL prolog if RGEN_TRACE is set
        if env::var("RGEN_TRACE").is_ok() {
            eprintln!("SPARQL prolog:");
            eprintln!("{}", prolog);
        }
        
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
        
        // Trace output path if RGEN_TRACE is set
        if env::var("RGEN_TRACE").is_ok() {
            eprintln!("Target output path: {}", out_path.display());
        }
        
        // Run the pipeline
        let rendered = self.run(&input, ctx)?;
        
        // Execute pre-hook if not dry run
        if !dry {
            Generator::execute_shell_hooks(
                template.front.sh_before.as_deref(),
                None,
                &rendered,
                vars,
                dry,
            )?;
        }
        
        // Write output unless dry run
        if !dry {
            // Handle injection vs direct write
            if template.front.inject {
                // Apply injection to target file
                Generator::apply_injection(&out_path, &rendered, &template.front, dry)?;
            } else {
                // Direct write to new file
                if let Some(parent) = out_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::write(&out_path, &rendered)?;
            }
        }
        
        // Execute post-hook if not dry run
        if !dry {
            Generator::execute_shell_hooks(
                None,
                template.front.sh_after.as_deref(),
                &rendered,
                vars,
                dry,
            )?;
        }
        
        Ok(out_path)
    }
}

impl PipelineBuilder {
    /// Create a new PipelineBuilder
    pub fn new() -> Self {
        Self {
            config: None,
            config_dir: None,
        }
    }
    
    /// Load rgen.toml config from the current working directory
    pub fn with_config_discovery(mut self) -> Result<Self> {
        let cwd = std::env::current_dir()?;
        if let Some((config, config_path)) = RgenConfig::discover_and_load(&cwd)? {
            self.config = Some(config);
            self.config_dir = Some(config_path.parent().unwrap().to_path_buf());
        }
        Ok(self)
    }
    
    /// Load rgen.toml config from a specific file
    pub fn with_config_file(mut self, config_path: &Path) -> Result<Self> {
        let config = RgenConfig::load_from_file(config_path)?;
        self.config = Some(config);
        self.config_dir = Some(config_path.parent().unwrap().to_path_buf());
        Ok(self)
    }
    
    /// Build the Pipeline with loaded configuration
    pub fn build(self) -> Result<Pipeline> {
        let (templates_dir, graph) = if let (Some(config), Some(config_dir)) = (&self.config, &self.config_dir) {
            // Use config-based templates directory
            let templates_dir = config.templates_dir_path(config_dir);
            let graph = Graph::new()?;
            
            // Preload RDF data from config
            for rdf_path in config.rdf_file_paths(config_dir) {
                if rdf_path.exists() {
                    graph.load_path(&rdf_path)?;
                }
            }
            
            // Load inline RDF content
            for inline_content in config.rdf_inline_content() {
                graph.insert_turtle(&inline_content)?;
            }
            
            (templates_dir, graph)
        } else {
            // Use defaults
            (PathBuf::from("templates"), Graph::new()?)
        };
        
        // Build Tera with glob support for includes/macros
        let tera = build_tera_with_glob(&templates_dir)?;
        
        Ok(Pipeline { tera, graph, templates_dir })
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
        let q = args.get("query").and_then(|v| v.as_str())
            .ok_or_else(|| tera::Error::msg("sparql: query required"))?;
        let want = args.get("var").and_then(|v| v.as_str());

        let final_q = if self.prolog.is_empty() {
            q.to_string()
        } else {
            format!("{}\n{}", self.prolog, q)
        };

        let cached = self.graph.query_cached(&final_q)
            .map_err(|e| tera::Error::msg(e.to_string()))?;

        let json_val = cached.to_json();

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
