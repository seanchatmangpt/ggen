use anyhow::Result;
use oxigraph::sparql::QueryResults;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tera::{Context, Function as TeraFunction, Tera};

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
        Ok(Self {
            tera,
            graph: Graph::new()?,
        })
    }

    /// Get mutable reference to the Tera instance
    pub fn tera_mut(&mut self) -> &mut Tera {
        &mut self.tera
    }

    /// Register SPARQL + local() for a given BASE+PREFIX set
    pub fn register_prefixes(&mut self, base: Option<&str>, prefixes: &BTreeMap<String, String>) {
        let prolog = build_prolog(prefixes, base);
        self.tera.register_function(
            "sparql",
            SparqlFn {
                graph: self.graph.clone(),
                prolog,
            },
        );
        self.tera.register_function("local", LocalFn);
    }

    /// Pure render of a template body with a ready Context.
    pub fn render_body(&mut self, body: &str, ctx: &Context) -> Result<String> {
        Ok(self.tera.render_str(body, ctx)?)
    }

    /// Render a template file and return a Plan
    pub fn render_file(
        &mut self, template_path: &Path, vars: &BTreeMap<String, String>, dry_run: bool,
    ) -> Result<Plan> {
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
        self.prefixes = pfx;
        self.base = base;
        self
    }
    pub fn with_rdf_files<S: Into<String>>(mut self, files: impl IntoIterator<Item = S>) -> Self {
        self.preload_ttl_files = files.into_iter().map(Into::into).collect();
        self
    }

    pub fn with_rdf_file<S: Into<String>>(mut self, file: S) -> Self {
        self.preload_ttl_files.push(file.into());
        self
    }
    pub fn with_inline_rdf<S: Into<String>>(mut self, blocks: impl IntoIterator<Item = S>) -> Self {
        self.preload_ttl_inline = blocks.into_iter().map(Into::into).collect();
        self
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
        let q = args
            .get("query")
            .and_then(|v| v.as_str())
            .ok_or_else(|| tera::Error::msg("sparql: query required"))?;
        let want = args.get("var").and_then(|v| v.as_str());

        let final_q = if self.prolog.is_empty() {
            q.to_string()
        } else {
            format!("{}\n{}", self.prolog, q)
        };

        let results = self
            .graph
            .query(&final_q)
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
        let s = s
            .strip_prefix('<')
            .and_then(|x| x.strip_suffix('>'))
            .unwrap_or(s);
        let idx = s
            .rfind(|c| c == '#' || c == '/')
            .map(|i| i + 1)
            .unwrap_or(0);
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

        // Handle injection templates
        if self.frontmatter.inject {
            return self.apply_injection();
        }

        // Handle regular file generation
        self.apply_regular()
    }

    /// Apply injection logic for modifying existing files
    fn apply_injection(&self) -> Result<()> {
        use crate::inject::{EolNormalizer, SkipIfGenerator};

        // Check if target file exists
        if !self.output_path.exists() {
            // Create new file with content
            if let Some(parent) = self.output_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&self.output_path, &self.content)?;
            println!("Created: {}", self.output_path.display());
            return Ok(());
        }

        // Read existing content
        let existing_content = std::fs::read_to_string(&self.output_path)?;

        // Check idempotency guards
        if let Some(skip_if) = &self.frontmatter.skip_if {
            if regex::Regex::new(skip_if)?.is_match(&existing_content) {
                println!("Skipped: pattern '{}' found in existing file", skip_if);
                return Ok(());
            }
        }

        // Check if content already exists (idempotent mode)
        if self.frontmatter.idempotent {
            if SkipIfGenerator::content_exists_in_file(&self.content, &self.output_path)? {
                println!("Skipped: content already exists in file (idempotent mode)");
                return Ok(());
            }
        }

        // Create backup if requested
        if self.frontmatter.backup.unwrap_or(false) {
            let backup_path = format!("{}.backup", self.output_path.display());
            std::fs::copy(&self.output_path, &backup_path)?;
            println!("Backup created: {}", backup_path);
        }

        // Normalize EOL for injection content
        let normalized_content =
            EolNormalizer::normalize_to_match_file(&self.content, &self.output_path)?;

        // Apply injection based on mode
        let new_content = self.inject_content(&existing_content, &normalized_content)?;

        // Execute shell hook before writing (if specified)
        if let Some(sh_before) = &self.frontmatter.sh_before {
            self.execute_shell_hook(sh_before, "before")?;
        }

        // Write the modified content
        std::fs::write(&self.output_path, new_content)?;
        println!("Injected: {}", self.output_path.display());

        // Execute shell hook after writing (if specified)
        if let Some(sh_after) = &self.frontmatter.sh_after {
            self.execute_shell_hook(sh_after, "after")?;
        }

        Ok(())
    }

    /// Apply regular file generation (non-injection)
    fn apply_regular(&self) -> Result<()> {
        // Check unless_exists guard
        if self.frontmatter.unless_exists && self.output_path.exists() {
            println!("Skipped: file already exists and unless_exists=true");
            return Ok(());
        }

        // Check force flag
        if !self.frontmatter.force && self.output_path.exists() {
            return Err(anyhow::anyhow!(
                "File already exists: {}. Use --force to overwrite.",
                self.output_path.display()
            ));
        }

        // Create parent directories if needed
        if let Some(parent) = self.output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Execute shell hook before writing (if specified)
        if let Some(sh_before) = &self.frontmatter.sh_before {
            self.execute_shell_hook(sh_before, "before")?;
        }

        // Write content
        std::fs::write(&self.output_path, &self.content)?;

        println!("Generated: {}", self.output_path.display());

        // Execute shell hook after writing (if specified)
        if let Some(sh_after) = &self.frontmatter.sh_after {
            self.execute_shell_hook(sh_after, "after")?;
        }

        Ok(())
    }

    /// Inject content into existing content based on frontmatter settings
    fn inject_content(&self, existing: &str, new_content: &str) -> Result<String> {
        let mut lines: Vec<&str> = existing.lines().collect();
        let new_lines: Vec<&str> = new_content.lines().collect();

        match (
            self.frontmatter.prepend,
            self.frontmatter.append,
            &self.frontmatter.before,
            &self.frontmatter.after,
            self.frontmatter.at_line,
        ) {
            (true, _, _, _, _) => {
                // Prepend mode
                let mut result = new_lines;
                result.extend(lines);
                Ok(result.join("\n"))
            }
            (_, true, _, _, _) => {
                // Append mode
                let mut result = lines;
                result.extend(new_lines);
                Ok(result.join("\n"))
            }
            (_, _, Some(before), _, _) => {
                // Before mode - find the line and insert before it
                if let Some(index) = lines.iter().position(|line| line.contains(before)) {
                    let mut result = lines[..index].to_vec();
                    result.extend(new_lines);
                    result.extend(lines[index..].to_vec());
                    Ok(result.join("\n"))
                } else {
                    // Pattern not found, append
                    let mut result = lines;
                    result.extend(new_lines);
                    Ok(result.join("\n"))
                }
            }
            (_, _, _, Some(after), _) => {
                // After mode - find the line and insert after it
                if let Some(index) = lines.iter().position(|line| line.contains(after)) {
                    let mut result = lines[..=index].to_vec();
                    result.extend(new_lines);
                    result.extend(lines[index + 1..].to_vec());
                    Ok(result.join("\n"))
                } else {
                    // Pattern not found, append
                    let mut result = lines;
                    result.extend(new_lines);
                    Ok(result.join("\n"))
                }
            }
            (_, _, _, _, Some(line_num)) => {
                // At line mode - insert at specific line number (1-based)
                let index = (line_num as usize).saturating_sub(1);
                let mut result = lines[..index].to_vec();
                result.extend(new_lines);
                result.extend(lines[index..].to_vec());
                Ok(result.join("\n"))
            }
            _ => {
                // Default to append if no mode specified
                let mut result = lines;
                result.extend(new_lines);
                Ok(result.join("\n"))
            }
        }
    }

    /// Execute a shell hook command
    fn execute_shell_hook(&self, command: &str, timing: &str) -> Result<()> {
        use std::process::Command;

        println!("Executing {} hook: {}", timing, command);

        // Execute the command in the current directory
        let output = Command::new("sh")
            .arg("-c")
            .arg(command)
            .current_dir(std::env::current_dir()?)
            .output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow::anyhow!("Shell hook failed: {}", stderr));
        }

        // Print stdout if any
        if !output.stdout.is_empty() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            print!("{}", stdout);
        }

        Ok(())
    }

    /// Print unified diff of what would be written
    pub fn print_diff(&self) -> Result<()> {
        println!("DRY RUN - Would generate: {}", self.output_path.display());

        if self.output_path.exists() {
            let existing_content = std::fs::read_to_string(&self.output_path)?;
            print_colorized_diff(&existing_content, &self.content, &self.output_path);
        } else {
            print_colorized_new_file(&self.content, &self.output_path);
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

/// Print colorized diff between two strings
fn print_colorized_diff(old: &str, new: &str, path: &Path) {
    use colored::*;

    println!("{} {}", "---".red(), path.display());
    println!("{} {}", "+++".green(), path.display());

    let old_lines: Vec<&str> = old.lines().collect();
    let new_lines: Vec<&str> = new.lines().collect();

    // Simple diff implementation with colors
    let max_lines = old_lines.len().max(new_lines.len());

    for i in 0..max_lines {
        let old_line = old_lines.get(i);
        let new_line = new_lines.get(i);

        match (old_line, new_line) {
            (Some(old), Some(new)) if old == new => {
                println!(" {}", old);
            }
            (Some(old), Some(new)) => {
                println!("{}{}", "-".red(), old.red());
                println!("{}{}", "+".green(), new.green());
            }
            (Some(old), None) => {
                println!("{}{}", "-".red(), old.red());
            }
            (None, Some(new)) => {
                println!("{}{}", "+".green(), new.green());
            }
            (None, None) => break,
        }
    }
}

/// Print colorized output for new file creation
fn print_colorized_new_file(content: &str, path: &Path) {
    use colored::*;

    println!("{} /dev/null", "---".red());
    println!("{} {}", "+++".green(), path.display());

    for line in content.lines() {
        println!("{}{}", "+".green(), line.green());
    }
}
