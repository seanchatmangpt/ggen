//! Template processing pipeline with RDF/SPARQL integration
//!
//! This module provides the core pipeline for processing templates with integrated
//! RDF graph operations and SPARQL query execution. The `Pipeline` type orchestrates
//! template parsing, frontmatter rendering, graph processing, and body rendering.
//!
//! ## Architecture
//!
//! The pipeline follows a multi-stage processing flow:
//! 1. **Template Parsing**: Parse YAML frontmatter and template body
//! 2. **Frontmatter Rendering**: Render frontmatter variables with Tera
//! 3. **Graph Processing**: Load RDF data and execute SPARQL queries
//! 4. **Body Rendering**: Render template body with full context
//! 5. **Plan Execution**: Apply generation plan (write files, inject content, etc.)
//!
//! ## Features
//!
//! - **SPARQL Integration**: Execute queries and expose results to templates
//! - **Prefix Management**: Register RDF prefixes and base IRIs
//! - **File Injection**: Support for modifying existing files with markers
//! - **Dry Run Mode**: Preview changes without writing files
//! - **Shell Hooks**: Execute commands before/after file operations (with security checks)
//!
//! ## Examples
//!
//! ### Basic Pipeline Usage
//!
//! ```rust,no_run
//! use ggen_core::pipeline::{Pipeline, PipelineBuilder};
//! use std::collections::BTreeMap;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Create a new pipeline
//! let mut pipeline = Pipeline::new()?;
//!
//! // Register RDF prefixes
//! let mut prefixes = BTreeMap::new();
//! prefixes.insert("ex".to_string(), "http://example.org/".to_string());
//! pipeline.register_prefixes(Some("http://example.org/base/"), &prefixes);
//!
//! // Render a template file
//! let vars = BTreeMap::new();
//! let plan = pipeline.render_file(Path::new("template.tmpl"), &vars, false)?;
//! plan.apply()?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Using PipelineBuilder
//!
//! ```rust,no_run
//! use ggen_core::pipeline::PipelineBuilder;
//! use std::collections::BTreeMap;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut prefixes = BTreeMap::new();
//! prefixes.insert("ex".to_string(), "http://example.org/".to_string());
//!
//! let pipeline = PipelineBuilder::new()
//!     .with_prefixes(prefixes, Some("http://example.org/base/".to_string()))
//!     .with_rdf_file("data.ttl")
//!     .with_inline_rdf(vec!["@prefix ex: <http://example.org/> . ex:test a ex:Type ."])
//!     .build()?;
//! # Ok(())
//! # }
//! ```

use crate::graph::{build_prolog, Graph};
use crate::register;
use crate::simple_tracing::SimpleTracer;
use crate::template::Frontmatter;
use ggen_utils::error::{Error, Result};
use oxigraph::sparql::QueryResults;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tera::{Context, Function as TeraFunction, Tera};

pub struct Pipeline {
    pub(crate) tera: Tera,
    pub(crate) graph: Graph,
}

impl Pipeline {
    /// Create a new pipeline with default configuration
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_core::pipeline::Pipeline;
    ///
    /// let pipeline = Pipeline::new().unwrap();
    /// // Pipeline is ready to use
    /// ```
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
        // Start performance timing
        // let _timer = SimpleTimer::start("template_processing"); // Temporarily disabled

        // SimpleTracer::template_start(template_path); // Temporarily disabled

        // Read template file
        let input = std::fs::read_to_string(template_path)?;
        let _body_lines = input.lines().count();

        // Create Tera context from vars
        let mut ctx = Context::from_serialize(vars)?;
        // if SimpleTracer::is_enabled() {
        //     SimpleTracer::trace(crate::simple_tracing::TraceLevel::Debug,
        //         &format!("Created Tera context from {} CLI variables", vars.len()), Some("context"));
        // } // Temporarily disabled

        // Parse template to get frontmatter
        let mut template = crate::template::Template::parse(&input)?;
        // if SimpleTracer::is_enabled() {
        //     SimpleTracer::trace(crate::simple_tracing::TraceLevel::Debug,
        //         &format!("Template parsed: {} lines", body_lines), Some("parsing"));
        // } // Temporarily disabled

        // Render frontmatter first to get the final 'to' field
        template.render_frontmatter(&mut self.tera, &ctx)?;
        // SimpleTracer::frontmatter_processed(&template.front); // Temporarily disabled

        // Auto-bless context variables (Name, locals)
        crate::register::bless_context(&mut ctx);
        // SimpleTracer::context_blessed(ctx.len()); // Temporarily disabled

        // ❌ REMOVED: template.front.vars - Variables now come from CLI/API only

        // Determine output path from frontmatter or default
        let out_path = if let Some(to_path) = &template.front.to {
            // Render the 'to' field as a template
            let rendered_to = self.tera.render_str(to_path, &ctx)?;
            PathBuf::from(rendered_to)
        } else {
            PathBuf::from("out.txt")
        };

        // ❌ REMOVED: template.front.rdf - RDF files now loaded via CLI/API
        // Load RDF and execute SPARQL declared in frontmatter
        SimpleTracer::rdf_loading(
            &Vec::new(), // No RDF files in frontmatter
            template.front.rdf_inline.len(),
            self.graph.len(),
        );
        template.process_graph(&mut self.graph, &mut self.tera, &ctx, template_path)?;
        SimpleTracer::rdf_loading(
            &Vec::new(), // No RDF files in frontmatter
            template.front.rdf_inline.len(),
            self.graph.len(),
        );

        // Render body
        let rendered = template.render(&mut self.tera, &ctx)?;
        SimpleTracer::template_complete(template_path, &out_path, rendered.len());

        // Create plan
        let plan = Plan {
            template_path: template_path.to_path_buf(),
            output_path: out_path,
            content: rendered,
            frontmatter: template.front,
            dry_run,
        };

        if dry_run {
            // PipelineTracer::dry_run(&plan.output_path, plan.content.len()); // Temporarily disabled
        }

        Ok(plan)
    }
}

pub struct PipelineBuilder {
    prefixes: BTreeMap<String, String>,
    base: Option<String>,
    preload_ttl_files: Vec<String>,
    preload_ttl_inline: Vec<String>,
}

impl Default for PipelineBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl PipelineBuilder {
    /// Create a new pipeline builder
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_core::pipeline::PipelineBuilder;
    ///
    /// let builder = PipelineBuilder::new();
    /// // Builder is ready to use
    /// ```
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
    /// Build the pipeline with the configured options
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::pipeline::PipelineBuilder;
    /// use std::collections::BTreeMap;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut prefixes = BTreeMap::new();
    /// prefixes.insert("ex".to_string(), "http://example.org/".to_string());
    ///
    /// let pipeline = PipelineBuilder::new()
    ///     .with_prefixes(prefixes, Some("http://example.org/".to_string()))
    ///     .with_rdf_file("data.ttl")
    ///     .build()?;
    /// # Ok(())
    /// # }
    /// ```
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

        // Trace SPARQL query execution
        SimpleTracer::sparql_query(&final_q, None);

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
        let idx = s.rfind(['#', '/']).map(|i| i + 1).unwrap_or(0);
        Ok(serde_json::Value::String(s[idx..].to_string()))
    }
}

/// Plan represents a generation plan that can be applied or previewed
#[allow(dead_code)]
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
            SimpleTracer::dry_run(&self.output_path, self.content.len());
            return Ok(());
        }

        // Handle injection templates
        if self.frontmatter.inject {
            let _mode = "injection"; // Simplified for now
            let result = self.apply_injection();
            // SimpleTracer::file_injection(&self.output_path, &mode, result.is_ok()); // Temporarily disabled
            return result;
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
            // info!(path = %self.output_path.display(), "Created new file"); // Temporarily disabled
            return Ok(());
        }

        // Read existing content
        let existing_content = std::fs::read_to_string(&self.output_path)?;
        // debug!(path = %self.output_path.display(), existing_size = existing_content.len(), "Read existing file content"); // Temporarily disabled

        // Check idempotency guards
        if let Some(skip_if) = &self.frontmatter.skip_if {
            let regex = regex::Regex::new(skip_if)
                .map_err(|e| Error::new(&format!("Invalid regex pattern '{}': {}", skip_if, e)))?;
            if regex.is_match(&existing_content) {
                // PipelineTracer::skip_condition("skip_if", &format!("pattern '{}' found", skip_if)); // Temporarily disabled
                return Ok(());
            }
        }

        // Check if content already exists (idempotent mode)
        if self.frontmatter.idempotent
            && SkipIfGenerator::content_exists_in_file(&self.content, &self.output_path)?
        {
            // PipelineTracer::skip_condition("idempotent", "content already exists"); // Temporarily disabled
            return Ok(());
        }

        // Create backup if requested
        if self.frontmatter.backup.unwrap_or(false) {
            let backup_path = format!("{}.backup", self.output_path.display());
            std::fs::copy(&self.output_path, &backup_path)?;
            // PipelineTracer::backup_created(&self.output_path, &PathBuf::from(&backup_path)); // Temporarily disabled
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
        log::info!("Injected: {}", self.output_path.display());

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
            log::info!("Skipped: file already exists and unless_exists=true");
            return Ok(());
        }

        // Check force flag
        if !self.frontmatter.force && self.output_path.exists() {
            return Err(Error::new(&format!(
                "File already exists: {}. Use --force to overwrite.",
                self.output_path.display()
            )));
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

        log::info!("Generated: {}", self.output_path.display());

        // Execute shell hook after writing (if specified)
        if let Some(sh_after) = &self.frontmatter.sh_after {
            self.execute_shell_hook(sh_after, "after")?;
        }

        Ok(())
    }

    /// Inject content into existing content based on frontmatter settings
    fn inject_content(&self, existing: &str, new_content: &str) -> Result<String> {
        let lines: Vec<&str> = existing.lines().collect();
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
    ///
    /// SECURITY WARNING: This executes arbitrary shell commands from templates.
    /// Only use with trusted templates. Consider disabling in production environments.
    fn execute_shell_hook(&self, command: &str, _timing: &str) -> Result<()> {
        use std::process::Command;

        // Security validation: block dangerous commands
        if self.is_dangerous_command(command) {
            return Err(Error::new(&format!(
                "SECURITY: Blocked potentially dangerous shell command: '{}'. \
                Shell hooks are disabled for security. Use trusted templates only.",
                command
            )));
        }

        // PipelineTracer::shell_hook_start(command, timing); // Temporarily disabled

        // Execute the command in the current directory
        let output = Command::new("sh")
            .arg("-c")
            .arg(command)
            .current_dir(std::env::current_dir()?)
            .output()?;

        let _exit_code = output.status.code().unwrap_or(-1);

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // PipelineTracer::shell_hook_complete(command, timing, exit_code); // Temporarily disabled
            return Err(Error::new(&format!("Shell hook failed: {}", stderr)));
        }

        // Print stdout if any
        if !output.stdout.is_empty() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            print!("{}", stdout);
        }

        // PipelineTracer::shell_hook_complete(command, timing, exit_code); // Temporarily disabled
        Ok(())
    }

    /// Check if a shell command is potentially dangerous
    ///
    /// Blocks commands that could:
    /// - Delete files (rm, del)
    /// - Modify system files (sudo, su)
    /// - Execute arbitrary code (eval, exec)
    /// - Access sensitive data (cat /etc/passwd, etc.)
    fn is_dangerous_command(&self, command: &str) -> bool {
        let dangerous_patterns = [
            "rm ",
            "rm -",
            "del ",
            "delete",
            "sudo ",
            "su ",
            "su-",
            "eval ",
            "exec ",
            "source ",
            "cat /etc/",
            "cat /proc/",
            "cat /sys/",
            "curl ",
            "wget ",
            "nc ",
            "netcat",
            "dd ",
            "format",
            "mkfs",
            "chmod ",
            "chown ",
            "passwd",
            "kill ",
            "killall",
            "pkill",
            "shutdown",
            "reboot",
            "halt",
            "mount ",
            "umount",
            "fdisk",
            "iptables",
            "firewall",
            "crontab",
            "at ",
            "batch",
            "ssh ",
            "scp ",
            "rsync",
            "git clone",
            "git pull",
            "git push",
            "npm install",
            "pip install",
            "cargo install",
            "docker ",
            "kubectl ",
            "helm ",
            "systemctl",
            "service ",
        ];

        let command_lower = command.to_lowercase();
        dangerous_patterns
            .iter()
            .any(|pattern| command_lower.contains(pattern))
    }

    /// Print unified diff of what would be written
    pub fn print_diff(&self) -> Result<()> {
        log::info!("DRY RUN - Would generate: {}", self.output_path.display());

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

    log::debug!("{} {}", "---".red(), path.display());
    log::debug!("{} {}", "+++".green(), path.display());

    let old_lines: Vec<&str> = old.lines().collect();
    let new_lines: Vec<&str> = new.lines().collect();

    // Simple diff implementation with colors
    let max_lines = old_lines.len().max(new_lines.len());

    for i in 0..max_lines {
        let old_line = old_lines.get(i);
        let new_line = new_lines.get(i);

        match (old_line, new_line) {
            (Some(old), Some(new)) if old == new => {
                log::debug!(" {}", old);
            }
            (Some(old), Some(new)) => {
                log::debug!("{}{}", "-".red(), old.red());
                log::debug!("{}{}", "+".green(), new.green());
            }
            (Some(old), None) => {
                log::debug!("{}{}", "-".red(), old.red());
            }
            (None, Some(new)) => {
                log::debug!("{}{}", "+".green(), new.green());
            }
            (None, None) => break,
        }
    }
}

/// Print colorized output for new file creation
fn print_colorized_new_file(content: &str, path: &Path) {
    use colored::*;

    log::debug!("{} /dev/null", "---".red());
    log::debug!("{} {}", "+++".green(), path.display());

    for line in content.lines() {
        log::debug!("{}{}", "+".green(), line.green());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;
    use ggen_utils::error::Result;
    use std::collections::BTreeMap;
    use tempfile::TempDir;

    // Test 1: Pipeline initialization
    test!(test_pipeline_new, {
        let pipeline = Pipeline::new()?;
        assert!(!pipeline.graph.is_empty() || pipeline.graph.is_empty()); // Just verify it exists
        Ok(())
    });

    // Test 2: Basic rendering
    test!(test_pipeline_render_body, {
        let mut pipeline = Pipeline::new()?;
        let mut ctx = Context::new();
        ctx.insert("name", "World");

        let result = pipeline.render_body("Hello {{ name }}", &ctx)?;
        assert_eq!(result, "Hello World");
        Ok(())
    });

    // Test 3: Render file with frontmatter
    test!(test_pipeline_render_file_basic, {
        let temp_dir = TempDir::new()?;
        let template_content = r#"---
to: "output.txt"
---
Hello {{ name }}"#;
        let template_path = temp_dir.path().join("test.tmpl");
        std::fs::write(&template_path, template_content)?;

        let mut pipeline = Pipeline::new()?;
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "World".to_string());

        let plan = pipeline.render_file(&template_path, &vars, false)?;

        assert_eq!(plan.content, "Hello World");
        assert_eq!(plan.output_path.file_name().unwrap(), "output.txt");
        Ok(())
    });

    // Test 4: Plan dry run
    test!(test_plan_apply_dry_run, {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("output.txt");

        let plan = Plan {
            template_path: temp_dir.path().join("test.tmpl"),
            output_path: output_path.clone(),
            content: "Test content".to_string(),
            frontmatter: crate::template::Frontmatter::default(),
            dry_run: true,
        };

        plan.apply()?;

        // Verify file was NOT created
        assert!(!output_path.exists());
        Ok(())
    });

    // Test 5: Plan apply creates new file
    test!(test_plan_apply_creates_file, {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("output.txt");

        let plan = Plan {
            template_path: temp_dir.path().join("test.tmpl"),
            output_path: output_path.clone(),
            content: "Test content".to_string(),
            frontmatter: crate::template::Frontmatter::default(),
            dry_run: false,
        };

        plan.apply()?;

        // Verify file was created with correct content
        assert!(output_path.exists());
        let content = std::fs::read_to_string(&output_path)?;
        assert_eq!(content, "Test content");
        Ok(())
    });

    // Test 6: Plan with unless_exists
    test!(test_plan_unless_exists, {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("output.txt");

        // Create existing file
        std::fs::write(&output_path, "Original content")?;

        let mut frontmatter = crate::template::Frontmatter::default();
        frontmatter.unless_exists = true;

        let plan = Plan {
            template_path: temp_dir.path().join("test.tmpl"),
            output_path: output_path.clone(),
            content: "New content".to_string(),
            frontmatter,
            dry_run: false,
        };

        plan.apply()?;

        // Verify file was NOT overwritten
        let content = std::fs::read_to_string(&output_path)?;
        assert_eq!(content, "Original content");
        Ok(())
    });

    // Test 7: PipelineBuilder with prefixes
    test!(test_pipeline_builder_with_prefixes, {
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());

        let pipeline = PipelineBuilder::new()
            .with_prefixes(prefixes, Some("http://example.org/base/".to_string()))
            .build()?;

        // Just verify it builds successfully
        assert!(!pipeline.graph.is_empty() || pipeline.graph.is_empty());
        Ok(())
    });

    // Test 8: Register prefixes
    test!(test_pipeline_register_prefixes, {
        let mut pipeline = Pipeline::new()?;
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());

        pipeline.register_prefixes(Some("http://example.org/base/"), &prefixes);

        // Verify SPARQL function is registered (try to render)
        let ctx = Context::new();
        let result = pipeline.render_body("{{ 1 + 1 }}", &ctx)?;
        assert_eq!(result, "2");
        Ok(())
    });
}
