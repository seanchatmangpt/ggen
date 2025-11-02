// ggen-ai/src/rdf/generator.rs
//
// Phase 5: Main Generator - Complete CLI generation pipeline

use anyhow::Result;
use std::path::{Path, PathBuf};

use crate::rdf::{QueryExecutor, RdfParser, TemplateRenderer};

/// Main entry point for RDF-to-CLI generation
pub struct CliGenerator {
    template_dir: PathBuf,
}

impl CliGenerator {
    /// Create a new CLI generator with the specified template directory
    pub fn new(template_dir: PathBuf) -> Self {
        Self { template_dir }
    }

    /// Generate a complete CLI project from a TTL file
    ///
    /// This is the main pipeline that orchestrates all phases:
    /// 1. Parse RDF (load schema + user TTL)
    /// 2. Execute SPARQL queries to extract project structure
    /// 3. Validate the extracted project data
    /// 4. Render all templates to generate project files
    /// 5. Run post-generation hooks (cargo fmt, cargo check)
    pub fn generate_from_ttl(&self, ttl_path: &Path, output_dir: &Path) -> Result<()> {
        println!("Generating CLI project from {}", ttl_path.display());

        // Step 1: Parse RDF
        println!("  [1/5] Parsing RDF...");
        let mut parser = RdfParser::new()?;
        parser.load_schema()?;
        parser.load_ttl(ttl_path)?;

        // Step 2: Execute SPARQL queries
        println!("  [2/5] Extracting project structure...");
        let executor = QueryExecutor::new(parser.get_store());
        let mut project = executor.extract_project()?;
        project.nouns = executor.extract_nouns()?;
        project.dependencies = executor.extract_dependencies()?;

        // Step 3: Validate project
        println!("  [3/5] Validating project...");
        validate_project(&project)?;

        // Step 4: Render templates
        println!("  [4/5] Rendering templates...");
        let renderer = TemplateRenderer::new(&self.template_dir)?;
        std::fs::create_dir_all(output_dir)?;
        renderer.render_all(&project, output_dir)?;

        // Step 5: Post-generation
        println!("  [5/5] Running post-generation hooks...");
        run_post_generation(output_dir)?;

        println!("✓ CLI project generated at {}", output_dir.display());

        Ok(())
    }
}

/// Validate the extracted project structure
///
/// Ensures:
/// - At least one noun exists
/// - Each noun has at least one verb
fn validate_project(project: &crate::rdf::types::CliProject) -> Result<()> {
    if project.nouns.is_empty() {
        anyhow::bail!("Project must have at least one noun");
    }

    for noun in &project.nouns {
        if noun.verbs.is_empty() {
            anyhow::bail!("Noun '{}' must have at least one verb", noun.name);
        }
    }

    Ok(())
}

/// Run post-generation hooks
///
/// Executes:
/// - cargo fmt: Format generated code
/// - cargo check: Verify project compiles
fn run_post_generation(output_dir: &Path) -> Result<()> {
    use std::process::Command;

    // Format code
    let fmt_status = Command::new("cargo")
        .args(&["fmt"])
        .current_dir(output_dir)
        .status()?;

    if !fmt_status.success() {
        anyhow::bail!("cargo fmt failed");
    }

    // Check compilation
    let check_status = Command::new("cargo")
        .args(&["check"])
        .current_dir(output_dir)
        .status()?;

    if !check_status.success() {
        anyhow::bail!("cargo check failed");
    }

    Ok(())
}
